#include "fortran03-codegen.h"
#include "fortran03-typeutils.h"
#include "cxx-nodecl-visitor.h"
#include "cxx-driver-decls.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include <string.h>

typedef
struct nodecl_codegen_visitor_tag
{
    // Base visitor
    nodecl_external_visitor_t _base_visitor;

    // Codegen
    FILE *file;
    int indent_level;
    scope_entry_t* current_sym;
} nodecl_codegen_visitor_t;

typedef void (*codegen_visitor_fun_t)(nodecl_codegen_visitor_t* visitor, nodecl_t node);
typedef void (*nodecl_visitor_fun_t)(nodecl_external_visitor_t* visitor, nodecl_t node);

static char* fortran_codegen_to_str(nodecl_t node);

// This is safer than using the macro directly as it will warn us against wrong types
// while the macro does not
static inline nodecl_visitor_fun_t codegen_visitor_fun(codegen_visitor_fun_t p)
{
    return NODECL_VISITOR_FUN(p);
}
static inline void codegen_walk(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    NODECL_WALK(visitor, node);
}

static void indent(nodecl_codegen_visitor_t* v)
{
    int i;
    for (i = 0; i < v->indent_level; i++)
    {
        // FIXME - Make this spacing configurable
        fprintf(v->file, "  ");
    }
}

static void codegen_comma_separated_list(nodecl_codegen_visitor_t *visitor, nodecl_t node)
{
    if (nodecl_is_null(node))
        return;

    AST list = nodecl_get_ast(node);

    ERROR_CONDITION(ASTType(list) != AST_NODE_LIST, "Invalid node kind", 0);

    AST it;
    for_each_element(list, it)
    {
        AST current = ASTSon1(it);

        codegen_walk(visitor, _nodecl_wrap(current));

        // If we are not the last
        if (it != list)
        {
            fprintf(visitor->file, ", ");
        }
    }
}

// Codegen
static void not_implemented_yet(nodecl_external_visitor_t* visitor UNUSED_PARAMETER, nodecl_t node)
{
    internal_error("WARNING -> Uninmplemented node! '%s'\n", ast_print_node_type(ASTType(nodecl_get_ast(node))));
}

static void codegen_top_level(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t list = nodecl_get_child(node, 0);
    codegen_walk(visitor, list);
}

static void declare_symbols_rec(nodecl_codegen_visitor_t* visitor, nodecl_t node);
static void declare_symbol(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry);

static void codegen_type(nodecl_codegen_visitor_t* visitor, 
        type_t* t, const char** type_specifier, const char **array_specifier)
{
    (*type_specifier) = "";
    char is_pointer = 0;
    if (is_pointer_type(t))
    {
        is_pointer = 1;
        t = pointer_type_get_pointee_type(t);
    }

    struct array_spec_tag {
        nodecl_t lower;
        nodecl_t upper;
        char is_undefined;
    } array_spec_list[MCXX_MAX_ARRAY_SPECIFIER] = { { nodecl_null(), nodecl_null(), 0 }  };

    int array_spec_idx;
    for (array_spec_idx = MCXX_MAX_ARRAY_SPECIFIER - 1; 
            is_fortran_array_type(t);
            array_spec_idx--)
    {
        if (array_spec_idx < 0)
        {
            internal_error("too many array dimensions %d\n", MCXX_MAX_ARRAY_SPECIFIER);
        }

        if (!array_type_is_unknown_size(t))
        {
            array_spec_list[array_spec_idx].lower = array_type_get_array_lower_bound(t);
            array_spec_list[array_spec_idx].upper = array_type_get_array_upper_bound(t);

            declare_symbols_rec(visitor, array_spec_list[array_spec_idx].lower);
            declare_symbols_rec(visitor, array_spec_list[array_spec_idx].upper);
        }
        else
        {
            array_spec_list[array_spec_idx].is_undefined = 1;
        }

        t = array_type_get_element_type(t);
    }

    char is_array = (array_spec_idx != (MCXX_MAX_ARRAY_SPECIFIER - 1));

    if (is_bool_type(t)
            || is_integer_type(t)
            || is_float_type(t)
            || is_double_type(t)
            || is_complex_type(t))
    {
        const char* type_name = NULL;
        char c[128] = { 0 };

        if (is_bool_type(t))
        {
            type_name = "LOGICAL";
        }
        // Do not move this after is_integer_type
        else if (is_character_type(t))
        {
            type_name = "CHARACTER";
        }
        else if (is_integer_type(t))
        {
            type_name = "INTEGER";
        }
        else if (is_float_type(t)
                || is_double_type(t))
        {
            type_name = "REAL";
        }
        else if (is_complex_type(t))
        {
            type_name = "COMPLEX";
        }
        else
        {
            internal_error("unreachable code", 0);
        }

        snprintf(c, 127, "%s(%zd)", type_name, type_get_size(t));
        c[127] = '\0';

        (*type_specifier) = uniquestr(c);
    }
    else if (is_class_type(t))
    {
        scope_entry_t* entry = named_type_get_symbol(t);
        char c[128] = { 0 };

        declare_symbol(visitor, entry);

        snprintf(c, 127, "TYPE(%s)", 
                entry->symbol_name);
        c[127] = '\0';

        (*type_specifier) = uniquestr(c);
    }
    else if (is_fortran_character_type(t))
    {
        nodecl_t length = array_type_get_array_size_expr(t);
        char c[128] = { 0 };
        snprintf(c, 127, "CHARACTER(LEN=%s)",
                nodecl_is_null(length) ? "*" : fortran_codegen_to_str(length));
        c[127] = '\0';
        (*type_specifier) = uniquestr(c);
    }
    else 
    {
        internal_error("Not a FORTRAN printable type '%s'\n", print_declarator(t));
    }

    if (is_pointer)
    {
        (*type_specifier) = strappend((*type_specifier), ", POINTER");
    }

    if (is_array)
    {
        array_spec_idx++;
        (*array_specifier) = "(";

        while (array_spec_idx <= (MCXX_MAX_ARRAY_SPECIFIER - 1))
        {
            if (!array_spec_list[array_spec_idx].is_undefined)
            {
                (*array_specifier) = strappend((*array_specifier), fortran_codegen_to_str(array_spec_list[array_spec_idx].lower));
                (*array_specifier) = strappend((*array_specifier), ":");
                (*array_specifier) = strappend((*array_specifier), fortran_codegen_to_str(array_spec_list[array_spec_idx].upper));
            }
            else
            {
                (*array_specifier) = strappend((*array_specifier), ":");
            }
            if ((array_spec_idx + 1) <= (MCXX_MAX_ARRAY_SPECIFIER - 1))
            {
                (*array_specifier) = strappend((*array_specifier), ", ");
            }
            array_spec_idx++;
        }

        (*array_specifier) = strappend((*array_specifier), ")");
    }
}

static void declare_symbol(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry)
{
    if (entry->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
        return;

    // Do not declare stuff not in our context
    if (visitor->current_sym != entry->decl_context.current_scope->related_entry)
        return;

    entry->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;

    switch (entry->kind)
    {
        case SK_VARIABLE:
            {
                const char* type_spec = "";
                const char* array_specifier = "";
                const char* initializer = "";

                codegen_type(visitor, entry->type_information, &type_spec, &array_specifier);

                const char* attribute_list = "";

                if (entry->entity_specs.is_allocatable)
                    attribute_list = strappend(attribute_list, ", ALLOCATABLE");
                if (entry->entity_specs.is_target)
                    attribute_list = strappend(attribute_list, ", TARGET");
                if (entry->entity_specs.is_value)
                    attribute_list = strappend(attribute_list, ", VALUE");
                if (entry->entity_specs.is_optional)
                    attribute_list = strappend(attribute_list, ", OPTIONAL");
                if (entry->entity_specs.is_static)
                    attribute_list = strappend(attribute_list, ", SAVE");
                if (is_volatile_qualified_type(entry->type_information))
                    attribute_list = strappend(attribute_list, ", VOLATILE");
                if (is_const_qualified_type(entry->type_information))
                {
                    attribute_list = strappend(attribute_list, ", PARAMETER");
                }

                if (!nodecl_is_null(entry->value))
                {
                    declare_symbols_rec(visitor, entry->value);
                    initializer = strappend(" = ", fortran_codegen_to_str(entry->value));
                }

                indent(visitor);
                fprintf(visitor->file, "%s%s :: %s%s%s\n", 
                        type_spec,
                        attribute_list,
                        entry->symbol_name,
                        array_specifier,
                        initializer);
                break;
            }
        case SK_NAMELIST:
            {
                break;
            }
        case SK_COMMON:
            {
                break;
            }
        case SK_CLASS:
            {
                indent(visitor);
                fprintf(visitor->file, "TYPE :: %s\n", entry->symbol_name);
                int i, num_components = class_type_get_num_nonstatic_data_members(entry->type_information);

                scope_entry_t* old_sym = visitor->current_sym;
                visitor->current_sym = entry;
                visitor->indent_level++;
                for (i = 0; i < num_components; i++)
                {
                    declare_symbol(visitor, 
                            class_type_get_nonstatic_data_member_num(entry->type_information, i));
                }
                visitor->indent_level--;
                visitor->current_sym = old_sym;

                indent(visitor);
                fprintf(visitor->file, "END TYPE %s\n", entry->symbol_name);
                break;
            }
        default:
            {
                internal_error("Unexpected symbol '%s'\n", symbol_kind_name(entry));
            }
    }
}

static void declare_symbols_rec(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    if (nodecl_is_null(node))
        return;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        declare_symbols_rec(visitor, nodecl_get_child(node, i));
    }

    scope_entry_t* entry = nodecl_get_symbol(node);
    if (entry != NULL)
    {
        declare_symbol(visitor, entry);
    }
}

static void declare_everything_needed(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    declare_symbols_rec(visitor, node);
}

static void codegen_function_code(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* entry = nodecl_get_symbol(node);
    nodecl_t statement_seq = nodecl_get_child(node, 0);
    nodecl_t internal_subprograms = nodecl_get_child(node, 1);

    scope_entry_t* old_sym = visitor->current_sym;
    visitor->current_sym = entry;

    switch (entry->kind)
    {
        case SK_PROGRAM:
            {
                const char* program_name = entry->symbol_name[0] == '_' ? "" : entry->symbol_name;
                fprintf(visitor->file, "PROGRAM %s\n", program_name);
                visitor->indent_level++;
                declare_everything_needed(visitor, statement_seq);
                codegen_walk(visitor, statement_seq);
                visitor->indent_level--;

                if (!nodecl_is_null(internal_subprograms))
                {
                    indent(visitor);
                    fprintf(visitor->file, "CONTAINS\n");

                    visitor->indent_level++;
                    codegen_walk(visitor, internal_subprograms);
                    visitor->indent_level--;
                }

                fprintf(visitor->file, "END PROGRAM %s\n", program_name);
                break;
            }
        // case SK_FUNCTION:
        //     {
        //         break;
        //     }
        // case SK_SUBROUTINE:
        //     {
        //         break;
        //     }
        default:
            {
                internal_error("Unexpected symbol kind %s", symbol_kind_name(entry));
                break;
            }
    }

    visitor->current_sym = old_sym;
}


#define OPERATOR_TABLE \
    PREFIX_UNARY_EXPRESSION(plus, " +") \
    PREFIX_UNARY_EXPRESSION(neg, " -") \
    PREFIX_UNARY_EXPRESSION(logical_not, " .NOT.") \
    BINARY_EXPRESSION(mul, " * ") \
    BINARY_EXPRESSION(div, " / ") \
    BINARY_EXPRESSION(add, " + ") \
    BINARY_EXPRESSION(minus, " - ") \
    BINARY_EXPRESSION(lower_than, " < ") \
    BINARY_EXPRESSION(greater_than, " > ") \
    BINARY_EXPRESSION(greater_or_equal_than, " <= ") \
    BINARY_EXPRESSION(lower_or_equal_than, " => ") \
    BINARY_EXPRESSION(logical_and, " .AND. ") \
    BINARY_EXPRESSION(logical_or, " .OR. ") \
    BINARY_EXPRESSION(power, " ** ") \
    BINARY_EXPRESSION(concat, " // ") \
    BINARY_EXPRESSION(class_member_access, " % ") 

#if 0
BINARY_EXPRESSION(equal, "=="
BINARY_EXPRESSION(different, "/=") 
BINARY_EXPRESSION(logical_equal, ".EQV.") 
BINARY_EXPRESSION(logical_different, ".NEQV.") 
#endif

// Note that in Fortran we must retain parentheses, so nodecl already contains
// them, no need to check priorities

#define PREFIX_UNARY_EXPRESSION(_name, _operand) \
    static void codegen_##_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) \
    { \
        nodecl_t rhs = nodecl_get_child(node, 0); \
        fprintf(visitor->file, "%s", _operand); \
        codegen_walk(visitor, rhs); \
    }
#define BINARY_EXPRESSION(_name, _operand) \
    static void codegen_##_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) \
    { \
        nodecl_t lhs = nodecl_get_child(node, 0); \
        nodecl_t rhs = nodecl_get_child(node, 1); \
        codegen_walk(visitor, lhs); \
        fprintf(visitor->file, "%s", _operand); \
        codegen_walk(visitor, rhs); \
    }
OPERATOR_TABLE
#undef PREFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION

static void codegen_parenthesized_expression(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t nest = nodecl_get_child(node, 0);
    fprintf(visitor->file, "(");
    codegen_walk(visitor, nest);
    fprintf(visitor->file, ")");
}

static type_t* basic_type(type_t* t)
{
    while (is_pointer_type(t))
    {
        t = pointer_type_get_pointee_type(t);
    }
    return t;
}

static void codegen_comparison(nodecl_codegen_visitor_t* visitor, nodecl_t node, 
        const char* operator_arith, 
        const char* operator_bool)
{
        nodecl_t lhs = nodecl_get_child(node, 0); 
        type_t* lhs_type = basic_type(nodecl_get_type(lhs));

        nodecl_t rhs = nodecl_get_child(node, 1); 
        type_t* rhs_type = basic_type(nodecl_get_type(rhs));

        codegen_walk(visitor, lhs); 

        if (is_bool_type(lhs_type) && is_bool_type(rhs_type))
        {
            fprintf(visitor->file, "%s", operator_bool); 
        }
        else
        {
            fprintf(visitor->file, "%s", operator_arith); 
        }

        codegen_walk(visitor, rhs); 
}

static void codegen_equal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_comparison(visitor, node, "==", ".EQV.");
}

static void codegen_different(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_comparison(visitor, node, "/=", ".NEQV.");
}

static void codegen_derreference(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    // No explicit dereference happens in Fortran
    nodecl_t op = nodecl_get_child(node, 0); 
    codegen_walk(visitor, op);
}

static void codegen_reference(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    // No explicit reference happens in Fortran
    nodecl_t op = nodecl_get_child(node, 0); 
    codegen_walk(visitor, op);
}

static void codegen_assignment(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
        nodecl_t lhs = nodecl_get_child(node, 0); 
        nodecl_t rhs = nodecl_get_child(node, 1); 

        scope_entry_t* symbol = nodecl_get_symbol(lhs);
        ERROR_CONDITION(symbol == NULL, "This should not be NULL", 0);

        codegen_walk(visitor, lhs);

        const char* operator = " = ";

        // Is this a pointer assignment?
        char is_ptr_assignment = 0;
        if (is_pointer_type(symbol->type_information))
        {
            if (!symbol->entity_specs.is_parameter
                    || symbol->entity_specs.is_value)
            {
                if (nodecl_get_kind(lhs) != NODECL_DERREFERENCE)
                {
                    is_ptr_assignment = 1;
                }
            }
            else
            {
                ERROR_CONDITION(nodecl_get_kind(lhs) != NODECL_DERREFERENCE, 
                        "A dummy argument must be derreferenced unless it is VALUE", 0);
                nodecl_t n = nodecl_get_child(lhs, 0);
                if (nodecl_get_kind(n) != NODECL_DERREFERENCE)
                {
                    is_ptr_assignment = 1;
                }
            }
        }

        if (is_ptr_assignment)
        {
            operator = " => ";
        }

        fprintf(visitor->file, operator);

        codegen_walk(visitor, rhs);
}

static void codegen_symbol(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* entry = nodecl_get_symbol(node);
    fprintf(visitor->file, "%s", entry->symbol_name);
}

static void codegen_integer_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const_value_t* value = nodecl_get_constant(node);

    fprintf(visitor->file, "%lld", (long long int)const_value_cast_to_8(value));

    int num_bytes = const_value_get_bytes(value);

    // Print the kind if it is different to the usual one 
    // FIXME: This should be configurable
    if (num_bytes != 4)
    {
        fprintf(visitor->file, "_%d", num_bytes);
    }
}

static void codegen_array_subscript(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t subscripted = nodecl_get_child(node, 0);
    nodecl_t subscript = nodecl_get_child(node, 1);

    codegen_walk(visitor, subscripted);

    fprintf(visitor->file, "(");
    // We keep a list instead of a single dimension for multidimensional arrays
    // alla Fortran
    codegen_comma_separated_list(visitor, subscript);
    fprintf(visitor->file, ")");
}

// FIXME - Fortran 2003 blocks
static void codegen_compound_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_walk(visitor, nodecl_get_child(node, 0));
}

static void codegen_expression_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, "\n");
}

static void fortran_codegen_init(nodecl_codegen_visitor_t* codegen_visitor)
{
    memset(codegen_visitor, 0, sizeof(*codegen_visitor));

    nodecl_init_walker((nodecl_external_visitor_t*)codegen_visitor, not_implemented_yet);

    NODECL_VISITOR(codegen_visitor)->visit_top_level = codegen_visitor_fun(codegen_top_level);
    NODECL_VISITOR(codegen_visitor)->visit_function_code = codegen_visitor_fun(codegen_function_code);
    NODECL_VISITOR(codegen_visitor)->visit_compound_statement = codegen_visitor_fun(codegen_compound_statement);
    NODECL_VISITOR(codegen_visitor)->visit_expression_statement = codegen_visitor_fun(codegen_expression_statement);
#define PREFIX_UNARY_EXPRESSION(_name, _) \
    NODECL_VISITOR(codegen_visitor)->visit_##_name = codegen_visitor_fun(codegen_##_name);
#define BINARY_EXPRESSION(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
    OPERATOR_TABLE
#undef BINARY_EXPRESSION
#undef PREFIX_UNARY_EXPRESSION
    NODECL_VISITOR(codegen_visitor)->visit_integer_literal = codegen_visitor_fun(codegen_integer_literal);
    NODECL_VISITOR(codegen_visitor)->visit_symbol = codegen_visitor_fun(codegen_symbol);
    NODECL_VISITOR(codegen_visitor)->visit_assignment = codegen_visitor_fun(codegen_assignment);
    NODECL_VISITOR(codegen_visitor)->visit_equal = codegen_visitor_fun(codegen_equal);
    NODECL_VISITOR(codegen_visitor)->visit_different = codegen_visitor_fun(codegen_different);
    NODECL_VISITOR(codegen_visitor)->visit_derreference = codegen_visitor_fun(codegen_derreference);
    NODECL_VISITOR(codegen_visitor)->visit_reference = codegen_visitor_fun(codegen_reference);
    NODECL_VISITOR(codegen_visitor)->visit_parenthesized_expression = codegen_visitor_fun(codegen_parenthesized_expression);
    NODECL_VISITOR(codegen_visitor)->visit_array_subscript = codegen_visitor_fun(codegen_array_subscript);
}

void fortran_codegen_translation_unit(FILE* f UNUSED_PARAMETER, AST a UNUSED_PARAMETER, scope_link_t* sl UNUSED_PARAMETER)
{
    nodecl_codegen_visitor_t codegen_visitor;

    if (sl == NULL)
    {
        sl = CURRENT_COMPILED_FILE->scope_link;
    }

    fortran_codegen_init(&codegen_visitor);
    
    codegen_visitor.file = f;
    codegen_visitor.indent_level = 0;
    codegen_visitor.current_sym = NULL;

    codegen_walk(&codegen_visitor, _nodecl_wrap(a));
}


static char* fortran_codegen_to_str(nodecl_t node)
{
    char *str = NULL;
    size_t size = 0;
    FILE* temporal_stream = open_memstream(&str, &size);

    nodecl_codegen_visitor_t codegen_visitor;

    fortran_codegen_init(&codegen_visitor);

    codegen_visitor.file = temporal_stream;
    codegen_visitor.indent_level = 0;
    codegen_visitor.current_sym = NULL;

    codegen_walk(&codegen_visitor, node);

    fclose(temporal_stream);

    return str;
}
