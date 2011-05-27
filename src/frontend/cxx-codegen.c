#include "cxx-codegen.h"
#include "cxx-utils.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "cxx-prettyprint.h"
#include "cxx-nodecl-visitor.h"
#include <string.h>
#include <limits.h>

typedef
struct nodecl_codegen_visitor_tag
{
    // Base visitor
    nodecl_external_visitor_t _base_visitor;

    // Codegen
    FILE *file;
    int indent_level;
    scope_entry_t* current_sym;

    // Minor details during codegen
    char in_condition;
    char initializer;
} nodecl_codegen_visitor_t;

static void indent(nodecl_codegen_visitor_t* v)
{
    int i;
    for (i = 0; i < v->indent_level; i++)
    {
        // FIXME - Make this configurable
        fprintf(v->file, "  ");
    }
}

static void walk_type_for_symbols(
        nodecl_codegen_visitor_t *visitor,
        type_t* t,
        char needs_def, 
        void symbol_to_declare(nodecl_codegen_visitor_t*, scope_entry_t*),
        void symbol_to_define(nodecl_codegen_visitor_t*, scope_entry_t*))
{
    if (t == NULL)
        return;

    if (is_pointer_type(t))
    {
        walk_type_for_symbols(visitor, pointer_type_get_pointee_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define);
    }
    else if (is_array_type(t))
    {
        walk_type_for_symbols(visitor, array_type_get_element_type(t), /* needs_def */ 1, symbol_to_declare, symbol_to_define);
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        walk_type_for_symbols(visitor, reference_type_get_referenced_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define);
    }
    else if (is_pointer_to_class_type(t))
    {
        walk_type_for_symbols(visitor, pointer_type_get_pointee_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define);
        walk_type_for_symbols(visitor, pointer_to_member_type_get_class_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define);
    }
    else if (is_function_type(t))
    {
        walk_type_for_symbols(visitor, function_type_get_return_type(t), 
                /* needs_def */ 0, symbol_to_declare, symbol_to_define);
        int i;
        for (i = 0; i < function_type_get_num_parameters(t); i++)
        {
            walk_type_for_symbols(visitor, function_type_get_parameter_type_num(t, i), 
                    /* needs_def */ 0, symbol_to_declare, symbol_to_define);
        }
    }
    else if (is_vector_type(t))
    {
        walk_type_for_symbols(visitor, vector_type_get_element_type(t), /* needs_def */ 1, symbol_to_declare, symbol_to_define);
    }
    else if (is_class_type(t))
    {
        scope_entry_t* class_entry = named_type_get_symbol(t);
        if (needs_def)
        {
            symbol_to_define(visitor, class_entry);
        }
        else
        {
            symbol_to_declare(visitor, class_entry);
        }
    }
    else if (is_enum_type(t))
    {
        scope_entry_t* enum_entry = named_type_get_symbol(t);
        if (needs_def)
        {
            symbol_to_define(visitor, enum_entry);
        }
        else
        {
            symbol_to_declare(visitor, enum_entry);
        }
    }
    else
    {
        // Do nothing as it will be a builtin type
    }
}

static void declare_symbol(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void define_symbol(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void declare_symbol_if_nonlocal(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void define_symbol_if_nonlocal(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);

static void codegen_type_of_symbol(
        nodecl_codegen_visitor_t *visitor,
        type_t* t,
        char needs_def)
{
    walk_type_for_symbols(visitor, t, needs_def, declare_symbol, define_symbol);
}

static void codegen_type_of_symbol_only_nonlocal(
        nodecl_codegen_visitor_t *visitor,
        type_t* t,
        char needs_def)
{
    walk_type_for_symbols(visitor, t, needs_def, declare_symbol_if_nonlocal, define_symbol_if_nonlocal);
}

static void add_to_clear_list(scope_entry_t* entry);

static void define_local_entities_in_trees(nodecl_codegen_visitor_t* visitor, nodecl_t node, scope_t* current_scope)
{
    if (nodecl_is_null(node))
        return;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        define_local_entities_in_trees(visitor, nodecl_get_children(node, i), current_scope);
    }

    scope_entry_t* entry = nodecl_get_symbol(node);
    if (entry != NULL
            && entry->type_information != NULL)
    {
        codegen_type_of_symbol(visitor, entry->type_information, /* needs def */ 1);
        if (current_scope == entry->decl_context.current_scope)
        {
            if (ASTType(nodecl_get_ast(node)) != AST_OBJECT_INIT)
            {
                define_symbol(visitor, entry);
            }
            else
            {
                // If this is an object init (and the traversal ensures that
                // they will be seen first) we assume it's already been defined
                entry->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                add_to_clear_list(entry);
            }
        }
    }

    type_t* type = nodecl_get_type(node);
    if (type != NULL)
    {
        codegen_type_of_symbol(visitor, type, /* needs def */ 1);
    }
}

static void define_nonlocal_entities_in_trees(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    if (nodecl_is_null(node))
        return;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        define_nonlocal_entities_in_trees(visitor, nodecl_get_children(node, i));
    }

    scope_entry_t* entry = nodecl_get_symbol(node);
    if (entry != NULL
            && entry->type_information != NULL)
    {
        codegen_type_of_symbol_only_nonlocal(visitor, entry->type_information, /* needs def */ 1);
        define_symbol_if_nonlocal(visitor, entry);
    }

    type_t* type = nodecl_get_type(node);
    if (type != NULL)
    {
        codegen_type_of_symbol_only_nonlocal(visitor, type, /* needs def */ 1);
    }
}

static scope_entry_list_t *_clear_list = NULL;

static void add_to_clear_list(scope_entry_t* entry)
{
    _clear_list = entry_list_add(_clear_list, entry);
}

static void run_clear_list(void)
{
    if (_clear_list != NULL)
    {
        scope_entry_list_iterator_t* it = entry_list_iterator_begin(_clear_list);
        while (!entry_list_iterator_end(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            entry->entity_specs.codegen_status = CODEGEN_STATUS_NONE;

            entry_list_iterator_next(it);
        }
        entry_list_iterator_free(it);
    }
    entry_list_free(_clear_list);
    _clear_list = NULL;
}

static void define_symbol(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    ERROR_CONDITION(symbol == NULL, "Invalid symbol", 0);

    if (symbol->do_not_print)
        return;

    add_to_clear_list(symbol);

    switch (symbol->kind)
    {
        case SK_VARIABLE:
            {
                declare_symbol(visitor, symbol);
                break;
            }
        case SK_TYPEDEF:
            {
                codegen_type_of_symbol(visitor,
                        symbol->type_information,
                        /* needs_def */ 1);
                // Do nothing if already defined
                if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
                    return;
                indent(visitor);
                fprintf(visitor->file, "typedef %s;\n", print_decl_type_str(symbol->type_information, 
                            symbol->decl_context, symbol->symbol_name));
                break;
            }
        case SK_ENUM:
            {
                C_LANGUAGE()
                {
                    // the symbol will be already called 'struct/union X' in C
                    indent(visitor);
                    fprintf(visitor->file, "%s\n", symbol->symbol_name);
                    indent(visitor);
                    fprintf(visitor->file, "{\n");
                }
                CXX_LANGUAGE()
                {
                    indent(visitor);
                    fprintf(visitor->file, "enum %s\n", symbol->symbol_name);
                    indent(visitor);
                    fprintf(visitor->file, "{\n");
                }
                visitor->indent_level++;

                int i;
                for (i = 0; i < enum_type_get_num_enumerators(symbol->type_information); i++)
                {
                    scope_entry_t* enumerator = enum_type_get_enumerator_num(symbol->type_information, i);
                    if (i != 0)
                    {
                        fprintf(visitor->file, ",\n");
                    }
                    indent(visitor);
                    fprintf(visitor->file, "%s", enumerator->symbol_name);
                    enumerator->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                }

                visitor->indent_level--;

                fprintf(visitor->file, "\n");
                indent(visitor);
                fprintf(visitor->file, "};\n");
                break;
            }
        case SK_CLASS:
            {
                // Do nothing if already defined
                if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
                    return;

                int i = 0;
                C_LANGUAGE()
                {
                    // the symbol will be already called 'struct/union X' in C
                    indent(visitor);
                    fprintf(visitor->file, "%s\n", symbol->symbol_name);
                    indent(visitor);
                    fprintf(visitor->file, "{\n");
                }
                access_specifier_t default_access_spec = AS_UNKNOWN;
                CXX_LANGUAGE()
                {
                    // TODO - Namespaces
                    const char *class_key = "";
                    switch (class_type_get_class_kind(symbol->type_information))
                    {
                        case CK_CLASS:
                            class_key = "class";
                            default_access_spec = AS_PRIVATE;
                            break;
                        case CK_STRUCT:
                            class_key = "struct";
                            default_access_spec = AS_PUBLIC;
                            break;
                        case CK_UNION:
                            class_key = "union";
                            default_access_spec = AS_PUBLIC;
                            break;
                        default:
                            internal_error("Invalid class kind", 0);
                    }

                    if (class_type_get_num_bases(symbol->type_information) != 0)
                    {
                        // We need to define all the bases first
                        for (i = 0; i < class_type_get_num_bases(symbol->type_information); i++)
                        {
                            scope_entry_t* entry = class_type_get_base_num(symbol->type_information, i, NULL, NULL, NULL);
                            define_symbol(visitor, entry);
                        }
                    }

                    indent(visitor);
                    fprintf(visitor->file, "%s %s", class_key, symbol->symbol_name);

                    if (class_type_get_num_bases(symbol->type_information) != 0)
                    {
                        fprintf(visitor->file, " : ");
                        for (i = 0; i < class_type_get_num_bases(symbol->type_information); i++)
                        {
                            if (i != 0)
                            {
                                fprintf(visitor->file, ", ");
                            }

                            access_specifier_t current_access_spec = AS_UNKNOWN;
                            char is_virtual = 0;
                            scope_entry_t* base = class_type_get_base_num(symbol->type_information, i, 
                                    &is_virtual, 
                                    /* is_dependent */ NULL, 
                                    &current_access_spec);

                            if (is_virtual)
                            {
                                fprintf(visitor->file, "virtual ");
                            }

                            if (current_access_spec != default_access_spec)
                            {
                                if (current_access_spec == AS_PUBLIC)
                                {
                                    fprintf(visitor->file, "public ");
                                }
                                else if (current_access_spec == AS_PRIVATE)
                                {
                                    fprintf(visitor->file, "private ");
                                }
                                else if (current_access_spec == AS_PROTECTED)
                                {
                                    fprintf(visitor->file, "protected ");
                                }
                                else
                                {
                                    internal_error("Unreachable code", 0);
                                }
                            }

                            fprintf(visitor->file, "%s", get_qualified_symbol_name(base, symbol->decl_context));
                        }
                    }

                    fprintf(visitor->file, "{\n");
                }


                access_specifier_t current_access_spec = default_access_spec;
                int num_members = class_type_get_num_members(symbol->type_information);
                for (i = 0; i < num_members; i++)
                {
                    scope_entry_t* entry = class_type_get_member_num(symbol->type_information, i);
                    access_specifier_t access_spec = entry->entity_specs.access;

                    CXX_LANGUAGE()
                    {
                        visitor->indent_level += 1;
                        if (current_access_spec != access_spec)
                        {
                            current_access_spec = access_spec;

                            indent(visitor);
                            if (current_access_spec == AS_PUBLIC)
                            {
                                fprintf(visitor->file, "public:\n");
                            }
                            else if (current_access_spec == AS_PRIVATE)
                            {
                                fprintf(visitor->file, "private:\n");
                            }
                            else if (current_access_spec == AS_PROTECTED)
                            {
                                fprintf(visitor->file, "protected:\n");
                            }
                            else
                            {
                                internal_error("Unreachable code", 0);
                            }
                        }
                    }

                    visitor->indent_level += 1;
                    declare_symbol(visitor, entry);
                    visitor->indent_level--;

                    CXX_LANGUAGE()
                    {
                        visitor->indent_level--;
                    }
                }

                indent(visitor);
                fprintf(visitor->file, "};\n");

                break;
            }
        default:
            {
                internal_error("I do not know how to define a %s\n", symbol_kind_name(symbol));
            }
    }

    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
}

static void declare_symbol(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    // Do nothing if already defined or declared
    if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED
            || symbol->entity_specs.codegen_status == CODEGEN_STATUS_DECLARED)
        return;

    add_to_clear_list(symbol);
    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DECLARED;

    if (symbol->do_not_print)
        return;

    switch (symbol->kind)
    {
        case SK_VARIABLE:
            {
                const char* decl_specifiers = "";
                const char* gcc_attributes = "";
                const char* declarator = "";

                if (symbol->entity_specs.is_static)
                {
                    decl_specifiers = strappend(decl_specifiers, "static ");
                }
                if (symbol->entity_specs.is_extern)
                {
                    decl_specifiers = strappend(decl_specifiers, "extern ");
                }
                else 
                {
                    // If this not a member, or if it is, is nonstatic, this has already been defined
                    if (!symbol->entity_specs.is_member
                            || !symbol->entity_specs.is_static)
                    {
                        symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                    }
                }
                if (symbol->entity_specs.is_thread)
                {
                    decl_specifiers = strappend(decl_specifiers, "__thread ");
                }
                if (symbol->entity_specs.is_mutable)
                {
                    decl_specifiers = strappend(decl_specifiers, "mutable ");
                }
                if (symbol->entity_specs.is_register)
                {
                    decl_specifiers = strappend(decl_specifiers, "register ");
                }

                declarator = print_decl_type_str(symbol->type_information, symbol->decl_context, 
                        symbol->symbol_name);

                indent(visitor);
                fprintf(visitor->file, "%s%s%s",
                        decl_specifiers, gcc_attributes, declarator);

                // Initializer
                if (!nodecl_is_null(symbol->value))
                {
                    C_LANGUAGE()
                    {
                        fprintf(visitor->file, "%s", " = ");
                    }
                    CXX03_LANGUAGE()
                    {
                        // We only need = if the initializer is a structured one
                        // and this is C++03, in C++1x syntax { } is always allowed
                        if (ASTType(nodecl_get_ast(symbol->value)) == AST_STRUCTURED_LITERAL)
                        {
                            fprintf(visitor->file, "%s", " = ");
                        }
                    }

                    visitor->initializer = 1;
                    NODECL_WALK(visitor, symbol->value);
                    visitor->initializer = 0;
                }

                if (!visitor->in_condition)
                {
                    fprintf(visitor->file, ";\n");
                }
                break;
            }
        case SK_CLASS:
            {
                indent(visitor);
                C_LANGUAGE()
                {
                    // the symbol will be already called 'struct/union X' in C
                    fprintf(visitor->file, "%s;\n", symbol->symbol_name);
                }
                CXX_LANGUAGE()
                {
                    // TODO - Namespaces
                    const char *class_key = "";
                    switch (class_type_get_class_kind(symbol->type_information))
                    {
                        case CK_CLASS:
                            class_key = "class";
                            break;
                        case CK_STRUCT:
                            class_key = "struct";
                            break;
                        case CK_UNION:
                            class_key = "union";
                            break;
                        default:
                            internal_error("Invalid class kind", 0);
                    }

                    fprintf(visitor->file, "%s %s;\n", class_key, symbol->symbol_name);
                }
                break;
            }
        case SK_ENUM:
            {
                // Enums cannot be only declared but defined
                define_symbol(visitor, symbol);
                break;
            }
        case SK_TYPEDEF:
            {
                // Get a declaration to the aliased type
                codegen_type_of_symbol(visitor,
                        symbol->type_information,
                        /* needs_def */ 0);

                // A typedef cannot be only declared, it is always defined
                symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                indent(visitor);
                fprintf(visitor->file, "typedef %s;\n", print_decl_type_str(symbol->type_information, 
                        symbol->decl_context, symbol->symbol_name));
                break;
            }
        default:
            {
                internal_error("Do not know how to declare a %s\n", symbol_kind_name(symbol));
                break;
            }
    }
}

static char is_local_symbol(scope_entry_t* entry)
{
    return entry != NULL
        && (entry->decl_context.current_scope->kind == BLOCK_SCOPE
                || entry->decl_context.current_scope->kind == FUNCTION_SCOPE);
}

static void declare_symbol_if_nonlocal(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    if (!is_local_symbol(symbol))
    {
        declare_symbol(visitor, symbol);
    }
}

static void define_symbol_if_nonlocal(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    if (!is_local_symbol(symbol))
    {
        define_symbol(visitor, symbol);
    }
}

// Lists during codegen
static void walk_list(nodecl_codegen_visitor_t *visitor, nodecl_t node, const char* separator)
{
    if (nodecl_is_null(node))
        return;

    AST list = nodecl_get_ast(node);

    ERROR_CONDITION(ASTType(list) != AST_NODE_LIST, "Invalid node kind", 0);

    AST it;
    for_each_element(list, it)
    {
        AST current = ASTSon1(it);
        NODECL_WALK(visitor, _nodecl_wrap(current));

        // If we are not the last
        if (it != list)
        {
            fprintf(visitor->file, "%s", separator);
        }
    }
}

// Codegen
static void not_implemented_yet(nodecl_external_visitor_t* visitor UNUSED_PARAMETER, nodecl_t node)
{
    fprintf(stderr, "WARNING -> Uninmplemented node! '%s'\n", ast_print_node_type(ASTType(nodecl_get_ast(node))));
}

static void codegen_symbol(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* entry = nodecl_get_symbol(node);
    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);

    fprintf(visitor->file, "%s", get_qualified_symbol_name(entry, entry->decl_context));
}

static void codegen_integer_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const_value_t* value = nodecl_get_constant(node);
    ERROR_CONDITION(value == NULL, "Invalid value", 0);

    // FIXME: Improve this to use integer literal suffix when possible
    // FIXME: Write 'A' instead of 65
    if (const_value_is_signed(value))
    {
        fprintf(visitor->file, "%lld", (long long int)const_value_cast_to_8(value));
    }
    else
    {
        fprintf(visitor->file, "%llu", (unsigned long long)const_value_cast_to_8(value));
    }
}

// We return negative numbers because it is easier to list them in descending priority order
// so we want the first one to be the most 
static int get_rank(nodecl_t op)
{
    switch (ASTType(nodecl_get_ast(op)))
    {
        case AST_STRING_LITERAL:
        case AST_FLOATING_LITERAL:
        case AST_BOOLEAN_LITERAL:
        case AST_STRUCTURED_LITERAL:
        case AST_PARENTHESIZED_EXPRESSION:
        case AST_BUILTIN:
            {
                return -1;
            }
        case AST_ARRAY_SUBSCRIPT:
        case AST_FUNCTION_CALL:
        case AST_CLASS_MEMBER_ACCESS:
        case AST_TYPEID:
            {
                return -2;
            }
       case AST_PREINCREMENT:
       case AST_PREDECREMENT:
       case AST_REFERENCE:
       case AST_DERREFERENCE:
       case AST_PLUS:
       case AST_NEG:
       case AST_LOGICAL_NOT:
       case AST_BITWISE_NOT:
       case AST_SIZEOF:
            // case AST_NEW
            // case AST_DELETE
            // case AST_ALIGNOF
            // case AST_REAL
            // case AST_IMAG
            // case AST_LABEL_ADDR
            {
                return -3;
            }
            // This one is special as we keep several casts in a single node
        case AST_CAST:
            {
                if (IS_C_LANGUAGE
                        || strcmp(nodecl_get_text(op), "C") == 0)
                {
                    return -4;
                }
                else
                {
                    // These casts are postfix expressions actually
                    // static_cast, dynamic_cast, reinterpret_cast, const_cast
                    return -2;
                }
            }
       case AST_POINTER_TO_MEMBER:
            return -5;
       case AST_MUL:
       case AST_DIV:
       case AST_MOD:
            return -6;
       case AST_ADD:
       case AST_MINUS:
            return -7;
       case AST_SHL:
       case AST_SHR:
            return -8;
       case AST_LOWER_THAN:
       case AST_LOWER_OR_EQUAL_THAN:
       case AST_GREATER_THAN:
       case AST_GREATER_OR_EQUAL_THAN:
            return -9;
       case AST_EQUAL:
       case AST_DIFFERENT:
            return -10;
       case AST_BITWISE_AND:
            return -11;
       case AST_BITWISE_XOR:
            return -12;
       case AST_BITWISE_OR:
            return -13;
       case AST_LOGICAL_AND:
            return -14;
       case AST_LOGICAL_OR:
            return -15;
       case AST_CONDITIONAL_EXPRESSION:
            return -16;
       case AST_ASSIGNMENT:
       case AST_MUL_ASSIGNMENT :
       case AST_DIV_ASSIGNMENT:
       case AST_ADD_ASSIGNMENT:
       case AST_SUB_ASSIGNMENT:
       case AST_SHL_ASSIGNMENT:
       case AST_SHR_ASSIGNMENT:
       case AST_BITWISE_AND_ASSIGNMENT:
       case AST_BITWISE_OR_ASSIGNMENT:
       case AST_BITWISE_XOR_ASSIGNMENT:
       case AST_THROW_EXPRESSION:
            return -17;
       default:
            return INT_MIN;
    }
}


// We do not keep parentheses in C/C++ so we may need to restore some of them
static char operand_has_lower_priority(nodecl_t current_operator, nodecl_t operand)
{
    // It does not have known lower priority
    char rank_current = get_rank(current_operator);
    char rank_operand = get_rank(operand);
    return rank_operand < rank_current;
}

// Expressions

#define OPERATOR_TABLE \
    PREFIX_UNARY_EXPRESSION(plus, "+") \
    PREFIX_UNARY_EXPRESSION(neg, "-") \
    PREFIX_UNARY_EXPRESSION(logical_not, "!") \
    PREFIX_UNARY_EXPRESSION(bitwise_not, "~") \
    PREFIX_UNARY_EXPRESSION(derreference, "*") \
    PREFIX_UNARY_EXPRESSION(reference, "&") \
    PREFIX_UNARY_EXPRESSION(preincrement, "++") \
    PREFIX_UNARY_EXPRESSION(predecrement, "--") \
    POSTFIX_UNARY_EXPRESSION(postincrement, "++") \
    POSTFIX_UNARY_EXPRESSION(postdecrement, "--") \
    BINARY_EXPRESSION(add, " + ") \
    BINARY_EXPRESSION(mul, " * ") \
    BINARY_EXPRESSION(div, " / ") \
    BINARY_EXPRESSION(mod, " % ") \
    BINARY_EXPRESSION(minus, " - ") \
    BINARY_EXPRESSION(equal, " == ") \
    BINARY_EXPRESSION(different, " != ") \
    BINARY_EXPRESSION(lower_than, " < ") \
    BINARY_EXPRESSION(lower_or_equal_than, " <= ") \
    BINARY_EXPRESSION(greater_than, " > ") \
    BINARY_EXPRESSION(greater_or_equal_than, " >= ") \
    BINARY_EXPRESSION(logical_and, " && ") \
    BINARY_EXPRESSION(logical_or, " || ") \
    BINARY_EXPRESSION(bitwise_and, " & ") \
    BINARY_EXPRESSION(bitwise_or, " | ") \
    BINARY_EXPRESSION(bitwise_xor, " ^ ") \
    BINARY_EXPRESSION(shl, " << ") \
    BINARY_EXPRESSION(shr, " >> ") \
    BINARY_EXPRESSION(assignment, " = ") \
    BINARY_EXPRESSION(mul_assignment, " *= ") \
    BINARY_EXPRESSION(div_assignment, " /= ") \
    BINARY_EXPRESSION(add_assignment, " += ") \
    BINARY_EXPRESSION(sub_assignment, " -= ") \
    BINARY_EXPRESSION(shl_assignment, " <<= ") \
    BINARY_EXPRESSION(shr_assignment, " >>= ") \
    BINARY_EXPRESSION(bitwise_and_assignment, " &= ") \
    BINARY_EXPRESSION(bitwise_or_assignment, " |= ") \
    BINARY_EXPRESSION(bitwise_xor_assignment, " ^= ") \
    BINARY_EXPRESSION(mod_assignment, " %= ") \
    BINARY_EXPRESSION(class_member_access, ".") \
    BINARY_EXPRESSION(pointer_to_member, ".*") \
    BINARY_EXPRESSION(comma, ", ") 

#define PREFIX_UNARY_EXPRESSION(_name, _operand) \
    static void codegen_##_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) \
    { \
        nodecl_t rhs = nodecl_get_children(node, 0); \
        char needs_parentheses = operand_has_lower_priority(node, rhs); \
        fprintf(visitor->file, "%s", _operand); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        NODECL_WALK(visitor, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
    }
#define POSTFIX_UNARY_EXPRESSION(_name, _operand) \
    static void codegen_##_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) \
    { \
        nodecl_t rhs = nodecl_get_children(node, 0); \
        char needs_parentheses = operand_has_lower_priority(node, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        NODECL_WALK(visitor, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
        fprintf(visitor->file, "%s", _operand); \
    }
#define BINARY_EXPRESSION(_name, _operand) \
    static void codegen_##_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) \
    { \
        nodecl_t lhs = nodecl_get_children(node, 0); \
        nodecl_t rhs = nodecl_get_children(node, 1); \
        char needs_parentheses = operand_has_lower_priority(node, lhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        NODECL_WALK(visitor, lhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
        fprintf(visitor->file, "%s", _operand); \
        needs_parentheses = operand_has_lower_priority(node, lhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        NODECL_WALK(visitor, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
    }
OPERATOR_TABLE
#undef POSTFIX_UNARY_EXPRESSION
#undef PREFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION

static void codegen_parenthesized_expression(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t nest = nodecl_get_children(node, 0);
    fprintf(visitor->file, "(");
    NODECL_WALK(visitor, nest);
    fprintf(visitor->file, ")");
}

// Statements
static void codegen_catch_handler(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t name = nodecl_get_children(node, 0);
    nodecl_t statement = nodecl_get_children(node, 0);
    type_t* type = nodecl_get_type(node);

    indent(visitor);
    fprintf(visitor->file, "catch (");

    if (nodecl_is_null(name))
    {
        // FIXME: Is this always safe?
        print_type_str(type, visitor->current_sym->decl_context);
    }
    else
    {
        int old = visitor->in_condition;
        visitor->in_condition = 1;
        NODECL_WALK(visitor, name);
        visitor->in_condition = old;
    }

    fprintf(visitor->file, ")");

    NODECL_WALK(visitor, statement);
}

static void codegen_try_block(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t statement = nodecl_get_children(node, 0);
    nodecl_t catch_handlers = nodecl_get_children(node, 0);
    nodecl_t any_catch_handler = nodecl_get_children(node, 0);
    indent(visitor);
    fprintf(visitor->file, "try\n");

    NODECL_WALK(visitor, statement);

    NODECL_WALK(visitor, catch_handlers);

    if (!nodecl_is_null(any_catch_handler))
    {
        indent(visitor);
        fprintf(visitor->file, "catch (...)\n");
        NODECL_WALK(visitor, any_catch_handler);
    }
}

static void codegen_pragma_custom_construct(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t pragma_line = nodecl_get_children(node, 0);
    nodecl_t statement = nodecl_get_children(node, 1);

    indent(visitor);

    fprintf(visitor->file, "#pragma %s", nodecl_get_text(node));
    NODECL_WALK(visitor, pragma_line);
    NODECL_WALK(visitor, statement);
}

static void codegen_pragma_clause_arg(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "%s", nodecl_get_text(node));
}

static void codegen_pragma_custom_clause(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t arguments = nodecl_get_children(node, 0);

    fprintf(visitor->file, " %s", nodecl_get_text(node));

    if (!nodecl_is_null(arguments))
    {
        fprintf(visitor->file, "(");
        walk_list(visitor, arguments, ", ");
        fprintf(visitor->file, ")");
    }
}

static void codegen_pragma_custom_line(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t parameters = nodecl_get_children(node, 0);
    nodecl_t clauses = nodecl_get_children(node, 1);

    fprintf(visitor->file, " %s", nodecl_get_text(node));

    if (!nodecl_is_null(parameters))
    {
        fprintf(visitor->file, "(");
        walk_list(visitor, parameters, ", ");
        fprintf(visitor->file, ")");
    }

    NODECL_WALK(visitor, clauses);
}

static void codegen_pragma_custom_directive(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t pragma_line = nodecl_get_children(node, 0);

    indent(visitor);
    fprintf(visitor->file, "#pragma %s", nodecl_get_text(node));
    NODECL_WALK(visitor, pragma_line);
    fprintf(visitor->file, "\n");
}

static void codegen_return_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t expression = nodecl_get_children(node, 0);

    indent(visitor);
    fprintf(visitor->file, "return ");

    NODECL_WALK(visitor, expression);

    fprintf(visitor->file, ";\n");
}

static void codegen_goto_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* label_sym = nodecl_get_symbol(node);

    indent(visitor);
    fprintf(visitor->file, "goto %s;\n", label_sym->symbol_name);
}

static void codegen_break_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node UNUSED_PARAMETER)
{
    indent(visitor);
    fprintf(visitor->file, "break;\n");
}

static void codegen_continue_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node UNUSED_PARAMETER)
{
    indent(visitor);
    fprintf(visitor->file, "continue;\n");
}

static void codegen_case_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t expression = nodecl_get_children(node, 0);
    nodecl_t statement = nodecl_get_children(node, 1);

    indent(visitor);
    fprintf(visitor->file, "case ");
    NODECL_WALK(visitor, expression);
    fprintf(visitor->file, " :\n");

    NODECL_WALK(visitor, statement);
}

static void codegen_default_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t statement = nodecl_get_children(node, 0);

    indent(visitor);
    fprintf(visitor->file, "default :\n");

    NODECL_WALK(visitor, statement);
}

static void codegen_switch_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t expression = nodecl_get_children(node, 0);
    nodecl_t statement = nodecl_get_children(node, 1);

    indent(visitor);
    fprintf(visitor->file, "switch (");
    int old = visitor->in_condition;
    int old_indent = visitor->indent_level;
    visitor->indent_level = 0;
    visitor->in_condition = 1;
    NODECL_WALK(visitor, expression);
    visitor->indent_level = old_indent;
    visitor->in_condition = old;
    fprintf(visitor->file, ")\n");

    visitor->indent_level += 2;
    NODECL_WALK(visitor, statement);
    visitor->indent_level -= 2;
}

static void codegen_labeled_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* label_sym = nodecl_get_symbol(node);
    nodecl_t statement = nodecl_get_children(node, 0);

    indent(visitor);
    fprintf(visitor->file, "%s : ", label_sym->symbol_name);

    int old = visitor->indent_level;
    visitor->indent_level = 0;
    NODECL_WALK(visitor, statement);
    visitor->indent_level = old;
}

static void codegen_if_else_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t condition = nodecl_get_children(node, 0);
    nodecl_t then = nodecl_get_children(node, 1);
    nodecl_t _else = nodecl_get_children(node, 2);

    indent(visitor);

    fprintf(visitor->file, "if (");
    int old = visitor->in_condition;
    int old_indent = visitor->indent_level;
    visitor->indent_level = 0;
    visitor->in_condition = 1;
    NODECL_WALK(visitor, condition);
    visitor->indent_level = old_indent;
    visitor->in_condition = old;
    fprintf(visitor->file, ")\n");

    visitor->indent_level++;
    NODECL_WALK(visitor, then);
    visitor->indent_level--;

    if (!nodecl_is_null(_else))
    {
        indent(visitor);
        fprintf(visitor->file, "else\n");
        visitor->indent_level++;
        NODECL_WALK(visitor, _else);
        visitor->indent_level--;
    }
}

static void codegen_for_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t loop_control = nodecl_get_children(node, 0);
    nodecl_t statement = nodecl_get_children(node, 1);

    indent(visitor);
    fprintf(visitor->file, "for (");
    NODECL_WALK(visitor, loop_control);
    fprintf(visitor->file, ")\n");

    visitor->indent_level++;
    NODECL_WALK(visitor, statement);
    visitor->indent_level--;
}

static void codegen_loop_control(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t init = nodecl_get_children(node, 0);
    nodecl_t cond = nodecl_get_children(node, 1);
    nodecl_t next = nodecl_get_children(node, 2);

    int old = visitor->in_condition;
    visitor->in_condition = 1;
    NODECL_WALK(visitor, init);
    fprintf(visitor->file, "; ");
    NODECL_WALK(visitor, cond);
    fprintf(visitor->file, "; ");
    NODECL_WALK(visitor, next);
    visitor->in_condition = old;
}

static void codegen_empty_statement(nodecl_codegen_visitor_t* visitor, 
        nodecl_t node UNUSED_PARAMETER)
{
    indent(visitor);
    fprintf(visitor->file, ";\n");
}

static void codegen_do_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t statement = nodecl_get_children(node, 0);
    nodecl_t condition = nodecl_get_children(node, 1);

    indent(visitor);
    fprintf(visitor->file, "do\n");

    visitor->indent_level++;
    NODECL_WALK(visitor, statement);
    visitor->indent_level--;

    indent(visitor);
    fprintf(visitor->file, "while (");
    NODECL_WALK(visitor, condition);
    fprintf(visitor->file, ");\n");
}

static void codegen_while_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t condition = nodecl_get_children(node, 0);
    nodecl_t statement = nodecl_get_children(node, 1);

    indent(visitor);
    fprintf(visitor->file, "while (");
    int old = visitor->in_condition;
    int old_indent = visitor->indent_level;
    visitor->indent_level = 0;
    visitor->in_condition = 1;
    NODECL_WALK(visitor, condition);
    visitor->indent_level = old_indent;
    visitor->in_condition = old;
    fprintf(visitor->file, ")\n");

    visitor->indent_level++;
    NODECL_WALK(visitor, statement);
    visitor->indent_level--;
}

static void codegen_expression_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t expression = nodecl_get_children(node, 0);
    indent(visitor);
    NODECL_WALK(visitor, expression);
    fprintf(visitor->file, ";\n");
}

static void codegen_compound_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "{\n");
    visitor->indent_level++;
    nodecl_t statement_seq = nodecl_get_children(node, 0);

    scope_entry_t* scope_symbol = nodecl_get_symbol(node);
    ERROR_CONDITION(scope_symbol == NULL || scope_symbol->kind != SK_SCOPE, "Invalid scoping symbol", 0);

    define_local_entities_in_trees(visitor, statement_seq, scope_symbol->decl_context.current_scope);

    NODECL_WALK(visitor, statement_seq);

    visitor->indent_level--;
    indent(visitor);
    fprintf(visitor->file, "}\n");
}

// Explicit object initialization
static void codegen_object_init(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* entry = nodecl_get_symbol(node);
    // This should be CODEGEN_STATUS_DEFINED
    entry->entity_specs.codegen_status = CODEGEN_STATUS_NONE;

    define_symbol(visitor, entry);
}

// Function code
static void codegen_function_code(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t statement_seq = nodecl_get_children(node, 0);
    nodecl_t internal_functions = nodecl_get_children(node, 1);

    if (!nodecl_is_null(internal_functions))
    {
        internal_error("C/C++ does not have internal functions", 0);
    }

    if (!nodecl_is_null(nodecl_get_children(statement_seq, 0)))
    {
        internal_error("C/C++ functions only have one statement", 0);
    }

    nodecl_t statement = nodecl_get_children(statement_seq, 1);

    scope_entry_t* symbol = nodecl_get_symbol(node);
    ERROR_CONDITION(symbol == NULL || symbol->kind != SK_FUNCTION, "Invalid symbol", 0);

    visitor->current_sym = symbol;

    codegen_type_of_symbol(visitor, function_type_get_return_type(symbol->type_information), /* needs_def */ 1);

    int i;
    int num_params = function_type_get_num_parameters(symbol->type_information);
    if (function_type_get_has_ellipsis(symbol->type_information))
        num_params--;

    for (i = 0; i < num_params; i++)
    {
        codegen_type_of_symbol(visitor, function_type_get_parameter_type_num(symbol->type_information, i), /* needs_def */ 1);
    }

    define_nonlocal_entities_in_trees(visitor, statement);

    const char* argument_names[symbol->entity_specs.num_related_symbols + 1];
    memset(argument_names, 0, sizeof(argument_names));

    for (i = 0; i < symbol->entity_specs.num_related_symbols; i++)
    {
        if (symbol->entity_specs.related_symbols[i] != NULL)
        {
            argument_names[i] = symbol->entity_specs.related_symbols[i]->symbol_name;
        }
    }

    const char* decl_spec_seq = "";
    if (symbol->entity_specs.is_static
            && !symbol->entity_specs.is_member)
    {
        decl_spec_seq = strappend(decl_spec_seq, "static ");
    }
    if (symbol->entity_specs.is_extern)
    {
        decl_spec_seq = strappend(decl_spec_seq, "extern ");
    }
    if (symbol->entity_specs.is_inline)
    {
        decl_spec_seq = strappend(decl_spec_seq, "inline ");
    }

    const char* gcc_attributes = "";
    for (i = 0; i < MCXX_MAX_GCC_ATTRIBUTES_PER_SYMBOL; i++)
    {
        if (symbol->entity_specs.gcc_attributes[i].attribute_name != NULL)
        {
            if (symbol->entity_specs.gcc_attributes[i].expression_list == NULL)
            {
                char c[256];
                snprintf(c, 255, "__attribute__((%s)) ", symbol->entity_specs.gcc_attributes[i].attribute_name);
                c[255] = '\0';

                gcc_attributes = strappend(gcc_attributes, c);
            }
            else
            {
                internal_error("Attribute Not yet implemented", 0);
                // char c[256];
                // snprintf(c, 255, "__attribute__((%s(%s))) ", 
                //         symbol->entity_specs.gcc_attributes[i].attribute_name,
                //         codegen_expression_list(visitor, symbol->entity_specs.gcc_attributes[i].expression_list, symbol->decl_context)
                //             );
                // c[255] = '\0';

                // gcc_attributes = strappend(gcc_attributes, c);
            }
        }
    }

    const char* declarator = get_declaration_string_internal(symbol->type_information,
            symbol->decl_context,
            symbol->symbol_name,
            /* initializer */ "",
            /* semicolon */ 0,
            /* num_parameter_names */ symbol->entity_specs.num_related_symbols,
            /* parameter_names */ argument_names,
            /* is_parameter */ 0);

    const char* exception_spec = "";
    CXX_LANGUAGE()
    {
        if (!symbol->entity_specs.any_exception)
        {
            exception_spec = " throw (";
            for (i = 0; i < symbol->entity_specs.num_exceptions; i++)
            {
                if (i != 0)
                {
                    exception_spec = strappend(exception_spec, ", ");
                }
                exception_spec = strappend(exception_spec, print_type_str(symbol->entity_specs.exceptions[i], symbol->decl_context));
            }
            exception_spec = strappend(exception_spec, " )");
        }
    }

    indent(visitor);
    fprintf(visitor->file, "%s%s%s%s\n", decl_spec_seq, gcc_attributes, declarator, exception_spec);

    NODECL_WALK(visitor, statement);
}

// Top level
static void codegen_top_level(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t list = nodecl_get_children(node, 0);
    NODECL_WALK(visitor, list);
}

typedef void (*codegen_visitor_fun_t)(nodecl_codegen_visitor_t* visitor, nodecl_t node);
typedef void (*nodecl_visitor_fun_t)(nodecl_external_visitor_t* visitor, nodecl_t node);

// This is safer than using the macro directly as it will warn us against wrong types
// while the macro does not
static inline nodecl_visitor_fun_t codegen_visitor_fun(codegen_visitor_fun_t p)
{
    return NODECL_VISITOR_FUN(p);
}

// External interface
void c_cxx_codegen_translation_unit(FILE *f, AST a, scope_link_t* sl UNUSED_PARAMETER)
{
    nodecl_codegen_visitor_t codegen_visitor;
    memset(&codegen_visitor, 0, sizeof(codegen_visitor));
    
    nodecl_init_walker((nodecl_external_visitor_t*)&codegen_visitor, not_implemented_yet);

    codegen_visitor.file = f;
    codegen_visitor.indent_level = 0;
    codegen_visitor.current_sym = NULL;

    NODECL_VISITOR(&codegen_visitor)->visit_nodecl_top_level = codegen_visitor_fun(codegen_top_level);
    NODECL_VISITOR(&codegen_visitor)->visit_function_code = codegen_visitor_fun(codegen_function_code);
    NODECL_VISITOR(&codegen_visitor)->visit_compound_statement = codegen_visitor_fun(codegen_compound_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_expression_statement = codegen_visitor_fun(codegen_expression_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_assignment = codegen_visitor_fun(codegen_assignment);
    NODECL_VISITOR(&codegen_visitor)->visit_add = codegen_visitor_fun(codegen_add);
    NODECL_VISITOR(&codegen_visitor)->visit_symbol = codegen_visitor_fun(codegen_symbol);
    NODECL_VISITOR(&codegen_visitor)->visit_integer_literal = codegen_visitor_fun(codegen_integer_literal);
    NODECL_VISITOR(&codegen_visitor)->visit_object_init = codegen_visitor_fun(codegen_object_init);
    NODECL_VISITOR(&codegen_visitor)->visit_if_else_statement = codegen_visitor_fun(codegen_if_else_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_for_statement = codegen_visitor_fun(codegen_for_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_loop_control = codegen_visitor_fun(codegen_loop_control);
    NODECL_VISITOR(&codegen_visitor)->visit_while_statement = codegen_visitor_fun(codegen_while_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_do_statement = codegen_visitor_fun(codegen_do_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_empty_statement = codegen_visitor_fun(codegen_empty_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_default_statement = codegen_visitor_fun(codegen_default_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_switch_statement = codegen_visitor_fun(codegen_switch_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_labeled_statement = codegen_visitor_fun(codegen_labeled_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_break_statement = codegen_visitor_fun(codegen_break_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_continue_statement = codegen_visitor_fun(codegen_continue_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_case_statement = codegen_visitor_fun(codegen_case_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_return_statement = codegen_visitor_fun(codegen_return_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_goto_statement = codegen_visitor_fun(codegen_goto_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_custom_directive = codegen_visitor_fun(codegen_pragma_custom_directive);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_custom_construct = codegen_visitor_fun(codegen_pragma_custom_construct);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_custom_clause = codegen_visitor_fun(codegen_pragma_custom_clause);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_clause_arg = codegen_visitor_fun(codegen_pragma_clause_arg);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_custom_line = codegen_visitor_fun(codegen_pragma_custom_line);
    NODECL_VISITOR(&codegen_visitor)->visit_try_block = codegen_visitor_fun(codegen_try_block);
    NODECL_VISITOR(&codegen_visitor)->visit_catch_handler = codegen_visitor_fun(codegen_catch_handler);
    NODECL_VISITOR(&codegen_visitor)->visit_parenthesized_expression = codegen_visitor_fun(codegen_parenthesized_expression);
    // All binary infix, unary prefix and unary postfix are here, look for the definition of OPERATOR_TABLE above
#define PREFIX_UNARY_EXPRESSION(_name, _) \
    NODECL_VISITOR(&codegen_visitor)->visit_##_name = codegen_visitor_fun(codegen_##_name);
#define POSTFIX_UNARY_EXPRESSION(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
#define BINARY_EXPRESSION(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
    OPERATOR_TABLE
#undef PREFIX_UNARY_EXPRESSION
#undef POSTFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION


    nodecl_walk((nodecl_external_visitor_t*)&codegen_visitor, _nodecl_wrap(a));

    run_clear_list();
}

#undef OPERATOR_TABLE
