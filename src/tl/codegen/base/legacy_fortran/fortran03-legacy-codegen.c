#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "fortran03-legacy-codegen.h"
#include "fortran03-typeutils.h"
#include "fortran03-buildscope.h"
#include "cxx-nodecl-visitor.h"
#include "cxx-driver-decls.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-entrylist.h"
#include <string.h>

#ifdef HAVE_QUADMATH_H
#include <quadmath.h>
#endif

typedef
struct nodecl_codegen_visitor_tag
{
    // Base visitor
    nodecl_external_visitor_t _base_visitor;

    // Codegen
    FILE *file;
    int indent_level;
    scope_entry_t* current_sym;

    scope_entry_t* current_module;

    // Ommit interface if we are already in one interface
    char in_interface;

    // Print loop control using colons instead of commas
    char in_forall;
} nodecl_codegen_visitor_t;

typedef void (*codegen_visitor_fun_t)(nodecl_codegen_visitor_t* visitor, nodecl_t node);
typedef void (*nodecl_visitor_fun_t)(nodecl_external_visitor_t* visitor, nodecl_t node);

static void declare_symbols_rec(nodecl_codegen_visitor_t* visitor, nodecl_t node);
static void declare_symbol(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry);

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

// FIXME - Make this spacing configurable
#define INDENT_BLANK "  "

static void indent(nodecl_codegen_visitor_t* v)
{
    int i;
    for (i = 0; i < v->indent_level; i++)
    {
        fprintf(v->file, INDENT_BLANK);
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

static void reverse_codegen_comma_separated_list(nodecl_codegen_visitor_t *visitor, nodecl_t node)
{
    if (nodecl_is_null(node))
        return;

    AST list = nodecl_get_ast(node);

    ERROR_CONDITION(ASTType(list) != AST_NODE_LIST, "Invalid node kind", 0);

    AST it = list;

    while (it != NULL)
    {
        AST current = ASTSon1(it);

        // If we are not the last
        if (it != list)
        {
            fprintf(visitor->file, ", ");
        }

        codegen_walk(visitor, _nodecl_wrap(current));

        it = ASTSon0(it);
    }
}

// Codegen
static void not_implemented_yet(nodecl_external_visitor_t* visitor UNUSED_PARAMETER, nodecl_t node)
{
    internal_error("Error -> Uninmplemented node! '%s' at %s\n", ast_print_node_type(ASTType(nodecl_get_ast(node))), 
            nodecl_get_locus(node));
}

static void codegen_module_header(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry)
{
    fprintf(visitor->file, "MODULE %s\n", entry->symbol_name);

    int i, num_components = entry->entity_specs.num_related_symbols;

    visitor->indent_level += 2;

    scope_entry_t* previous_sym = visitor->current_sym;
    visitor->current_sym = entry;
    for (i = 0; i < num_components; i++)
    {
        scope_entry_t* sym = entry->entity_specs.related_symbols[i];
        if (sym->kind != SK_FUNCTION
                || sym->entity_specs.is_generic_spec
                || sym->entity_specs.in_module != visitor->current_module)
        {
            declare_symbol(visitor, sym);
        }
    }
    visitor->current_sym = previous_sym;
    visitor->indent_level -= 1;

    indent(visitor);
    fprintf(visitor->file, "CONTAINS\n");

    visitor->indent_level += 1;

}

static void codegen_module_footer(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry)
{
    visitor->indent_level -= 2;
    fprintf(visitor->file, "END MODULE %s\n", entry->symbol_name);
}

static void codegen_object_init(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* entry = nodecl_get_symbol(node);

    switch (entry->kind)
    {
        // If the FE generates this it means we found a module with no functions
        case SK_MODULE:
            {
                // This is needed when a module (which had no functions) is
                // extended with new functions, the tree is scanned first for
                // functions, but this node is left untouched, so just do
                // nothing if found after the whole module was already printed
                if (entry->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
                    return;
                entry->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;

                scope_entry_t* old_module = visitor->current_module;
                visitor->current_module = entry;
                codegen_module_header(visitor, entry);
                codegen_module_footer(visitor, entry);
                visitor->current_module = old_module;
                break;
            }
        default:
            {
                internal_error("Unexpected symbol %s\n", symbol_kind_name(entry));
                break;
            }
    }
}

typedef
struct nodecl_codegen_pre_visitor_tag
{
    // Base visitor
    nodecl_external_visitor_t _base_visitor;

    int num_modules;
    scope_entry_t** modules;
} nodecl_codegen_pre_visitor_t;

static void pre_visit_function_code(nodecl_external_visitor_t* visitor, nodecl_t node)
{
    nodecl_codegen_pre_visitor_t *pre_visitor = (nodecl_codegen_pre_visitor_t*)visitor;

    scope_entry_t* entry = nodecl_get_symbol(node);

    if (entry->entity_specs.in_module != NULL)
    {
        P_LIST_ADD_ONCE(pre_visitor->modules,
                pre_visitor->num_modules,
                entry->entity_specs.in_module);
    }
}

static void codegen_top_level(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t list = nodecl_get_child(node, 0);

    nodecl_codegen_pre_visitor_t pre_visitor;
    memset(&pre_visitor, 0, sizeof(pre_visitor));

    nodecl_init_walker((nodecl_external_visitor_t*)&pre_visitor, NULL);

    NODECL_VISITOR(&pre_visitor)->visit_function_code = pre_visit_function_code;
    nodecl_walk((nodecl_external_visitor_t*)&pre_visitor, list);

    int i;
    for (i = 0; i < pre_visitor.num_modules; i++)
    {
        scope_entry_t* old_module = visitor->current_module;

        scope_entry_t* current_module = pre_visitor.modules[i];

        current_module->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;

        visitor->current_module = current_module;

        codegen_module_header(visitor, current_module);
        codegen_walk(visitor, list);
        codegen_module_footer(visitor, current_module);

        visitor->current_module = old_module;
    }

    codegen_walk(visitor, list);
}

static void codegen_type(nodecl_codegen_visitor_t* visitor, 
        type_t* t, const char** type_specifier, const char **array_specifier,
        char is_dummy)
{
    (*type_specifier) = "";

    t = no_ref(t);

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

        array_spec_list[array_spec_idx].lower = array_type_get_array_lower_bound(t);
        if (nodecl_is_constant(array_spec_list[array_spec_idx].lower))
        {
            array_spec_list[array_spec_idx].lower = 
                const_value_to_nodecl(nodecl_get_constant(array_spec_list[array_spec_idx].lower));
        }
        else
        {
            declare_symbols_rec(visitor, array_spec_list[array_spec_idx].lower);
        }

        if (!array_type_is_unknown_size(t))
        {
            array_spec_list[array_spec_idx].upper = array_type_get_array_upper_bound(t);

            if (nodecl_is_constant(array_spec_list[array_spec_idx].upper))
            {
                array_spec_list[array_spec_idx].upper = 
                    const_value_to_nodecl(nodecl_get_constant(array_spec_list[array_spec_idx].upper));
            }
            else
            {
                declare_symbols_rec(visitor, array_spec_list[array_spec_idx].upper);
            }
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
            || is_floating_type(t)
            || is_double_type(t)
            || is_complex_type(t))
    {
        const char* type_name = NULL;
        char c[128] = { 0 };

        if (is_bool_type(t))
        {
            type_name = "LOGICAL";
        }
        else if (is_integer_type(t))
        {
            type_name = "INTEGER";
        }
        else if (is_floating_type(t))
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

        size_t size = type_get_size(t);
        if (is_floating_type(t))
        {
            // KIND of floats is their size in byes (using the bits as in IEEE754) 
            size = (floating_type_get_info(t)->bits) / 8;
        }
        else if (is_complex_type(t))
        {
            // KIND of a complex is the KIND of its component type
            type_t* f = complex_type_get_base_type(t);
            size = (floating_type_get_info(f)->bits) / 8;
        }

        snprintf(c, 127, "%s(%zd)", type_name, size);

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
        char c[128] = { 0 };
        snprintf(c, 127, "CHARACTER(LEN=%s)",
                array_type_is_unknown_size(t) ? "*" : _fortran_codegen_to_str(array_type_get_array_upper_bound(t)));
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

        char explicitly_shaped = 0;

        while (array_spec_idx <= (MCXX_MAX_ARRAY_SPECIFIER - 1))
        {
            if (!array_spec_list[array_spec_idx].is_undefined)
            {
                (*array_specifier) = strappend((*array_specifier), _fortran_codegen_to_str(array_spec_list[array_spec_idx].lower));
                (*array_specifier) = strappend((*array_specifier), ":");
                (*array_specifier) = strappend((*array_specifier), _fortran_codegen_to_str(array_spec_list[array_spec_idx].upper));

                explicitly_shaped = 1;
            }
            else
            {
                if (is_dummy)
                {
                    if (!nodecl_is_null(array_spec_list[array_spec_idx].lower))
                    {
                        (*array_specifier) = strappend((*array_specifier), _fortran_codegen_to_str(array_spec_list[array_spec_idx].lower));
                        (*array_specifier) = strappend((*array_specifier), ":");
                    }
                    else
                    {
                        if (!explicitly_shaped)
                        {
                            (*array_specifier) = strappend((*array_specifier), ":");
                        }
                        else
                        {
                            (*array_specifier) = strappend((*array_specifier), "*");
                        }
                    }
                }
                else
                {
                    (*array_specifier) = strappend((*array_specifier), ":");
                }
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

static void codegen_procedure_declaration_header(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry)
{
    char is_function = (function_type_get_return_type(entry->type_information) != NULL);
    indent(visitor);
    if (entry->entity_specs.is_recursive)
    {
        fprintf(visitor->file, "RECURSIVE ");
    }
    if (entry->entity_specs.is_pure)
    {
        fprintf(visitor->file, "PURE ");
    }
    if (entry->entity_specs.is_elemental)
    {
        fprintf(visitor->file, "ELEMENTAL ");
    }

    fprintf(visitor->file, "%s %s(", 
            is_function ? "FUNCTION" : "SUBROUTINE",
            entry->symbol_name);
    int i, num_params = entry->entity_specs.num_related_symbols;
    // In a function the last one is the result
    if (is_function)
    {
        ERROR_CONDITION(num_params <= 0, "Invalid number of parameters %d in function", num_params);
        num_params--;
    }
    for (i = 0; i < num_params; i++)
    {
        if (i != 0)
        {
            fprintf(visitor->file, ", ");
        }
        fprintf(visitor->file, "%s", entry->entity_specs.related_symbols[i]->symbol_name);
    }
    fprintf(visitor->file, ")");
    if (is_function)
    {
        scope_entry_t* result = entry->entity_specs.related_symbols[num_params];
        ERROR_CONDITION(!result->entity_specs.is_result, "This is not the result symbol!", 0);
        if (strcmp(result->symbol_name, entry->symbol_name) != 0)
        {
            fprintf(visitor->file, " RESULT(%s)", result->symbol_name);
        }
    }
    fprintf(visitor->file, "\n");
}

static void codegen_procedure_declaration_footer(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry)
{
    char is_function = (function_type_get_return_type(entry->type_information) != NULL);

    indent(visitor);
    fprintf(visitor->file, "END %s %s\n", (is_function ? "FUNCTION" : "SUBROUTINE"), entry->symbol_name);
}

static const char* get_generic_specifier_str(const char *c)
{
    const char* const op_prefix = ".operator.";
    if (strlen(c) > strlen(op_prefix))
    {
        if (strncmp(c, op_prefix, strlen(op_prefix)) == 0)
        {
            c += strlen(op_prefix);

            // .operator.=
            if (*c == '='
                    && *(c + 1) == '\0')
            {
                return uniquestr("ASSIGNMENT(=)");
            }
            // .operator.XXX
            else
            {
                char t[256];
                snprintf(t, 255, "OPERATOR(%s)", c);
                t[255] = '\0';

                return uniquestr(t);
            }
        }
    }
    return c;
}

static void declare_symbol(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry)
{
    ERROR_CONDITION(entry == NULL, "Invalid symbol to declare", 0);

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

                codegen_type(visitor, entry->type_information, &type_spec, &array_specifier,
                        /* is_dummy */ entry->entity_specs.is_parameter);

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
                if (entry->entity_specs.is_parameter)
                {
                    switch (entry->entity_specs.intent_kind)
                    {
                        case INTENT_IN:
                            {
                                attribute_list = strappend(attribute_list, ", INTENT(IN)");
                                break;
                            }
                        case INTENT_OUT:
                            {
                                attribute_list = strappend(attribute_list, ", INTENT(OUT)");
                                break;
                            }
                        case INTENT_INOUT:
                            {
                                attribute_list = strappend(attribute_list, ", INTENT(INOUT)");
                                break;
                            }
                        default:
                            {
                            }
                    }
                }

                if (!nodecl_is_null(entry->value))
                {
                    declare_symbols_rec(visitor, entry->value);
                    if (is_pointer_type(no_ref(entry->type_information)))
                    {
                        initializer = strappend(" => ", _fortran_codegen_to_str(entry->value));
                    }
                    else
                    {
                        initializer = strappend(" = ", _fortran_codegen_to_str(entry->value));
                    }
                }

                indent(visitor);
                fprintf(visitor->file, "%s%s :: %s%s%s\n", 
                        type_spec,
                        attribute_list,
                        entry->symbol_name,
                        array_specifier,
                        initializer);

                if (entry->entity_specs.is_in_common)
                {
                    declare_symbol(visitor, entry->entity_specs.in_common);
                }

                if (entry->entity_specs.is_cray_pointee)
                {
                    declare_symbol(visitor, entry->entity_specs.cray_pointer);
                    indent(visitor);
                    fprintf(visitor->file, "POINTER (%s, %s)\n", 
                            entry->entity_specs.cray_pointer->symbol_name,
                            entry->symbol_name);
                }
                break;
            }
        case SK_NAMELIST:
        case SK_COMMON:
            {
                int i, num_symbols = entry->entity_specs.num_related_symbols;
                for (i = 0; i < num_symbols; i++)
                {
                    scope_entry_t* common_member = entry->entity_specs.related_symbols[i];
                    declare_symbol(visitor, common_member);
                }

                const char* keyword = NULL;
                const char* symbol_name = NULL;
                if (entry->kind == SK_NAMELIST)
                {
                    keyword = "NAMELIST";
                    symbol_name = entry->symbol_name;
                }
                else // COMMON
                {
                    keyword = "COMMON";
                    // Ignore ".common."
                    symbol_name = entry->symbol_name + strlen(".common.");
                }

                indent(visitor);
                fprintf(visitor->file, "%s / %s / ", keyword, symbol_name);
                for (i = 0; i < num_symbols; i++)
                {
                    scope_entry_t* common_member = entry->entity_specs.related_symbols[i];
                    if (i != 0)
                        fprintf(visitor->file, ", ");

                    fprintf(visitor->file, "%s", common_member->symbol_name);
                }
                fprintf(visitor->file, "\n");

                if (entry->kind == SK_NAMELIST
                        && entry->entity_specs.is_static)
                {
                    indent(visitor);
                    fprintf(visitor->file, "SAVE /%s/\n", entry->symbol_name);
                }
                break;
            }
        case SK_FUNCTION:
            {
                if (entry->entity_specs.is_generic_spec)
                {
                    indent(visitor);
                    fprintf(visitor->file, "INTERFACE %s\n", 
                            get_generic_specifier_str(entry->symbol_name));
                    int i, num_items = entry->entity_specs.num_related_symbols;
                    visitor->indent_level++;
                    for (i = 0; i < num_items; i++)
                    {
                        scope_entry_t* iface = entry->entity_specs.related_symbols[i];

                        if (visitor->current_module != NULL
                               && (iface->entity_specs.in_module == visitor->current_module))
                        {
                            indent(visitor);
                            fprintf(visitor->file, "MODULE PROCEDURE %s\n", iface->symbol_name);
                        }
                        else
                        {
                            char old_in_interface = visitor->in_interface;
                            visitor->in_interface = 1;
                            declare_symbol(visitor, iface);
                            visitor->in_interface = old_in_interface;
                        }
                    }
                    visitor->indent_level--;
                    indent(visitor);
                    fprintf(visitor->file, "END INTERFACE %s\n", get_generic_specifier_str(entry->symbol_name));
                }
                else if (function_type_get_lacking_prototype(entry->type_information))
                {
                    indent(visitor);

                    if (function_type_get_return_type(entry->type_information) != NULL)
                    {
                        const char* type_spec = NULL;
                        const char* array_specifier = NULL;
                        codegen_type(visitor, function_type_get_return_type(entry->type_information), 
                                &type_spec, &array_specifier, /* is_dummy */ 0);
                        fprintf(visitor->file, "%s, EXTERNAL :: %s\n", type_spec, entry->symbol_name);
                    }
                    else
                    {
                        fprintf(visitor->file, "EXTERNAL :: %s\n", entry->symbol_name);
                    }
                }
                // Statement functions
                else if (entry->entity_specs.is_stmt_function)
                {
                    int i, num_params = entry->entity_specs.num_related_symbols;

                    for (i = 0; i < num_params; i++)
                    {
                        scope_entry_t* sym = entry->entity_specs.related_symbols[i];
                        declare_symbol(visitor, sym);
                    }

                    indent(visitor);
                    fprintf(visitor->file, "%s(", entry->symbol_name);
                    for (i = 0; i < num_params; i++)
                    {
                        scope_entry_t* dummy = entry->entity_specs.related_symbols[i];
                        if (dummy->entity_specs.is_result)
                            continue;

                        if (i != 0)
                            fprintf(visitor->file, ", ");
                        fprintf(visitor->file, "%s", dummy->symbol_name);
                    }

                    fprintf(visitor->file, ") = ");
                    codegen_walk(visitor, entry->value);
                    fprintf(visitor->file, "\n");
                }
                else
                {
                    if (!visitor->in_interface)
                    {
                        indent(visitor);
                        fprintf(visitor->file, "INTERFACE\n");
                        visitor->indent_level++;
                    }
                    codegen_procedure_declaration_header(visitor, entry);

                    scope_entry_t* old = visitor->current_sym;
                    visitor->current_sym = entry;
                    int i, num_params = entry->entity_specs.num_related_symbols;
                    visitor->indent_level++;
                    indent(visitor);
                    fprintf(visitor->file, "IMPLICIT NONE\n");
                    for (i = 0; i < num_params; i++)
                    {
                        declare_symbol(visitor, entry->entity_specs.related_symbols[i]);
                    }
                    visitor->indent_level--;
                    visitor->current_sym = old;

                    codegen_procedure_declaration_footer(visitor, entry);

                    if (!visitor->in_interface)
                    {
                        visitor->indent_level--;
                        indent(visitor);
                        fprintf(visitor->file, "END INTERFACE\n");
                    }
                }
                break;
            }
        case SK_CLASS:
            {
                indent(visitor);
                fprintf(visitor->file, "TYPE :: %s\n", entry->symbol_name);

                scope_entry_t* old_sym = visitor->current_sym;
                visitor->current_sym = entry;
                visitor->indent_level++;
                scope_entry_list_t* members = class_type_get_nonstatic_data_members(entry->type_information);
                scope_entry_list_iterator_t* it = NULL;
                for (it = entry_list_iterator_begin(members);
                        !entry_list_iterator_end(it);
                        entry_list_iterator_next(it))
                {
                    declare_symbol(visitor, 
                            entry_list_iterator_current(it));
                }
                entry_list_iterator_free(it);
                entry_list_free(members);
                visitor->indent_level--;
                visitor->current_sym = old_sym;

                indent(visitor);
                fprintf(visitor->file, "END TYPE %s\n", entry->symbol_name);
                break;
            }
        case SK_LABEL:
            {
                // This is basically for FORMAT labels
                if (!nodecl_is_null(entry->value))
                {
                    indent(visitor);
                    fprintf(visitor->file, "%s FORMAT", entry->symbol_name);

                    int old_indent_level = visitor->indent_level;
                    visitor->indent_level = 0;
                    codegen_walk(visitor, entry->value);
                    visitor->indent_level = old_indent_level;

                    fprintf(visitor->file, "\n");
                }
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
    if (entry != NULL
            && !entry->entity_specs.from_module)
    {
        declare_symbol(visitor, entry);
    }
}

static void declare_everything_needed(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    declare_symbols_rec(visitor, node);
}

static void codegen_use_statement(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry)
{
    ERROR_CONDITION(!entry->entity_specs.from_module, "Symbol '%s' must be from module\n", entry->symbol_name);

    if (entry->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
        return;

    indent(visitor);
    if (!entry->entity_specs.is_renamed)
    {
        fprintf(visitor->file, "USE %s, ONLY: %s\n", 
                entry->entity_specs.from_module->symbol_name,
                entry->symbol_name);
    }
    else
    {
        fprintf(visitor->file, "USE %s, ONLY: %s => %s\n", 
                entry->entity_specs.from_module->symbol_name,
                entry->symbol_name,
                entry->entity_specs.alias_to->symbol_name);
    }

    entry->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
}

static void declare_symbols_from_modules_rec(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    if (nodecl_is_null(node))
        return;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        declare_symbols_from_modules_rec(visitor, nodecl_get_child(node, i));
    }

    scope_entry_t* entry = nodecl_get_symbol(node);
    if (entry != NULL)
    {
        if (is_class_type(entry->type_information))
        {
            scope_entry_t* class_entry = named_type_get_symbol(entry->type_information);
            if (class_entry->entity_specs.from_module)
            {
                codegen_use_statement(visitor, class_entry);
            }
        }
        if (entry->entity_specs.from_module)
        {
            codegen_use_statement(visitor, entry);
        }
    }
}

static void declare_use_statements(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    declare_symbols_from_modules_rec(visitor, node);
}

static void codegen_procedure(nodecl_codegen_visitor_t* visitor, scope_entry_t* entry, nodecl_t statement_seq, nodecl_t internal_subprograms)
{
    visitor->indent_level++;

    declare_use_statements(visitor, statement_seq);

    indent(visitor);
    fprintf(visitor->file, "IMPLICIT NONE\n");

    if (entry->kind == SK_FUNCTION)
    {
        int i, num_params = entry->entity_specs.num_related_symbols;
        for (i = 0; i < num_params; i++)
        {
            declare_symbol(visitor, entry->entity_specs.related_symbols[i]);
        }
    }

    declare_everything_needed(visitor, statement_seq);

    scope_entry_t* data_symbol = get_data_symbol_info(entry->decl_context);
    if (data_symbol != NULL)
    {
        codegen_walk(visitor, data_symbol->value);
    }
    scope_entry_t* equivalence_symbol = get_equivalence_symbol_info(entry->decl_context);
    if (equivalence_symbol != NULL)
    {
        codegen_walk(visitor, equivalence_symbol->value);
    }

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
}

static void codegen_fortran_io_spec(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "%s = ", nodecl_get_text(node));

    nodecl_t nodecl_value = nodecl_get_child(node, 0);
    codegen_walk(visitor, nodecl_value);
}

static void codegen_print_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "PRINT ");

    nodecl_t nodecl_format = nodecl_get_child(node, 0);
    nodecl_t nodecl_io_spec = nodecl_get_child(node, 1);

    codegen_walk(visitor, nodecl_format);

    if (!nodecl_is_null(nodecl_io_spec))
    {
        fprintf(visitor->file, ", ");
        codegen_comma_separated_list(visitor, nodecl_io_spec);
    }

    fprintf(visitor->file, "\n");
}

static void codegen_write_or_read_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node, const char* keyword)
{
    indent(visitor);
    fprintf(visitor->file, "%s (", keyword);

    nodecl_t nodecl_io_spec_list = nodecl_get_child(node, 0);
    codegen_comma_separated_list(visitor, nodecl_io_spec_list);

    fprintf(visitor->file, ") ");

    nodecl_t nodecl_io_item_list = nodecl_get_child(node, 1);
    codegen_comma_separated_list(visitor, nodecl_io_item_list);

    fprintf(visitor->file, "\n");
}

static void codegen_write_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_write_or_read_statement(visitor, node, "WRITE");
}

static void codegen_read_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_write_or_read_statement(visitor, node, "READ");
}

static void codegen_stop_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "STOP");

    nodecl_t nodecl_code = nodecl_get_child(node, 0);

    if (!nodecl_is_null(nodecl_code))
    {
        fprintf(visitor->file, " ");
        codegen_walk(visitor, nodecl_code);
    }
    fprintf(visitor->file, "\n");
}

static void codegen_computed_goto_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "GOTO (");
    codegen_comma_separated_list(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, ") ");
    codegen_walk(visitor, nodecl_get_child(node, 1));
    fprintf(visitor->file, "\n");
}

static void codegen_allocation_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node, const char* keyword)
{
    indent(visitor);
    fprintf(visitor->file, "%s (", keyword);
    codegen_comma_separated_list(visitor, nodecl_get_child(node, 0));

    nodecl_t node_fortran_io_spec = nodecl_get_child(node, 1);
    if (!nodecl_is_null(node_fortran_io_spec))
    {
        fprintf(visitor->file, ", ");
        codegen_comma_separated_list(visitor, node_fortran_io_spec);
    }

    fprintf(visitor->file, ")\n");
}

static void codegen_allocate_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_allocation_statement(visitor, node, "ALLOCATE");
}

static void codegen_deallocate_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_allocation_statement(visitor, node, "DEALLOCATE");
}

static void codegen_nullify_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "NULLIFY (");
    codegen_comma_separated_list(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, ")\n");
}

static void codegen_arithmetic_if_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "IF (");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, ") ");
    codegen_walk(visitor, nodecl_get_child(node, 1));
    fprintf(visitor->file, ", ");
    codegen_walk(visitor, nodecl_get_child(node, 2));
    fprintf(visitor->file, ", ");
    codegen_walk(visitor, nodecl_get_child(node, 3));
    fprintf(visitor->file, "\n");
}

static void codegen_assigned_goto_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "GOTO ");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, " (");
    codegen_comma_separated_list(visitor, nodecl_get_child(node, 1));
    fprintf(visitor->file, ")\n");
}

static void codegen_label_assign_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "ASSIGN ");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, " TO ");
    codegen_walk(visitor, nodecl_get_child(node, 1));
    fprintf(visitor->file, "\n");
}

static void codegen_io_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "%s ", nodecl_get_text(node));

    nodecl_t nodecl_io_spec = nodecl_get_child(node, 0);
    if (!nodecl_is_null(nodecl_io_spec))
    {
        fprintf(visitor->file, "(");
        codegen_comma_separated_list(visitor, nodecl_io_spec);
        fprintf(visitor->file, ")");
    }

    nodecl_t nodecl_io_items = nodecl_get_child(node, 0);
    if (!nodecl_is_null(nodecl_io_items))
    {
        if (!nodecl_is_null(nodecl_io_spec))
        {
            fprintf(visitor->file, " ");
        }
        codegen_comma_separated_list(visitor, nodecl_io_items);
    }
    fprintf(visitor->file, "\n");
}

static void codegen_open_close_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node, const char* keyword)
{
    indent(visitor);
    fprintf(visitor->file, "%s (", keyword);
    nodecl_t nodecl_io_spec = nodecl_get_child(node, 0);
    if (!nodecl_is_null(nodecl_io_spec))
    {
        codegen_comma_separated_list(visitor, nodecl_io_spec);
    }
    fprintf(visitor->file, ")\n");
}

static void codegen_open_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_open_close_statement(visitor, node, "OPEN");
}

static void codegen_close_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_open_close_statement(visitor, node, "CLOSE");
}

static void codegen_empty_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node UNUSED_PARAMETER)
{
    indent(visitor);
    fprintf(visitor->file, "CONTINUE\n");
}

static void codegen_if_else_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node UNUSED_PARAMETER)
{
    nodecl_t nodecl_condition = nodecl_get_child(node, 0);
    nodecl_t nodecl_then = nodecl_get_child(node, 1);
    nodecl_t nodecl_else = nodecl_get_child(node, 2);

    indent(visitor);
    fprintf(visitor->file, "IF (");
    codegen_walk(visitor, nodecl_condition);
    fprintf(visitor->file, ")");

    fprintf(visitor->file, " THEN\n");

    visitor->indent_level++;
    codegen_walk(visitor, nodecl_then);
    visitor->indent_level--;

    if (!nodecl_is_null(nodecl_else))
    {
        indent(visitor);
        fprintf(visitor->file, "ELSE\n");

        visitor->indent_level++;
        codegen_walk(visitor, nodecl_else);
        visitor->indent_level--;
    }

    indent(visitor);
    fprintf(visitor->file, "END IF\n");
}

static void codegen_return_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node UNUSED_PARAMETER)
{
    // Note that for functions (not subroutines) we actually 'return F'
    // however what it must be printed is just 'RETURN'
    indent(visitor);
    fprintf(visitor->file, "RETURN\n");
}

static void codegen_labeled_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* label_sym = nodecl_get_symbol(node);

    indent(visitor);
    fprintf(visitor->file, "%s ", label_sym->symbol_name);

    int old_indent_level = visitor->indent_level;
    visitor->indent_level = 0;
    nodecl_t nodecl_statement = nodecl_get_child(node, 0);
    codegen_walk(visitor, nodecl_statement);
    visitor->indent_level = old_indent_level;
}

static void codegen_goto_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* label = nodecl_get_symbol(node);
    indent(visitor);
    fprintf(visitor->file, "GOTO %s\n", label->symbol_name);
}

static void codegen_loop_control(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t init = nodecl_get_child(node, 0);
    nodecl_t cond = nodecl_get_child(node, 1);
    nodecl_t next = nodecl_get_child(node, 2);

    const char* separator = ", ";

    // Use a colon for ':' in FORALL
    if (visitor->in_forall)
    {
        separator = ":";
    }

    if (!nodecl_is_null(init))
    {
        // Needed for DO but not for FORALL which uses a (
        if (!visitor->in_forall)
        {
            fprintf(visitor->file, " ");
        }

        codegen_walk(visitor, init);

        if (!nodecl_is_null(cond))
        {
            fprintf(visitor->file, "%s", separator);
            codegen_walk(visitor, cond);
        }
        if (!nodecl_is_null(next))
        {
            fprintf(visitor->file, "%s", separator);
            codegen_walk(visitor, next);
        }
        else
        {
            fprintf(visitor->file, "%s1", separator);
        }
    }
}

static void codegen_while_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "DO WHILE(");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, ")\n");
    visitor->indent_level++;
    codegen_walk(visitor, nodecl_get_child(node, 1));
    visitor->indent_level--;
    indent(visitor);
    fprintf(visitor->file, "END DO\n");
}

static void codegen_switch_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "SELECT CASE (");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, ")\n");
    visitor->indent_level += 2;
    codegen_walk(visitor, nodecl_get_child(node, 1));
    visitor->indent_level -= 2;
    indent(visitor);
    fprintf(visitor->file, "END SELECT\n");
}

static void codegen_case_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    visitor->indent_level--;
    indent(visitor);
    fprintf(visitor->file, "CASE (");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, ")\n");
    visitor->indent_level++;

    codegen_walk(visitor, nodecl_get_child(node, 1));
}

static void codegen_default_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    visitor->indent_level--;
    indent(visitor);
    fprintf(visitor->file, "CASE DEFAULT\n");
    visitor->indent_level++;
    codegen_walk(visitor, nodecl_get_child(node, 0));
}

static void codegen_continue_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node UNUSED_PARAMETER)
{
    indent(visitor);
    fprintf(visitor->file, "CYCLE\n");
}

static void codegen_break_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node UNUSED_PARAMETER)
{
    indent(visitor);
    fprintf(visitor->file, "EXIT\n");
}

static void codegen_for_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "DO");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, "\n");
    visitor->indent_level++;
    codegen_walk(visitor, nodecl_get_child(node, 1));
    visitor->indent_level--;
    indent(visitor);
    fprintf(visitor->file, "END DO\n");
}

static void codegen_floating_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    type_t* t = nodecl_get_type(node);

    int kind = floating_type_get_info(t)->bits / 8;
    int precision = floating_type_get_info(t)->p + 1;

    const_value_t* value = nodecl_get_constant(node);
    if (const_value_is_float(value))
    {
        fprintf(visitor->file, "%.*E_%d", precision, const_value_cast_to_float(value), kind);
    }
    else if (const_value_is_double(value))
    {
        fprintf(visitor->file, "%.*E_%d", precision, const_value_cast_to_double(value), kind);
    }
    else if (const_value_is_long_double(value))
    {
        fprintf(visitor->file, "%.*LE_%d", precision, const_value_cast_to_long_double(value), kind);
    }
#ifdef HAVE_QUADMATH_H
    else if (const_value_is_float128(value))
    {
        __float128 f128 = const_value_cast_to_float128(value);
        int n = quadmath_snprintf (NULL, 0, "%.*Qe", precision, f128);
        char c[n+1];
        quadmath_snprintf (c, n, "%.*Qe", precision, f128);
        c[n] = '\0';
        fprintf(visitor->file, "%s_%d", c, kind);
    }
#endif
}

static void codegen_context(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_walk(visitor, nodecl_get_child(node, 0));
}

static void codegen_function_code(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* entry = nodecl_get_symbol(node);
    nodecl_t context = nodecl_get_child(node, 0);
    nodecl_t statement_seq = nodecl_get_child(context, 0);
    nodecl_t internal_subprograms = nodecl_get_child(node, 2);

    // Module procedures are only printed if we are in the current module
    if (visitor->current_module != entry->entity_specs.in_module)
        return;

    scope_entry_t* old_sym = visitor->current_sym;
    visitor->current_sym = entry;

    if (entry->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
        return;
    entry->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;

    switch (entry->kind)
    {
        case SK_PROGRAM:
            {
                // If it is __MAIN__ do not print the name
                const char* program_name = entry->symbol_name[0] == '_' ? "MAIN__" : entry->symbol_name;
                fprintf(visitor->file, "PROGRAM %s\n", program_name);
                visitor->indent_level++;

                codegen_procedure(visitor, entry, statement_seq, internal_subprograms);

                fprintf(visitor->file, "END PROGRAM %s\n", program_name);
                break;
            }
        case SK_FUNCTION:
            {
                codegen_procedure_declaration_header(visitor, entry);
                codegen_procedure(visitor, entry, statement_seq, internal_subprograms);
                codegen_procedure_declaration_footer(visitor, entry);
                break;
            }
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
    BINARY_EXPRESSION(lower_or_equal_than, " >= ") \
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

static void codegen_subscript_triplet(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t nodecl_lower = nodecl_get_child(node, 0);
    nodecl_t nodecl_upper = nodecl_get_child(node, 1);
    nodecl_t nodecl_stride = nodecl_get_child(node, 2);

    if (!nodecl_is_null(nodecl_lower))
        codegen_walk(visitor, nodecl_lower);

    fprintf(visitor->file, ":");

    if (!nodecl_is_null(nodecl_upper))
        codegen_walk(visitor, nodecl_upper);

    // If the stride is not 1, do not print
    if (!(nodecl_is_constant(nodecl_stride)
                && const_value_is_integer(nodecl_get_constant(nodecl_stride))
                && const_value_is_nonzero(
                    const_value_eq(nodecl_get_constant(nodecl_stride),
                        const_value_get_one(/* num_bytes */ fortran_get_default_integer_type_kind(), /* signed */ 1)))))
    {
        fprintf(visitor->file, ":");
        codegen_walk(visitor, nodecl_stride);
    }
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
    codegen_comparison(visitor, node, " == ", " .EQV. ");
}

static void codegen_different(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_comparison(visitor, node, " /= ", " .NEQV. ");
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
        if (is_pointer_type(no_ref(symbol->type_information))
                && (nodecl_get_kind(lhs) != NODECL_DERREFERENCE))
        {
            is_ptr_assignment = 1;
        }

        if (is_ptr_assignment)
        {
            operator = " => ";
        }

        fprintf(visitor->file, "%s", operator);

        codegen_walk(visitor, rhs);
}

static void codegen_symbol(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* entry = nodecl_get_symbol(node);
    fprintf(visitor->file, "%s", entry->symbol_name);
}

static void codegen_text(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "%s", nodecl_get_text(node));
}

static void codegen_string_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const_value_t* v = nodecl_get_constant(node);

    int length = 0;
    int *bytes = NULL;
    const_value_string_unpack(v, &bytes, &length);

    fprintf(visitor->file, "\"");

    int i;

    for (i = 0; i < length; i++)
    {
        int current = bytes[i];

        if (current == '\"')
        {
            fprintf(visitor->file, "\"\"");
        }
        else
        {
            fprintf(visitor->file, "%c", (char)current);
        }
    }

    fprintf(visitor->file, "\"");

    free(bytes);
}

static void codegen_boolean_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const_value_t* val = nodecl_get_constant(node);

    if (const_value_is_zero(val))
    {
        fprintf(visitor->file, ".FALSE.");
    }
    else
    {
        fprintf(visitor->file, ".TRUE.");
    }
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

static void codegen_complex_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "(");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, ", ");
    codegen_walk(visitor, nodecl_get_child(node, 1));
    fprintf(visitor->file, ")");
}

static void codegen_structured_value(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "(/ ");
    codegen_comma_separated_list(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, " /)");
}

static void codegen_array_subscript(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t subscripted = nodecl_get_child(node, 0);
    nodecl_t subscript = nodecl_get_child(node, 1);

    codegen_walk(visitor, subscripted);

    fprintf(visitor->file, "(");
    // We keep a list instead of a single dimension for multidimensional arrays
    // alla Fortran
    reverse_codegen_comma_separated_list(visitor, subscript);
    fprintf(visitor->file, ")");
}

static void codegen_named_pair_spec(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t name = nodecl_get_child(node, 0);
    nodecl_t expr = nodecl_get_child(node, 1);

    if (!nodecl_is_null(name))
    {
        fprintf(visitor->file, "%s = ", nodecl_get_symbol(name)->symbol_name);
    }

    codegen_walk(visitor, expr);
}

static void codegen_function_call(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t called = nodecl_get_child(node, 0);
    nodecl_t arguments = nodecl_get_child(node, 1);

    scope_entry_t* entry = nodecl_get_symbol(called);

    ERROR_CONDITION(entry == NULL, "Invalid symbol in call", 0);

    char is_call = (function_type_get_return_type(entry->type_information) == NULL);

    if (is_call)
    {
        fprintf(visitor->file, "CALL ");
    }

    codegen_walk(visitor, called);

    fprintf(visitor->file, "(");
    codegen_comma_separated_list(visitor, arguments);
    fprintf(visitor->file, ")");
}

static void codegen_fortran_data(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    declare_symbols_rec(visitor, nodecl_get_child(node, 0));
    declare_symbols_rec(visitor, nodecl_get_child(node, 1));

    indent(visitor);
    fprintf(visitor->file, "DATA ");
    codegen_comma_separated_list(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, " / ");
    codegen_comma_separated_list(visitor, nodecl_get_child(node, 1));
    fprintf(visitor->file, " /\n");
}

static void codegen_fortran_equivalence(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    declare_symbols_rec(visitor, nodecl_get_child(node, 0));
    declare_symbols_rec(visitor, nodecl_get_child(node, 1));

    indent(visitor);
    fprintf(visitor->file, "EQUIVALENCE (");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, ", ");
    codegen_comma_separated_list(visitor, nodecl_get_child(node, 1));
    fprintf(visitor->file, ")\n");
}

static void codegen_implied_do(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t nodecl_symbol = nodecl_get_child(node, 0);
    nodecl_t nodecl_range = nodecl_get_child(node, 1);
    nodecl_t nodecl_expressions = nodecl_get_child(node, 2);

    fprintf(visitor->file, "(");
    codegen_comma_separated_list(visitor, nodecl_expressions);

    fprintf(visitor->file, ", ");

    codegen_walk(visitor, nodecl_symbol);

    fprintf(visitor->file, " = ");

    nodecl_t nodecl_lower = nodecl_get_child(nodecl_range, 0);
    nodecl_t nodecl_upper = nodecl_get_child(nodecl_range, 1);
    nodecl_t nodecl_stride = nodecl_get_child(nodecl_range, 2);

    codegen_walk(visitor, nodecl_lower);
    fprintf(visitor->file, ", ");
    codegen_walk(visitor, nodecl_upper);

    if (!nodecl_is_null(nodecl_stride))
    {
        fprintf(visitor->file, ", ");
        codegen_walk(visitor, nodecl_stride);
    }

    fprintf(visitor->file, ")");
}

static void codegen_forall(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t loop_control_seq = nodecl_get_child(node, 0);
    nodecl_t mask = nodecl_get_child(node, 1);
    nodecl_t statement_seq = nodecl_get_child(node, 2);

    indent(visitor);
    fprintf(visitor->file, "FORALL (");

    char old_value = visitor->in_forall;
    visitor->in_forall = 1;
    codegen_comma_separated_list(visitor, loop_control_seq);
    visitor->in_forall = old_value;

    if (!nodecl_is_null(mask))
    {
        fprintf(visitor->file, ", ");
        codegen_walk(visitor, mask);
    }

    fprintf(visitor->file, ")\n");

    visitor->indent_level++;
    codegen_walk(visitor, statement_seq);
    visitor->indent_level--;

    indent(visitor);
    fprintf(visitor->file, "END FORALL\n");
}

static void codegen_where(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t nodecl_where_set = nodecl_get_child(node, 0);

    int num_where_pairs = 0;
    nodecl_t* where_pair = nodecl_unpack_list(nodecl_where_set, &num_where_pairs);

    int i;
    for (i = 0; i < num_where_pairs; i++)
    {
        const char* keyword = "WHERE";
        if (i > 0)
            keyword = "ELSEWHERE";

        indent(visitor);
        fprintf(visitor->file, "%s", keyword);

        nodecl_t mask_expr = nodecl_get_child(where_pair[i], 0);
        nodecl_t statement_seq = nodecl_get_child(where_pair[i], 1);

        if (!nodecl_is_null(mask_expr))
        {
            fprintf(visitor->file, " (");
            codegen_walk(visitor, mask_expr);
            fprintf(visitor->file, ")");
        }
        fprintf(visitor->file, "\n");

        visitor->indent_level++;
        codegen_walk(visitor, statement_seq);
        visitor->indent_level--;
    }

    indent(visitor);
    fprintf(visitor->file, "END WHERE\n");
}

// FIXME - Fortran 2003 blocks
static void codegen_compound_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_walk(visitor, nodecl_get_child(node, 0));
}

static void codegen_unknown_pragma(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "%s\n", nodecl_get_text(node));
}

static void codegen_pragma_clause_arg(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "%s", nodecl_get_text(node));
}

static void codegen_pragma_custom_clause(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, " %s(", nodecl_get_text(node));
    codegen_comma_separated_list(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, ")");
}

static void codegen_pragma_custom_line(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, " %s", nodecl_get_text(node));
    codegen_walk(visitor, nodecl_get_child(node, 1));
}
static void codegen_pragma_custom_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "!$%s", nodecl_get_text(node));
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, "\n");
    codegen_walk(visitor, nodecl_get_child(node, 1));
}

static void codegen_conversion(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    // Do nothing
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
    NODECL_VISITOR(codegen_visitor)->visit_context = codegen_visitor_fun(codegen_context);
    NODECL_VISITOR(codegen_visitor)->visit_compound_statement = codegen_visitor_fun(codegen_compound_statement);
    NODECL_VISITOR(codegen_visitor)->visit_expression_statement = codegen_visitor_fun(codegen_expression_statement);
    NODECL_VISITOR(codegen_visitor)->visit_object_init = codegen_visitor_fun(codegen_object_init);
#define PREFIX_UNARY_EXPRESSION(_name, _) \
    NODECL_VISITOR(codegen_visitor)->visit_##_name = codegen_visitor_fun(codegen_##_name);
#define BINARY_EXPRESSION(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
    OPERATOR_TABLE
#undef BINARY_EXPRESSION
#undef PREFIX_UNARY_EXPRESSION

    NODECL_VISITOR(codegen_visitor)->visit_range = codegen_visitor_fun(codegen_subscript_triplet);
    NODECL_VISITOR(codegen_visitor)->visit_string_literal = codegen_visitor_fun(codegen_string_literal);
    NODECL_VISITOR(codegen_visitor)->visit_text = codegen_visitor_fun(codegen_text);
    NODECL_VISITOR(codegen_visitor)->visit_structured_value = codegen_visitor_fun(codegen_structured_value);
    NODECL_VISITOR(codegen_visitor)->visit_boolean_literal = codegen_visitor_fun(codegen_boolean_literal);
    NODECL_VISITOR(codegen_visitor)->visit_integer_literal = codegen_visitor_fun(codegen_integer_literal);
    NODECL_VISITOR(codegen_visitor)->visit_complex_literal = codegen_visitor_fun(codegen_complex_literal);
    NODECL_VISITOR(codegen_visitor)->visit_floating_literal = codegen_visitor_fun(codegen_floating_literal);
    NODECL_VISITOR(codegen_visitor)->visit_symbol = codegen_visitor_fun(codegen_symbol);
    NODECL_VISITOR(codegen_visitor)->visit_assignment = codegen_visitor_fun(codegen_assignment);
    NODECL_VISITOR(codegen_visitor)->visit_equal = codegen_visitor_fun(codegen_equal);
    NODECL_VISITOR(codegen_visitor)->visit_different = codegen_visitor_fun(codegen_different);
    NODECL_VISITOR(codegen_visitor)->visit_derreference = codegen_visitor_fun(codegen_derreference);
    NODECL_VISITOR(codegen_visitor)->visit_reference = codegen_visitor_fun(codegen_reference);
    NODECL_VISITOR(codegen_visitor)->visit_parenthesized_expression = codegen_visitor_fun(codegen_parenthesized_expression);
    NODECL_VISITOR(codegen_visitor)->visit_array_subscript = codegen_visitor_fun(codegen_array_subscript);
    NODECL_VISITOR(codegen_visitor)->visit_function_call = codegen_visitor_fun(codegen_function_call);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_named_pair_spec = codegen_visitor_fun(codegen_named_pair_spec);
    NODECL_VISITOR(codegen_visitor)->visit_empty_statement = codegen_visitor_fun(codegen_empty_statement);
    NODECL_VISITOR(codegen_visitor)->visit_if_else_statement = codegen_visitor_fun(codegen_if_else_statement);
    NODECL_VISITOR(codegen_visitor)->visit_return_statement = codegen_visitor_fun(codegen_return_statement);
    NODECL_VISITOR(codegen_visitor)->visit_labeled_statement = codegen_visitor_fun(codegen_labeled_statement);
    NODECL_VISITOR(codegen_visitor)->visit_goto_statement = codegen_visitor_fun(codegen_goto_statement);
    NODECL_VISITOR(codegen_visitor)->visit_for_statement = codegen_visitor_fun(codegen_for_statement);
    NODECL_VISITOR(codegen_visitor)->visit_while_statement = codegen_visitor_fun(codegen_while_statement);
    NODECL_VISITOR(codegen_visitor)->visit_loop_control = codegen_visitor_fun(codegen_loop_control);
    NODECL_VISITOR(codegen_visitor)->visit_switch_statement = codegen_visitor_fun(codegen_switch_statement);
    NODECL_VISITOR(codegen_visitor)->visit_case_statement = codegen_visitor_fun(codegen_case_statement);
    NODECL_VISITOR(codegen_visitor)->visit_default_statement = codegen_visitor_fun(codegen_default_statement);
    NODECL_VISITOR(codegen_visitor)->visit_break_statement = codegen_visitor_fun(codegen_break_statement);
    NODECL_VISITOR(codegen_visitor)->visit_continue_statement = codegen_visitor_fun(codegen_continue_statement);

    NODECL_VISITOR(codegen_visitor)->visit_fortran_io_spec = codegen_visitor_fun(codegen_fortran_io_spec);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_print_statement = codegen_visitor_fun(codegen_print_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_write_statement = codegen_visitor_fun(codegen_write_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_read_statement = codegen_visitor_fun(codegen_read_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_stop_statement = codegen_visitor_fun(codegen_stop_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_computed_goto_statement = codegen_visitor_fun(codegen_computed_goto_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_io_statement = codegen_visitor_fun(codegen_io_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_open_statement = codegen_visitor_fun(codegen_open_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_close_statement = codegen_visitor_fun(codegen_close_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_allocate_statement = codegen_visitor_fun(codegen_allocate_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_deallocate_statement = codegen_visitor_fun(codegen_deallocate_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_nullify_statement = codegen_visitor_fun(codegen_nullify_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_arithmetic_if_statement = codegen_visitor_fun(codegen_arithmetic_if_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_label_assign_statement = codegen_visitor_fun(codegen_label_assign_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_assigned_goto_statement = codegen_visitor_fun(codegen_assigned_goto_statement);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_implied_do = codegen_visitor_fun(codegen_implied_do);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_data = codegen_visitor_fun(codegen_fortran_data);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_equivalence = codegen_visitor_fun(codegen_fortran_equivalence);

    NODECL_VISITOR(codegen_visitor)->visit_fortran_forall = codegen_visitor_fun(codegen_forall);
    NODECL_VISITOR(codegen_visitor)->visit_fortran_where = codegen_visitor_fun(codegen_where);

    NODECL_VISITOR(codegen_visitor)->visit_conversion = codegen_visitor_fun(codegen_conversion);

    NODECL_VISITOR(codegen_visitor)->visit_unknown_pragma = codegen_visitor_fun(codegen_unknown_pragma);
    NODECL_VISITOR(codegen_visitor)->visit_pragma_custom_clause = codegen_visitor_fun(codegen_pragma_custom_clause);
    NODECL_VISITOR(codegen_visitor)->visit_pragma_custom_line = codegen_visitor_fun(codegen_pragma_custom_line);
    NODECL_VISITOR(codegen_visitor)->visit_pragma_custom_statement = codegen_visitor_fun(codegen_pragma_custom_statement);
    NODECL_VISITOR(codegen_visitor)->visit_pragma_clause_arg = codegen_visitor_fun(codegen_pragma_clause_arg);

}

void _fortran_codegen_translation_unit(FILE* f, nodecl_t node)
{
    nodecl_codegen_visitor_t codegen_visitor;

    fortran_codegen_init(&codegen_visitor);
    
    codegen_visitor.file = f;
    codegen_visitor.indent_level = 0;
    codegen_visitor.current_sym = NULL;

    codegen_walk(&codegen_visitor, node);
}


char* _fortran_codegen_to_str(nodecl_t node)
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

