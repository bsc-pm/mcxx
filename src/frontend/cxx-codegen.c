#include "cxx-codegen.h"
#include "cxx-utils.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "cxx-nodecl-visitor.h"
#include "cxx-prettyprint.h"
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

    scope_entry_t* global_namespace;
    scope_entry_t* opened_namespace;

    char in_condition;
    nodecl_t condition_top;
    char in_initializer;
    char mem_init_list;
} nodecl_codegen_visitor_t;


typedef void (*codegen_visitor_fun_t)(nodecl_codegen_visitor_t* visitor, nodecl_t node);
typedef void (*nodecl_visitor_fun_t)(nodecl_external_visitor_t* visitor, nodecl_t node);

static void codegen_template_parameters(nodecl_codegen_visitor_t* visitor, template_parameter_list_t* template_parameters);

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

static void codegen_fill_namespace_list_rec(nodecl_codegen_visitor_t* visitor, 
        scope_entry_t* namespace_sym, 
        scope_entry_t** list, 
        int* position)
{
    ERROR_CONDITION(namespace_sym == NULL, "Invalid symbol", 0);
    ERROR_CONDITION(namespace_sym->kind != SK_NAMESPACE, "Symbol '%s' is not a namespace", namespace_sym->symbol_name);
    if (namespace_sym == visitor->global_namespace)
    {
        *position = 0;
    }
    else
    {
        codegen_fill_namespace_list_rec(visitor, 
                namespace_sym->decl_context.current_scope->related_entry,
                list,
                position);
        ERROR_CONDITION(*position == MCXX_MAX_SCOPES_NESTING, "Too many scopes", 0);
        list[*position] = namespace_sym;
        (*position)++;
    }
}

static void codegen_move_namespace_from_to(nodecl_codegen_visitor_t* visitor, scope_entry_t* from, scope_entry_t* to)
{
    scope_entry_t* namespace_nesting_from[MCXX_MAX_SCOPES_NESTING] = { 0 };
    scope_entry_t* namespace_nesting_to[MCXX_MAX_SCOPES_NESTING] = { 0 };

    int num_from = 0;
    codegen_fill_namespace_list_rec(visitor, from, namespace_nesting_from, &num_from);
    int num_to = 0;
    codegen_fill_namespace_list_rec(visitor, to, namespace_nesting_to, &num_to);

    // We only have to close and open the noncommon suffixes
    int common;
    for (common = 0; 
            (common < num_from) 
            && (common < num_to) 
            && (namespace_nesting_from[common] == namespace_nesting_to[common]); 
            common++)
    {
        // Empty body
    }

    int i;
    for (i = common; i < num_from; i++)
    {
        visitor->indent_level--;
        indent(visitor);
        fprintf(visitor->file, "}\n");
    }

    for (i = common; i < num_to; i++)
    {
        indent(visitor);
        const char* real_name = namespace_nesting_to[i]->symbol_name;
        //
        // Anonymous namespace has special properties that we want to preserve
        if (strcmp(real_name, "(unnamed)") == 0)
        {
            real_name = "/* anonymous */";
        }

        fprintf(visitor->file, "namespace %s {\n", real_name);
        if ((i + 1) < num_from)
        {
            fprintf(visitor->file, " ");
        }
        visitor->indent_level++;
    }
}

static void codegen_move_to_namespace_of_symbol(nodecl_codegen_visitor_t* visitor, scope_entry_t* symbol)
{
    C_LANGUAGE()
    {
        return;
    }
    // Get the namespace where this symbol has been declared
    scope_t* enclosing_namespace = symbol->decl_context.namespace_scope;
    scope_entry_t* namespace_sym = enclosing_namespace->related_entry;

    // First close the namespaces
    codegen_move_namespace_from_to(visitor, visitor->opened_namespace, namespace_sym);
    visitor->opened_namespace = namespace_sym;
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
    else if (is_named_class_type(t))
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
    else if (is_unnamed_class_type(t))
    {
        // Special case for nested members
        scope_entry_list_t* members = class_type_get_members(t);
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it));
        {
            scope_entry_t* member = entry_list_iterator_current(it);
            walk_type_for_symbols(visitor, member->type_information, /* needs_def */ 1, symbol_to_declare, symbol_to_define);
        }
        entry_list_iterator_free(it);
        entry_list_free(members);
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
        define_local_entities_in_trees(visitor, nodecl_get_child(node, i), current_scope);
    }

    scope_entry_t* entry = nodecl_get_symbol(node);
    if (entry != NULL
            && entry->type_information != NULL)
    {
        codegen_type_of_symbol(visitor, entry->type_information, /* needs def */ 1);

        define_local_entities_in_trees(visitor, entry->value, current_scope);

        if (current_scope == entry->decl_context.current_scope)
        {
            if (nodecl_get_kind(node) != NODECL_OBJECT_INIT)
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
        define_nonlocal_entities_in_trees(visitor, nodecl_get_child(node, i));
    }

    scope_entry_t* entry = nodecl_get_symbol(node);
    if (entry != NULL
            && entry->type_information != NULL)
    {
        codegen_type_of_symbol_only_nonlocal(visitor, entry->type_information, /* needs def */ 1);
        define_symbol_if_nonlocal(visitor, entry);

        define_nonlocal_entities_in_trees(visitor, entry->value);
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
#if 0
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
#endif
}

static void define_symbol(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    ERROR_CONDITION(symbol == NULL, "Invalid symbol", 0);

    if (symbol->entity_specs.is_injected_class_name)
        symbol = named_type_get_symbol(symbol->entity_specs.class_type);

    if (symbol->do_not_print)
        return;

    // Do nothing if already defined
    if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
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
                // codegen_move_to_namespace_of_symbol(visitor, symbol);
                // indent(visitor);
                // fprintf(visitor->file, "typedef %s;\n", print_decl_type_str(symbol->type_information, 
                //             symbol->decl_context, symbol->symbol_name));
                break;
            }
        case SK_ENUMERATOR:
            {
                define_symbol(visitor, named_type_get_symbol(symbol->type_information));
                break;
            }
        case SK_ENUM:
            {
                // First define what may be needed of this 
                int i;
                for (i = 0; i < enum_type_get_num_enumerators(symbol->type_information); i++)
                {
                    scope_entry_t* enumerator = enum_type_get_enumerator_num(symbol->type_information, i);
                    define_nonlocal_entities_in_trees(visitor, enumerator->value);
                    enumerator->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                }

                codegen_move_to_namespace_of_symbol(visitor, symbol);
                C_LANGUAGE()
                {
                    // the symbol will be already called 'enum X' in C
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

                for (i = 0; i < enum_type_get_num_enumerators(symbol->type_information); i++)
                {
                    scope_entry_t* enumerator = enum_type_get_enumerator_num(symbol->type_information, i);
                    if (i != 0)
                    {
                        fprintf(visitor->file, ",\n");
                    }
                    indent(visitor);
                    fprintf(visitor->file, "%s", enumerator->symbol_name);
                }

                visitor->indent_level--;

                fprintf(visitor->file, "\n");
                indent(visitor);
                fprintf(visitor->file, "};\n");
                break;
            }
        case SK_CLASS:
            {
                int i = 0;

                scope_entry_list_t* friends = class_type_get_friends(symbol->type_information);

                scope_entry_list_t* members = class_type_get_members(symbol->type_information);
                scope_entry_list_iterator_t* it = NULL;
                for (it = entry_list_iterator_begin(members);
                        !entry_list_iterator_end(it);
                        entry_list_iterator_next(it))
                {
                    scope_entry_t* member = entry_list_iterator_current(it);

                    codegen_type_of_symbol(visitor, member->type_information, /* needs_def */ 1);
                }
                entry_list_iterator_free(it);

                access_specifier_t default_access_spec = AS_UNKNOWN;

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

                C_LANGUAGE()
                {
                    indent(visitor);
                    if (symbol->entity_specs.is_anonymous
                            && symbol->decl_context.current_scope->kind == CLASS_SCOPE)
                    {
                        // If is anonymous and in class scope, this is an anonymously declared class inside a class specifier,
                        // thus, do not print its name, only its class-key
                        fprintf(visitor->file, "%s\n", class_key);
                    }
                    else
                    {
                        // Usual case: the symbol will be already called 'struct/union X' in C
                        fprintf(visitor->file, "%s\n", symbol->symbol_name);
                    }
                    indent(visitor);
                    fprintf(visitor->file, "{\n");
                }

                CXX_LANGUAGE()
                {
                    // This class is a member class we need to define its enclosing class first
                    if (symbol->entity_specs.is_member)
                    {
                        define_symbol(visitor, 
                                named_type_get_symbol(symbol->entity_specs.class_type));
                    }

                    char is_template_specialized = 0;
                    char is_primary_template = 0;

                    type_t* template_type = NULL;
                    type_t* primary_template = NULL;
                    scope_entry_t* primary_symbol = NULL;

                    if (is_template_specialized_type(symbol->type_information))
                    {
                        is_template_specialized = 1;
                        template_type = template_specialized_type_get_related_template_type(symbol->type_information);
                        primary_template = template_type_get_primary_type(template_type);
                        primary_symbol = named_type_get_symbol(primary_template);

                        if (primary_symbol != symbol)
                        {
                            declare_symbol(visitor, primary_symbol);
                        }
                        else
                        {
                            is_primary_template = 1;
                        }
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

                    // Define all friends that have been declared
                    for (it = entry_list_iterator_begin(friends);
                            !entry_list_iterator_end(it);
                            entry_list_iterator_next(it))
                    {
                        scope_entry_t* friend = entry_list_iterator_current(it);

                        // The user did not declare it, ignore it
                        if (is_friend_declared(friend))
                            continue;

                        declare_symbol(visitor, friend);
                    }
                    entry_list_iterator_free(it);

                    codegen_move_to_namespace_of_symbol(visitor, symbol);

                    if (is_template_specialized)
                    {
                        type_t* class_type = get_actual_class_type(symbol->type_information);
                        if (class_type_is_complete_independent(class_type)
                                || class_type_is_incomplete_independent(class_type))
                        {
                            indent(visitor);
                            fprintf(visitor->file, "template <>\n");
                        }
                        else
                        {
                            ERROR_CONDITION(!is_primary_template, "Only the primary template is actually allowed!\n", 0);

                            template_parameter_list_t* template_parameters = template_specialized_type_get_template_arguments(
                                    symbol->type_information);

                            indent(visitor);
                            fprintf(visitor->file, "template <");
                            codegen_template_parameters(visitor, template_parameters);
                            fprintf(visitor->file, ">\n");
                        }
                    }

                    indent(visitor);
                    const char* qualified_name = get_qualified_symbol_name(symbol, symbol->decl_context);

                    // Global qualification is not valid here
                    while (qualified_name[0] == ':')
                    {
                        qualified_name++;
                    }

                    fprintf(visitor->file, "%s %s", class_key, qualified_name);
                    
                    // From here we assume its already defined
                    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;

                    if (is_template_specialized
                            && !is_primary_template)
                    {
                        fprintf(visitor->file, "%s", 
                                get_template_arguments_str(symbol, symbol->decl_context));
                    }

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

                    fprintf(visitor->file, "\n");
                    indent(visitor);
                    fprintf(visitor->file, "{\n");
                }


                access_specifier_t current_access_spec = default_access_spec;
                for (it = entry_list_iterator_begin(members);
                        !entry_list_iterator_end(it);
                        entry_list_iterator_next(it))
                {
                    scope_entry_t* member = entry_list_iterator_current(it);
                    access_specifier_t access_spec = member->entity_specs.access;

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

                    C_LANGUAGE()
                    {
                        // Everything must be properly defined in C
                        define_symbol(visitor, member);
                    }
                    CXX_LANGUAGE()
                    {
                        declare_symbol(visitor, member);
                    }
                    // This has been already defined here
                    member->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                    visitor->indent_level--;

                    CXX_LANGUAGE()
                    {
                        visitor->indent_level--;
                    }
                }
                entry_list_iterator_free(it);
                entry_list_free(members);

                // Print friends
                for (it = entry_list_iterator_begin(friends);
                        !entry_list_iterator_end(it);
                        entry_list_iterator_next(it))
                {
                    scope_entry_t* friend = entry_list_iterator_current(it);
                    // The user did not declare it, ignore it
                    if (is_friend_declared(friend))
                        continue;

                    char is_primary_template = 0;
                    scope_entry_t* template_symbol = NULL;

                    visitor->indent_level++;
                    // Since friends are a tad bit special we will handle them here
                    if (is_template_specialized_type(friend->type_information))
                    {
                        type_t* template_type = template_specialized_type_get_related_template_type(friend->type_information);
                        template_symbol = template_type_get_related_symbol(template_type);
                        type_t* primary_template = template_type_get_primary_type(template_type);
                        scope_entry_t* primary_symbol = named_type_get_symbol(primary_template);

                        if (primary_symbol == symbol)
                        {
                            indent(visitor);
                            fprintf(visitor->file, "template <");
                            template_parameter_list_t* template_parameters = template_type_get_template_parameters(template_type);
                            codegen_template_parameters(visitor, template_parameters);
                            fprintf(visitor->file, ">\n");

                            is_primary_template = 1;
                        }
                    }

                    indent(visitor);
                    fprintf(visitor->file, "friend ");

                    if (friend->kind == SK_CLASS)
                    {
                        const char *friend_class_key = "";
                        switch (class_type_get_class_kind(symbol->type_information))
                        {
                            case CK_CLASS:
                                friend_class_key = "class";
                                break;
                            case CK_STRUCT:
                                friend_class_key = "struct";
                                break;
                            case CK_UNION:
                                friend_class_key = "union";
                                break;
                            default:
                                internal_error("Invalid class kind", 0);
                        }

                        if (!is_primary_template)
                        {
                            fprintf(visitor->file, "%s %s;\n", friend_class_key, 
                                    get_qualified_symbol_name(friend, friend->decl_context));
                        }
                        else
                        {
                            fprintf(visitor->file, "%s %s;\n", friend_class_key, 
                                    get_qualified_symbol_name(template_symbol, template_symbol->decl_context));
                        }
                    }
                    else if (friend->kind == SK_FUNCTION)
                    {
                        type_t* real_type = friend->type_information;
                        if (symbol->entity_specs.is_conversion)
                        {
                            real_type = get_new_function_type(NULL, NULL, 0);
                        }

                        const char* declarator = print_decl_type_str(real_type,
                                friend->decl_context,
                                get_qualified_symbol_name(friend, friend->decl_context));

                        fprintf(visitor->file, "%s;\n", declarator);
                    }
                    else
                    {
                        internal_error("Invalid friend symbol kind '%s'\n", symbol_kind_name(friend));
                    }

                    visitor->indent_level--;
                }
                entry_list_iterator_free(it);
                entry_list_free(friends);

                indent(visitor);
                fprintf(visitor->file, "};\n");

                break;
            }
        case SK_FUNCTION:
            {
                // Functions are not defined but only declared
                declare_symbol(visitor, symbol);
                break;
            }
        default:
            {
                internal_error("I do not know how to define a %s\n", symbol_kind_name(symbol));
            }
    }

    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
}

static void codegen_template_parameters(nodecl_codegen_visitor_t* visitor, template_parameter_list_t* template_parameters)
{
    int i;
    for (i = 0; i < template_parameters->num_parameters; i++)
    {
        template_parameter_t* tpl_param = template_parameters->parameters[i];

        if (i != 0)
        {
            fprintf(visitor->file, ", ");
        }
        scope_entry_t* symbol = tpl_param->entry;

        switch (tpl_param->kind)
        {
            case TPK_TYPE:
                {
                    fprintf(visitor->file, "typename %s", symbol->symbol_name);
                    break;
                }
            case TPK_NONTYPE:
                {
                    const char* declaration = print_decl_type_str(symbol->type_information,
                                symbol->decl_context,
                                symbol->symbol_name);
                    if (declaration[0] == ':'
                            && i == 0)
                    {
                        fprintf(visitor->file, " ");
                    }

                    fprintf(visitor->file, "%s", declaration);
                    break;
                }
            case TPK_TEMPLATE:
                {
                    type_t* template_type = symbol->type_information;
                    fprintf(visitor->file, "template <");
                    codegen_template_parameters(visitor, template_type_get_template_parameters(template_type));
                    fprintf(visitor->file, "> ");
                    fprintf(visitor->file, " class %s", symbol->symbol_name);
                    break;
                }
            default:
                {
                    internal_error("Invalid template parameter kind", 0);
                }
        }
    }
}

static void walk_expression_list(nodecl_codegen_visitor_t *visitor, nodecl_t node);

static void declare_symbol(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    if (symbol->entity_specs.is_injected_class_name)
        symbol = named_type_get_symbol(symbol->entity_specs.class_type);

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
                else if (symbol->entity_specs.is_extern)
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
                        unmangle_symbol_name(symbol));

                codegen_move_to_namespace_of_symbol(visitor, symbol);
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

                    char equal_is_needed = 0;
                    CXX03_LANGUAGE()
                    {
                        // We only need = if the initializer is a structured one
                        // and this is C++03, in C++1x syntax { } is always allowed
                        equal_is_needed = 1;
                        if (nodecl_get_kind(symbol->value) == NODECL_FUNCTION_CALL)
                        {
                            scope_entry_t* called_sym = nodecl_get_symbol(nodecl_get_child(symbol->value, 0));
                            if (called_sym->entity_specs.is_constructor
                                    && equivalent_types(called_sym->entity_specs.class_type, 
                                        no_ref(symbol->type_information)))
                            {
                                equal_is_needed = 0;
                            }
                        }
                    }

                    if (equal_is_needed)
                    {
                        fprintf(visitor->file, "%s", " = ");
                    }

                    char in_initializer = visitor->in_initializer;
                    visitor->in_initializer = 1;
                    if (!nodecl_is_null(symbol->value))
                    {
                        fprintf(visitor->file, "(");
                        codegen_walk(visitor, symbol->value);
                        fprintf(visitor->file, ")");
                    }
                    visitor->in_initializer = in_initializer;
                }

                if (!visitor->in_condition)
                {
                    fprintf(visitor->file, ";\n");
                }
                break;
            }
        case SK_CLASS:
            {
                C_LANGUAGE()
                {
                    // the symbol will be already called 'struct/union X' in C
                    indent(visitor);
                    fprintf(visitor->file, "%s;\n", symbol->symbol_name);
                }
                CXX_LANGUAGE()
                {
                    // This class is a member class we need to define its enclosing class first
                    if (symbol->entity_specs.is_member)
                    {
                        define_symbol(visitor, 
                                named_type_get_symbol(symbol->entity_specs.class_type));
                    }

                    char is_template_specialized = 0;
                    char is_primary_template = 0;

                    type_t* template_type = NULL;
                    type_t* primary_template = NULL;
                    scope_entry_t* primary_symbol = NULL;

                    if (is_template_specialized_type(symbol->type_information))
                    {
                        is_template_specialized = 1;
                        template_type = template_specialized_type_get_related_template_type(symbol->type_information);
                        primary_template = template_type_get_primary_type(template_type);
                        primary_symbol = named_type_get_symbol(primary_template);
                        declare_symbol(visitor, primary_symbol);

                        if (primary_symbol != symbol)
                        {
                            declare_symbol(visitor, primary_symbol);
                        }
                        else
                        {
                            is_primary_template = 1;
                        }
                    }


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

                    codegen_move_to_namespace_of_symbol(visitor, symbol);

                    if (is_template_specialized)
                    {
                        type_t* class_type = get_actual_class_type(symbol->type_information);
                        if (class_type_is_complete_independent(class_type)
                                || class_type_is_incomplete_independent(class_type))
                        {
                            indent(visitor);
                            fprintf(visitor->file, "template <>\n");
                        }
                        else
                        {
                            ERROR_CONDITION(!is_primary_template, "Only the primary template is actually allowed!\n", 0);
                            template_parameter_list_t* template_parameters = template_specialized_type_get_template_arguments(
                                    symbol->type_information);

                            indent(visitor);
                            fprintf(visitor->file, "template <");
                            codegen_template_parameters(visitor, template_parameters);
                            fprintf(visitor->file, ">\n");
                        }
                    }

                    indent(visitor);
                    fprintf(visitor->file, "%s %s", class_key, symbol->symbol_name);

                    if (is_template_specialized
                            && !is_primary_template)
                    {
                        fprintf(visitor->file, "%s", 
                                get_template_arguments_str(symbol, symbol->decl_context));
                    }

                    fprintf(visitor->file, ";\n");
                }
                break;
            }
        case SK_ENUMERATOR:
            {
                declare_symbol(visitor, named_type_get_symbol(symbol->type_information));
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
                // // A typedef cannot be only declared, it is always defined
                // symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                // codegen_move_to_namespace_of_symbol(visitor, symbol);
                // indent(visitor);
                // fprintf(visitor->file, "typedef %s;\n", print_decl_type_str(symbol->type_information, 
                //         symbol->decl_context, symbol->symbol_name));
                break;
            }
        case SK_FUNCTION:
            {
                walk_type_for_symbols(visitor,
                        symbol->type_information,
                        0,
                        declare_symbol,
                        define_symbol);

                CXX_LANGUAGE()
                {
                    codegen_move_to_namespace_of_symbol(visitor, symbol);

                    if (is_template_specialized_type(symbol->type_information))
                    {
                        type_t* template_type = template_specialized_type_get_related_template_type(symbol->type_information);
                        type_t* primary_template = template_type_get_primary_type(template_type);
                        scope_entry_t* primary_symbol = named_type_get_symbol(primary_template);
                        declare_symbol(visitor, primary_symbol);

                        if (primary_symbol != symbol)
                        {
                            indent(visitor);
                            fprintf(visitor->file, "template <>\n");
                        }
                        else
                        {
                            indent(visitor);
                            fprintf(visitor->file, "template <");
                            template_parameter_list_t* template_parameters = template_type_get_template_parameters(template_type);
                            codegen_template_parameters(visitor, template_parameters);
                            fprintf(visitor->file, ">\n");
                        }
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
                    C_LANGUAGE()
                    {
                        decl_spec_seq = strappend(decl_spec_seq, "__inline ");
                    }
                    CXX_LANGUAGE()
                    {
                        decl_spec_seq = strappend(decl_spec_seq, "inline ");
                    }
                }

                const char* gcc_attributes = "";
                int i;
                for (i = 0; i < MCXX_MAX_GCC_ATTRIBUTES_PER_SYMBOL; i++)
                {
                    if (symbol->entity_specs.gcc_attributes[i].attribute_name != NULL)
                    {
                        if (nodecl_is_null(symbol->entity_specs.gcc_attributes[i].expression_list))
                        {
                            char c[256];
                            snprintf(c, 255, " __attribute__((%s))", symbol->entity_specs.gcc_attributes[i].attribute_name);
                            c[255] = '\0';

                            gcc_attributes = strappend(gcc_attributes, c);
                        }
                        else
                        {
                            // We print the expression in a temporary file
                            nodecl_codegen_visitor_t str_visitor = *visitor;

                            char *attribute_expr_list = NULL;
                            size_t size = 0;
                            FILE* temporal_stream = open_memstream(&attribute_expr_list, &size);

                            str_visitor.file = temporal_stream;
                            walk_expression_list(&str_visitor, symbol->entity_specs.gcc_attributes[i].expression_list);
                            fclose(str_visitor.file);

                            char c[256];
                            snprintf(c, 255, " __attribute__((%s(%s)))", 
                                    symbol->entity_specs.gcc_attributes[i].attribute_name,
                                    attribute_expr_list
                                    );
                            c[255] = '\0';

                            gcc_attributes = strappend(gcc_attributes, c);
                        }
                    }
                }

                char* asm_specification = "";
                if (symbol->entity_specs.asm_specification != NULL)
                {
                    nodecl_codegen_visitor_t str_visitor = *visitor;

                    size_t size = 0;
                    FILE* temporal_stream = open_memstream(&asm_specification, &size);

                    str_visitor.file = temporal_stream;
                    codegen_walk(&str_visitor, _nodecl_wrap(symbol->entity_specs.asm_specification));
                    fclose(str_visitor.file);
                }

                type_t* real_type = symbol->type_information;
                if (symbol->entity_specs.is_conversion)
                {
                    real_type = get_new_function_type(NULL, NULL, 0);
                }

                const char* declarator = print_decl_type_str(real_type,
                        symbol->decl_context,
                        unmangle_symbol_name(symbol));

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
                fprintf(visitor->file, "%s%s%s%s%s;\n", decl_spec_seq, declarator, exception_spec, gcc_attributes, asm_specification);
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
        codegen_walk(visitor, _nodecl_wrap(current));

        // If we are not the last
        if (it != list)
        {
            fprintf(visitor->file, "%s", separator);
        }
    }
}

// Special case for expression lists where top level comma operators demmand
// additional parentheses
static void walk_expression_list(nodecl_codegen_visitor_t *visitor, nodecl_t node)
{
    if (nodecl_is_null(node))
        return;

    AST list = nodecl_get_ast(node);

    ERROR_CONDITION(ASTType(list) != AST_NODE_LIST, "Invalid node kind", 0);

    AST it;
    for_each_element(list, it)
    {
        AST current = ASTSon1(it);

        if (ASTType(current) == NODECL_COMMA)
        {
            fprintf(visitor->file, "(");
        }

        codegen_walk(visitor, _nodecl_wrap(current));

        if (ASTType(current) == NODECL_COMMA)
        {
            fprintf(visitor->file, ")");
        }

        // If we are not the last
        if (it != list)
        {
            fprintf(visitor->file, ", ");
        }
    }
}

static void walk_expression_unpacked_list(nodecl_codegen_visitor_t *visitor, nodecl_t* node, int i)
{
    int j;
    for (j = 0; j < i; j++)
    {
        if (j != 0)
        {
            fprintf(visitor->file, ", ");
        }

        AST current = nodecl_get_ast(node[j]);

        if (ASTType(current) == NODECL_COMMA)
        {
            fprintf(visitor->file, "(");
        }

        codegen_walk(visitor, _nodecl_wrap(current));

        if (ASTType(current) == NODECL_COMMA)
        {
            fprintf(visitor->file, ")");
        }
    }
}

// Codegen
static void not_implemented_yet(nodecl_external_visitor_t* visitor UNUSED_PARAMETER, nodecl_t node)
{
    internal_error("WARNING -> Uninmplemented node! '%s'\n", ast_print_node_type(nodecl_get_kind(node)));
}

static void codegen_symbol(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* entry = nodecl_get_symbol(node);
    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);

    CXX_LANGUAGE()
    {
        if (!entry->entity_specs.is_template_parameter)
        {
            fprintf(visitor->file, "%s", get_qualified_symbol_name(entry, entry->decl_context));
        }
        else
        {
            fprintf(visitor->file, "%s", entry->symbol_name);
        }
    }
    C_LANGUAGE()
    {
        fprintf(visitor->file, "%s", entry->symbol_name);
    }
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

static void codegen_string_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "%s", nodecl_get_text(node));
}

static void codegen_floating_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const_value_t* value = nodecl_get_constant(node);
    ERROR_CONDITION(value == NULL, "Invalid value", 0);

    if (const_value_is_float(value))
    {
        fprintf(visitor->file, "%.24ef", const_value_cast_to_float(value));
    }
    else if (const_value_is_double(value))
    {
        fprintf(visitor->file, "%.53e", const_value_cast_to_double(value));
    }
    else if (const_value_is_long_double(value))
    {
        fprintf(visitor->file, "%.113LeL", const_value_cast_to_long_double(value));
    }
}

static void codegen_boolean_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const_value_t* val = nodecl_get_constant(node);

    if (const_value_is_zero(val))
    {
        fprintf(visitor->file, "false");
    }
    else
    {
        fprintf(visitor->file, "true");
    }
}

// We return negative numbers because it is easier to list them in descending priority order
static int get_rank_kind(node_t n, const char* text)
{
    switch (n)
    {
        case NODECL_SYMBOL:
        case NODECL_STRING_LITERAL:
        case NODECL_INTEGER_LITERAL:
        case NODECL_FLOATING_LITERAL:
        case NODECL_BOOLEAN_LITERAL:
        case NODECL_STRUCTURED_LITERAL:
        case NODECL_PARENTHESIZED_EXPRESSION:
        case NODECL_BUILTIN_EXPR:
            {
                return -1;
            }
        case NODECL_ARRAY_SUBSCRIPT:
        case NODECL_FUNCTION_CALL:
        case NODECL_CLASS_MEMBER_ACCESS:
        case NODECL_TYPEID:
        case NODECL_POSTINCREMENT:
        case NODECL_POSTDECREMENT:
            {
                return -2;
            }
        case NODECL_REFERENCE:
        case NODECL_DERREFERENCE:
        case NODECL_PLUS:
        case NODECL_NEG:
        case NODECL_LOGICAL_NOT:
        case NODECL_BITWISE_NOT:
        case NODECL_SIZEOF:
        case NODECL_NEW:
        case NODECL_DELETE:
        case NODECL_PREINCREMENT:
        case NODECL_PREDECREMENT:
            // FIXME: Missing GCC nodes 
            // FIXME: Do we want them or we can use builtins?
            // case NODECL_ALIGNOF
            // case NODECL_REAL
            // case NODECL_IMAG
            // case NODECL_LABEL_ADDR
            {
                return -3;
            }
            // This one is special as we keep several casts in a single node
        case NODECL_CAST:
            {
                if (IS_C_LANGUAGE
                        || ((text != NULL) && (strcmp(text, "C") == 0)))
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
            // This is a pointer to member
        case NODECL_OFFSET:
            return -5;
        case NODECL_MUL:
        case NODECL_DIV:
        case NODECL_MOD:
            return -6;
        case NODECL_ADD:
        case NODECL_MINUS:
            return -7;
        case NODECL_SHL:
        case NODECL_SHR:
            return -8;
        case NODECL_LOWER_THAN:
        case NODECL_LOWER_OR_EQUAL_THAN:
        case NODECL_GREATER_THAN:
        case NODECL_GREATER_OR_EQUAL_THAN:
            return -9;
        case NODECL_EQUAL:
        case NODECL_DIFFERENT:
            return -10;
        case NODECL_BITWISE_AND:
            return -11;
        case NODECL_BITWISE_XOR:
            return -12;
        case NODECL_BITWISE_OR:
            return -13;
        case NODECL_LOGICAL_AND:
            return -14;
        case NODECL_LOGICAL_OR:
            return -15;
        case NODECL_CONDITIONAL_EXPRESSION:
            return -16;
        case NODECL_ASSIGNMENT:
        case NODECL_MUL_ASSIGNMENT :
        case NODECL_DIV_ASSIGNMENT:
        case NODECL_ADD_ASSIGNMENT:
        case NODECL_SUB_ASSIGNMENT:
        case NODECL_SHL_ASSIGNMENT:
        case NODECL_SHR_ASSIGNMENT:
        case NODECL_BITWISE_AND_ASSIGNMENT:
        case NODECL_BITWISE_OR_ASSIGNMENT:
        case NODECL_BITWISE_XOR_ASSIGNMENT:
        case NODECL_THROW:
            return -17;
        case NODECL_COMMA:
            return -18;
        default:
            // Lowest priority possible. This is a conservative approach that
            // will work always albeit it will introduce some unnecessary
            // parentheses for unknown expressions
            return -1000;
    }
    return -1000;
}

static int get_rank(nodecl_t n)
{
    return get_rank_kind(nodecl_get_kind(n), nodecl_get_text(n));
}

static char is_bitwise_bin_operator(node_t n)
{
    return n == NODECL_BITWISE_AND
        || n == NODECL_BITWISE_OR
        || n == NODECL_BITWISE_XOR;
}


// We do not keep parentheses in C/C++ so we may need to restore some of them
static char operand_has_lower_priority(nodecl_t current_operator, nodecl_t operand)
{
    // It does not have known lower priority
    int rank_current = get_rank(current_operator);
    int rank_operand = get_rank(operand);

    node_t current_kind = nodecl_get_kind(current_operator);
    node_t operand_kind = nodecl_get_kind(operand);

    if (is_bitwise_bin_operator(current_kind) // &
            && is_bitwise_bin_operator(operand_kind)) // |
    {
        // For the sake of clarity
        // a | b & c  -> a | (b & c)
        return 1;
    }

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
    PREFIX_UNARY_EXPRESSION(delete, "delete ") \
    PREFIX_UNARY_EXPRESSION(delete_array, "delete[] ") \
    PREFIX_UNARY_EXPRESSION(throw, "throw ") \
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
    BINARY_EXPRESSION_ASSIG(assignment, " = ") \
    BINARY_EXPRESSION_ASSIG(mul_assignment, " *= ") \
    BINARY_EXPRESSION_ASSIG(div_assignment, " /= ") \
    BINARY_EXPRESSION_ASSIG(add_assignment, " += ") \
    BINARY_EXPRESSION_ASSIG(sub_assignment, " -= ") \
    BINARY_EXPRESSION_ASSIG(shl_assignment, " <<= ") \
    BINARY_EXPRESSION_ASSIG(shr_assignment, " >>= ") \
    BINARY_EXPRESSION_ASSIG(bitwise_and_assignment, " &= ") \
    BINARY_EXPRESSION_ASSIG(bitwise_or_assignment, " |= ") \
    BINARY_EXPRESSION_ASSIG(bitwise_xor_assignment, " ^= ") \
    BINARY_EXPRESSION_ASSIG(mod_assignment, " %= ") \
    BINARY_EXPRESSION(class_member_access, ".") \
    BINARY_EXPRESSION(offset, ".*") \
    BINARY_EXPRESSION(comma, ", ") 

#define PREFIX_UNARY_EXPRESSION(_name, _operand) \
    static void codegen_##_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) \
    { \
        nodecl_t rhs = nodecl_get_child(node, 0); \
        char needs_parentheses = operand_has_lower_priority(node, rhs); \
        fprintf(visitor->file, "%s", _operand); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        codegen_walk(visitor, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
    }
#define POSTFIX_UNARY_EXPRESSION(_name, _operand) \
    static void codegen_##_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) \
    { \
        nodecl_t rhs = nodecl_get_child(node, 0); \
        char needs_parentheses = operand_has_lower_priority(node, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        codegen_walk(visitor, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
        fprintf(visitor->file, "%s", _operand); \
    }
#define BINARY_EXPRESSION(_name, _operand) \
    static void codegen_##_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) \
    { \
        nodecl_t lhs = nodecl_get_child(node, 0); \
        nodecl_t rhs = nodecl_get_child(node, 1); \
        char needs_parentheses = operand_has_lower_priority(node, lhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        codegen_walk(visitor, lhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
        fprintf(visitor->file, "%s", _operand); \
        needs_parentheses = operand_has_lower_priority(node, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        codegen_walk(visitor, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
    }
#define BINARY_EXPRESSION_ASSIG(_name, _operand) \
    static void codegen_##_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) \
    { \
        if (visitor->in_condition && visitor->condition_top.tree == node.tree) \
        { \
            fprintf(visitor->file, "("); \
        } \
        nodecl_t lhs = nodecl_get_child(node, 0); \
        nodecl_t rhs = nodecl_get_child(node, 1); \
        char needs_parentheses = operand_has_lower_priority(node, lhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        codegen_walk(visitor, lhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
        fprintf(visitor->file, "%s", _operand); \
        needs_parentheses = operand_has_lower_priority(node, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, "("); \
        } \
        codegen_walk(visitor, rhs); \
        if (needs_parentheses) \
        { \
            fprintf(visitor->file, ")"); \
        } \
        if (visitor->in_condition && visitor->condition_top.tree == node.tree) \
        { \
            fprintf(visitor->file, ")"); \
        } \
    }
OPERATOR_TABLE
#undef POSTFIX_UNARY_EXPRESSION
#undef PREFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION
#undef BINARY_EXPRESSION_ASSIG

static void codegen_parenthesized_expression(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t nest = nodecl_get_child(node, 0);
    fprintf(visitor->file, "(");
    codegen_walk(visitor, nest);
    fprintf(visitor->file, ")");
}

static void codegen_new(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t initializer = nodecl_get_child(node, 0);
    nodecl_t placement = nodecl_get_child(node, 1);

    fprintf(visitor->file, "new ");

    if (!nodecl_is_null(placement))
    {
        fprintf(visitor->file, "(");
        walk_expression_list(visitor, placement);
        fprintf(visitor->file, ") ");
    }

    type_t* t = nodecl_get_type(node);
    ERROR_CONDITION(!is_pointer_type(t), "Invalid type for NODECL_NEW", 0);

    t = pointer_type_get_pointee_type(t);

    // Maybe there is no symbol!!!
    fprintf(visitor->file, "%s", print_type_str(t, visitor->current_sym->decl_context));

    if (!nodecl_is_null(initializer))
    {
        int old_initializer = visitor->in_initializer;
        visitor->in_initializer = 1;
        codegen_walk(visitor, initializer);
        visitor->in_initializer = old_initializer;
    }
}

static void codegen_sizeof(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    type_t* t = nodecl_get_type(node);

    fprintf(visitor->file, "sizeof(%s)", print_type_str(t, CURRENT_COMPILED_FILE->global_decl_context));
}

static void codegen_conditional_expression(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t cond = nodecl_get_child(node, 0);
    nodecl_t then = nodecl_get_child(node, 1);
    nodecl_t _else = nodecl_get_child(node, 2);

    if (operand_has_lower_priority(node, cond))
    {
        fprintf(visitor->file, "(");
    }
    codegen_walk(visitor, cond);
    if (operand_has_lower_priority(node, cond))
    {
        fprintf(visitor->file, ")");
    }

    fprintf(visitor->file, " ? ");

    if (operand_has_lower_priority(node, then))
    {
        fprintf(visitor->file, "(");
    }
    codegen_walk(visitor, then);
    if (operand_has_lower_priority(node, then))
    {
        fprintf(visitor->file, ")");
    }

    fprintf(visitor->file, " : ");

    if (operand_has_lower_priority(node, _else))
    {
        fprintf(visitor->file, "(");
    }
    codegen_walk(visitor, _else);
    if (operand_has_lower_priority(node, _else))
    {
        fprintf(visitor->file, ")");
    }
}

static void codegen_cast(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const char* cast_kind = nodecl_get_text(node);
    type_t* t = nodecl_get_type(node);
    nodecl_t nest = nodecl_get_child(node, 0);

    if (IS_C_LANGUAGE
            || strcmp(cast_kind, "C") == 0)
    {
        fprintf(visitor->file, "(%s)", print_type_str(t, visitor->current_sym->decl_context));
        char needs_parentheses = operand_has_lower_priority(node, nest);
        if (needs_parentheses)
        {
            fprintf(visitor->file, "(");
        }
        codegen_walk(visitor, nest);
        if (needs_parentheses)
        {
            fprintf(visitor->file, ")");
        }
    }
    else
    {
        fprintf(visitor->file, "%s<%s>(", cast_kind, print_type_str(t, visitor->current_sym->decl_context));
        codegen_walk(visitor, nest);
        fprintf(visitor->file, ")");
    }
}

static void codegen_function_call(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t called_entity = nodecl_get_child(node, 0);
    nodecl_t arguments = nodecl_get_child(node, 1);

    enum call_kind
    {
        INVALID_CALL = 0,
        ORDINARY_CALL,
        NONSTATIC_MEMBER_CALL,
        CONSTRUCTOR_INITIALIZATION
    } kind = ORDINARY_CALL;

    scope_entry_t* called_symbol = nodecl_get_symbol(called_entity);
    if (called_symbol != NULL)
    {
        if (called_symbol->kind == SK_FUNCTION
                && called_symbol->entity_specs.is_member)
        {
            if (called_symbol->entity_specs.is_constructor)
            {
                kind = CONSTRUCTOR_INITIALIZATION;
            }
            else if (!called_symbol->entity_specs.is_static)
            {
                kind = NONSTATIC_MEMBER_CALL;
            }
        }
    }

    switch (kind)
    {
        case ORDINARY_CALL:
            {
                char needs_parentheses = operand_has_lower_priority(node, called_entity);
                if (needs_parentheses) {
                    fprintf(visitor->file, "(");
                }
                codegen_walk(visitor, called_entity);
                if (needs_parentheses)
                {
                    fprintf(visitor->file, ")");
                }
                fprintf(visitor->file, "(");
                walk_expression_list(visitor, arguments);
                fprintf(visitor->file, ")");
                break;
            }
        case NONSTATIC_MEMBER_CALL:
            {
                int num_items = 0;
                nodecl_t* unpacked_list = nodecl_unpack_list(arguments, &num_items);

                ERROR_CONDITION(!(num_items >= 1), "A nonstatic member call lacks the implicit argument", 0);

                char needs_parentheses = (get_rank(unpacked_list[0])
                        < get_rank_kind(NODECL_CLASS_MEMBER_ACCESS, NULL));
                
                if (needs_parentheses)
                {
                    fprintf(visitor->file, "(");
                }
                codegen_walk(visitor, unpacked_list[0]);
                if (needs_parentheses)
                {
                    fprintf(visitor->file, ")");
                }
                fprintf(visitor->file, ".");

                if (!called_symbol->entity_specs.is_virtual)
                {
                    codegen_walk(visitor, called_entity);
                }
                else
                {
                    fprintf(visitor->file, "%s", unmangle_symbol_name(called_symbol));
                }

                fprintf(visitor->file, "(");

                walk_expression_unpacked_list(visitor, unpacked_list + 1, num_items - 1);

                fprintf(visitor->file, ")");

                free(unpacked_list);
                break;
            }
        case CONSTRUCTOR_INITIALIZATION:
            {
                // Do not print what is being called
                // if we are in an initializer
                if (!visitor->in_initializer)
                {
                    scope_entry_t* class_symbol = named_type_get_symbol(called_symbol->entity_specs.class_type);
                    fprintf(visitor->file, "%s", get_qualified_symbol_name(class_symbol, class_symbol->decl_context));
                    fprintf(visitor->file, "(");
                }
                char old_initializer = visitor->in_initializer;
                visitor->in_initializer = 0;
                walk_expression_list(visitor, arguments);
                visitor->in_initializer = old_initializer;
                if (!visitor->in_initializer)
                {
                    fprintf(visitor->file, ")");
                }
                break;
            }
        default:
            {
                internal_error("Unhandled function call kind", 0);
            }
    }
}

static void codegen_any_list_sep(nodecl_codegen_visitor_t* visitor, nodecl_t node, const char* separator)
{
    nodecl_t any_list = nodecl_get_child(node, 0);
    walk_list(visitor, any_list, separator);
}

static void codegen_any_list(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t any_list = nodecl_get_child(node, 0);
    codegen_walk(visitor, any_list);
}

static void codegen_builtin(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const char* builtin_name = nodecl_get_text(node);
    nodecl_t any_list_holder = nodecl_get_child(node, 0);
    nodecl_t any_list = nodecl_get_child(any_list_holder, 0);
    if (strcmp(builtin_name, "gcc-asm-definition") == 0)
    {
        indent(visitor);
        // FIXME - We are missing __volatile__ keyword
        int num_items = 0;
        nodecl_t* unpacked_list = nodecl_unpack_list(any_list, &num_items);

        ERROR_CONDITION(num_items != 4, "Invalid number of items for gcc-asm-definition", 0);

        fprintf(visitor->file, "__asm__ (");
        codegen_walk(visitor, unpacked_list[0]); // string-literal
        fprintf(visitor->file, " : ");
        codegen_any_list_sep(visitor, unpacked_list[1], ", "); // gcc-asm-operand
        fprintf(visitor->file, " : ");
        codegen_any_list_sep(visitor, unpacked_list[2], ", "); // gcc-asm-operand
        fprintf(visitor->file, " : ");
        codegen_any_list_sep(visitor, unpacked_list[3], ", "); // gcc-asm-operand
        fprintf(visitor->file, ");");

        free(unpacked_list);
    }
    else if (strcmp(builtin_name, "gcc-asm-operand") == 0)
    {
        int num_items = 0;
        nodecl_t* unpacked_list = nodecl_unpack_list(any_list, &num_items);

        nodecl_t identifier = nodecl_null();
        nodecl_t string_literal = nodecl_null();
        nodecl_t expression = nodecl_null();
        if (num_items == 3)
        {
            string_literal = unpacked_list[0];
            expression = unpacked_list[1];
            identifier = unpacked_list[2];
        }
        else if (num_items == 2)
        {
            expression = unpacked_list[1];
            string_literal = unpacked_list[0];
        }
        else
        {
            internal_error("Invalid gcc-asm-operand builtin", 0);
        }

        if (!nodecl_is_null(identifier))
        {
            fprintf(visitor->file, "[");
            codegen_walk(visitor, identifier);
            fprintf(visitor->file, "]");
        }
        codegen_walk(visitor, string_literal); // string-literal
        fprintf(visitor->file, "(");
        codegen_walk(visitor, expression); // expression
        fprintf(visitor->file, ")");

        free(unpacked_list);
    }
    else if (strcmp(builtin_name, "gcc-asm-spec") == 0)
    {
        int num_items = 0;
        nodecl_t* unpacked_list = nodecl_unpack_list(any_list, &num_items);

        ERROR_CONDITION(num_items != 1, "Invalid tree for gcc-asm-spec", 0);

        fprintf(visitor->file, " __asm(");
        codegen_walk(visitor, unpacked_list[0]);
        fprintf(visitor->file, ")");

        free(unpacked_list);
    }
    else
    {
        internal_error("Unhandled '%s' builtin at %s\n", builtin_name, nodecl_get_locus(node));
    }
}

static void codegen_structured_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t items = nodecl_get_child(node, 0);

    fprintf(visitor->file, "{ ");
    walk_expression_list(visitor, items);
    fprintf(visitor->file, " }");
}

static void codegen_field_designator(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t field = nodecl_get_child(node, 0);
    nodecl_t next = nodecl_get_child(node, 1);

    fprintf(visitor->file, ".");
    codegen_walk(visitor, field);

    if (nodecl_get_kind(field) != NODECL_FIELD_DESIGNATOR
            && nodecl_get_kind(field) != NODECL_INDEX_DESIGNATOR)
    {
        fprintf(visitor->file, " = ");
    }
    codegen_walk(visitor, next);
}

static void codegen_index_designator(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t _index = nodecl_get_child(node, 0);
    nodecl_t next = nodecl_get_child(node, 1);

    fprintf(visitor->file, "[");
    codegen_walk(visitor, _index);
    fprintf(visitor->file, "]");

    if (nodecl_get_kind(_index) != NODECL_FIELD_DESIGNATOR
            && nodecl_get_kind(_index) != NODECL_INDEX_DESIGNATOR)
    {
        fprintf(visitor->file, " = ");
    }
    codegen_walk(visitor, next);
}

static void codegen_array_subscript(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t subscripted = nodecl_get_child(node, 0);
    nodecl_t subscript = nodecl_get_child(node, 1);

    if (operand_has_lower_priority(node, subscripted))
    {
    fprintf(visitor->file, "(");
    }
    codegen_walk(visitor, subscripted);
    if (operand_has_lower_priority(node, subscripted))
    {
    fprintf(visitor->file, ")");
    }

    // We keep a list instead of a single dimension for multidimensional arrays
    // alla Fortran
    AST subscript_list = nodecl_get_ast(subscript);
    AST it;
    for_each_element(subscript_list, it)
    {
        fprintf(visitor->file, "[");
        codegen_walk(visitor, _nodecl_wrap(ASTSon1(it)));
        fprintf(visitor->file, "]");
    }
}

// Statements
static void codegen_catch_handler(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t name = nodecl_get_child(node, 0);
    nodecl_t statement = nodecl_get_child(node, 0);
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
        int old_condition = visitor->in_condition;
        nodecl_t old_condition_top = visitor->condition_top;

        visitor->in_condition = 1;
        visitor->condition_top = name;

        codegen_walk(visitor, name);

        visitor->condition_top = old_condition_top;
        visitor->in_condition = old_condition;
    }

    fprintf(visitor->file, ")");

    codegen_walk(visitor, statement);
}

static void codegen_try_block(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t statement = nodecl_get_child(node, 0);
    nodecl_t catch_handlers = nodecl_get_child(node, 0);
    nodecl_t any_catch_handler = nodecl_get_child(node, 0);
    indent(visitor);
    fprintf(visitor->file, "try\n");

    codegen_walk(visitor, statement);

    codegen_walk(visitor, catch_handlers);

    if (!nodecl_is_null(any_catch_handler))
    {
        indent(visitor);
        fprintf(visitor->file, "catch (...)\n");
        codegen_walk(visitor, any_catch_handler);
    }
}

static void codegen_pragma_custom_construct(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t pragma_line = nodecl_get_child(node, 0);
    nodecl_t statement = nodecl_get_child(node, 1);

    indent(visitor);

    fprintf(visitor->file, "#pragma %s", nodecl_get_text(node));
    codegen_walk(visitor, pragma_line);
    codegen_walk(visitor, statement);
}

static void codegen_pragma_clause_arg(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "%s", nodecl_get_text(node));
}

static void codegen_pragma_custom_clause(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t arguments = nodecl_get_child(node, 0);

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
    nodecl_t parameters = nodecl_get_child(node, 0);
    nodecl_t clauses = nodecl_get_child(node, 1);

    fprintf(visitor->file, " %s", nodecl_get_text(node));

    if (!nodecl_is_null(parameters))
    {
        fprintf(visitor->file, "(");
        walk_list(visitor, parameters, ", ");
        fprintf(visitor->file, ")");
    }

    codegen_walk(visitor, clauses);
}

static void codegen_pragma_custom_directive(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t pragma_line = nodecl_get_child(node, 0);

    indent(visitor);
    fprintf(visitor->file, "#pragma %s", nodecl_get_text(node));
    codegen_walk(visitor, pragma_line);
    fprintf(visitor->file, "\n");
}

static void codegen_return_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t expression = nodecl_get_child(node, 0);

    indent(visitor);
    fprintf(visitor->file, "return ");

    codegen_walk(visitor, expression);

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
    nodecl_t expression = nodecl_get_child(node, 0);
    nodecl_t statement = nodecl_get_child(node, 1);

    indent(visitor);
    fprintf(visitor->file, "case ");
    codegen_walk(visitor, expression);
    fprintf(visitor->file, " :\n");

    codegen_walk(visitor, statement);
}

static void codegen_default_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t statement = nodecl_get_child(node, 0);

    indent(visitor);
    fprintf(visitor->file, "default :\n");

    codegen_walk(visitor, statement);
}

static void codegen_switch_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t expression = nodecl_get_child(node, 0);
    nodecl_t statement = nodecl_get_child(node, 1);

    indent(visitor);
    fprintf(visitor->file, "switch (");
    nodecl_t old_condition_top = visitor->condition_top;
    int old_condition = visitor->in_condition;
    int old_indent = visitor->indent_level;

    visitor->indent_level = 0;
    visitor->in_condition = 1;
    visitor->condition_top = expression;

    codegen_walk(visitor, expression);

    visitor->indent_level = old_indent;
    visitor->in_condition = old_condition;
    visitor->condition_top = old_condition_top;

    fprintf(visitor->file, ")\n");

    visitor->indent_level += 2;
    codegen_walk(visitor, statement);
    visitor->indent_level -= 2;
}

static void codegen_labeled_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* label_sym = nodecl_get_symbol(node);
    nodecl_t statement = nodecl_get_child(node, 0);

    indent(visitor);
    fprintf(visitor->file, "%s : ", label_sym->symbol_name);

    int old = visitor->indent_level;
    visitor->indent_level = 0;
    codegen_walk(visitor, statement);
    visitor->indent_level = old;
}

static void codegen_if_else_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t condition = nodecl_get_child(node, 0);
    nodecl_t then = nodecl_get_child(node, 1);
    nodecl_t _else = nodecl_get_child(node, 2);

    indent(visitor);

    fprintf(visitor->file, "if (");
    int old_condition = visitor->in_condition;
    int old_indent = visitor->indent_level;
    nodecl_t old_condition_top = visitor->condition_top;

    visitor->indent_level = 0;
    visitor->in_condition = 1;
    visitor->condition_top = condition;

    codegen_walk(visitor, condition);

    visitor->indent_level = old_indent;
    visitor->in_condition = old_condition;
    visitor->condition_top = old_condition_top;

    fprintf(visitor->file, ")\n");

    visitor->indent_level++;
    codegen_walk(visitor, then);
    visitor->indent_level--;

    if (!nodecl_is_null(_else))
    {
        indent(visitor);
        fprintf(visitor->file, "else\n");
        visitor->indent_level++;
        codegen_walk(visitor, _else);
        visitor->indent_level--;
    }
}

static void codegen_for_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t loop_control = nodecl_get_child(node, 0);
    nodecl_t statement = nodecl_get_child(node, 1);

    indent(visitor);
    fprintf(visitor->file, "for (");
    codegen_walk(visitor, loop_control);
    fprintf(visitor->file, ")\n");

    visitor->indent_level++;
    codegen_walk(visitor, statement);
    visitor->indent_level--;
}

static void codegen_loop_control(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t init = nodecl_get_child(node, 0);
    nodecl_t cond = nodecl_get_child(node, 1);
    nodecl_t next = nodecl_get_child(node, 2);

    // No condition top as "for((i=0); ...)" looks unnecessary ugly
    int old = visitor->in_condition;
    visitor->in_condition = 1;

    codegen_walk(visitor, init);
    fprintf(visitor->file, "; ");

    nodecl_t old_condition_top = visitor->condition_top;
    visitor->condition_top = cond;

    // But it is desirable for the condition in "for( ... ; (i = x) ; ...)"
    codegen_walk(visitor, cond);
    fprintf(visitor->file, "; ");

    visitor->condition_top = old_condition_top;

    // Here we do not care about parentheses "for ( ... ; ... ; i = i + 1)"
    codegen_walk(visitor, next);
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
    nodecl_t statement = nodecl_get_child(node, 0);
    nodecl_t condition = nodecl_get_child(node, 1);

    indent(visitor);
    fprintf(visitor->file, "do\n");

    visitor->indent_level++;
    codegen_walk(visitor, statement);
    visitor->indent_level--;

    indent(visitor);
    fprintf(visitor->file, "while (");
    codegen_walk(visitor, condition);
    fprintf(visitor->file, ");\n");
}

static void codegen_while_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t condition = nodecl_get_child(node, 0);
    nodecl_t statement = nodecl_get_child(node, 1);

    indent(visitor);
    fprintf(visitor->file, "while (");
    int old = visitor->in_condition;
    nodecl_t old_condition_top = visitor->condition_top;
    int old_indent = visitor->indent_level;
    visitor->indent_level = 0;
    visitor->in_condition = 1;
    visitor->condition_top = condition;
    codegen_walk(visitor, condition);
    visitor->indent_level = old_indent;
    visitor->in_condition = old;
    visitor->condition_top = old_condition_top;
    fprintf(visitor->file, ")\n");

    visitor->indent_level++;
    codegen_walk(visitor, statement);
    visitor->indent_level--;
}

static void codegen_expression_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t expression = nodecl_get_child(node, 0);
    indent(visitor);
    codegen_walk(visitor, expression);
    fprintf(visitor->file, ";\n");
}

static void codegen_compound_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "{\n");
    visitor->indent_level++;
    nodecl_t statement_seq = nodecl_get_child(node, 0);

    scope_entry_t* scope_symbol = nodecl_get_symbol(node);
    ERROR_CONDITION(scope_symbol == NULL || scope_symbol->kind != SK_SCOPE, "Invalid scoping symbol", 0);

    define_local_entities_in_trees(visitor, statement_seq, scope_symbol->decl_context.current_scope);

    codegen_walk(visitor, statement_seq);

    visitor->indent_level--;
    indent(visitor);
    fprintf(visitor->file, "}\n");
}

// Explicit object initialization
static void codegen_object_init(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    scope_entry_t* entry = nodecl_get_symbol(node);

    if (!visitor->mem_init_list)
    {
        codegen_type_of_symbol(visitor, entry->type_information, /* needs def */ 1);

        entry->entity_specs.codegen_status = CODEGEN_STATUS_NONE;
        define_symbol(visitor, entry);
    }
    else
    {
        nodecl_t nodecl_init_expr = nodecl_get_child(node, 0);

        fprintf(visitor->file, "%s(", entry->symbol_name);
        char in_initializer = visitor->in_initializer;
        visitor->in_initializer = 1;
        codegen_walk(visitor, nodecl_init_expr);
        visitor->in_initializer = in_initializer;
        fprintf(visitor->file, ")");
    }
}

// Function code
static void codegen_function_code(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t statement_seq = nodecl_get_child(node, 0);
    nodecl_t initializers = nodecl_get_child(node, 1);
    nodecl_t internal_functions = nodecl_get_child(node, 2);

    if (!nodecl_is_null(internal_functions))
    {
        internal_error("C/C++ does not have internal functions", 0);
    }

    if (!nodecl_is_null(nodecl_get_child(statement_seq, 0)))
    {
        internal_error("C/C++ functions only have one statement", 0);
    }

    nodecl_t statement = nodecl_get_child(statement_seq, 1);

    scope_entry_t* symbol = nodecl_get_symbol(node);
    ERROR_CONDITION(symbol == NULL || symbol->kind != SK_FUNCTION, "Invalid symbol", 0);

    if (symbol->entity_specs.is_member)
    {
        scope_entry_t* class_symbol = named_type_get_symbol(symbol->entity_specs.class_type);
        define_symbol(visitor, class_symbol);
    }
    else
    {
        if (is_template_specialized_type(symbol->type_information))
        {
            type_t* template_type = template_specialized_type_get_related_template_type(symbol->type_information);
            type_t* primary_template = template_type_get_primary_type(template_type);
            scope_entry_t* primary_symbol = named_type_get_symbol(primary_template);
            declare_symbol(visitor, primary_symbol);
        }
    }

    codegen_type_of_symbol(visitor, function_type_get_return_type(symbol->type_information), /* needs_def */ 1);

    visitor->current_sym = symbol;

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
            symbol->entity_specs.related_symbols[i]->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
            add_to_clear_list(symbol->entity_specs.related_symbols[i]);
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
        C_LANGUAGE()
        {
            decl_spec_seq = strappend(decl_spec_seq, "__inline ");
        }
        CXX_LANGUAGE()
        {
            decl_spec_seq = strappend(decl_spec_seq, "inline ");
        }
    }

    const char* gcc_attributes = "";
    int num_attrs = 0;
    for (i = 0; i < MCXX_MAX_GCC_ATTRIBUTES_PER_SYMBOL; i++)
    {
        if (symbol->entity_specs.gcc_attributes[i].attribute_name != NULL)
            num_attrs++;
    }
    for (i = 0; i < num_attrs; i++)
    {
        const char* separator = " ";
        // if ((i + 1) == num_attrs)
        //     separator = "";

        if (nodecl_is_null(symbol->entity_specs.gcc_attributes[i].expression_list))
        {
            char c[256];
            snprintf(c, 255, "__attribute__((%s))%s", symbol->entity_specs.gcc_attributes[i].attribute_name, separator);
            c[255] = '\0';

            gcc_attributes = strappend(gcc_attributes, c);
        }
        else
        {
            // We print the expression in a temporary file
            nodecl_codegen_visitor_t str_visitor = *visitor;

            char *attribute_expr_list = NULL;
            size_t size = 0;
            FILE* temporal_stream = open_memstream(&attribute_expr_list, &size);

            str_visitor.file = temporal_stream;
            walk_expression_list(&str_visitor, symbol->entity_specs.gcc_attributes[i].expression_list);
            fclose(str_visitor.file);

            char c[256];
            snprintf(c, 255, " __attribute__((%s(%s)))%s", 
                    symbol->entity_specs.gcc_attributes[i].attribute_name,
                    attribute_expr_list,
                    separator
                    );
            c[255] = '\0';

            gcc_attributes = strappend(gcc_attributes, c);
        }
    }

    char* asm_specification = "";
    if (symbol->entity_specs.asm_specification != NULL)
    {
        nodecl_codegen_visitor_t str_visitor = *visitor;

        size_t size = 0;
        FILE* temporal_stream = open_memstream(&asm_specification, &size);

        str_visitor.file = temporal_stream;
        codegen_walk(&str_visitor, _nodecl_wrap(symbol->entity_specs.asm_specification));
        fclose(str_visitor.file);
    }

    const char* qualified_name = unmangle_symbol_name(symbol);

    if (symbol->entity_specs.is_member)
    {
        scope_entry_t* class_symbol = named_type_get_symbol(symbol->entity_specs.class_type);

        const char* class_name = class_symbol->symbol_name;

        if (is_template_specialized_type(class_symbol->type_information))
        {
            class_name = strappend(class_name, 
                    get_template_arguments_str(class_symbol, class_symbol->decl_context));
        }

        qualified_name = strappend(strappend(class_name, "::"), qualified_name);
    }

    // FIXME: non-template member functions of template classes require special treatment
    if (is_template_specialized_type(symbol->type_information))
    {
        qualified_name = strappend(qualified_name, 
                get_template_arguments_str(symbol, symbol->decl_context));
    }

    type_t* real_type = symbol->type_information;
    if (symbol->entity_specs.is_conversion)
    {
        real_type = get_new_function_type(NULL, NULL, 0);
    }

    const char* declarator = get_declaration_string_internal(real_type,
            symbol->decl_context,
            qualified_name,
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

    codegen_move_to_namespace_of_symbol(visitor, symbol);

    if (is_template_specialized_type(symbol->type_information))
    {
        indent(visitor);
        fprintf(visitor->file, "template <>\n");
    }

    indent(visitor);
    fprintf(visitor->file, "%s%s%s%s%s\n", decl_spec_seq, gcc_attributes, declarator, exception_spec, asm_specification);

    if (!nodecl_is_null(initializers))
    {
        visitor->indent_level++;
        indent(visitor);

        fprintf(visitor->file, ": ");

        int old_mem_init_list = visitor->mem_init_list;
        visitor->mem_init_list = 1;
        walk_list(visitor, initializers, ", ");
        visitor->mem_init_list = old_mem_init_list;

        visitor->indent_level--;

        fprintf(visitor->file, "\n");
    }

    codegen_walk(visitor, statement);
}

static void codegen_cxx_raw(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    prettyprint(visitor->file, nodecl_unwrap_cxx_raw(node));
}

// Top level
static void codegen_top_level(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t list = nodecl_get_child(node, 0);
    codegen_walk(visitor, list);
}

static void c_cxx_codegen_init(nodecl_codegen_visitor_t* codegen_visitor)
{
    nodecl_init_walker((nodecl_external_visitor_t*)codegen_visitor, not_implemented_yet);

    NODECL_VISITOR(codegen_visitor)->visit_top_level = codegen_visitor_fun(codegen_top_level);
    NODECL_VISITOR(codegen_visitor)->visit_function_code = codegen_visitor_fun(codegen_function_code);
    NODECL_VISITOR(codegen_visitor)->visit_compound_statement = codegen_visitor_fun(codegen_compound_statement);
    NODECL_VISITOR(codegen_visitor)->visit_expression_statement = codegen_visitor_fun(codegen_expression_statement);
    NODECL_VISITOR(codegen_visitor)->visit_assignment = codegen_visitor_fun(codegen_assignment);
    NODECL_VISITOR(codegen_visitor)->visit_add = codegen_visitor_fun(codegen_add);
    NODECL_VISITOR(codegen_visitor)->visit_symbol = codegen_visitor_fun(codegen_symbol);
    NODECL_VISITOR(codegen_visitor)->visit_integer_literal = codegen_visitor_fun(codegen_integer_literal);
    NODECL_VISITOR(codegen_visitor)->visit_string_literal = codegen_visitor_fun(codegen_string_literal);
    NODECL_VISITOR(codegen_visitor)->visit_boolean_literal = codegen_visitor_fun(codegen_boolean_literal);
    NODECL_VISITOR(codegen_visitor)->visit_floating_literal = codegen_visitor_fun(codegen_floating_literal);
    NODECL_VISITOR(codegen_visitor)->visit_object_init = codegen_visitor_fun(codegen_object_init);
    NODECL_VISITOR(codegen_visitor)->visit_if_else_statement = codegen_visitor_fun(codegen_if_else_statement);
    NODECL_VISITOR(codegen_visitor)->visit_for_statement = codegen_visitor_fun(codegen_for_statement);
    NODECL_VISITOR(codegen_visitor)->visit_loop_control = codegen_visitor_fun(codegen_loop_control);
    NODECL_VISITOR(codegen_visitor)->visit_while_statement = codegen_visitor_fun(codegen_while_statement);
    NODECL_VISITOR(codegen_visitor)->visit_do_statement = codegen_visitor_fun(codegen_do_statement);
    NODECL_VISITOR(codegen_visitor)->visit_empty_statement = codegen_visitor_fun(codegen_empty_statement);
    NODECL_VISITOR(codegen_visitor)->visit_default_statement = codegen_visitor_fun(codegen_default_statement);
    NODECL_VISITOR(codegen_visitor)->visit_switch_statement = codegen_visitor_fun(codegen_switch_statement);
    NODECL_VISITOR(codegen_visitor)->visit_labeled_statement = codegen_visitor_fun(codegen_labeled_statement);
    NODECL_VISITOR(codegen_visitor)->visit_break_statement = codegen_visitor_fun(codegen_break_statement);
    NODECL_VISITOR(codegen_visitor)->visit_continue_statement = codegen_visitor_fun(codegen_continue_statement);
    NODECL_VISITOR(codegen_visitor)->visit_case_statement = codegen_visitor_fun(codegen_case_statement);
    NODECL_VISITOR(codegen_visitor)->visit_return_statement = codegen_visitor_fun(codegen_return_statement);
    NODECL_VISITOR(codegen_visitor)->visit_goto_statement = codegen_visitor_fun(codegen_goto_statement);
    NODECL_VISITOR(codegen_visitor)->visit_pragma_custom_directive = codegen_visitor_fun(codegen_pragma_custom_directive);
    NODECL_VISITOR(codegen_visitor)->visit_pragma_custom_construct = codegen_visitor_fun(codegen_pragma_custom_construct);
    NODECL_VISITOR(codegen_visitor)->visit_pragma_custom_clause = codegen_visitor_fun(codegen_pragma_custom_clause);
    NODECL_VISITOR(codegen_visitor)->visit_pragma_clause_arg = codegen_visitor_fun(codegen_pragma_clause_arg);
    NODECL_VISITOR(codegen_visitor)->visit_pragma_custom_line = codegen_visitor_fun(codegen_pragma_custom_line);
    NODECL_VISITOR(codegen_visitor)->visit_try_block = codegen_visitor_fun(codegen_try_block);
    NODECL_VISITOR(codegen_visitor)->visit_catch_handler = codegen_visitor_fun(codegen_catch_handler);
    NODECL_VISITOR(codegen_visitor)->visit_parenthesized_expression = codegen_visitor_fun(codegen_parenthesized_expression);
    NODECL_VISITOR(codegen_visitor)->visit_new = codegen_visitor_fun(codegen_new);
    NODECL_VISITOR(codegen_visitor)->visit_delete = codegen_visitor_fun(codegen_delete);
    NODECL_VISITOR(codegen_visitor)->visit_delete_array = codegen_visitor_fun(codegen_delete_array);
    NODECL_VISITOR(codegen_visitor)->visit_throw = codegen_visitor_fun(codegen_throw);
    NODECL_VISITOR(codegen_visitor)->visit_function_call = codegen_visitor_fun(codegen_function_call);
    NODECL_VISITOR(codegen_visitor)->visit_cast = codegen_visitor_fun(codegen_cast);
    NODECL_VISITOR(codegen_visitor)->visit_sizeof = codegen_visitor_fun(codegen_sizeof);
    NODECL_VISITOR(codegen_visitor)->visit_conditional_expression = codegen_visitor_fun(codegen_conditional_expression);
    NODECL_VISITOR(codegen_visitor)->visit_builtin_decl = codegen_visitor_fun(codegen_builtin);
    NODECL_VISITOR(codegen_visitor)->visit_builtin_expr = codegen_visitor_fun(codegen_builtin);
    NODECL_VISITOR(codegen_visitor)->visit_any_list = codegen_visitor_fun(codegen_any_list);
    NODECL_VISITOR(codegen_visitor)->visit_structured_literal = codegen_visitor_fun(codegen_structured_literal);
    NODECL_VISITOR(codegen_visitor)->visit_field_designator = codegen_visitor_fun(codegen_field_designator);
    NODECL_VISITOR(codegen_visitor)->visit_index_designator = codegen_visitor_fun(codegen_index_designator);
    NODECL_VISITOR(codegen_visitor)->visit_array_subscript = codegen_visitor_fun(codegen_array_subscript);
    // All binary infix, unary prefix and unary postfix are here, look for the definition of OPERATOR_TABLE above
#define PREFIX_UNARY_EXPRESSION(_name, _) \
    NODECL_VISITOR(codegen_visitor)->visit_##_name = codegen_visitor_fun(codegen_##_name);
#define POSTFIX_UNARY_EXPRESSION(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
#define BINARY_EXPRESSION(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
#define BINARY_EXPRESSION_ASSIG(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
    OPERATOR_TABLE
#undef PREFIX_UNARY_EXPRESSION
#undef POSTFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION

    NODECL_VISITOR(codegen_visitor)->visit_cxx_raw = codegen_visitor_fun(codegen_cxx_raw);
}

// External interface
void c_cxx_codegen_translation_unit(FILE *f, nodecl_t node, scope_link_t* sl)
{
    nodecl_codegen_visitor_t codegen_visitor;
    memset(&codegen_visitor, 0, sizeof(codegen_visitor));

    if (sl == NULL)
    {
        sl = CURRENT_COMPILED_FILE->scope_link;
    }

    c_cxx_codegen_init(&codegen_visitor);
    
    codegen_visitor.file = f;
    codegen_visitor.indent_level = 0;
    decl_context_t global_context = scope_link_get_decl_context(sl, nodecl_get_ast(node));
    codegen_visitor.current_sym = global_context.global_scope->related_entry;
    codegen_visitor.global_namespace = codegen_visitor.current_sym;
    codegen_visitor.opened_namespace = codegen_visitor.global_namespace;

    codegen_walk(&codegen_visitor, node);

    codegen_move_namespace_from_to(&codegen_visitor, codegen_visitor.opened_namespace, codegen_visitor.global_namespace);

    run_clear_list();
}

#undef OPERATOR_TABLE

char* c_cxx_codegen_to_str(nodecl_t node)
{
    char *str = NULL;
    size_t size = 0;
    FILE* temporal_stream = open_memstream(&str, &size);

    nodecl_codegen_visitor_t codegen_visitor;

    c_cxx_codegen_init(&codegen_visitor);

    codegen_visitor.file = temporal_stream;
    codegen_visitor.indent_level = 0;
    decl_context_t global_context = scope_link_get_decl_context(CURRENT_COMPILED_FILE->scope_link, nodecl_get_ast(node));
    codegen_visitor.current_sym = global_context.global_scope->related_entry;
    codegen_visitor.global_namespace = codegen_visitor.current_sym;
    codegen_visitor.opened_namespace = codegen_visitor.global_namespace;

    codegen_walk(&codegen_visitor, node);

    fclose(temporal_stream);

    return str;
}
