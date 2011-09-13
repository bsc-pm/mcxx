#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "cxx-codegen.h"
#include "cxx-utils.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "cxx-nodecl-visitor.h"
#include "cxx-buildscope.h"
#include "cxx-prettyprint.h"
#include <string.h>
#include <ctype.h>

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

    scope_entry_t* global_namespace;
    scope_entry_t* opened_namespace;

    scope_t* current_scope;

    char in_condition;
    nodecl_t condition_top;
    char in_copy_initializer;
    char in_direct_initializer;
    char inside_structured_value;
    char mem_init_list;

    int num_classes_being_defined;
    scope_entry_t* classes_being_defined[MCXX_MAX_SCOPES_NESTING];

    scope_entry_list_t* pending_nested_types_to_define;

    char in_member_declaration;

    char do_not_emit_declarations;
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

static void declare_all_in_template_arguments(nodecl_codegen_visitor_t* visitor, template_parameter_list_t* template_arguments);

#define MAX_WALK_TYPES 2048
int _stack_walked_types_top = 0;
type_t* _stack_walked_types[MAX_WALK_TYPES];

static void walk_type_for_symbols(
        nodecl_codegen_visitor_t *visitor,
        type_t* t,
        char needs_def, 
        void symbol_to_declare(nodecl_codegen_visitor_t*, scope_entry_t*),
        void symbol_to_define(nodecl_codegen_visitor_t*, scope_entry_t*),
        void (*define_entities_in_tree)(nodecl_codegen_visitor_t* visitor, nodecl_t))
{
    if (t == NULL)
        return;

    {
        int i;
        for (i = _stack_walked_types_top - 1; i >= 0; i--)
        {
            if (_stack_walked_types[i] == t)
                return;
        }
    }

    // This poisons return, do not return from this function
#define return 1=1;

    ERROR_CONDITION(_stack_walked_types_top == MAX_WALK_TYPES, "Too many types walked %d!\n", _stack_walked_types_top);
    _stack_walked_types[_stack_walked_types_top] = t;
    _stack_walked_types_top++;

    if (is_named_type(t)
            && named_type_get_symbol(t)->kind == SK_TYPEDEF)
    {
        walk_type_for_symbols(visitor, named_type_get_symbol(t)->type_information, 
                needs_def, 
                symbol_to_declare, symbol_to_define,
                define_entities_in_tree);

        symbol_to_define(visitor, named_type_get_symbol(t));
    }
    else if (is_pointer_type(t))
    {
        walk_type_for_symbols(visitor, pointer_type_get_pointee_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (is_pointer_to_member_type(t))
    {
        walk_type_for_symbols(visitor, pointer_type_get_pointee_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define,
                define_entities_in_tree);

        walk_type_for_symbols(visitor, pointer_to_member_type_get_class_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (is_array_type(t))
    {
        define_entities_in_tree(visitor, array_type_get_array_size_expr(t));
        walk_type_for_symbols(visitor, array_type_get_element_type(t), /* needs_def */ 1, symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        walk_type_for_symbols(visitor, reference_type_get_referenced_type(t), needs_def, symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (is_function_type(t))
    {
        walk_type_for_symbols(visitor, function_type_get_return_type(t), 
                /* needs_def */ 0, symbol_to_declare, symbol_to_define, define_entities_in_tree);
        int i;
        for (i = 0; i < function_type_get_num_parameters(t); i++)
        {
            walk_type_for_symbols(visitor, function_type_get_parameter_type_num(t, i), 
                    /* needs_def */ 0, symbol_to_declare, symbol_to_define, define_entities_in_tree);
        }
    }
    else if (is_vector_type(t))
    {
        walk_type_for_symbols(visitor, vector_type_get_element_type(t), /* needs_def */ 1, symbol_to_declare, symbol_to_define, define_entities_in_tree);
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
                entry_list_iterator_next(it))
        {
            scope_entry_t* member = entry_list_iterator_current(it);
            walk_type_for_symbols(visitor, member->type_information, /* needs_def */ 1, symbol_to_declare, symbol_to_define, define_entities_in_tree);
        }
        entry_list_iterator_free(it);
        entry_list_free(members);
    }
    else if (is_unnamed_enumerated_type(t))
    {
        int i;
        for (i = 0; i < enum_type_get_num_enumerators(t); i++)
        {
            // FIXME - Walk every enumerator
            scope_entry_t* enumerator = enum_type_get_enumerator_num(t, i);
            define_entities_in_tree(visitor, enumerator->value);
        }
    }
    else if (is_named_enumerated_type(t))
    {
        scope_entry_t* enum_entry = named_type_get_symbol(t);
        walk_type_for_symbols(visitor, enum_entry->type_information, /* needs_def */ 1, symbol_to_declare, symbol_to_define, define_entities_in_tree);

        if (needs_def)
        {
            symbol_to_define(visitor, enum_entry);
        }
        else
        {
            symbol_to_declare(visitor, enum_entry);
        }
    }
    else if (is_unresolved_overloaded_type(t))
    {
        scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(t);

        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(unresolved_set);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_sym = entry_list_iterator_current(it);

            if (needs_def)
            {
                symbol_to_define(visitor, current_sym);
            }
            else
            {
                symbol_to_declare(visitor, current_sym);
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(unresolved_set);
    }
    else
    {
        // Do nothing as it will be a builtin type
    }
#undef return

    _stack_walked_types_top--;
    ERROR_CONDITION(_stack_walked_types_top < 0, "Invalid stack index", 0);
}

static void declare_symbol(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void define_symbol(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void declare_symbol_if_nonlocal(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void define_symbol_if_nonlocal(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void declare_symbol_if_local(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void define_symbol_if_local(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void declare_symbol_if_nonnested(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);
static void define_symbol_if_nonnested(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol);

static void define_nonnested_entities_in_trees(nodecl_codegen_visitor_t*, nodecl_t);
static void define_nonlocal_entities_in_trees(nodecl_codegen_visitor_t*, nodecl_t);
static void define_all_entities_in_trees(nodecl_codegen_visitor_t*, nodecl_t);

static void add_to_clear_list(scope_entry_t* entry);


static void define_generic_entities(nodecl_codegen_visitor_t* visitor, nodecl_t node,
        void decl_sym_fun(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol),
        void def_sym_fun(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol),
        void define_entities_fun(nodecl_codegen_visitor_t* visitor, nodecl_t node),
        void define_entry_fun(nodecl_codegen_visitor_t *visitor, 
            nodecl_t node, scope_entry_t* entry,
            void def_sym_fun_2(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol))
        )
{
    if (nodecl_is_null(node))
        return;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        define_generic_entities(visitor, 
                nodecl_get_child(node, i),
                decl_sym_fun,
                def_sym_fun,
                define_entities_fun,
                define_entry_fun
                );
    }

    scope_entry_t* entry = nodecl_get_symbol(node);
    if (entry != NULL
            && entry->type_information != NULL)
    {
        walk_type_for_symbols(visitor, entry->type_information, /* needs_def */ 1, 
                decl_sym_fun,
                def_sym_fun,
                define_entities_fun
                );

        define_entry_fun(visitor, node, entry, def_sym_fun);

        define_generic_entities(visitor, entry->value,
                decl_sym_fun,
                def_sym_fun,
                define_entities_fun,
                define_entry_fun
                );
    }

    type_t* type = nodecl_get_type(node);
    if (type != NULL)
    {
        walk_type_for_symbols(visitor, 
                type, 
                /* needs_def */ 1, 
                decl_sym_fun,
                def_sym_fun,
                define_entities_fun);
    }

    if (nodecl_get_kind(node) == NODECL_CONVERSION)
    {
        // Special cases for conversion nodes 
        //
        // When a pointer or reference to class type (or pointer to member) is
        // converted from a derived class to a base class, it requires both
        // classes be defined but since they are pointers, generic
        // walk_type_for_symbols does not realize this fact. NODECL_CONVERSION
        // nodes appear where a standard conversion has been applied by the
        // frontend during typechecking
        type_t* dest_type = nodecl_get_type(node);
        type_t* source_type = nodecl_get_type(nodecl_get_child(node, 0));

        if ((is_reference_to_class_type(dest_type)
                && is_reference_to_class_type(source_type)
                && class_type_is_base(no_ref(dest_type), no_ref(source_type)))
                || (is_pointer_to_class_type(no_ref(dest_type))
                    && is_pointer_to_class_type(no_ref(source_type))
                    && class_type_is_base(
                        pointer_type_get_pointee_type(no_ref(dest_type)),
                        pointer_type_get_pointee_type(no_ref(source_type))))
                || (is_pointer_to_member_type(no_ref(dest_type))
                    && is_pointer_to_member_type(no_ref(source_type))
                    && class_type_is_base(
                        // This is OK, for pointers to members conversion is Base to Derived (not Derived to Base)
                        pointer_to_member_type_get_class_type(no_ref(source_type)),
                        pointer_to_member_type_get_class_type(no_ref(dest_type)))))
        {
            type_t* base_class = NULL;
            type_t* derived_class = NULL;
            if (is_reference_to_class_type(dest_type)
                        && is_reference_to_class_type(source_type))
            {
                base_class = no_ref(dest_type);
                derived_class = no_ref(source_type);
            }
            else if (is_pointer_to_class_type(no_ref(dest_type))
                    && is_pointer_to_class_type(no_ref(source_type)))
            {
                base_class = pointer_type_get_pointee_type(no_ref(dest_type));
                derived_class = pointer_type_get_pointee_type(no_ref(source_type));
            }
            else if (is_pointer_to_member_type(no_ref(dest_type))
                    && is_pointer_to_member_type(no_ref(source_type)))
            {
                base_class = pointer_type_get_pointee_type(pointer_to_member_type_get_class_type(no_ref(source_type)));
                derived_class = pointer_type_get_pointee_type(pointer_to_member_type_get_class_type(no_ref(dest_type)));
            }
            else
            {
                internal_error("Code unreachable", 0);
            }


            walk_type_for_symbols(visitor, base_class, /* needs_def */ 1, 
                    decl_sym_fun, 
                    def_sym_fun,
                    define_entities_fun);
            walk_type_for_symbols(visitor, derived_class, /* needs_def */ 1, 
                    decl_sym_fun, 
                    def_sym_fun,
                    define_entities_fun);
        }
    }
}

static void entry_just_define(nodecl_codegen_visitor_t *visitor, 
        nodecl_t node UNUSED_PARAMETER, 
        scope_entry_t* entry,
        void def_sym_fun(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
        )
{
    def_sym_fun(visitor, entry);
}

static void entry_local_definition(nodecl_codegen_visitor_t *visitor, 
        nodecl_t node UNUSED_PARAMETER, 
        scope_entry_t* entry,
        void def_sym_fun(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol) 
        )
{
    if (visitor->current_scope == entry->decl_context.current_scope)
    {
        if (nodecl_get_kind(node) != NODECL_OBJECT_INIT)
        {
            def_sym_fun(visitor, entry);
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

static void define_local_entities_in_trees(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    define_generic_entities(visitor, node, 
            declare_symbol_if_local,
            define_symbol_if_local,
            define_local_entities_in_trees,
            entry_local_definition);
}

static void define_nonlocal_entities_in_trees(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    define_generic_entities(visitor, node, 
            declare_symbol_if_nonlocal,
            define_symbol_if_nonlocal,
            define_nonlocal_entities_in_trees,
            entry_just_define);
}

static void define_nonnested_entities_in_trees(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    define_generic_entities(visitor, node, 
            declare_symbol_if_nonnested,
            define_symbol_if_nonnested,
            define_nonnested_entities_in_trees,
            entry_just_define);
}

static void define_all_entities_in_trees(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    define_generic_entities(visitor, node, 
            declare_symbol,
            define_symbol,
            define_all_entities_in_trees,
            entry_just_define);
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

static char symbol_is_same_or_nested_in(scope_entry_t* symbol, scope_entry_t* class_sym)
{
    if (symbol->entity_specs.is_member)
    {
        return symbol_is_same_or_nested_in(
                named_type_get_symbol(symbol->entity_specs.class_type),
                class_sym);
    }
    else
    {
        return symbol == class_sym;
    }
}



// Classes are so complex that they reserve a whole routine for them
static scope_entry_list_t* define_required_before_class(nodecl_codegen_visitor_t* visitor, scope_entry_t* symbol)
{
    static int _num_being_checked_for_required = 0;
    static scope_entry_t* _being_checked_for_required[MCXX_MAX_SCOPES_NESTING] = { 0 };

    visitor->pending_nested_types_to_define = NULL;

    int i;
    for (i = 0; i < _num_being_checked_for_required; i++)
    {
        if (_being_checked_for_required[i] == symbol)
            return NULL;
    }

    ERROR_CONDITION(_num_being_checked_for_required == MCXX_MAX_SCOPES_NESTING, "Too much required before class recursion", 0);
    _being_checked_for_required[_num_being_checked_for_required] = symbol;
    _num_being_checked_for_required++;

    scope_entry_list_iterator_t* it = NULL;
    if (symbol->kind == SK_CLASS)
    {
        if (is_template_specialized_type(symbol->type_information)
                && template_specialized_type_get_template_arguments(symbol->type_information)->num_parameters != 0)
        {
            template_parameter_list_t* template_arguments = template_specialized_type_get_template_arguments(
                    symbol->type_information);
            declare_all_in_template_arguments(visitor, template_arguments);

            type_t* template_type = template_specialized_type_get_related_template_type(symbol->type_information);
            type_t* primary_template = template_type_get_primary_type(template_type);
            scope_entry_t* primary_symbol = named_type_get_symbol(primary_template);

            if (primary_symbol != symbol)
            {
                declare_symbol_if_nonnested(visitor, primary_symbol);
            }
        }

        if (class_type_get_num_bases(symbol->type_information) != 0)
        {
            // We need to define all the bases first
            for (i = 0; i < class_type_get_num_bases(symbol->type_information); i++)
            {
                scope_entry_t* entry = class_type_get_base_num(symbol->type_information, i, NULL, NULL, NULL);
                define_symbol_if_nonnested(visitor, entry);
            }
        }

        scope_entry_list_t* members = class_type_get_members(symbol->type_information);
        for (it = entry_list_iterator_begin(members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* member = entry_list_iterator_current(it);

            if (member->kind == SK_USING)
            {
                // Do nothing with these
            }
            else if (member->kind != SK_CLASS
                    && member->kind != SK_ENUM)
            {
                if (member->kind == SK_VARIABLE
                        && member->entity_specs.is_static
                        && !nodecl_is_null(member->value))
                {
                    define_nonnested_entities_in_trees(visitor, member->value);
                }
                walk_type_for_symbols(visitor, member->type_information, /* needs_def */ 1, 
                        declare_symbol_if_nonnested, 
                        define_symbol_if_nonnested,
                        define_nonnested_entities_in_trees);
            }
            else if (member->kind == SK_ENUM)
            {
                for (i = 0; i < enum_type_get_num_enumerators(member->type_information); i++)
                {
                    scope_entry_t* enumerator = enum_type_get_enumerator_num(member->type_information, i);
                    define_nonnested_entities_in_trees(visitor, enumerator->value);
                }
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(members);


        scope_entry_list_t* friends = class_type_get_friends(symbol->type_information);
        for (it = entry_list_iterator_begin(friends);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* friend = entry_list_iterator_current(it);
            walk_type_for_symbols(visitor, friend->type_information, /* needs_def */ 0, 
                    declare_symbol_if_nonnested, 
                    define_symbol_if_nonnested,
                    define_nonnested_entities_in_trees);

            if (!is_friend_declared(friend))
            {
                declare_symbol_if_nonnested(visitor, friend);
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(friends);
    }
    else if (symbol->kind == SK_ENUM
             || symbol->kind == SK_ENUMERATOR)
    {
        walk_type_for_symbols(visitor, symbol->type_information, /* needs_def */ 0, 
                declare_symbol_if_nonnested, 
                define_symbol_if_nonnested,
                define_nonnested_entities_in_trees);
    }
    else 
    {
        internal_error("Unexpected symbol kind %s\n", symbol_kind_name(symbol));
    }


    // This will compute a list of symbols that must be defined inside the class
    scope_entry_list_t* result = NULL;

    // Remove ourselves from the result list. This happens because nonnested
    // routines add all the enclosing symbols too, and the top most should not
    // be included
    for (it = entry_list_iterator_begin(visitor->pending_nested_types_to_define);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current = entry_list_iterator_current(it);
        if (current != symbol)
        {
            result = entry_list_add_once(result, current);
        }
    }

    visitor->pending_nested_types_to_define = NULL;

    scope_entry_list_t* must_be_defined_inside_class = entry_list_copy(result);

    for (it = entry_list_iterator_begin(must_be_defined_inside_class);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        // Now for each of these symbols define whatever they might require too
        scope_entry_t* entry = entry_list_iterator_current(it);
        scope_entry_list_t* pending_symbols = define_required_before_class(visitor, entry);
        scope_entry_list_t* old_result = result;
        result = entry_list_merge(result, pending_symbols);
        entry_list_free(old_result);
    }

    entry_list_free(must_be_defined_inside_class);

    _num_being_checked_for_required--;

    return result;
}

static void declare_all_in_template_arguments(nodecl_codegen_visitor_t* visitor, template_parameter_list_t* template_arguments)
{
    int i;
    for (i = 0; i < template_arguments->num_parameters; i++)
    {
        template_parameter_value_t* argument =  template_arguments->arguments[i];

        switch (argument->kind)
        {
            case TPK_TYPE:
                {
                    walk_type_for_symbols(visitor,
                            argument->type,
                            /* needs_def */ 0,
                            declare_symbol_if_nonnested,
                            define_symbol_if_nonnested,
                            define_nonnested_entities_in_trees);
                    break;
                }
            case TPK_NONTYPE:
                {
                    walk_type_for_symbols(visitor,
                            argument->type,
                            /* needs_def */ 1,
                            declare_symbol_if_nonnested,
                            define_symbol_if_nonnested,
                            define_nonnested_entities_in_trees);
                    define_nonnested_entities_in_trees(visitor, argument->value);
                    break;
                }
            case TPK_TEMPLATE:
                {
                    declare_symbol(visitor, named_type_get_symbol(argument->type));
                    break;
                }
            default:
                {
                    internal_error("Code unreachable", 0);
                }
        }
    }
}

static char is_member_type(scope_entry_t* t)
{
    return t->kind == SK_ENUM
        || t->kind == SK_CLASS
        || t->kind == SK_TYPEDEF
        /* || t->kind == SK_TEMPLATE */;
}

static char is_member_nontype(scope_entry_t* t)
{
    return !is_member_type(t);
}

static void define_class_symbol_aux(nodecl_codegen_visitor_t* visitor, scope_entry_t* symbol, 
        scope_entry_list_t* symbols_defined_inside_class, int level)
{
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

    // 1. Declaration of the class key part
    C_LANGUAGE()
    {
        indent(visitor);
        // Usual case: the symbol will be already called 'struct/union X' in C
        fprintf(visitor->file, "%s\n", symbol->symbol_name);
        indent(visitor);
        fprintf(visitor->file, "{\n");
    }

    char is_dependent_class = 0;
    CXX_LANGUAGE()
    {
        char is_template_specialized = 0;
        char is_primary_template = 0;

        type_t* template_type = NULL;
        type_t* primary_template = NULL;
        scope_entry_t* primary_symbol = NULL;

        if (is_template_specialized_type(symbol->type_information)
                && template_specialized_type_get_template_arguments(symbol->type_information)->num_parameters != 0)
        {
            is_template_specialized = 1;
            template_type = template_specialized_type_get_related_template_type(symbol->type_information);
            primary_template = template_type_get_primary_type(template_type);
            primary_symbol = named_type_get_symbol(primary_template);

            if (primary_symbol == symbol)
            {
                is_primary_template = 1;
            }

            type_t* class_type = get_actual_class_type(symbol->type_information);
            if (!class_type_is_complete_independent(class_type)
                    && !class_type_is_incomplete_independent(class_type))
            {
                // If this is dependent and it is not the primary template do
                // not continue, declaring the primary should have been enough
                //
                // This may happen for template functions which implicitly name
                // dependent specializations (such as those defined using
                // default template arguments). It also may be caused by a bug
                // in the frontend, though
                if (!is_primary_template)
                {
                    return; 
                }
                is_dependent_class = 1;
            }
        }

        // *** From here everything required should have been declared ***

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
                ERROR_CONDITION(!is_primary_template, "Only the primary template is allowed "
                        "as a dependent template specialized type!\n", 0);

                template_parameter_list_t* template_parameters = template_specialized_type_get_template_arguments(
                        symbol->type_information);

                indent(visitor);
                fprintf(visitor->file, "template <");
                codegen_template_parameters(visitor, template_parameters);
                fprintf(visitor->file, ">\n");
            }
        }

        indent(visitor);
        const char* qualified_name = NULL;
        if (level == 0)
        {
            char is_dependent = 0;
            int max_qualif_level = 0;

            qualified_name = get_class_qualification_of_symbol_without_template(symbol, symbol->decl_context, &is_dependent, &max_qualif_level);

            // Note that this case will already have template arguments if needed
        }
        else
        {
            qualified_name = symbol->symbol_name;
        }
        if (is_template_specialized
                && !is_primary_template)
        {
            qualified_name = strappend(qualified_name, 
                    get_template_arguments_str(symbol, symbol->decl_context));
        }


        if (!symbol->entity_specs.is_anonymous_union)
        {
            fprintf(visitor->file, "%s %s", class_key, qualified_name);
        }
        else
        {
            fprintf(visitor->file, "%s", class_key);
        }

        // From here we assume its already defined
        symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;

        if (level == 0
                && is_primary_template)
        {
            // We do not define primary templates on the outermost level
            fprintf(visitor->file, ";\n");
            return;
        }

        if (class_type_get_num_bases(symbol->type_information) != 0)
        {
            fprintf(visitor->file, " : ");
            int i;
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

    // 2. Now declare members
    scope_entry_list_t* members = class_type_get_members(symbol->type_information);

    access_specifier_t current_access_spec = default_access_spec;

    struct iteration_member_tag
    {
        char (*filter)(scope_entry_t*);
    } filter_set[] = { 
        { is_member_type }, 
        { is_member_nontype }, 
        { NULL } 
    };

    // We have to iterate several times
    int i = 0;
    while (filter_set[i].filter != NULL)
    {
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* member = entry_list_iterator_current(it);
            if (!(filter_set[i].filter)(member))
                continue;

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

            char old_in_member_declaration = visitor->in_member_declaration;
            visitor->in_member_declaration = 1;

            C_LANGUAGE()
            {
                // Everything must be properly defined in C
                define_symbol(visitor, member);
                member->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
            }
            CXX_LANGUAGE()
            {
                if (member->kind == SK_CLASS)
                {
                    if (is_template_specialized_type(member->type_information)
                            && template_specialized_type_get_template_arguments(member->type_information)->num_parameters != 0)
                    {
                        type_t* related_template = template_specialized_type_get_related_template_type(member->type_information);
                        type_t* primary_template = template_type_get_primary_type(related_template);
                        char is_primary_template = (named_type_get_symbol(primary_template) == member);

                        // C++ has a problem here: we cannot explicitly
                        // specialize a member template class in non
                        // namespace scope but we need the definition, here
                        // EXCEPTIONALLY we will emit dependent code
                        // because this language quirk. Other solutions
                        // (like flattening the members) are more
                        // cumbersome, more painstaking and more
                        // painstaking than this one.
                        if (is_primary_template)
                        {
                            // Check every complete specialization
                            char one_specialization_defined_inside_the_class = 0;
                            int j;
                            for (j = 0; 
                                    j < template_type_get_num_specializations(related_template);
                                    j++)
                            {
                                type_t* current_specialization = template_type_get_specialization_num(related_template, j);
                                scope_entry_t* current_spec_sym = named_type_get_symbol(current_specialization);

                                if (class_type_is_complete_independent(current_specialization)
                                        && entry_list_contains(symbols_defined_inside_class, current_spec_sym))
                                {
                                    one_specialization_defined_inside_the_class = 1;
                                }
                            }

                            if (one_specialization_defined_inside_the_class
                                    || is_dependent_class)
                            {
                                define_class_symbol_aux(visitor, member, symbols_defined_inside_class, level + 1);
                                member->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                            }
                            else
                            {
                                declare_symbol(visitor, member);
                                member->entity_specs.codegen_status = CODEGEN_STATUS_DECLARED;
                            }
                        }
                        else
                        {
                            // Do not emit anything but mark the symbols 
                            if (entry_list_contains(symbols_defined_inside_class, member))
                            {
                                member->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                            }
                            else
                            {
                                member->entity_specs.codegen_status = CODEGEN_STATUS_DECLARED;
                            }
                        }
                    }
                    else
                    {
                        if (entry_list_contains(symbols_defined_inside_class, member))
                        {
                            define_class_symbol_aux(visitor, member, symbols_defined_inside_class, level + 1);
                            member->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                        }
                        else
                        {
                            declare_symbol(visitor, member);
                            member->entity_specs.codegen_status = CODEGEN_STATUS_DECLARED;
                        }
                    }
                }
                else if (member->kind == SK_USING)
                {
                    indent(visitor);
                    ERROR_CONDITION(!is_unresolved_overloaded_type(member->type_information), "Invalid SK_USING symbol\n", 0);

                    scope_entry_list_t* used_entities = unresolved_overloaded_type_get_overload_set(member->type_information);
                    scope_entry_t* entry = entry_list_head(used_entities);
                    entry_list_free(used_entities);

                    char is_dependent = 0;
                    int max_qualif_level = 0;
                    fprintf(visitor->file, "using %s;\n", 
                            get_fully_qualified_symbol_name_without_template(entry, 
                                entry->decl_context, 
                                &is_dependent, 
                                &max_qualif_level));
                }
                else if (member->kind == SK_ENUM
                        || member->kind == SK_TYPEDEF)
                {
                    define_symbol(visitor, member);
                    member->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                }
                else 
                {
                    declare_symbol(visitor, member);
                    if (member->kind == SK_VARIABLE 
                            && (!member->entity_specs.is_static
                                || ((is_integral_type(member->type_information) 
                                        || is_enum_type(member->type_information))
                                    && is_const_qualified_type(member->type_information))))
                    {
                        member->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                    }
                    else
                    {
                        member->entity_specs.codegen_status = CODEGEN_STATUS_DECLARED;
                    }
                }
            }
            visitor->in_member_declaration = old_in_member_declaration;

            visitor->indent_level--;

            CXX_LANGUAGE()
            {
                visitor->indent_level--;
            }
        }
        entry_list_iterator_free(it);

        i++;
    }

    entry_list_free(members);

    // 3. Declare friends
    scope_entry_list_t* friends = class_type_get_friends(symbol->type_information);

    scope_entry_list_iterator_t* it = NULL;
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
        if (is_template_specialized_type(friend->type_information)
                && template_specialized_type_get_template_arguments(friend->type_information)->num_parameters != 0)
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
}

static void define_class_symbol(nodecl_codegen_visitor_t* visitor, scope_entry_t* symbol)
{
    // fprintf(stderr, "DEFINING CLASS '%s'\n", get_qualified_symbol_name(symbol, symbol->decl_context));

    scope_entry_list_t* old_pending = visitor->pending_nested_types_to_define;

    ERROR_CONDITION(visitor->num_classes_being_defined >= MCXX_MAX_SCOPES_NESTING, "Too many classes", 0);
    visitor->classes_being_defined[visitor->num_classes_being_defined] = symbol;
    visitor->num_classes_being_defined++;

    scope_entry_list_t* symbols_defined_inside_class = define_required_before_class(visitor, symbol);

    define_class_symbol_aux(visitor, symbol, symbols_defined_inside_class, /* level */ 0);

    visitor->num_classes_being_defined--;

    visitor->pending_nested_types_to_define = old_pending;
}

static char symbol_is_nested_in_defined_classes(nodecl_codegen_visitor_t* visitor, scope_entry_t* symbol);

static void define_symbol(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    ERROR_CONDITION(symbol == NULL, "Invalid symbol", 0);

    if (visitor->do_not_emit_declarations)
        return;

    if (symbol->do_not_print)
        return;

    if (symbol->entity_specs.is_injected_class_name)
        symbol = named_type_get_symbol(symbol->entity_specs.class_type);

    if (symbol->entity_specs.is_member)
    {
        scope_entry_t* class_entry = named_type_get_symbol(symbol->entity_specs.class_type);
        if (!symbol_is_nested_in_defined_classes(visitor, class_entry))
        {
            define_symbol_if_nonnested(visitor, class_entry);
        }
    }

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
                // Template parameters are not to be defined, ever
                if (symbol->entity_specs.is_template_parameter)
                    break;

                codegen_move_to_namespace_of_symbol(visitor, symbol);
                indent(visitor);
                fprintf(visitor->file, "typedef %s;\n",
                        print_decl_type_str(symbol->type_information,
                            symbol->decl_context,
                            symbol->symbol_name));
                break;
            }
        case SK_ENUMERATOR:
            {
                define_symbol(visitor, named_type_get_symbol(symbol->type_information));
                break;
            }
        case SK_ENUM:
            {
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

                    if (!nodecl_is_null(enumerator->value))
                    {
                        fprintf(visitor->file, " = ");
                        codegen_walk(visitor, enumerator->value);
                    }
                }

                visitor->indent_level--;

                fprintf(visitor->file, "\n");
                indent(visitor);
                fprintf(visitor->file, "};\n");
                break;
            }
        case SK_CLASS:
            {
                define_class_symbol(visitor, symbol);
                break;
            }
        case SK_FUNCTION:
            {
                // Functions are not defined but only declared
                declare_symbol(visitor, symbol);
                break;
            }
        case SK_TEMPLATE_PARAMETER:
        case SK_TEMPLATE_TYPE_PARAMETER:
        case SK_TEMPLATE_TEMPLATE_PARAMETER:
            {
                // Do nothing
                break;
            }
        case SK_DEPENDENT_ENTITY:
            {
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
    // First traversal to ensure that everything is declared
    for (i = 0; i < template_parameters->num_parameters; i++)
    {
        template_parameter_t* tpl_param = template_parameters->parameters[i];
        switch (tpl_param->kind)
        {
            case TPK_NONTYPE:
                {
                    walk_type_for_symbols(visitor, tpl_param->entry->type_information,
                            /* needs_def */ 1,
                            declare_symbol_if_nonnested,
                            define_symbol_if_nonnested,
                            define_nonnested_entities_in_trees);
                    break;
                }
            case TPK_TYPE:
            case TPK_TEMPLATE:
                {
                    break;
                }
            default:
                {
                    internal_error("Invalid template parameter kind", 0);
                }
        }
    }
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
    if (visitor->do_not_emit_declarations)
        return;

    if (symbol->entity_specs.is_injected_class_name)
        symbol = named_type_get_symbol(symbol->entity_specs.class_type);

    if (symbol->do_not_print)
        return;

    if (symbol->entity_specs.is_member)
    {
        scope_entry_t* class_entry = named_type_get_symbol(symbol->entity_specs.class_type);
        if (!symbol_is_nested_in_defined_classes(visitor, class_entry))
        {
            define_symbol_if_nonnested(visitor, class_entry);
        }
    }

    // Do nothing if already defined or declared
    if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED
            || symbol->entity_specs.codegen_status == CODEGEN_STATUS_DECLARED)
        return;

    add_to_clear_list(symbol);
    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DECLARED;

    switch (symbol->kind)
    {
        case SK_VARIABLE:
            {
                if (symbol->entity_specs.is_builtin)
                    break;

                const char* decl_specifiers = "";
                const char* gcc_attributes = "";
                const char* declarator = "";

                if (is_named_class_type(symbol->type_information)
                        && named_type_get_symbol(symbol->type_information)->entity_specs.is_anonymous_union)
                {
                    break;
                }

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

                // Emit the initializer for nonmembers and nonstatic members in
                // non member declarations or member declarations if they have
                // integral or enum type
                char emit_initializer = 0;
                if (!nodecl_is_null(symbol->value)
                        && (!symbol->entity_specs.is_member
                            || (symbol->entity_specs.is_static
                                && (!visitor->in_member_declaration
                                    || ((is_integral_type(symbol->type_information) 
                                            || is_enum_type(symbol->type_information))
                                        && is_const_qualified_type(symbol->type_information))))))
                {
                    emit_initializer = 1;
                    define_nonnested_entities_in_trees(visitor, symbol->value);
                }

                codegen_move_to_namespace_of_symbol(visitor, symbol);
                indent(visitor);
                fprintf(visitor->file, "%s%s%s",
                        decl_specifiers, gcc_attributes, declarator);

                // Initializer
                if (emit_initializer)
                {
                    char equal_is_needed = 0;
                    C_LANGUAGE()
                    {
                        equal_is_needed = 1;
                    }

                    CXX03_LANGUAGE()
                    {
                        // We only need = if the initializer is a structured one
                        // and this is C++03, in C++1x syntax { } is always allowed
                        equal_is_needed = 1;
                        if (nodecl_get_kind(symbol->value) == NODECL_FUNCTION_CALL)
                        {
                            scope_entry_t* called_sym = nodecl_get_symbol(nodecl_get_child(symbol->value, 0));
                            nodecl_t arguments = nodecl_get_child(symbol->value, 1);
                            if (!nodecl_is_null(arguments)
                                    && called_sym->entity_specs.is_constructor
                                    && equivalent_types(get_unqualified_type(called_sym->entity_specs.class_type), 
                                        get_unqualified_type(no_ref(symbol->type_information))))
                            {
                                equal_is_needed = 0;
                            }
                        }
                    }

                    if (equal_is_needed)
                    {
                        fprintf(visitor->file, "%s", " = ");

                        char old_in_initializer = visitor->in_copy_initializer;
                        visitor->in_copy_initializer = 1;

                        codegen_walk(visitor, symbol->value);

                        visitor->in_copy_initializer = old_in_initializer;
                    }
                    else
                    {
                        char old_in_initializer = visitor->in_direct_initializer;
                        visitor->in_direct_initializer = 1;

                        fprintf(visitor->file, "(");
                        codegen_walk(visitor, symbol->value);
                        fprintf(visitor->file, ")");

                        visitor->in_direct_initializer = old_in_initializer;
                    }

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
                    if (symbol->entity_specs.is_member)
                    {
                        // A nested symbol can be declared only if we are
                        // defining its immediate enclosing class, otherwise request for a definition of the enclosing class
                        if (visitor->num_classes_being_defined == 0
                                || (visitor->classes_being_defined[visitor->num_classes_being_defined-1] != 
                                    named_type_get_symbol(symbol->entity_specs.class_type)))
                        {
                            define_symbol(visitor, 
                                    named_type_get_symbol(symbol->entity_specs.class_type));
                            return;
                        }
                    }

                    char is_template_specialized = 0;
                    char is_primary_template = 0;

                    type_t* template_type = NULL;
                    type_t* primary_template = NULL;
                    scope_entry_t* primary_symbol = NULL;

                    if (is_template_specialized_type(symbol->type_information)
                            && template_specialized_type_get_template_arguments(symbol->type_information)->num_parameters != 0)
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

                        type_t* class_type = get_actual_class_type(symbol->type_information);
                        template_parameter_list_t* template_arguments = template_specialized_type_get_template_arguments(
                                symbol->type_information);
                        declare_all_in_template_arguments(visitor, template_arguments);

                        if (!class_type_is_complete_independent(class_type)
                                && !class_type_is_incomplete_independent(class_type))
                        {
                            // If this is dependent and it is not the primary template do
                            // not continue, declaring the primary should have been enough
                            //
                            // This may happen for template functions which implicitly name
                            // dependent specializations (such as those defined using
                            // default template arguments). It also may be caused by a bug
                            // in the frontend, though
                            if (!is_primary_template)
                            {
                                return; 
                            }
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
                            ERROR_CONDITION(!is_primary_template, "Only the primary template is allowed "
                                    "as a dependent template specialized type!\n", 0);

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
                        fprintf(visitor->file, "%s", get_template_arguments_str(symbol, symbol->decl_context));
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
                // Typedefs can't be simply declared
                define_symbol(visitor, symbol);
                break;
            }
        case SK_FUNCTION:
            {
                // If this function was not user declared, do not print
                if (symbol->entity_specs.is_member
                        && !symbol->entity_specs.is_user_declared)
                    break;

                walk_type_for_symbols(visitor,
                        symbol->type_information,
                        0,
                        declare_symbol_if_nonlocal,
                        define_symbol_if_nonlocal,
                        define_nonlocal_entities_in_trees);

                char is_primary_template = 0;
                CXX_LANGUAGE()
                {
                    codegen_move_to_namespace_of_symbol(visitor, symbol);

                    if (is_template_specialized_type(symbol->type_information)
                            && template_specialized_type_get_template_arguments(symbol->type_information)->num_parameters != 0)
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
                            is_primary_template = 1;
                        }
                    }

                    // Default arguments
                    //
                    // int i;
                    // int num_param_types = function_type_get_num_parameters(symbol->type_information);
                    // char has_ellipsis = function_type_get_has_ellipsis(symbol->type_information);
                    // if (has_ellipsis)
                    //     num_param_types--;
                    // for (i = 0; i < num_param_types; i++)
                    // {
                    //     if (symbol->entity_specs.default_argument_info != NULL 
                    //             && symbol->entity_specs.default_argument_info[i] != NULL)
                    //     {
                    //         define_all_entities_in_trees(visitor, symbol->entity_specs.default_argument_info[i]->argument);
                    //     }
                    // }
                }

                const char* decl_spec_seq = "";
                if (symbol->entity_specs.is_static)
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
                if (!nodecl_is_null(symbol->entity_specs.asm_specification))
                {
                    nodecl_codegen_visitor_t str_visitor = *visitor;

                    size_t size = 0;
                    FILE* temporal_stream = open_memstream(&asm_specification, &size);

                    str_visitor.file = temporal_stream;
                    codegen_walk(&str_visitor, symbol->entity_specs.asm_specification);
                    fclose(str_visitor.file);
                }

                type_t* real_type = symbol->type_information;
                if (symbol->entity_specs.is_conversion
                        || symbol->entity_specs.is_destructor)
                {
                    real_type = get_new_function_type(NULL, NULL, 0);
                }

                const char* function_name = unmangle_symbol_name(symbol);

                if (is_template_specialized_type(symbol->type_information)
                        // Conversions do not allow templates
                        && !is_primary_template
                        && !symbol->entity_specs.is_conversion)
                {
                    function_name = strappend(function_name, 
                            get_template_arguments_str(symbol, symbol->decl_context));
                }

#if 0
                const char *declarator = print_type_str(function_type_get_return_type(real_type),
                        symbol->decl_context);

                declarator = strappend(declarator, " ");
                declarator = strappend(declarator, function_name);

                declarator = strappend(declarator, "(");

                int num_param_types = function_type_get_num_parameters(real_type);
                char has_ellipsis = function_type_get_has_ellipsis(real_type);
                if (has_ellipsis)
                    num_param_types--;
                for (i = 0; i < num_param_types; i++)
                {
                    if (i > 0)
                    {
                        declarator = strappend(declarator, ", ");
                    }

                    declarator = strappend(declarator, 
                            print_type_str(function_type_get_parameter_type_num(real_type, i), symbol->decl_context));

                    if (symbol->entity_specs.default_argument_info != NULL 
                            && symbol->entity_specs.default_argument_info[i] != NULL)
                    {
                        declarator = strappend(declarator, " = ");
                        declarator = strappend(declarator, 
                                c_cxx_codegen_to_str(symbol->entity_specs.default_argument_info[i]->argument));
                    }
                }

                declarator = strappend(declarator, ")");

                if (is_const_qualified_type(real_type))
                {
                    declarator = strappend(declarator, " const");
                }
                if (is_volatile_qualified_type(real_type))
                {
                    declarator = strappend(declarator, " volatile");
                }
#endif
                const char* declarator = print_decl_type_str(real_type,
                        symbol->decl_context,
                        function_name);

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
                        exception_spec = strappend(exception_spec, ")");
                    }
                }

                indent(visitor);
                fprintf(visitor->file, "%s%s%s%s%s;\n", decl_spec_seq, declarator, exception_spec, asm_specification, gcc_attributes);
                break;
            }
        case SK_TEMPLATE_PARAMETER:
        case SK_TEMPLATE_TYPE_PARAMETER:
        case SK_TEMPLATE_TEMPLATE_PARAMETER:
            {
                // Do nothing
                break;
            }
        case SK_DEPENDENT_ENTITY:
            {
                scope_entry_t* entry = NULL;
                nodecl_t dependent_parts = nodecl_null();
                dependent_typename_get_components(symbol->type_information, &entry, &dependent_parts);

                declare_symbol(visitor, entry);
                // fprintf(visitor->file, "%s", get_qualified_symbol_name(entry, entry->decl_context));
                // if (!nodecl_is_null(dependent_parts))
                // {
                //     fprintf(visitor->file, "::");
                //     codegen_walk(visitor, dependent_parts);
                // }
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
                || entry->decl_context.current_scope->kind == FUNCTION_SCOPE
                || (entry->entity_specs.is_member && is_local_symbol(named_type_get_symbol(entry->entity_specs.class_type))));
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

static void declare_symbol_if_local(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    if (is_local_symbol(symbol))
    {
        declare_symbol(visitor, symbol);
    }
}

static void define_symbol_if_local(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    if (is_local_symbol(symbol))
    {
        define_symbol(visitor, symbol);
    }
}


static char symbol_is_nested_in_defined_classes(nodecl_codegen_visitor_t* visitor, scope_entry_t* symbol)
{
    int i;
    for (i = 0; i < visitor->num_classes_being_defined; i++)
    {
        // If the symbol we are going to define is nested in one of
        // the classes being defined, do not define now. It will be defined later
        if (symbol_is_same_or_nested_in(symbol, visitor->classes_being_defined[i]))
        {
            return 1;
        }
    }

    return 0;
}

static void declare_symbol_if_nonnested(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    if (!symbol_is_nested_in_defined_classes(visitor, symbol)
            || !symbol->entity_specs.is_member)
    {
        declare_symbol(visitor, symbol);
    }
}

static void define_symbol_if_nonnested(nodecl_codegen_visitor_t *visitor, scope_entry_t* symbol)
{
    if (!symbol_is_nested_in_defined_classes(visitor, symbol))
    {
        define_symbol(visitor, symbol);
    }
    else
    {
        visitor->pending_nested_types_to_define = 
            entry_list_add_once(visitor->pending_nested_types_to_define, symbol);
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

    if (entry->entity_specs.is_member)
    {
        define_symbol(visitor, named_type_get_symbol(entry->entity_specs.class_type));
    }

    CXX_LANGUAGE()
    {
        // dependent_name_part_t* dependent_parts = NULL;
        // if (entry->kind == SK_DEPENDENT_ENTITY)
        // {
        //     dependent_typename_get_components(entry->type_information, &entry, &dependent_parts);
        //     dependent_name_part_t* it_dependent_parts = dependent_parts;

        //     while (it_dependent_parts != NULL)
        //     {
        //         if (it_dependent_parts->template_arguments != NULL)
        //         {
        //             declare_all_in_template_arguments(visitor, it_dependent_parts->template_arguments);
        //         }
        //         it_dependent_parts = it_dependent_parts->next;
        //     }
        // }
        
        if (!entry->entity_specs.is_template_parameter
                && !entry->entity_specs.is_builtin)
        {
            fprintf(visitor->file, "%s", get_qualified_symbol_name(entry, entry->decl_context));
        }
        else
        {
            fprintf(visitor->file, "%s", entry->symbol_name);
        }

        // while (dependent_parts != NULL)
        // {
        //     if (dependent_parts->template_arguments != NULL)
        //     {
        //         fprintf(visitor->file, "::template %s<%s>", dependent_parts->name, 
        //                 template_arguments_to_str(dependent_parts->template_arguments, entry->decl_context));
        //     }
        //     else
        //     {
        //         fprintf(visitor->file, "::%s", dependent_parts->name);
        //     }
        //     dependent_parts = dependent_parts->next;
        // }
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

    type_t* t = nodecl_get_type(node);

    if (is_char_type(t))
    {
        unsigned char b = const_value_cast_to_1(value);

        switch (b)
        {
            case '\'' : { fprintf(visitor->file, "%s", "'\\''"); break; }
            case '\\': { fprintf(visitor->file, "%s",  "'\\\\'"); break; }
            case '\a' : { fprintf(visitor->file, "%s", "'\\a'"); break; }
            case '\b' : { fprintf(visitor->file, "%s", "'\\b'"); break; }
            case '\e' : { fprintf(visitor->file, "%s", "'\\e'"); break; } // GCC extension
            case '\f' : { fprintf(visitor->file, "%s", "'\\f'"); break; }
            case '\n' : { fprintf(visitor->file, "%s", "'\\n'"); break; }
            case '\r' : { fprintf(visitor->file, "%s", "'\\r'"); break; }
            case '\t' : { fprintf(visitor->file, "%s", "'\\t'"); break; }
            case '\v' : { fprintf(visitor->file, "%s", "'\\v'"); break; }
            case '\"' : { fprintf(visitor->file, "%s", "'\\\"'"); break; }
            default: {
                         if (isprint(b))
                         {
                             if (is_signed_char_type(t))
                             {
                                 fprintf(visitor->file, "'%c'", (signed char) b);
                             }
                             else
                             {
                                 fprintf(visitor->file, "'%c'", (unsigned char) b);
                             }
                         }
                         else
                         {
                             fprintf(visitor->file, "'\\x%x'", b);
                         }
                     }
        }
    }
    else if (IS_CXX_LANGUAGE && is_wchar_t_type(t))
    {
        unsigned int mb = const_value_cast_to_4(value);
        fprintf(visitor->file, "L'\\x%x'", mb);
    }
    else 
    {
        unsigned long long int v = const_value_cast_to_8(value);

        if (is_signed_int_type(t))
        {
            fprintf(visitor->file, "%lld", (signed long long int)v);
        }
        else if (is_unsigned_int_type(t))
        {
            fprintf(visitor->file, "%lluU", v);
        }
        else if (is_signed_long_int_type(t))
        {
            fprintf(visitor->file, "%lldL", v);
        }
        else if (is_unsigned_long_int_type(t)) 
        {
            fprintf(visitor->file, "%lluLU", v);
        }
        else if (is_signed_long_long_int_type(t))
        {
            fprintf(visitor->file, "%lldLL", v);
        }
        else if (is_unsigned_long_long_int_type(t))
        {
            fprintf(visitor->file, "%lluLLU", v);
        }
        else
        {
            // Remaining integers like 'short'
            if (const_value_is_signed(value))
            {
                fprintf(visitor->file, "%lld", v);
            }
            else
            {
                fprintf(visitor->file, "%llu", v);
            }
        }
    }
}

static void codegen_complex_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    // This is a GCC extension
    //
    // In C this complex literal is created using "2j" so it will always be a
    // literal integer/float and the real part will be zero
    
    // nodecl_t real_part = nodecl_get_child(node, 0); // Zero
    nodecl_t imag_part = nodecl_get_child(node, 0); 

    codegen_walk(visitor, imag_part);
    fprintf(visitor->file, "i"); // we could use 'j' too
}

static void codegen_text(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "%s", nodecl_get_text(node));
}

static const char* quote_c_string(int* c, int length, char is_wchar)
{
    char *quoted_string = NULL;
    size_t size = 0;
    FILE* temporal_stream = open_memstream(&quoted_string, &size);

    if (is_wchar)
    {
        fprintf(temporal_stream, "%s", "L");
    }

    fprintf(temporal_stream, "%s", "\"");

    int i;
    for (i = 0; i < length; i++)
    {
        int current = c[i];

        if (current == '\n')
        {
            fprintf(temporal_stream, "\\n");
        }
        else if (current ==  '\'')
        {
            fprintf(temporal_stream, "\\\'");
        }
        else if (current ==  '"')
        {
            fprintf(temporal_stream, "\\\"");
        }
        else if (current ==  '?')
        {
            fprintf(temporal_stream, "\\\?");
        }
        else if (current ==  '\\')
        {
            fprintf(temporal_stream, "\\\\");
        }
        else if (current ==  '\a')
        {
            fprintf(temporal_stream, "\\\a");
        }
        else if (current ==  '\b')
        {
            fprintf(temporal_stream, "\\\b");
        }
        else if (current ==  '\f')
        {
            fprintf(temporal_stream, "\\\f");
        }
        else if (current ==  '\n')
        {
            fprintf(temporal_stream, "\\\n");
        }
        else if (current ==  '\r')
        {
            fprintf(temporal_stream, "\\\r");
        }
        else if (current ==  '\t')
        {
            fprintf(temporal_stream, "\\\t");
        }
        else if (current ==  '\v')
        {
            fprintf(temporal_stream, "\\\v");
        }
        else if (isprint(current))
        {
            fprintf(temporal_stream, "%c", (char)current);
        }
        // Best effort
        else
        {
            if (!is_wchar
                    || (current < 255))
            {
                fprintf(temporal_stream, "\\x%x", current);
            }
            else
            {
                fprintf(temporal_stream, "\\U%x", current);
            }
        }
    }

    fprintf(temporal_stream, "%s", "\"");
    fclose(temporal_stream);

    const char* result = uniquestr(quoted_string);
    free(quoted_string);
    return result;
}

static void codegen_string_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const_value_t* v = nodecl_get_constant(node);

    int *bytes = NULL;
    int length = 0;
    const_value_string_unpack(v, &bytes, &length);

    type_t* element_type = array_type_get_element_type(no_ref(nodecl_get_type(node)));
    char is_wchar = !is_unsigned_char_type(element_type)
        && !is_signed_char_type(element_type);

    fprintf(visitor->file, "%s", quote_c_string(bytes, length, is_wchar));
    free(bytes);
}

static void codegen_floating_literal(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    const_value_t* value = nodecl_get_constant(node);
    ERROR_CONDITION(value == NULL, "Invalid value", 0);

    type_t* t = nodecl_get_type(node);
    int precision = floating_type_get_info(t)->p + 1;

    if (const_value_is_float(value))
    {
        fprintf(visitor->file, "%.*ef", precision, const_value_cast_to_float(value));
    }
    else if (const_value_is_double(value))
    {
        fprintf(visitor->file, "%.*e", precision, const_value_cast_to_double(value));
    }
    else if (const_value_is_long_double(value))
    {
        fprintf(visitor->file, "%.*LeL", precision, const_value_cast_to_long_double(value));
    }
#ifdef HAVE_QUADMATH_H
    else if (const_value_is_float128(value))
    {
        __float128 f128 = const_value_cast_to_float128(value);
        int n = quadmath_snprintf (NULL, 0, "%.*Qe", precision, f128);
        char c[n+1];
        quadmath_snprintf (c, n, "%.*Qe", precision, f128);
        c[n] = '\0';
        fprintf(visitor->file, "%sQ", c);
    }
#endif
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
        case NODECL_STRUCTURED_VALUE:
        case NODECL_PARENTHESIZED_EXPRESSION:
        case NODECL_BUILTIN_EXPR:
        case NODECL_COMPOUND_EXPRESSION:
            {
                return -1;
            }
        case NODECL_ARRAY_SUBSCRIPT:
        case NODECL_FUNCTION_CALL:
        case NODECL_CLASS_MEMBER_ACCESS:
        case NODECL_PSEUDO_DESTRUCTOR_NAME:
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
        case NODECL_REAL_PART:
        case NODECL_IMAG_PART:
            // FIXME: Missing GCC nodes 
            // FIXME: Do we want them or we can use builtins?
            // case NODECL_ALIGNOF
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
    switch (nodecl_get_kind(n))
    {
        // Special cases
        case NODECL_CONVERSION:
            {
                return get_rank(nodecl_get_child(n, 0));
            }
        default:
            return get_rank_kind(nodecl_get_kind(n), nodecl_get_text(n));
    }
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
    PREFIX_UNARY_EXPRESSION(real_part, "__real__ ") \
    PREFIX_UNARY_EXPRESSION(imag_part, "__imag__ ") \
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
    BINARY_EXPRESSION(offset, ".*") \
    BINARY_EXPRESSION(comma, ", ") 

// BINARY_EXPRESSION(class_member_access, ".") 

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

static void codegen_class_member_access(nodecl_codegen_visitor_t* visitor, nodecl_t node) 
{
    nodecl_t lhs = nodecl_get_child(node, 0); 
    nodecl_t rhs = nodecl_get_child(node, 1); 

    scope_entry_t* sym = nodecl_get_symbol(rhs);

    char is_anonymous = is_named_class_type(sym->type_information)
        && named_type_get_symbol(sym->type_information)->entity_specs.is_anonymous_union;

    char needs_parentheses = operand_has_lower_priority(node, lhs); 
    if (needs_parentheses) 
    { 
        fprintf(visitor->file, "("); 
    } 
    codegen_walk(visitor, lhs); 
    if (needs_parentheses) 
    { 
        fprintf(visitor->file, ")"); 
    } 
    // Do not print anonymous symbols
    if (!is_anonymous)
    {
        fprintf(visitor->file, "%s", "."); 
        needs_parentheses = operand_has_lower_priority(node, rhs); 
        if (needs_parentheses) 
        { 
            fprintf(visitor->file, "("); 
        } 
        codegen_walk(visitor, rhs); 
        if (needs_parentheses) 
        { 
            fprintf(visitor->file, ")"); 
        } 
    }
}

static void codegen_pseudo_destructor_name(nodecl_codegen_visitor_t* visitor, nodecl_t node) 
{
    nodecl_t lhs = nodecl_get_child(node, 0); 
    nodecl_t rhs = nodecl_get_child(node, 1); 

    char needs_parentheses = operand_has_lower_priority(node, lhs); 
    if (needs_parentheses) 
    { 
        fprintf(visitor->file, "("); 
    } 
    codegen_walk(visitor, lhs); 
    if (needs_parentheses) 
    { 
        fprintf(visitor->file, ")"); 
    } 
    fprintf(visitor->file, "%s", "."); 
    codegen_walk(visitor, rhs); 
}

static void codegen_pointer_to_member(nodecl_codegen_visitor_t* visitor, nodecl_t node) 
{
    scope_entry_t* field = nodecl_get_symbol(node);

    fprintf(visitor->file, "&");
    fprintf(visitor->file, "%s", get_qualified_symbol_name(field, field->decl_context));
}

static void codegen_throw(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t expr = nodecl_get_child(node, 0);

    fprintf(visitor->file, "throw");

    if (!nodecl_is_null(expr))
    {
        fprintf(visitor->file, " ");
        codegen_walk(visitor, expr);
    }
}

static void codegen_typeid(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t expr = nodecl_get_child(node, 0);

    fprintf(visitor->file, "typeid(");
    codegen_walk(visitor, expr);
    fprintf(visitor->file, ")");
}

static void codegen_explicit_type_cast(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    type_t* t = nodecl_get_type(node);

    fprintf(visitor->file, "%s", print_type_str(t, visitor->current_sym->decl_context));

    nodecl_t parenthesized_init = nodecl_get_child(node, 0);
    codegen_walk(visitor, parenthesized_init);
}

static void codegen_type(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    type_t* t = nodecl_get_type(node);

    fprintf(visitor->file, "%s", print_type_str(t, visitor->current_sym->decl_context));
}

static void codegen_compound_statement_for_compound_expression(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "{\n");
    visitor->indent_level++;
    nodecl_t statement_seq = nodecl_get_child(node, 0);

    scope_entry_t* scope_symbol = nodecl_get_symbol(node);
    ERROR_CONDITION(scope_symbol == NULL || scope_symbol->kind != SK_SCOPE, "Invalid scoping symbol", 0);

    visitor->current_scope = scope_symbol->decl_context.current_scope;
    define_local_entities_in_trees(visitor, statement_seq);
    visitor->current_scope = NULL;

    codegen_walk(visitor, statement_seq);

    visitor->indent_level--;
    indent(visitor);
    fprintf(visitor->file, "}");
}

static void codegen_compound_expression(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "(");

    nodecl_t compound = nodecl_get_child(node, 0);
    codegen_compound_statement_for_compound_expression(visitor, compound);

    fprintf(visitor->file, ")");
}

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
        fprintf(visitor->file, "(");

        int old_initializer = visitor->in_direct_initializer;
        visitor->in_direct_initializer = 1;
        codegen_walk(visitor, initializer);
        visitor->in_direct_initializer = old_initializer;

        fprintf(visitor->file, ")");
    }
}

static void codegen_sizeof(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t type = nodecl_get_child(node, 0);
    type_t* t = nodecl_get_type(type);

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

static void codegen_function_call_(nodecl_codegen_visitor_t* visitor, nodecl_t node, char is_virtual_call)
{
    nodecl_t called_entity = nodecl_get_child(node, 0);
    nodecl_t arguments = nodecl_get_child(node, 1);

    enum call_kind
    {
        INVALID_CALL = 0,
        ORDINARY_CALL,
        NONSTATIC_MEMBER_CALL,
        STATIC_MEMBER_CALL,
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
            else if (called_symbol->entity_specs.is_static)
            {
                kind = STATIC_MEMBER_CALL;
            }
        }
    }

    switch (kind)
    {
        case ORDINARY_CALL:
        case STATIC_MEMBER_CALL:
            {
                char needs_parentheses = operand_has_lower_priority(node, called_entity);
                if (needs_parentheses) 
                {
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

                if (is_virtual_call)
                {
                    ERROR_CONDITION(called_symbol == NULL, "Virtual call lacks called symbol", 0);
                    fprintf(visitor->file, "%s", unmangle_symbol_name(called_symbol));
                }
                else
                {
                    codegen_walk(visitor, called_entity);
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
                if (!visitor->in_direct_initializer)
                {
                    scope_entry_t* class_symbol = named_type_get_symbol(called_symbol->entity_specs.class_type);
                    fprintf(visitor->file, "%s", get_qualified_symbol_name(class_symbol, class_symbol->decl_context));
                    fprintf(visitor->file, "(");
                }
                char old_initializer = visitor->in_direct_initializer;
                visitor->in_direct_initializer = 0;
                walk_expression_list(visitor, arguments);
                visitor->in_direct_initializer = old_initializer;
                if (!visitor->in_direct_initializer)
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

static void codegen_function_call(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_function_call_(visitor, node, /* is_virtual_call */ 0);
}

static void codegen_virtual_function_call(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_function_call_(visitor, node, /* is_virtual_call */ 1);
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
    else if (strcmp(builtin_name, "unknown-pragma") == 0)
    {
        fprintf(visitor->file, "#pragma ");
        codegen_walk(visitor, any_list);
        fprintf(visitor->file, "\n");
    }
    else
    {
        internal_error("Unhandled '%s' builtin at %s\n", builtin_name, nodecl_get_locus(node));
    }
}

static void codegen_structured_value(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t items = nodecl_get_child(node, 0);
    type_t* type = nodecl_get_type(node);

    enum structured_value_kind 
    {
        INVALID = 0,
        // (T) { expr-list }
        GCC_POSTFIX,
        // T(single-expression)
        CXX03_EXPLICIT,
        // T{expr-list}
        CXX1X_EXPLICIT,
    } kind = INVALID;

    if (IS_C_LANGUAGE)
    {
        kind = GCC_POSTFIX;
    }
    else if (IS_CXX03_LANGUAGE)
    {
        if ((nodecl_is_null(items)
                    || (nodecl_list_length(items) == 1))
                && (is_named_type(type) || is_builtin_type(no_ref(type))))
        {
            kind = CXX03_EXPLICIT;
        }
        else
        {
            kind = GCC_POSTFIX;
        }
    }
    else if (IS_CXX1X_LANGUAGE)
    {
        if (is_vector_type(no_ref(type)))
        {
            // This is nonstandard, lets fallback to gcc
            kind = GCC_POSTFIX;
        }
        else if (is_named_type(type))
        {
            kind = CXX1X_EXPLICIT;
        }
        else
        {
            // If this is not a named type fallback to gcc
            kind = GCC_POSTFIX;
        }
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    if (visitor->inside_structured_value)
    {
        kind = GCC_POSTFIX;
    }

    switch (kind)
    {
        // (T) { expr-list }
        case GCC_POSTFIX:
            {
                if (!visitor->in_copy_initializer
                        && !visitor->in_direct_initializer
                        && !visitor->inside_structured_value)
                {
                    fprintf(visitor->file, "(%s)", print_type_str(type, visitor->current_sym->decl_context));
                }

                char old_in_direct_initializer = visitor->in_direct_initializer;
                char old_in_copy_initializer = visitor->in_copy_initializer;

                visitor->in_copy_initializer = 0;
                visitor->in_direct_initializer = 0;

                char inside_structured_value = visitor->inside_structured_value;
                visitor->inside_structured_value = 1;

                fprintf(visitor->file, "{ ");
                walk_expression_list(visitor, items);
                fprintf(visitor->file, " }");

                visitor->inside_structured_value = inside_structured_value;
                visitor->in_copy_initializer = old_in_copy_initializer;
                visitor->in_direct_initializer = old_in_direct_initializer;
                break;
            }
        case CXX03_EXPLICIT:
            {
                if (!visitor->in_direct_initializer
                        || nodecl_is_null(items))
                {
                    fprintf(visitor->file, "%s", print_type_str(type, visitor->current_sym->decl_context));
                }

                char old_in_direct_initializer = visitor->in_direct_initializer;
                char old_in_copy_initializer = visitor->in_copy_initializer;

                visitor->in_copy_initializer = 0;
                visitor->in_direct_initializer = 0;

                char inside_structured_value = visitor->inside_structured_value;
                visitor->inside_structured_value = 0;

                if (nodecl_is_null(items))
                {
                    fprintf(visitor->file, "()");
                }
                else
                {
                    if (!visitor->in_copy_initializer)
                    {
                        fprintf(visitor->file, "(");
                    }
                    walk_expression_list(visitor, items);
                    if (!visitor->in_copy_initializer)
                    {
                        fprintf(visitor->file, ")");
                    }
                }

                visitor->inside_structured_value = inside_structured_value;
                visitor->in_copy_initializer = old_in_copy_initializer;
                visitor->in_direct_initializer = old_in_direct_initializer;
                break;
                break;
            }
        case CXX1X_EXPLICIT:
            {
                if (!visitor->in_direct_initializer)
                {
                    fprintf(visitor->file, "%s", print_type_str(type, visitor->current_sym->decl_context));
                }

                char old_in_direct_initializer = visitor->in_direct_initializer;
                char old_in_copy_initializer = visitor->in_copy_initializer;

                visitor->in_copy_initializer = 0;
                visitor->in_direct_initializer = 0;

                char inside_structured_value = visitor->inside_structured_value;
                visitor->inside_structured_value = 1;

                fprintf(visitor->file, "{ ");
                walk_expression_list(visitor, items);
                fprintf(visitor->file, " }");

                visitor->inside_structured_value = inside_structured_value;
                visitor->in_copy_initializer = old_in_copy_initializer;
                visitor->in_direct_initializer = old_in_direct_initializer;
                break;
            }
        default:
            {
                internal_error("Code unreachable", 0);
            }
    }
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
    nodecl_t statement = nodecl_get_child(node, 1);
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
        int old_indent = visitor->indent_level;
        visitor->indent_level = 0;

        visitor->in_condition = 1;
        visitor->condition_top = name;

        codegen_walk(visitor, name);

        visitor->indent_level = old_indent;
        visitor->condition_top = old_condition_top;
        visitor->in_condition = old_condition;
    }

    fprintf(visitor->file, ")\n");

    codegen_walk(visitor, statement);
}

static void codegen_try_block(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t statement = nodecl_get_child(node, 0);
    nodecl_t catch_handlers = nodecl_get_child(node, 1);
    nodecl_t any_catch_handler = nodecl_get_child(node, 2);
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

    visitor->current_scope = scope_symbol->decl_context.current_scope;
    define_local_entities_in_trees(visitor, statement_seq);
    visitor->current_scope = NULL;

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
        walk_type_for_symbols(visitor, entry->type_information, 
                /* needs def */ 1,
                declare_symbol,
                define_symbol,
                define_all_entities_in_trees);

        entry->entity_specs.codegen_status = CODEGEN_STATUS_NONE;
        define_symbol(visitor, entry);
    }
    else
    {
        nodecl_t nodecl_init_expr = nodecl_get_child(node, 0);

        fprintf(visitor->file, "%s(", entry->symbol_name);
        char in_direct_initializer = visitor->in_direct_initializer;
        visitor->in_direct_initializer = 1;
        codegen_walk(visitor, nodecl_init_expr);
        visitor->in_direct_initializer = in_direct_initializer;
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

    if (nodecl_list_length(statement_seq) != 1)
    {
        internal_error("C/C++ functions only have one statement", 0);
    }

    nodecl_t statement = nodecl_list_head(statement_seq);

    scope_entry_t* symbol = nodecl_get_symbol(node);
    ERROR_CONDITION(symbol == NULL || symbol->kind != SK_FUNCTION, "Invalid symbol", 0);

    if (symbol->entity_specs.is_member)
    {
        scope_entry_t* class_symbol = named_type_get_symbol(symbol->entity_specs.class_type);
        define_symbol(visitor, class_symbol);
    }
    else
    {
        if (is_template_specialized_type(symbol->type_information)
                && template_specialized_type_get_template_arguments(symbol->type_information)->num_parameters != 0)

        {
            type_t* template_type = template_specialized_type_get_related_template_type(symbol->type_information);
            type_t* primary_template = template_type_get_primary_type(template_type);
            scope_entry_t* primary_symbol = named_type_get_symbol(primary_template);
            declare_symbol(visitor, primary_symbol);
        }
    }

    walk_type_for_symbols(visitor, function_type_get_return_type(symbol->type_information),
            /* needs_def */ 1,
            declare_symbol,
            define_symbol,
            define_all_entities_in_trees);

    visitor->current_sym = symbol;

    int i;
    int num_params = function_type_get_num_parameters(symbol->type_information);
    if (function_type_get_has_ellipsis(symbol->type_information))
        num_params--;

    for (i = 0; i < num_params; i++)
    {
        walk_type_for_symbols(visitor, function_type_get_parameter_type_num(symbol->type_information, i),
                /* needs_def */ 1,
                declare_symbol,
                define_symbol,
                define_all_entities_in_trees);
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
    if (!nodecl_is_null(symbol->entity_specs.asm_specification))
    {
        nodecl_codegen_visitor_t str_visitor = *visitor;

        size_t size = 0;
        FILE* temporal_stream = open_memstream(&asm_specification, &size);

        str_visitor.file = temporal_stream;
        codegen_walk(&str_visitor, symbol->entity_specs.asm_specification);
        fclose(str_visitor.file);
    }

    char is_dependent = 0;
    int max_qualif_level = 0;
    const char* qualified_name = get_class_qualification_of_symbol_without_template(symbol, symbol->decl_context, &is_dependent, &max_qualif_level);

    if (is_template_specialized_type(symbol->type_information)
            // Conversions do not allow templates
            && !symbol->entity_specs.is_conversion)
    {
        qualified_name = strappend(qualified_name, 
                get_template_arguments_str(symbol, symbol->decl_context));
    }

    // Note that we _must_ ignore typedefs for a function type in a definition
    type_t* real_type = advance_over_typedefs(symbol->type_information);
    if (symbol->entity_specs.is_conversion
            || symbol->entity_specs.is_destructor)
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
            exception_spec = strappend(exception_spec, ")");
        }
    }

    codegen_move_to_namespace_of_symbol(visitor, symbol);

    if (is_template_specialized_type(symbol->type_information)
            && template_specialized_type_get_template_arguments(symbol->type_information)->num_parameters != 0)
    {
        indent(visitor);
        fprintf(visitor->file, "template <>\n");
    }

    indent(visitor);
    fprintf(visitor->file, "%s%s%s%s%s\n", decl_spec_seq, gcc_attributes, declarator, exception_spec, asm_specification);

    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;

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

static void codegen_c99_field_designator(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, ".");
    codegen_walk(visitor, nodecl_get_child(node, 0));
}

static void codegen_c99_index_designator(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "[");
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, "]");
}

static void codegen_c99_designated_initializer(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_walk(visitor, nodecl_get_child(node, 0));
    fprintf(visitor->file, " = ");
    codegen_walk(visitor, nodecl_get_child(node, 1));
}

static void codegen_cxx_equal_initializer(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, " = ");
    codegen_walk(visitor, nodecl_get_child(node, 0));
}

static void codegen_cxx_dep_name_simple(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "%s", nodecl_get_text(node));
}

static void codegen_cxx_dep_template_id(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    codegen_walk(visitor, nodecl_get_child(node, 0));

    fprintf(visitor->file, "%s", 
            template_arguments_to_str(nodecl_get_template_parameters(node),  
                CURRENT_COMPILED_FILE->global_decl_context));
}

static void codegen_cxx_dep_name_nested(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    walk_list(visitor, nodecl_get_child(node, 0), "::");
}

static void codegen_cxx_dep_global_name_nested(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "::");
    codegen_cxx_dep_name_nested(visitor, node);
}

static void codegen_cxx_dep_name_conversion(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "operator %s", 
            print_type_str(nodecl_get_type(node), 
                CURRENT_COMPILED_FILE->global_decl_context));
}

static void codegen_cxx_parenthesized_initializer(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "(");
    if (!nodecl_is_null(nodecl_get_child(node, 0)))
    {
        codegen_walk(visitor, nodecl_get_child(node, 0));
    }
    fprintf(visitor->file, ")");
}

static void codegen_cxx_braced_initializer(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "{");
    if (!nodecl_is_null(nodecl_get_child(node, 0)))
    {
        codegen_walk(visitor, nodecl_get_child(node, 0));
    }
    fprintf(visitor->file, "}");
}

static void codegen_offsetof(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    fprintf(visitor->file, "__builtin_offsetof(");

    codegen_walk(visitor, nodecl_get_child(node, 0));

    fprintf(visitor->file, ", ");

    // Except for the first, the remaining must be printed as usual
    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(node, 1), &num_items);

    int i;
    for (i = 0; i < num_items; i++)
    {
        if (i == 0)
        {
            ERROR_CONDITION((nodecl_get_kind(list[i]) != NODECL_C99_FIELD_DESIGNATOR), "Invalid node", 0);

            fprintf(visitor->file, "%s", nodecl_get_text(nodecl_get_child(list[0], 0)));
        }
        else
        {
            codegen_walk(visitor, list[i]);
        }
    }

    fprintf(visitor->file, ")");
}

static void codegen_conversion(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    // Do nothing
    codegen_walk(visitor, nodecl_get_child(node, 0));
}

static void codegen_err_expr(nodecl_codegen_visitor_t* visitor, nodecl_t node UNUSED_PARAMETER)
{
    fprintf(visitor->file, "<<error expression>>");
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
    NODECL_VISITOR(codegen_visitor)->visit_complex_literal = codegen_visitor_fun(codegen_complex_literal);
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
    NODECL_VISITOR(codegen_visitor)->visit_virtual_function_call = codegen_visitor_fun(codegen_virtual_function_call);
    NODECL_VISITOR(codegen_visitor)->visit_cast = codegen_visitor_fun(codegen_cast);
    NODECL_VISITOR(codegen_visitor)->visit_sizeof = codegen_visitor_fun(codegen_sizeof);
    NODECL_VISITOR(codegen_visitor)->visit_conditional_expression = codegen_visitor_fun(codegen_conditional_expression);
    NODECL_VISITOR(codegen_visitor)->visit_builtin_decl = codegen_visitor_fun(codegen_builtin);
    NODECL_VISITOR(codegen_visitor)->visit_builtin_expr = codegen_visitor_fun(codegen_builtin);
    NODECL_VISITOR(codegen_visitor)->visit_any_list = codegen_visitor_fun(codegen_any_list);
    NODECL_VISITOR(codegen_visitor)->visit_structured_value = codegen_visitor_fun(codegen_structured_value);
    NODECL_VISITOR(codegen_visitor)->visit_field_designator = codegen_visitor_fun(codegen_field_designator);
    NODECL_VISITOR(codegen_visitor)->visit_index_designator = codegen_visitor_fun(codegen_index_designator);
    NODECL_VISITOR(codegen_visitor)->visit_array_subscript = codegen_visitor_fun(codegen_array_subscript);
    NODECL_VISITOR(codegen_visitor)->visit_text = codegen_visitor_fun(codegen_text);
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
    NODECL_VISITOR(codegen_visitor)->visit_class_member_access = codegen_visitor_fun(codegen_class_member_access);
    NODECL_VISITOR(codegen_visitor)->visit_pseudo_destructor_name = codegen_visitor_fun(codegen_pseudo_destructor_name);
    NODECL_VISITOR(codegen_visitor)->visit_pointer_to_member = codegen_visitor_fun(codegen_pointer_to_member);
    NODECL_VISITOR(codegen_visitor)->visit_compound_expression = codegen_visitor_fun(codegen_compound_expression);
    NODECL_VISITOR(codegen_visitor)->visit_typeid = codegen_visitor_fun(codegen_typeid);
    NODECL_VISITOR(codegen_visitor)->visit_type = codegen_visitor_fun(codegen_type);

    NODECL_VISITOR(codegen_visitor)->visit_conversion = codegen_visitor_fun(codegen_conversion);

    NODECL_VISITOR(codegen_visitor)->visit_err_expr = codegen_visitor_fun(codegen_err_expr);

    NODECL_VISITOR(codegen_visitor)->visit_c99_field_designator = codegen_visitor_fun(codegen_c99_field_designator);
    NODECL_VISITOR(codegen_visitor)->visit_c99_index_designator = codegen_visitor_fun(codegen_c99_index_designator);
    NODECL_VISITOR(codegen_visitor)->visit_c99_designated_initializer = codegen_visitor_fun(codegen_c99_designated_initializer);
    NODECL_VISITOR(codegen_visitor)->visit_offsetof = codegen_visitor_fun(codegen_offsetof);

    NODECL_VISITOR(codegen_visitor)->visit_cxx_parenthesized_initializer = codegen_visitor_fun(codegen_cxx_parenthesized_initializer);
    NODECL_VISITOR(codegen_visitor)->visit_cxx_braced_initializer = codegen_visitor_fun(codegen_cxx_braced_initializer);
    NODECL_VISITOR(codegen_visitor)->visit_cxx_equal_initializer = codegen_visitor_fun(codegen_cxx_equal_initializer);

    NODECL_VISITOR(codegen_visitor)->visit_cxx_dep_name_simple = codegen_visitor_fun(codegen_cxx_dep_name_simple);
    NODECL_VISITOR(codegen_visitor)->visit_cxx_dep_template_id = codegen_visitor_fun(codegen_cxx_dep_template_id);
    NODECL_VISITOR(codegen_visitor)->visit_cxx_dep_global_name_nested = codegen_visitor_fun(codegen_cxx_dep_global_name_nested);
    NODECL_VISITOR(codegen_visitor)->visit_cxx_dep_name_nested = codegen_visitor_fun(codegen_cxx_dep_name_nested);
    NODECL_VISITOR(codegen_visitor)->visit_cxx_dep_name_conversion = codegen_visitor_fun(codegen_cxx_dep_name_conversion);

    NODECL_VISITOR(codegen_visitor)->visit_cxx_explicit_type_cast = codegen_visitor_fun(codegen_explicit_type_cast);
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
    memset(&codegen_visitor, 0, sizeof(codegen_visitor));

    c_cxx_codegen_init(&codegen_visitor);

    codegen_visitor.file = temporal_stream;
    codegen_visitor.indent_level = 0;
    decl_context_t global_context = scope_link_get_decl_context(CURRENT_COMPILED_FILE->scope_link, nodecl_get_ast(node));
    codegen_visitor.current_sym = global_context.global_scope->related_entry;
    codegen_visitor.global_namespace = codegen_visitor.current_sym;
    codegen_visitor.opened_namespace = codegen_visitor.global_namespace;

    codegen_visitor.do_not_emit_declarations = 1;

    codegen_walk(&codegen_visitor, node);

    fclose(temporal_stream);

    return str;
}
