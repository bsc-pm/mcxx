/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-driver.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-prettyprint.h"
#include "cxx-cexpr.h"
#include "cxx-ambiguity.h"
#include "cxx-buildscope.h"
#include "hash.h"
#include "hash_iterator.h"

static scope_t* new_scope(void)
{
    scope_t* sc = calloc(1, sizeof(*sc));
    sc->hash = hash_create(HASH_SIZE, HASHFUNC(prime_hash), KEYCMPFUNC(strcmp));

    return sc;
}

char same_scope(scope_t* stA, scope_t* stB)
{
    return (stA->hash == stB->hash);
}

// static char is_not_incomplete(scope_entry_t* entry)
// {
//     return (entry->type_information->type->incomplete == 0);
// }

// Creates a new namespace scope, a new global scope is created by just
// passing a NULL enclosing namespace
scope_t* new_namespace_scope(scope_t* enclosing_scope, char* qualification_name)
{
    scope_t* result = new_scope();

    if (qualification_name != NULL)
    {
        result->qualification_name = strdup(qualification_name);
    }

    result->kind = NAMESPACE_SCOPE;
    result->contained_in = enclosing_scope;

    result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;

    return result;
}

// Creates a new prototype scope
scope_t* new_prototype_scope(scope_t* enclosing_scope)
{
    scope_t* result = new_scope();
    result->kind = PROTOTYPE_SCOPE;

    result->contained_in = enclosing_scope;

    result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;

    return result;
}

// Creates a new block scope
scope_t* new_block_scope(scope_t* enclosing_scope, scope_t* prototype_scope, scope_t* function_scope)
{
    scope_t* result = new_scope();

    result->kind = BLOCK_SCOPE;
    result->prototype_scope = prototype_scope;
    result->function_scope = function_scope;
    result->contained_in = enclosing_scope;

    result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;
    
    // Create artificial entry for the block scope
    static int scope_number = 0;
    char* c = calloc(256, sizeof(char));
    sprintf(c, "(#%d)", scope_number);
    scope_number++;

    scope_entry_t* new_block_scope_entry = new_symbol(enclosing_scope, c);
    new_block_scope_entry->kind = SK_SCOPE;

    new_block_scope_entry->related_scope = result;

    new_block_scope_entry->defined = 1;
    
    return result;
}

// Creates a new function scope
scope_t* new_function_scope(scope_t* enclosing_scope, scope_t* prototype_scope)
{
    scope_t* result = new_scope();
    
    result->kind = FUNCTION_SCOPE;
    result->prototype_scope = prototype_scope;
    result->contained_in = enclosing_scope;

    result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;
    
    return result;
}

// Creates a new class scope
scope_t* new_class_scope(scope_t* enclosing_scope, char* qualification_name)
{
    scope_t* result = new_scope();

    result->kind = CLASS_SCOPE;

    if (qualification_name != NULL)
    {
        result->qualification_name = strdup(qualification_name);
    }

    result->contained_in = enclosing_scope;

    result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;
    
    return result;
}

scope_t* new_template_scope(scope_t* enclosing_scope)
{
    scope_t* result = new_scope();

    result->kind = TEMPLATE_SCOPE;
    // result->contained_in = enclosing_scope;
    // result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;

    return result;
}

scope_entry_t* new_symbol(scope_t* sc, char* name)
{
    scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, name);

    scope_entry_t* result;

    result = calloc(1, sizeof(*result));
    result->symbol_name = strdup(name);
    result->scope = copy_scope(sc);

    if (result_set != NULL)
    {
        scope_entry_list_t* new_set = (scope_entry_list_t*) calloc(1, sizeof(*new_set));

        // Put the new entry in front of the previous
        *new_set = *result_set;

        result_set->next = new_set;
        result_set->entry = result;
    }
    else
    {
        result_set = (scope_entry_list_t*) calloc(1, sizeof(*result_set));
        result_set->entry = result;
        result_set->next = NULL; // redundant, though

        hash_put(sc->hash, name, result_set);
    }

    return result;
}

static char* scope_names[] =
{
    [UNDEFINED_SCOPE] = "UNDEFINED_SCOPE",
    [NAMESPACE_SCOPE] = "NAMESPACE_SCOPE",
    [FUNCTION_SCOPE] = "FUNCTION_SCOPE",
    [PROTOTYPE_SCOPE] = "PROTOTYPE_SCOPE",
    [BLOCK_SCOPE] = "BLOCK_SCOPE",
    [CLASS_SCOPE] = "CLASS_SCOPE",
    [TEMPLATE_SCOPE] = "TEMPLATE_SCOPE",
};

scope_entry_list_t* query_in_symbols_of_scope(scope_t* sc, char* name)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Looking in symbols of %s scope %p -> '%s'...", scope_names[sc->kind], sc, name);
    }
    scope_entry_list_t* result = (scope_entry_list_t*) hash_get(sc->hash, name);

    DEBUG_CODE()
    {
        if (result == NULL)
        {
            fprintf(stderr, "not found\n");
        }
        else
        {
            fprintf(stderr, "found\n");
        }
    }

    return result;
}

/*
 * Insert entry in the scope
 */
void insert_entry(scope_t* sc, scope_entry_t* entry)
{
    ERROR_CONDITION((entry->symbol_name == NULL), "Inserting a symbol entry without name!", 0);
    
    scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, entry->symbol_name);


    if (result_set != NULL)
    {
        scope_entry_list_t* new_set = (scope_entry_list_t*) calloc(1, sizeof(*new_set));

        // Put the new entry in front of the previous
        *new_set = *result_set;

        result_set->next = new_set;
        result_set->entry = entry;
    }
    else
    {
        result_set = (scope_entry_list_t*) calloc(1, sizeof(*result_set));
        result_set->entry = entry;
        result_set->next = NULL; // redundant, though

        hash_put(sc->hash, entry->symbol_name, result_set);
    }
}


void remove_entry(scope_t* sc, scope_entry_t* entry)
{
    ERROR_CONDITION((entry->symbol_name == NULL), "Removing a symbol entry without name!", 0);

    scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, entry->symbol_name);

    scope_entry_list_t* current = result_set;
    scope_entry_list_t* previous = NULL;

    while (current != NULL)
    {
        if (current->entry == entry)
        {
            if (previous != NULL)
            {
                // Unlink from the structure
                previous->next = current->next;
            }
            else
            {
                // Delete the whole entry
                hash_delete(sc->hash, entry->symbol_name);
            }
            break;
        }

        previous = current;
        current = current->next;
    }
}

/*
 * Returns the scope of this nested name specification
 */
scope_t* query_nested_name_spec(scope_t* sc, AST global_op, AST nested_name,
        scope_entry_list_t** result_entry_list, char* is_dependent,
        decl_context_t decl_context)
{
    return query_nested_name_spec_flags(sc, global_op, nested_name, 
            result_entry_list, is_dependent, LF_NONE,
            decl_context);
}

scope_t* query_nested_name_spec_flags(scope_t* sc, AST global_op, AST
        param_nested_name, scope_entry_list_t** result_entry_list, char*
        is_dependent, lookup_flags_t lookup_flags, decl_context_t decl_context)
{
    int qualif_level = 0;
    // We'll start the search in "sc"
    scope_t* lookup_scope = sc;
    scope_entry_list_t* entry_list = NULL;

    *is_dependent = 0;

    // We've been told to look up in the first enclosing namespace scope
    if (BITMAP_TEST(lookup_flags, LF_IN_NAMESPACE_SCOPE))
    {
        lookup_scope = copy_scope(enclosing_namespace_scope(lookup_scope));

        ERROR_CONDITION((lookup_scope == NULL), "Namespace scope not found!\n", 0);

        lookup_scope->template_scope = sc->template_scope;
    }

    // unless we are told to start in the global scope
    if (global_op != NULL)
    {
        lookup_scope = CURRENT_COMPILED_FILE(global_scope);
    }

    lookup_scope = copy_scope(lookup_scope);
    lookup_scope->template_scope = sc->template_scope;

    AST nested_name = param_nested_name;
    char seen_class = 0;
    // Traverse the qualification tree
    while (nested_name != NULL)
    {
        AST nested_name_spec = ASTSon0(nested_name);

        switch (ASTType(nested_name_spec))
        {
            case AST_SYMBOL :
                {
                    // Do nothing if we have seen that this is dependent
                    if (*is_dependent)
                    {
                        break;
                    }

                    if (qualif_level == 0)
                    {
                        entry_list = query_unqualified_name(lookup_scope, ASTText(nested_name_spec));
                    }
                    else
                    {
                        entry_list = query_in_symbols_of_scope(lookup_scope, ASTText(nested_name_spec));
                    }

                    // If not found, null
                    if (entry_list == NULL)
                    {
                        return NULL;
                    }

                    // Now filter for SK_CLASS or SK_NAMESPACE
                    enum cxx_symbol_kind filter[5] = {SK_CLASS, SK_NAMESPACE, SK_TYPEDEF, 
                        SK_TEMPLATE_TYPE_PARAMETER, SK_TEMPLATE_TEMPLATE_PARAMETER};
                    entry_list = filter_symbol_kind_set(entry_list, 5, filter);

                    if (entry_list == NULL)
                    {
                        return NULL;
                    }

                    scope_entry_t* entry = entry_list->entry;


                    if (entry->kind == SK_TYPEDEF)
                    {
                        // Advance over typedefs
                        type_t* aliased_type = entry->type_information;
                        aliased_type = advance_over_typedefs(aliased_type);

                        // Now get the entry_t*
                        // If this is a typedef it can only be a typedef
                        // against a type that should be a typename or a
                        // template dependent type
                        if (aliased_type->kind != TK_DIRECT
                                || (aliased_type->type->kind != STK_USER_DEFINED
                                    && aliased_type->type->kind != STK_TEMPLATE_DEPENDENT_TYPE))
                        {
                            return NULL;
                        }

                        if ((aliased_type->type->kind == STK_TEMPLATE_DEPENDENT_TYPE)
                                && BITMAP_TEST(lookup_flags, LF_EXPRESSION))
                        {
                            *is_dependent = 1;
                            return NULL;
                        }

                        entry = aliased_type->type->user_defined_type;

                        if (entry == NULL)
                        {
                            return NULL;
                        }

                        // We need to instantiate it
                        // If this is not an expression
                        if (!BITMAP_TEST(lookup_flags, LF_EXPRESSION))
                        {
                            // And the entry is a specialized class
                            if (entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
                                    // and the symbol is not complete but independent (thus it can be instantiated)
                                    && (entry->type_information->type->template_nature == TPN_INCOMPLETE_INDEPENDENT)
                                    && !is_dependent_type(entry->type_information, decl_context)
                                    && !BITMAP_TEST(lookup_flags, LF_NO_INSTANTIATE))
                            {
                                // Instantiation happenning here
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Instantiation of '%s' within qualified name lookup\n", entry->symbol_name);
                                }
                                scope_entry_list_t* candidates = query_in_symbols_of_scope(entry->scope, entry->symbol_name);

                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Filtering entry %p from candidate list\n", entry);
                                }
                                candidates = filter_entry_from_list(candidates, entry);
                                // candidates = filter_symbol_using_predicate(candidates, is_not_incomplete);

                                template_argument_list_t* current_template_arguments = 
                                    entry->type_information->type->template_arguments;

                                matching_pair_t* matched_template = NULL;
                                
                                matched_template = solve_template(candidates,
                                        current_template_arguments, entry->scope, 0, decl_context);

                                // while (matched_template != NULL
                                //      && matched_template->entry->type_information->type->template_class_body == NULL)
                                // {
                                //  DEBUG_CODE()
                                //  {
                                //      fprintf(stderr, "This specialization has empty body\n", 0);
                                //  }
                                //  candidates = filter_entry_from_list(candidates, matched_template->entry);
                                //  matched_template = solve_template(candidates,
                                //          current_template_arguments, entry->scope, 0, decl_context);
                                // }

                                // if (matched_template == NULL)
                                // {
                                //  internal_error("Sux\n", 0);
                                // }
                                // else
                                {
                                    instantiate_template_in_symbol(entry, matched_template, 
                                            current_template_arguments, entry->scope, decl_context);
                                }
                            }
                        }
                        else
                        {
                            // If we are in context of an expression lookup and the entity found is 
                            // a dependent one, we return a dependent entity
                            if ((entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
                                        || entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
                                    && (entry->type_information->type->template_nature == TPN_INCOMPLETE_DEPENDENT
                                        || entry->type_information->type->template_nature == TPN_COMPLETE_DEPENDENT))
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Returning a dependent entity due to the lookup of '%s'\n",
                                            entry->symbol_name);
                                }
                                *is_dependent = 1;
                                return NULL;
                            }
                        }
                    }

                    if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                    {
                        *is_dependent = 1;
                        break;
                    }

                    // Classes do not have namespaces within them
                    if (seen_class && (entry->kind == SK_NAMESPACE))
                    {
                        return NULL;
                    }

                    // Once seen a class no more namespaces can appear
                    if (entry->kind == SK_CLASS)
                    {
                        seen_class = 1;
                    }

                    // It looks fine, update the scope
                    scope_t* previous_scope = lookup_scope;
                    lookup_scope = copy_scope(entry->related_scope);

                    // This can be null if the type is incomplete and we are
                    // declaring a pointer to member
                    if (lookup_scope != NULL)
                    {
                        lookup_scope->template_scope = previous_scope->template_scope;
                    }

                    break;
                }
            case AST_TEMPLATE_ID :
                {
                    solve_possibly_ambiguous_template_id(nested_name_spec, sc, decl_context);

                    // Nothing else is necessary if we've seen that this was dependent
                    if (*is_dependent)
                    {
                        break;
                    }

                    lookup_flags_t instantiation_flag = LF_INSTANTIATE;

                    if (qualif_level == 0)
                    {
                        entry_list = query_unqualified_template_id_flags(nested_name_spec, sc, lookup_scope, 
                                instantiation_flag | lookup_flags, decl_context);
                    }
                    else
                    {
                        entry_list = query_template_id_flags(nested_name_spec, sc, lookup_scope, 
                                instantiation_flag | lookup_flags, decl_context);
                    }

                    if (entry_list == NULL)
                    {
                        return NULL;
                    }

                    if (entry_list->entry->kind == SK_DEPENDENT_ENTITY
                            || entry_list->entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                    {
                        *is_dependent = 1;
                        break;
                    }

                    scope_entry_t* entry = entry_list->entry;

                    scope_t* previous_scope = lookup_scope;
                    lookup_scope = copy_scope(entry->related_scope);

                    // This can be null if the type is incomplete and we are
                    // declaring a pointer to member
                    if (lookup_scope != NULL)
                    {
                        lookup_scope->template_scope = previous_scope->template_scope;
                    }

                    seen_class = 1;
                    break;
                }
            default:
                {
                    internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(nested_name_spec)));
                    break;
                }
        }

        qualif_level++;
        nested_name = ASTSon1(nested_name);
    }

    // If this was seen as dependent, do not return anything
    if (*is_dependent)
    {
        return NULL;
    }

    if (result_entry_list != NULL)
    {
        *result_entry_list = entry_list;
    }

    return lookup_scope;
}

scope_entry_list_t* query_nested_name(scope_t* sc, AST global_op, AST nested_name, AST name, 
        unqualified_lookup_behaviour_t unqualified_lookup,
        decl_context_t decl_context)
{
    return query_nested_name_flags(sc, global_op, nested_name, name, unqualified_lookup, 
            LF_NONE, decl_context);
}

// Similar to query_nested_name_spec but searches the name
scope_entry_list_t* query_nested_name_flags(scope_t* sc, AST global_op, AST nested_name, AST name, 
        unqualified_lookup_behaviour_t unqualified_lookup, lookup_flags_t lookup_flags,
        decl_context_t decl_context)
{
    scope_entry_list_t* result = NULL;
    scope_t* lookup_scope;

    if (global_op == NULL && nested_name == NULL)
    {
        char* symbol_name = ASTText(name);

        lookup_scope = sc;

        if (BITMAP_TEST(lookup_flags, LF_CONSTRUCTOR))
        {
            symbol_name = strprepend(symbol_name, "constructor ");
        }

        if (BITMAP_TEST(lookup_flags, LF_IN_NAMESPACE_SCOPE))
        {
            lookup_scope = copy_scope(enclosing_namespace_scope(sc));
            ERROR_CONDITION((lookup_scope == NULL), "Enclosing namespace not found\n", 0);

            lookup_scope->template_scope = sc->template_scope;
        }

        // This is an unqualified identifier
        switch (ASTType(name))
        {
            case AST_SYMBOL :
                switch (unqualified_lookup)
                {
                    case FULL_UNQUALIFIED_LOOKUP :
                        {
                            result = query_unqualified_name(lookup_scope, symbol_name);
                            break;
                        }
                    case NOFULL_UNQUALIFIED_LOOKUP :
                        {
                            result = query_in_symbols_of_scope(lookup_scope, symbol_name);
                            break;
                        }
                    default :
                        {
                            internal_error("Invalid lookup behaviour", 0);
                        }
                }
                break;
            case AST_TEMPLATE_ID:
                {
                    solve_possibly_ambiguous_template_id(name, sc, decl_context);

                    result = query_unqualified_template_id_flags(name, sc, 
                            lookup_scope, lookup_flags, decl_context);
                }
                break;
            default :
                internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(name)));
        }
    }
    else
    {
        char is_dependent = 0;
        if (((lookup_scope = query_nested_name_spec_flags(sc, global_op, nested_name, 
                        NULL, &is_dependent, lookup_flags, decl_context)) != NULL)
                // We cannot lookup things inside things that are dependent
                // since they are not completely instantiated
                && !is_dependent)
        {
            switch (ASTType(name))
            {
                case AST_SYMBOL :
                    {
                        char* symbol_name = ASTText(name);

                        if (BITMAP_TEST(lookup_flags, LF_CONSTRUCTOR))
                        {
                            symbol_name = strprepend(symbol_name, "constructor ");
                        }

                        result = query_unqualified_name_flags(lookup_scope, symbol_name, lookup_flags | LF_FROM_QUALIFIED);
                    }
                    break;
                case AST_TEMPLATE_ID:
                    {
                        solve_possibly_ambiguous_template_id(name, sc, decl_context);
                        result = query_template_id_flags(name, sc, lookup_scope, lookup_flags,
                                decl_context);
                    }
                    break;
                case AST_CONVERSION_FUNCTION_ID :
                    {
                        char* conversion_function_name = get_conversion_function_name(name, lookup_scope, NULL,
                                decl_context);
                        result = query_unqualified_name_flags(lookup_scope, conversion_function_name, lookup_flags | LF_FROM_QUALIFIED);
                        break;
                    }
                case AST_DESTRUCTOR_TEMPLATE_ID :
                case AST_DESTRUCTOR_ID :
                    {
                        char* symbol_name = ASTText(ASTSon0(name));
                        result = query_unqualified_name_flags(lookup_scope, symbol_name, lookup_flags | LF_FROM_QUALIFIED);
                        break;
                    }
                case AST_OPERATOR_FUNCTION_ID  :
                    {
                        // An unqualified operator_function_id "operator +"
                        char* operator_function_name = get_operator_function_name(name);

                        result = query_unqualified_name_flags(lookup_scope, operator_function_name, 
                                lookup_flags | LF_FROM_QUALIFIED);
                        break;
                    }
                default :
                    internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(name)));
            }
        }
        else if (is_dependent)
        {
            scope_entry_t* dependent_entity = calloc(1, sizeof(*dependent_entity));
            dependent_entity->kind = SK_DEPENDENT_ENTITY;

            return create_list_from_entry(dependent_entity);
        }
    }

    return result;
}


static scope_entry_list_t* query_template_id_internal(AST template_id, scope_t* sc, scope_t* lookup_scope, 
        unqualified_lookup_behaviour_t unqualified_lookup, lookup_flags_t lookup_flags,
        decl_context_t decl_context)
{
    AST symbol = ASTSon0(template_id);
    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to resolve template '%s'\n", ASTText(symbol));
    }

    scope_entry_list_t* entry_list; 
    
    if (unqualified_lookup == FULL_UNQUALIFIED_LOOKUP)
    {
        entry_list = query_unqualified_name(lookup_scope, ASTText(symbol));
    }
    else
    {
        entry_list = query_in_symbols_of_scope(lookup_scope, ASTText(symbol));
    }

    enum cxx_symbol_kind filter_templates[5] = {
        SK_TEMPLATE_PRIMARY_CLASS, 
        SK_TEMPLATE_SPECIALIZED_CLASS,
        SK_TEMPLATE_FUNCTION,
        SK_TEMPLATE_TEMPLATE_PARAMETER,
        SK_TEMPLATE_ALIAS
    };

    entry_list = filter_symbol_kind_set(entry_list, 5, filter_templates);

    if (entry_list == NULL)
    {
        if (BITMAP_TEST(lookup_flags, LF_NO_FAIL))
        {
            return NULL;
        }
        else
        {
            internal_error("Template '%s' not found! (line=%d)\n", 
                prettyprint_in_buffer(template_id),
                ASTLine(template_id));
        }
    }

    scope_entry_list_t* template_functions = filter_symbol_kind(entry_list, SK_TEMPLATE_FUNCTION);
    if (template_functions != NULL)
    {
        // This is naming a template function
        // Just return them, do not instantiate
        solve_possibly_ambiguous_template_id(template_id, sc, decl_context);

        return template_functions;
    }

    scope_entry_list_t* template_template_param = filter_symbol_kind(entry_list, SK_TEMPLATE_TEMPLATE_PARAMETER);
    if (template_template_param != NULL)
    {
        return template_template_param;
    }

    // Solve template_alias properly
    scope_entry_list_t* template_alias_list = filter_symbol_kind(entry_list, SK_TEMPLATE_ALIAS);
    if (template_alias_list != NULL)
    {
        scope_entry_t* template_alias = template_alias_list->entry;

        type_t* alias_type_info = template_alias->template_alias_type;

        DEBUG_CODE()
        {
            fprintf(stderr, "Symbol '%s' is a template alias\n", ASTText(symbol));
        }

        if (alias_type_info->kind != TK_DIRECT
                || alias_type_info->type->kind != STK_USER_DEFINED)
        {
            internal_error("Expected a template name type\n", 0);
        }
        
        entry_list = create_list_from_entry(alias_type_info->type->user_defined_type);
        symbol = ASTLeaf(AST_SYMBOL, 
                alias_type_info->type->user_defined_type->line,
                alias_type_info->type->user_defined_type->symbol_name);
    }

    // Now readjust the scope to really find the templates and not just the
    // injected class name
    scope_t* previous_template_scope = lookup_scope->template_scope;
    lookup_scope = entry_list->entry->scope;
    lookup_scope->template_scope = previous_template_scope;

    if (unqualified_lookup == FULL_UNQUALIFIED_LOOKUP)
    {
        entry_list = query_unqualified_name(lookup_scope, ASTText(symbol));
    }
    else
    {
        entry_list = query_in_symbols_of_scope(lookup_scope, ASTText(symbol));
    }

    enum cxx_symbol_kind filter_template_classes[3] = {
        SK_TEMPLATE_PRIMARY_CLASS, 
        SK_TEMPLATE_SPECIALIZED_CLASS,
        SK_TEMPLATE_TEMPLATE_PARAMETER
    };

    entry_list = filter_symbol_kind_set(entry_list, 3, filter_template_classes);

    if (entry_list == NULL)
    {
        if (BITMAP_TEST(lookup_flags, LF_NO_FAIL))
        {
            return NULL;
        }
        else
        {
            internal_error("Template not found! (line=%d)\n", ASTLine(template_id));
        }
    }

    template_argument_list_t* current_template_arguments = NULL;
    // Note the scope being different here
    build_scope_template_arguments(template_id, lookup_scope, sc, sc, 
            &current_template_arguments, decl_context);

    // First try to match exactly an existing template
    // because this is a parameterized template-id
    char will_not_instantiate = 1;

    if (BITMAP_TEST(lookup_flags, LF_INSTANTIATE))
    {
        will_not_instantiate = 0;
    }

    // LF_NO_INSTANTIATE has higher precedence over LF_INSTANTIATE
    if (BITMAP_TEST(lookup_flags, LF_NO_INSTANTIATE))
    {
        will_not_instantiate = 1;
    }

    int i;
    char seen_dependent_args = 0;
    for (i = 0; (i < current_template_arguments->num_arguments)
            && !seen_dependent_args; i++)
    {
        template_argument_t* argument = current_template_arguments->argument_list[i];
        if (argument->kind == TAK_TYPE)
        {
            if (is_dependent_type(argument->type, decl_context))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "-> Dependent type template argument '%s'\n",
                            prettyprint_in_buffer(argument->argument_tree));
                }
                seen_dependent_args = 1;
            }
        }
        else if (argument->kind == TAK_TEMPLATE)
        {
            // Fix this
        }
        else if (argument->kind == TAK_NONTYPE)
        {
            if (is_dependent_expression(argument->argument_tree, argument->scope, decl_context))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "-> Dependent expression template argument '%s'\n",
                            prettyprint_in_buffer(argument->argument_tree));
                }
                seen_dependent_args = 1;
            }
            // literal_value_t value = evaluate_constant_expression(argument->argument_tree, argument->scope);

            // if (value.kind == LVK_DEPENDENT_EXPR)
            // {
            //     seen_dependent_args = 1;
            // }

            // if (value.kind == LVK_INVALID)
            // {
            //     DEBUG_CODE()
            //     {
            //         fprintf(stderr, "-> Template not returned since one of its arguments is an invalid expression\n");
            //     }
            //     return NULL;
            // }
        }
    }

    // If this is considered in the context of an expression and dependent args
    // have been seen, create a dependent entity

    if (BITMAP_TEST(lookup_flags, LF_EXPRESSION) 
            && seen_dependent_args)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "This is a dependent template-id used in an expression\n");
        }

        scope_entry_t* dependent_entity = calloc(1, sizeof(*dependent_entity));
        dependent_entity->kind = SK_DEPENDENT_ENTITY;

        return create_list_from_entry(dependent_entity);
    }

    // If we have seen dependent arguments, we will not instantiate
    // unless we are asked to
    if (seen_dependent_args
            && !BITMAP_TEST(decl_context.decl_flags, DF_ALWAYS_CREATE_SPECIALIZATION))
    {
        will_not_instantiate |= seen_dependent_args;
        DEBUG_CODE()
        {
            fprintf(stderr, "The template-id '%s' has dependent arguments and will not be instantiated here (df_always_create=%d || lf_always_create=%d)\n",
                    prettyprint_in_buffer(template_id),
                    BITMAP_TEST(decl_context.decl_flags, DF_ALWAYS_CREATE_SPECIALIZATION), // always zero
                    BITMAP_TEST(lookup_flags, LF_ALWAYS_CREATE_SPECIALIZATION));
        }
    }

    if (!BITMAP_TEST(decl_context.decl_flags, DF_ALWAYS_CREATE_SPECIALIZATION))
    {
        seen_dependent_args = 0;
    }
    
    DEBUG_CODE()
    {
        fprintf(stderr, "-> Looking for exact match templates\n");
    }

    matching_pair_t* matched_template = solve_template(entry_list,
            current_template_arguments, sc, /* exact = */ 1, decl_context);

    if (matched_template != NULL)
    {
        scope_entry_t* matched_entry = matched_template->entry;
        if (!will_not_instantiate 
                    && (matched_entry->type_information->type->template_nature == TPN_INCOMPLETE_INDEPENDENT))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "-> Instantiating something that was declared before but not instantiated\n");
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "Filtering matched entry %p from entry list\n", matched_entry);
            }
            scope_entry_list_t* fixed_entry_list = filter_entry_from_list(entry_list, matched_entry);
            matched_template = solve_template(fixed_entry_list,
                    current_template_arguments, sc, 0, decl_context);

            instantiate_template_in_symbol(matched_entry, matched_template, current_template_arguments, 
                    sc, decl_context);

            // And now restart this function but now we want an exact match
            return query_template_id_internal(template_id, sc, lookup_scope, unqualified_lookup, 
                    lookup_flags & (~LF_INSTANTIATE), decl_context);
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "-> Just returning the exact matching template %p\n", matched_entry);
            }
            return create_list_from_entry(matched_entry);
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "-> Not selected an exact template\n");
        }
    }

    // If we are here there is no exact match thus we may have to instantiate
    // the template
    if (!will_not_instantiate)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "-> Solving the template without exact match\n");
        }
        
        // entry_list = filter_symbol_using_predicate(entry_list, is_not_incomplete);

        matched_template = solve_template(entry_list, current_template_arguments, sc, /* exact= */ 0,
                decl_context);

        if (matched_template == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "-> Nothing has matched, no template was selected\n");
            }
            return NULL;
        }

        DEBUG_CODE()
        {
            int i;
            fprintf(stderr, "=== Unification details for selected template %p\n", matched_template->entry);
            for (i = 0; i < matched_template->unif_set->num_elems; i++)
            {
                unification_item_t* unif_item = matched_template->unif_set->unif_list[i];
                fprintf(stderr, "Parameter num: %d || Parameter nesting: %d || Parameter name: %s <- ",
                        unif_item->parameter_num, unif_item->parameter_nesting, unif_item->parameter_name);
                if (unif_item->value != NULL)
                {
                    fprintf(stderr, "[type] %s", 
                            print_declarator(unif_item->value, sc));
                }
                else if (unif_item->expression != NULL)
                {
                    fprintf(stderr, "[expr] %s", 
                            prettyprint_in_buffer(unif_item->expression));
                }
                else
                {
                    fprintf(stderr, "(unknown)");
                }
                fprintf(stderr, "\n");
            }
            fprintf(stderr, "=== End of unification details for selected template\n");
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "-> Instantiating the template\n");
        }
        // We have to instantiate the template
        instantiate_template(matched_template, current_template_arguments, sc, 
                ASTLine(template_id), decl_context);

        // And now restart this function but now we want an exact match
        return query_template_id_internal(template_id, sc, lookup_scope, unqualified_lookup, 
                lookup_flags & (~LF_INSTANTIATE), decl_context);
    }
    else
    {
        // We won't instantiate the template
        matched_template = solve_template(entry_list, current_template_arguments, sc, /* exact= */ 0,
                decl_context);

        if (!seen_dependent_args
                 || BITMAP_TEST(lookup_flags, LF_ALWAYS_CREATE_SPECIALIZATION)
                 || BITMAP_TEST(decl_context.decl_flags, DF_ALWAYS_CREATE_SPECIALIZATION))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "-> Since we are not instantiating, creating the holding symbol\n");
            }
            scope_entry_t* holding_symbol = create_holding_symbol_for_template(entry_list->entry,
                    current_template_arguments,
                    sc, ASTLine(template_id), decl_context);
            return create_list_from_entry(holding_symbol);

        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "-> An existing specialization found %p (line %d), returning this one\n",
                        matched_template, matched_template->entry->line);
            }
            return create_list_from_entry(matched_template->entry);
        }
    }
}

scope_entry_list_t* query_unqualified_template_id(AST template_id, scope_t* sc, 
        scope_t* lookup_scope, decl_context_t decl_context)
{
    return query_template_id_internal(template_id, sc, lookup_scope, FULL_UNQUALIFIED_LOOKUP, LF_NONE,
            decl_context);
}

scope_entry_list_t* query_unqualified_template_id_flags(AST template_id, scope_t* sc, scope_t* lookup_scope,
        lookup_flags_t lookup_flags, decl_context_t decl_context)
{
    return query_template_id_internal(template_id, sc, lookup_scope, FULL_UNQUALIFIED_LOOKUP, lookup_flags,
            decl_context);
}

scope_entry_list_t* query_template_id(AST template_id, scope_t* sc, scope_t* lookup_scope,
        decl_context_t decl_context)
{
    return query_template_id_internal(template_id, sc, lookup_scope, 
            NOFULL_UNQUALIFIED_LOOKUP, LF_NONE, decl_context);
}

scope_entry_list_t* query_template_id_flags(AST template_id, scope_t* sc, 
        scope_t* lookup_scope, lookup_flags_t lookup_flags, 
        decl_context_t decl_context)
{
    return query_template_id_internal(template_id, sc, lookup_scope, 
            NOFULL_UNQUALIFIED_LOOKUP, lookup_flags, decl_context);
}

scope_entry_list_t* query_id_expression(scope_t* sc, AST id_expr, 
        unqualified_lookup_behaviour_t unqualified_lookup, decl_context_t decl_context)
{
    return query_id_expression_flags(sc, id_expr, unqualified_lookup, LF_NONE, decl_context);
}

scope_entry_list_t* query_id_expression_flags(scope_t* sc, AST id_expr, 
        unqualified_lookup_behaviour_t unqualified_lookup, lookup_flags_t lookup_flags,
        decl_context_t decl_context)
{
    switch (ASTType(id_expr))
    {
        // Unqualified ones
        case AST_SYMBOL :
            {
                char* id_expr_name = ASTText(id_expr);

                if (BITMAP_TEST(lookup_flags, LF_CONSTRUCTOR))
                {
                    id_expr_name = strprepend(id_expr_name, "constructor ");
                }

                scope_entry_list_t* result = NULL;
                switch (unqualified_lookup)
                {
                    case FULL_UNQUALIFIED_LOOKUP :
                        {
                            result = query_unqualified_name(sc, id_expr_name);
                            break;
                        }
                    case NOFULL_UNQUALIFIED_LOOKUP :
                        {
                            result = query_in_symbols_of_scope(sc, id_expr_name);
                            break;
                        }
                    default :
                        {
                            internal_error("Invalid lookup behaviour", 0);
                        }
                }
                return result;
                break;
            }
        case AST_DESTRUCTOR_TEMPLATE_ID :
        case AST_DESTRUCTOR_ID :
            {
                // An unqualified destructor name "~name"
                // 'name' should be a class in this scope
                AST symbol = ASTSon0(id_expr);
                scope_entry_list_t* result = query_in_symbols_of_scope(sc, ASTText(symbol));

                return result;

                break;
            }
        case AST_TEMPLATE_ID :
            {
                // An unqualified template_id "identifier<stuff>"
                AST symbol = ASTSon0(id_expr);

                solve_possibly_ambiguous_template_id(id_expr, sc, decl_context);

                scope_entry_list_t* result = query_unqualified_name(sc, ASTText(symbol));

                return result;
                break;
            }
        case AST_OPERATOR_FUNCTION_ID :
            {
                // An unqualified operator_function_id "operator +"
                char* operator_function_name = get_operator_function_name(id_expr);

                scope_entry_list_t* result = query_in_symbols_of_scope(sc, operator_function_name);

                return result;
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            {
                char* conversion_function_name = get_conversion_function_name(id_expr, sc, NULL,
                        decl_context);
                scope_entry_list_t* result = query_unqualified_name(sc, conversion_function_name);
                return result;
                break;
            }
            // Qualified ones
        case AST_QUALIFIED_ID :
            {
                // A qualified id "a::b::c"
                AST global_op = ASTSon0(id_expr);
                AST nested_name = ASTSon1(id_expr);
                AST symbol = ASTSon2(id_expr);

                scope_entry_list_t* result = query_nested_name_flags(sc, global_op, nested_name, 
                        symbol, FULL_UNQUALIFIED_LOOKUP, lookup_flags, decl_context);

                return result;
                break;
            }
        case AST_QUALIFIED_TEMPLATE :
            {
                // A qualified template "a::b::template c<a>"
                internal_error("Unsupported qualified template", 0);
                break;
            }
        case AST_QUALIFIED_OPERATOR_FUNCTION_ID :
            {
                // A qualified operator function_id "a::b::operator +"
                internal_error("Unsupported qualified operator id", 0);
                break;
            }
        default :
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(id_expr)));
                break;
            }
    }

    return NULL;
}

scope_entry_list_t* create_list_from_entry(scope_entry_t* entry)
{
    scope_entry_list_t* result = calloc(1, sizeof(*result));
    result->entry = entry;
    result->next = NULL;

    return result;
}

static scope_entry_list_t* query_in_template_nesting(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
    scope_entry_list_t* result = NULL;

    while (st != NULL && result == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "template lookup: ");
        }
        result = query_in_symbols_of_scope(st, unqualified_name);
        st = st->template_scope;
    }

    return result;
}

static scope_entry_list_t* lookup_block_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
    // First check the scope
    scope_entry_list_t* result = NULL;
    DEBUG_CODE()
    {
        fprintf(stderr, "Looking up '%s' in block...", unqualified_name);
    }
    result = query_in_symbols_of_scope(st, unqualified_name);

    if (result != NULL)
    {
        return result;
    }

    // Search in the namespaces
    DEBUG_CODE()
    {
        fprintf(stderr, "not found.\nLooking up '%s' in used namespaces...", unqualified_name);
    }
    int i;
    for (i = 0; i < st->num_used_namespaces; i++)
    {
        result = query_in_symbols_of_scope(st->use_namespace[i], unqualified_name);
        if (result != NULL)
        {
            return result;
        }
    }

    // Search in the scope of labels
    if (st->function_scope != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "not found.\nLooking up '%s' in labels...", unqualified_name);
        }
        result = query_in_symbols_of_scope(st->function_scope, unqualified_name);
        if (result != NULL)
        {
            return result;
        }
    }
    
    // Search in the scope of parameters
    if (st->prototype_scope != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "not found.\nLooking up '%s' in parameters...", unqualified_name);
        }
        result = query_in_symbols_of_scope(st->prototype_scope, unqualified_name);
        if (result != NULL)
        {
            return result;
        }
    }

    if (!BITMAP_TEST(lookup_flags, LF_FROM_QUALIFIED))
    {
        // If the name is unqualified
        // Otherwise, if template scoping is available, check in the template scope
        if (st->template_scope != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "not found.\nLooking up '%s' in template parameters...", unqualified_name);
            }
            result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
            if (result != NULL)
            {
                return result;
            }
        }

        // Otherwise try to find anything in the enclosing scope
        if (st->contained_in != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
            }
            result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
        }
    }

    return result;
}

static scope_entry_list_t* lookup_prototype_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
    scope_entry_list_t* result = NULL;

    result = query_in_symbols_of_scope(st, unqualified_name);
    if (result != NULL)
    {
        return result;
    }

    // Otherwise, if template scoping is available, check in the template scope
    if (st->template_scope != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Looking up '%s' in template parameters...", unqualified_name);
        }
        result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
        if (result != NULL)
        {
            return result;
        }
    }

    // Otherwise try to find anything in the enclosing scope
    if (st->contained_in != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
        }
        result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
    }

    return result;
}

static scope_entry_list_t* lookup_namespace_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
    // First check the scope
    scope_entry_list_t* result = NULL;

    DEBUG_CODE()
    {
        fprintf(stderr, "Looking up '%s' within namespace...", unqualified_name);
    }

    result = query_in_symbols_of_scope(st, unqualified_name);

    if (result != NULL)
    {
        return result;
    }
    
    // Otherwise, if template scoping is available, check in the template scope
    if (st->template_scope != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Looking up '%s' in template parameters...", unqualified_name);
        }
        result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
        if (result != NULL)
        {
            return result;
        }
    }

    // Search in the namespaces
    DEBUG_CODE()
    {
        fprintf(stderr, "not found.\nLooking up '%s' in used namespaces (%d)...", unqualified_name, st->num_used_namespaces);
    }
    int i;
    for (i = 0; i < st->num_used_namespaces; i++)
    {
        result = query_in_symbols_of_scope(st->use_namespace[i], unqualified_name);
        if (result != NULL)
        {
            return result;
        }
    }

    if (!BITMAP_TEST(lookup_flags, LF_FROM_QUALIFIED))
    {
        // Otherwise try to find anything in the enclosing scope
        // but only if it is unqualified
        if (st->contained_in != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
            }
            result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
        }
    }

    return result;
}

static scope_entry_list_t* lookup_function_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
    // First check the scope
    scope_entry_list_t* result = NULL;
    DEBUG_CODE()
    {
        fprintf(stderr, "Looking up '%s' in function scope...", unqualified_name);
    }
    result = query_in_symbols_of_scope(st, unqualified_name);
    
    if (result != NULL)
    {
        return result;
    }
    
    // Search in the namespaces
    DEBUG_CODE()
    {
        fprintf(stderr, "not found.\nLooking up '%s' in used namespaces...", unqualified_name);
    }
    int i;
    for (i = 0; i < st->num_used_namespaces; i++)
    {
        result = query_in_symbols_of_scope(st->use_namespace[i], unqualified_name);
        if (result != NULL)
        {
            return result;
        }
    }

    // Otherwise try to find anything in the enclosing scope
    if (st->contained_in != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
        }
        result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
    }

    return result;
}

static scope_entry_list_t* lookup_class_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
    // First check the scope
    scope_entry_list_t* result = NULL;
    DEBUG_CODE()
    {
        fprintf(stderr, "Looking up '%s' in class...", unqualified_name);
    }
    result = query_in_symbols_of_scope(st, unqualified_name);

    if (result != NULL)
    {
        return result;
    }

    // Search in the namespaces
    DEBUG_CODE()
    {
        fprintf(stderr, "not found.\nLooking up '%s' in used namespaces...", unqualified_name);
    }
    int i;
    for (i = 0; i < st->num_used_namespaces; i++)
    {
        result = query_in_symbols_of_scope(st->use_namespace[i], unqualified_name);
        if (result != NULL)
        {
            return result;
        }
    }
    
    // Search in the bases
    DEBUG_CODE()
    {
        fprintf(stderr, "not found.\nLooking up '%s' in bases (%d)...", unqualified_name, st->num_base_scopes);
    }
    for (i = 0; i < st->num_base_scopes; i++)
    {
        result = query_unqualified_name_flags(st->base_scope[i], unqualified_name, lookup_flags);
        if (result != NULL)
        {
            return result;
        }
    }

    if (!BITMAP_TEST(lookup_flags, LF_FROM_QUALIFIED))
    {
        // If it is unqualified
        // Otherwise, if template scoping is available, check in the template scope
        if (st->template_scope != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "not found.\nLooking up '%s' in template parameters...", unqualified_name);
            }
            result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
            if (result != NULL)
            {
                return result;
            }
        }

        // Otherwise try to find anything in the enclosing scope
        if (st->contained_in != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
            }
            result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
        }
    }

    return result;
}

static scope_entry_list_t* lookup_template_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
    // First check the scope
    scope_entry_list_t* result = NULL;
    DEBUG_CODE()
    {
        fprintf(stderr, "Looking up '%s' in template scope...", unqualified_name);
    }
    result = query_in_symbols_of_scope(st, unqualified_name);

    if (result != NULL)
    {
        return result;
    }

    // Otherwise, if template scoping is available, check in the template scope
    if (st->template_scope != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "not found.\nLooking up '%s' in template parameters...", unqualified_name);
        }
        result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
        if (result != NULL)
        {
            return result;
        }
    }

    // Otherwise try to find anything in the enclosing scope
    if (st->contained_in != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
        }
        result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
    }

    return result;
}

scope_entry_list_t* query_unqualified_name_flags(scope_t* st, char* unqualified_name, 
        lookup_flags_t lookup_flags)
{
    static int nesting_level = 0;

    nesting_level++;

    scope_entry_list_t* result = NULL;

    switch (st->kind)
    {
        case PROTOTYPE_SCOPE :
            DEBUG_CODE()
            {
                fprintf(stderr, "Starting lookup in prototype scope %p\n", st);
            }
            result = lookup_prototype_scope(st, unqualified_name, lookup_flags);
            break;
        case NAMESPACE_SCOPE :
            DEBUG_CODE()
            {
                fprintf(stderr, "Starting lookup in namespace scope %p\n", st);
            }
            result = lookup_namespace_scope(st, unqualified_name, lookup_flags);
            break;
        case FUNCTION_SCOPE :
            DEBUG_CODE()
            {
                fprintf(stderr, "Starting lookup in function scope %p\n", st);
            }
            result = lookup_function_scope(st, unqualified_name, lookup_flags);
            break;
        case BLOCK_SCOPE :
            DEBUG_CODE()
            {
                fprintf(stderr, "Starting lookup in block scope %p\n", st);
            }
            result = lookup_block_scope(st, unqualified_name, lookup_flags);
            break;
        case CLASS_SCOPE :
            DEBUG_CODE()
            {
                fprintf(stderr, "Starting lookup in class scope %p\n", st);
            }
            result = lookup_class_scope(st, unqualified_name, lookup_flags);
            break;
        case TEMPLATE_SCOPE :
            DEBUG_CODE()
            {
                fprintf(stderr, "Starting lookup in template scope %p\n", st);
            }
            result = lookup_template_scope(st, unqualified_name, lookup_flags);
            break;
        case UNDEFINED_SCOPE :
        default :
            internal_error("Invalid scope kind=%d!\n", st->kind);
    }

    nesting_level--;

    DEBUG_CODE()
    {
        if (nesting_level == 0)
        {
            if (result != NULL)
            {
                fprintf(stderr, "found\n");
            }
            else
            {
                fprintf(stderr, "not found.\n");
            }
        }
    }

    return result;
}

scope_entry_list_t* query_unqualified_name(scope_t* st, char* unqualified_name)
{
    return query_unqualified_name_flags(st, unqualified_name, LF_NONE);
}

// char incompatible_symbol_exists(scope_t* sc, AST id_expr, enum cxx_symbol_kind symbol_kind)
// {
//  scope_entry_list_t* entry_list = query_id_expression(sc, id_expr);
//  char found_incompatible = 0;
// 
//  while (!found_incompatible && entry_list != NULL)
//  {
//      found_incompatible = (entry_list->entry->kind != symbol_kind);
// 
//      entry_list = entry_list->next;
//  }
// 
//  return found_incompatible;
// }


scope_entry_list_t* filter_symbol_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set)
{
    scope_entry_list_t* result = NULL;
    scope_entry_list_t* iter = entry_list;
    
    while (iter != NULL)
    {
        int i;
        char found = 0;
        for (i = 0; (i < num_kinds) && !found; i++)
        {
            if (iter->entry->kind == symbol_kind_set[i])
            {
                scope_entry_list_t* new_item = calloc(1, sizeof(*new_item));
                new_item->entry = iter->entry;
                new_item->next = result;
                result = new_item;
                found = 1;
            }
        }

        iter = iter->next;
    }

    return result;
}

scope_entry_list_t* filter_symbol_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind)
{
    scope_entry_list_t* result = NULL;

    result = filter_symbol_kind_set(entry_list, 1, &symbol_kind);

    return result;
}


scope_entry_list_t* filter_symbol_non_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set)
{
    scope_entry_list_t* result = NULL;
    scope_entry_list_t* iter = entry_list;
    
    while (iter != NULL)
    {
        int i;
        char found = 0;
        for (i = 0; (i < num_kinds) && !found; i++)
        {
            if (iter->entry->kind == symbol_kind_set[i])
            {
                found = 1;
            }
        }

        if (!found)
        {
            scope_entry_list_t* new_item = calloc(1, sizeof(*new_item));
            new_item->entry = iter->entry;
            new_item->next = result;
            result = new_item;
        }

        iter = iter->next;
    }

    return result;
}

scope_entry_list_t* filter_symbol_non_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind)
{
    scope_entry_list_t* result = NULL;

    result = filter_symbol_non_kind_set(entry_list, 1, &symbol_kind);

    return result;
}

scope_entry_list_t* filter_entry_from_list(scope_entry_list_t* entry_list, scope_entry_t* entry)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Filtering %p (line %d) from candidate list %p\n", 
                entry, entry->line, entry_list);
    }
    scope_entry_list_t* result = NULL;
    scope_entry_list_t* iter = entry_list;
    
    while (iter != NULL)
    {
        if (iter->entry != entry)
        {
            scope_entry_list_t* new_item = calloc(1, sizeof(*new_item));
            new_item->entry = iter->entry;
            new_item->next = result;
            result = new_item;
        }

        iter = iter->next;
    }

    return result;
}

scope_entry_list_t* filter_symbol_using_predicate(scope_entry_list_t* entry_list, char (*f)(scope_entry_t*))
{
    scope_entry_list_t* result = NULL;
    scope_entry_list_t* iter = entry_list;
    
    while (iter != NULL)
    {
        if (f(iter->entry))
        {
            scope_entry_list_t* new_item = calloc(1, sizeof(*new_item));
            new_item->entry = iter->entry;
            new_item->next = result;
            result = new_item;
        }

        iter = iter->next;
    }

    return result;
}

/*
 * Returns a type if and only if this entry_list contains just one type
 * specifier. If another identifier is found it returns NULL
 */
scope_entry_t* filter_simple_type_specifier(scope_entry_list_t* entry_list)
{
    int non_type_name = 0;
    scope_entry_t* result = NULL;

    char seen_class_name = 0;
    char seen_function_name = 0;

    while (entry_list != NULL)
    {
        scope_entry_t* simple_type_entry = entry_list->entry;

        if (simple_type_entry->kind != SK_ENUM 
                && simple_type_entry->kind != SK_CLASS 
                && simple_type_entry->kind != SK_TYPEDEF 
                && simple_type_entry->kind != SK_TEMPLATE_TYPE_PARAMETER
                && simple_type_entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
                && simple_type_entry->kind != SK_TEMPLATE_PRIMARY_CLASS
                && simple_type_entry->kind != SK_TEMPLATE_SPECIALIZED_CLASS
                && simple_type_entry->kind != SK_GCC_BUILTIN_TYPE)
        {
            // Functions with name of a class are constructors and do not hide
            // the class symbol
            seen_function_name |= (simple_type_entry->kind == SK_FUNCTION);
            DEBUG_CODE()
            {
                fprintf(stderr, "Found a '%d' that is non type\n", simple_type_entry->kind);
            }
            non_type_name++;
        }
        else
        {
            seen_class_name |= simple_type_entry->kind == SK_CLASS;
            result = simple_type_entry;
        }

        entry_list = entry_list->next;
    }

    // There is something that is not a type name here and hides this simple type spec
    if (non_type_name != 0 
            && !seen_class_name 
            && !seen_function_name)
    {
        return NULL;
    }
    else
    {
        return result;
    }
}

scope_entry_list_t* append_scope_entry_lists(scope_entry_list_t* a, scope_entry_list_t* b)
{
    scope_entry_list_t* result = NULL;
    scope_entry_list_t* iter;

    iter = a;
    while (iter != NULL)
    {
        scope_entry_list_t* new_entry = calloc(1, sizeof(*new_entry));

        new_entry->entry = iter->entry;
        new_entry->next = result;

        result = new_entry;

        iter = iter->next;
    }

    iter = b;
    while (iter != NULL)
    {
        scope_entry_list_t* new_entry = calloc(1, sizeof(*new_entry));

        new_entry->entry = iter->entry;
        new_entry->next = result;

        result = new_entry;

        iter = iter->next;
    }

    return result;
}

scope_t* enclosing_namespace_scope(scope_t* st)
{
    if (st == NULL)
        return NULL;

    if (st->contained_in == NULL)
    {
        return NULL;
    }

    while (st != NULL
            && st->kind != NAMESPACE_SCOPE)
    {
        st = st->contained_in;
    }

    return st;
}

// Copy scope. It preserves the structure of the scopes but not the contents.
// Useful for dynamic scopes like templates that might appear and disappear
// FIXME - ???
// scope_t* copy_scope(scope_t* st)
// {
//     if (st == NULL)
//         return NULL;
// 
//     scope_t* result = calloc(1, sizeof(*result));
// 
//     // bitwise copy
//     *result = *st;
// 
//     return result;
// }

scope_t* copy_scope(scope_t* st)
{
    if (st == NULL)
        return NULL;

    scope_t* result = calloc(1, sizeof(*result));

    // bitwise copy
    *result = *st;

    result->template_scope = copy_scope(st->template_scope);

    // It is not needed since the "contained_in" relationship does not change
    //
    // result->contained_in = copy_scope(st->contained_in);

    return result;
}

// Only for "simple" symbols, this is, that are not members and they are simply contained
// in a nest of namespaces
static char* get_fully_qualified_symbol_name_simple(scope_t* st, char* current_qualif_name)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Getting qualification via scope current='%s'\n", current_qualif_name);
    }
    char* result = current_qualif_name;

    scope_t* current_scope = st;

    while (current_scope != NULL)
    {
        if (current_scope->qualification_name != NULL)
        {
            char* nested_name = strappend(current_scope->qualification_name, "::");
            result = strappend(nested_name, result);
        }

        current_scope = current_scope->contained_in;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Fully qualified name simple '%s'\n", result);
    }

    return result;
}

static scope_entry_t* find_template_parameter(scope_t* st, scope_entry_t* template_param)
{
    Iterator* it = (Iterator*) hash_iterator_create(st->hash);
    for ( iterator_first(it); 
            !iterator_finished(it); 
            iterator_next(it))
    {
        scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);

        scope_entry_t* entry = entry_list->entry;

        if (entry->kind == template_param->kind
                && (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                    || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
        {
            type_t* type_current = entry->type_information;
            type_t* type_param = template_param->type_information;

            simple_type_t* t1 = type_current->type;
            simple_type_t* t2 = type_param->type;

            if ((t1->template_parameter_num == t2->template_parameter_num)
                        && (t1->template_parameter_nesting == t2->template_parameter_nesting))
            {
                return entry;
            }
        }
    }

    return NULL;
}

static char* give_name_for_template_parameter(scope_entry_t* entry, scope_t* st)
{
    if (st->kind != TEMPLATE_SCOPE)
    {
        st = st->template_scope;
    }

    char found = 0;

    while (st != NULL)
    {
        scope_entry_t* template_parameter = find_template_parameter(st, entry);
        if (template_parameter != NULL)
        {
            return template_parameter->symbol_name;
        }

        st = st->template_scope;
    }

    if (!found)
    {
        internal_error("Template parameter '%s' not found in scope\n", entry->symbol_name);
    }

    return NULL;
}

char* get_unqualified_template_symbol_name(scope_entry_t* entry, scope_t* st)
{
    char* result = "";

    // It is not enough with the name, we have to print the arguments
    result = strappend(result, "<");
    template_argument_list_t* template_arguments = entry->type_information->type->template_arguments;

    int i;
    for (i = 0; i < template_arguments->num_arguments; i++)
    {
        template_argument_t* template_argument = template_arguments->argument_list[i];

        // Avoid unnecessary cluttering and the fact that these implicit types
        // come from strange typedefs that would force us to solve completely
        if (template_argument->implicit)
            continue;

        if (i != 0)
        {
            result = strappend(result, ", ");
        }

        switch (template_argument->kind)
        {
            // Print the type
            case TAK_TEMPLATE:
            case TAK_TYPE:
                {
                    char* abstract_declaration;

                    abstract_declaration = 
                        get_declaration_string_internal(template_argument->type, st, "", "", 0, NULL, NULL);

                    result = strappend(result, abstract_declaration);
                    break;
                }
            case TAK_NONTYPE:
                {
                    result = strappend(result, prettyprint_in_buffer(template_argument->argument_tree));
                    break;
                }
            default:
                {
                    fprintf(stderr, "Undefined template argument\n");
                    break;
                }
        }
    }

    result = strappend(result, ">");

    return result;
}

// Get the fully qualified symbol name in the scope of the ocurrence
char* get_fully_qualified_symbol_name(scope_entry_t* entry, scope_t* st, char* is_dependent, int* max_qualif_level)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Getting fully qualified symbol name for '%s'\n", entry->symbol_name);
    }

    // If this is the injected symbol, ignore it and get the real entry
    if (entry->injected_class_name)
    {
        entry = entry->injected_class_referred_symbol;
    }

    char* result = strdup(entry->symbol_name);

    if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        // This symbol must be looked up for the proper real name
        result = give_name_for_template_parameter(entry, st);

        (*is_dependent) |= is_dependent_type(entry->type_information, default_decl_context);
        return result;
    }

    if (entry->kind == SK_TEMPLATE_PRIMARY_CLASS
            || entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
    {
        result = strappend(result, get_unqualified_template_symbol_name(entry, st));
        if (entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
        {
            // They are always dependent
            (*is_dependent) = 1;
        }
        else // if (entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
        {
            simple_type_t* type = entry->type_information->type;

            // They are only dependent if it is so in the template
            if (type->template_nature == TPN_INCOMPLETE_DEPENDENT
                    || type->template_nature == TPN_COMPLETE_DEPENDENT)
            {
                (*is_dependent) = 1;
            }
        }
    }

    if (entry->is_member)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "The symbol is a member, getting the qualified symbol name of the enclosing class\n");
        }
        // We need the qualification of the class
        scope_entry_t* class_symbol = get_class_symbol(entry->class_type);

        if (class_symbol != NULL)
        {
            (*max_qualif_level)++;

            char* class_qualification = get_fully_qualified_symbol_name(class_symbol, st, is_dependent, max_qualif_level);

            DEBUG_CODE()
            {
                fprintf(stderr, "The qualified name of the enclosing class of '%s' is '%s'\n", result, class_qualification);
            }

            class_qualification = strappend(class_qualification, "::");

            result = strappend(class_qualification, result);
        }
    } 
    else if (!entry->is_member)
    {
        // This symbol is already simple enough
        result = get_fully_qualified_symbol_name_simple(entry->scope, result);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Fully qualified name is '%s'\n", result);
    }

    return result;
}
