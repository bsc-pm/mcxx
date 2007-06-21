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
#include <stdio.h>
#include <string.h>
#include "cxx-typeunif.h"
#include "cxx-typeutils.h"
#include "cxx-cexpr.h"
#include "cxx-scope.h"
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-solvetemplate.h"
#include "cxx-prettyprint.h"

static type_t* get_type_template_parameter_unification(unification_set_t* unif_set, int num, int nesting);
static AST get_nontype_template_parameter_unification(unification_set_t* unif_set, int num, int nesting);

// Will try to find a substitution to unificate t1 to t2
//
// e.g.   Q*    can    be unificated to   T**    with   [Q <- T*]
//        T**   cannot be unificated to   Q*
//
char unificate_two_types(type_t* t1, type_t* t2, scope_t* st, 
        unification_set_t** unif_set, decl_context_t decl_context)
{
    cv_qualifier_t cv_qualif_1 = CV_NONE;
    cv_qualifier_t cv_qualif_2 = CV_NONE;
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualif_1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualif_2);

    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to unificate '%s' <- '%s'\n",
                print_declarator(t1, st),
                print_declarator(t2, st));
    }

    // Normalize types
    // If the user defined type points to a template parameter, we will use the
    // template parameter
    if (t1->kind == TK_DIRECT && 
            t1->type->kind == STK_USER_DEFINED)
    {
        // Check first if t1 is a template parameter
        type_t* user_defined_type = NULL;
        user_defined_type = advance_over_typedefs(t1->type->user_defined_type->type_information);
        if (user_defined_type->kind == TK_DIRECT
                && (user_defined_type->type->kind == STK_TYPE_TEMPLATE_PARAMETER
                    || user_defined_type->type->kind == STK_TEMPLATE_TEMPLATE_PARAMETER))
        {
            t1 = user_defined_type;
        }
    }

    if (t2->kind == TK_DIRECT && 
            t2->type->kind == STK_USER_DEFINED)
    {
        // Check first if t1 is a template parameter
        type_t* user_defined_type = NULL;
        user_defined_type = advance_over_typedefs(t2->type->user_defined_type->type_information);
        if (user_defined_type->kind == TK_DIRECT
                && (user_defined_type->type->kind == STK_TYPE_TEMPLATE_PARAMETER
                    || user_defined_type->type->kind == STK_TEMPLATE_TEMPLATE_PARAMETER))
        {
            t2 = user_defined_type;
        }
    }

    // If it is a template parameter (or a user defined type pointing to it)
    // then perform unification
    if (t1->kind == TK_DIRECT 
            && t1->type->kind == STK_TYPE_TEMPLATE_PARAMETER)
    {
        // First check if this parameter has not been already unified
        type_t* previous_unif = get_type_template_parameter_unification(*unif_set, t1->type->template_parameter_num,
                t1->type->template_parameter_nesting);
        if (previous_unif == NULL)
        {
            // Check that t1 is less cv-qualified than t2
            if ((cv_qualif_1 | cv_qualif_2) == (cv_qualif_2))
            {
                unification_item_t* unif_item = calloc(1, sizeof(*unif_item));

                // This number will be the position of the argument
                // within the specialization ! Not of the whole template
                DEBUG_CODE()
                {
                    fprintf(stderr, "Unified parameter = %d (name=%s)\n", t1->type->template_parameter_num,
                            t1->type->template_parameter_name);
                }
                unif_item->parameter_num = t1->type->template_parameter_num;
                unif_item->parameter_nesting = t1->type->template_parameter_nesting;
                unif_item->parameter_name = t1->type->template_parameter_name;

                // Copy the type
                type_t* unified_type = copy_type(t2);
                unif_item->value = unified_type;
                
                // We have to remove the inverse qualification. If t1 is const
                // and t2 is const too, the unified type is not const
                //
                // const T <- const int [T = int] and not [T = const int]
                //
                unified_type->cv_qualifier = cv_qualif_2 & (~cv_qualif_1);

                P_LIST_ADD((*unif_set)->unif_list, (*unif_set)->num_elems, unif_item);
            }
            else
            {
                // We cannot unify 'const X' with 'Y' (although we can unify 'X' with 'const Y')
                DEBUG_CODE()
                {
                    fprintf(stderr, "Unification parameter is more cv-qualified than the argument\n");
                }
                return 0;
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Parameter = %d (name=%s) already unified\n", 
                        t1->type->template_parameter_num, t1->type->template_parameter_name);
            }
            // Check is the same unification we are going to do
            if (!equivalent_types(previous_unif, t2, st, CVE_CONSIDER, decl_context))
            {
                // They're not equivalent, thus not unificable
                DEBUG_CODE()
                {
                    fprintf(stderr, "Previous unification type '%s' does not match the current one '%s'\n",
                            print_declarator(previous_unif, st),
                            print_declarator(t2, st));
                }
                return 0;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Previous unification DOES match the current one\n");
                }
            }
        }

        // They have been unified
        return 1;
    }
    // template template parameters are handled a bit different
    else if (t1->kind == TK_DIRECT
            && t1->type->kind == STK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Unificating template template parameter\n");
        }

        if (t2->kind != TK_DIRECT
                || t2->type->kind != STK_USER_DEFINED
                || (t2->type->user_defined_type->kind != SK_TEMPLATE_PRIMARY_CLASS
                    && t2->type->user_defined_type->kind != SK_TEMPLATE_SPECIALIZED_CLASS
                    && t2->type->user_defined_type->kind != SK_TEMPLATE_TEMPLATE_PARAMETER))
        {
            // This cannot be unified at all, only templates are valid here
            return 0;
        }

        type_t* previous_unif = get_type_template_parameter_unification(*unif_set, t1->type->template_parameter_num,
                t1->type->template_parameter_nesting);
        if (previous_unif != NULL)
        {
            // Check is the same unification we are going to do
            if (!equivalent_types(previous_unif, t2, st, CVE_CONSIDER, decl_context))
            {
                // They're not equivalent, thus not unificable
                DEBUG_CODE()
                {
                    fprintf(stderr, "Previous unification does not match the current one\n");
                }
                return 0;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Previous unification DOES match the current one\n");
                }
            }
        }
        else
        {
            unification_item_t* unif_item = calloc(1, sizeof(*unif_item));

            // This number will be the position of the argument
            // within the specialization ! Not of the whole template
            DEBUG_CODE()
            {
                fprintf(stderr, "Unified template template parameter = %d (name=%s)\n", t1->type->template_parameter_num,
                        t1->type->template_parameter_name);
            }
            unif_item->parameter_num = t1->type->template_parameter_num;
            unif_item->parameter_nesting = t1->type->template_parameter_nesting;
            unif_item->parameter_name = t1->type->template_parameter_name;
            unif_item->value = t2;

            P_LIST_ADD((*unif_set)->unif_list, (*unif_set)->num_elems, unif_item);
        }
        
        // They have been unified
        return 1;
    }

    if (t1->kind == TK_DIRECT 
            && t2->kind == TK_DIRECT
            && t1->type->kind == STK_USER_DEFINED 
            && t2->type->kind == STK_USER_DEFINED)
    {
        scope_entry_t* entry_t1 = t1->type->user_defined_type;
        scope_entry_t* entry_t2 = t2->type->user_defined_type;

        if ((entry_t1->kind == SK_TEMPLATE_PRIMARY_CLASS
                    || entry_t1->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
                && (entry_t2->kind == SK_TEMPLATE_PRIMARY_CLASS
                    || entry_t2->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
                && (strcmp(entry_t1->symbol_name, entry_t2->symbol_name) == 0)
                && same_scope(entry_t1->scope, entry_t2->scope))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Arguments are both the same template '%s', unificating via arguments\n",
                        entry_t1->symbol_name);
            }
            // If they are the same templates they might be unified via its arguments
            //
            //   A<T> can be unified with A<int>   with   [T <- int]
            //

            simple_type_t* simple_type_t1 = entry_t1->type_information->type;
            simple_type_t* simple_type_t2 = entry_t2->type_information->type;

            if (simple_type_t1->template_arguments->num_arguments 
                    != simple_type_t2->template_arguments->num_arguments)
            {
                internal_error("Number of template arguments differs %d != %d", 
                        simple_type_t1->template_arguments->num_arguments,
                        simple_type_t2->template_arguments->num_arguments);
            }

            template_argument_list_t* arguments = simple_type_t2->template_arguments;
            template_argument_list_t* specialized = simple_type_t1->template_arguments;

            char unificable = 0;

            unificable = match_one_template(arguments, specialized, entry_t1, st, *unif_set, decl_context);

            return unificable;
        }
    }

    // t1 is not a template parameter, so to be unificable they have to be of
    // same shape
    if (t1->kind != t2->kind)
        return 0;

    // t1->kind == t2->kind
    switch (t1->kind)
    {
        case TK_DIRECT :
            {
                // If they were unificable they would have been unified before
                return equivalent_types(t1, t2, st, CVE_CONSIDER, decl_context);
                // return equivalent_simple_types(t1->type, t2->type, st);
                break;
            }
        case TK_REFERENCE :
        case TK_POINTER :
            {
                return unificate_two_types(t1->pointer->pointee, t2->pointer->pointee, st, unif_set,
                        decl_context);
                break;
            }
        case TK_POINTER_TO_MEMBER :
            return unificate_two_types(t1->pointer->pointee, t2->pointer->pointee, st, unif_set,
                    decl_context)
                && unificate_two_types(t1->pointer->pointee_class->type_information,
                        t2->pointer->pointee_class->type_information, st, unif_set, decl_context);
            break;
        case TK_ARRAY :
            {
                // If t1 is NULL, check that t2 is NULL too
                if (t1->array->array_expr == NULL)
                {
                    return (t2->array->array_expr == NULL);
                }
                // If t1 is not NULL but t2 is, then this cannot be unified
                if (t2->array->array_expr == NULL)
                {
                    return 0;
                }

                if (!unificate_two_expressions(unif_set, t1->array->array_expr,
                            t1->array->array_expr_scope, t2->array->array_expr,
                            t2->array->array_expr_scope, decl_context))
                {
                    return 0;
                }
                
                if (!unificate_two_types(t1->array->element_type, t2->array->element_type, st, unif_set,
                        decl_context))
                {
                    return 0;
                }
                break;
            }
        case TK_FUNCTION :
            // A function will be unified by steps. First unify the return
            // then the parameters
            {
                if (!unificate_two_types(t1->function->return_type, t2->function->return_type, st, 
                            unif_set, decl_context))
                {
                    return 0;
                }

                if (t1->function->num_parameters != t2->function->num_parameters)
                {
                    return 0;
                }

                int i;
                for (i = 0; i < t1->function->num_parameters; i++)
                {
                    // Fix this should ignore outermost cv qualifier
                    type_t* par1 = t1->function->parameter_list[i]->type_info;
                    type_t* par2 = t2->function->parameter_list[i]->type_info;

                    if (!unificate_two_types(par1, par2, st, unif_set,
                                decl_context))
                    {
                        return 0;
                    }
                }
                break;
            }
        default :
            internal_error("Unknown type kind %d\n", t1->kind);
    }
    // Unifications succeeded
    return 1;
}


char unificate_two_expressions(unification_set_t **unif_set, 
        AST left_tree, scope_t* left_scope, 
        AST right_tree, scope_t* right_scope, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to unify expression '%s' <- '%s'\n",
                prettyprint_in_buffer(left_tree),
                prettyprint_in_buffer(right_tree));
    }
    literal_value_t left_value = 
        evaluate_constant_expression(left_tree, 
                left_scope,
                decl_context);

    literal_value_t right_value = 
        evaluate_constant_expression(right_tree, 
                right_scope,
                decl_context);

    if (left_value.kind != LVK_DEPENDENT_EXPR)
    {
        // The scope is unused in this function
        if (!equal_literal_values(left_value, right_value, left_scope))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "==> They are different\n");
                fprintf(stderr, "=== Unification failed\n");
            }
            return 0;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "==> They are the same!\n");
            }
            return 1;
        }
    }
    else // Left value is dependent
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Left part of the unification '%s' is dependent\n", prettyprint_in_buffer(left_tree));
        }
        // Should be a simple identifier, otherwise it is not unificable
        left_tree = advance_expression_nest(left_tree);
        if (ASTType(left_tree) == AST_SYMBOL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Left part '%s' is a simple expression\n", prettyprint_in_buffer(left_tree));
            }
            scope_entry_list_t *result = query_id_expression(left_scope, left_tree,
                    FULL_UNQUALIFIED_LOOKUP, decl_context);

            ERROR_CONDITION((result == NULL), "Template argument of specialization '%s', not found",
                    prettyprint_in_buffer(left_tree));

            scope_entry_t* entry = result->entry;

            ERROR_CONDITION((entry->kind != SK_TEMPLATE_PARAMETER), "Symbol '%s' is not a nontype template parameter",
                    entry->symbol_name);

            // This can be a non simple type (currently only a pointer or function type)
            int template_parameter_num = base_type(entry->type_information)->type->template_parameter_num;
            int template_parameter_nesting = base_type(entry->type_information)->type->template_parameter_nesting;

            AST previous_unif = get_nontype_template_parameter_unification(
                    *unif_set, 
                    template_parameter_num, 
                    template_parameter_nesting);

            if (previous_unif == NULL)
            {
                unification_item_t* unif_item = calloc(1, sizeof(*unif_item));
                unif_item->expression = right_tree;
                unif_item->parameter_name = ASTText(left_tree);

                unif_item->parameter_num = template_parameter_num;
                unif_item->parameter_nesting = template_parameter_nesting;

                if (right_value.kind != LVK_INVALID
                        && right_value.kind != LVK_DEPENDENT_EXPR)
                {
                    unif_item->expression = tree_from_literal_value(right_value);
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "==> Expression unified\n");
                }

                P_LIST_ADD((*unif_set)->unif_list, (*unif_set)->num_elems, unif_item);
                return 1;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "==> Expression already unified before\n");
                }

                literal_value_t previous_value = 
                    evaluate_constant_expression(previous_unif, 
                            right_scope,
                            decl_context);

                if (equal_literal_values(previous_value, right_value, right_scope))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "==> Previous expression is the same\n");
                    }
                    return 1;
                }
                else
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "==> Previous expression is different.\n");
                        fprintf(stderr, "==> Unification failed\n");
                    }
                    return 0;
                }
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Left part '%s' is not a simple expression, cannot be unified\n", prettyprint_in_buffer(left_tree));
            }
            return 0;
        }
    }
}

static type_t* get_type_template_parameter_unification(unification_set_t* unif_set, int num, int nesting)
{
    int i;
    for (i = 0; i < unif_set->num_elems; i++)
    {
        if (unif_set->unif_list[i]->parameter_num == num
                && unif_set->unif_list[i]->parameter_nesting == nesting)
        {
            return unif_set->unif_list[i]->value;
        }
    }

    return NULL;
}

static AST get_nontype_template_parameter_unification(unification_set_t* unif_set, int num, int nesting)
{
    int i;
    for (i = 0; i < unif_set->num_elems; i++)
    {
        if (unif_set->unif_list[i]->parameter_num == num
                && unif_set->unif_list[i]->parameter_nesting == nesting)
        {
            return unif_set->unif_list[i]->expression;
        }
    }

    return NULL;
}

