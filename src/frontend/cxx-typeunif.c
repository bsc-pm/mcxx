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

static AST get_nontype_template_parameter_unification(unification_set_t* unif_set, int nesting, int position);
static char equivalent_dependent_expressions(AST left_tree, decl_context_t left_decl_context, AST
        right_tree, decl_context_t right_decl_context, unification_set_t** unif_set);
static char equivalent_expression_trees(AST left_tree, decl_context_t left_decl_context, AST right_tree, 
        decl_context_t right_decl_context);

static type_t* get_type_template_parameter_unification(unification_set_t* unif_set,
        int nesting, int position);

// Will try to find a substitution to unificate t1 to t2
//
// e.g.   Q*    can    be unificated to   T**    with   [Q <- T*]
//        T**   cannot be unificated to   Q*
//
char unificate_two_types(type_t* t1, type_t* t2, unification_set_t** unif_set, decl_context_t decl_context)
{
    cv_qualifier_t cv_qualif_1 = CV_NONE;
    cv_qualifier_t cv_qualif_2 = CV_NONE;
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualif_1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualif_2);

    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to unificate type '%s' <- '%s'\n",
                print_declarator(t1, decl_context),
                print_declarator(t2, decl_context));
    }

    // Normalize types
    // If the user defined type points to a template parameter, we will use the
    // template parameter

    // 1. If it is a template parameter (or a user defined type pointing to it)
    // then perform unification
    if (is_named_type(t1) &&
            named_type_get_symbol(t1)->kind == SK_TEMPLATE_TYPE_PARAMETER)
    {
        scope_entry_t* s1 = named_type_get_symbol(t1);

        // First check if this parameter has not been already unified
        type_t* previous_unif = get_type_template_parameter_unification(*unif_set, 
                s1->entity_specs.template_parameter_nesting,
                s1->entity_specs.template_parameter_position);
        if (previous_unif == NULL)
        {
            // Check that t1 is less cv-qualified than t2
            if ((cv_qualif_1 | cv_qualif_2) == (cv_qualif_2))
            {
                unification_item_t* unif_item = calloc(1, sizeof(*unif_item));

                // This number will be the position of the argument
                // within the specialization ! Not of the whole template
                //
                // e.g: template <typename _T> struct A;
                //      template <int _N, typename _Q> struct A<_Q[_N]>;
                //
                // the position here is the one for _N and _Q not the one for _T
                // (because what is being unified is _N <-- ?? and _Q <-- ??)
                DEBUG_CODE()
                {
                    fprintf(stderr, "Unified template parameter (%s) (%d,%d) \n", 
                            s1->symbol_name,
                            s1->entity_specs.template_parameter_nesting,
                            s1->entity_specs.template_parameter_position);
                }
                unif_item->parameter_position = s1->entity_specs.template_parameter_position;
                unif_item->parameter_nesting = s1->entity_specs.template_parameter_nesting;
                unif_item->parameter_name = s1->symbol_name;

                // Copy the type
                type_t* unified_type = t2;
                // We have to remove the inverse qualification. If t1 is const
                // and t2 is const too, the unified type is not const
                //
                // const T <- const int [T = int] and not [T = const int]
                // 
                unified_type = get_cv_qualified_type(unified_type, cv_qualif_2 & (~cv_qualif_1));

                unif_item->value = unified_type;
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
                fprintf(stderr, "Parameter (%s) (%d,%d) already unified\n", 
                        s1->symbol_name,
                        s1->entity_specs.template_parameter_nesting,
                        s1->entity_specs.template_parameter_position);
            }
            // Check is the same unification we are going to do
            if (!equivalent_types(previous_unif, t2, CVE_CONSIDER, decl_context))
            {
                // They're not equivalent, thus not unificable
                DEBUG_CODE()
                {
                    fprintf(stderr, "Previous unification type '%s' does not match the current one '%s'\n",
                            print_declarator(previous_unif, decl_context),
                            print_declarator(t2, decl_context));
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
    else if (is_named_type(t1)
            && (named_type_get_symbol(t1)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Unificating template template parameter\n");
        }

        if (!is_named_type(t2)
                || (named_type_get_symbol(t2)->kind != SK_TEMPLATE_PRIMARY_CLASS
                    && named_type_get_symbol(t2)->kind != SK_TEMPLATE_SPECIALIZED_CLASS
                    && named_type_get_symbol(t2)->kind != SK_TEMPLATE_TEMPLATE_PARAMETER))

        {
            // This cannot be unified at all, only templates are valid here
            return 0;
        }

        scope_entry_t* s1 = named_type_get_symbol(t1);

        type_t* previous_unif = get_type_template_parameter_unification(*unif_set, 
                s1->entity_specs.template_parameter_nesting,
                s1->entity_specs.template_parameter_position);
        if (previous_unif != NULL)
        {
            // Check is the same unification we are going to do
            if (!equivalent_types(previous_unif, t2,  CVE_CONSIDER, decl_context))
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
            // (see a similar comment above)
            DEBUG_CODE()
            {
                fprintf(stderr, "Unified template template parameter (%s) (%d,%d)\n", 
                        s1->symbol_name,
                        s1->entity_specs.template_parameter_nesting,
                        s1->entity_specs.template_parameter_position);
            }
            unif_item->parameter_nesting = s1->entity_specs.template_parameter_nesting;
            unif_item->parameter_position = s1->entity_specs.template_parameter_position;
            unif_item->parameter_name = s1->symbol_name;
            unif_item->value = t2;

            P_LIST_ADD((*unif_set)->unif_list, (*unif_set)->num_elems, unif_item);
        }
        
        // They have been unified
        return 1;
    }

    if (is_named_type(t1) 
            && is_named_type(t2))
    {
        scope_entry_t* entry_t1 = named_type_get_symbol(t1);
        scope_entry_t* entry_t2 = named_type_get_symbol(t2);

        if ((entry_t1->kind == SK_TEMPLATE_PRIMARY_CLASS
                    || entry_t1->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
                && (entry_t2->kind == SK_TEMPLATE_PRIMARY_CLASS
                    || entry_t2->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
                && (strcmp(entry_t1->symbol_name, entry_t2->symbol_name) == 0)
                && same_scope(entry_t1->decl_context.current_scope, entry_t2->decl_context.current_scope))
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

            template_argument_list_t* arguments = 
                template_type_get_template_arguments(get_actual_class_type(t2));
            template_argument_list_t* specialized = 
                template_type_get_template_arguments(get_actual_class_type(t1));

            if (arguments->num_arguments 
                    != specialized->num_arguments)
            {
                internal_error("Number of template arguments differs %d != %d", 
                        arguments->num_arguments,
                        specialized->num_arguments);
            }


            char unificable = 0;

            unificable = match_one_template(arguments, specialized, entry_t1, *unif_set, decl_context);

            return unificable;
        }
    }


    // Structural unification, one of them must succeed
    if (is_non_derived_type(t1)
            && is_non_derived_type(t2))
    {
        // If they were unificable they would have been unified before
        return equivalent_types(t1, t2, CVE_CONSIDER, decl_context);
    }
    else if (is_pointer_type(t1)
            && is_pointer_type(t2))
    {
        return unificate_two_types(pointer_type_get_pointee_type(t1), pointer_type_get_pointee_type(t2), unif_set,
                decl_context);
    }
    else if (is_pointer_to_member_type(t1)
            && is_pointer_to_member_type(t2))
    {
        return unificate_two_types(pointer_type_get_pointee_type(t1), pointer_type_get_pointee_type(t2), unif_set,
                decl_context)
            && unificate_two_types(pointer_to_member_type_get_class_type(t1),
                    pointer_to_member_type_get_class_type(t2), unif_set, decl_context);
    }
    else if (is_array_type(t1)
            && is_array_type(t2))
    {
        // If t1 is NULL, check that t2 is NULL too
        if (array_type_get_array_size_expr(t1) == NULL
                && array_type_get_array_size_expr(t2) != NULL)
        {
            return 0;
        }
        // If t1 is not NULL but t2 is, then this cannot be unified
        if (array_type_get_array_size_expr(t1) != NULL
                && array_type_get_array_size_expr(t2) == NULL)
        {
            return 0;
        }

        if (array_type_get_array_size_expr(t1) != NULL
                && array_type_get_array_size_expr(t2) != NULL)
        {
            if (!unificate_two_expressions(unif_set, 
                        array_type_get_array_size_expr(t1),
                        array_type_get_array_size_expr_context(t1), 
                        array_type_get_array_size_expr(t2),
                        array_type_get_array_size_expr_context(t2)))
            {
                return 0;
            }
        }

        if (!unificate_two_types(array_type_get_element_type(t1), 
                    array_type_get_element_type(t2), 
                    unif_set,
                    decl_context))
        {
            return 0;
        }

        return 1;
    }
    else if (is_function_type(t1)
            && is_function_type(t2))
    {
        if (!unificate_two_types(function_type_get_return_type(t1), 
                    function_type_get_return_type(t2), 
                    unif_set, 
                    decl_context))
        {
            return 0;
        }

        if (function_type_get_num_parameters(t1) != function_type_get_num_parameters(t2))
        {
            return 0;
        }

        if (function_type_get_has_ellipsis(t1) != function_type_get_has_ellipsis(t2))
        {
            return 0;
        }

        int i;
        for (i = 0; i < function_type_get_num_parameters(t1); i++)
        {
            // Fix this should ignore outermost cv qualifier
            type_t* par1 = function_type_get_parameter_type_num(t1, i);
            type_t* par2 = function_type_get_parameter_type_num(t2, i);

            if (!unificate_two_types(par1, par2, unif_set,
                        decl_context))
            {
                return 0;
            }
        }

        return 1;
    }

    // None was structurally unificable
    return 0;
}


char unificate_two_expressions(unification_set_t **unif_set, 
        AST left_tree, decl_context_t left_decl_context, 
        AST right_tree, decl_context_t right_decl_context)
{
    return equivalent_dependent_expressions(left_tree, left_decl_context, right_tree,
            right_decl_context, unif_set);
}

static type_t* get_type_template_parameter_unification(unification_set_t* unif_set, int nesting, int position)
{
    int i;
    for (i = 0; i < unif_set->num_elems; i++)
    {
        if (unif_set->unif_list[i]->parameter_position == position
                && unif_set->unif_list[i]->parameter_nesting == nesting)
        {
            return unif_set->unif_list[i]->value;
        }
    }

    return NULL;
}

static AST get_nontype_template_parameter_unification(unification_set_t* unif_set, int nesting, int position)
{
    int i;
    for (i = 0; i < unif_set->num_elems; i++)
    {
        if (unif_set->unif_list[i]->parameter_position == position
                && unif_set->unif_list[i]->parameter_nesting == nesting)
        {
            return unif_set->unif_list[i]->expression;
        }
    }

    return NULL;
}

static char equivalent_dependent_expressions(AST left_tree, decl_context_t left_decl_context, AST
        right_tree, decl_context_t right_decl_context, unification_set_t** unif_set)
{
    left_tree = advance_expression_nest(left_tree);
    right_tree = advance_expression_nest(right_tree);

    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to unify expression '%s' <- '%s'\n",
                prettyprint_in_buffer(left_tree),
                prettyprint_in_buffer(right_tree));
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to match by means of plain constant evaluation\n");
    }
    
    if (equivalent_expression_trees(left_tree, left_decl_context, right_tree, right_decl_context))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Plain evaluation succeeded. They have the same value\n");
        }
        return 1;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Plain evaluation failed. Trying structural equivalence\n");
        }
    }

    char equal_trees = (ASTType(left_tree) == ASTType(right_tree));
    if (!equal_trees && 
            (ASTType(left_tree) != AST_SYMBOL))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Expression '%s' is different to '%s' and cannot be unified\n", 
                    prettyprint_in_buffer(left_tree), prettyprint_in_buffer(right_tree));
        }
        return 0;
    }

    switch (ASTType(left_tree))
    {
        case AST_SYMBOL :
            {
                // We can reach here with ASTType(left_tree) !=
                // ASTType(right_tree) but ASTType(left_tree) is always an
                // AST_SYMBOL. So, do not assume they are the same!
                DEBUG_CODE()
                {
                    fprintf(stderr, "Left part '%s' is a simple expression\n", prettyprint_in_buffer(left_tree));
                }
                scope_entry_list_t *result = query_id_expression(left_decl_context, left_tree);

                ERROR_CONDITION((result == NULL), "Template argument of specialization '%s', not found",
                        prettyprint_in_buffer(left_tree));

                scope_entry_t* entry = result->entry;
                
                if (entry->kind != SK_TEMPLATE_PARAMETER)
                {
                    // Do not try to unify if the left part is not a nontype template parameter
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Left part is not a nontype template parameter\n");
                    }
                    if (!equal_trees)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Trees are not equal, not unified\n");
                        }
                        return 0;
                    }
                    else
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Trees are equal, trying plain comparison by constant evaluation\n");
                        }

                        return equivalent_expression_trees(left_tree, left_decl_context, right_tree, right_decl_context);
                    }
                }
                else
                {
                    // This can be a non simple type (currently only a pointer or function type)
                    int template_parameter_position = 
                        entry->entity_specs.template_parameter_position;
                    int template_parameter_nesting = 
                        entry->entity_specs.template_parameter_nesting;
                    
                    // Left part is a nontype template parameter
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Left part '%s' is nontype template parameter pos=%d nest=%d\n", 
                                prettyprint_in_buffer(left_tree),
                                template_parameter_position,
                                template_parameter_nesting);
                    }

                    AST previous_unif = get_nontype_template_parameter_unification(
                            *unif_set, 
                            template_parameter_nesting,
                            template_parameter_position);
                    if (previous_unif == NULL)
                    {
                        unification_item_t* unif_item = calloc(1, sizeof(*unif_item));
                        unif_item->expression = right_tree;
                        unif_item->decl_context = right_decl_context;
                        unif_item->parameter_name = ASTText(left_tree);

                        unif_item->parameter_position = template_parameter_position;
                        unif_item->parameter_nesting = template_parameter_nesting;

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

                        // If both are AST_SYMBOL let's rely on plain comparison
                        // otherwise we would start an infinite recursion
                        char equivalent = 0;
                        if ((ASTType(right_tree) == ASTType(previous_unif))
                                && (ASTType(right_tree) == AST_SYMBOL))
                        {
                            scope_entry_list_t *right_entry_list = query_id_expression(right_decl_context, right_tree);

                            ERROR_CONDITION(right_entry_list == NULL, "Right symbol of unification not found", 0);

                            scope_entry_list_t *previous_unif_entry_list = query_id_expression(right_decl_context, previous_unif);

                            ERROR_CONDITION(previous_unif_entry_list == NULL, "Previous unified symbol not found", 0);

                            scope_entry_t* right_entry = right_entry_list->entry;
                            scope_entry_t* previous_entry = previous_unif_entry_list->entry;

                            if (right_entry->kind != SK_TEMPLATE_PARAMETER
                                    || previous_entry->kind != SK_TEMPLATE_PARAMETER)
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Checking previous unification using plain constant evaluation\n");
                                }
                                return equivalent_expression_trees(right_tree, right_decl_context, previous_unif, right_decl_context);
                            }
                            else
                            {
                                int right_template_parameter_position = 
                                    right_entry->entity_specs.template_parameter_position;
                                int right_template_parameter_nesting = 
                                    right_entry->entity_specs.template_parameter_nesting;

                                int previous_template_parameter_position = 
                                    previous_entry->entity_specs.template_parameter_position;
                                int previous_template_parameter_nesting = 
                                    previous_entry->entity_specs.template_parameter_nesting;

                                if ((right_template_parameter_position == previous_template_parameter_position)
                                        && (right_template_parameter_nesting == previous_template_parameter_nesting))
                                {
                                    DEBUG_CODE()
                                    {
                                        fprintf(stderr, "They are the same positional nontype template parameter\n");
                                    }
                                    return 1;
                                }
                            }
                        }
                        else
                        {
                            // Otherwise try a structural match
                            equivalent = equivalent_dependent_expressions(previous_unif,
                                    right_decl_context, right_tree, right_decl_context, unif_set);
                        }
                        if (equivalent)
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
                                fprintf(stderr, "==> They are different\n");
                                fprintf(stderr, "=== Unification failed\n");
                            }
                            return 0;
                        }
                    }
                }

                break;
            }
        case AST_QUALIFIED_ID :
            {
                // There are several restrictions on what can happen here.
                // Redundant code
                //
                // If this had to return 1, it should have been returned before
                // so most of the times this will fail
                return equivalent_expression_trees(left_tree, left_decl_context, right_tree, right_decl_context);
                break;
            }
        case AST_LOGICAL_OR :
        case AST_LOGICAL_AND :
        case AST_BITWISE_OR :
        case AST_BITWISE_XOR :
        case AST_BITWISE_AND :
        case AST_DIFFERENT_OP :
        case AST_EQUAL_OP :
        case AST_LOWER_THAN :
        case AST_GREATER_THAN :
        case AST_GREATER_OR_EQUAL_THAN :
        case AST_LOWER_OR_EQUAL_THAN :
        case AST_SHL_OP :
        case AST_SHR_OP :
        case AST_ADD_OP :
        case AST_MINUS_OP :
        case AST_MULT_OP :
        case AST_MOD_OP :
        case AST_DIV_OP :
            {
                AST left_tree_0 = ASTSon0(left_tree);
                AST left_tree_1 = ASTSon1(left_tree);

                AST right_tree_0 = ASTSon0(right_tree);
                AST right_tree_1 = ASTSon1(right_tree);

                return equivalent_dependent_expressions(left_tree_0,
                        left_decl_context, right_tree_0, right_decl_context, unif_set)
                    && equivalent_dependent_expressions(left_tree_1, 
                            left_decl_context, right_tree_1, right_decl_context, unif_set);
                break;
            }
        case AST_CAST_EXPRESSION :
            // They share a similar tree layout
        case AST_STATIC_CAST : 
        case AST_DYNAMIC_CAST : 
        case AST_REINTERPRET_CAST : 
        case AST_CONST_CAST : 
            {
                // Let's assume they cast onto the same thing
                // otherwise we should check it too ...
                //
                // AST type_id = ASTSon0(a);
                // AST type_spec_seq = ASTSon0(type_id);
                // AST type_spec = ASTSon1(type_spec_seq);
                WARNING_MESSAGE("Currently casting not considered when unificating expressions '%s' and '%s'\n",
                        prettyprint_in_buffer(left_tree),
                        prettyprint_in_buffer(right_tree));

                AST left_cast = ASTSon1(left_tree);
                AST right_cast = ASTSon1(right_tree);

                return equivalent_dependent_expressions(left_cast, left_decl_context,
                        right_cast, right_decl_context, unif_set);
                break;
            }
        case AST_CONDITIONAL_EXPRESSION :
            {
                AST left_condition = ASTSon0(left_tree);
                AST left_true = ASTSon1(left_tree);
                AST left_false = ASTSon2(left_tree);

                AST right_condition = ASTSon0(right_tree);
                AST right_true = ASTSon1(right_tree);
                AST right_false = ASTSon2(right_tree);

                return equivalent_dependent_expressions(left_condition, left_decl_context,
                        right_condition, right_decl_context, unif_set)
                    && equivalent_dependent_expressions(left_true, left_decl_context,
                            right_true, right_decl_context, unif_set)
                    && equivalent_dependent_expressions(left_false, left_decl_context,
                            right_false, right_decl_context, unif_set);
                break;
            }
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
        case AST_CHARACTER_LITERAL :
        case AST_BOOLEAN_LITERAL :
            // Check literal values
            return equivalent_expression_trees(left_tree, left_decl_context, right_tree, right_decl_context);
        case AST_PLUS_OP :
        case AST_NOT_OP :
        case AST_NEG_OP :
        case AST_COMPLEMENT_OP :
            {
                AST left_operand = ASTSon0(left_tree);
                AST right_operand = ASTSon0(right_tree);

                return equivalent_dependent_expressions(left_operand, left_decl_context,
                        right_operand, right_decl_context, unif_set);
            }
        case AST_SIZEOF :
            {
                // If they are expressions they are still workable
                AST left_sizeof = ASTSon0(left_tree);
                AST right_sizeof = ASTSon0(right_tree);

                return equivalent_dependent_expressions(left_sizeof, left_decl_context,
                        right_sizeof, right_decl_context, unif_set);
            }
        case AST_SIZEOF_TYPEID :
            {
                // We should unificate the types
                internal_error("Unification of sizeof(type-id) not yet implemented\n", 0);
                return 0;
            }
        case AST_EXPLICIT_TYPE_CONVERSION :
            {
                // Take the last one
                AST left_expression_list = ASTSon1(left_tree);
                ERROR_CONDITION((ASTSon0(left_expression_list) != NULL), 
                        "In '%s' cannot cast left_tree constant expression formed with an expression list longer than 1", 
                        node_information(left_tree));
                AST left_first_expression = ASTSon1(left_expression_list);

                AST right_expression_list = ASTSon1(right_tree);
                ERROR_CONDITION((ASTSon0(right_expression_list) != NULL), 
                        "In '%s' cannot cast right_tree constant expression formed with an expression list longer than 1", 
                        node_information(right_tree));
                AST right_first_expression = ASTSon1(right_expression_list);

                return equivalent_dependent_expressions(left_first_expression,
                        left_decl_context, right_first_expression, right_decl_context,
                        unif_set);
            }
        case AST_REFERENCE :
            {
                AST left_reference = advance_expression_nest(ASTSon0(left_tree));
                AST right_reference = advance_expression_nest(ASTSon0(right_tree));

                if (ASTType(left_reference) != AST_SYMBOL
                        && ASTType(left_reference) != AST_QUALIFIED_ID
                        && ASTType(right_reference) != AST_SYMBOL
                        && ASTType(right_reference) != AST_QUALIFIED_ID)
                {
                    return 0;
                }
                else
                {
                    // Get the symbol and use its pointer in the symbol table as a value
                    scope_entry_list_t* left_result_list = query_id_expression(left_decl_context,
                            left_reference);
                    scope_entry_list_t* right_result_list = query_id_expression(right_decl_context,
                            right_reference);

                    if (left_result_list == NULL
                            || right_result_list == NULL)
                    {
                        return 0;
                    }

                    return (left_result_list->entry == right_result_list->entry);
                }
            }
        default:
            internal_error("Unknown node type '%s' in %s\n", ast_print_node_type(ASTType(right_tree)), 
                    node_information(right_tree));
            return 0;
            break;
    }
}

static char equivalent_expression_trees(AST left_tree, decl_context_t left_decl_context, AST right_tree, 
        decl_context_t right_decl_context)
{
    literal_value_t left_value = evaluate_constant_expression(left_tree, left_decl_context);
    literal_value_t right_value = evaluate_constant_expression(right_tree, right_decl_context);

    return equal_literal_values(left_value, right_value, right_decl_context);
}
