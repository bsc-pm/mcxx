/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "cxx-typededuc.h"
#include "cxx-exprtype.h"
#include "cxx-cexpr.h"
#include "cxx-scope.h"
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-solvetemplate.h"
#include "cxx-prettyprint.h"

unsigned long long int _bytes_typeunif = 0;

long long int typeunif_used_memory(void)
{
    return _bytes_typeunif;
}

static char equivalent_expression_trees(AST left_tree, decl_context_t left_decl_context, AST right_tree, 
        decl_context_t right_decl_context);

static void unificate_unresolved_overloaded(type_t* t1, type_t* t2, 
        deduction_set_t** deduction_set, decl_context_t decl_context, 
        const char *filename, int line);

// Will try to find a substitution to unificate t1 to t2
//
// e.g.   Q*    can    be unificated to   T**    with   [Q <- T*]
//        T**   cannot be unificated to   Q*
//
void unificate_two_types(type_t* t1, type_t* t2, deduction_set_t** deduction_set, 
        decl_context_t decl_context, const char *filename, int line)
{
    cv_qualifier_t cv_qualif_1 = CV_NONE;
    cv_qualifier_t cv_qualif_2 = CV_NONE;
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualif_1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualif_2);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Trying to unificate type '%s' <- '%s'\n",
                print_declarator(t1, decl_context),
                print_declarator(t2, decl_context));
    }

    if (is_unresolved_overloaded_type(t2))
    {
        // Special case for unresolved overloaded function types:
        //  - try all cases in the hope that any will match
        unificate_unresolved_overloaded(t1, t2, deduction_set, decl_context, filename, line);
        // Nothing else must be done with this t2
        return;
    }

    // 1. If it is a template parameter (or a user defined type pointing to it)
    // then save a deduction
    if (is_named_type(t1) 
            && named_type_get_symbol(t1)->kind == SK_TEMPLATE_TYPE_PARAMETER
            /* && is_less_or_equal_cv_qualified(cv_qualif_1, cv_qualif_2) */)
    {
        scope_entry_t* s1 = named_type_get_symbol(t1);
        deduction_t* deduction = get_unification_item_template_parameter(
                deduction_set, s1);

        deduced_parameter_t current_deduced_parameter;
        memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

        current_deduced_parameter.type = get_cv_qualified_type(t2, cv_qualif_2 & (~cv_qualif_1));

        char found = 0;
        int i;
        for (i = 0; i < deduction->num_deduced_parameters; i++)
        {
            deduced_parameter_t* previous_deduced_parameter = deduction->deduced_parameters[i];

            type_t* previous_deduced_type = previous_deduced_parameter->type;

            if (equivalent_types(previous_deduced_type, 
                        current_deduced_parameter.type, 
                        decl_context))
            {
                found = 1;
                break;
            }
        }

        if (!found)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Type deduction for type template parameter (%s) (%d,%d) with value '%s' \n", 
                        s1->symbol_name,
                        s1->entity_specs.template_parameter_nesting,
                        s1->entity_specs.template_parameter_position,
                        print_declarator(current_deduced_parameter.type, decl_context));
            }

            deduced_parameter_t* new_deduced_parameter = counted_calloc(1, sizeof(*new_deduced_parameter), &_bytes_typeunif);
            *new_deduced_parameter = current_deduced_parameter;

            P_LIST_ADD(deduction->deduced_parameters, deduction->num_deduced_parameters, new_deduced_parameter);
        }
        // Nothing else to do
        return;
    }
    // template template parameters are handled a bit different
    else if (is_named_type(t1)
            && (named_type_get_symbol(t1)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
    {
        // Only other template-template parameters are valid here or template-names of
        // classes
        if (!is_named_type(t2)
                || (named_type_get_symbol(t2)->kind != SK_TEMPLATE
                    && named_type_get_symbol(t2)->kind != SK_TEMPLATE_TEMPLATE_PARAMETER))
        {
            // Nothing else to do
            return;
        }

        scope_entry_t* s1 = named_type_get_symbol(t1);

        deduction_t* deduction = get_unification_item_template_parameter(
                deduction_set, s1);

        deduced_parameter_t current_deduced_parameter;
        memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

        current_deduced_parameter.type = t2;

        char found = 0;
        int i;
        for (i = 0; i < deduction->num_deduced_parameters; i++)
        {
            deduced_parameter_t* previous_deduced_parameter = deduction->deduced_parameters[i];

            type_t* previous_deduced_type = previous_deduced_parameter->type;

            if (equivalent_types(previous_deduced_type, 
                        current_deduced_parameter.type,
                        decl_context))
            {
                found = 1;
                break;
            }
        }

        if (!found)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Type deduction for template template parameter (%s) (%d,%d) with value '%s' \n", 
                        s1->symbol_name,
                        s1->entity_specs.template_parameter_nesting,
                        s1->entity_specs.template_parameter_position,
                        print_declarator(current_deduced_parameter.type, decl_context));
            }

            deduced_parameter_t* new_deduced_parameter = counted_calloc(1, sizeof(*new_deduced_parameter), &_bytes_typeunif);
            *new_deduced_parameter = current_deduced_parameter;

            P_LIST_ADD(deduction->deduced_parameters, deduction->num_deduced_parameters, new_deduced_parameter);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Unificating template template parameter\n");
        }

        // Nothing else to do
        return;
    }

    // Structural unification, one of them must succeed
    if (is_non_derived_type(t1)
            && is_non_derived_type(t2))
    {
        // Unificate template types
        //
        // A<_T> <- A<int>
        //
        // or
        //
        // _V<_T> <- A<int>
        //
        if (is_named_class_type(t1)
                && is_named_class_type(t2)
                && is_template_specialized_type(get_actual_class_type(t1))
                && is_template_specialized_type(get_actual_class_type(t2))
                && ((template_specialized_type_get_related_template_type(get_actual_class_type(t1)) 
                        == template_specialized_type_get_related_template_type(get_actual_class_type(t2)))
                    // Allow t1 to be a template-template parameter
                    || (template_type_get_related_symbol(
                            template_specialized_type_get_related_template_type(
                                get_actual_class_type(t1)))->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                    )
           )
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Class type is a specialized one, unificating arguments\n");
            }

            scope_entry_t* t1_related_symbol =
                template_type_get_related_symbol(
                        template_specialized_type_get_related_template_type(
                            get_actual_class_type(t1)));

            // If t1 is a template-template parameter it means that t1 can be
            // unified to the template type of t2
            if (t1_related_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: This is a template-id built after a template-template parameter\n");
                }

                // Now we request an unification item for this template-parameter
                deduction_t* deduction = get_unification_item_template_parameter(
                        deduction_set, t1_related_symbol);

                deduced_parameter_t current_deduced_parameter;
                memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

                scope_entry_t* t2_related_symbol = 
                    template_type_get_related_symbol(
                            template_specialized_type_get_related_template_type(
                                get_actual_class_type(t2)));

                current_deduced_parameter.type = get_user_defined_type(t2_related_symbol);

                char found = 0;
                int i;
                for (i = 0; i < deduction->num_deduced_parameters; i++)
                {
                    deduced_parameter_t* previous_deduced_parameter = deduction->deduced_parameters[i];

                    type_t* previous_deduced_type = previous_deduced_parameter->type;

                    if (equivalent_types(previous_deduced_type, 
                                current_deduced_parameter.type,
                                decl_context))
                    {
                        found = 1;
                        break;
                    }
                }

                if (!found)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEUNIF: Type deduction for template template parameter (%s) (%d,%d) with value '%s' \n", 
                                t1_related_symbol->symbol_name,
                                t1_related_symbol->entity_specs.template_parameter_nesting,
                                t1_related_symbol->entity_specs.template_parameter_position,
                                print_declarator(current_deduced_parameter.type, decl_context));
                    }

                    deduced_parameter_t* new_deduced_parameter = counted_calloc(1, sizeof(*new_deduced_parameter), &_bytes_typeunif);
                    *new_deduced_parameter = current_deduced_parameter;

                    P_LIST_ADD(deduction->deduced_parameters, deduction->num_deduced_parameters, new_deduced_parameter);
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: Unificating template template parameter against template-name in a template-id\n");
                }
            }

            template_argument_list_t *targ_list_1 
                = template_specialized_type_get_template_arguments(get_actual_class_type(t1));
            template_argument_list_t *targ_list_2 
                = template_specialized_type_get_template_arguments(get_actual_class_type(t2));

            int i;
            for (i = 0; i < targ_list_1->num_arguments; i++)
            {
                template_argument_t* current_arg_1 = targ_list_1->argument_list[i];
                template_argument_t* current_arg_2 = targ_list_2->argument_list[i];

                switch (current_arg_1->kind)
                {
                    case TAK_TEMPLATE:
                    case TAK_TYPE:
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "TYPEUNIF: Unificating template/type-template argument %d\n",
                                        i);
                            }
                            unificate_two_types(current_arg_1->type, current_arg_2->type, 
                                    deduction_set, decl_context, filename, line);
                        }
                        break;
                    case TAK_NONTYPE:
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "TYPEUNIF: Unificating nontype-template argument %d\n",
                                        i);
                            }
                            unificate_two_expressions(deduction_set, 
                                    current_arg_1->expression,
                                    current_arg_1->expression_context,
                                    current_arg_2->expression,
                                    current_arg_2->expression_context);
                        }
                        break;
                    default:
                        {
                            internal_error("Invalid template argument kind", 0);
                        }
                }
            }
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Arguments unificated\n");
            }
            // Nothing else to do
            return;
        }

        // Nothing else to do
        return;
    }
    else if (is_pointer_type(t1)
            && is_pointer_type(t2))
    {
        unificate_two_types(pointer_type_get_pointee_type(t1), pointer_type_get_pointee_type(t2), deduction_set,
                decl_context, filename, line);
        return;
    }
    else if (is_lvalue_reference_type(t1)
            && is_lvalue_reference_type(t2))
    {
        unificate_two_types(reference_type_get_referenced_type(t1), reference_type_get_referenced_type(t2), deduction_set,
                decl_context, filename, line);
        return;
    }
    else if (is_pointer_to_member_type(t1)
            && is_pointer_to_member_type(t2))
    {
        unificate_two_types(pointer_type_get_pointee_type(t1), pointer_type_get_pointee_type(t2), deduction_set,
                decl_context, filename, line);
        unificate_two_types(pointer_to_member_type_get_class_type(t1),
                pointer_to_member_type_get_class_type(t2), deduction_set, decl_context, filename, line);
        return;
    }
    else if (is_array_type(t1)
            && is_array_type(t2))
    {
        if (array_type_get_array_size_expr(t1) != NULL
                && array_type_get_array_size_expr(t2) != NULL)
        {
            unificate_two_expressions(deduction_set, 
                    array_type_get_array_size_expr(t1),
                    array_type_get_array_size_expr_context(t1), 
                    array_type_get_array_size_expr(t2),
                    array_type_get_array_size_expr_context(t2));
        }

        unificate_two_types(array_type_get_element_type(t1), 
                array_type_get_element_type(t2), 
                deduction_set,
                decl_context,
                filename, line);

        return;
    }
    else if (is_function_type(t1)
            && is_function_type(t2))
    {
        unificate_two_types(function_type_get_return_type(t1), 
                function_type_get_return_type(t2), 
                deduction_set, 
                decl_context,
                filename, line);

        if (function_type_get_num_parameters(t1) == function_type_get_num_parameters(t2)
                && (function_type_get_has_ellipsis(t1) == function_type_get_has_ellipsis(t2)))
        {
            int num_parameters = function_type_get_num_parameters(t1);
            if (function_type_get_has_ellipsis(t1))
                num_parameters--;

            int i;
            for (i = 0; i < num_parameters; i++)
            {
                // Fix this should ignore outermost cv qualifier
                type_t* par1 = function_type_get_parameter_type_num(t1, i);
                type_t* par2 = function_type_get_parameter_type_num(t2, i);

                unificate_two_types(par1, par2, deduction_set,
                        decl_context, filename, line);
            }
        }

        return;
    }
}

void unificate_two_expressions(deduction_set_t **deduction_set, 
        AST left_tree, decl_context_t left_decl_context, 
        AST right_tree, decl_context_t right_decl_context)
{
    equivalent_dependent_expressions(left_tree, left_decl_context, right_tree,
            right_decl_context, deduction_set);
}

deduction_t* get_unification_item_template_parameter(deduction_set_t** deduction_set, scope_entry_t* s1)
{
    int position = s1->entity_specs.template_parameter_position;
    int nesting = s1->entity_specs.template_parameter_nesting;

    ERROR_CONDITION(!s1->entity_specs.is_template_parameter,
            "This must be a template-parameter", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Getting unification item for template parameter '%s'\n",
                s1->symbol_name);
    }

    int i;
    for (i = 0; i < (*deduction_set)->num_deductions; i++)
    {
        if ((*deduction_set)->deduction_list[i]->parameter_position == position
                && (*deduction_set)->deduction_list[i]->parameter_nesting == nesting)
        {
            return (*deduction_set)->deduction_list[i];
        }
    }

    deduction_t* result = counted_calloc(1, sizeof(*result), &_bytes_typeunif);
    switch (s1->kind)
    {
        case SK_TEMPLATE_PARAMETER:
            {
                result->kind = TPK_NONTYPE;
                break;
            }
        case SK_TEMPLATE_TYPE_PARAMETER :
            {
                result->kind = TPK_TYPE;
                break;
            }
        case SK_TEMPLATE_TEMPLATE_PARAMETER:
            {
                result->kind = TPK_TEMPLATE;
                break;
            }
        default:
            internal_error("Invalid symbol kind", 0);
    }

    result->parameter_position = s1->entity_specs.template_parameter_position;
    result->parameter_nesting = s1->entity_specs.template_parameter_nesting;
    result->parameter_name = s1->symbol_name;

    P_LIST_ADD((*deduction_set)->deduction_list, (*deduction_set)->num_deductions, result);

    return result;
}

char equivalent_dependent_expressions(AST left_tree, decl_context_t left_decl_context, AST
        right_tree, decl_context_t right_decl_context, deduction_set_t** unif_set)
{
    left_tree = advance_expression_nest(left_tree);
    right_tree = advance_expression_nest(right_tree);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Trying to unify expression '%s' <- '%s'\n",
                prettyprint_in_buffer(left_tree),
                prettyprint_in_buffer(right_tree));
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Trying to match by means of plain constant evaluation\n");
    }

    type_t* left_type = ASTExprType(left_tree);
    type_t* right_type = ASTExprType(right_tree);

    if ((left_type != NULL && is_dependent_expr_type(left_type))
            || (right_type != NULL && is_dependent_expr_type(right_type)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Some of the expressions is dependent, falling back to structural equivalence\n");
        }
    }
    else
    {
        if (equivalent_expression_trees(left_tree, left_decl_context, right_tree, right_decl_context))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Plain evaluation succeeded. They have the same value\n");
            }
            return 1;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Plain evaluation failed. Trying structural equivalence\n");
            }
        }
    }

    char equal_trees = (ASTType(left_tree) == ASTType(right_tree));
    if (!equal_trees && 
            (ASTType(left_tree) != AST_SYMBOL))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Expression '%s' is different to '%s' and cannot be unified\n", 
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
                    fprintf(stderr, "TYPEUNIF: Left part '%s' is a simple expression\n", prettyprint_in_buffer(left_tree));
                }

                // If left part is a nontype template parameter, then try to unify
                type_t* expr_type = ASTExprType(left_tree);
                if (expr_type != NULL
                        && is_dependent_expr_type(expr_type)
                        && is_named_type(expr_type)
                        && named_type_get_symbol(expr_type)->kind == SK_TEMPLATE_PARAMETER)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEUNIF: Left part '%s' found to be a nontype template parameter\n", 
                                prettyprint_in_buffer(left_tree));
                    }
                    scope_entry_t* left_symbol = named_type_get_symbol(expr_type);
                    deduction_t* deduction = get_unification_item_template_parameter(unif_set,
                            left_symbol);

                    deduced_parameter_t current_deduced_parameter;
                    memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

                    current_deduced_parameter.expression = right_tree;
                    current_deduced_parameter.decl_context = right_decl_context;
                    current_deduced_parameter.type = left_symbol->type_information;

                    // Fold if possible
                    {
                        type_t* right_tree_type = ASTExprType(right_tree);
                        if (!is_dependent_expression(right_tree, right_decl_context)
                                && (right_tree_type == NULL
                                    || !is_unresolved_overloaded_type(right_tree_type)))
                        {
                            literal_value_t literal 
                                = evaluate_constant_expression(right_tree, right_decl_context);
                            current_deduced_parameter.expression = tree_from_literal_value(literal);
                        }
                    }

                    char found = 0;
                    int i;
                    for (i = 0; i < deduction->num_deduced_parameters; i++)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEUNIF: Checking previous deductions\n");
                        }
                        deduced_parameter_t* previous_deduced_parameter = deduction->deduced_parameters[i];

                        AST previous_deduced_expression = previous_deduced_parameter->expression;
                        type_t* previous_deduced_expr_type = ASTExprType(previous_deduced_expression);
                        type_t* right_tree_type = ASTExprType(right_tree);

                        if (previous_deduced_expr_type != NULL
                                && is_dependent_expr_type(previous_deduced_expr_type)
                                && is_named_type(previous_deduced_expr_type)
                                && named_type_get_symbol(previous_deduced_expr_type)->kind == SK_TEMPLATE_PARAMETER

                                && right_tree_type != NULL
                                && is_dependent_expr_type(right_tree_type)
                                && is_named_type(right_tree_type)
                                && named_type_get_symbol(right_tree_type)->kind == SK_TEMPLATE_PARAMETER)
                        {
                            scope_entry_t* previous_unified_template_param = 
                                named_type_get_symbol(previous_deduced_expr_type);
                            int previous_unified_expr_parameter_position = 
                                previous_unified_template_param->entity_specs.template_parameter_position;
                            int previous_unified_expr_parameter_nesting = 
                                previous_unified_template_param->entity_specs.template_parameter_nesting;

                            scope_entry_t* right_tree_symbol = 
                                named_type_get_symbol(right_tree_type);
                            int currently_unified_template_param_position = 
                                right_tree_symbol->entity_specs.template_parameter_position;
                            int currently_unified_template_param_nesting = 
                                right_tree_symbol->entity_specs.template_parameter_nesting;

                            DEBUG_CODE()
                            {
                                fprintf(stderr, "TYPEUNIF: In previous deduction both left tree '%s' and right tree '%s'"
                                        " are nontype template parameters. Checking if they are the same\n", 
                                        prettyprint_in_buffer(left_tree),
                                        prettyprint_in_buffer(right_tree));
                            }

                            if ((currently_unified_template_param_position == previous_unified_expr_parameter_position)
                                    && (currently_unified_template_param_nesting == previous_unified_expr_parameter_nesting))
                            {
                                found = 1;
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "TYPEUNIF: They are the same\n");
                                }
                            }
                            else
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "TYPEUNIF: They are different\n");
                                }
                            }
                        }
                        else
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "TYPEUNIF: Checking if previous deduced expression '%s' and right part '%s'"
                                        " are the same\n", 
                                        prettyprint_in_buffer(previous_deduced_expression),
                                        prettyprint_in_buffer(right_tree));
                            }
                            if (equivalent_dependent_expressions(previous_deduced_expression,
                                        right_decl_context, right_tree, right_decl_context, unif_set))
                            {
                                found = 1;
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "TYPEUNIF: They are the same\n");
                                }
                            }
                            else
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "TYPEUNIF: They are different\n");
                                }
                            }
                        }
                    }

                    if (!found)
                    {
                        deduced_parameter_t* new_deduced_parameter = counted_calloc(1, sizeof(*new_deduced_parameter), &_bytes_typeunif);
                        *new_deduced_parameter = current_deduced_parameter;

                        P_LIST_ADD(deduction->deduced_parameters, deduction->num_deduced_parameters, new_deduced_parameter);
                    }

                    char equivalent = 1;
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEUNIF: Checking if it is exactly the same template parameter '%s' and '%s'\n",
                                prettyprint_in_buffer(left_tree),
                                prettyprint_in_buffer(right_tree));
                    }
                    type_t* right_tree_type = ASTExprType(right_tree);

                    if (right_tree_type != NULL
                            && is_dependent_expr_type(right_tree_type)
                            && is_named_type(right_tree_type)
                            && named_type_get_symbol(right_tree_type)->kind == SK_TEMPLATE_PARAMETER)
                    {
                        scope_entry_t* right_symbol = named_type_get_symbol(right_tree_type);

                        if ((right_symbol->entity_specs.template_parameter_nesting 
                                    == left_symbol->entity_specs.template_parameter_nesting)
                                && (right_symbol->entity_specs.template_parameter_position 
                                    == left_symbol->entity_specs.template_parameter_position))
                        {
                            found = 1;
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "TYPEUNIF: They are the same\n");
                            }
                            equivalent = 1;
                        }
                        else
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "TYPEUNIF: They are different\n");
                            }
                            equivalent = 0;
                        }
                    }

                    return equivalent;
                }

                // If it had to much it would have done before
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: Left part '%s' is not a nontype template parameter\n", 
                            prettyprint_in_buffer(left_tree));
                }
                return 0;

                break;
            }
        case AST_QUALIFIED_ID :
            {
                AST global_op_1 = ASTSon0(left_tree);
                AST nested_name_1 = ASTSon1(left_tree);
                AST unqualified_part_1 = ASTSon2(left_tree);

                AST global_op_2 = ASTSon0(right_tree);
                AST nested_name_2 = ASTSon1(right_tree);
                AST unqualified_part_2 = ASTSon2(right_tree);

                if ((global_op_1 == NULL
                        || global_op_2 == NULL)
                        &&  (global_op_1 != global_op_2))
                {
                    return 0;
                }

                // Doing a syntactic comparison since anything else can be compared here
                // as an expression ...
                return syntactic_comparison_of_nested_names(nested_name_1, nested_name_2, 
                        unqualified_part_1, unqualified_part_2, left_decl_context);
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
                        ast_location(left_tree));
                AST left_first_expression = ASTSon1(left_expression_list);

                AST right_expression_list = ASTSon1(right_tree);
                ERROR_CONDITION((ASTSon0(right_expression_list) != NULL), 
                        "In '%s' cannot cast right_tree constant expression formed with an expression list longer than 1", 
                        ast_location(right_tree));
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
                    ast_location(right_tree));
            return 0;
            break;
    }
}

static char equivalent_expression_trees(AST left_tree, decl_context_t left_decl_context, AST right_tree, 
        decl_context_t right_decl_context)
{
    literal_value_t literal1 = evaluate_constant_expression(left_tree, left_decl_context);
    literal_value_t literal2 = evaluate_constant_expression(right_tree, right_decl_context);

    return equal_literal_values(literal1, literal2, right_decl_context);
}

char same_functional_expression(AST left_tree, decl_context_t left_decl_context, AST right_tree, 
        decl_context_t right_decl_context)
{
    deduction_set_t* deduction_set = counted_calloc(1, sizeof(*deduction_set), &_bytes_typeunif);

    return equivalent_dependent_expressions(left_tree, left_decl_context, right_tree, right_decl_context, 
            &deduction_set);
}

static void unificate_unresolved_overloaded(type_t* t1, type_t* t2, 
        deduction_set_t** deduction_set, decl_context_t decl_context,
        const char *filename, int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Unifying with an unresolved overloaded type. Unfolding it\n");
    }
    scope_entry_list_t* it = unresolved_overloaded_type_get_overload_set(t2);

    template_argument_list_t* explicit_template_arguments 
        = unresolved_overloaded_type_get_explicit_template_arguments(t2);

    while (it != NULL)
    {
        scope_entry_t* entry = it->entry;

        type_t* function_type = NULL;

        if (entry->kind == SK_FUNCTION)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Trying unification with '%s' at '%s:%d' of type '%s'\n",
                        entry->symbol_name,
                        entry->file,
                        entry->line,
                        print_declarator(entry->type_information, decl_context));
            }
            function_type = entry->type_information;
        }
        else if (entry->kind == SK_TEMPLATE)
        {
            // Try to deduce it with what we are given
            type_t* specialization_type = template_type_get_primary_type(entry->type_information);
            scope_entry_t* specialization_symbol = named_type_get_symbol(specialization_type);
            type_t* specialized_function_type = specialization_symbol->type_information;

            template_parameter_list_t* template_parameters = 
                template_specialized_type_get_template_parameters(specialized_function_type);

            deduction_set_t* deduction_result = NULL;
            if (deduce_arguments_from_call_to_specific_template_function(/* no arguments */ NULL,
                        /* num_arguments */ 0, specialization_type, template_parameters,
                        decl_context, &deduction_result, filename, line, 
                        explicit_template_arguments))
            {
                template_argument_list_t* argument_list = build_template_argument_list_from_deduction_set(
                        deduction_result);

                // Now get a specialized template type for this
                // function (this will sign it in if it does not exist)
                type_t* named_specialization_type = template_type_get_specialized_type(entry->type_information,
                        argument_list, /* no template parameters */ template_parameters,
                        decl_context, line, filename);

                // Update entry and its function type
                entry = named_type_get_symbol(named_specialization_type);
                function_type = entry->type_information;

                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: Trying unification with [template] '%s' at '%s:%d' of type '%s'\n",
                            entry->symbol_name,
                            entry->file,
                            entry->line,
                            print_declarator(entry->type_information, decl_context));
                }
            }
            else
            {
                // Ignore this one
                it = it->next;
                continue;
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        if (entry->entity_specs.is_member
                && !entry->entity_specs.is_static)
        {
            function_type = get_pointer_to_member_type(function_type,
                    named_type_get_symbol(entry->entity_specs.class_type));
        }

        // Now perform deduction
        unificate_two_types(t1, function_type, deduction_set, decl_context, filename, line);

        it = it->next;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Unification of unresolved overloaded types ended\n");
    }
}
