/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

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
#include "cxx-instantiation.h"

unsigned long long int _bytes_typeunif = 0;

long long int typeunif_used_memory(void)
{
    return _bytes_typeunif;
}

static char equivalent_expression_trees(AST left_tree, AST right_tree);

static void unificate_unresolved_overloaded(type_t* t1, type_t* t2, 
        deduction_set_t** deduction_set, decl_context_t decl_context, 
        const char *filename, int line, deduction_flags_t flags);

// Will try to find a substitution to unificate t1 to t2
//
// e.g.   Q*    can    be unificated to   T**    with   [Q <- T*]
//        T**   cannot be unificated to   Q*
//
void unificate_two_types(type_t* t1, type_t* t2, deduction_set_t** deduction_set, 
        decl_context_t decl_context, const char *filename, int line,
        deduction_flags_t flags)
{
    cv_qualifier_t cv_qualif_1 = CV_NONE;
    cv_qualifier_t cv_qualif_2 = CV_NONE;
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualif_1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualif_2);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Trying to unificate type '%s' <- '%s'\n",
                print_declarator(t1),
                print_declarator(t2));
    }

    if (is_unresolved_overloaded_type(t2))
    {
        // Special case for unresolved overloaded function types:
        //  - try all cases in the hope that any will match
        unificate_unresolved_overloaded(t1, t2, deduction_set, decl_context, filename, line, flags);
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
                        current_deduced_parameter.type))
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
                        print_declarator(current_deduced_parameter.type));
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
                        current_deduced_parameter.type))
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
                        print_declarator(current_deduced_parameter.type));
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
            if (t1_related_symbol != NULL
                    && t1_related_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
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
                                current_deduced_parameter.type))
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
                                print_declarator(current_deduced_parameter.type));
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
                                    deduction_set, decl_context, filename, line, flags);
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
                                    current_arg_2->expression_context,
                                    flags);
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

        // Special case where we unificate with all the bases of a given class
        if (!flags.do_not_allow_conversions
                && is_named_class_type(t1)
                // t1 is a template-id
                && is_template_specialized_type(get_actual_class_type(t1))
                && is_named_class_type(t2)
                && !is_dependent_type(t2))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Unificating against bases\n");
            }

            /*
             * Here we have to instantiate t2 but only if it is an incomplete independent class.
             *
             * Note that this code is invalid (obviously)
             *
             *    template <typename _T>
             *    struct A 
             *    { 
             *    };
             *    
             *    template <typename _T>
             *    struct B;
             *    
             *    template <typename _T>
             *    void f(A<_T>*);
             *    
             *    void g(void)
             *    {
             *        B<float> *b;
             *    
             *        f(b);
             *    }
             *
             * while the following is valid
             *
             *    template <typename _T>
             *    struct A 
             *    { 
             *    };
             *    
             *    template <typename _T>
             *    struct B : A<_T> { };
             *    
             *    template <typename _T>
             *    void f(A<_T>*);
             *    
             *    void g(void)
             *    {
             *        B<float> *b;
             *    
             *        f(b);  // will call f(A<int>*)
             *    }
             */

            if (class_type_is_incomplete_independent(get_actual_class_type(t2)))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: Instantiating class '%s' since we will try to unificate against bases\n",
                            print_declarator(t2));
                }

                instantiate_template_class(named_type_get_symbol(t2), decl_context, filename, line);

                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: Class '%s' instantiated, now we can proceed to check bases\n",
                            print_declarator(t2));
                }
            }

            scope_entry_list_t* all_bases = class_type_get_all_bases(get_actual_class_type(t2), /* include_dependent */ 0);
            scope_entry_list_t* it = all_bases;

            while (it != NULL)
            {
                scope_entry_t* entry = it->entry;

                unificate_two_types(t1, get_user_defined_type(entry), deduction_set, decl_context, filename, line, flags);

                it = it->next;
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Bases unificated\n");
            }
        }

        // Nothing else to do
        return;
    }
    else if (is_pointer_type(t1)
            && is_pointer_type(t2))
    {
        unificate_two_types(pointer_type_get_pointee_type(t1), pointer_type_get_pointee_type(t2), deduction_set,
                decl_context, filename, line, flags);
        return;
    }
    else if (is_lvalue_reference_type(t1)
            && is_lvalue_reference_type(t2))
    {
        unificate_two_types(reference_type_get_referenced_type(t1), reference_type_get_referenced_type(t2), deduction_set,
                decl_context, filename, line, flags);
        return;
    }
    else if (is_pointer_to_member_type(t1)
            && is_pointer_to_member_type(t2))
    {
        unificate_two_types(pointer_type_get_pointee_type(t1), pointer_type_get_pointee_type(t2), deduction_set,
                decl_context, filename, line, flags);
        unificate_two_types(pointer_to_member_type_get_class_type(t1),
                pointer_to_member_type_get_class_type(t2), deduction_set, decl_context, filename, line,
                flags);
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
                    array_type_get_array_size_expr_context(t2),
                    flags);
        }

        unificate_two_types(array_type_get_element_type(t1), 
                array_type_get_element_type(t2), 
                deduction_set,
                decl_context,
                filename, line, flags);

        return;
    }
    else if (is_function_type(t1)
            && is_function_type(t2))
    {
        unificate_two_types(function_type_get_return_type(t1), 
                function_type_get_return_type(t2), 
                deduction_set, 
                decl_context,
                filename, line, flags);

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
                        decl_context, filename, line, flags);
            }
        }

        return;
    }
}

void unificate_two_expressions(deduction_set_t **deduction_set, 
        AST left_tree, decl_context_t left_decl_context, 
        AST right_tree, decl_context_t right_decl_context,
        deduction_flags_t flags)
{
    equivalent_dependent_expressions(left_tree, left_decl_context, right_tree,
            right_decl_context, deduction_set, flags);
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
        right_tree, decl_context_t right_decl_context, deduction_set_t** unif_set,
        deduction_flags_t flags)
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

    type_t* left_type = expression_get_type(left_tree);
    type_t* right_type = expression_get_type(right_tree);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: left_type = %s\n", print_declarator(left_type));
        fprintf(stderr, "TYPEUNIF: right_type = %s\n", print_declarator(right_type));
    }

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
        if (equivalent_expression_trees(left_tree, right_tree))
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
                type_t* expr_type = expression_get_type(left_tree);

                scope_entry_t* left_symbol = NULL;
                if (expr_type != NULL
                        && is_template_parameter_name(left_tree)
                        && ((left_symbol = lookup_template_parameter_name(left_decl_context, left_tree)) != NULL)
                        && left_symbol->kind == SK_TEMPLATE_PARAMETER)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEUNIF: Left part '%s' found to be a nontype template parameter\n", 
                                prettyprint_in_buffer(left_tree));
                    }
                    deduction_t* deduction = get_unification_item_template_parameter(unif_set,
                            left_symbol);

                    deduced_parameter_t current_deduced_parameter;
                    memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

                    current_deduced_parameter.expression = right_tree;
                    current_deduced_parameter.decl_context = right_decl_context;
                    current_deduced_parameter.type = left_symbol->type_information;

                    // Fold if possible
                    if (expression_is_constant(right_tree))
                    {
                        current_deduced_parameter.expression = 
                            const_value_to_tree(expression_get_constant(right_tree));
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
                        type_t* previous_deduced_expr_type = expression_get_type(previous_deduced_expression);

                        scope_entry_t* previous_unified_template_param = NULL;
                        scope_entry_t* right_tree_symbol = NULL;
                        if (previous_deduced_expr_type != NULL
                                && is_template_parameter_name(previous_deduced_expression)
                                && ((previous_unified_template_param = 
                                        lookup_template_parameter_name(previous_deduced_parameter->decl_context,
                                            previous_deduced_expression)) != NULL)
                                && previous_unified_template_param->kind == SK_TEMPLATE_PARAMETER

                                && is_template_parameter_name(right_tree)
                                && ((right_tree_symbol = lookup_template_parameter_name(right_decl_context, 
                                    right_tree)) != NULL)
                                && right_tree_symbol->kind == SK_TEMPLATE_PARAMETER)
                        {
                            int previous_unified_expr_parameter_position = 
                                previous_unified_template_param->entity_specs.template_parameter_position;
                            int previous_unified_expr_parameter_nesting = 
                                previous_unified_template_param->entity_specs.template_parameter_nesting;

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
                                        right_decl_context, right_tree, right_decl_context, unif_set, flags))
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

                    scope_entry_t* right_symbol = NULL;
                    if (is_template_parameter_name(right_tree)
                            && ((right_symbol = lookup_template_parameter_name(right_decl_context, right_tree)) != NULL)
                            && right_symbol->kind == SK_TEMPLATE_PARAMETER)
                    {
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
                // Perform a query and compare their dependent types
                scope_entry_list_t* result_left = query_id_expression_flags(left_decl_context, 
                        left_tree, DF_DEPENDENT_TYPENAME);
                scope_entry_list_t* result_right = query_id_expression_flags(right_decl_context, 
                        right_tree, DF_DEPENDENT_TYPENAME);

                if (result_left == NULL
                        || result_right == NULL)
                    return 0;

                if (result_left->entry->kind != SK_DEPENDENT_ENTITY
                        || result_right->entry->kind != SK_DEPENDENT_ENTITY)
                    return 0;

                // Both are dependent entities now, compare their dependent types
                type_t* dependent_type_left = result_left->entry->type_information;
                type_t* dependent_type_right = result_right->entry->type_information;

                return equivalent_types(dependent_type_left, dependent_type_right);
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
                        left_decl_context, right_tree_0, right_decl_context, unif_set, flags)
                    && equivalent_dependent_expressions(left_tree_1, 
                            left_decl_context, right_tree_1, right_decl_context, unif_set, flags);
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
                        right_cast, right_decl_context, unif_set, flags);
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
                        right_condition, right_decl_context, unif_set, flags)
                    && equivalent_dependent_expressions(left_true, left_decl_context,
                            right_true, right_decl_context, unif_set, flags)
                    && equivalent_dependent_expressions(left_false, left_decl_context,
                            right_false, right_decl_context, unif_set, flags);
                break;
            }
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
        case AST_CHARACTER_LITERAL :
        case AST_BOOLEAN_LITERAL :
            // Check literal values
            return equivalent_expression_trees(left_tree, right_tree);
        case AST_PLUS_OP :
        case AST_NOT_OP :
        case AST_NEG_OP :
        case AST_COMPLEMENT_OP :
            {
                AST left_operand = ASTSon0(left_tree);
                AST right_operand = ASTSon0(right_tree);

                return equivalent_dependent_expressions(left_operand, left_decl_context,
                        right_operand, right_decl_context, unif_set, flags);
            }
        case AST_SIZEOF :
            {
                // If they are expressions they are still workable
                AST left_sizeof = ASTSon0(left_tree);
                AST right_sizeof = ASTSon0(right_tree);

                return equivalent_dependent_expressions(left_sizeof, left_decl_context,
                        right_sizeof, right_decl_context, unif_set, flags);
            }
        case AST_SIZEOF_TYPEID :
            {
                type_t* sizeof_left_type = expression_get_type(ASTSon0(left_tree));
                type_t* sizeof_right_type = expression_get_type(ASTSon0(right_tree));

                // We do not unificate sizeofs (though we could), just assert
                // if the sizeof'd type is the same
                return equivalent_types(sizeof_left_type, sizeof_right_type);
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
                        unif_set, flags);
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
        case AST_GXX_TYPE_TRAITS:
            {
                // fprintf(stderr, "%s: warning: (still) unsupported type traits '%s' and '%s' comparison. Assuming equals\n",
                //         prettyprint_in_buffer(right_tree),
                //         prettyprint_in_buffer(left_tree));
                // FIXME
                return 1;
            }
        default:
            internal_error("Unknown node type '%s' in %s\n", ast_print_node_type(ASTType(right_tree)), 
                    ast_location(right_tree));
            return 0;
            break;
    }
}

static char equivalent_expression_trees(AST left_tree, AST right_tree)
{
    return const_value_is_nonzero(
            const_value_eq(
                expression_get_constant(left_tree),
                expression_get_constant(right_tree)));
}

char same_functional_expression(AST left_tree, decl_context_t left_decl_context, AST right_tree, 
        decl_context_t right_decl_context, deduction_flags_t flags)
{
    deduction_set_t* deduction_set = counted_calloc(1, sizeof(*deduction_set), &_bytes_typeunif);

    return equivalent_dependent_expressions(left_tree, left_decl_context, right_tree, right_decl_context, 
            &deduction_set, flags);
}

static void unificate_unresolved_overloaded(type_t* t1, type_t* t2, 
        deduction_set_t** deduction_set, decl_context_t decl_context,
        const char *filename, int line,
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Unifying with an unresolved overloaded type. Unfolding it\n");
    }
    scope_entry_list_t* it = unresolved_overloaded_type_get_overload_set(t2);

    template_argument_list_t* explicit_template_arguments 
        = unresolved_overloaded_type_get_explicit_template_arguments(t2);

    char is_template = 0;

    while (it != NULL)
    {
        scope_entry_t* entry = it->entry;

        type_t* function_type = NULL;

        if (entry->kind == SK_FUNCTION)
        {
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
                        argument_list, template_parameters,
                        decl_context, line, filename);

                // Update entry and its function type
                entry = named_type_get_symbol(named_specialization_type);
                function_type = entry->type_information;

                is_template = 1;
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

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Trying unification with %s '%s' at '%s:%d' of type '%s'\n",
                    is_template ? "[template]" : "",
                    entry->symbol_name,
                    entry->file,
                    entry->line,
                    print_declarator(function_type));
        }

        // Now perform deduction
        unificate_two_types(t1, function_type, deduction_set, decl_context, filename, line, flags);

        it = it->next;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Unification of unresolved overloaded types ended\n");
    }
}

#if 0
static void unificate_template_arguments_(template_argument_list_t* targ_list_1, 
        template_argument_list_t* targ_list_2, 
        decl_context_t decl_context, 
        deduction_set_t** deduction_set,
        const char* filename,
        int line,
        deduction_flags_t flags)
{
    if (targ_list_1->num_arguments != targ_list_2->num_arguments)
    {
        return;
    }

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
                            deduction_set, decl_context, filename, line, flags);
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
                            current_arg_2->expression_context, 
                            flags);
                }
                break;
            default:
                {
                    internal_error("Invalid template argument kind", 0);
                }
        }
    }
}

static void unificate_template_id_(AST left_tree, AST right_tree, 
        decl_context_t left_decl_context, 
        decl_context_t right_decl_context, 
        deduction_set_t** deduction_set,
        deduction_flags_t flags)
{
    ERROR_CONDITION(ASTType(left_tree) != AST_TEMPLATE_ID
            || ASTType(right_tree) != AST_TEMPLATE_ID, "Wrong trees, must be template-id both", 0);

    if (strcmp(ASTText(ASTSon0(left_tree)), ASTText(ASTSon0(right_tree))) != 0)
            return;

    template_argument_list_t* targ_list_1 =
        get_template_arguments_from_syntax(ASTSon1(left_tree),
                left_decl_context, 
                /* nesting_level */ 0);

    template_argument_list_t* targ_list_2 =
        get_template_arguments_from_syntax(ASTSon1(right_tree),
                right_decl_context, 
                /* nesting_level */ 0);

    // It does not matter very much which decl context we use
    unificate_template_arguments_(targ_list_1,
            targ_list_2,
            left_decl_context,
            deduction_set,
            ASTFileName(left_tree),
            ASTLine(left_tree),
            flags);
}
#endif

#if 0
static void unificate_unqualified_id_(AST left_tree, 
        AST right_tree, 
        decl_context_t left_decl_context,
        decl_context_t right_decl_context,
        deduction_set_t** deduction_set,
        deduction_flags_t flags)
{
    if (ASTType(left_tree) == ASTType(right_tree))
    {
        switch (ASTType(left_tree))
        {
            case AST_SYMBOL:
            case AST_OPERATOR_FUNCTION_ID:
            case AST_CONVERSION_FUNCTION_ID :
            case AST_DESTRUCTOR_ID :
                {
                    return;
                    // Do nothing for these
                }
            case AST_DESTRUCTOR_TEMPLATE_ID :
                {
                    return unificate_template_id_(ASTSon0(left_tree),
                            ASTSon1(right_tree), left_decl_context,
                            right_decl_context, deduction_set, flags);
                }
            case AST_TEMPLATE_ID :
                {
                    return unificate_template_id_(left_tree, right_tree,
                            left_decl_context, right_decl_context,
                            deduction_set, flags);
                }
            default:
                {
                    internal_error("Unhandled node type '%s'\n", ast_print_node_type(ASTType(left_tree)));
                }
        }
    }
}

static void unificate_nested_name_specifier_(AST left_tree, 
        AST right_tree, 
        decl_context_t left_decl_context,
        decl_context_t right_decl_context,
        deduction_set_t** deduction_set,
        deduction_flags_t flags)
{
    if ((ASTSon1(left_tree) == NULL
                && ASTSon1(right_tree) != NULL)
            || (ASTSon1(left_tree) != NULL
             && ASTSon1(right_tree) == NULL))
        return;

    AST left_nest = ASTSon0(left_tree);
    AST right_nest = ASTSon1(right_tree);
    if (ASTType(left_nest) == ASTType(right_nest))
    {
        // If the name is totally dependent, we have to rely on a plain syntactic comparison
        switch (ASTType(left_nest))
        {
            case AST_SYMBOL:
                {
                    // Do nothing
                    break;
                }
            case AST_TEMPLATE_ID:
                {
                    // This is enough
                    unificate_template_id_(left_nest, right_nest, left_decl_context, right_decl_context, deduction_set, flags);
                    break;
                }
            default:
                internal_error("Unhandled node '%s'\n", ast_print_node_type(ASTType(left_nest)));
        }
    }

    // More nested specifiers to be checked
    if (ASTSon1(left_tree) != NULL
            && ASTSon1(right_tree) != NULL)
    {
        unificate_nested_name_specifier_(ASTSon1(left_tree),
                ASTSon1(right_tree), left_decl_context, right_decl_context,
                deduction_set, flags);
    }
}

static void unificate_nested_name_specifier_first_(AST left_tree, 
        AST right_tree, 
        decl_context_t left_decl_context,
        decl_context_t right_decl_context,
        deduction_set_t** deduction_set,
        deduction_flags_t flags)
{
    if ((ASTSon1(left_tree) == NULL
                && ASTSon1(right_tree) != NULL)
            || (ASTSon1(left_tree) != NULL
             && ASTSon1(right_tree) == NULL))
        return;

    AST left_nest = ASTSon0(left_tree);
    AST right_nest = ASTSon1(right_tree);

    // The first qualifier is more interesting since it can involve
    // type-template parameters and template-template parameters
    //
    // Examples:
    //
    //    template <typename _T>  A<_T> :: B
    //    template <typename _T> _T :: B
    //    template <template <typename> class _W> _W<int> :: B
    // 
    // The issue here is that the thing being unificated in these cases
    // may be qualified as well, so
    //  
    //    _T :: iterator  unifies with std :: vector<int> :: iterator
    //           with _T <- std::vector<int>
    // 
    // So, whenever we find a template type-template parameter we have
    // to match it with another one
    
    if (ASTType(left_nest) == AST_SYMBOL)
    {
        scope_entry_list_t* entry_list = query_id_expression(left_decl_context, left_nest);

        if (entry_list == NULL
                || entry_list->next != NULL)
            return;

        scope_entry_t* entry = entry_list->entry;

        if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
        {
            // Try to match the right part to it

            char keep_advancing = 1;

            // Advance until we find a class or another template-type
            while (keep_advancing)
            {
                switch (ASTType(right_nest))
                {
                    case AST_SYMBOL:
                    case AST_TEMPLATE_ID:
                        {
                            scope_entry_list_t* entry_list_2 = query_id_expression(right_decl_context, right_nest);
                            if (entry_list_2 == NULL
                                    || entry_list_2->next != NULL)
                                return;

                            scope_entry_t* entry2 = entry_list_2->entry;


                            if (entry2->kind == SK_TYPEDEF)
                            {
                                type_t* aliased_type = advance_over_typedefs(entry2->type_information);

                                if (!is_named_type(aliased_type))
                                    return;

                                entry2 = named_type_get_symbol(aliased_type);
                            }

                            if (entry2->kind == SK_TEMPLATE_TYPE_PARAMETER
                                    || entry2->kind == SK_CLASS)
                            {
                                deduced_parameter_t current_deduced_parameter;
                                memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

                                if (entry2->kind == SK_TEMPLATE_TYPE_PARAMETER)
                                {
                                    current_deduced_parameter.type = get_user_defined_type(entry2);
                                }
                                else
                                {
                                    current_deduced_parameter.type = entry2->type_information;
                                }

                                // Create an unification item
                                deduction_t* deduction = get_unification_item_template_parameter(
                                        deduction_set, entry);

                                char found = 0;
                                int i;
                                for (i = 0; i < deduction->num_deduced_parameters; i++)
                                {
                                    // Do not readd if they are the same type
                                    deduced_parameter_t* previous_deduced_parameter = deduction->deduced_parameters[i];

                                    type_t* previous_deduced_type = previous_deduced_parameter->type;

                                    if (equivalent_types(previous_deduced_type, 
                                                current_deduced_parameter.type))
                                    {
                                        found = 1;
                                        break;
                                    }
                                }
                                if (!found)
                                {
                                    deduced_parameter_t* new_deduced_parameter = counted_calloc(1, sizeof(*new_deduced_parameter), &_bytes_typeunif);
                                    *new_deduced_parameter = current_deduced_parameter;

                                    P_LIST_ADD(deduction->deduced_parameters, deduction->num_deduced_parameters, new_deduced_parameter);
                                }
                                keep_advancing = 0;
                            }
                            else if (entry2->kind == SK_NAMESPACE)
                            {
                                right_decl_context = entry2->namespace_decl_context;
                                // keep advancing in this case

                                // Advance right tree
                                if (ASTSon1(right_tree) == NULL)
                                    return;

                                right_tree = ASTSon1(right_tree);
                                right_nest = ASTSon0(right_tree);
                            }
                            break;
                        }
                    default:
                        {
                            internal_error("Unhandled node type '%s'\n", ast_print_node_type(ASTType(right_nest)));
                        }
                }
            }
        }
    }
    else if (ASTType(left_nest) == AST_TEMPLATE_ID)
    {
        // FIXME - template template parameters!!!
        unificate_template_id_(left_nest, right_nest, left_decl_context, right_decl_context, deduction_set, flags);
    }

    // More nested specifiers to be checked
    if (ASTSon1(left_tree) != NULL
            && ASTSon1(right_tree) != NULL)
    {
        unificate_nested_name_specifier_(ASTSon1(left_tree),
                ASTSon1(right_tree), left_decl_context, right_decl_context,
                deduction_set, flags);
    }
}

static void unificate_id_expressions_(AST left_id_expr, AST right_id_expr,
        decl_context_t left_decl_context, 
        decl_context_t right_decl_context,
        deduction_set_t** deduction_set,
        deduction_flags_t flags)
{
    if (ASTType(left_id_expr) != ASTType(right_id_expr))
        return;

    switch (ASTType(left_id_expr))
    {
        case AST_QUALIFIED_ID:
        case AST_QUALIFIED_TEMPLATE:
            {
                char left_has_global = ASTSon0(left_id_expr) != NULL;
                char right_has_global = ASTSon0(right_id_expr) != NULL;

                if (left_has_global != right_has_global)
                    return;

                char left_has_qualif = ASTSon1(left_id_expr) != NULL;
                char right_has_qualif = ASTSon1(right_id_expr) != NULL;

                if (left_has_qualif != right_has_qualif)
                    return;

                if (left_has_qualif)
                {
                    unificate_nested_name_specifier_first_(
                            ASTSon1(left_id_expr),
                            ASTSon1(right_id_expr), 
                            left_decl_context,
                            right_decl_context, 
                            deduction_set,
                            flags);
                }

                unificate_unqualified_id_(
                        ASTSon2(left_id_expr),
                        ASTSon2(right_id_expr),
                        left_decl_context,
                        right_decl_context,
                        deduction_set,
                        flags);
                break;
            }
        default:
            {
                unificate_unqualified_id_(
                        left_id_expr,
                        right_id_expr,
                        left_decl_context,
                        right_decl_context,
                        deduction_set,
                        flags);
            }
    }
}

static const char* get_name_of_template_parameter(
        template_parameter_list_t* template_parameters,
        int nesting,
        int position)
{
    int i;
    for (i = 0; i < template_parameters->num_template_parameters; i++)
    {
        template_parameter_t* current_template_parameter 
            = template_parameters->template_parameters[i];

        if ((current_template_parameter->entry->entity_specs.template_parameter_nesting == nesting)
                && (current_template_parameter->entry->entity_specs.template_parameter_position == position))
        {
            return current_template_parameter->entry->symbol_name;
        }
    }

    internal_error("Not found template parameter with nest=%d and position=%d",
            nesting, position);
}
#endif

deduction_flags_t deduction_flags_empty()
{
    deduction_flags_t flags;
    memset(&flags, 0, sizeof(flags));

    return flags;
}
