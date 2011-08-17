/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
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
#include "cxx-entrylist.h"
#include "cxx-codegen.h"

unsigned long long int _bytes_typeunif = 0;

long long int typeunif_used_memory(void)
{
    return _bytes_typeunif;
}

static char equivalent_expression_trees(nodecl_t left_tree, nodecl_t right_tree);

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

    // No unification is ever possible for this type
    if (is_braced_list_type(t2))
    {
        // Unificate with each of the types of the braced list type
        int i, num_types = braced_list_type_get_num_types(t2);
        for (i = 0; i < num_types; i++)
        {
            unificate_two_types(t1, braced_list_type_get_type_num(t2, i), 
                    deduction_set, decl_context, filename, line, flags);
        }

        // Nothing else remains
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

            template_parameter_list_t *targ_list_1 
                = template_specialized_type_get_template_arguments(get_actual_class_type(t1));
            template_parameter_list_t *targ_list_2 
                = template_specialized_type_get_template_arguments(get_actual_class_type(t2));

            int i;
            for (i = 0; i < targ_list_1->num_parameters; i++)
            {
                template_parameter_value_t* current_arg_1 = targ_list_1->arguments[i];
                template_parameter_value_t* current_arg_2 = targ_list_2->arguments[i];

                switch (current_arg_1->kind)
                {
                    case TPK_TEMPLATE:
                    case TPK_TYPE:
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
                    case TPK_NONTYPE:
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "TYPEUNIF: Unificating nontype-template argument %d\n",
                                        i);
                            }
                            unificate_two_expressions(deduction_set, 
                                    current_arg_1->value,
                                    current_arg_2->value,
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

            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(all_bases);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* entry = entry_list_iterator_current(it);
                unificate_two_types(t1, get_user_defined_type(entry), deduction_set, decl_context, filename, line, flags);
            }
            entry_list_iterator_free(it);
            entry_list_free(all_bases);

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

        if (!nodecl_is_null(array_type_get_array_size_expr(t1))
                && !nodecl_is_null(array_type_get_array_size_expr(t2)))
        {
            unificate_two_expressions(deduction_set, 
                    array_type_get_array_size_expr(t1),
                    array_type_get_array_size_expr(t2),
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

static char equivalent_dependent_expressions(nodecl_t left_tree, 
        nodecl_t right_tree, 
        deduction_set_t** unif_set,
        deduction_flags_t flags);

void unificate_two_expressions(deduction_set_t **deduction_set, 
        nodecl_t left_tree, 
        nodecl_t right_tree, 
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Attempting to unificate expression '%s' <- '%s'\n",
                c_cxx_codegen_to_str(left_tree),
                c_cxx_codegen_to_str(right_tree));
    }

    equivalent_dependent_expressions(left_tree, right_tree,
            deduction_set, flags);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Unification of expression '%s' <- '%s' ended\n",
                c_cxx_codegen_to_str(left_tree),
                c_cxx_codegen_to_str(right_tree));
    }
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

static char equivalent_dependent_expressions_cxx_dependent_expr(AST left_tree, AST right_tree,
        deduction_set_t** unif_set,
        deduction_flags_t flags);

static char equivalent_dependent_expressions(nodecl_t left_tree, 
        nodecl_t right_tree, 
        deduction_set_t** unif_set,
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Checking whether %s and %s are equivalent\n",
                c_cxx_codegen_to_str(left_tree),
                c_cxx_codegen_to_str(right_tree));
    }
    if (nodecl_expr_is_value_dependent(left_tree)
            || nodecl_expr_is_value_dependent(right_tree))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Some of the expressions is dependent, falling back to structural equivalence\n");
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Trying to match %s and %s by means of plain constant evaluation\n",
                    c_cxx_codegen_to_str(left_tree),
                    c_cxx_codegen_to_str(right_tree));
        }
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

    if (!nodecl_expr_is_value_dependent(left_tree)
            && !nodecl_expr_is_value_dependent(right_tree))
    {
        scope_entry_t* left_symbol = nodecl_get_symbol(left_tree);
        scope_entry_t* right_symbol = nodecl_get_symbol(right_tree);

        if (left_symbol != NULL
                && left_symbol == right_symbol)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Left tree '%s' and right tree '%s'"
                        " are the same symbol. They are trivially equivalent\n", 
                        c_cxx_codegen_to_str(left_tree),
                        c_cxx_codegen_to_str(right_tree));
            }
            return 1;
        }

        if (right_symbol != NULL)
        {
            // Advance right value
            if (!nodecl_is_null(right_symbol->value))
                return equivalent_dependent_expressions(left_tree, right_symbol->value, unif_set, flags);
        }

        if (left_symbol != NULL)
        {
            // Advance left value
            if (left_symbol->kind != SK_TEMPLATE_PARAMETER // A template parameter may have a value of no interest here
                    && !nodecl_is_null(left_symbol->value))
                return equivalent_dependent_expressions(left_symbol->value, right_tree, unif_set, flags);

            // Try to unify using this template parameter
            if (left_symbol->kind == SK_TEMPLATE_PARAMETER)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: Left part '%s' found to be a nontype template parameter\n", 
                            c_cxx_codegen_to_str(left_tree));
                }
                deduction_t* deduction = get_unification_item_template_parameter(unif_set,
                        left_symbol);

                deduced_parameter_t current_deduced_parameter;
                memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

                current_deduced_parameter.value = right_tree;
                current_deduced_parameter.type = left_symbol->type_information;

                // Fold if possible
                if (nodecl_is_constant(right_tree))
                {
                    current_deduced_parameter.value = 
                        const_value_to_nodecl(nodecl_get_constant(right_tree));
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

                    nodecl_t previous_deduced_value = previous_deduced_parameter->value;

                    scope_entry_t* previous_unified_symbol = nodecl_get_symbol(previous_deduced_value);

                    if (previous_unified_symbol != NULL
                            && right_symbol != NULL
                            && previous_unified_symbol->kind == SK_TEMPLATE_PARAMETER
                            && right_symbol->kind == SK_TEMPLATE_PARAMETER)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEUNIF: In previous deduction both left tree '%s' and right tree '%s'"
                                    " are nontype template parameters. Checking if they are the same\n", 
                                    c_cxx_codegen_to_str(left_tree),
                                    c_cxx_codegen_to_str(right_tree));
                        }

                        int previous_unified_expr_parameter_position = 
                            previous_unified_symbol->entity_specs.template_parameter_position;
                        int previous_unified_expr_parameter_nesting = 
                            previous_unified_symbol->entity_specs.template_parameter_nesting;

                        int currently_unified_template_param_position = 
                            right_symbol->entity_specs.template_parameter_position;
                        int currently_unified_template_param_nesting = 
                            right_symbol->entity_specs.template_parameter_nesting;

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
                                    c_cxx_codegen_to_str(previous_deduced_value),
                                    c_cxx_codegen_to_str(right_tree));
                        }
                        if (equivalent_dependent_expressions(previous_deduced_value,
                                    right_tree, unif_set, flags))
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
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEUNIF: Added unification for '%s' <- '%s'\n",
                                c_cxx_codegen_to_str(left_tree),
                                c_cxx_codegen_to_str(right_tree));
                    }
                }

                char equivalent = 0;

                if (right_symbol != NULL
                        && right_symbol->kind == SK_TEMPLATE_PARAMETER)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEUNIF: Checking if it is exactly the same template parameter '%s' and '%s'\n",
                                c_cxx_codegen_to_str(left_tree),
                                c_cxx_codegen_to_str(right_tree));
                    }
                    if ((right_symbol->entity_specs.template_parameter_nesting 
                                == left_symbol->entity_specs.template_parameter_nesting)
                            && (right_symbol->entity_specs.template_parameter_position 
                                == left_symbol->entity_specs.template_parameter_position))
                    {
                        found = 1;
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEUNIF: They are the same template parameter\n");
                        }
                        equivalent = 1;
                    }
                    else
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEUNIF: They are different template parameters\n");
                        }
                        equivalent = 0;
                    }
                }

                return equivalent;
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Left part '%s' is not a nontype template parameter, they can't be equivalent expressions\n", 
                        c_cxx_codegen_to_str(left_tree));
            }

            return 0;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Nodecls are not comparable\n");
        }

        return 0;
    }
    else if (nodecl_expr_is_value_dependent(left_tree)
            && nodecl_expr_is_value_dependent(right_tree))
    {
        decl_context_t dummy;
        internal_error("Not yet implemented", 0);
        // return equivalent_dependent_expressions_cxx_dependent_expr(nodecl_unwrap_cxx_dependent_expr(left_tree, &dummy),
        //         nodecl_unwrap_cxx_dependent_expr(right_tree, &dummy),
        //         unif_set,
        //         flags);
    }
    else
    {
        internal_error("Not yet implemented", 0);

        // // These are special cases we allow
        // if (nodecl_expr_is_value_dependent(left_tree))
        // {
        //     decl_context_t dummy;
        //     AST left_expr = nodecl_unwrap_cxx_dependent_expr(left_tree, &dummy);

        //     DEBUG_CODE()
        //     {
        //         fprintf(stderr, "TYPEUNIF: Left is C++ raw '%s' (%s), maybe it has a symbol\n", prettyprint_in_buffer(left_expr), 
        //                 ast_print_node_type(ASTType(left_expr)));
        //     }

        //     internal_error("Not yet implemented", 0);
        //     // if (expression_has_symbol(left_expr))
        //     // {
        //     //     scope_entry_t* sym = expression_get_symbol(left_expr);
        //     //     return equivalent_dependent_expressions(nodecl_make_symbol(sym, sym->file, sym->line), right_tree, unif_set, flags);
        //     // }
        // }
        // if (nodecl_expr_is_value_dependent(right_tree))
        // {
        //     decl_context_t dummy;
        //     AST right_expr = nodecl_unwrap_cxx_dependent_expr(right_tree, &dummy);

        //     DEBUG_CODE()
        //     {
        //         fprintf(stderr, "TYPEUNIF: Right is C++ raw '%s' (%s), maybe it has a symbol\n", prettyprint_in_buffer(right_expr), 
        //                 ast_print_node_type(ASTType(right_expr)));
        //     }

        //     internal_error("Not yet implemented", 0);
        //     // if (expression_has_symbol(right_expr))
        //     // {
        //     //     scope_entry_t* sym = expression_get_symbol(right_expr);
        //     //     return equivalent_dependent_expressions(left_tree, nodecl_make_symbol(sym, sym->file, sym->line), unif_set, flags);
        //     // }
        // }

        // DEBUG_CODE()
        // {
        //     fprintf(stderr, "TYPEUNIF: One is C++ raw and other is nodecl, this can't be equivalent\n");
        // }
        return 0;
    }

    return 0;
}

static char equivalent_dependent_expressions_cxx_dependent_expr(AST left_tree, AST right_tree,
        deduction_set_t** unif_set,
        deduction_flags_t flags)
{
    left_tree = advance_expression_nest(left_tree);
    right_tree = advance_expression_nest(right_tree);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Checking whether raw C++ trees %s and %s are equivalent\n",
                prettyprint_in_buffer(left_tree),
                prettyprint_in_buffer(right_tree));
    }

    nodecl_t nodecl_left = expression_get_nodecl(left_tree);
    if (!nodecl_is_null(nodecl_left)
            && nodecl_expr_is_value_dependent(nodecl_left))
    {
        nodecl_left = nodecl_null();
    }

    nodecl_t nodecl_right = expression_get_nodecl(right_tree);
    if (!nodecl_is_null(nodecl_right)
            && nodecl_expr_is_value_dependent(nodecl_right))
    {
        nodecl_right = nodecl_null();
    }

    // Try to go back to nodecl if possible
    if (!nodecl_is_null(nodecl_left)
            && !nodecl_is_null(nodecl_right))
        return equivalent_dependent_expressions(nodecl_left, nodecl_right, unif_set, flags);

    if (ASTType(left_tree) != ASTType(right_tree))
        return 0;

    switch (ASTType(left_tree))
    {
        case AST_SYMBOL :
        case AST_QUALIFIED_ID :
            {
                internal_error("Not yet implemented", 0);

#if 0
                scope_entry_t* left_symbol = expression_get_symbol(left_tree);
                scope_entry_t* right_symbol = expression_get_symbol(right_tree);

                if (left_symbol != NULL
                        && right_symbol != NULL)
                {
                    return equivalent_dependent_expressions(
                            nodecl_make_symbol(left_symbol, left_symbol->file, left_symbol->line),
                            nodecl_make_symbol(right_symbol, right_symbol->file, right_symbol->line),
                            unif_set, flags);
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: One of the symbolic trees lacks a symbol, they can't be equivalent\n");
                }
                return 0;
#endif
                break;
            }
        case AST_LOGICAL_OR :
        case AST_LOGICAL_AND :
        case AST_BITWISE_OR :
        case AST_BITWISE_XOR :
        case AST_BITWISE_AND :
        case AST_DIFFERENT :
        case AST_EQUAL :
        case AST_LOWER_THAN :
        case AST_GREATER_THAN :
        case AST_GREATER_OR_EQUAL_THAN :
        case AST_LOWER_OR_EQUAL_THAN :
        case AST_SHL :
        case AST_SHR :
        case AST_ADD :
        case AST_MINUS :
        case AST_MUL :
        case AST_MOD :
        case AST_DIV :
            {
                AST left_tree_0 = ASTSon0(left_tree);
                AST left_tree_1 = ASTSon1(left_tree);

                AST right_tree_0 = ASTSon0(right_tree);
                AST right_tree_1 = ASTSon1(right_tree);

                return equivalent_dependent_expressions_cxx_dependent_expr(left_tree_0,
                        right_tree_0, unif_set, flags)
                    && equivalent_dependent_expressions_cxx_dependent_expr(left_tree_1, 
                            right_tree_1, unif_set, flags);
                break;
            }
        case AST_CAST :
            // They share a similar tree layout
        case AST_STATIC_CAST : 
        case AST_DYNAMIC_CAST : 
        case AST_REINTERPRET_CAST : 
        case AST_CONST_CAST : 
            {
                internal_error("Code unreachable 4\n", 0);
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

                return equivalent_dependent_expressions_cxx_dependent_expr(left_condition, 
                        right_condition, unif_set, flags)
                    && equivalent_dependent_expressions_cxx_dependent_expr(left_true, 
                            right_true, unif_set, flags)
                    && equivalent_dependent_expressions_cxx_dependent_expr(left_false, 
                            right_false, unif_set, flags);
                break;
            }
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
        case AST_CHARACTER_LITERAL :
        case AST_BOOLEAN_LITERAL :
            {
                // Check literal values
                internal_error("Code unreachable 5\n", 0);
                break;
            }
        case AST_PLUS :
        case AST_LOGICAL_NOT :
        case AST_NEG :
        case AST_BITWISE_NOT :
            {
                AST left_operand = ASTSon0(left_tree);
                AST right_operand = ASTSon0(right_tree);

                return equivalent_dependent_expressions_cxx_dependent_expr(left_operand, 
                        right_operand, unif_set, flags);
            }
        case AST_SIZEOF :
            {
                // If they are expressions they are still workable
                AST left_sizeof = ASTSon0(left_tree);
                AST right_sizeof = ASTSon0(right_tree);

                return equivalent_dependent_expressions_cxx_dependent_expr(left_sizeof, 
                        right_sizeof, unif_set, flags);
            }
        case AST_SIZEOF_TYPEID :
            {
                internal_error("Not yet implemented", 0);
#if 0
                type_t* sizeof_left_type = expression_get_type(ASTSon0(left_tree));
                type_t* sizeof_right_type = expression_get_type(ASTSon0(right_tree));

                // We do not unificate sizeofs (though we could), just assert
                // if the sizeof'd type is the same
                return equivalent_types(sizeof_left_type, sizeof_right_type);
#endif
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

                return equivalent_dependent_expressions_cxx_dependent_expr(left_first_expression,
                        right_first_expression, 
                        unif_set, flags);
            }
        case AST_REFERENCE :
            {
                AST left_reference = advance_expression_nest(ASTSon0(left_tree));
                AST right_reference = advance_expression_nest(ASTSon0(right_tree));

                return equivalent_dependent_expressions_cxx_dependent_expr(left_reference,
                        right_reference, 
                        unif_set, flags);
            }
        case AST_GXX_TYPE_TRAITS:
            {
                // fprintf(stderr, "%s: warning: (still) unsupported type traits '%s' and '%s' comparison. Assuming equals\n",
                //         prettyprint_in_buffer(right_tree),
                //         prettyprint_in_buffer(left_tree));
                // FIXME
                internal_error("Code unreachable 6", 0);
                return 1;
            }
        default:
            internal_error("Unknown node type '%s' in %s\n", ast_print_node_type(ASTType(right_tree)), 
                    ast_location(right_tree));
            return 0;
            break;
    }

    return 0;
}

static char equivalent_expression_trees(nodecl_t left_tree, nodecl_t right_tree)
{
    if (nodecl_is_constant(left_tree)
            && nodecl_is_constant(right_tree))
    {
        return const_value_is_nonzero(
                const_value_eq(
                    nodecl_get_constant(left_tree),
                    nodecl_get_constant(right_tree)));
    }
    return 0;
}

char same_functional_expression(nodecl_t left_tree, nodecl_t right_tree, 
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Checking whether '%s' and '%s' are functionally equivalent\n",
                c_cxx_codegen_to_str(left_tree),
                c_cxx_codegen_to_str(right_tree));
    }
    deduction_set_t* deduction_set = counted_calloc(1, sizeof(*deduction_set), &_bytes_typeunif);

    char c = equivalent_dependent_expressions(left_tree, right_tree,  
            &deduction_set, flags);

    // Free it, it is unused after this
    free(deduction_set);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: '%s' and '%s' %s functionally equivalent\n",
                c_cxx_codegen_to_str(left_tree),
                c_cxx_codegen_to_str(right_tree),
                c ? "ARE" : "are NOT");
    }

    return c;
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
    scope_entry_list_t* overloaded_set = unresolved_overloaded_type_get_overload_set(t2);

    template_parameter_list_t* explicit_template_parameters 
        = unresolved_overloaded_type_get_explicit_template_arguments(t2);

    char is_template = 0;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(overloaded_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

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
                template_specialized_type_get_template_arguments(specialized_function_type);

            deduction_set_t* deduction_result = NULL;
            if (deduce_arguments_from_call_to_specific_template_function(/* no arguments */ NULL,
                        /* num_arguments */ 0, specialization_type, template_parameters,
                        decl_context, &deduction_result, filename, line, 
                        explicit_template_parameters))
            {
                template_parameter_list_t* argument_list = build_template_parameter_list_from_deduction_set(
                        template_parameters,
                        deduction_result);

                // Now get a specialized template type for this
                // function (this will sign it in if it does not exist)
                type_t* named_specialization_type = template_type_get_specialized_type(entry->type_information,
                        argument_list, decl_context, line, filename);

                // Update entry and its function type
                entry = named_type_get_symbol(named_specialization_type);
                function_type = entry->type_information;

                is_template = 1;
            }
            else
            {
                // Ignore this one
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
    }
    entry_list_iterator_free(it);
    entry_list_free(overloaded_set);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Unification of unresolved overloaded types ended\n");
    }
}

deduction_flags_t deduction_flags_empty()
{
    deduction_flags_t flags;
    memset(&flags, 0, sizeof(flags));

    return flags;
}
