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
static char equivalent_dependent_expressions(AST left_tree, scope_t* left_scope, AST
        right_tree, scope_t* right_scope, unification_set_t** unif_set,
        decl_context_t decl_context);
static char equivalent_expression_trees(AST left_tree, scope_t* left_scope, AST right_tree, 
        scope_t* right_scope, decl_context_t decl_context);

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
        fprintf(stderr, "Trying to unificate type '%s' <- '%s'\n",
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

                    char is_ellipse_1 = t1->function->parameter_list[i]->is_ellipsis;
                    char is_ellipse_2 = t2->function->parameter_list[i]->is_ellipsis;

                    // This cannot be unified
                    if (is_ellipse_1 != is_ellipse_2)
                        return 0;

                    // This is OK "..." can be unified with "..."
                    if (is_ellipse_1 && is_ellipse_2)
                        continue;

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

    return equivalent_dependent_expressions(left_tree, left_scope, right_tree,
            right_scope, unif_set, decl_context);
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

static char equivalent_dependent_expressions(AST left_tree, scope_t* left_scope, AST
        right_tree, scope_t* right_scope, unification_set_t** unif_set,
        decl_context_t decl_context)
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
    
    if (equivalent_expression_trees(left_tree, left_scope, right_tree, right_scope, decl_context))
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
                scope_entry_list_t *result = query_id_expression(left_scope, left_tree,
                        FULL_UNQUALIFIED_LOOKUP, decl_context);

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

                        return equivalent_expression_trees(left_tree, left_scope, right_tree, right_scope, decl_context);
                    }
                }
                else
                {
                    // Left part is a nontype template parameter
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Left part is a nontype template parameter\n");
                    }
                    
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
                        unif_item->expr_scope = right_scope;
                        unif_item->parameter_name = ASTText(left_tree);

                        unif_item->parameter_num = template_parameter_num;
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
                            scope_entry_list_t *right_entry_list = query_id_expression(right_scope, right_tree,
                                    FULL_UNQUALIFIED_LOOKUP, decl_context);

                            ERROR_CONDITION(right_entry_list == NULL, "Right symbol of unification not found", 0);

                            scope_entry_list_t *previous_unif_entry_list = query_id_expression(right_scope, previous_unif,
                                    FULL_UNQUALIFIED_LOOKUP, decl_context);

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
                                return equivalent_expression_trees(right_tree, right_scope, previous_unif, right_scope, decl_context);
                            }
                            else
                            {
                                int right_template_parameter_num = base_type(right_entry->type_information)->type->template_parameter_num;
                                int right_template_parameter_nesting = base_type(right_entry->type_information)->type->template_parameter_nesting;

                                int previous_template_parameter_num = base_type(previous_entry->type_information)->type->template_parameter_num;
                                int previous_template_parameter_nesting = base_type(previous_entry->type_information)->type->template_parameter_nesting;

                                if ((right_template_parameter_num == previous_template_parameter_num)
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
                                    right_scope, right_tree, right_scope, unif_set, decl_context);
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
                return equivalent_expression_trees(left_tree, left_scope, right_tree, right_scope, decl_context);
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
                        left_scope, right_tree_0, right_scope, unif_set,
                        decl_context) 
                    && equivalent_dependent_expressions(left_tree_1, 
                            left_scope, right_tree_1, right_scope, unif_set, 
                            decl_context);
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

                return equivalent_dependent_expressions(left_cast, left_scope,
                        right_cast, right_scope, unif_set, decl_context);
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

                return equivalent_dependent_expressions(left_condition, left_scope,
                        right_condition, right_scope, unif_set, decl_context)
                    && equivalent_dependent_expressions(left_true, left_scope,
                            right_true, right_scope, unif_set, decl_context)
                    && equivalent_dependent_expressions(left_false, left_scope,
                            right_false, right_scope, unif_set, decl_context);
                break;
            }
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
        case AST_CHARACTER_LITERAL :
        case AST_BOOLEAN_LITERAL :
            // Check literal values
            return equivalent_expression_trees(left_tree, left_scope, right_tree, right_scope, decl_context);
        case AST_PLUS_OP :
        case AST_NOT_OP :
        case AST_NEG_OP :
        case AST_COMPLEMENT_OP :
            {
                AST left_operand = ASTSon0(left_tree);
                AST right_operand = ASTSon0(right_tree);

                return equivalent_dependent_expressions(left_operand, left_scope,
                        right_operand, right_scope, unif_set, decl_context);
            }
        case AST_SIZEOF :
            {
                // If they are expressions they are still workable
                AST left_sizeof = ASTSon0(left_tree);
                AST right_sizeof = ASTSon0(right_tree);

                return equivalent_dependent_expressions(left_sizeof, left_scope,
                        right_sizeof, right_scope, unif_set, decl_context);
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
                        left_scope, right_first_expression, right_scope,
                        unif_set, decl_context);
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
                    scope_entry_list_t* left_result_list = query_id_expression(left_scope,
                            left_reference, 
                            FULL_UNQUALIFIED_LOOKUP,
                            decl_context);
                    scope_entry_list_t* right_result_list = query_id_expression(right_scope,
                            right_reference, 
                            FULL_UNQUALIFIED_LOOKUP,
                            decl_context);

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

static char equivalent_expression_trees(AST left_tree, scope_t* left_scope, AST right_tree, 
        scope_t* right_scope, decl_context_t decl_context)
{
    literal_value_t left_value = evaluate_constant_expression(left_tree, left_scope, decl_context);
    literal_value_t right_value = evaluate_constant_expression(right_tree, right_scope, decl_context);

    return equal_literal_values(left_value, right_value, right_scope);
}
