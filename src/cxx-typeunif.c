#include <stdio.h>
#include <string.h>
#include "cxx-typeunif.h"
#include "cxx-typeutils.h"
#include "cxx-cexpr.h"
#include "cxx-scope.h"
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-solvetemplate.h"

static type_t* get_template_parameter_unification(unification_set_t* unif_set, int num, int nesting);

// Will try to find a substitution to unificate t1 to t2
//
// e.g.   Q*    can    be unificated to   T**    with   [Q <- T*]
//        T**   cannot be unificated to   Q*
//
char unificate_two_types(type_t* t1, type_t* t2, scope_t* st, 
        unification_set_t** unif_set, decl_context_t decl_context)
{

    t1 = advance_over_typedefs(t1);
    t2 = advance_over_typedefs(t2);

    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to unificate '%s' <- '%s'\n",
                print_declarator(t1, st),
                print_declarator(t2, st));
    }

    // Normalize types
    // If the user defined type points to a template parameter, we will use the
    // template parameter
    type_t* original_t1 = t1;
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
        type_t* previous_unif = get_template_parameter_unification(*unif_set, t1->type->template_parameter_num,
                t1->type->template_parameter_nesting);
        if (previous_unif == NULL)
        {
            // Check that t1 is less cv-qualified than t2

            if ((original_t1->cv_qualifier | t2->cv_qualifier) == (t2->cv_qualifier))
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
                unif_item->value = t2;

                P_LIST_ADD((*unif_set)->unif_list, (*unif_set)->num_elems, unif_item);
            }
            else
            {
                // We cannot unify 'const X' with 'Y' (even if we can unify 'X' with 'const Y')
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

        type_t* previous_unif = get_template_parameter_unification(*unif_set, t1->type->template_parameter_num,
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
                literal_value_t v1 = evaluate_constant_expression(t1->array->array_expr, st, decl_context);
                literal_value_t v2 = evaluate_constant_expression(t2->array->array_expr, st, decl_context);

                if (equal_literal_values(v1, v2, st))
                {
                    return unificate_two_types(t1->array->element_type, t2->array->element_type, st, unif_set,
                            decl_context);
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

static type_t* get_template_parameter_unification(unification_set_t* unif_set, int num, int nesting)
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
