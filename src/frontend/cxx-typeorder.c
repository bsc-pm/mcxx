#include "cxx-typeorder.h"
#include "cxx-typededuc.h"
#include "cxx-typeutils.h"
#include "cxx-typeunif.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"

char is_sound_type(type_t* t, decl_context_t decl_context)
{
    ERROR_CONDITION(t == NULL, "Invalid NULL here", 0);

    if (is_array_type(t))
    {
        type_t* element_type = array_type_get_element_type(t);
        if (is_void_type(element_type)
                || is_reference_type(element_type)
                || is_function_type(element_type))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: Deduced type is not sound because it is an array of void/references/functions\n");
            }
            return 0;
        }

        literal_value_t value = evaluate_constant_expression(array_type_get_array_size_expr(t),
                array_type_get_array_size_expr_context(t));

        if (literal_value_is_zero(value)
                || literal_value_is_negative(value))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: Deduced type is not sound because it is an array of negative length\n");
            }
            return 0;
        }

        return is_sound_type(element_type, decl_context);
    }
    else if (is_pointer_type(t))
    {
        // A pointer to a reference is not valid (int*& is valid but int&* not)
        if (is_reference_type(pointer_type_get_pointee_type(t)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: Deduced type is not sound because it is a pointer to a reference type\n");
            }
            return 0;
        }

        return is_sound_type(pointer_type_get_pointee_type(t), decl_context);
    }
    else if (is_reference_type(t))
    {
        // A reference to a reference is not valid (int&& is not valid)
        // (Note: in newer versions of C++ int&& is a rvalue-reference we do not support that)
        if( (is_reference_type(reference_type_get_referenced_type(t))
                    || is_void_type(reference_type_get_referenced_type(t))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: Deduced type is not sound because it is a reference to a reference type\n");
            }
            return 0;
        }

        return is_sound_type(reference_type_get_referenced_type(t), decl_context);
    }
    else if (is_function_type(t))
    {
        int num_parameters = function_type_get_num_parameters(t);

        if (function_type_get_has_ellipsis(t))
            num_parameters--;

        int i;
        for (i = 0; i < num_parameters; i++)
        {
            type_t* parameter_type = function_type_get_parameter_type_num(t, i);

            if (is_void_type(parameter_type)
                    || !is_sound_type(parameter_type, decl_context))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEORDER: Deduced type is not sound because it is a function type with a void type parameter\n");
                }
                return 0;
            }
        }

        return is_sound_type(function_type_get_return_type(t), decl_context);
    }
    else if (is_dependent_typename_type(t))
    {
        internal_error("Such a type should not reach here", 0);
    }
    else if (is_named_type(t))
    {
        scope_entry_t* entry = named_type_get_symbol(t);

        if (entry == NULL
                || entry->kind == SK_TEMPLATE_PARAMETER
                || entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            // This is trivially sound always
            return 1;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: Deduced type seems sound\n");
    }

    // Seems fine
    return 1;
}

char is_less_or_equal_specialized_template_class(type_t* c1, type_t* c2, decl_context_t decl_context,
        deduction_set_t** deduction_set, const char *filename, int line)
{
    ERROR_CONDITION(!is_named_class_type(c1)
            || !is_named_class_type(c2)
            || !is_template_specialized_type(get_actual_class_type(c1))
            || !is_template_specialized_type(get_actual_class_type(c2)),
            "Specialized classes are not", 0);

    parameter_info_t c1_parameters[1] =
    {
        { .is_ellipsis = 0, .type_info = c1, .original_type = NULL }
    };
    parameter_info_t c2_parameters[1] =
    {
        { .is_ellipsis = 0, .type_info = c2, .original_type = NULL }
    };

    type_t* faked_type_1 = get_new_function_type(get_void_type(), 
            c1_parameters, 1);

    set_as_template_specialized_type(faked_type_1,
            template_specialized_type_get_template_arguments(get_actual_class_type(c1)),
            // Can be NULL if c1 is a full specialization
            template_specialized_type_get_template_parameters(get_actual_class_type(c1)),
            template_specialized_type_get_related_template_type(get_actual_class_type(c1)));

    type_t* faked_type_2 = get_new_function_type(get_void_type(), 
            c2_parameters, 1);

    return is_less_or_equal_specialized_template_function(faked_type_1, faked_type_2, 
            decl_context, deduction_set, 
            /* explicit_template_arguments */ NULL,
            filename, line,
            /* is_conversion */ 0);
}

static char is_less_or_equal_specialized_template_conversion_function(
        type_t* f1, type_t* f2, 
        decl_context_t decl_context, deduction_set_t** deduction_set,
        const char *filename, int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: Computing whether one function is less or equal specialized than the other\n");
    }
    ERROR_CONDITION(!is_function_type(f1) || !is_function_type(f2), "functions types are not", 0);

    if (deduction_set != NULL)
    {
        *deduction_set = NULL;
    }

    int num_arguments = function_type_get_num_parameters(f2);
    if (function_type_get_has_ellipsis(f2))
        num_arguments--;

    int num_parameters = function_type_get_num_parameters(f1);
    if (function_type_get_has_ellipsis(f1))
        num_parameters--;

    if (num_arguments != num_parameters)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEORDER: It is not less or equal specialized template function because number arguments != number parameters\n");
        }
        return 0;
    }

    type_t* arguments[1];
    type_t* parameters[1];

    num_arguments = 1;
    arguments[0] = function_type_get_return_type(f2);

    num_parameters = 1;
    parameters[0] = function_type_get_return_type(f1);

    deduction_set_t* deduction_result = NULL;
    // Try to deduce types of template type F1 using F2

    template_parameter_list_t* template_parameters = 
        template_specialized_type_get_template_parameters(f1);

    if (!deduce_template_arguments_common(
                template_parameters,
                arguments, num_arguments,
                parameters, decl_context,
                &deduction_result, filename, line,
                /* explicit_template_arguments */ NULL))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEORDER: No deduction was possible\n");
        }
        return 0;
    }

    // Now check that the updated types match exactly
    template_argument_list_t* deduced_template_argument_list = 
        build_template_argument_list_from_deduction_set(deduction_result);

    {
        type_t* original_type = function_type_get_return_type(f1);
        
        type_t* updated_type = update_type(deduced_template_argument_list, 
                original_type, 
                decl_context,
                filename, line);

        // Check the soundness of the updated type
        if (update_type == NULL
                || !is_sound_type(updated_type, decl_context))
        {
            DEBUG_CODE()
            {
                if (update_type == NULL)
                {
                    fprintf(stderr, "TYPEORDER: The deduced type was not constructible\n");
                }
                else
                {
                    fprintf(stderr, "TYPEORDER: The deduced type '%s' is not sound\n", print_declarator(updated_type, decl_context));
                }

                fprintf(stderr, "TYPEORDER: It is not less or equal specialized template function because type deduction failed\n");
            }
            return 0;
        }

        type_t* argument_type = function_type_get_return_type(f2);

        if (!equivalent_types(updated_type, argument_type, decl_context))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: It is not less or equal specialized template function "
                        "because type of parameter (%s) and type of argument (%s) do not match\n",
                        print_declarator(updated_type, decl_context),
                        print_declarator(argument_type, decl_context));
            }
            return 0;
        }
    }

    if (deduction_set != NULL)
    {
        *deduction_set = deduction_result;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: It IS less or equal specialized\n");
    }

    return 1;
}

char is_less_or_equal_specialized_template_function(type_t* f1, type_t* f2,
        decl_context_t decl_context, deduction_set_t** deduction_set,
        template_argument_list_t* explicit_template_arguments,
        const char *filename, int line, char is_conversion)
{
    if (is_conversion)
    {
        return is_less_or_equal_specialized_template_conversion_function(f1, f2,
                decl_context, deduction_set, filename, line);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: Computing whether one function is less or equal specialized than the other\n");
    }
    ERROR_CONDITION(!is_function_type(f1) || !is_function_type(f2), "functions types are not", 0);

    if (deduction_set != NULL)
    {
        *deduction_set = NULL;
    }

    int num_arguments = function_type_get_num_parameters(f2);
    if (function_type_get_has_ellipsis(f2))
        num_arguments--;

    int num_parameters = function_type_get_num_parameters(f1);
    if (function_type_get_has_ellipsis(f1))
        num_parameters--;

    if (num_arguments != num_parameters)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEORDER: It is not less or equal specialized template function because number arguments != number parameters\n");
        }
        return 0;
    }

#define MAX_ARGUMENTS_FOR_DEDUCTION (256)
    type_t * arguments[MAX_ARGUMENTS_FOR_DEDUCTION];
    type_t * parameters[MAX_ARGUMENTS_FOR_DEDUCTION];

    int i;
    for (i = 0; i < num_arguments; i++)
    {
        ERROR_CONDITION(i >= MAX_ARGUMENTS_FOR_DEDUCTION, 
                "Too many types for deduction", 0);
        arguments[i] = function_type_get_parameter_type_num(f2, i);
        parameters[i] = function_type_get_parameter_type_num(f1, i);
    }

    deduction_set_t* deduction_result = NULL;
    // Try to deduce types of template type F1 using F2

    template_parameter_list_t* template_parameters = 
        template_specialized_type_get_template_parameters(f1);

    if (!deduce_template_arguments_common(
                template_parameters,
                arguments, num_arguments,
                parameters, decl_context,
                &deduction_result, filename, line,
                explicit_template_arguments))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEORDER: No deduction was possible\n");
        }
        return 0;
    }

    // Now check that the updated types match exactly
    template_argument_list_t* deduced_template_argument_list = 
        build_template_argument_list_from_deduction_set(deduction_result);

    for (i = 0; i < num_arguments; i++)
    {
        type_t* original_type = function_type_get_parameter_type_num(f1, i);
        
        type_t* updated_type = update_type(deduced_template_argument_list, 
                original_type, 
                decl_context,
                filename, line);

        // Check the soundness of the updated type
        if (update_type == NULL
                || !is_sound_type(updated_type, decl_context))
        {
            DEBUG_CODE()
            {
                if (update_type == NULL)
                {
                    fprintf(stderr, "TYPEORDER: The deduced type was not constructible\n");
                }
                else
                {
                    fprintf(stderr, "TYPEORDER: The deduced type '%s' is not sound\n", print_declarator(updated_type, decl_context));
                }

                fprintf(stderr, "TYPEORDER: It is not less or equal specialized template function because type deduction failed\n");
            }
            return 0;
        }

        type_t* argument_type = function_type_get_parameter_type_num(f2, i);

        if (!equivalent_types(updated_type, argument_type, decl_context))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: It is not less or equal specialized template function "
                        "because type of parameter (%s) and type of argument (%s) do not match\n",
                        print_declarator(updated_type, decl_context),
                        print_declarator(argument_type, decl_context));
            }
            return 0;
        }
    }

    if (deduction_set != NULL)
    {
        *deduction_set = deduction_result;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: It IS less or equal specialized\n");
    }

    return 1;
}
