#include <stdio.h>
#include <string.h>
#include "cxx-overload.h"
#include "cxx-utils.h"
#include "cxx-typecalc.h"
#include "cxx-typeutils.h"

static scope_entry_t* resolve_overload(scope_t* st, AST argument_list, scope_entry_list_t* candidate_functions, 
        type_t* object_type);

static int count_argument_list(AST argument_list)
{
    if (ASTType(argument_list) == AST_EMPTY_PARAMETER_DECLARATION_CLAUSE)
    {
        return 0;
    }
    else
    {
        int i = 0;
        AST iter;
        for_each_element(argument_list, iter)
        {
            AST par = ASTSon1(iter);

            if (ASTType(par) != AST_VARIADIC_ARG)
            {
                i++;
            }
        }
        return i;
    }
}


static char function_is_argument_viable(scope_entry_t* entry, 
        int num_args, scope_t* st)
{
    if (entry->kind != SK_FUNCTION)
    {
        internal_error("Expecting a symbol function here!", 0);
    }

    function_info_t* function_info = entry->type_information->function;
    if (num_args != function_info->num_parameters)
    {
        // Number of arguments is different to number of parameters
        if (num_args < function_info->num_parameters)
        {
            // Ensure parameters have default initializer
            parameter_info_t* param_for_last_arg = function_info->parameter_list[num_args];

            if (param_for_last_arg->default_argument == NULL)
            {
                // This is not a viable function because
                // it does not have a default argument here
                return 0;
            }
        }
        else // num_args > function_info->num_parameters
        {
            // The last one should be an ellipsized argument
            parameter_info_t* param_for_last_arg = function_info->parameter_list[function_info->num_parameters-1];

            if (!param_for_last_arg->is_ellipsis)
            {
                // This is not a viable function because
                // it does not have an ellipsized argument
                return 0;
            }
        }
    }

    // It looks callable with "num_arg" arguments
    return 1;
}

// That's a bit inefficient but argument lists are in general rather short
// (at most less than ten parameters)
static AST get_argument_i(AST argument_list, int i)
{
    AST iter;
    int j = 0;
    for_each_element(argument_list, iter)
    {
        if (i == j)
        {
            return ASTSon1(iter);
        }
        j++;
    }

    return NULL;
}

static void build_standard_conversion_sequence(type_t* argument_type, value_type_t argument_value_type, 
        type_t* parameter_type, standard_conversion_sequence_t* sequence, scope_t* st)
{
    // Advance over typedefs
    argument_type = advance_over_typedefs(argument_type);
    parameter_type = advance_over_typedefs(parameter_type);

    // If the types are the same except for top level cv_qualifier, then this
    // is a standard conversion sequence of identity conversion
    if (equivalent_types(argument_type, parameter_type, st, CVE_IGNORE_OUTERMOST))
    {
        sequence->scs_category |= SCS_IDENTITY;
        return;
    }
    
    // A plenty of cases for reference binding
    if (is_reference_type(parameter_type))
    {
        cv_qualifier_t cv_qualif_parameter = *(get_outermost_cv_qualifier(parameter_type->pointer->pointee));
        cv_qualifier_t cv_qualif_argument = *(get_outermost_cv_qualifier(argument_type));

        if (is_reference_type(argument_type))
        {
            cv_qualif_argument = *(get_outermost_cv_qualifier(argument_type->pointer->pointee));
        }

        if (is_reference_compatible(parameter_type->pointer->pointee, argument_type, st))
        {
            // If argument_type is an lvalue parameter_type should be a non-volatile const type
            if (argument_value_type == VT_LVALUE
                    || (argument_value_type == VT_RVALUE
                        && (cv_qualif_parameter == CV_CONST)))
            {
                char is_valid = 1;
                char is_cv_qualif_adjust = 0;
                char is_conversion = 0;

                if (cv_qualif_argument != cv_qualif_parameter
                        && ((cv_qualif_argument | cv_qualif_parameter) == cv_qualif_parameter))
                {
                    if ((sequence->scs_category & SCS_QUALIFICATION_ADJUSTMENT) 
                            != SCS_QUALIFICATION_ADJUSTMENT)
                    {
                        // The argument is less cv-qualified than the parameter
                        is_cv_qualif_adjust = 1;
                    }
                    else
                    {
                        is_valid = 0;
                    }
                }

                if (is_class_type(parameter_type->pointer->pointee)
                        && is_class_type(argument_type)
                        && is_base_class_of(parameter_type->pointer->pointee, argument_type))
                {
                    if ((sequence->scs_category & SCS_CONVERSION) != SCS_CONVERSION)
                    {
                        is_conversion = 1;
                    }
                    else
                    {
                        is_valid = 0;
                    }
                }

                if (is_valid)
                {
                    sequence->scs_category |= SCS_IDENTITY;
                    if (is_cv_qualif_adjust)
                    {
                        sequence->scs_category |= SCS_QUALIFICATION_ADJUSTMENT;
                    }
                    if (is_conversion)
                    {
                        sequence->scs_category |= SCS_CONVERSION;
                    }
                    return;
                }
            }
        }
    }

    // Lvalue conversions
    if ((sequence->scs_category & SCS_LVALUE_TRANSFORMATION) != SCS_LVALUE_TRANSFORMATION)
    {
        // Check array to pointer
        if ((argument_type->kind == TK_POINTER
                    && parameter_type->kind == TK_ARRAY)
                || (argument_type->kind == TK_ARRAY
                    && parameter_type->kind == TK_POINTER))
        {
            // Normalize both to pointer
            if (argument_type->kind == TK_ARRAY)
            {
                argument_type->kind = TK_POINTER;
                argument_type->pointer = calloc(1, sizeof(*(argument_type->pointer)));
                argument_type->pointer->pointee = argument_type->array->element_type;
            }
            else
            {
                parameter_type->kind = TK_POINTER;
                parameter_type->pointer = calloc(1, sizeof(*(parameter_type->pointer)));
                parameter_type->pointer->pointee = parameter_type->array->element_type;
            }

            sequence->scs_category |= SCS_LVALUE_TRANSFORMATION;
            // And construct the proper conversion
            build_standard_conversion_sequence(argument_type, argument_value_type, parameter_type, sequence, st);
            return;
        }

        // Check function to pointer
        if ((argument_type->kind == TK_FUNCTION
                    && parameter_type->kind == TK_POINTER
                    && parameter_type->pointer->pointee->kind == TK_FUNCTION)
                || (parameter_type->kind == TK_FUNCTION
                    && argument_type->kind == TK_POINTER
                    && argument_type->pointer->pointee->kind == TK_FUNCTION))
        {
            // Normalize both to the function
            if (argument_type->kind == TK_POINTER)
            {
                argument_type = argument_type->pointer->pointee;
            }
            else
            {
                parameter_type = parameter_type->pointer->pointee;
            }

            sequence->scs_category |= SCS_LVALUE_TRANSFORMATION;
            // Construct the proper conversion
            build_standard_conversion_sequence(argument_type, argument_value_type, parameter_type, sequence, st);
            return;
        }
    }

    if ((sequence->scs_category & SCS_PROMOTION) != SCS_PROMOTION
            && (sequence->scs_category & SCS_CONVERSION) != SCS_CONVERSION)
    {
        // Check for integral promotions/conversions
        if (is_fundamental_type(argument_type)
                && is_fundamental_type(parameter_type))
        {
            if (can_be_promoted_to_dest(argument_type, parameter_type))
            {
                sequence->scs_category |= SCS_PROMOTION;
                return;
            }
            else if (can_be_converted_to_dest(argument_type, parameter_type))
            {
                sequence->scs_category |= SCS_CONVERSION;
                return;
            }
        }
        else if (is_enumerated_type(argument_type) 
                && is_fundamental_type(parameter_type))
        {
                sequence->scs_category |= SCS_CONVERSION;
                return;
        }
        else if (is_class_type(argument_type)
                && is_class_type(parameter_type)
                && is_base_class_of(parameter_type, argument_type))
        {
            sequence->scs_category |= SCS_CONVERSION;
            return;
        }

        /*
         * T1 *a;
         * T2 *b;
         *
         *   a = b;
         *
         * is valid (provided T1 and T2 are different types) only if T1 is a base class of T2
         * or if T1 is a void type
         */
        if (is_pointer_type(argument_type)
                && is_pointer_type(parameter_type))
        {
            char to_void = 0;
            char derived_to_base = 0;
            char cv_adjust = 0;
            if (pointer_can_be_converted_to_dest(argument_type, parameter_type, st, &to_void, 
                        &derived_to_base, &cv_adjust))
            {
                char valid = 1;

                if (to_void || derived_to_base)
                {
                    if ((sequence->scs_category & SCS_CONVERSION) == SCS_CONVERSION)
                    {
                        valid = 0;
                    }
                }

                if (cv_adjust)
                {
                    if (valid || ((sequence->scs_category & SCS_QUALIFICATION_ADJUSTMENT) 
                                == SCS_QUALIFICATION_ADJUSTMENT))
                    {
                        valid = 0;
                    }
                }

                if (valid)
                {
                    if (to_void)
                    {
                        sequence->is_nonvoid_pointer_to_void = 1;
                    }
                    if (derived_to_base || to_void)
                    {
                        sequence->scs_category |= SCS_CONVERSION;
                    }

                    if (cv_adjust)
                    {
                        sequence->scs_category |= SCS_QUALIFICATION_ADJUSTMENT;
                    }
                    return;
                }
            }
        }

        /*
         * T A::* p1;
         * T B::* p2;
         *
         *   p2 = p1;
         *
         * is valid if A is a base class of B (just the opposite of the previous case)
         */
        if (is_pointer_to_member_type(argument_type)
                && is_pointer_to_member_type(parameter_type))
        {
            if (equivalent_types(argument_type->pointer->pointee, 
                        parameter_type->pointer->pointee,
                        st, CVE_CONSIDER))
            {
                if (argument_type->pointer->pointee_class != 
                        parameter_type->pointer->pointee_class)
                {
                    if (is_base_class_of(argument_type->pointer->pointee_class->type_information,
                                parameter_type->pointer->pointee_class->type_information))
                    {
                        sequence->scs_category |= SCS_CONVERSION;
                        return;
                    }
                }
            }
        }
    }

    // Boolean conversions
    if (is_bool_type(parameter_type))
    {
        if (is_integral_type(argument_type)
                || is_pointer_type(argument_type)
                || is_enumerated_type(argument_type))
        {
            sequence->scs_category |= SCS_CONVERSION;
            sequence->is_pointer_to_bool = 1;
            return;
        }
    }
    
    // No valid SCS found
    sequence->scs_category = SCS_UNKNOWN;
}

void build_user_defined_conversion_sequence(type_t* argument_type, value_type_t argument_value_type, type_t* parameter_type,
        one_implicit_conversion_sequence_t* sequence, scope_t* st)
{
    sequence->kind = ICS_USER_DEFINED;
    // First case,
    //
    // a) argument is of type class and has an operator that yields a value that can be
    //    SCS converted to parameter type
    //
    // Advance over typedefs
    argument_type = advance_over_typedefs(argument_type);
    parameter_type = advance_over_typedefs(parameter_type);

    if (is_class_type(argument_type))
    {
        // Check for conversion functions
#warning TODO - At the moment assume this conversion operator can be called now (if the \
        object where it is applied is const it is not feasible to call \
        non-const operator).

        type_t* class_type = get_class_type(argument_type);

        one_implicit_conversion_sequence_t attempt_scs;
        int i;
        for (i = 0; i < class_type->type->class_info->num_conversion_functions; i++)
        {
            // Clear this attempt SCS
            memset(&attempt_scs, 0, sizeof(attempt_scs));
            // It is possible to build a SCS from the original argument expression type to the class type ?
            build_standard_conversion_sequence(argument_type, argument_value_type, 
                    class_type, &(attempt_scs.standard_conversion[0]), st);

            if (attempt_scs.standard_conversion[0].scs_category != SCS_UNKNOWN)
            {
                conversion_function_t* conv_funct = class_type->type->class_info->conversion_function_list[i];

                // It is possible to build a SCS from the converted type to the parameter type ?
                build_standard_conversion_sequence(conv_funct->conversion_type, argument_value_type, 
                        parameter_type, &(attempt_scs.standard_conversion[1]), st);

                if (attempt_scs.standard_conversion[1].scs_category != SCS_UNKNOWN)
                {
                    // It is possible
                    if (sequence->user_defined.udc_category != UDC_VALID
                            && sequence->user_defined.udc_category != UDC_AMBIGUOUS)
                    {
                        sequence->user_defined.udc_category = UDC_VALID;
                        sequence->standard_conversion[0] = attempt_scs.standard_conversion[0];
                        sequence->standard_conversion[0].orig_type = argument_type;
                        sequence->standard_conversion[0].dest_type = class_type;

                        sequence->standard_conversion[1] = attempt_scs.standard_conversion[1];
                        sequence->standard_conversion[1].orig_type = conv_funct->conversion_type;
                        sequence->standard_conversion[1].dest_type = parameter_type;

                        sequence->user_defined.udc_conv_funct = conv_funct;
                    }
                    else
                    {
                        // There is more than one conversion possible (it is a bit
                        // strange this could happen with just conversion operators
                        // but let's consider this case anyway)
                        memset(sequence, 0, sizeof(sequence));
                        sequence->user_defined.udc_category = UDC_AMBIGUOUS;
                    }
                }
            }
        }
    }

    // Second case, it is possible to convert via a SCS the argument to the
    // parameter of a constructor of the parameter type (provided the parameter
    // is of class type) ?
    if (is_named_class_type(parameter_type))
    {
        type_t* class_type = get_class_type(parameter_type);
        one_implicit_conversion_sequence_t attempt_scs;

        int i;
        for (i = 0; i < class_type->type->class_info->num_constructors; i++)
        {
            // Clear this attempt SCS
            memset(&attempt_scs, 0, sizeof(attempt_scs));

            scope_entry_t* constructor = class_type->type->class_info->constructor_list[i];
            type_t* constructor_type = constructor->type_information;

            if (constructor_type->kind != TK_FUNCTION)
            {
                internal_error("The constructor has no functional type", 0);
            }

            if (constructor_type->function->num_parameters == 1
                    && !constructor_type->function->is_explicit)
            {
                // It is possible to build a SCS from the original argument expression type to the parameter
                // of the converting constructor ?
                build_standard_conversion_sequence(argument_type, argument_value_type, 
                        constructor_type->function->parameter_list[0]->type_info, 
                        &(attempt_scs.standard_conversion[0]), st);

                if (attempt_scs.standard_conversion[0].scs_category != SCS_UNKNOWN)
                {

                    // It is possible to build a SCS from the converted type to the parameter type ?
                    build_standard_conversion_sequence(class_type, VT_RVALUE,
                            parameter_type, &(attempt_scs.standard_conversion[1]), st);

                    if (attempt_scs.standard_conversion[1].scs_category != SCS_UNKNOWN)
                    {
                        // It is possible an SCS to this parameter
                        if (sequence->user_defined.udc_category != UDC_VALID
                                && sequence->user_defined.udc_category != UDC_AMBIGUOUS)
                        {
                            sequence->user_defined.udc_category = UDC_VALID;
                            sequence->user_defined.udc_constr_funct = constructor;
                            sequence->standard_conversion[0] = attempt_scs.standard_conversion[0];
                            sequence->standard_conversion[0].orig_type = argument_type;
                            sequence->standard_conversion[0].dest_type = constructor_type->function->parameter_list[0]->type_info;

                            sequence->standard_conversion[1] = attempt_scs.standard_conversion[1];
                            sequence->standard_conversion[1].orig_type = class_type;
                            sequence->standard_conversion[1].dest_type = parameter_type;
                        }
                        else
                        {
                            // There is more than one conversion possible
                            memset(sequence, 0, sizeof(sequence));
                            sequence->user_defined.udc_category = UDC_AMBIGUOUS;
                        }
                    }
                }
            }
        }
    }
}

static one_implicit_conversion_sequence_t* 
build_one_implicit_conversion_sequence(scope_entry_t* entry, int n_arg, AST argument_list, scope_t* st)
{
    // Now compute an ICS for this argument.
    // First get the type of the argument expression
    AST argument = get_argument_i(argument_list, n_arg);

#warning Reenable when full overload works
#if 0
    calculated_type_t* type_result_set = calculate_expression_type(argument, st);
#else
    calculated_type_t* type_result_set = NULL;
#endif

    if (type_result_set->num_types != 1)
    {
#warning Additional overload unsupported
        internal_error("Additional overload unsupported", 0);
    }

    one_implicit_conversion_sequence_t* result = calloc(1, sizeof(*result));

    type_t* argument_type = type_result_set->types[0];
    type_t* parameter_type = entry->type_information->function->parameter_list[n_arg]->type_info;

    // Copy the types since this function will modify them
    build_standard_conversion_sequence(copy_type(argument_type), type_result_set->value_type,
            copy_type(parameter_type), &(result->standard_conversion[0]), st);

    if (result->standard_conversion[0].scs_category != SCS_UNKNOWN)
    {
        result->kind = ICS_STANDARD;
        result->standard_conversion[0].orig_type = advance_over_typedefs(argument_type);
        result->standard_conversion[0].dest_type = advance_over_typedefs(parameter_type);
        return result;
    }

    // There might be a user defined conversion sequence provided that
    //
    //  a) the argument is of class type and has an operator that yields a type
    //     that can be SCS converted to the parameter type.
    //
    //     struct A
    //     {
    //        operator T&();
    //     };
    //
    //     void f(T k);
    //     void f(char* d);
    //
    //     void g()
    //     {
    //        A b;
    //        f(b); <-- Will call f(T);
    //     }
    //
    //  b) or otherwise, the parameter is of class type that and has am implicit constructor that 
    //     has an argument of a type that can be converted to with a SCS from the argument
    //
    //     struct A
    //     {
    //        A(const char* c);
    //     };
    //
    //     void f(A a);
    //     void f(double d);
    //
    //     void g()
    //     {
    //        char* c;
    //        f(c); <-- Will call f(A);
    //     }
    //
    //  It is possible to find an ambiguous user defined conversions in the
    //  case where is possible to convert a type to T1 and T2 and there are
    //  f(T1) and f(T2)
    //
    //     struct A
    //     {
    //        operator T1();
    //     };
    //
    //     struct T2
    //     {
    //        T2(A a);
    //     };
    //
    //     void f(T1 t1);
    //     void f(T2 t2);
    //
    //     void g()
    //     {
    //        A a;
    //        f(a); <-- f(T1) or f(T2) ?
    //     }

    // Clear the result
    memset(result, 0, sizeof(*result));

    build_user_defined_conversion_sequence(copy_type(argument_type), type_result_set->value_type,
            copy_type(parameter_type), result, st);

    if (result->user_defined.udc_category == UDC_UNKNOWN)
    {
        // There is no valid UDC nor SCS
        return NULL;
    }
    else
    {
        return result;
    }
}

static implicit_conversion_sequence_t* build_implicit_conversion_sequence(scope_entry_t* entry, int num_args, 
        AST argument_list, scope_t* st)
{
    int num_pars = entry->type_information->function->num_parameters;
    int i;

    implicit_conversion_sequence_t* result = calloc(1, sizeof(*result));

#warning Non const member functions are not viable if the object where invoked is not const too
    DEBUG_CODE()
    {
        fprintf(stderr, "Building ICS for function '");
    }

    print_declarator(entry->type_information, st);

    DEBUG_CODE()
    {
        fprintf(stderr, "'\n");
    }
    DEBUG_MESSAGE("Number of arguments = %d | Number of parameters = %d", num_args, num_pars);

    for (i = 0; i < num_args; i++)
    {
        one_implicit_conversion_sequence_t* one_ics;
        if (num_args > num_pars)
        {
            one_ics = calloc(1, sizeof(*one_ics));
            // This can only be by ellipsis nature
            one_ics->kind = ICS_ELLIPSIS;
            P_LIST_ADD(result->conversion, result->num_arg, one_ics);
        }
        else
        {
            one_implicit_conversion_sequence_t* one_ics = build_one_implicit_conversion_sequence(entry, i, argument_list, st);

            if (one_ics == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Function '");
                    print_declarator(entry->type_information, st);
                    fprintf(stderr, "' does not have an ICS\n");
                }
                return NULL;
            }

            P_LIST_ADD(result->conversion, result->num_arg, one_ics);
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Built an ICS for function '");
        print_declarator(entry->type_information, st);
        fprintf(stderr, "'\n");
    }

    return result;
}

static char is_better_standard_conversion_sequence(standard_conversion_sequence_t* s1,
        standard_conversion_sequence_t* s2, scope_t* st)
{
    // if (s1->kind != ICS_STANDARD
    //      || s2->kind != ICS_STANDARD)
    // {
    //  internal_error("This function only operates on standard sequences", 0);
    // }

    // If s1 is a proper subsequence of s2 then s1 is better
    if (s1->scs_category != s2->scs_category)
    {
        // The identity always is a subset of any non-identity standard
        // conversion
        if (s1->scs_category == SCS_IDENTITY)
        {
            // s1 only is the identity
            return 1;
        }

        // Every conversion in s2 is in s1. Thus s1 is better
        if ((s1->scs_category | s2->scs_category) == s1->scs_category)
        {
            return 1;
        }

        // Compare ranks
        //
        // If s2 is a conversion...
        if ((s2->scs_category & SCS_CONVERSION) == SCS_CONVERSION)
        {
            // ... but s1 it is not, then s1 is bettern scs than s2
            if ((s1->scs_category & SCS_CONVERSION) != SCS_CONVERSION)
            {
                return 1;
            }
        }
        // otherwise, if s2 is a promotion...
        else if ((s2->scs_category & SCS_PROMOTION) == SCS_PROMOTION)
        {
            // ... and s1 is an exact match, then s1 is better
            if (((s1->scs_category & SCS_IDENTITY) == SCS_IDENTITY)
                    || ((s1->scs_category & SCS_LVALUE_TRANSFORMATION) == SCS_LVALUE_TRANSFORMATION)
                    || ((s1->scs_category & SCS_QUALIFICATION_ADJUSTMENT) == SCS_QUALIFICATION_ADJUSTMENT))
            {
                return 1;
            }
        }

        // Now both s1 and s2 have the same rank, let's apply 13.3.3.2/4 to further distinguish them

        // A conversion that is not a conversion of a pointer (or pointer to
        // member) to too bool, is better than another conversion that is such
        // a conversion
        if (s2->is_pointer_to_bool && !s1->is_pointer_to_bool)
        {
            return 1;
        }

        // If class B is derived from class A, conversion from B* to A* is better than conversion
        // of B* to void* 
        //
        // s1: B* -> A*
        // s2: B* -> void*
        //
        // where A is base of B*
        if (is_pointer_to_class_type(s1->orig_type) // s1: B_1* -> ?
                && is_pointer_to_class_type(s2->orig_type) // s2: B_2* -> ?
                && equivalent_types(s1->orig_type, s2->orig_type, st, CVE_IGNORE_OUTERMOST) // B_1* = B_2* = B*
                && is_pointer_to_class_type(s1->dest_type) // s1: B* -> A*
                && is_base_class_of(s1->dest_type->pointer->pointee, 
                    s1->orig_type->pointer->pointee) // A* is base of B*
                && is_void_pointer_type(s2->dest_type)) // s2: B* -> void*
        {
            return 1;
        }

        // If class A is base of class B, conversion from A* to void is better than B* to void
        // s1: A* -> void*
        // s2: B* -> void*
        if (is_pointer_to_class_type(s1->orig_type) // s1: A* -> ?
                && is_void_pointer_type(s1->dest_type) // s1: A* -> void*
                && is_pointer_to_class_type(s2->orig_type) // s2: B* -> ?
                && is_void_pointer_type(s2->dest_type) // s2: B* -> void*
                && is_base_class_of(s1->orig_type->pointer->pointee,
                    s2->orig_type->pointer->pointee)) // A is base class of B
        {
            return 1;
        }

        // If class A is base of class B and class B is base of class C
        // Conversion from C* to B* is better than conversion from C* to A*
        if (is_pointer_to_class_type(s1->orig_type) // s1: C_1* -> ?
                && is_pointer_to_class_type(s1->dest_type) // s2: C_1* -> B*
                && is_pointer_to_class_type(s2->orig_type) // s2: C_2* -> ?
                && is_pointer_to_class_type(s2->dest_type) // s2: C_2* -> A*
                && equivalent_types(s1->orig_type, s2->orig_type, st,
                    CVE_IGNORE_OUTERMOST) // C_1* = C_2* = C*
                && is_base_class_of(s2->dest_type->pointer->pointee, 
                    s1->dest_type->pointer->pointee) // A is base class of B
                && is_base_class_of(s1->dest_type->pointer->pointee, // B is base class of C
                    s2->orig_type->pointer->pointee)) 
        {
            return 1;
        }

        // This is an additional one
        // If class A is base of class B and class B is base of class C
        // Conversion from C& to B& is better than conversion from C& to A&
        if (is_reference_to_class_type(s1->orig_type) // s1: C_1& -> ?
                && is_reference_to_class_type(s1->dest_type) // s2: C_1& -> B&
                && is_reference_to_class_type(s2->orig_type) // s2: C_2& -> ?
                && is_reference_to_class_type(s2->dest_type) // s2: C_2& -> A&
                && equivalent_types(s1->orig_type, s2->orig_type, st,
                    CVE_IGNORE_OUTERMOST) // C_1& = C_2& = C
                && is_base_class_of(s2->dest_type->pointer->pointee, 
                    s1->dest_type->pointer->pointee) // A is base class of B
                && is_base_class_of(s1->dest_type->pointer->pointee, // B is base class of C
                    s2->orig_type->pointer->pointee)) 
        {
            return 1;
        }

        // If class A is base of class B and class B is base of class C
        // Conversion from C to B& is better than conversion from C to A&
        if (is_class_type(s1->orig_type) // s1: C_1 -> ?
                && is_reference_to_class_type(s1->dest_type) // s2: C_1 -> B&
                && is_class_type(s2->orig_type) // s2: C_2 -> ?
                && is_reference_to_class_type(s2->dest_type) // s2: C_2 -> A&
                && equivalent_types(s1->orig_type, s2->orig_type, st,
                    CVE_IGNORE_OUTERMOST) // C_1 = C_2 = C
                && is_base_class_of(s2->dest_type->pointer->pointee, 
                    s1->dest_type->pointer->pointee) // A is base class of B
                && is_base_class_of(s1->dest_type->pointer->pointee, // B is base class of C
                    s2->orig_type)) 
        {
            return 1;
        }

        // If class A is base of class B and class B is base of class C
        // Conversion of A::* to B::* is better than conversion of A::* to C::*
        if (is_pointer_to_member_type(s1->orig_type) // s1: C_1::* -> ?
                && is_pointer_to_member_type(s1->dest_type) // s2: C_1::* -> B::*
                && is_pointer_to_member_type(s2->orig_type) // s2: C_2::* -> ?
                && is_pointer_to_member_type(s2->dest_type) // s2: C_2::* -> A::*
                && equivalent_types(s1->orig_type, s2->orig_type, st,
                    CVE_IGNORE_OUTERMOST) // C_1::* = C_2::* = C::*
                && is_base_class_of(s2->dest_type->pointer->pointee_class->type_information, 
                    s1->dest_type->pointer->pointee_class->type_information) // A is base class of B
                && is_base_class_of(s1->dest_type->pointer->pointee_class->type_information, // B is base class of C
                    s2->orig_type->pointer->pointee_class->type_information)) 
        {
            return 1;
        }

        // If class A is base of class B and class B is base of class C
        // Conversion of C to B is better than conversion of C to A
        if (is_class_type(s1->orig_type) // s1: C_1 -> ?
                && is_class_type(s2->orig_type) // s2: C_2 -> ?
                && equivalent_types(s1->orig_type, s2->orig_type, st, CVE_IGNORE_OUTERMOST) // C_1 = C_2 = C
                && is_class_type(s1->dest_type) // s1: C -> B
                && is_class_type(s2->dest_type) // s2: C -> A
                && is_base_class_of(s2->dest_type, s1->dest_type) // A is base of B
                && is_base_class_of(s1->dest_type, s1->orig_type)) // B is base of C
        {
            return 1;
        }

        // If class A is base of class B and class B is base of class C
        // Conversion of B* to A* is better than conversion of C* to A*
        if (is_pointer_to_class_type(s1->orig_type) // s1: B* -> ?
                && is_pointer_to_class_type(s1->dest_type) // s1: B* -> A_1*
                && is_pointer_to_class_type(s2->orig_type) // s2: C* -> ?
                && is_pointer_to_class_type(s2->dest_type) // s2: C* -> A_2*
                && equivalent_types(s1->dest_type, s2->dest_type, st, CVE_IGNORE_OUTERMOST) // A_1 = A_2 = A
                && is_base_class_of(s1->dest_type->pointer->pointee,
                    s1->orig_type->pointer->pointee) // A is base of B
                && is_base_class_of(s1->orig_type->pointer->pointee, // B is base of C
                    s2->orig_type->pointer->pointee))
        {
            return 1;
        }
        
        // If class A is base of class B and class B is base of class C
        // Conversion of B& to A& is better than conversion of C& to A&
        if (is_reference_to_class_type(s1->orig_type) // s1: B& -> ?
                && is_reference_to_class_type(s1->dest_type) // s1: B& -> A_1&
                && is_reference_to_class_type(s2->orig_type) // s2: C& -> ?
                && is_reference_to_class_type(s2->dest_type) // s2: C& -> A_2&
                && equivalent_types(s1->dest_type, s2->dest_type, st, CVE_IGNORE_OUTERMOST) // A_1 = A_2 = A
                && is_base_class_of(s1->dest_type->pointer->pointee,
                    s1->orig_type->pointer->pointee) // A is base of B
                && is_base_class_of(s1->orig_type->pointer->pointee, // B is base of C
                    s2->orig_type->pointer->pointee))
        {
            return 1;
        }
        
        // If class A is base of class B and class B is base of class C
        // Conversion of B to A& is better than conversion of C to A&
        if (is_reference_to_class_type(s1->orig_type) // s1: B& -> ?
                && is_reference_to_class_type(s1->dest_type) // s1: B& -> A_1&
                && is_class_type(s2->orig_type) // s2: C -> ?
                && is_reference_to_class_type(s2->dest_type) // s2: C -> A_2&
                && equivalent_types(s1->dest_type, s2->dest_type, st, CVE_IGNORE_OUTERMOST) // A_1 = A_2 = A
                && is_base_class_of(s1->dest_type->pointer->pointee,
                    s1->orig_type->pointer->pointee) // A is base of B
                && is_base_class_of(s1->orig_type->pointer->pointee, // B is base of C
                    s2->orig_type))
        {
            return 1;
        }
        
        // If class A is base of class B and class B is base of class C
        // Conversion of B::* to C::* is better than conversion of A::* to C::*
        if (is_pointer_to_member_type(s1->orig_type) // s1: B::* -> ?
                && is_pointer_to_member_type(s1->dest_type) // s1: B::* -> C_1::*
                && is_pointer_to_member_type(s2->orig_type) // s2: A::* -> ?
                && is_pointer_to_member_type(s2->dest_type) // s2: A::* -> C_2::*
                && equivalent_types(s1->dest_type, s2->dest_type, st,
                    CVE_IGNORE_OUTERMOST) // C_1 = C_2 = C
                && is_base_class_of(s2->orig_type->pointer->pointee_class->type_information,
                    s1->orig_type->pointer->pointee_class->type_information) // A is base of B
                && is_base_class_of(s1->orig_type->pointer->pointee_class->type_information,
                    s1->dest_type->pointer->pointee_class->type_information)) // B is base of C
        {
            return 1;
        }
        
        // If class A is base of class B and class B is base of class C
        // Conversion of B to A is better than conversion of C to A
        if (is_reference_to_class_type(s1->orig_type) // s1: B -> ?
                && is_class_type(s1->dest_type) // s1: B -> A_1
                && is_class_type(s2->orig_type) // s2: C -> ?
                && is_class_type(s2->dest_type) // s2: C -> A_2
                && equivalent_types(s1->dest_type, s2->dest_type, st, CVE_IGNORE_OUTERMOST) // A_1 = A_2 = A
                && is_base_class_of(s1->dest_type, s1->orig_type) // A is base of B
                && is_base_class_of(s1->orig_type, s2->orig_type))// B is base of C
        {
            return 1;
        }
    }

    return 0;
}

static char is_better_conversion_sequence(one_implicit_conversion_sequence_t* s1,
        one_implicit_conversion_sequence_t* s2, scope_t* st)
{
    // A standard conversions equence is a better conversion sequence than an
    // ellipsis conversion or user defined conversion
    if (s1->kind == ICS_STANDARD
            && (s2->kind == ICS_USER_DEFINED
                || s2->kind == ICS_ELLIPSIS))
    {
        return 1;
    }

    // A user-defined conversion sequence is a better conversion sequence than
    // an ellipsis conversion sequence
    if (s1->kind == ICS_USER_DEFINED
            && s2->kind == ICS_ELLIPSIS)
    {
        return 1;
    }

    if (s1->kind == s2->kind)
    {
        if (s1->kind == ICS_STANDARD)
        {
            return is_better_standard_conversion_sequence(&(s1->standard_conversion[0]), 
                    &(s2->standard_conversion[0]), st);
        }
        else if (s1->kind == ICS_USER_DEFINED)
        {
            
            if (s1->user_defined.udc_category != UDC_AMBIGUOUS
                    && s2->user_defined.udc_category != UDC_AMBIGUOUS)
            {
                // A user defined conversion is better than another user defined
                // conversion if they contain the same user-defined conversion
                // function or constructor and if the second standard conversion
                // sequence of U1 is better than the second standard conversion
                // sequence.
                if ((s1->user_defined.udc_conv_funct != NULL && s2->user_defined.udc_conv_funct != NULL
                            && s1->user_defined.udc_conv_funct == s2->user_defined.udc_conv_funct)
                        || (s1->user_defined.udc_constr_funct != NULL && s2->user_defined.udc_constr_funct != NULL
                            && s1->user_defined.udc_constr_funct == s2->user_defined.udc_constr_funct))
                {
                    return is_better_standard_conversion_sequence(&(s1->standard_conversion[1]), 
                            &(s2->standard_conversion[1]), st);
                }
            }
        }
    }

    return 0;
}

static viable_function_list_t* calculate_viable_functions(scope_entry_list_t* candidate_functions, 
        int num_args, AST argument_list, scope_t* st, type_t* object_type)
{
    scope_entry_list_t* iter = candidate_functions;

    viable_function_list_t* result = NULL;

    while (iter != NULL)
    {
        if (function_is_argument_viable(iter->entry, num_args, st))
        {
            // Right. Now we have to build an ICS (implicit conversion sequence)
            // for this function
            implicit_conversion_sequence_t* ics = 
                build_implicit_conversion_sequence(iter->entry, num_args, argument_list, st);
            
            if (ics != NULL)
            {
                viable_function_list_t* current_funct = calloc(1, sizeof(*current_funct));
                current_funct->entry = iter->entry;
                current_funct->ics_num_args = num_args;
                current_funct->ics = ics;
                current_funct->next = result;
                result = current_funct;
            }
        }
        iter = iter->next;
    }

    return result;
}


static char is_better_viable_function(viable_function_list_t* f1,
        viable_function_list_t* f2, scope_t* st)
{
    /*
     * A viable function f1 is defined to be a better function than another viable
     * function f2 if for all arguments "i", ics[i](f1) is not a worse conversion
     * sequence than ics[i](f2) and
     *
     * a) for some argument j, ics[j](f1) is better than ics[j](f2), or if not
     * that
     *
     * b) f1 is a non template function and f2 is a function template
     * specialization, or if not that
     *
     * c) f1 and f2 are function template specializations, and the function
     * template for f1 is more specialized than the template for f2, according
     * to the partial ordering rules described in 14.5.5.2, or if not that
     *
     * d) the context is an initialization by user-defined conversion and the
     * standard conversion sequence from the return type of F1 to the
     * destination type (the entity being initialized) is a better conversion
     * sequence than the standard conversion sequence from the return type of
     * f2 to the destination type. (rofi: I think this one is subsumed by the
     * order of a user defined conversion)
     *
     */

    char some_is_worse = 0;
    char some_is_better = 0;
    int i;
    for (i = 0; i < f1->ics_num_args; i++)
    {
        if (is_better_conversion_sequence(f1->ics->conversion[i], f2->ics->conversion[i], st))
        {
            some_is_better = 1;
        }
        else
        {
            some_is_worse = 1;
        }
    }

    if (some_is_worse)
    {
        // Should be no worse for any of the arguments
        return 0;
    }

    // This is a)
    if (some_is_better)
    {
        return 1;
    }

    // This is b)
    if (f1->entry->kind == SK_FUNCTION
            && f2->entry->kind == SK_TEMPLATE_FUNCTION)
    {
        return 1;
    }

    // This is c)
#warning Missing partial template specialization ordering [13.3.3/1 and 14.5.5.2]
#if 0
    if (f1->entry->kind == SK_TEMPLATE_FUNCTION
            && f2->entry->kind == SK_TEMPLATE_FUNCTION
            && is_function_template_more_specialized(f1, f2))
    {
        return 1;
    }
#endif 

    // This is d)
    /*
        struct A {
          A();
          operator int();
          operator double();
        };
        
        A a
        
        int i = a; // A::operator int() will be used to convert "a" to int since A::operator double()
                   // would imply a conversion from double to int and A::operator int() does not
                   // requires this conversion
        float x = a; // Ambiguity, both A::operator int() and A::operator double() can be
                     // applied to yield a value that will be further converted to float.
     */
#warning Missing ICS best viable definition in the context of an initialization [13.3.3/1]

    // Otherwise, this is not better
    return 0;
}

static scope_entry_t* choose_best_viable_function(viable_function_list_t* viable_functions,
        scope_t* st)
{
    /*
     * The algorithm is as follows.
     * a) Find a candidate to be the best one. This is a classical maximum search.
     * b) Recheck this candidate is really the best with all functions.
     *
     * According to the standard a) will yield a F function that might be the best one
     * but we have to ensure that there is no G that will be better than G, otherwise
     * no best viable function exists
     */
    if (viable_functions == NULL)
    {
        DEBUG_MESSAGE("No viable functions available!", 0);
        return NULL;
    }

    viable_function_list_t* result = viable_functions;
    viable_function_list_t* iter = viable_functions->next;

    while (iter != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "iter > result? %d\n", is_better_viable_function(iter, result, st));
            fprintf(stderr, "iter < result? %d\n", is_better_viable_function(result, iter, st));
        }

        if (is_better_viable_function(iter, result, st))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Choosing function '");
                print_declarator(iter->entry->type_information, result->entry->scope);
                fprintf(stderr, "' because is better than '");
                print_declarator(result->entry->type_information, result->entry->scope);
                fprintf(stderr, "'\n");
            }

            result = iter;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Function '");
                print_declarator(iter->entry->type_information, result->entry->scope);
                fprintf(stderr, "' is still better than '");
                print_declarator(result->entry->type_information, result->entry->scope);
                fprintf(stderr, "'\n");
            }
        }

        iter = iter->next;
    }

    // Now check is really the best
    iter = viable_functions;
    char is_still_the_best = 1;
    while ((iter != NULL) && is_still_the_best)
    {
        if (iter != result)
        {
            is_still_the_best = is_better_viable_function(result, iter, st);
            if (!is_still_the_best)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Function '");
                    print_declarator(result->entry->type_information, result->entry->scope);
                    fprintf(stderr, "' is not better than '");
                    print_declarator(iter->entry->type_information, result->entry->scope);
                    fprintf(stderr, "'\n");
                }
            }
        }

        iter = iter->next;
    }

    if (!is_still_the_best)
    {
        fprintf(stderr, "There was no best viable function\n");
        return NULL;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Determined the best viable function\n");
            fprintf(stderr, "Best viable function is '");
            print_declarator(result->entry->type_information, result->entry->scope);
            fprintf(stderr, "'\n");
        }
        return result->entry;
    }
}

// This function returns the unique overload or NULL if it is unavailable or it
// is ambiguous
scope_entry_t* resolve_overload(scope_t* st, AST argument_list, 
        scope_entry_list_t* candidate_functions, type_t* object_type)
{
    // Early out for common cases
    if (candidate_functions == NULL
            || candidate_functions->next == NULL)
    {
        return candidate_functions->entry;
    }

    int num_args = count_argument_list(argument_list);
    DEBUG_MESSAGE("Counted %d arguments", num_args);

    viable_function_list_t* viable_functions;

    DEBUG_MESSAGE("Calculating viable functions", 0);
    viable_functions = calculate_viable_functions(candidate_functions, num_args, argument_list, st, object_type);

    int viable_functions_count = 0;
    viable_function_list_t* iter = viable_functions;
    while (iter != NULL)
    {
        viable_functions_count++;
        iter = iter->next;
    }
    DEBUG_MESSAGE("Determined %d viable functions", viable_functions_count);

    DEBUG_MESSAGE("Choosing best viable function", 0);
    scope_entry_t* best_viable_function = choose_best_viable_function(viable_functions, st);

    if (best_viable_function == NULL)
    {
        internal_error("No viable function found !\n", 0);
    }

    return best_viable_function;
}

