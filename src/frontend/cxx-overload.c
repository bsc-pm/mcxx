#include "cxx-overload.h"
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-typeorder.h"
#include "cxx-typededuc.h"
#include "cxx-instantiation.h"
#include "cxx-utils.h"
#include "cxx-scope.h"
#include "cxx-exprtype.h"

#include <string.h>

typedef
enum implicit_conversion_sequence_kind_tag
{
    ICSK_INVALID = 0,
    ICSK_STANDARD,
    ICSK_USER_DEFINED,
    ICSK_ELLIPSIS,
} implicit_conversion_sequence_kind_t;

typedef
struct implicit_conversion_sequence_tag
{
    implicit_conversion_sequence_kind_t kind;

    // Meaningful only in ICSK_STANDARD
    standard_conversion_t first_sc;

    // These below are only meaningful when ICSK_USER_DEFINED
    scope_entry_t* conversor;
    standard_conversion_t second_sc;
    char is_ambiguous_ics;
} implicit_conversion_sequence_t;

static
implicit_conversion_sequence_t invalid_ics = { .kind = ICSK_INVALID, .is_ambiguous_ics = 0 };

typedef
struct overload_entry_list_tag
{
    scope_entry_t* entry;
    struct overload_entry_list_tag* next;

    char requires_ambiguous_ics;
} overload_entry_list_t;

static char better_ics(implicit_conversion_sequence_t ics1,
        implicit_conversion_sequence_t ics2,
        decl_context_t decl_context);
static
char standard_conversion_is_better(standard_conversion_t scs1, 
        standard_conversion_t scs2,
        decl_context_t decl_context);

static
char is_better_function_flags(scope_entry_t* f,
        scope_entry_t* g,
        decl_context_t decl_context,
        type_t **argument_types,
        int num_types,
        char no_standard_conversions,
        const char *filename,
        int line);

static
char standard_conversion_is_better(standard_conversion_t scs1, 
        standard_conversion_t scs2,
        decl_context_t decl_context);

static type_t* result_type_after_conversion(scope_entry_t* conversor)
{
    type_t* result = NULL;

    ERROR_CONDITION(conversor->kind != SK_FUNCTION,
            "This must be a function", 0);

    if (conversor->entity_specs.is_constructor)
    {
        ERROR_CONDITION(!conversor->entity_specs.is_conversor_constructor,
                "This is not a conversor constructor", 0);

        result = conversor->entity_specs.class_type;
    }
    else if (conversor->entity_specs.is_conversion)
    {
        result = function_type_get_return_type(conversor->type_information);
    }
    else
    {
        internal_error("Invalid conversor function %s at '%s:%d'\n", conversor->symbol_name,
                conversor->file, conversor->line);
    }

    return result;
}

static char is_better_initialization_ics(
        implicit_conversion_sequence_t ics_1,
        implicit_conversion_sequence_t ics_2,
        type_t* dest,
        decl_context_t decl_context,
        const char *filename,
        int line)
{
    // This checks all what is_better_function does but adds an additional
    // check for the initialization issue
    //
    // struct A
    // {
    //    operator float();
    //    operator int();
    // };
    //
    // A a;
    // float f = a;
    //
    // Both 'A::operator float' and 'A::operator int' are equally good because,
    // actually, no ICS was built for them since they are handled as 'function
    // () returning int' and 'function () returning float' (no argument type to
    // parameter type ICS was built) respectively, so the only thing that leads
    // us to choose 'A::operator float' is the fact that the SCS between
    // 'float->float' is better than 'int->float'.

    ERROR_CONDITION((ics_1.kind != ICSK_USER_DEFINED
                || ics_2.kind != ICSK_USER_DEFINED),
            "Both ICS must be user defined", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Checking if conversion %s at %s:%d is better than %s at %s:%d\n",
                ics_1.conversor->symbol_name,
                ics_1.conversor->file,
                ics_1.conversor->line,
                ics_2.conversor->symbol_name,
                ics_2.conversor->file,
                ics_2.conversor->line);
    }

    if (is_better_function_flags(ics_1.conversor,
                ics_2.conversor,
                decl_context, 
                /* argument_types */ NULL, 
                /* num_argument_types */ 0,
                /* no_user_defined_conversions */ 1,
                filename, line))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Conversion %s at %s:%d is better than %s at %s:%d because it is a better function\n",
                    ics_1.conversor->symbol_name,
                    ics_1.conversor->file,
                    ics_1.conversor->line,
                    ics_2.conversor->symbol_name,
                    ics_2.conversor->file,
                    ics_2.conversor->line);
        }
        return 1;
    }

    // Maybe they are equally good, so we have to check the conversion destination type
    if (!is_better_function_flags(ics_2.conversor,
                ics_1.conversor,
                decl_context, 
                /* argument_types */ NULL, 
                /* num_argument_types */ 0,
                /* no_user_defined_conversions */ 1,
                filename, line))
    {
        // Get the converted type after the conversion
        type_t* converted_type_1 = result_type_after_conversion(ics_1.conversor);
        type_t* converted_type_2 = result_type_after_conversion(ics_2.conversor);

        standard_conversion_t scs_1;
        if (!standard_conversion_between_types(&scs_1, converted_type_1, dest, decl_context))
        {
            internal_error("A SCS should exist!", 0);
        }
        standard_conversion_t scs_2;
        if (!standard_conversion_between_types(&scs_2, converted_type_2, dest, decl_context))
        {
            internal_error("A SCS should exist!", 0);
        }

        if (standard_conversion_is_better(scs_1, scs_2, decl_context))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Conversion %s at %s:%d is better than %s at %s:%d "
                        "because converted type is better\n",
                        ics_1.conversor->symbol_name,
                        ics_1.conversor->file,
                        ics_1.conversor->line,
                        ics_2.conversor->symbol_name,
                        ics_2.conversor->file,
                        ics_2.conversor->line);
            }
            return 1;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Conversion %s at %s:%d is NOT better than %s at %s:%d\n",
                ics_1.conversor->symbol_name,
                ics_1.conversor->file,
                ics_1.conversor->line,
                ics_2.conversor->symbol_name,
                ics_2.conversor->file,
                ics_2.conversor->line);
    }

    return 0;
}

static void compute_ics_flags(type_t* orig, type_t* dest, decl_context_t decl_context, 
        implicit_conversion_sequence_t *result, char no_user_defined_conversions,
        const char* filename, int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "ICS: Computing ICS from '%s' -> '%s'\n", 
                print_declarator(orig, decl_context),
                print_declarator(dest, decl_context));
    }

    *result = invalid_ics;

    // This might happen in some cases
    if (dest == NULL)
        return;

    if (is_ellipsis_type(dest))
    {
        result->kind = ICSK_ELLIPSIS;
        // No need to check anything else
        return;
    }

    cv_qualifier_t cv_orig = CV_NONE;
    orig = advance_over_typedefs_with_cv_qualif(orig, &cv_orig);
    cv_qualifier_t cv_dest = CV_NONE;
    dest = advance_over_typedefs_with_cv_qualif(dest, &cv_dest);

    // If this an unresolved address of overload function try to solve it here
    // if it can't be solved, there is no ICS, it is not an error
    if (is_unresolved_overloaded_type(orig))
    {

        scope_entry_t* solved_function = address_of_overloaded_function(
                unresolved_overloaded_type_get_overload_set(orig),
                unresolved_overloaded_type_get_explicit_template_arguments(orig),
                dest,
                decl_context,
                filename,
                line);

        if (solved_function != NULL)
        {
            if (!solved_function->entity_specs.is_member
                    || solved_function->entity_specs.is_static)
            {
                orig = get_reference_type(solved_function->type_information);
            }
            else
            {
                orig = get_reference_type(get_pointer_to_member_type(
                            solved_function->type_information,
                            named_type_get_symbol(solved_function->entity_specs.class_type)));
            }
            // And proceed evaluating this ICS
        }
        else
        {
            // Invalid ICS
            return;
        }
    }

    // Given a class 'A' base of a class 'B'
    //
    // To compute that 'B&' can be converted to 'A&' requires testing if 'A' is a base of 'B'
    // so 'B' must be instantiated.
    if (is_named_class_type(no_ref(orig))
            && class_type_is_incomplete_independent(get_actual_class_type(no_ref(orig))))
    {
        scope_entry_t* symbol = named_type_get_symbol(no_ref(orig));
        instantiate_template(symbol, decl_context, filename, line);
    }
    // Given a class 'A' base of a class 'B'
    //
    // To compute that 'T A::*' can be converted to 'T B::*' requires testing if 'A' is a base of 'B'
    // so 'B' must be instantiated.
    if (is_pointer_to_member_type(no_ref(dest)))
    {
        scope_entry_t* class_symbol = pointer_to_member_type_get_class(no_ref(dest));
        if (class_type_is_incomplete_independent(class_symbol->type_information))
        {
            instantiate_template(class_symbol, decl_context, filename, line);
        }
    }

    standard_conversion_t standard_conv;
    if (standard_conversion_between_types(&standard_conv, orig, dest, decl_context))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: There is a standard conversion from '%s' -> '%s'\n", 
                    print_declarator(standard_conv.orig, decl_context),
                    print_declarator(standard_conv.dest, decl_context));
        }

        result->kind = ICSK_STANDARD;
        result->first_sc = standard_conv;

        // No need to check anything else
        return;
    }

    // Nothing else to do
    if (no_user_defined_conversions)
        return;

    // So no standard conversion is possible let's try with a user defined
    // conversion
#define MAX_USER_DEFINED_CONVERSIONS (256)
    implicit_conversion_sequence_t user_defined_conversions[MAX_USER_DEFINED_CONVERSIONS];
    int num_user_defined_conversions = 0;
    memset(user_defined_conversions, 0, sizeof(user_defined_conversions));

    // Compute user defined conversions by means of conversion functions
    if (is_class_type(no_ref(orig)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: Checking user-defined conversions by means of conversion functions\n");
        }

        // Get the real class type (it will have been instantiated before if needed)
        type_t* class_type = no_ref(orig);

        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: Looking for user defined conversions from '%s' to '%s'\n",
                    print_declarator(orig, decl_context),
                    print_declarator(dest, decl_context));
        }

        // Maybe there is a conversion function from class_type to something standard
        // convertible to dest
        scope_entry_list_t* conversion_list = class_type_get_all_conversions(
                get_actual_class_type(class_type), decl_context);
        scope_entry_list_t* it = conversion_list;

        while (it != NULL)
        {
            scope_entry_t* conv_funct = it->entry;

            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Considering user defined conversion '%s' declared at '%s:%d'\n",
                        conv_funct->symbol_name,
                        conv_funct->file,
                        conv_funct->line);
            }

            if (is_template_specialized_type(conv_funct->type_information))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: Symbol '%s' at '%s:%d' is a template conversion function, "
                            "deducing its arguments\n",
                            conv_funct->symbol_name,
                            conv_funct->file,
                            conv_funct->line);
                }
                // This is a template so we have to get the proper specialization

                // Get the primary specialization
                type_t* specialization_function = get_user_defined_type(conv_funct);
                // Get its template parameters
                template_parameter_list_t* template_parameters 
                    = template_specialized_type_get_template_parameters(conv_funct->type_information);

                deduction_set_t* deduction_result = NULL;
                // Now deduce the arguments
                if (!deduce_arguments_of_conversion(dest, specialization_function,
                            template_parameters,
                            decl_context, &deduction_result, filename, line))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "ICS: Deduced arguments for template conversion function failed, skipping\n");
                    }
                    continue;
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: Deduced arguments for template conversion function succeeded\n");
                }

                // If the deduction succeeded just get a specialization and use it for the whole
                // conversion
                template_argument_list_t* template_arguments = 
                    build_template_argument_list_from_deduction_set(deduction_result);

                type_t* template_type = template_specialized_type_get_related_template_type(conv_funct->type_information);

                type_t* named_specialization_type = template_type_get_specialized_type(template_type,
                        template_arguments,
                        /* no template parameters */ NULL,
                        decl_context,
                        line, filename);

                // Now update the symbol
                conv_funct = named_type_get_symbol(named_specialization_type);

                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: Specialized conversion function '%s' is '%s'\n",
                            conv_funct->symbol_name,
                            print_declarator(conv_funct->type_information, decl_context));
                }
            }

            type_t* converted_type = function_type_get_return_type(conv_funct->type_information);

            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Checking conversion function of '%s' to '%s'\n",
                        print_declarator(orig, decl_context),
                        print_declarator(converted_type, decl_context));
            }

            // The implicit parameter of this operator function is a reference
            // to the class type, this will filter not eligible conversion functions
            // (e.g. given a 'const T' we cannot call a non-const method)
            type_t* implicit_parameter = conv_funct->entity_specs.class_type;
            if (is_const_qualified_type(conv_funct->type_information))
            {
                implicit_parameter = get_cv_qualified_type(implicit_parameter, CV_CONST);
            }
            implicit_parameter = get_reference_type(implicit_parameter);

            standard_conversion_t first_sc;
            standard_conversion_t second_sc;
            if (standard_conversion_between_types(&first_sc, orig, 
                        implicit_parameter, decl_context)
                    && standard_conversion_between_types(&second_sc, converted_type, 
                        dest, decl_context))
            {
                implicit_conversion_sequence_t *current = &(user_defined_conversions[num_user_defined_conversions]);
                num_user_defined_conversions++;

                current->kind = ICSK_USER_DEFINED;
                current->first_sc = first_sc;
                current->conversor = conv_funct;
                current->second_sc = second_sc;

                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: Details of this potential user defined conversion\n"
                            "ICS:     SCS1: %s -> %s\n"
                            "ICS:     Conversion function: %s (%s:%d)\n"
                            "ICS:     SCS2: %s -> %s\n",
                            print_declarator(current->first_sc.orig, decl_context),
                            print_declarator(current->first_sc.dest, decl_context),
                            current->conversor->symbol_name,
                            current->conversor->file,
                            current->conversor->line,
                            print_declarator(current->second_sc.orig, decl_context),
                            print_declarator(current->second_sc.dest, decl_context));
                }
            }

            it = it->next;
        }
    }

    // Compute user defined conversions by means of constructors
    if(is_class_type(no_ref(dest)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: Checking user-defined conversions by means of conversor constructors\n");
        }
        // Get the real class type
        type_t* class_type = no_ref(dest);

        // Instantiate the destination if needed
        if (is_named_class_type(class_type)
                && class_type_is_incomplete_independent(get_actual_class_type(class_type)))
        {
            scope_entry_t* symbol = named_type_get_symbol(class_type);
            instantiate_template(symbol, decl_context, filename, line);
        }

        int i;
        int num_constructors = class_type_get_num_constructors(get_actual_class_type(class_type));
        for (i = 0; i < num_constructors; i++)
        {
            scope_entry_t* constructor = class_type_get_constructors_num(
                    get_actual_class_type(class_type), i);

            // This is not an eligible conversor constructor
            if (!constructor->entity_specs.is_conversor_constructor)
                continue;

            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Considering conversor constructor '%s' declared at '%s:%d'\n",
                        constructor->symbol_name,
                        constructor->file,
                        constructor->line);
            }

            if (is_template_specialized_type(constructor->type_information))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: Symbol '%s' at '%s:%d' is a template conversor constructor, "
                            "deducing its arguments\n",
                            constructor->symbol_name,
                            constructor->file,
                            constructor->line);
                }
                // This is a template so we have to get the proper specialization

                // Get the primary specialization
                type_t* specialization_function = get_user_defined_type(constructor);
                // Get its template parameters
                template_parameter_list_t* template_parameters 
                    = template_specialized_type_get_template_parameters(constructor->type_information);

                deduction_set_t* deduction_result = NULL;
                // Now deduce the arguments
                type_t* argument_types[1] = { orig };
                if (!deduce_arguments_from_call_to_specific_template_function(argument_types, 
                            /* num_arguments = */ 1, 
                            specialization_function,
                            template_parameters,
                            decl_context, 
                            &deduction_result, filename, line,
                            /* explicit template arguments */ NULL))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "ICS: Deduced arguments for template conversor constructor failed, skipping\n");
                    }
                    continue;
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: Deduced arguments for template conversor constructor succeeded\n");
                }

                // If the deduction succeeded just get a specialization and use it for the whole
                // conversion
                template_argument_list_t* template_arguments = 
                    build_template_argument_list_from_deduction_set(deduction_result);

                type_t* template_type = template_specialized_type_get_related_template_type(constructor->type_information);

                type_t* named_specialization_type = template_type_get_specialized_type(template_type,
                        template_arguments,
                        /* no template parameters */ NULL,
                        decl_context,
                        line, filename);

                // Now update the symbol
                constructor = named_type_get_symbol(named_specialization_type);

                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: Specialized conversor constructor '%s' is '%s'\n",
                            constructor->symbol_name,
                            print_declarator(constructor->type_information, decl_context));
                }
            }

            type_t* conversion_source_type = function_type_get_parameter_type_num(constructor->type_information, 0);

            standard_conversion_t first_sc;
            if (standard_conversion_between_types(&first_sc, orig, conversion_source_type, decl_context))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: First conversion from the original type '%s' "
                            "to the parameter type '%s' of the constructor suceeded\n",
                            print_declarator(orig, decl_context),
                            print_declarator(conversion_source_type, decl_context));
                }
                standard_conversion_t second_sc;
                if (standard_conversion_between_types(&second_sc, class_type, dest, decl_context))
                {
                    implicit_conversion_sequence_t *current = &(user_defined_conversions[num_user_defined_conversions]);
                    num_user_defined_conversions++;

                    current->kind = ICSK_USER_DEFINED;
                    current->first_sc = first_sc;
                    current->conversor = constructor;
                    current->second_sc = second_sc;

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "ICS: Details of this potential user defined conversion\n"
                                "ICS:     SCS1: %s -> %s\n"
                                "ICS:     Conversion function: %s (%s:%d)\n"
                                "ICS:     SCS2: %s -> %s\n",
                                print_declarator(current->first_sc.orig, decl_context),
                                print_declarator(current->first_sc.dest, decl_context),
                                current->conversor->symbol_name,
                                current->conversor->file,
                                current->conversor->line,
                                print_declarator(current->second_sc.orig, decl_context),
                                print_declarator(current->second_sc.dest, decl_context));
                    }
                }
            }
        }
    }

    if (num_user_defined_conversions > 0)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: Found %d user defined conversions\n", num_user_defined_conversions);

            if (num_user_defined_conversions > 1)
            {
                fprintf(stderr, "ICS: Must choose the best one or deem it ambiguous\n");
            }
        }
        // Pick the first one as the current best
        int current_best = 0;
        *result = user_defined_conversions[0];

        // We should select the best one
        int i;
        for (i = 1; i < num_user_defined_conversions; i++)
        {
            if (is_better_initialization_ics(user_defined_conversions[i],
                        user_defined_conversions[current_best],
                        dest,
                        decl_context, filename, line))
            {
                // Update the best
                current_best = i;
                *result = user_defined_conversions[i];
            }
        }

        // Check that it is actually the best
        for (i = 0; i < num_user_defined_conversions; i++)
        {
            if (i == current_best)
                continue;

            if (!is_better_initialization_ics(user_defined_conversions[current_best],
                        user_defined_conversions[i],
                        dest,
                        decl_context, filename, line))
            {
                // It is not best, set it to ambiguous
                result->is_ambiguous_ics = 1;
                break;
            }
        }

        if (result->is_ambiguous_ics)
        {
            DEBUG_CODE()
            {
                fprintf(stderr,
                        "ICS: Conversion from '%s' -> '%s' requires an AMBIGUOUS user defined sequence\n",
                        print_declarator(orig, decl_context),
                        print_declarator(dest, decl_context));
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Conversion from '%s' -> '%s' requires a user defined sequence\n",
                        print_declarator(orig, decl_context),
                        print_declarator(dest, decl_context));
                fprintf(stderr, "ICS: Details of this user defined conversion\n"
                        "ICS:     SCS1: %s -> %s\n"
                        "ICS:     Conversion function: %s (%s:%d)\n"
                        "ICS:     SCS2: %s -> %s\n",
                        print_declarator(result->first_sc.orig, decl_context),
                        print_declarator(result->first_sc.dest, decl_context),
                        result->conversor->symbol_name,
                        result->conversor->file,
                        result->conversor->line,
                        print_declarator(result->second_sc.orig, decl_context),
                        print_declarator(result->second_sc.dest, decl_context));
            }
        }
    }
}

static void compute_ics(type_t* orig, type_t* dest, decl_context_t decl_context, 
        implicit_conversion_sequence_t *result,
        const char* filename, int line)
{
    return compute_ics_flags(orig, dest, decl_context, result, 
            /* no_user_defined_conversions = */ 0,
            filename, line);
}

char type_can_be_implicitly_converted_to(type_t* orig, type_t* dest, decl_context_t decl_context, 
        char *ambiguous_conversion)
{
    implicit_conversion_sequence_t result;
    compute_ics(orig, dest, decl_context, &result, 
            /* filename = */ NULL, /* line = */ 0);

    *ambiguous_conversion = result.is_ambiguous_ics;

    return (result.kind != ICSK_INVALID);
}

/*
 * Returns true if scs1 is a proper subsequence of scs2
 */
static char standard_conversion_is_subsequence(standard_conversion_t scs1, standard_conversion_t scs2)
{
    // Note that lvalue transformations are not considered here
    if (scs1.conv[1] == SCI_NO_CONVERSION
            && scs1.conv[2] == SCI_NO_CONVERSION
            && (scs2.conv[1] != SCI_NO_CONVERSION
                || scs2.conv[2] != SCI_NO_CONVERSION))
    {
        // Standard says that identity SCS are always subsequences of non-identity ones
        return 1;
    }

    int i;
    int num_no_conv = 0;
    // We start from 1 because we ignore any lvalue-transformation (so says the
    // standard)
    for (i = 1; i < 3; i++)
    {
        if (scs1.conv[i] != scs2.conv[i])
        {
            if (scs1.conv[i] != SCI_NO_CONVERSION)
            {
                // Cannot be a subset if they are different
                return 0;
            }
            else
            {
                num_no_conv++;
            }
        }
    }

    if (num_no_conv == 0)
    {
        // Cannot be a proper subset if they are all the same
        return 0;
    }
    else return 1;
}

typedef
enum standard_conversion_rank_tag
{
    SCR_INVALID = 0,
    SCR_EXACT_MATCH,
    SCR_PROMOTION,
    SCR_CONVERSION
} standard_conversion_rank_t;

static standard_conversion_rank_t standard_conversion_get_rank(standard_conversion_t scs)
{
    standard_conversion_rank_t result = SCR_INVALID;

    if (scs.conv[0] == SCI_IDENTITY)
    {
        return SCR_EXACT_MATCH;
    }

    if (scs.conv[0] == SCI_LVALUE_TO_RVALUE
            || scs.conv[0] == SCI_FUNCTION_TO_POINTER
            || scs.conv[0] == SCI_ARRAY_TO_POINTER
            || scs.conv[2] == SCI_QUALIFICATION_CONVERSION)
    {
        result = SCR_EXACT_MATCH;
    }

    if (scs.conv[1] == SCI_INTEGRAL_PROMOTION
            || scs.conv[1] == SCI_VECTOR_INTEGRAL_PROMOTION
            || scs.conv[1] == SCI_FLOATING_PROMOTION)
    {
        result = SCR_PROMOTION;
    }

    if (scs.conv[1] == SCI_INTEGRAL_CONVERSION
            || scs.conv[1] == SCI_FLOATING_CONVERSION
            || scs.conv[1] == SCI_FLOATING_INTEGRAL_CONVERSION
            || scs.conv[1] == SCI_POINTER_CONVERSION
            || scs.conv[1] == SCI_POINTER_TO_MEMBER_CONVERSION
            || scs.conv[1] == SCI_BOOLEAN_CONVERSION)
    {
        result = SCR_CONVERSION;
    }

    ERROR_CONDITION(result == SCR_INVALID, "This cannot be invalid here", 0);

    return result;
}

static char standard_conversion_is_pointer_to_bool(standard_conversion_t scs)
{
    return ((is_pointer_type(scs.orig) || is_pointer_to_member_type(scs.orig))
            && is_bool_type(scs.dest));
}

static char standard_conversion_has_better_rank(standard_conversion_t scs1, 
        standard_conversion_t scs2, decl_context_t decl_context)
{
    standard_conversion_rank_t rank1 = standard_conversion_get_rank(scs1);
    standard_conversion_rank_t rank2 = standard_conversion_get_rank(scs2);

    // If any is invalid ranked is not a better rank
    if (rank1 == SCR_INVALID || rank2 == SCR_INVALID)
        return 0;

    if (rank1 < rank2)
    {
        return 1;
    }
    else if (rank1 == rank2)
    {
        // A conversion that is not a conversion of a pointer or
        // pointer-to-member to bool is better than another conversion
        // that is such a conversion
        if (!standard_conversion_is_pointer_to_bool(scs1)
                && standard_conversion_is_pointer_to_bool(scs2))
        {
            return 1;
        }

        /*
         * Some checks on "derivedness" and type kind are probably
         * rendundant below, but it is ok
         */
        if (equivalent_types(scs1.orig, scs2.orig, decl_context))
            // Both SCSs have same source type
        {
            // Fix redundant reference to class pointer
            if (is_reference_type(scs1.orig)
                    && is_pointer_to_class_type(reference_type_get_referenced_type(scs1.orig)))
            {
                scs1.orig = reference_type_get_referenced_type(scs1.orig);
            }
            if (is_reference_type(scs2.orig)
                    && is_pointer_to_class_type(reference_type_get_referenced_type(scs2.orig)))
            {
                scs2.orig = reference_type_get_referenced_type(scs2.orig);
            }

            if (is_pointer_to_class_type(scs1.orig) // B* ->
                    && is_pointer_to_class_type(scs1.dest) // A*

                    && pointer_to_class_type_is_derived(scs1.orig, scs1.dest) // B is derived from A

                    /* && is_pointer_to_class_type(scs2.orig) */ // B* ->
                    && is_pointer_to_void_type(scs2.dest) // void*
               )
            {
                // If class B is derived from class A conversion of B* to A* is
                // better than conversion from B* to void*
                return 1;
            }

            if (is_pointer_to_class_type(scs1.orig) // C* ->
                    && is_pointer_to_class_type(scs1.dest) // B*
                    && pointer_to_class_type_is_derived(scs1.orig, scs1.dest) // C is derived from B

                    /* && is_pointer_to_class_type(scs2.orig) */ // C* ->
                    && is_pointer_to_class_type(scs2.dest) // A*
                    /* && pointer_to_class_type_is_derived(scs2.orig, scs2.dest) */ // C is derived from A

                    && pointer_to_class_type_is_derived(scs1.dest, scs2.dest) // B is derived from A
               )
            {
                // If class C derives from B and B from A, conversion from C*
                // to B* is better than conversion from C* to A*
                return 1;
            }

            if (is_pointer_to_member_type(scs1.orig) // A::* ->
                    && is_pointer_to_member_type(scs1.dest) // B::* 
                    && class_type_is_derived(pointer_to_member_type_get_class_type(scs1.dest),
                        pointer_to_member_type_get_class_type(scs1.orig)) // B is derived from A

                    /* && is_pointer_to_member_type(scs2.orig) */ // A::* ->
                    && is_pointer_to_member_type(scs2.dest) // C::*
                    /* && class_type_is_derived(pointer_to_member_type_get_class_type(scs2.dest),
                       pointer_to_member_type_get_class_type(scs2.orig)) */ // C is derived from A

                    && class_type_is_derived(pointer_to_member_type_get_class_type(scs2.dest),
                        pointer_to_member_type_get_class_type(scs1.dest)) // C is derived from B
               )
            {
                // If class C derives from B and B from A, conversion from A::*
                // to B::* is better than conversion of A::* to C::*
                return 1;
            }

            if (is_class_type(scs1.orig) // C ->
                    && is_class_type(scs1.dest) // B
                    && class_type_is_derived(scs1.orig, scs1.dest) // C derives from B

                    && is_class_type(scs2.orig) // C ->
                    && is_class_type(scs2.dest) // A 

                    && class_type_is_derived(scs1.dest, scs2.dest) // B derives from A
               )
            {
                // If class C derives from B and B from A, a conversion C -> B
                // is better than C -> A
                return 1;
            }

            if (is_class_type(scs1.orig) // C ->
                    && is_reference_to_class_type(scs1.dest) // B&
                    && class_type_is_derived(scs1.orig, 
                        reference_type_get_referenced_type(scs1.dest)) // C derives from B

                    && is_reference_to_class_type(scs2.dest) // A&
                    && class_type_is_derived(reference_type_get_referenced_type(scs1.dest),
                        reference_type_get_referenced_type(scs2.dest)) // B derives from A
               )
            {
                // If class C derives from B and B from A, a conversion C -> B&
                // is better than C -> A&
                return 1;
            }

        }

        // Both SCS have same dest
        if (equivalent_types(scs1.dest, scs2.dest, decl_context))
        {
            if (is_pointer_to_class_type(scs1.orig) // A* ->
                    && is_pointer_to_void_type(scs1.dest) // void*

                    && is_pointer_to_class_type(scs2.orig) // B* ->
                    /* && is_pointer_to_void_type(scs2.dest) */ // void*

                    && pointer_to_class_type_is_derived(scs2.orig, scs1.orig) // B is derived from A
               )
            {
                // If class B derives from A, conversion of A* to
                // void* is better than conversion from B* to void*
                return 1;
            }

            if (is_pointer_to_class_type(scs1.orig) // B* ->
                    && is_pointer_to_class_type(scs1.dest) // A*

                    && pointer_to_class_type_is_derived(scs1.orig, scs1.dest) // B is derived from A

                    && is_pointer_to_class_type(scs2.orig) // C* -> 
                    && is_pointer_to_class_type(scs2.dest) // A* 
                    /* && pointer_to_class_type_is_derived(scs2.orig, scs2.dest) */ // C is derived from A

                    && pointer_to_class_type_is_derived(scs2.orig, scs1.orig) // C is derived from B
               )
            {
                // If class C derives from B and B from A, B* -> A* is better
                // than C* -> A*, 
                return 1;
            }

            if (is_class_type(scs1.orig) // B
                    && is_class_type(scs1.dest) // A
                    && class_type_is_derived(scs1.orig, scs1.dest) // B derives from A

                    && is_class_type(scs2.orig) // C
                    /* && is_class_type(scs2.dest) */ // A

                    && class_type_is_derived(scs2.orig, scs1.orig) // C derives from B
               )
            {
                // If class C derives from B and B from A, conversion B -> A 
                // is better than C -> A
                return 1;
            }

            if (is_class_type(scs1.orig) // B
                    && is_reference_to_class_type(scs1.dest) // A
                    && class_type_is_derived(scs1.orig, 
                        reference_type_get_referenced_type(scs1.dest)) // B is derived from A

                    && is_class_type(scs2.orig) // C
                    /* && is_reference_to_class_type(scs2.dest) */ // A
                    && class_type_is_derived(scs2.orig, 
                        scs1.orig) // C is derived from B
               ) 
            {
                return 1;
            }
        }
        // rank1 == rank2
        return 0;
    }
    else // rank1 > rank2
        return 0;
}

static char standard_conversion_differs_qualification(standard_conversion_t scs1,
        standard_conversion_t scs2, decl_context_t decl_context)
{
    if ((scs1.conv[0] == scs2.conv[0])
            && (scs1.conv[1] == scs2.conv[1])
            && (scs1.conv[2] == scs2.conv[2])
            && (scs1.conv[2] == SCI_POINTER_CONVERSION))
    {
        cv_qualifier_t cv_qualif_1 = CV_NONE;
        /* type_t* type_1 = */ advance_over_typedefs_with_cv_qualif(scs1.dest, &cv_qualif_1);

        cv_qualifier_t cv_qualif_2 = CV_NONE;
        /* type_t* type_2 = */ advance_over_typedefs_with_cv_qualif(scs2.dest, &cv_qualif_2);

        if (((cv_qualif_1 | cv_qualif_2) == cv_qualif_1) 
                && equivalent_types(get_unqualified_type(scs1.dest), 
                    get_unqualified_type(scs2.dest), 
                    decl_context))
        {
            return 1;
        }
    }
    else if ((scs1.conv[0] == scs2.conv[0])
            && (scs1.conv[1] == scs2.conv[1])
            && (scs1.conv[2] == scs2.conv[2]))
    {
        // If both are reference bindings, and scs2 leads to a type more qualified
        if (is_reference_type(scs1.dest)
                && is_reference_type(scs2.dest))
        {
            type_t* dest1 = get_unqualified_type(reference_type_get_referenced_type(scs1.dest));
            type_t* dest2 = get_unqualified_type(reference_type_get_referenced_type(scs2.dest));

            if (equivalent_types(dest1, dest2, decl_context)
                    && is_more_cv_qualified_type(reference_type_get_referenced_type(scs2.dest),
                        reference_type_get_referenced_type(scs1.dest)))
            {
                return 1;
            }
        }
    }

    return 0;
}

static
char standard_conversion_is_better(standard_conversion_t scs1, 
        standard_conversion_t scs2,
        decl_context_t decl_context)
{
    if (standard_conversion_is_subsequence(scs1, scs2))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Standard conversion SCS1 is better "
                    "than SCS2 because the first is a subsequence of the second\n");
        }
        return 1;
    }
    else if (standard_conversion_has_better_rank(scs1, scs2, decl_context))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Standard conversion SCS1 is better "
                    "than SCS2 because the first has better rank than the second\n");
        }
        return 1;
    }
    else if (standard_conversion_differs_qualification(scs1, scs2, decl_context))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Standard conversion SCS1 is better "
                    "than SCS2 because the first has better qualification than the second\n");
        }
        return 1;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCS: Standard conversion SCS1 is not better than SCS2\n");
    }
    return 0;
}

/*
 * Returns true if ics1 is known to be better than ics2
 */
static char better_ics(implicit_conversion_sequence_t ics1,
        implicit_conversion_sequence_t ics2,
        decl_context_t decl_context)
{
    if (ics1.kind == ICSK_STANDARD
            && (ics2.kind == ICSK_USER_DEFINED
                || ics2.kind == ICSK_ELLIPSIS))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: ICS1 is better than ICS2 because it is a "
                    "standard conversion against a user defined or ellipsis one\n");
        }
        return 1;
    }

    if (ics1.kind == ICSK_STANDARD
            && ics2.kind == ICSK_STANDARD)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: ICS1 and ICS2 are both standard conversions\n");
        }
        if (standard_conversion_is_better(ics1.first_sc, ics2.first_sc, decl_context))
            return 1;
    }
    else if (ics1.kind == ICSK_USER_DEFINED 
            && ics2.kind == ICSK_USER_DEFINED)
    {
        if (ics1.conversor == ics2.conversor
                && standard_conversion_is_better(ics1.second_sc, ics2.second_sc, decl_context))
            return 1;
    }
    
    return 0;
}

static overload_entry_list_t* compute_viable_functions(scope_entry_list_t* candidate_functions,
        type_t** argument_types, int num_types, decl_context_t decl_context,
        const char *filename, int line)
{
    overload_entry_list_t *result = NULL;
    scope_entry_list_t *it = candidate_functions;

    // There is always one more type, than argument types
    while (it != NULL)
    {
        scope_entry_t* candidate = it->entry;

        ERROR_CONDITION(!is_function_type(candidate->type_information),
                "This is not a function", 0);

        int num_arguments = num_types - 1;

        if (can_be_called_with_number_of_arguments(candidate, num_arguments))
        {
            // We have to check every argument type
            int first_type = 0;
            if (candidate->entity_specs.is_static 
                    || !candidate->entity_specs.is_member)
            {
                first_type = 1;
            }

            int num_parameters = 
                function_type_get_num_parameters(candidate->type_information);
            if (function_type_get_has_ellipsis(candidate->type_information))
                num_parameters--;

            char still_viable = 1;
            int i;
            char requires_ambiguous_conversion = 0;
            for (i = first_type; (i < num_types) && still_viable; i++)
            {
                int argument_number = i - 1;
                implicit_conversion_sequence_t ics_to_candidate;
                if (i == 0)
                {
                    ERROR_CONDITION(!candidate->entity_specs.is_member, "Should be member!", 0);
                    ERROR_CONDITION(argument_types[0] == NULL, "No implicit object given and this is a nonstatic member function", 0);

                    type_t* member_object_type = candidate->entity_specs.class_type;
                    member_object_type = get_cv_qualified_type(member_object_type, 
                            get_cv_qualifier(candidate->type_information));
                    member_object_type = get_reference_type(member_object_type);

                    compute_ics(argument_types[i], 
                            member_object_type,
                            decl_context, 
                            &ics_to_candidate, filename, line);
                }
                else
                {
                    type_t* parameter_type = NULL;
                    if (argument_number >= num_parameters)
                    {
                        parameter_type = get_ellipsis_type();
                    }
                    else
                    {
                        parameter_type = function_type_get_parameter_type_num(
                                candidate->type_information, 
                                argument_number);
                    }

                    compute_ics(argument_types[i], 
                            parameter_type,
                            decl_context, 
                            &ics_to_candidate, filename, line);
                }

                if (ics_to_candidate.kind == ICSK_INVALID)
                {
                    still_viable = 0;
                }
                else
                {
                    if (ics_to_candidate.kind == ICSK_USER_DEFINED)
                    {
                        requires_ambiguous_conversion |= ics_to_candidate.is_ambiguous_ics;
                    }
                }
            }

            if (still_viable)
            {
                overload_entry_list_t* new_result = calloc(1, sizeof(*new_result));
                new_result->entry = candidate;
                new_result->next = result;
                new_result->requires_ambiguous_ics = requires_ambiguous_conversion;
                result = new_result;
            }
        }

        it = it->next;
    }

    return result;
}

// States whether f is better than g
static
char is_better_function_flags(scope_entry_t* f,
        scope_entry_t* g,
        decl_context_t decl_context,
        type_t **argument_types,
        int num_types,
        char no_user_defined_conversions,
        const char *filename,
        int line)
{
    // A function is better if all ICS are equal or best, so if any is not
    // better or equal, then it is not better
    //
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Checking if [%s, %s:%d, %s] is better than [%s, %s:%d, %s]\n",
                f->symbol_name,
                f->file,
                f->line,
                print_declarator(f->type_information, decl_context),
                g->symbol_name,
                g->file,
                g->line,
                print_declarator(g->type_information, decl_context));
    }

    int first_type = 0;

    if (f->entity_specs.is_static
            || !f->entity_specs.is_member
            || g->entity_specs.is_static
            || !g->entity_specs.is_member)
    {
        first_type = 1;
    }

    char some_is_better = 0;
    int i;
    for (i = first_type; i < num_types; i++)
    {
        implicit_conversion_sequence_t ics_to_f;
        implicit_conversion_sequence_t ics_to_g;

        int argument_number = i - 1;

        if (i == 0)
        {
            type_t* member_object_type = NULL;

            ERROR_CONDITION(!f->entity_specs.is_member, "Should be member!", 0);
            member_object_type = f->entity_specs.class_type;
            member_object_type = get_cv_qualified_type(member_object_type, 
                    get_cv_qualifier(f->type_information));
            member_object_type = get_reference_type(member_object_type);

            compute_ics_flags(argument_types[i], member_object_type, decl_context, 
                    &ics_to_f, no_user_defined_conversions, filename, line);

            ERROR_CONDITION(!g->entity_specs.is_member, "Should be member!", 0);
            member_object_type = g->entity_specs.class_type;
            member_object_type = get_cv_qualified_type(member_object_type, 
                    get_cv_qualifier(g->type_information));
            member_object_type = get_reference_type(member_object_type);

            compute_ics_flags(argument_types[i], member_object_type, decl_context, 
                    &ics_to_g, no_user_defined_conversions, filename, line);
        }
        else
        {
            {
                int num_parameters_f = function_type_get_num_parameters(f->type_information);
                if (function_type_get_has_ellipsis(f->type_information))
                    num_parameters_f--;

                type_t* parameter_type_f = NULL;
                if (argument_number >= num_parameters_f)
                {
                    parameter_type_f = get_ellipsis_type();
                }
                else
                {
                    parameter_type_f = function_type_get_parameter_type_num(
                            f->type_information, 
                            argument_number);
                }

                compute_ics_flags(argument_types[i], 
                        parameter_type_f,
                        decl_context, &ics_to_f, 
                        no_user_defined_conversions, filename, line);
            }
            {
                int num_parameters_g = function_type_get_num_parameters(g->type_information);
                if (function_type_get_has_ellipsis(g->type_information))
                    num_parameters_g--;

                type_t* parameter_type_g = NULL;
                if (argument_number >= num_parameters_g)
                {
                    parameter_type_g = get_ellipsis_type();
                }
                else
                {
                    parameter_type_g = function_type_get_parameter_type_num(
                            g->type_information, 
                            argument_number);
                }

                compute_ics_flags(argument_types[i], 
                        parameter_type_g,
                        decl_context, &ics_to_g, 
                        no_user_defined_conversions, filename, line);
            }
        }

        if (better_ics(ics_to_f, ics_to_g, decl_context))
        {
            some_is_better = 1;
            continue;
        }

        // It is not better, maybe it is just as good
        if (better_ics(ics_to_g, ics_to_f, decl_context))
        {
            // It turned out that this one is actualy worse, so 'f' is not better than 'g'
            return 0;
        }
    }

    // If we saw that some argument ICS was really better, then it is better
    if (some_is_better)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Found that [%s, %s:%d, %s] IS better"
                    " than [%s, %s:%d, %s] because some argument in the first"
                    " function has a better ICS than the respective one in the second\n",
                    f->symbol_name,
                    f->file,
                    f->line,
                    print_declarator(f->type_information, decl_context),
                    g->symbol_name,
                    g->file,
                    g->line,
                    print_declarator(g->type_information, decl_context));
        }
        return 1;
    }

    // or if not that, non-template functions are preferred over template
    // functions
    if (!is_template_specialized_type(f->type_information)
            && is_template_specialized_type(g->type_information))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Found that [%s, %s:%d, %s] IS better than [%s, %s:%d, %s] because "
                    "the first is not a template-specialization and the second is\n",
                    f->symbol_name,
                    f->file,
                    f->line,
                    print_declarator(f->type_information, decl_context),
                    g->symbol_name,
                    g->file,
                    g->line,
                    print_declarator(g->type_information, decl_context));
        }
        return 1;
    }
    
    if (is_template_specialized_type(f->type_information)
            && is_template_specialized_type(g->type_information))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Found that [%s, %s:%d, %s] and [%s, %s:%d, %s] are template functions "
                    "so we have to check which one is more specialized\n",
                    f->symbol_name,
                    f->file,
                    f->line,
                    print_declarator(f->type_information, decl_context),
                    g->symbol_name,
                    g->file,
                    g->line,
                    print_declarator(g->type_information, decl_context));
        }
        // if ¬(g <= f) then f < g
        deduction_set_t* deduction_set = NULL;
        if (!is_less_or_equal_specialized_template_function(g->type_information, 
                    f->type_information, decl_context, &deduction_set, 
                    /* explicit_template_arguments */ NULL,
                    filename, line, /* is_conversion */ 0))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Found that template-function [%s, %s:%d, %s] is more "
                        "specialized than template-function [%s, %s:%d, %s]\n",
                        f->symbol_name,
                        f->file,
                        f->line,
                        print_declarator(f->type_information, decl_context),
                        g->symbol_name,
                        g->file,
                        g->line,
                        print_declarator(g->type_information, decl_context));
            }
            return 1;
        }
    }
    
    // It is not better (it might be equally good, though)
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Found that [%s, %s:%d, %s] is NOT better than [%s, %s:%d, %s]\n",
                f->symbol_name,
                f->file,
                f->line,
                print_declarator(f->type_information, decl_context),
                g->symbol_name,
                g->file,
                g->line,
                print_declarator(g->type_information, decl_context));
    }
    return 0;
}

static
char is_better_function(scope_entry_t* f,
        scope_entry_t* g,
        decl_context_t decl_context,
        type_t **argument_types,
        int num_types,
        const char *filename,
        int line)
{
    return is_better_function_flags(f, g, decl_context, argument_types, num_types,
            /* no_standard_conversions = */ 0, filename, line);
}


/*
 * num_arguments includes the implicit argument so it should never be zero, at least 1
 */
scope_entry_t* solve_overload(scope_entry_list_t* candidate_functions, 
        type_t **argument_types, int num_arguments,
        decl_context_t decl_context,
        const char *filename, int line)
{
    DEBUG_CODE()
    {

        fprintf(stderr, "OVERLOAD: Have to solve overload of an invocation with types\n");

        if (num_arguments > 0)
        {
            int i;
            for (i  = 0; i < num_arguments; i++)
            {
                if (argument_types[i] == NULL
                        && i == 0)
                {
                    fprintf(stderr, "OVERLOAD:    [0] <No implicit argument considered>\n");
                }
                else
                {
                    fprintf(stderr, "OVERLOAD:    [%d] %s", i, print_declarator(argument_types[i], decl_context));
                    if (i == 0)
                    {
                        fprintf(stderr, " <implicit argument type>");
                    }
                    fprintf(stderr, "\n");
                }
            }
        }
        else
        {
            fprintf(stderr, "OVERLOAD:    No arguments\n");
        }
        fprintf(stderr, "OVERLOAD: using one of these overloaded functions\n");
        if (candidate_functions != NULL)
        {
            scope_entry_list_t *it = candidate_functions;
            while (it != NULL)
            {
                fprintf(stderr, "OVERLOAD:    %s, %s:%d [%s] %s\n",
                        it->entry->symbol_name,
                        it->entry->file,
                        it->entry->line,
                        print_declarator(it->entry->type_information, decl_context),
                        (it->entry->entity_specs.is_builtin_function ? "<builtin function>" : ""));

                it = it->next;
            }
        }
        else
        {
            fprintf(stderr, "OVERLOAD:    No candidate functions given!\n");
        }
    }
    
    // First get the viable functions
    overload_entry_list_t *viable_functions = compute_viable_functions(candidate_functions, 
            argument_types, num_arguments, decl_context, filename, line);

    if (viable_functions == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: No better function found as no viable function exists\n");
        }
        return NULL;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Viable functions considered for overload\n");
            overload_entry_list_t *it = viable_functions;
            while (it != NULL)
            {
                fprintf(stderr, "OVERLOAD:    %s, %s:%d [%s]\n",
                        it->entry->symbol_name,
                        it->entry->file,
                        it->entry->line,
                        print_declarator(it->entry->type_information, decl_context));

                it = it->next;
            }
        }
    }

    overload_entry_list_t* best_viable = viable_functions;

    overload_entry_list_t* it = viable_functions->next;

    // First tournament
    while (it != NULL)
    {
        scope_entry_t* current = it->entry;

        if (is_better_function(current, best_viable->entry,
                    decl_context, argument_types, num_arguments,
                    filename, line))
        {
            best_viable = it;
        }

        it = it->next;
    }

    // Second tournament, now we have to ensure that the best_viable we have
    // now is the best one, so, all (but ourselves) should be worse than us
    char best_found = 1;
    it = viable_functions;
    while ((it != NULL) 
            && (best_viable != NULL))
    {
        scope_entry_t* current = it->entry;
        // Do not compare to ourselves
        if (current != best_viable->entry)
        {
            if (!is_better_function(best_viable->entry, current, 
                        decl_context, argument_types, num_arguments,
                        filename, line))
            {
                fprintf(stderr, "Ambiguous call to '%s'\n",
                        get_declaration_string_internal(current->type_information,
                            current->decl_context,
                            current->symbol_name, 
                            "", // initializer
                            0, // semicolon
                            NULL, // num_parameter_names
                            NULL, // parameter_names
                            0 // is_parameter
                            ));
                best_found = 0;
            }
        }

        it = it->next;
    }

    if (!best_found)
    {
        // Write also the one that was the best
        fprintf(stderr, "Ambiguous call to '%s'\n",
                get_declaration_string_internal(best_viable->entry->type_information,
                    best_viable->entry->decl_context,
                    best_viable->entry->symbol_name, 
                    "", // initializer
                    0, // semicolon
                    NULL, // num_parameter_names
                    NULL, // parameter_names
                    0 // is_parameter
                    ));
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: There is no best function\n");
        }
        return NULL;
    }
    else
    {
        if (best_viable->requires_ambiguous_ics)
        {
            fprintf(stderr, "Call to '%s' requires ambiguous conversion\n",
                    get_declaration_string_internal(best_viable->entry->type_information,
                        best_viable->entry->decl_context,
                        best_viable->entry->symbol_name, 
                        "", // initializer
                        0, // semicolon
                        NULL, // num_parameter_names
                        NULL, // parameter_names
                        0 // is_parameter
                        ));
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: There is no best function because ambiguous conversion\n");
            }
            return NULL;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Best viable function is [%s, %s]\n", 
                best_viable->entry->symbol_name,
                print_declarator(best_viable->entry->type_information, decl_context));
    }
    return best_viable->entry;
}

scope_entry_t* address_of_overloaded_function(scope_entry_list_t* overload_set, 
        template_argument_list_t* explicit_template_arguments,
        type_t* target_type,
        decl_context_t decl_context,
        const char *filename,
        int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Solving the address of overload function-name at '%s:%d'\n",
                filename, line);
    }
    // Check sanity of the target type
    if (!is_pointer_type(target_type)
            && !is_pointer_to_member_type(target_type)
            && !is_reference_type(target_type)
            && !is_function_type(target_type))
    {
        return NULL;
    }

    type_t* functional_type = NULL;
    scope_entry_t *class_type = NULL;

    if (is_pointer_type(target_type))
    {
        functional_type = pointer_type_get_pointee_type(target_type);
    }
    else if (is_pointer_to_member_type(target_type))
    {
        functional_type = pointer_type_get_pointee_type(target_type);
        class_type = pointer_to_member_type_get_class(target_type);
    }
    else if (is_reference_type(target_type))
    {
        functional_type = reference_type_get_referenced_type(target_type);
    }
    else if (is_function_type(target_type))
    {
        functional_type = target_type;
    }

    if (!is_function_type(functional_type))
    {
        return NULL;
    }

    // We can proceed now
    scope_entry_list_t* it = overload_set;

    scope_entry_list_t* viable_functions = NULL;

    char num_nonspecialized = 0;
    scope_entry_t* non_specialized = NULL;

    while (it != NULL)
    {
        scope_entry_t* current_fun = it->entry;

        if (current_fun->kind == SK_FUNCTION)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: When solving address of overload: checking '%s' "
                        "against overload '%s' ('%s' at '%s:%d')\n",
                        print_declarator(current_fun->type_information, decl_context),
                        print_declarator(target_type, decl_context),
                        current_fun->symbol_name,
                        current_fun->file,
                        current_fun->line);
            }
            char can_match = 0;

            if (current_fun->entity_specs.is_member 
                    && !current_fun->entity_specs.is_static
                    && is_pointer_to_member_type(target_type)
                    && equivalent_types(get_actual_class_type(current_fun->entity_specs.class_type),
                        get_actual_class_type(class_type->type_information), decl_context))
            {
                can_match = 1;
            }
            else if ((!current_fun->entity_specs.is_member
                        || (current_fun->entity_specs.is_member
                            && current_fun->entity_specs.is_static))
                    && !is_pointer_to_member_type(target_type))
            {
                can_match = 1;
            }

            if (can_match
                    && equivalent_types(current_fun->type_information, 
                        functional_type, decl_context))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: When solving address of overload: function "
                            "'%s' at '%s:%d' matches the target type\n",
                            current_fun->symbol_name,
                            current_fun->file,
                            current_fun->line);
                }
                scope_entry_list_t* new_viable_fun = calloc(1, sizeof(*viable_functions));
                new_viable_fun->entry = current_fun;
                new_viable_fun->next = viable_functions;
                viable_functions = new_viable_fun;

                num_nonspecialized++;
                // This makes sense only when (num_nonspecialized == 1)
                non_specialized = current_fun;
            }
        }
        else if (current_fun->kind == SK_TEMPLATE)
        {
            // We are in a case like this one
            //
            // template <typename _T>
            // _T f(_T);
            //
            // void g()
            // {
            //   int (*k)(int);
            //   k = f;
            // }
            //
            // The above assignment is identic to the following one
            //
            //   k = f<int>;
            //
            // but the compiler has to discover this by means of deduction

            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: When solving address of overload function: function '%s' is a template-name. "
                        "Deducing its template parameters\n", 
                        current_fun->symbol_name);
            }

            type_t* named_primary_type = template_type_get_primary_type(current_fun->type_information);
            scope_entry_t* primary_symbol = named_type_get_symbol(named_primary_type);

            char can_match = 0;

            if (primary_symbol->entity_specs.is_member 
                    && !primary_symbol->entity_specs.is_static
                    && is_pointer_to_member_type(target_type)
                    && equivalent_types(get_actual_class_type(primary_symbol->entity_specs.class_type),
                        get_actual_class_type(class_type->type_information), decl_context))
            {
                can_match = 1;
            }
            else if ((!primary_symbol->entity_specs.is_member
                        || (primary_symbol->entity_specs.is_member
                            && primary_symbol->entity_specs.is_static))
                    && !is_pointer_to_member_type(target_type))
            {
                can_match = 1;
            }

            if (can_match)
            {
                template_parameter_list_t* template_parameters 
                    = template_type_get_template_parameters(current_fun->type_information);

                type_t* argument_types[1] = { functional_type };
                int num_argument_types = 1;

                type_t* primary_type = primary_symbol->type_information;
                type_t* parameter_types[1] = { primary_type };

                deduction_set_t* deduced_arguments = NULL;

                if (deduce_template_arguments_common(
                            template_parameters,
                            argument_types, num_argument_types,
                            parameter_types, 
                            decl_context,
                            &deduced_arguments, filename, line,
                            explicit_template_arguments))
                {

                    template_argument_list_t* argument_list = build_template_argument_list_from_deduction_set(
                            deduced_arguments);
                    type_t* named_specialization_type = template_type_get_specialized_type(current_fun->type_information,
                            argument_list, /* no template parameters */ NULL,
                            decl_context, line, filename);

                    scope_entry_t* named_symbol = named_type_get_symbol(named_specialization_type);

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "OVERLOAD: When solving address of overload function: "
                                "template function-name specialization "
                                "'%s' at ('%s:%d') is a matching specialization with type '%s'\n",
                                named_symbol->symbol_name,
                                named_symbol->file,
                                named_symbol->line,
                                print_declarator(named_symbol->type_information, decl_context));
                    }

                    scope_entry_list_t* new_viable_fun 
                        = calloc(1, sizeof(*viable_functions));
                    new_viable_fun->entry = named_symbol;
                    new_viable_fun->next = viable_functions;
                    viable_functions = new_viable_fun;
                }
            }
        }
        else
        {
            internal_error("Unreachable code", 0);
        }

        it = it->next;
    }

    if (viable_functions == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: When solving address of overload: "
                    "no function was found to match the target type\n");
        }
        return NULL;
    }


    if (num_nonspecialized != 0)
    {
        // If more than one matched, error
        if (num_nonspecialized != 1)
        {
            // More than one nonspecialized function matches the types
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: When solving address of overload: more than one nonspecialized function matches\n");
            }
            return NULL;
        }

        // More than one nonspecialized function matches the types
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: When solving address of overload: solved to nonspecialized function '%s' at '%s:%d'\n",
                    non_specialized->symbol_name,
                    non_specialized->file,
                    non_specialized->line);
        }
        return non_specialized;
    }
    else // num_nonspecialized == 0
    {
        // Now we need the more specialized one
        // we will do a two scans algorithm

        scope_entry_list_t* it2 = viable_functions;
        scope_entry_t* most_specialized = it2->entry;
        it2 = it2->next;

        while (it2 != NULL)
        {
            scope_entry_t* current = it2->entry;
            deduction_set_t* deduction_set = NULL;
            if (!is_less_or_equal_specialized_template_function(
                        current->type_information,
                        most_specialized->type_information,
                        decl_context,
                        &deduction_set, 
                        /* explicit_template_arguments */ NULL,
                        filename, line, /* is_conversion */ 0))
            {
                // if (!(a<=b)) it2 means that a > b
                most_specialized = current;
            }

            it2 = it2->next;
        }

        // Now check it2 is actually the most specialized one
        it2 = viable_functions;

        while (it2 != NULL)
        {
            if (it2->entry != most_specialized)
            {
                scope_entry_t* current = it2->entry;
                deduction_set_t* deduction_set = NULL;
                if (is_less_or_equal_specialized_template_function(
                            most_specialized->type_information,
                            current->type_information,
                            decl_context,
                            &deduction_set, 
                            /* explicit_template_arguments */ NULL,
                            filename, line, /* is_conversion */ 0))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "OVERLOAD: When solving address of overload: no matching "
                                "specialization was the most specialized\n");
                    }
                    return NULL;
                }
            }
            it2 = it2->next;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: When solving address of overload: solved to matching "
                    "specialization '%s' (at '%s:%d' with type '%s') since it is the most specialized\n",
                    most_specialized->symbol_name,
                    most_specialized->file,
                    most_specialized->line,
                    print_declarator(most_specialized->type_information, decl_context));
        }

        return most_specialized;
    }

    return NULL;
}
