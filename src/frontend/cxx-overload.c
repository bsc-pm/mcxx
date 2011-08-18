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



#include "cxx-overload.h"
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-typeorder.h"
#include "cxx-typededuc.h"
#include "cxx-instantiation.h"
#include "cxx-utils.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"
#include "cxx-exprtype.h"

#include <string.h>

static unsigned long long int _bytes_overload = 0;

unsigned long long overload_used_memory(void)
{
    return _bytes_overload;
}

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
    candidate_t* candidate;
    struct overload_entry_list_tag* next;

    implicit_conversion_sequence_t ics_arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];

    char requires_ambiguous_ics;
} overload_entry_list_t;

static char better_ics(implicit_conversion_sequence_t ics1,
        implicit_conversion_sequence_t ics2);
static
char standard_conversion_is_better(standard_conversion_t scs1, 
        standard_conversion_t scs2);

static
char is_better_function_flags(overload_entry_list_t* f,
        overload_entry_list_t* g,
        decl_context_t decl_context,
        const char *filename,
        int line);

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

    // Build proper arguments for is_better_function_flags
    candidate_t candidate_1;
    memset(&candidate_1, 0, sizeof(candidate_1));

    candidate_1.entry = ics_1.conversor;
    candidate_1.num_args = 0;
    candidate_1.args = NULL;

    overload_entry_list_t ovl_entry_1;
    memset(&ovl_entry_1, 0, sizeof(ovl_entry_1));

    ovl_entry_1.candidate = &candidate_1;

    // Candidate 2
    candidate_t candidate_2;
    memset(&candidate_2, 0, sizeof(candidate_2));

    candidate_2.entry = ics_2.conversor;
    candidate_2.num_args = 0;
    candidate_2.args = NULL;

    overload_entry_list_t ovl_entry_2;
    memset(&ovl_entry_2, 0, sizeof(ovl_entry_2));

    ovl_entry_2.candidate = &candidate_2;

    if (is_better_function_flags(&ovl_entry_1,
                &ovl_entry_2,
                decl_context, 
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
    if (!is_better_function_flags(&ovl_entry_2,
                &ovl_entry_1,
                decl_context, 
                filename, line))
    {
        // Get the converted type after the conversion
        type_t* converted_type_1 = result_type_after_conversion(ics_1.conversor);
        type_t* converted_type_2 = result_type_after_conversion(ics_2.conversor);

        standard_conversion_t scs_1;
        if (!standard_conversion_between_types(&scs_1, converted_type_1, dest))
        {
            internal_error("A SCS should exist!", 0);
        }
        standard_conversion_t scs_2;
        if (!standard_conversion_between_types(&scs_2, converted_type_2, dest))
        {
            internal_error("A SCS should exist!", 0);
        }

        if (standard_conversion_is_better(scs_1, scs_2))
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
        implicit_conversion_sequence_t *result, 
        char no_user_defined_conversions,
        char is_implicit_argument,
        const char* filename, int line);

static void compute_ics_braced_list(type_t* orig, type_t* dest, decl_context_t decl_context, 
        implicit_conversion_sequence_t *result, 
        char no_user_defined_conversions,
        char is_implicit_argument,
        const char* filename, int line)
{
    fprintf(stderr, "ICS FOR BRACED LISTS: orig_type = %s\n",
            print_declarator(orig));
    fprintf(stderr, "ICS FOR BRACED LISTS: dest_type = %s\n",
            print_declarator(dest));

    scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(decl_context, 
            NULL, 
            /* mandatory */ 0);

    *result = invalid_ics;

    if (std_initializer_list_template != NULL)
    {
        if (is_template_specialized_type(dest)
                && equivalent_types(template_specialized_type_get_related_template_type(dest), 
                    std_initializer_list_template->type_information))
        {
            template_parameter_list_t* template_parameters = template_specialized_type_get_template_arguments(dest);

            ERROR_CONDITION( (template_parameters->num_parameters == 0), 
                    "Invalid template argument for std::init_list", 0);
            type_t* new_dest = template_parameters->arguments[0]->type;

            int i;
            int num_types = braced_list_type_get_num_types(orig);
            for (i = 0; i < num_types; i++)
            {
                implicit_conversion_sequence_t current;

                compute_ics_flags(braced_list_type_get_type_num(orig, i), 
                        new_dest, decl_context,
                        &current,
                        no_user_defined_conversions,
                        is_implicit_argument,
                        filename, line);

                if (current.kind == ICSK_INVALID)
                {
                    *result = current;
                    return;
                }
                if (result->kind == ICSK_INVALID
                        || better_ics(*result, current))
                {
                    *result = current;
                }
            }
        }
    }
    else if (is_class_type(dest)
            && !is_aggregate_type(dest))
    {
        scope_entry_list_t* candidates = NULL;
        scope_entry_t* conversors[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];

        type_t* arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { orig };

        scope_entry_t* constructor = solve_init_list_constructor(
                dest,
                arguments, 1,
                /* is_explicit */ 0,
                decl_context,
                filename, line,
                conversors,
                &candidates);
        entry_list_free(candidates);
                
        if (constructor != NULL)
        {
            result->kind = ICSK_USER_DEFINED;
            // Silly way of getting the identity
            standard_conversion_between_types(&result->first_sc, dest, dest);
            result->conversor = constructor;
            // FIXME: This should be the "real" dest (including
            // cv-qualification, rvalue refs, etc)
            standard_conversion_between_types(&result->second_sc, dest, dest);
        }
    }
    else if (is_class_type(dest)
            && is_aggregate_type(dest))
    {
        // Just check that each of the types can be used to initialize the
        // nonstatic data members

        scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(dest);

        if (entry_list_size(nonstatic_data_members) != braced_list_type_get_num_types(orig))
            return;

        scope_entry_list_iterator_t* it = NULL;
        int i;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            implicit_conversion_sequence_t init_ics = invalid_ics;
            scope_entry_t* member = entry_list_iterator_current(it);

            compute_ics_flags(braced_list_type_get_type_num(orig, i),
                    member->type_information,
                    decl_context,
                    &init_ics,
                    /* no_user_defined_conversions */ 0,
                    /* is_implicit_argument */ 0,
                    filename, line);
            
            if (init_ics.kind == ICSK_INVALID)
                return;
            i++;
        }
        entry_list_iterator_free(it);
        entry_list_free(nonstatic_data_members);

        result->kind = ICSK_USER_DEFINED;
        // Silly way of getting the identity
        standard_conversion_between_types(&result->first_sc, dest, dest);
        // FIXME: Which constructor??? Do aggregates have a special constructor???
        result->conversor = NULL;
        // FIXME: This should be the "real" dest (including
        // cv-qualification, rvalue refs, etc)
        standard_conversion_between_types(&result->second_sc, dest, dest);
    }
    else if (is_array_type(dest))
    {
        type_t* element_type = array_type_get_element_type(dest);

        int i, num_elems = braced_list_type_get_num_types(orig);
        for (i = 0; i < num_elems; i++)
        {
            implicit_conversion_sequence_t init_ics = invalid_ics;

            compute_ics_flags(braced_list_type_get_type_num(orig, i),
                    element_type,
                    decl_context,
                    &init_ics,
                    /* no_user_defined_conversions */ 0,
                    /* is_implicit_argument */ 0,
                    filename, line);

            if (init_ics.kind == ICSK_INVALID)
                return;
        }

        result->kind = ICSK_USER_DEFINED;
        // Silly way of getting the identity
        standard_conversion_between_types(&result->first_sc, dest, dest);
        // FIXME: Which constructor??? Do aggregates have a special constructor???
        result->conversor = NULL;
        // FIXME: This should be the "real" dest (including
        // cv-qualification, rvalue refs, etc)
        standard_conversion_between_types(&result->second_sc, dest, dest);
    }
    else if (!is_class_type(dest))
    {
        if (braced_list_type_get_num_types(orig) == 1)
        {
            compute_ics_flags(braced_list_type_get_type_num(orig, 0),
                    dest,
                    decl_context,
                    result,
                    /* no_user_defined_conversions */ 1,
                    /* is_implicit_argument */ 0,
                    filename, line);
        }
    }
}

static void compute_ics_flags(type_t* orig, type_t* dest, decl_context_t decl_context, 
        implicit_conversion_sequence_t *result, 
        char no_user_defined_conversions,
        char is_implicit_argument,
        const char* filename, int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "ICS: Computing ICS from '%s' -> '%s'\n", 
                print_declarator(orig),
                print_declarator(dest));
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

    if (is_braced_list_type(orig))
    {
        compute_ics_braced_list(orig, dest, decl_context, result, 
                no_user_defined_conversions, 
                is_implicit_argument, 
                filename, line);
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
        scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(orig);
        scope_entry_t* solved_function = address_of_overloaded_function(
                unresolved_set,
                unresolved_overloaded_type_get_explicit_template_arguments(orig),
                dest,
                decl_context,
                filename,
                line);
        entry_list_free(unresolved_set);

        if (solved_function != NULL)
        {
            if (!solved_function->entity_specs.is_member
                    || solved_function->entity_specs.is_static)
            {
                orig = get_lvalue_reference_type(solved_function->type_information);
            }
            else
            {
                orig = get_lvalue_reference_type(get_pointer_to_member_type(
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
        instantiate_template_class(symbol, decl_context, filename, line);
    }
    // Given a class 'A' base of a class 'B'
    //
    // To compute that 'T A::*' can be converted to 'T B::*' requires testing if 'A' is a base of 'B'
    // so 'B' must be instantiated.
    if (is_pointer_to_member_type(no_ref(dest)))
    {
        scope_entry_t* class_symbol = pointer_to_member_type_get_class(no_ref(dest));
        if (class_type_is_incomplete_independent(get_actual_class_type(class_symbol->type_information)))
        {
            instantiate_template_class(class_symbol, decl_context, filename, line);
        }
    }

    standard_conversion_t standard_conv;
    if (standard_conversion_between_types(&standard_conv, orig, dest))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: There is a standard conversion from '%s' -> '%s'\n", 
                    print_declarator(standard_conv.orig),
                    print_declarator(standard_conv.dest));
        }

        result->kind = ICSK_STANDARD;
        result->first_sc = standard_conv;

        // No need to check anything else
        return;
    }

    if (is_implicit_argument)
    {
        // An implicit argument of rvalue type C can be bound to the implicit
        // argument parameter of type C
        if (is_named_class_type(no_ref(orig))
                && class_type_is_incomplete_independent(get_actual_class_type(no_ref(orig))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Instantiating destination type know if it is derived or not\n");
            }
            scope_entry_t* symbol = named_type_get_symbol(no_ref(orig));
            instantiate_template_class(symbol, decl_context, filename, line);
            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Destination type instantiated\n");
            }
        }

        if ((equivalent_types(no_ref(orig), no_ref(dest))
                    || (is_class_type(no_ref(orig))
                            && is_class_type(no_ref(dest))
                            && class_type_is_base(no_ref(dest), no_ref(orig))))
                && (!is_const_qualified_type(no_ref(orig))
                    || is_const_qualified_type(no_ref(dest))))
        {
            result->kind = ICSK_STANDARD;
            result->first_sc.orig = no_ref(orig);
            result->first_sc.dest = dest;
            result->first_sc.conv[0] = SCI_IDENTITY;
            result->first_sc.conv[1] = SCI_NO_CONVERSION;
            result->first_sc.conv[2] = SCI_NO_CONVERSION;

            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: We allow binding the implicit argument of type '%s' to the implicit parameter type '%s'\n",
                        print_type_str(orig, decl_context),
                        print_type_str(dest, decl_context));
            }
        }
        return;
    }

    // Nothing else to do
    if (no_user_defined_conversions)
        return;

    // So no standard conversion is possible let's try with a user defined
    // conversion
    implicit_conversion_sequence_t user_defined_conversions[MCXX_MAX_USER_DEFINED_CONVERSIONS];
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
                    print_declarator(orig),
                    print_declarator(dest));
        }

        // Maybe there is a conversion function from class_type to something standard
        // convertible to dest
        scope_entry_list_t* conversion_list = class_type_get_all_conversions(
                get_actual_class_type(class_type), decl_context);

        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(conversion_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* conv_funct = entry_list_iterator_current(it);

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
                    = template_specialized_type_get_template_arguments(conv_funct->type_information);

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
                template_parameter_list_t* deduced_template_parameters = 
                    build_template_parameter_list_from_deduction_set(template_parameters, deduction_result);

                type_t* template_type = template_specialized_type_get_related_template_type(conv_funct->type_information);

                type_t* named_specialization_type = template_type_get_specialized_type(template_type,
                        deduced_template_parameters,
                        decl_context, filename, line);

                if (named_specialization_type == NULL)
                {
                    fprintf(stderr, "ICS: Cannot specialize conversion function\n");
                    continue;
                }

                // Now update the symbol
                conv_funct = named_type_get_symbol(named_specialization_type);

                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: Specialized conversion function '%s' is '%s'\n",
                            conv_funct->symbol_name,
                            print_declarator(conv_funct->type_information));
                }
            }

            type_t* converted_type = function_type_get_return_type(conv_funct->type_information);

            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Checking conversion function of '%s' to '%s'\n",
                        print_declarator(orig),
                        print_declarator(converted_type));
            }

            // The implicit parameter of this operator function is a reference
            // to the class type, this will filter not eligible conversion functions
            // (e.g. given a 'const T' we cannot call a non-const method)
            type_t* implicit_parameter = conv_funct->entity_specs.class_type;
            if (is_const_qualified_type(conv_funct->type_information))
            {
                implicit_parameter = get_cv_qualified_type(implicit_parameter, CV_CONST);
            }
            implicit_parameter = get_lvalue_reference_type(implicit_parameter);

            standard_conversion_t first_sc;
            standard_conversion_t second_sc;

            implicit_conversion_sequence_t ics_call;
            memset(&ics_call, 0, sizeof(ics_call));
            compute_ics_flags(orig, implicit_parameter, 
                    decl_context, &ics_call,
                    /* no_user_defined_conversions */ 1,
                    /* is_implicit_argument */ 1,
                    filename, line);
            first_sc = ics_call.first_sc;

            if (ics_call.kind == ICSK_STANDARD 
                    && standard_conversion_between_types(&second_sc, converted_type, dest))
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
                            print_declarator(current->first_sc.orig),
                            print_declarator(current->first_sc.dest),
                            current->conversor->symbol_name,
                            current->conversor->file,
                            current->conversor->line,
                            print_declarator(current->second_sc.orig),
                            print_declarator(current->second_sc.dest));
                }
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(conversion_list);
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
            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Instantiating destination type to get conversor constructors\n");
            }
            scope_entry_t* symbol = named_type_get_symbol(class_type);
            instantiate_template_class(symbol, decl_context, filename, line);
            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Destination type instantiated\n");
            }
        }

        scope_entry_list_t* constructors = class_type_get_constructors(get_actual_class_type(class_type));
        scope_entry_list_iterator_t* it;
        for (it = entry_list_iterator_begin(constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* constructor = entry_list_iterator_current(it);

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
                    = template_specialized_type_get_template_arguments(constructor->type_information);

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
                template_parameter_list_t* deduced_template_parameters = 
                    build_template_parameter_list_from_deduction_set(template_parameters, deduction_result);

                type_t* template_type = template_specialized_type_get_related_template_type(constructor->type_information);
 
                type_t* named_specialization_type = template_type_get_specialized_type(template_type,
                        deduced_template_parameters,
                        decl_context, filename, line); 

                if (named_specialization_type == NULL)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "ICS: Cannot specialize conversor constructor\n");
                    }
                    continue;
                }

                // Now update the symbol
                constructor = named_type_get_symbol(named_specialization_type);

                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: Specialized conversor constructor '%s' is '%s'\n",
                            constructor->symbol_name,
                            print_declarator(constructor->type_information));
                }
            }

            type_t* conversion_source_type = function_type_get_parameter_type_num(constructor->type_information, 0);

            standard_conversion_t first_sc;
            if (standard_conversion_between_types(&first_sc, orig, conversion_source_type))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "ICS: First conversion from the original type '%s' "
                            "to the parameter type '%s' of the constructor suceeded\n",
                            print_declarator(orig),
                            print_declarator(conversion_source_type));
                }
                standard_conversion_t second_sc;
                if (standard_conversion_between_types(&second_sc, class_type, dest))
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
                                print_declarator(current->first_sc.orig),
                                print_declarator(current->first_sc.dest),
                                current->conversor->symbol_name,
                                current->conversor->file,
                                current->conversor->line,
                                print_declarator(current->second_sc.orig),
                                print_declarator(current->second_sc.dest));
                    }
                }
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(constructors);
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
                        print_declarator(orig),
                        print_declarator(dest));
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: Conversion from '%s' -> '%s' requires a user defined sequence\n",
                        print_declarator(orig),
                        print_declarator(dest));
                fprintf(stderr, "ICS: Details of this user defined conversion\n"
                        "ICS:     SCS1: %s -> %s\n"
                        "ICS:     Conversion function: %s (%s:%d)\n"
                        "ICS:     SCS2: %s -> %s\n",
                        print_declarator(result->first_sc.orig),
                        print_declarator(result->first_sc.dest),
                        result->conversor->symbol_name,
                        result->conversor->file,
                        result->conversor->line,
                        print_declarator(result->second_sc.orig),
                        print_declarator(result->second_sc.dest));
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
            /* is_implicit_argument */ 0,
            filename, line);
}

char type_can_be_implicitly_converted_to(type_t* orig, type_t* dest, decl_context_t decl_context, 
        char *ambiguous_conversion, scope_entry_t** conversor,
        const char* filename, int line)
{
    CXX_LANGUAGE()
    {
        implicit_conversion_sequence_t result;
        compute_ics(orig, dest, decl_context, &result, 
                filename, line);

        *ambiguous_conversion = result.is_ambiguous_ics;

        if (conversor != NULL 
                && result.kind == ICSK_USER_DEFINED)
        {
            *conversor = result.conversor;
        }

        return (result.kind != ICSK_INVALID);
    }
    C_LANGUAGE()
    {
        internal_error("This function cannot be used in C", 0);
    }
    return 0;
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
    return ((is_pointer_type(no_ref(scs.orig)) || is_pointer_to_member_type(no_ref(scs.orig)))
            && is_bool_type(no_ref(scs.dest)));
}

static char standard_conversion_has_better_rank(standard_conversion_t scs1, 
        standard_conversion_t scs2)
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
        if (equivalent_types(scs1.orig, scs2.orig))
            // Both SCSs have same source type
        {
            // If both target types are the same, regardless the qualification,
            // this rank won't be better
            if (equivalent_types(get_unqualified_type(no_ref(scs1.dest)), get_unqualified_type(no_ref(scs2.dest)))
                    || (is_pointer_to_class_type(scs1.dest)
                        && is_pointer_to_class_type(scs2.dest)
                        && equivalent_types(get_unqualified_type(pointer_type_get_pointee_type(scs1.dest)),
                            get_unqualified_type(pointer_type_get_pointee_type(scs2.dest)))))
            {
                return 0;
            }

            // Fix redundant reference to class pointer
            if (is_lvalue_reference_type(scs1.orig)
                    && is_pointer_to_class_type(reference_type_get_referenced_type(scs1.orig)))
            {
                scs1.orig = reference_type_get_referenced_type(scs1.orig);
            }
            if (is_lvalue_reference_type(scs2.orig)
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

            if (is_class_type(no_ref(scs1.orig)) // C ->
                    && is_lvalue_reference_to_class_type(scs1.dest) // B&
                    && class_type_is_derived(no_ref(scs1.orig),
                        reference_type_get_referenced_type(scs1.dest)) // C derives from B&

                    && is_lvalue_reference_to_class_type(scs2.dest) // A&
                    && class_type_is_derived(reference_type_get_referenced_type(scs1.dest),
                        reference_type_get_referenced_type(scs2.dest)) // B& derives from A&
               )
            {
                // binding of an expression of type C to a reference of type B& is better
                // than binding an expression of type C to a reference of type A&
                return 1;
            }

        }

        // Both SCS have same dest
        if (equivalent_types(scs1.dest, scs2.dest))
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

            if (is_class_type(no_ref(scs1.orig)) // B
                    && is_lvalue_reference_to_class_type(scs1.dest) // A&
                    && class_type_is_derived(no_ref(scs1.orig), 
                        reference_type_get_referenced_type(scs1.dest)) // B& is derived from A&

                    && is_class_type(scs2.orig) // C&
                    /* && is_reference_to_class_type(scs2.dest) */ // A&
                    && class_type_is_derived(scs2.orig, 
                        no_ref(scs1.orig)) // C& is derived from B&
               ) 
            {
                // Binding of an expression type B to a reference of type A& is better than binding
                // an expression of type C to a reference of type A&
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
        standard_conversion_t scs2)
{
    if ((scs1.conv[0] == scs2.conv[0])
            && (scs1.conv[1] == scs2.conv[1])
            && (scs1.conv[2] == scs2.conv[2])
            && (scs1.conv[2] == SCI_POINTER_CONVERSION))
    {
        // FIXME - What about the deprecated literal string conversion?
        cv_qualifier_t cv_qualif_1 = CV_NONE;
        /* type_t* type_1 = */ advance_over_typedefs_with_cv_qualif(scs1.dest, &cv_qualif_1);

        cv_qualifier_t cv_qualif_2 = CV_NONE;
        /* type_t* type_2 = */ advance_over_typedefs_with_cv_qualif(scs2.dest, &cv_qualif_2);

        // Check that they yield similar types and scs2 is more qualified
        if (((cv_qualif_1 | cv_qualif_2) == cv_qualif_1) 
                && equivalent_types(get_unqualified_type(scs1.dest), 
                    get_unqualified_type(scs2.dest)
                    ))
        {
            return 1;
        }
    }
    else if ((scs1.conv[0] == scs2.conv[0])
            && (scs1.conv[1] == scs2.conv[1])
            && (scs1.conv[2] == scs2.conv[2]))
    {
        // If both are reference bindings, and scs2 binds a lvalue to a rvalue-reference
        // while scs1 binds a lvalue to a lvalue-reference, scs1 is better
        //
        // scs1: int& -> int&
        // scs2: int& -> int&&
        //
        // scs1 is better
        if (is_lvalue_reference_type(scs1.orig)        // binds a lvalue
                && is_lvalue_reference_type(scs1.dest) // to a lvalue-reference

                && is_lvalue_reference_type(scs2.orig) // binds a lvalue
                && is_rvalue_reference_type(scs2.dest)) // to a rvalue-reference
        {
            return 1;
        }

        // If both are reference bindings, and scs2 leads to the same type more qualified,
        // then scs1 is better than scs1
        if ((is_lvalue_reference_type(scs1.dest) 
                    || is_rvalue_reference_type(scs1.dest))
                && (is_lvalue_reference_type(scs2.dest) 
                    || is_rvalue_reference_type(scs2.dest)))
        {
            type_t* dest1 = get_unqualified_type(reference_type_get_referenced_type(scs1.dest));
            type_t* dest2 = get_unqualified_type(reference_type_get_referenced_type(scs2.dest));

            if (equivalent_types(dest1, dest2)
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
        standard_conversion_t scs2)
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
    else if (standard_conversion_has_better_rank(scs1, scs2))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Standard conversion SCS1 is better "
                    "than SCS2 because the first has better rank than the second\n");
        }
        return 1;
    }
    else if (standard_conversion_differs_qualification(scs1, scs2))
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
        implicit_conversion_sequence_t ics2)
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
        if (standard_conversion_is_better(ics1.first_sc, ics2.first_sc))
            return 1;
    }
    else if (ics1.kind == ICSK_USER_DEFINED 
            && ics2.kind == ICSK_USER_DEFINED)
    {
        if (ics1.conversor == ics2.conversor
                && standard_conversion_is_better(ics1.second_sc, ics2.second_sc))
            return 1;
    }
    
    return 0;
}

static char can_be_called_with_number_of_arguments_ovl(scope_entry_t* entry, int num_arguments)
{
    if (entry->entity_specs.is_member
            && !entry->entity_specs.is_constructor)
        num_arguments--;
    return can_be_called_with_number_of_arguments(entry, num_arguments);
}

static overload_entry_list_t* compute_viable_functions(candidate_t* candidate_functions,
        decl_context_t decl_context,
        const char *filename, int line)
{
    overload_entry_list_t *result = NULL;
    candidate_t *it = candidate_functions;

    while (it != NULL)
    {
        scope_entry_t* orig_candidate = it->entry;
        int num_arguments = it->num_args;
        type_t** argument_types = it->args;

        scope_entry_t* candidate = entry_advance_aliases(orig_candidate);

        ERROR_CONDITION(!is_function_type(candidate->type_information),
                "This is not a function", 0);

        if (can_be_called_with_number_of_arguments_ovl(candidate, num_arguments))
        {
            implicit_conversion_sequence_t ics_arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
            {
                int j;
                for (j = 0; j < MCXX_MAX_FUNCTION_CALL_ARGUMENTS; j++)
                {
                    ics_arguments[j] = invalid_ics;
                }
            }

            int num_parameters = 
                function_type_get_num_parameters(candidate->type_information);
            if (function_type_get_has_ellipsis(candidate->type_information))
                num_parameters--;

            char still_viable = 1;
            int i = 0;
            char requires_ambiguous_conversion = 0;

            // Static members have their implicit argument simply ignored
            if (candidate->entity_specs.is_member
                    && candidate->entity_specs.is_static)
            {
                i = 1;
            }

            for (; (i < num_arguments) && still_viable; i++)
            {
                int argument_number = i;

                implicit_conversion_sequence_t ics_to_candidate;
                if (i == 0
                        && candidate->entity_specs.is_member
                        && !candidate->entity_specs.is_constructor)
                {
                    // Set 'this' parameter (not argument!)
                    type_t* member_object_type = NULL;

                    // Using entities use the class where they are used, not
                    // their original type
                    if (orig_candidate->kind == SK_USING)
                    {
                        member_object_type = orig_candidate->entity_specs.class_type;
                    }
                    else
                    {
                        member_object_type = candidate->entity_specs.class_type;
                    }
                    member_object_type = get_cv_qualified_type(member_object_type, 
                            get_cv_qualifier(candidate->type_information));
                    member_object_type = get_lvalue_reference_type(member_object_type);

                    compute_ics_flags(argument_types[i], 
                            member_object_type,
                            decl_context, 
                            &ics_to_candidate,
                            /* no_user_defined_conversions */ 1,
                            /* is_implicit_argument */ 1,
                            filename, line);
                }
                else
                {
                    // The implicit is not counted in the function type, so skew it
                    if (candidate->entity_specs.is_member
                            && !candidate->entity_specs.is_constructor)
                        argument_number--;

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

                    ics_arguments[i] = ics_to_candidate;
                }
            }

            if (still_viable)
            {
                overload_entry_list_t* new_result = counted_calloc(1, sizeof(*new_result), &_bytes_overload);
                new_result->candidate = it;
                new_result->next = result;
                new_result->requires_ambiguous_ics = requires_ambiguous_conversion;
                result = new_result;

                int j;
                for (j = 0; j < MCXX_MAX_FUNCTION_CALL_ARGUMENTS; j++)
                {
                    // Copy all ICS of this overloaded function entry
                    result->ics_arguments[j] = ics_arguments[j];
                }
            }
        }

        it = it->next;
    }

    return result;
}

// States whether f is better than g
static
char is_better_function_flags(overload_entry_list_t* ovl_f,
        overload_entry_list_t* ovl_g,
        decl_context_t decl_context,
        const char *filename,
        int line)
{
    scope_entry_t* f = ovl_f->candidate->entry;
    scope_entry_t* g = ovl_g->candidate->entry;

    // A function is better if all ICS are equal or best, so if any is not
    // better or equal, then it is not better
    //
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Checking if [%s, %s:%d] is better than [%s, %s:%d]\n",
                print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                f->file,
                f->line,
                print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                g->file,
                g->line);
    }

    int first_type = 0;

    if (f->entity_specs.is_member
            && g->entity_specs.is_member
            && f->entity_specs.is_static)
    {
        first_type = 1;
    }

    ERROR_CONDITION(ovl_f->candidate->num_args != ovl_g->candidate->num_args, 
            "Mismatch in number of arguments", 0);

    char some_is_better = 0;
    char some_is_worse = 0;
    int i;
    for (i = first_type; i < ovl_f->candidate->num_args && !some_is_worse; i++)
    {
        implicit_conversion_sequence_t ics_to_f = ovl_f->ics_arguments[i];
        implicit_conversion_sequence_t ics_to_g = ovl_g->ics_arguments[i];
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Comparing ICSs of argument %d\n", i);
        }

        if (better_ics(ics_to_f, ics_to_g))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: ICS %d of first function is better than second\n", i);
            }
            some_is_better = 1;
        }
        else if (better_ics(ics_to_g, ics_to_f))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: ICS %d of first function is worse than second\n", i);
            }
            // It turned out that this one is actually worse, so 'f' is not better than 'g'
            some_is_worse = 1;
        }
    }

    if (!some_is_worse)
    {
        // If we saw that some argument ICS was really better and none was worse,
        // then it is better
        if (some_is_better)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Found that [%s, %s:%d] IS better"
                        " than [%s, %s:%d] because some argument in the first"
                        " function has a better ICS than the respective one in the second\n",
                        print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                        f->file,
                        f->line,
                        print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                        g->file,
                        g->line);
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
                fprintf(stderr, "OVERLOAD: Found that [%s, %s:%d] IS better than [%s, %s:%d] because "
                        "the first is not a template-specialization and the second is\n",
                        print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                        f->file,
                        f->line,
                        print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                        g->file,
                        g->line);
            }
            return 1;
        }

        if (is_template_specialized_type(f->type_information)
                && is_template_specialized_type(g->type_information))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Found that [%s, %s:%d] and [%s, %s:%d] are template functions "
                        "so we have to check which one is more specialized\n",
                        print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                        f->file,
                        f->line,
                        print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                        g->file,
                        g->line);
            }
            // if ¬(f <= g) then f > g
            deduction_set_t* deduction_set = NULL;
            if (!is_less_or_equal_specialized_template_function(
                        // Why is it so convoluted to get the type of the primary specialization ?
                        named_type_get_symbol(template_type_get_primary_type(
                                template_specialized_type_get_related_template_type(f->type_information)))->type_information,
                        named_type_get_symbol(template_type_get_primary_type(
                                template_specialized_type_get_related_template_type(g->type_information)))->type_information, 
                        decl_context, &deduction_set, 
                        /* explicit_template_parameters */ NULL,
                        filename, line, /* is_conversion */ 0))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: Found that template-function [%s, %s:%d] is more "
                            "specialized than template-function [%s, %s:%d]\n",
                            print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                            f->file,
                            f->line,
                            print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                            g->file,
                            g->line);
                }
                return 1;
            }
        }
    }

    // It is not better (it might be equally good, though)
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Found that [%s, %s:%d] is NOT better than [%s, %s:%d]\n",
                print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                f->file,
                f->line,
                print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                g->file,
                g->line);
    }
    return 0;
}

static
char is_better_function(overload_entry_list_t* f,
        overload_entry_list_t* g,
        decl_context_t decl_context,
        const char *filename,
        int line)
{
    return is_better_function_flags(f, g, decl_context, filename, line);
}


/*
 * num_arguments includes the implicit argument so it should never be zero, at least 1
 */
scope_entry_t* solve_overload(candidate_t* candidate_set,
        decl_context_t decl_context,
        const char *filename, int line,
        scope_entry_t** conversors)
{
    DEBUG_CODE()
    {

        fprintf(stderr, "OVERLOAD: Have to solve overload of functions\n");
        if (candidate_set != NULL)
        {
            int i = 0;
            candidate_t *it = candidate_set;
            while (it != NULL)
            {
                scope_entry_t* entry = entry_advance_aliases(it->entry);
                fprintf(stderr, "OVERLOAD: Candidate %d: %s, %s:%d [%s] %s\n",
                        i,
                        entry->symbol_name,
                        entry->file,
                        entry->line,
                        print_declarator(entry->type_information),
                        (entry->entity_specs.is_builtin ? "<builtin function>" : ""));

                fprintf(stderr, "OVERLOAD: Candidate %d: called with (", i);
                if (it->num_args == 0)
                {
                    fprintf(stderr, "<<no arguments>>");
                }
                else
                {
                    int j;
                    for (j = 0; j < it->num_args; j++)
                    {
                        if (j == 0 
                                && entry->entity_specs.is_member
                                && !entry->entity_specs.is_constructor)
                        {
                            fprintf(stderr, "[[implicit argument]] ");
                        }
                        ERROR_CONDITION(it->args[j] == NULL, "Invalid argument %d type!\n", j);

                        fprintf(stderr, "%s", print_declarator(it->args[j]) );

                        if ((j + 1) != it->num_args)
                        {
                            fprintf(stderr, ", ");
                        }
                    }
                }
                fprintf(stderr, ")\n");

                i++;
                it = it->next;
            }
        }
        else
        {
            fprintf(stderr, "OVERLOAD:    No candidate functions given!\n");
        }
    }

    // Special case for overloaded builtins of gcc
    if (candidate_set != NULL
            && candidate_set->next == NULL
            && is_computed_function_type(candidate_set->entry->type_information))
    {
        // Use this function removing all lvalues that might have arosen
        int i;
        for (i = 1; i < candidate_set->num_args; i++)
        {
            candidate_set->args[i] = no_ref(candidate_set->args[i]);
        }

        computed_function_type_t compute_type_function = 
            computed_function_type_get_computing_function(candidate_set->entry->type_information);

        const_value_t* const_value = NULL;
        scope_entry_t* solved_function = compute_type_function(candidate_set->entry, 
                candidate_set->args, 
                NULL,
                candidate_set->num_args,
                &const_value);

        return solved_function;
    }

    // Additional safety check to avoid mixing computed function types with
    // normal (overloaded) C++ functions
    {
        candidate_t *it = candidate_set;
        while (it != NULL)
        {
            scope_entry_t* entry = it->entry;

            if (is_computed_function_type(entry->type_information))
            {
                internal_error("Normal overloaded functions got somehow mixed with computed function types", 0);
            }

            if (entry->kind != SK_FUNCTION
                    && entry->kind != SK_USING)
            {
                internal_error("Invalid symbol of kind '%s' found during overload\n", symbol_kind_name(entry));
            }

            it = it->next;
        }
    }
    
    // First get the viable functions
    overload_entry_list_t *viable_functions = compute_viable_functions(candidate_set, 
            decl_context, filename, line);

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
                scope_entry_t* entry = entry_advance_aliases(it->candidate->entry);
                fprintf(stderr, "OVERLOAD:    %s:%d: %s %s\n",
                        entry->file,
                        entry->line,
                        print_decl_type_str(
                            entry->type_information,
                            entry->decl_context,
                            entry->symbol_name),
                        entry->entity_specs.is_builtin ? "<builtin>" : "");

                it = it->next;
            }
        }
    }

    overload_entry_list_t* best_viable = viable_functions;
    overload_entry_list_t* it = viable_functions->next;

    // First tournament
    while (it != NULL)
    {
        if (is_better_function(it, best_viable,
                    decl_context, filename, line))
        {
            best_viable = it;
        }

        it = it->next;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Second tournament, ensuring the best viable so far is really the best\n");
    }

    // Second tournament, now we have to ensure that the best_viable we have
    // now is the best one, so, all (but ourselves) should be worse than us
    char best_found = 1;
    it = viable_functions;
    while ((it != NULL) 
            && (best_viable != NULL))
    {
        overload_entry_list_t* current = it;
        // Do not compare to ourselves
        if (current != best_viable)
        {
            if (!is_better_function(best_viable, current, 
                        decl_context, filename, line))
            {
                DEBUG_CODE()
                {
                    scope_entry_t* entry = entry_advance_aliases(current->candidate->entry);
                    fprintf(stderr, "Ambiguous call to '%s'\n",
                            get_declaration_string_internal(entry->type_information,
                                entry->decl_context,
                                entry->symbol_name, 
                                "", // initializer
                                0, // semicolon
                                0, // num_parameter_names
                                NULL, // parameter_names
                                0 // is_parameter
                                ));
                }
                best_found = 0;
            }
        }

        it = it->next;
    }

    if (!best_found)
    {
        // Write also the one that was the best
        DEBUG_CODE()
        {
            scope_entry_t* entry = entry_advance_aliases(best_viable->candidate->entry);
            fprintf(stderr, "Ambiguous call to '%s'\n",
                    get_declaration_string_internal(entry->type_information,
                        entry->decl_context,
                        entry->symbol_name, 
                        "", // initializer
                        0, // semicolon
                        0, // num_parameter_names
                        NULL, // parameter_names
                        0 // is_parameter
                        ));
            fprintf(stderr, "OVERLOAD: There is no best function\n");
        }
        return NULL;
    }
    else
    {
        if (best_viable->requires_ambiguous_ics)
        {
            DEBUG_CODE()
            {
                scope_entry_t* entry = entry_advance_aliases(best_viable->candidate->entry);
                fprintf(stderr, "Call to '%s' requires ambiguous conversion\n",
                        get_declaration_string_internal(entry->type_information,
                            entry->decl_context,
                            entry->symbol_name, 
                            "", // initializer
                            0, // semicolon
                            0, // num_parameter_names
                            NULL, // parameter_names
                            0 // is_parameter
                            ));
                fprintf(stderr, "OVERLOAD: There is no best function because ambiguous conversion\n");
            }
            return NULL;
        }
    }

    DEBUG_CODE()
    {
        scope_entry_t* entry = entry_advance_aliases(best_viable->candidate->entry);
        fprintf(stderr, "OVERLOAD: Best viable function is [%s, %s]\n", 
                entry->symbol_name,
                print_declarator(entry->type_information));
    }

    if (conversors != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: List of called conversors\n");
        }
        int i;
        for (i = 0; i < best_viable->candidate->num_args; i++)
        {
            if (best_viable->ics_arguments[i].kind == ICSK_USER_DEFINED)
            {
                conversors[i] = best_viable->ics_arguments[i].conversor;
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD:    Argument %d: '%s' at %s:%d\n",
                            i,
                            conversors[i]->symbol_name,
                            conversors[i]->file,
                            conversors[i]->line);
                }
            }
            else
            {
                conversors[i] = NULL;
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD:    Argument %d: <no conversor called>\n",
                            i);
                }
            }
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: End of list of called conversors\n");
        }
    }

    return entry_advance_aliases(best_viable->candidate->entry);
}

scope_entry_t* address_of_overloaded_function(scope_entry_list_t* overload_set, 
        template_parameter_list_t* explicit_template_parameters,
        type_t* target_type,
        decl_context_t decl_context,
        const char *filename,
        int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Solving the address of overload function-name '%s' at '%s:%d'\n",
                /* use the first in the set */
                overload_set != NULL ? entry_list_head(overload_set)->symbol_name : "<overload set empty>",
                filename, line);
    }

    // If the set is a singleton, try first a simpler approach
    if (entry_list_size(overload_set) == 1)
    {
        scope_entry_t* item = entry_list_head(overload_set);
        standard_conversion_t sc;
        if (standard_conversion_between_types(&sc, item->type_information, target_type))
        {
            return item;
        }
    }

    // Check sanity of the target type
    if (!is_pointer_type(target_type)
            && !is_pointer_to_member_type(target_type)
            && !is_lvalue_reference_type(target_type)
            && !is_function_type(target_type))
    {
        return NULL;
    }

    type_t* functional_type = NULL;
    scope_entry_t *class_type = NULL;

    if (is_pointer_to_function_type(target_type))
    {
        functional_type = pointer_type_get_pointee_type(target_type);
    }
    else if (is_pointer_to_member_type(target_type))
    {
        functional_type = pointer_type_get_pointee_type(target_type);
        class_type = pointer_to_member_type_get_class(target_type);
    }
    else if (is_lvalue_reference_type(target_type))
    {
        functional_type = reference_type_get_referenced_type(target_type);
    }
    else if (is_function_type(target_type))
    {
        functional_type = target_type;
    }

    if (!is_function_type(functional_type))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Type '%s' is not a function type\n", print_declarator(target_type));
        }
        return NULL;
    }

    // We can proceed now
    scope_entry_list_t* viable_functions = NULL;

    char num_nonspecialized = 0;
    scope_entry_t* non_specialized = NULL;

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_fun = entry_list_iterator_current(it);

        if (current_fun->kind == SK_FUNCTION)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: When solving address of overload: checking '%s' "
                        "against (target) overload '%s' ('%s' at '%s:%d')\n",
                        print_declarator(current_fun->type_information),
                        print_declarator(target_type),
                        current_fun->symbol_name,
                        current_fun->file,
                        current_fun->line);
            }
            char can_match = 0;

            if (current_fun->entity_specs.is_member 
                    && !current_fun->entity_specs.is_static
                    && is_pointer_to_member_type(target_type)
                    && (equivalent_types(get_actual_class_type(current_fun->entity_specs.class_type),
                            get_actual_class_type(class_type->type_information))
                        || class_type_is_base(get_actual_class_type(current_fun->entity_specs.class_type),
                            get_actual_class_type(class_type->type_information))
                       )
               )
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
                        functional_type))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: When solving address of overload: function "
                            "'%s' at '%s:%d' matches the target type\n",
                            current_fun->symbol_name,
                            current_fun->file,
                            current_fun->line);
                }
                viable_functions = entry_list_add(viable_functions, current_fun);

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
                    && (equivalent_types(get_actual_class_type(primary_symbol->entity_specs.class_type),
                            get_actual_class_type(class_type->type_information))
                        || class_type_is_base(get_actual_class_type(primary_symbol->entity_specs.class_type),
                            get_actual_class_type(class_type->type_information))
                       )
               )
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

                if (deduce_template_parameters_common(
                            template_parameters,
                            argument_types, num_argument_types,
                            parameter_types, 
                            decl_context,
                            &deduced_arguments, filename, line,
                            explicit_template_parameters,
                            deduction_flags_empty()))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "OVERLOAD: When solving address of overload function: "
                                "template function-name specialization "
                                "'%s' deducing template arguments\n",
                                current_fun->symbol_name);
                    }

                    template_parameter_list_t* argument_list = build_template_parameter_list_from_deduction_set(
                            template_parameters,
                            deduced_arguments);
                    type_t* named_specialization_type = template_type_get_specialized_type(current_fun->type_information,
                            argument_list, decl_context, filename, line);

                    if (named_specialization_type != NULL)
                    {
                        scope_entry_t* named_symbol = named_type_get_symbol(named_specialization_type);

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "OVERLOAD: When solving address of overload function: "
                                    "template function-name specialization "
                                    "'%s' at ('%s:%d') is a matching specialization with type '%s'\n",
                                    named_symbol->symbol_name,
                                    named_symbol->file,
                                    named_symbol->line,
                                    print_declarator(named_symbol->type_information));
                        }

                        viable_functions = entry_list_add(viable_functions, named_symbol);
                    }
                }
            }
        }
        else
        {
            internal_error("Unreachable code", 0);
        }

    }
    entry_list_iterator_free(it);

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

        scope_entry_list_iterator_t* it2 = entry_list_iterator_begin(viable_functions);
        scope_entry_t* most_specialized = entry_list_iterator_current(it2);
        entry_list_iterator_next(it2);

        while (!entry_list_iterator_end(it2))
        {
            scope_entry_t* current = entry_list_iterator_current(it2);
            deduction_set_t* deduction_set = NULL;
            if (!is_less_or_equal_specialized_template_function(
                        current->type_information,
                        most_specialized->type_information,
                        decl_context,
                        &deduction_set, 
                        /* explicit_template_parameters */ NULL,
                        filename, line, /* is_conversion */ 0))
            {
                // if (!(a<=b)) it2 means that a > b
                most_specialized = current;
            }
            entry_list_iterator_next(it2);
        }
        entry_list_iterator_free(it2);

        // Now check it2 is actually the most specialized one
        it2 = entry_list_iterator_begin(viable_functions);
        while (!entry_list_iterator_end(it2))
        {
            scope_entry_t* current = entry_list_iterator_current(it2);
            if (current != most_specialized)
            {
                deduction_set_t* deduction_set = NULL;
                if (is_less_or_equal_specialized_template_function(
                            most_specialized->type_information,
                            current->type_information,
                            decl_context,
                            &deduction_set, 
                            /* explicit_template_parameters */ NULL,
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
            entry_list_iterator_next(it2);
        }
        entry_list_iterator_free(it2);

        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: When solving address of overload: solved to matching "
                    "specialization '%s' (at '%s:%d' with type '%s') since it is the most specialized\n",
                    most_specialized->symbol_name,
                    most_specialized->file,
                    most_specialized->line,
                    print_declarator(most_specialized->type_information));
        }

        return most_specialized;
    }

    return NULL;
}


static scope_entry_t* solve_constructor_(type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        char is_explicit, 
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** conversors,
        // Output arguments
        scope_entry_list_t** candidates,
        char init_constructors_only)
{
    ERROR_CONDITION(!is_named_class_type(class_type), "This is not a named class type", 0);

    if (class_type_is_incomplete_independent(get_actual_class_type(class_type)))
    {
        instantiate_template_class(named_type_get_symbol(class_type), decl_context, filename, line);
    }

    scope_entry_list_t* constructor_list = NULL;

    scope_entry_list_t* constructors = class_type_get_constructors(get_actual_class_type(class_type));
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
    {
        scope_entry_t* constructor 
            = entry_list_iterator_current(it);

        // If the context is not explicit ignore all constructors defined as explicit
        if (!is_explicit
                && constructor->entity_specs.is_explicit)
        {
            continue;
        }

        // Filter init constructors only
        if (init_constructors_only)
        {
            scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(decl_context, NULL, /* mandatory */ 1);

            int num_parameters = function_type_get_num_parameters(constructor->type_information);
            // Number of real parameters, ellipsis are counted as parameters
            // but only in the type system
            if (function_type_get_has_ellipsis(constructor->type_information))
                num_parameters--;

            char is_initializer_list_ctor = 0;
            if (num_parameters > 0
                    && can_be_called_with_number_of_arguments(constructor, 1))
            {
                type_t* first_param = function_type_get_parameter_type_num(constructor->type_information, 0);

                if (is_class_type(first_param))
                    first_param = get_actual_class_type(first_param);

                if (is_template_specialized_type(first_param)
                        && equivalent_types(template_specialized_type_get_related_template_type(first_param), 
                            std_initializer_list_template->type_information))
                {
                    is_initializer_list_ctor = 1;
                }
            }

            if (!is_initializer_list_ctor)
                continue;
        }

        // For template specialized types, use the template symbol
        if (is_template_specialized_type(constructor->type_information))
        {
            type_t* template_type = template_specialized_type_get_related_template_type(constructor->type_information);
            constructor = template_type_get_related_symbol(template_type);
        }

        constructor_list = entry_list_add(constructor_list, constructor);
    }
    entry_list_iterator_free(it);
    entry_list_free(constructors);

    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(constructor_list,
            NULL, argument_types, num_arguments,
            decl_context,
            filename, line, /* explicit_template_parameters */ NULL);

    scope_entry_t* augmented_conversors[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    memset(augmented_conversors, 0, sizeof(augmented_conversors));

    candidate_t* candidate_set = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        candidate_set = add_to_candidate_set(candidate_set,
                entry_list_iterator_current(it),
                num_arguments,
                argument_types);
    }
    entry_list_iterator_free(it);

    // Store the candidates here
    *candidates = overload_set;

    // Now we have all the constructors, perform an overload resolution on them
    scope_entry_t* overload_resolution = solve_overload(candidate_set, 
            decl_context, 
            filename, line, 
            augmented_conversors);

    int i;
    for (i = 0; i < num_arguments; i++)
    {
        conversors[i] = augmented_conversors[i+1];
    }

    return overload_resolution;
}

scope_entry_t* solve_constructor(
        type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        char is_explicit, 
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** conversors,
        scope_entry_list_t** candidates)
{
    return solve_constructor_(class_type,
            argument_types,
            num_arguments,
            is_explicit,
            decl_context,
            filename, line,
            conversors,
            candidates,
            /* init_constructors_only */ 0);
}

scope_entry_t* solve_init_list_constructor(
        type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        char is_explicit, 
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** conversors,
        scope_entry_list_t** candidates)
{
    ERROR_CONDITION(num_arguments != 1, "This function expects a single argument type", 0);
    ERROR_CONDITION(!is_braced_list_type(argument_types[0]), 
            "This function expects a single argument of type braced initializer list", 0);

    scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(decl_context, NULL, /* mandatory */ 1);

    char has_initializer_list_ctor = 0;
    if (std_initializer_list_template != NULL)
    {
        scope_entry_list_t* constructors = class_type_get_constructors(get_actual_class_type(class_type));
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            int num_parameters = function_type_get_num_parameters(entry->type_information);
            // Number of real parameters, ellipsis are counted as parameters
            // but only in the type system
            if (function_type_get_has_ellipsis(entry->type_information))
                num_parameters--;

            if (num_parameters > 0
                    && can_be_called_with_number_of_arguments(entry, 1))
            {
                type_t* first_param = function_type_get_parameter_type_num(entry->type_information, 0);

                if (is_class_type(first_param))
                    first_param = get_actual_class_type(first_param);

                if (is_template_specialized_type(first_param)
                        && equivalent_types(template_specialized_type_get_related_template_type(first_param), 
                            std_initializer_list_template->type_information))
                {
                    has_initializer_list_ctor = 1;
                }
            }
        }

        entry_list_iterator_free(it);
        entry_list_free(constructors);
    }

    if (has_initializer_list_ctor)
    {
        return solve_constructor_(class_type,
                argument_types,
                num_arguments,
                is_explicit,
                decl_context,
                filename, line,
                conversors,
                candidates,
                /* init_constructors_only */ 1);
    }
    else
    {
        return solve_constructor_(class_type,
                braced_list_type_get_types(argument_types[0]),
                braced_list_type_get_num_types(argument_types[0]),
                is_explicit,
                decl_context,
                filename, line,
                conversors,
                candidates,
                /* init_constructors_only */ 0);
    }
}

candidate_t* add_to_candidate_set(candidate_t* candidate_set,
        scope_entry_t* entry,
        int num_args,
        type_t** args)
{
    candidate_t* result = counted_calloc(1, sizeof(*result), &_bytes_overload);

    result->next = candidate_set;

    result->entry = entry;

    result->num_args = num_args;
    result->args = args;

    // Sanity check
    int i;
    for (i = 0; i < result->num_args; i++)
    {
        ERROR_CONDITION(result->args[i] == NULL, "An argument type cannot be NULL", 0);
    }

    return result;
}
