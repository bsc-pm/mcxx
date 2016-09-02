/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
#include "cxx-gccbuiltins.h"
#include "cxx-diagnostic.h"
#include "cxx-intelsupport.h"

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
    standard_conversion_t first_sc;

    // These below are only meaningful only for ICSK_USER_DEFINED
    scope_entry_t* conversor;
    standard_conversion_t second_sc;
    _Bool is_list_ics:1;
    _Bool is_aggregate_ics:1;
    _Bool is_ambiguous:1;
} implicit_conversion_sequence_t;

static const implicit_conversion_sequence_t invalid_ics = { .kind = ICSK_INVALID };

static implicit_conversion_sequence_t ics_make_identity(type_t* orig, type_t* dest)
{
    implicit_conversion_sequence_t res = {
        .kind = ICSK_STANDARD,
    };

    res.first_sc = get_identity_scs(orig, dest);
    return res;
}

static implicit_conversion_sequence_t ics_make_standard(standard_conversion_t scs)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "ICS: New standard conversion from '%s' to '%s'\n",
                print_declarator(scs.orig),
                print_declarator(scs.dest));
    }

    implicit_conversion_sequence_t res = {
        .kind = ICSK_STANDARD,
        .first_sc = scs
    };

    return res;
}

static implicit_conversion_sequence_t ics_make_ambiguous(void)
{
    implicit_conversion_sequence_t res = {
        .kind = ICSK_USER_DEFINED, // They are ranked the same way
        .is_ambiguous = 1,
    };

    return res;
}

static char ics_is_ambiguous(implicit_conversion_sequence_t ics)
{
    return (ics.kind == ICSK_USER_DEFINED && ics.is_ambiguous);
}

static implicit_conversion_sequence_t ics_make_user_defined(standard_conversion_t first_sc,
        scope_entry_t* conversor,
        standard_conversion_t second_sc)
{
    implicit_conversion_sequence_t res = {
        .kind = ICSK_USER_DEFINED,
        .first_sc = first_sc,
        .conversor = conversor,
        .second_sc = second_sc
    };

    DEBUG_CODE()
    {
        fprintf(stderr, "ICS: New user defined conversion\n");
        fprintf(stderr, "ICS:   First SCS: from '%s' to '%s'\n", print_declarator(first_sc.orig), print_declarator(first_sc.dest));
        fprintf(stderr, "ICS:   Conversor: '%s'\n", get_qualified_symbol_name(conversor, conversor->decl_context));
        fprintf(stderr, "ICS:   Second SCS: from '%s' to '%s'\n", print_declarator(second_sc.orig), print_declarator(second_sc.orig));
    }

    return res;
}

static char standard_conversion_between_types_for_overload(
        standard_conversion_t *scs,
        type_t* orig,
        type_t* dest,
        const locus_t* locus);

static void compute_scs_for_implicit_argument(
        standard_conversion_t* scs,
        type_t* orig,
        type_t* dest,
        ref_qualifier_t ref_qualifier,
        const locus_t* locus)
{
    *scs = get_invalid_scs();
    if (ref_qualifier == REF_QUALIFIER_NONE)
    {
        // An implicit argument of rvalue type C can be bound to the implicit
        // argument parameter of type C
        if ((equivalent_types(no_ref(orig), no_ref(dest))
                    || (is_class_type(no_ref(orig))
                        && is_class_type(no_ref(dest))
                        && class_type_is_base_instantiating(no_ref(dest), no_ref(orig), locus)
                        && !class_type_is_ambiguous_base_of_derived_class(no_ref(dest), no_ref(orig))))
                && (!is_const_qualified_type(no_ref(orig))
                    || is_const_qualified_type(no_ref(dest))))
        {
            *scs = get_identity_scs(orig, dest);
        }
    }
    else if (ref_qualifier == REF_QUALIFIER_LVALUE
            || ref_qualifier == REF_QUALIFIER_RVALUE)
    {
        standard_conversion_t standard_conv;
        if (standard_conversion_between_types_for_overload(&standard_conv, orig, dest, locus))
        {
            *scs = standard_conv;
        }
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static implicit_conversion_sequence_t ics_make_user_defined_using_conversor(type_t* orig,
        type_t* dest,
        scope_entry_t* conversor,
        const locus_t* locus)
{
    if (conversor == NULL)
        return invalid_ics;

    if (symbol_entity_specs_get_is_constructor(conversor))
    {
        type_t* conversion_source_type = function_type_get_parameter_type_num(conversor->type_information, 0);
        // Ellipsis is special and can only happen here, normalize it to orig
        // so standard_conversion_between_types_for_overload returns an
        // identity.
        if (is_ellipsis_type(conversion_source_type))
            conversion_source_type = orig;

        type_t* class_type = symbol_entity_specs_get_class_type(conversor);

        standard_conversion_t first_sc;
        if (!standard_conversion_between_types_for_overload(&first_sc, orig, conversion_source_type, locus))
            return invalid_ics;

        standard_conversion_t second_sc;
        if (!standard_conversion_between_types_for_overload(&second_sc, class_type, dest, locus))
            return invalid_ics;

        return ics_make_user_defined(first_sc, conversor, second_sc);
    }
    else if (symbol_entity_specs_get_is_conversion(conversor))
    {
        type_t* converted_type = function_type_get_return_type(conversor->type_information);
        ref_qualifier_t conv_ref_qualifier = function_type_get_ref_qualifier(conversor->type_information);

        type_t* implicit_parameter = symbol_entity_specs_get_class_type(conversor);
        if (is_const_qualified_type(conversor->type_information))
        {
            implicit_parameter = get_cv_qualified_type(implicit_parameter, CV_CONST);
        }
        if (conv_ref_qualifier == REF_QUALIFIER_NONE
                || conv_ref_qualifier == REF_QUALIFIER_LVALUE)
        {
            implicit_parameter = get_lvalue_reference_type(implicit_parameter);
        }
        else if (conv_ref_qualifier == REF_QUALIFIER_RVALUE)
        {
            implicit_parameter = get_rvalue_reference_type(implicit_parameter);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        standard_conversion_t first_sc;
        compute_scs_for_implicit_argument(
                &first_sc,
                orig,
                implicit_parameter,
                conv_ref_qualifier,
                locus);

        standard_conversion_t second_sc;
        standard_conversion_between_types_for_overload(
                &second_sc,
                converted_type,
                dest,
                locus);

        return ics_make_user_defined(first_sc, conversor, second_sc);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

typedef
struct overload_entry_list_tag
{
    candidate_t* candidate;
    struct overload_entry_list_tag* next;

    int num_ics_arguments;
    implicit_conversion_sequence_t *ics_arguments;

    char requires_ambiguous_ics;
} overload_entry_list_t;

static char better_ics(implicit_conversion_sequence_t ics1,
        implicit_conversion_sequence_t ics2);
static
char standard_conversion_is_better(standard_conversion_t scs1, 
        standard_conversion_t scs2);

static char is_better_function_despite_equal_ics(scope_entry_t* f,
        scope_entry_t* g,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // flags
        int num_arguments);

#if 0
static char is_better_initialization_ics(
        implicit_conversion_sequence_t ics_1,
        implicit_conversion_sequence_t ics_2,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    // Note that an ICS has two SCS's so we lexicographically compare them
    // a:<SCS[0,0], SCS[0,0]> and b:<SCS[1,0], SCS[1,1]>
    //
    // a > b if
    //     SCS[0,0]>SCS[1,0]
    //   or if
    //     SCS[0,0]==SCS[1,0] and SCS[0,1]>SCS[1,1]
    // But since we do not have an equality operator, SCS[0,0] == SCS[1,0]
    // the second condition is actually implemented as
    //     not(SCS[0,0]>SCS[1,0]) and not(SCS[1,0]>SCS[0,0]) and SCS[0,1]>SCS[1,1]

    // Check the first SCS
    {
        // Get the converted type before the conversion

        standard_conversion_t scs_1 = ics_1.first_sc;
        standard_conversion_t scs_2 = ics_2.first_sc;

        if (standard_conversion_is_better(scs_1, scs_2))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Conversion %s at %s is better than %s at %s "
                        "because first converted type is better\n",
                        ics_1.conversor->symbol_name,
                        locus_to_str(ics_1.conversor->locus),
                        ics_2.conversor->symbol_name,
                        locus_to_str(ics_2.conversor->locus));
            }
            return 1;
        }

        if (standard_conversion_is_better(scs_2, scs_1))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Conversion %s at %s is NOT better than %s at %s "
                        "because second converted type is better\n",
                        ics_1.conversor->symbol_name,
                        locus_to_str(ics_1.conversor->locus),
                        ics_2.conversor->symbol_name,
                        locus_to_str(ics_2.conversor->locus));
            }
            return 0;
        }
    }

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
    //
    // Check the second SCS
    {
        standard_conversion_t scs_1 = ics_1.second_sc;
        standard_conversion_t scs_2 = ics_2.second_sc;

        if (standard_conversion_is_better(scs_1, scs_2))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Conversion %s at %s is better than %s at %s "
                        "because second converted type is better\n",
                        ics_1.conversor->symbol_name,
                        locus_to_str(ics_1.conversor->locus),
                        ics_2.conversor->symbol_name,
                        locus_to_str(ics_2.conversor->locus));
            }
            return 1;
        }
    }

    if (is_better_function_despite_equal_ics(ics_1.conversor, ics_2.conversor, decl_context, locus))
        return 1;

    return 0;
}
#endif

static char standard_conversion_between_types_for_overload(
        standard_conversion_t *scs,
        type_t* orig,
        type_t* dest,
        const locus_t* locus)
{
    if (is_class_type(orig)
            && is_class_type(dest)
            && equivalent_types(orig, dest))
    {
            standard_conversion_t result = {
                .orig = orig,
                .dest = dest,
                .conv = { SCI_IDENTITY, SCI_NO_CONVERSION, SCI_NO_CONVERSION }
            };

            *scs = result;
            return 1;
        }
    else if (is_class_type(no_ref(orig))
            && is_class_type(dest)
            && class_type_is_base_strict_instantiating(dest, no_ref(orig), locus))
    {
        standard_conversion_t result = {
            .orig = orig,
            .dest = dest,
            .conv = { SCI_DERIVED_TO_BASE, SCI_NO_CONVERSION, SCI_NO_CONVERSION }
        };

        *scs = result;
        return 1;
    }

    return standard_conversion_between_types(scs,
            orig,
            dest,
            locus);
}

static void compute_ics_flags(type_t* orig, type_t* dest, const decl_context_t* decl_context, 
        implicit_conversion_sequence_t *result, 
        char no_user_defined_conversions,
        char is_implicit_argument,
        char needs_contextual_conversion,
        ref_qualifier_t ref_qualifier,
        const locus_t* locus);

static char solve_list_initialization_of_class_type_(
        type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        enum initialization_kind initialization_kind,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // Out
        scope_entry_t** constructor,
        scope_entry_list_t** candidates,
        char *is_ambiguous);

// Create a dummy initializer that preserves braced initializers
static nodecl_t build_dummy_initializer_for_braced_initialization(
        type_t* orig,
        const locus_t* locus)
{
    nodecl_t nodecl_type_list = nodecl_null();
    int num_types = braced_list_type_get_num_types(orig);

    int i;
    for (i = 0; i < num_types; i++)
    {
        type_t* t = braced_list_type_get_type_num(orig, i);
        nodecl_t nodecl_current;

        if (is_braced_list_type(t))
        {
            nodecl_current =
                build_dummy_initializer_for_braced_initialization(t, locus);
        }
        else
        {
            nodecl_current = nodecl_make_cxx_initializer(
                    nodecl_make_dummy(
                        braced_list_type_get_type_num(orig, i),
                        locus),
                    braced_list_type_get_type_num(orig, i),
                    locus);
        }

        nodecl_type_list = nodecl_append_to_list(
                nodecl_type_list,
                nodecl_current);
    }

    return nodecl_make_cxx_braced_initializer(
            nodecl_type_list,
            orig,
            locus);
}

static void compute_ics_braced_list(type_t* orig, type_t* dest, const decl_context_t* decl_context, 
        implicit_conversion_sequence_t *result, 
        char no_user_defined_conversions,
        char is_implicit_argument,
        const locus_t* locus)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "ICS: List-initialization sequence: orig_type = %s\n",
                print_declarator(orig));
        fprintf(stderr, "ICS: List-initialization sequence: dest_type = %s\n",
                print_declarator(dest));
    }

    scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(decl_context, 
            locus, /* mandatory */ 0);

    *result = invalid_ics;

    int num_types = braced_list_type_get_num_types(orig);

    type_t* ref_dest = dest;
    if (is_rvalue_reference_type(dest)
            || (is_lvalue_reference_type(dest) && is_const_qualified_type(no_ref(dest))))
    {
        dest = get_unqualified_type(no_ref(dest));
    }

    if (std_initializer_list_template != NULL
            && is_class_type(dest)
            && is_template_specialized_type(get_actual_class_type(dest))
            && equivalent_types(template_specialized_type_get_related_template_type(get_actual_class_type(dest)), 
                std_initializer_list_template->type_information))
    {
        template_parameter_list_t* template_parameters =
            template_specialized_type_get_template_arguments(get_actual_class_type(dest));

        ERROR_CONDITION( (template_parameters->num_parameters == 0), 
                "Invalid template argument for std::init_list", 0);
        type_t* new_dest = template_parameters->arguments[0]->type;

        int i;
        for (i = 0; i < num_types; i++)
        {
            implicit_conversion_sequence_t current;

            compute_ics_flags(braced_list_type_get_type_num(orig, i), 
                    new_dest, decl_context,
                    &current,
                    no_user_defined_conversions,
                    is_implicit_argument,
                    /* needs_contextual_conversion */ 0,
                    REF_QUALIFIER_NONE,
                    locus);

            if (current.kind == ICSK_INVALID)
            {
                *result = current;
                return;
            }
            if (result->kind == ICSK_INVALID
                    || better_ics(*result, current))
            {
                // Get always the worst conversion
                *result = current;
            }
        }

        result->is_list_ics = 1;
    }
    else if (is_class_type(dest)
            && num_types == 0
            && !no_user_defined_conversions)
    {
        scope_entry_t* default_constructor = class_type_get_default_constructor(dest);

        if (default_constructor != NULL
                && !symbol_entity_specs_get_is_explicit(default_constructor))
        {
            result->kind = ICSK_USER_DEFINED;
            result->first_sc = get_identity_scs(orig, dest);
            result->conversor = default_constructor;
            result->second_sc = get_identity_scs(dest, ref_dest);
            result->is_list_ics = 1;
        }
    }
    else if (is_class_type(dest)
            && !is_aggregate_type(dest)
            && !no_user_defined_conversions)
    {
        scope_entry_list_t* candidates = NULL;

        int num_arguments = braced_list_type_get_num_types(orig);
        type_t* arguments[num_arguments + 1];
        int i;
        for (i = 0; i < num_arguments; i++)
        {
            arguments[i] = braced_list_type_get_type_num(orig, i);
        }

        char is_ambiguous = 0;
        scope_entry_t* constructor = NULL;
        char ok = solve_list_initialization_of_class_type_(
                dest,
                arguments, num_arguments,
                IK_COPY_INITIALIZATION | IK_NO_MORE_USER_DEFINED_CONVERSIONS,
                decl_context,
                locus,
                // out
                &constructor,
                &candidates,
                &is_ambiguous);
        entry_list_free(candidates);

        if (ok)
        {
            result->kind = ICSK_USER_DEFINED;
            result->first_sc = get_identity_scs(orig, dest);
            result->conversor = constructor;
            result->second_sc = get_identity_scs(dest, ref_dest);
            result->is_list_ics = 1;
        }
        else if (is_ambiguous)
        {
            *result = ics_make_ambiguous();
        }
    }
    else if ((is_class_type(dest)
                || is_array_type(dest)
                || is_complex_type(dest)
                || is_vector_type(dest))
            && is_aggregate_type(dest))
    {
        // Aggregate initialization is so complex that we will use the cxx-exprtype code
        // rather than poorly mimicking it here. First we build a fake expression
        // that represents the initializer
        nodecl_t braced_initializer =
            build_dummy_initializer_for_braced_initialization(orig, locus);

        nodecl_t nodecl_result = nodecl_null();

        diagnostic_context_push_buffered();
        check_nodecl_braced_initializer(
                braced_initializer,
                decl_context,
                dest,
                /* is_explicit_type_cast */ 0,
                /* allow_excess_of_initializers */ 0,
                IK_COPY_INITIALIZATION,
                &nodecl_result);
        diagnostic_context_pop_and_discard();
        nodecl_free(braced_initializer);

        if (nodecl_is_err_expr(nodecl_result))
            return;

        result->kind = ICSK_USER_DEFINED;
        // Aggregate initialization
        result->first_sc = get_identity_scs(orig, dest);
        result->conversor = NULL; // No conversor, actually!
        result->second_sc = get_identity_scs(dest, ref_dest);
        result->is_list_ics = 1;
        result->is_aggregate_ics = 1;
    }
    // An array of N elements, and the length of the braced initializer is M, where M <= N. If M < N then
    // the element type of the array may be default-initialized
    else if (is_array_type(dest) // We know it is not an aggregate
            && !nodecl_is_null(array_type_get_array_size_expr(dest))
            && nodecl_is_constant(array_type_get_array_size_expr(dest))
            && (braced_list_type_get_num_types(orig) <=
                const_value_cast_to_signed_int(
                    nodecl_get_constant(array_type_get_array_size_expr(dest)))))
    {
        type_t* new_dest = array_type_get_element_type(dest);

        int i;
        for (i = 0; i < num_types; i++)
        {
            implicit_conversion_sequence_t current;

            compute_ics_flags(braced_list_type_get_type_num(orig, i), 
                    new_dest, decl_context,
                    &current,
                    no_user_defined_conversions,
                    is_implicit_argument,
                    /* needs_contextual_conversion */ 0,
                    REF_QUALIFIER_NONE,
                    locus);

            if (current.kind == ICSK_INVALID)
            {
                *result = current;
                return;
            }
            if (result->kind == ICSK_INVALID
                    || better_ics(*result, current))
            {
                // Get always the worst conversion
                *result = current;
            }
        }

        // M < N
        if (num_types <
                const_value_cast_to_signed_int(
                    nodecl_get_constant(array_type_get_array_size_expr(dest))))
        {
            if (!check_default_initialization_of_type(
                    new_dest,
                    decl_context,
                    locus,
                    /* constructor */ NULL))
            {
                *result = invalid_ics;
                return;
            }
        }

        result->is_list_ics = 1;
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
                    /* needs_contextual_conversion */ 0,
                    REF_QUALIFIER_NONE,
                    locus);
            if (result->kind != ICSK_INVALID)
            {
                result->is_list_ics = 1;
            }
        }
        else if (braced_list_type_get_num_types(orig) == 0)
        {
            *result = ics_make_identity(orig, ref_dest);
            result->is_list_ics = 1;
        }
    }
}

static scope_entry_t* get_specialized_conversion(
        scope_entry_t* conv_funct,
        type_t* dest,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "ICS: Symbol '%s' at '%s' is a template conversion function, "
                "deducing its arguments\n",
                conv_funct->symbol_name,
                locus_to_str(conv_funct->locus));
    }
    // This is a template so we have to get the proper specialization

    // Get its template parameters
    template_parameter_list_t* type_template_parameters = 
        template_type_get_template_parameters(template_specialized_type_get_related_template_type(conv_funct->type_information));
    template_parameter_list_t* template_parameters 
        = template_specialized_type_get_template_arguments(conv_funct->type_information);

    template_parameter_list_t* deduced_template_arguments = NULL;
    // Now deduce the arguments

    if (deduce_template_arguments_for_conversion_function(
                conv_funct,
                dest,
                template_parameters,
                type_template_parameters,
                // FIXME:
                /* raw_explicit_template_arguments */ NULL,
                decl_context,
                locus,
                // out
                &deduced_template_arguments) == DEDUCTION_FAILURE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: Deduced arguments for template conversion function failed, skipping\n");
        }
        return NULL;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "ICS: Deduced arguments for template conversion function succeeded\n");
    }

    // If the deduction succeeded just get a specialization and use it for the whole
    // conversion
    type_t* template_type = template_specialized_type_get_related_template_type(conv_funct->type_information);

    type_t* named_specialization_type = template_type_get_specialized_type(template_type,
            deduced_template_arguments,
            decl_context, locus);
    free_template_parameter_list(deduced_template_arguments);

    if (named_specialization_type == NULL)
    {
        fprintf(stderr, "ICS: Cannot specialize conversion function\n");
        return NULL;
    }

    // Now update the symbol
    conv_funct = named_type_get_symbol(named_specialization_type);

    DEBUG_CODE()
    {
        fprintf(stderr, "ICS: Specialized conversion function '%s' is '%s'\n",
                conv_funct->symbol_name,
                print_declarator(conv_funct->type_information));
    }
    return conv_funct;
}

static char solve_initialization_of_nonclass_type_ics(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context, 
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus,
        implicit_conversion_sequence_t* ics);

static char solve_initialization_of_reference_type_ics(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus,
        implicit_conversion_sequence_t* ics);

static char solve_initialization_of_nonclass_nonreference_type_ics(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context, 
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus,
        implicit_conversion_sequence_t* ics);

static char solve_initialization_of_class_type_(
        type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        enum initialization_kind initialization_kind,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // Out
        scope_entry_t** constructor,
        scope_entry_list_t** candidates,
        char *is_ambiguous);

static void compute_ics_flags(type_t* orig, type_t* dest, const decl_context_t* decl_context, 
        implicit_conversion_sequence_t *result, 
        char no_user_defined_conversions,
        char is_implicit_argument,
        char needs_contextual_conversion,
        ref_qualifier_t ref_qualifier,
        const locus_t* locus)
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
                locus);
        return;
    }

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
                locus);
        entry_list_free(unresolved_set);

        if (solved_function != NULL)
        {
            if (!symbol_entity_specs_get_is_member(solved_function)
                    || symbol_entity_specs_get_is_static(solved_function))
            {
                orig = get_lvalue_reference_type(solved_function->type_information);
            }
            else
            {
                orig = get_pointer_to_member_type(
                            solved_function->type_information,
                            symbol_entity_specs_get_class_type(solved_function));
            }
            // And proceed evaluating this ICS
        }
    }

    if (is_implicit_argument)
    {
        standard_conversion_t scs;
        compute_scs_for_implicit_argument(
                &scs,
                orig,
                dest,
                ref_qualifier,
                locus);

        if (!standard_conversion_is_invalid(scs))
        {
            *result = ics_make_standard(scs);
        }
        return;
    }

    standard_conversion_t standard_conv;
    if (standard_conversion_between_types_for_overload(&standard_conv, orig, dest, locus))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: There is a standard conversion from '%s' -> '%s'\n", 
                    print_declarator(standard_conv.orig),
                    print_declarator(standard_conv.dest));
        }

        *result = ics_make_standard(standard_conv);

        // No need to check anything else
        return;
    }

    // Nothing else to do
    if (no_user_defined_conversions)
        return;

    // Compute user defined conversions by means of conversion functions
    int num_user_defined_conversions = 0;
    implicit_conversion_sequence_t user_defined_conversion;

    // Compute user defined conversions by means of constructors
    if (is_class_type(dest))
    {
        scope_entry_t* constructor = NULL;
        scope_entry_list_t* candidates = NULL;
        enum initialization_kind specific_initialization;
        if (is_class_type(no_ref(orig))
                && class_type_is_derived_instantiating(
                    no_ref(orig),
                    dest,
                    locus))
        {
            specific_initialization = IK_BY_CONSTRUCTOR;
        }
        else
        {
            specific_initialization = IK_BY_USER_DEFINED_CONVERSION;
        }
        char is_ambiguous = 0;
        char ok = solve_initialization_of_class_type_(
                    get_unqualified_type(dest),
                    &orig, 1,
                    IK_COPY_INITIALIZATION | specific_initialization | IK_NO_MORE_USER_DEFINED_CONVERSIONS,
                    decl_context,
                    locus,
                    // Out
                    &constructor,
                    &candidates,
                    &is_ambiguous);
        entry_list_free(candidates);

        if (ok)
        {
            user_defined_conversion
                = ics_make_user_defined_using_conversor(orig, dest, constructor, locus);
            num_user_defined_conversions++;
        }
        else if (is_ambiguous)
        {
            user_defined_conversion = ics_make_ambiguous();
            num_user_defined_conversions++;
        }
    }
    else // not a class type (may be a reference or a non-class, or both)
    {
        scope_entry_t* conversor = NULL;
        scope_entry_list_t* candidates = NULL;
        implicit_conversion_sequence_t current_ics;

        enum initialization_kind initialization_kind = IK_INVALID;
        if (!needs_contextual_conversion)
        {
            initialization_kind = IK_COPY_INITIALIZATION | IK_NO_MORE_USER_DEFINED_CONVERSIONS;
        }
        else
        {
            initialization_kind = IK_DIRECT_INITIALIZATION | IK_NO_MORE_USER_DEFINED_CONVERSIONS;
        }

        char ok = solve_initialization_of_nonclass_type_ics(
                orig,
                dest,
                decl_context,
                initialization_kind,
                &conversor,
                &candidates,
                locus,
                &current_ics);
        entry_list_free(candidates);

        if (ok && conversor != NULL)
        {
            user_defined_conversion = current_ics;
            num_user_defined_conversions++;
        }
    }


    if (num_user_defined_conversions > 0)
    {
        *result = user_defined_conversion;
        DEBUG_CODE()
        {
            if (!ics_is_ambiguous(user_defined_conversion))
            {
                fprintf(stderr, "ICS: Conversion from '%s' -> '%s' requires a user defined sequence\n",
                        print_declarator(orig),
                        print_declarator(dest));
                fprintf(stderr, "ICS: Details of this user defined conversion\n"
                        "ICS:     SCS1: %s -> %s\n"
                        "ICS:     Conversion function: %s (%s)\n"
                        "ICS:     SCS2: %s -> %s\n",
                        print_declarator(result->first_sc.orig),
                        print_declarator(result->first_sc.dest),
                        result->conversor ? result->conversor->symbol_name : "<<NULL>>",
                        result->conversor ? locus_to_str(result->conversor->locus) : locus_to_str(0),
                        print_declarator(result->second_sc.orig),
                        print_declarator(result->second_sc.dest));
            }
            else
            {
                fprintf(stderr, "ICS: Conversion from '%s' -> '%s' requires an ambiguous user defined sequence\n",
                        print_declarator(orig),
                        print_declarator(dest));
            }
        }
    }
}

static scope_entry_t* solve_overload_(candidate_t* candidate_set,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        type_t* dest,
        const locus_t* locus,
        // Out
        char *is_ambiguous);

static char solve_initialization_of_direct_reference_type_ics(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus,
        implicit_conversion_sequence_t* ics)
{
    // 13.3.1.6 [over.match.ref]
    // Under the conditions specified in 8.5.3, a reference can be bound directly to a glvalue or class prvalue that is
    // the result of applying a conversion function to an initializer expression. Overload resolution is used to select
    // the conversion function to be invoked. Assuming that "cv1 T" is the underlying type of the reference being
    // initialized, and "cv S" is the type of the initializer expression, with S a class type, the candidate functions
    // are selected as follows:
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Solving initializaion of reference type '%s' using a value of type '%s' "
                "by direct reference\n",
                print_declarator(dest),
                print_declarator(orig));
    }
    *ics = invalid_ics;

    // Direct reference binding
    // We need to use overload here using conversion functions (as we
    // know the destination is not a class)
    scope_entry_list_t* candidate_list = NULL;

    // 13.3.1.6 [over.match.ref]
    // The conversion functions of S and its base classes are considered.
    standard_conversion_t scs;
    scope_entry_list_t* conversions =
        class_type_get_all_conversions(get_actual_class_type(no_ref(orig)),
                decl_context);

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(conversions);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* conversion
            = entry_list_iterator_current(it);

        if (is_template_specialized_type(conversion->type_information))
        {
            conversion = get_specialized_conversion(conversion, dest,
                    decl_context, locus);
            if (conversion == NULL)
                continue;
        }

        if (initialization_kind & IK_COPY_INITIALIZATION
                && symbol_entity_specs_get_is_explicit(conversion))
            continue;

        type_t* return_type =
            function_type_get_return_type(conversion->type_information);

        char ok = 0;
        if (!symbol_entity_specs_get_is_explicit(conversion))
        {
            // 13.3.1.6 [over.match.ref]
            // Those non-explicit conversion
            // functions that are not hidden within S and yield type "lvalue reference to cv2 T2" (when initializing
            // an lvalue reference or an rvalue reference to function) or " cv2 T2" or "rvalue reference to cv2 T2"
            // (when initializing an rvalue reference or an lvalue reference to function), where "cv1 T" is reference-
            // compatible (8.5.3) with "cv2 T2", are candidate functions.
            if (!is_lvalue_reference_type(dest)
                    || is_lvalue_reference_type(return_type))
            {
                ok = type_is_reference_compatible_to(
                        no_ref(dest),
                        no_ref(return_type));
            }
        }
        else
        {
            // 13.3.1.6 [over.match.ref]
            // For direct-initialization, those explicit
            // conversion functions that are not hidden within S and yield type "lvalue reference to cv2 T2" or "cv2
            // T2" or "rvalue reference to cv2 T2," respectively, where T2 is the same type as T or can be converted
            // to type T with a qualification conversion (4.4), are also candidate functions
            ok = (standard_conversion_between_types(&scs,
                        return_type,
                        dest,
                        locus)
                    && (standard_conversion_is_identity(scs)
                        || (scs.conv[2] == SCI_QUALIFICATION_CONVERSION)));
        }

        if (ok)
        {
            candidate_list = entry_list_add(candidate_list, conversion);
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(conversions);

    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(
            candidate_list,
            NULL, &orig, 1,
            decl_context,
            locus, /* explicit_template_arguments */ NULL);
    entry_list_free(candidate_list);

    candidate_t* candidate_set = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        candidate_set = candidate_set_add(candidate_set,
                entry_list_iterator_current(it),
                1,
                &orig);
    }
    entry_list_iterator_free(it);

    // Now we have all the candidates, perform an overload resolution on them
    char is_ambiguous = 0;
    scope_entry_t* overload_resolution = solve_overload_(candidate_set,
            decl_context,
            initialization_kind | IK_BY_DIRECT_REFERENCE_BINDING,
            dest,
            locus,
            // Out
            &is_ambiguous);
    candidate_set_free(&candidate_set);

    if (!is_ambiguous)
    {
        *ics = ics_make_user_defined_using_conversor(orig, dest, overload_resolution, locus);
    }
    else
    {
        *ics = ics_make_ambiguous();
    }
    *candidates = overload_set;
    *conversor = overload_resolution;

    DEBUG_CODE()
    {
        if (overload_resolution != NULL)
        {
            fprintf(stderr, "OVERLOAD: Solving initializaion of reference type '%s' using a value of type '%s' "
                    "by direct reference binding succeeded using conversion '%s' at %s\n",
                    print_declarator(dest),
                    print_declarator(orig),
                    get_qualified_symbol_name(overload_resolution, overload_resolution->decl_context),
                    locus_to_str(overload_resolution->locus));
        }
        else
        {
            fprintf(stderr, "OVERLOAD: Solving initializaion of reference type '%s' using a value of type '%s' "
                    "by direct reference binding failed\n",
                    print_declarator(dest),
                    print_declarator(orig));
        }
    }

    return (overload_resolution != NULL);
}

static scope_entry_list_t* conversion_function_candidates_initialization_of_nonclass_nonreference_type(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        const locus_t* locus)
{
    // 13.3.1.5 [over.match.conv]
    // Under the conditions specified in 8.5, as part of an initialization of
    // an object of nonclass type, a conversion function can be invoked to
    // convert an initializer expression of class type to the type of the
    // object being initialized.

    scope_entry_list_t* candidate_list = NULL;

    class_type_complete_if_possible(
            named_type_get_symbol(no_ref(orig)),
            decl_context,
            locus);

    // 13.3.1.5 [over.match.conv]
    // The conversion functions of S and its base classes are considered.
    scope_entry_list_t* conversions =
        class_type_get_all_conversions(get_actual_class_type(no_ref(orig)),
                decl_context);

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(conversions);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* conversion
            = entry_list_iterator_current(it);

        if (is_template_specialized_type(conversion->type_information))
        {
            conversion = get_specialized_conversion(conversion, dest,
                    decl_context, locus);
            if (conversion == NULL)
                continue;
        }

        // 13.3.1.5 [over.match.conv]
        // Conversion functions that return a cv-qualified type are considered to yield
        // the cv-unqualified version of that type for this process of selecting candidate
        // functions. Conversion functions that return "reference to cv2 X" return lvalues
        // or xvalues, depending on the type of reference, of type "cv2 X" and are
        // therefore considered to yield X for this process of selecting candidate
        // functions.
        type_t* return_type =
            function_type_get_return_type(conversion->type_information);

        standard_conversion_t scs;
        char ok = 0;

        if (!symbol_entity_specs_get_is_explicit(conversion))
        {
            // 13.3.1.5 [over.match.conv]
            // Those non-explicit conversion functions that are not hidden within S
            // and yield type T or a type that can be converted to type T via a
            // standard conversion sequence (13.3.3.1.1) are candidate functions.
            ok = (standard_conversion_between_types(&scs,
                    return_type,
                    dest,
                    locus));

        }
        else if (symbol_entity_specs_get_is_explicit(conversion)
                && (initialization_kind & IK_DIRECT_INITIALIZATION))
        {
            // 13.3.1.5 [over.match.conv]
            // For direct-initialization, those explicit conversion functions that are not
            // hidden within S and yield type T or a type that can be converted to type T
            // with a qualification conversion (4.4) are also candidate functions.
            ok = (standard_conversion_between_types(&scs,
                    return_type,
                    dest,
                    locus));
            ok = ok && (standard_conversion_is_identity(scs)
                    || (scs.conv[2] == SCI_QUALIFICATION_CONVERSION));
        }

        if (ok)
        {
            candidate_list = entry_list_add(candidate_list, conversion);
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(conversions);

    return candidate_list;
}

static scope_entry_list_t* conversion_function_candidates_initialization_of_class_type(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        const locus_t* locus)
{
    scope_entry_list_t* candidate_list = NULL;
    // NOTE: Despite the name of the relevant section of the standard, this code can be
    // used also for direct initialization in this context
    //
    // struct A
    // {
    // };
    //
    // struct B
    // {
    //   operator A();
    // };
    //
    // B b;
    // A a(b);

    // 13.3.1.4 [over.match.copy]
    // When the type of the initializer expression is a class type "cv
    // S", the non-explicit conversion functions of S and its base classes
    // are considered.
    scope_entry_list_t* conversions = class_type_get_all_conversions(
            get_actual_class_type(no_ref(orig)),
            decl_context);

    scope_entry_list_iterator_t* it;
    for (it = entry_list_iterator_begin(conversions);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* conversion
            = entry_list_iterator_current(it);

        if (is_template_specialized_type(conversion->type_information))
        {
            conversion = get_specialized_conversion(conversion, dest,
                    decl_context, locus);
            if (conversion == NULL)
                continue;
        }

        if (initialization_kind & IK_COPY_INITIALIZATION)
        {
            // 13.3.1.4 [over.match.copy]
            // When initializing a temporary to be bound to the
            // first parameter of a constructor that takes a reference to possibly
            // cv-qualified T as its first argument, called with a single argument
            // in the context of direct-initialization of an object of type "cv2
            // T", explicit conversion functions are also considered.
            if (symbol_entity_specs_get_is_explicit(conversion))
                continue;
        }

        // 13.3.1.4 [over.match.copy]
        // Those that are not hidden within S and yield a type whose cv-unqualified
        // version is the same type as T or is a derived class thereof are
        // candidate functions. Conversion functions that return "reference to X"
        // return lvalues or xvalues, depending on the type of reference, of type X
        // and are therefore considered to yield X for this process of selecting
        // candidate functions.
        type_t* return_type =
            get_unqualified_type(
                    no_ref(function_type_get_return_type(conversion->type_information)));

        if (!equivalent_types(return_type, dest)
                && (!is_class_type(return_type)
                    || !class_type_is_derived_instantiating(return_type, dest, locus)))
            continue;

        candidate_list = entry_list_add(candidate_list, conversion);
    }
    entry_list_iterator_free(it);
    entry_list_free(conversions);

    return candidate_list;
}

static char solve_initialization_of_nonclass_nonreference_type_ics(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus,
        implicit_conversion_sequence_t* ics)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Solving initialization of nonclass nonreference type '%s' using a value of type '%s'\n",
                print_declarator(dest),
                print_declarator(orig));
    }
    *ics = invalid_ics;

    ERROR_CONDITION(is_any_reference_type(dest)
            || is_class_type(dest),
            "Invalid type", 0);
    C_LANGUAGE()
    {
        internal_error("This function cannot be used in C", 0);
    }

    CXX_LANGUAGE()
    {
        standard_conversion_t scs;
        if (standard_conversion_between_types(&scs, orig, get_unqualified_type(dest), locus))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Initialization of nonclass nonreference "
                        "type '%s' using a value of type '%s' solved using a standard conversion sequence\n",
                        print_declarator(dest),
                        print_declarator(orig));
            }
            *conversor = 0;
            *ics = ics_make_standard(scs);
            return 1;
        }
        else if (is_class_type(no_ref(orig)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Initialization of nonclass nonreference "
                        "type '%s' using a value of class type '%s' will attempt a user defined conversion\n",
                        print_declarator(dest),
                        print_declarator(orig));
            }
            // We need to use overload here using conversion functions (as we
            // know the destination is not a class)
            scope_entry_list_t* candidate_list =
                conversion_function_candidates_initialization_of_nonclass_nonreference_type(
                    no_ref(orig),
                    dest,
                    decl_context,
                    initialization_kind,
                    locus);

            scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(
                    candidate_list,
                    NULL, &orig, 1,
                    decl_context,
                    locus, /* explicit_template_arguments */ NULL);
            entry_list_free(candidate_list);

            candidate_t* candidate_set = NULL;
            scope_entry_list_iterator_t* it;
            for (it = entry_list_iterator_begin(overload_set);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                candidate_set = candidate_set_add(candidate_set,
                        entry_list_iterator_current(it),
                        1,
                        &orig);
            }
            entry_list_iterator_free(it);

            // Now we have all the candidates, perform an overload resolution on them
            char is_ambiguous = 0;
            scope_entry_t* overload_resolution = solve_overload_(candidate_set,
                    decl_context,
                    initialization_kind | IK_BY_USER_DEFINED_CONVERSION,
                    dest,
                    locus,
                    // Out
                    &is_ambiguous);
            candidate_set_free(&candidate_set);

            *candidates = overload_set;
            *conversor = overload_resolution;

            if (!is_ambiguous)
            {
                *ics = ics_make_user_defined_using_conversor(orig, dest, *conversor, locus);
            }
            else
            {
                *ics = ics_make_ambiguous();
            }

            DEBUG_CODE()
            {
                if (overload_resolution != NULL)
                {
                    fprintf(stderr, "OVERLOAD: Initialization of nonclass nonreference "
                            "type '%s' using a value of class type '%s' uses conversor '%s' (at %s)\n",
                            print_declarator(dest),
                            print_declarator(orig),
                            get_qualified_symbol_name(overload_resolution, overload_resolution->decl_context),
                            locus_to_str(overload_resolution->locus));
                }
                else
                {
                    fprintf(stderr, "OVERLOAD: Initialization of nonclass nonreference "
                            "type '%s' using a value of class type '%s' is not possible\n",
                            print_declarator(dest),
                            print_declarator(orig));
                }
            }

            return (overload_resolution != NULL);
        }
    }
    return 0;
}

static char solve_initialization_of_nonclass_nonreference_type(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context, 
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus)
{
    implicit_conversion_sequence_t dummy_ics = invalid_ics;
    return solve_initialization_of_nonclass_nonreference_type_ics(
        orig,
        dest,
        decl_context, 
        initialization_kind,
        conversor,
        candidates,
        locus,
        &dummy_ics);
}

static char solve_initialization_of_reference_type_ics(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus,
        implicit_conversion_sequence_t* ics)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Solving initialization of reference type '%s' using a value of type '%s'\n",
                print_declarator(dest),
                print_declarator(orig));
    }
    *ics = invalid_ics;
    ERROR_CONDITION(!is_any_reference_type(dest), "Invalid type", 0);

    if (is_lvalue_reference_type(dest))
    {
        if (is_lvalue_reference_type(orig)
                && type_is_reference_compatible_to(no_ref(dest), no_ref(orig)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Initialization of reference type '%s' using a value of type '%s' "
                        " solved because the first is reference compatible to the second\n",
                        print_declarator(dest),
                        print_declarator(orig));
            }
            standard_conversion_t scs;
            standard_conversion_between_types(&scs, orig, dest, locus);
            *ics = ics_make_standard(scs);
            return 1;
        }

        if (is_class_type(no_ref(orig))
                && solve_initialization_of_direct_reference_type_ics(orig, dest,
                    decl_context, initialization_kind,
                    conversor, candidates, locus, ics))
            return 1;
    }

    if ((is_lvalue_reference_type(dest)
                && is_const_qualified_type(no_ref(dest))
                && !is_volatile_qualified_type(no_ref(dest)))
            || (is_rvalue_reference_type(dest)))
    {
        if ((is_rvalue_reference_type(orig)
                || is_class_type(orig)
                || (is_lvalue_reference_type(orig) && is_function_type(no_ref(orig))))
                && type_is_reference_compatible_to(no_ref(dest), no_ref(orig)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Initialization of reference type '%s' using a value of type '%s' "
                        "solved because the first is reference compatible to the second and the "
                        "second is either rvalue, class or lvalue of function\n",
                        print_declarator(dest),
                        print_declarator(orig));
            }
            standard_conversion_t scs;
            standard_conversion_between_types(&scs, orig, dest, locus);
            *ics = ics_make_standard(scs);
            return 1;
        }

        if (is_class_type(no_ref(orig))
                && !type_is_reference_related_to(no_ref(dest), no_ref(orig))
                && solve_initialization_of_direct_reference_type_ics(orig, dest,
                    decl_context, initialization_kind,
                    conversor, candidates, locus, ics))
            return 1;

        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Initialization of reference type '%s' using a value of type '%s' "
                        "will attempt to bind to a temporary\n",
                        print_declarator(dest),
                        print_declarator(orig));
            }
            // Construct a temporary
            char ok = 0;
            char is_ambiguous = 0;
            if (is_class_type(no_ref(dest)))
            {
                scope_entry_t* constructor = NULL;
                enum initialization_kind specific_initialization;
                if (is_class_type(no_ref(orig))
                        && class_type_is_derived_instantiating(
                            no_ref(orig),
                            no_ref(dest),
                            locus))
                {
                    specific_initialization = IK_BY_CONSTRUCTOR;
                }
                else
                {
                    specific_initialization = IK_BY_USER_DEFINED_CONVERSION;
                }
                ok = solve_initialization_of_class_type_(
                        no_ref(dest),
                        &orig, 1,
                        IK_COPY_INITIALIZATION  | specific_initialization | (initialization_kind & IK_NO_MORE_USER_DEFINED_CONVERSIONS),
                        decl_context,
                        locus,
                        // Out
                        &constructor,
                        candidates,
                        &is_ambiguous);

                if (ok)
                {
                    // [DR1604]
                    //
                    // If T1 is a class type, user-defined conversions are
                    // considered using the rules for copy-initialization of an
                    // object of type "cv1 T1" by user-defined conversion (8.5
                    // [dcl.init], 13.3.1.4 [over.match.copy]); the program is
                    // ill-formed if the corresponding non-reference
                    // copy-initialization would be ill-formed. The result of the
                    // call to the conversion function, as described for the
                    // non-reference copy-initialization, is then used to
                    // direct-initialize the reference. The program is ill-formed
                    // if the direct-initialization does not result in a direct
                    // binding or if it involves a user-defined conversion.
                    type_t* relevant_type = NULL;

                    if (symbol_entity_specs_get_is_conversion(constructor))
                    {
                        relevant_type = function_type_get_return_type(constructor->type_information);
                    }
                    else if (symbol_entity_specs_get_is_constructor(constructor))
                    {
                        // Note that this cannot happen with conversions because unresolved overloaded are not classes
                        if (is_unresolved_overloaded_type(orig))
                        {
                            scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(orig);
                            scope_entry_t* solved_function = address_of_overloaded_function(
                                    unresolved_set,
                                    unresolved_overloaded_type_get_explicit_template_arguments(orig),
                                    // first parameter of the constructor
                                    function_type_get_parameter_type_num(constructor->type_information, 0),
                                    decl_context,
                                    locus);
                            entry_list_free(unresolved_set);

                            if (solved_function != NULL)
                            {
                                if (!symbol_entity_specs_get_is_member(solved_function)
                                        || symbol_entity_specs_get_is_static(solved_function))
                                {
                                    orig = get_lvalue_reference_type(solved_function->type_information);
                                }
                                else
                                {
                                    orig = get_pointer_to_member_type(
                                            solved_function->type_information,
                                            symbol_entity_specs_get_class_type(solved_function));
                                }
                            }
                            else
                            {
                                internal_error("Code unreachable", 0);
                            }
                        }

                        relevant_type = symbol_entity_specs_get_class_type(constructor),
                                      get_cv_qualifier(no_ref(orig));

                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }

                    // Now verify if the resulting type can actually be
                    // directly-bound. Here we repeat some of the checks above
                    // but orig is now relevant_type
                    ok = 0;
                    if ((is_lvalue_reference_type(dest)
                                && is_const_qualified_type(no_ref(dest))
                                && !is_volatile_qualified_type(no_ref(dest)))
                            || (is_rvalue_reference_type(dest)))
                    {
                        if ((is_rvalue_reference_type(relevant_type)
                                    || is_class_type(relevant_type)
                                    || (is_lvalue_reference_type(relevant_type) && is_function_type(no_ref(relevant_type))))
                                && type_is_reference_compatible_to(no_ref(dest), no_ref(relevant_type)))
                        {
                            ok = 1;
                        }
                    }

                    if (!ok)
                        return 0;

                    *conversor = constructor;
                }
            }
            else
            {
                ok = solve_initialization_of_nonclass_nonreference_type(
                        orig,
                        get_unqualified_type(no_ref(dest)),
                        decl_context,
                        IK_COPY_INITIALIZATION | (initialization_kind & IK_NO_MORE_USER_DEFINED_CONVERSIONS),
                        conversor,
                        candidates,
                        locus);

                if (ok)
                {
                    if (is_unresolved_overloaded_type(orig))
                    {
                        scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(orig);
                        scope_entry_t* solved_function = address_of_overloaded_function(
                                unresolved_set,
                                unresolved_overloaded_type_get_explicit_template_arguments(orig),
                                dest,
                                decl_context,
                                locus);
                        entry_list_free(unresolved_set);

                        if (solved_function != NULL)
                        {
                            if (!symbol_entity_specs_get_is_member(solved_function)
                                    || symbol_entity_specs_get_is_static(solved_function))
                            {
                                orig = get_lvalue_reference_type(solved_function->type_information);
                            }
                            else
                            {
                                orig = get_pointer_to_member_type(
                                        solved_function->type_information,
                                        symbol_entity_specs_get_class_type(solved_function));
                            }
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }
                    }
                }
            }
            if (ok && (!type_is_reference_related_to(no_ref(dest), no_ref(orig))
                        // if is type reference related then dest must be more or equal cv-qualified
                        || (is_more_or_equal_cv_qualified_type(no_ref(dest), no_ref(get_unqualified_type(orig)))
                            // if is type reference related and what is being initialized is more cv-qualified
                            && (!is_rvalue_reference_type(dest)
                                // it is an rvalue reference, it cannot be initialized with an lvalue
                                || !is_lvalue_reference_type(orig)))))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: Initialization of reference type '%s' using a value of type '%s' "
                            "binding to a temporary succeeded\n",
                            print_declarator(dest),
                            print_declarator(orig));
                }
                if (!is_ambiguous)
                {
                    *ics = ics_make_user_defined_using_conversor(orig, dest, *conversor, locus);
                }
                else
                {
                    *ics = ics_make_ambiguous();
                }
                return 1;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: Initialization of reference type '%s' using a value of type '%s' "
                            "binding to a temporary failed\n",
                            print_declarator(dest),
                            print_declarator(orig));
                }
            }
        }
    }

    return 0;
}

static char solve_initialization_of_nonclass_type_ics(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context, 
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus,
        implicit_conversion_sequence_t* ics)
{
    *ics = invalid_ics;
    if (is_unresolved_overloaded_type(orig))
    {
        scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(orig);
        scope_entry_t* solved_function = address_of_overloaded_function(
                unresolved_set,
                unresolved_overloaded_type_get_explicit_template_arguments(orig),
                dest,
                decl_context,
                locus);
        entry_list_free(unresolved_set);

        if (solved_function != NULL)
        {
            if (!symbol_entity_specs_get_is_member(solved_function)
                    || symbol_entity_specs_get_is_static(solved_function))
            {
                orig = get_lvalue_reference_type(solved_function->type_information);
            }
            else
            {
                orig = get_pointer_to_member_type(
                        solved_function->type_information,
                        symbol_entity_specs_get_class_type(solved_function));
            }
        }
    }

    if (is_any_reference_type(dest))
    {
        return solve_initialization_of_reference_type_ics(
                orig,
                dest,
                decl_context, 
                initialization_kind,
                conversor,
                candidates,
                locus,
                ics);
    }
    else
    {
        return solve_initialization_of_nonclass_nonreference_type_ics(
                orig,
                dest,
                decl_context, 
                initialization_kind,
                conversor,
                candidates,
                locus,
                ics);
    }
}

char solve_initialization_of_nonclass_type(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context, 
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus)
{
    implicit_conversion_sequence_t dummy_ics;
    return solve_initialization_of_nonclass_type_ics(
            orig,
            dest,
            decl_context,
            initialization_kind,
            conversor,
            candidates,
            locus,
            &dummy_ics);
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

static char is_pointer_conversion(standard_conversion_item_t sci)
{
    return sci == SCI_POINTER_TO_VOID_CONVERSION
        || sci == SCI_ZERO_TO_POINTER_CONVERSION
        || sci == SCI_ZERO_TO_NULLPTR
        || sci == SCI_CLASS_POINTER_DERIVED_TO_BASE_CONVERSION
        || sci == SCI_NULLPTR_TO_POINTER_CONVERSION;
}

static standard_conversion_rank_t standard_conversion_get_rank(standard_conversion_t scs)
{
    standard_conversion_rank_t result = SCR_INVALID;

    if (scs.conv[0] == SCI_IDENTITY)
    {
        return SCR_EXACT_MATCH;
    }

    // Not a real conversion, used only for overload
    if (scs.conv[0] == SCI_DERIVED_TO_BASE)
    {
        return SCR_CONVERSION;
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
            || scs.conv[1] == SCI_FLOATING_PROMOTION
            || scs.conv[1] == SCI_FLOAT_TO_COMPLEX_PROMOTION
            || scs.conv[1] == SCI_COMPLEX_PROMOTION
            || scs.conv[1] == SCI_COMPLEX_FLOATING_INTEGRAL_CONVERSION
            || scs.conv[1] == SCI_FLOAT_TO_COMPLEX_CONVERSION
            || scs.conv[1] == SCI_COMPLEX_TO_FLOAT_CONVERSION
            || scs.conv[1] == SCI_COMPLEX_CONVERSION
            || scs.conv[1] == SCI_SCALAR_TO_VECTOR_CONVERSION)
    {
        result = SCR_PROMOTION;
    }

    if (scs.conv[1] == SCI_INTEGRAL_CONVERSION
            || scs.conv[1] == SCI_FLOATING_CONVERSION
            || scs.conv[1] == SCI_FLOATING_INTEGRAL_CONVERSION
            || scs.conv[1] == SCI_INTEGRAL_TO_COMPLEX_CONVERSION
            || scs.conv[1] == SCI_COMPLEX_TO_INTEGRAL_CONVERSION
            || scs.conv[1] == SCI_INTEGRAL_FLOATING_CONVERSION
            || is_pointer_conversion(scs.conv[1])
            || scs.conv[1] == SCI_POINTER_TO_MEMBER_BASE_TO_DERIVED_CONVERSION
            || scs.conv[1] == SCI_BOOLEAN_CONVERSION)
    {
        result = SCR_CONVERSION;
    }

    ERROR_CONDITION(result == SCR_INVALID, "This cannot be invalid here", 0);

    return result;
}

static char standard_conversion_is_pointer_to_bool(standard_conversion_t scs)
{
    return ((is_pointer_type(no_ref(scs.orig))
                || is_pointer_to_member_type(no_ref(scs.orig))
                // Array types may get here if we applied array to pointer
                || (is_array_type(no_ref(scs.orig)) && (scs.conv[0] == SCI_ARRAY_TO_POINTER)))
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

        // Intel vector extension: we favour extensions to the same types
        {
            int vector_size = 0; // unused
            // vector type -> vector type
            if (is_vector_type(no_ref(scs1.orig))
                    && is_vector_type(no_ref(scs1.dest))
                    // vector type -> intel vector struct
                    && is_vector_type(no_ref(scs2.orig))
                    && is_intel_vector_struct_type(no_ref(scs2.orig), &vector_size))
            {
                return 1;
            }

            // intel vector struct -> intel vector struct
            if (is_intel_vector_struct_type(no_ref(scs1.orig), &vector_size)
                    && is_intel_vector_struct_type(no_ref(scs1.dest), &vector_size)
                    // intel vector -> vector type
                    && is_intel_vector_struct_type(no_ref(scs2.orig), &vector_size)
                    && is_vector_type(no_ref(scs2.dest)))
            {
                return 1;
            }
        }

        /*
         * Some checks on "derivedness" and type kind are probably
         * rendundant below, but it is ok
         */
        if (equivalent_types(scs1.orig, scs2.orig))
            // Both SCSs have same source type
        {
            /*
             * GCC extension:
             *  A conversion from scalar arithmetic type to complex is worse than a
             *  conversion between scalar arithmetic types
             */
            if (is_arithmetic_type(scs1.orig)
                    && !is_complex_type(scs1.orig)

                    && is_arithmetic_type(no_ref(scs1.dest))
                    && !is_complex_type(no_ref(scs1.dest))

                    && is_complex_type(no_ref(scs2.dest))
                    )
            {
                return 1;
            }

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
            if (is_any_reference_type(scs1.orig)
                    && is_pointer_to_class_type(reference_type_get_referenced_type(scs1.orig)))
            {
                scs1.orig = reference_type_get_referenced_type(scs1.orig);
            }
            if (is_any_reference_type(scs2.orig)
                    && is_pointer_to_class_type(reference_type_get_referenced_type(scs2.orig)))
            {
                scs2.orig = reference_type_get_referenced_type(scs2.orig);
            }

            if (is_pointer_to_class_type(scs1.orig) // B* ->
                    && is_pointer_to_class_type(scs1.dest) // A*

                    && class_type_is_derived(
                        pointer_type_get_pointee_type(scs1.orig),
                        pointer_type_get_pointee_type(scs1.dest)) // B is derived from A

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
                    && class_type_is_derived(
                        pointer_type_get_pointee_type(scs1.orig),
                        pointer_type_get_pointee_type(scs1.dest)) // C is derived from B

                    /* && is_pointer_to_class_type(scs2.orig) */ // C* ->
                    && is_pointer_to_class_type(scs2.dest) // A*
                    /* && pointer_to_class_type_is_derived(scs2.orig, scs2.dest) */ // C is derived from A

                    && class_type_is_derived(
                        pointer_type_get_pointee_type(scs1.dest),
                        pointer_type_get_pointee_type(scs2.dest)) // B is derived from A
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
                    && is_any_reference_to_class_type(scs1.dest) // B&
                    && class_type_is_derived(no_ref(scs1.orig),
                        reference_type_get_referenced_type(scs1.dest)) // C derives from B&

                    && is_any_reference_to_class_type(scs2.dest) // A&
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

                    && class_type_is_derived(
                        pointer_type_get_pointee_type(scs2.orig),
                        pointer_type_get_pointee_type(scs1.orig)) // B is derived from A
               )
            {
                // If class B derives from A, conversion of A* to
                // void* is better than conversion from B* to void*
                return 1;
            }

            if (is_pointer_to_class_type(scs1.orig) // B* ->
                    && is_pointer_to_class_type(scs1.dest) // A*

                    && class_type_is_derived(
                        pointer_type_get_pointee_type(scs1.orig), 
                        pointer_type_get_pointee_type(scs1.dest)) // B is derived from A

                    && is_pointer_to_class_type(scs2.orig) // C* -> 
                    && is_pointer_to_class_type(scs2.dest) // A* 
                    /* && pointer_to_class_type_is_derived(scs2.orig, scs2.dest) */ // C is derived from A

                    && class_type_is_derived(
                        pointer_type_get_pointee_type(scs2.orig), 
                        pointer_type_get_pointee_type(scs1.orig)) // C is derived from B
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
                    && is_any_reference_to_class_type(scs1.dest) // A&
                    && class_type_is_derived(no_ref(scs1.orig), 
                        reference_type_get_referenced_type(scs1.dest)) // B& is derived from A&

                    && is_class_type(scs2.orig) // C&
                    /* && is_any_reference_to_class_type(scs2.dest) */ // A&
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
    if (scs1.conv[0] == scs2.conv[0]
            && is_pointer_type(scs1.dest)
            && is_pointer_type(scs2.dest)
            && pointer_types_are_similar(scs1.dest, scs2.dest))
    {
        // S1 and S2 differ only in their qualification conversion and yield similar types T1 and T2
        // respectively, and the cv-qualification signature of type T1 is a proper subset of the cv-qualification
        // signature of type T2
        if (is_more_cv_qualified_type(pointer_type_get_pointee_type(scs2.dest),
                    pointer_type_get_pointee_type(scs1.dest)))
        {
            return 1;
        }
    }
    else if ((scs1.conv[0] == scs2.conv[0])
            && (scs1.conv[1] == scs2.conv[1]))
    {
        // If both are reference bindings, and scs2 leads to the same type more qualified,
        // then scs1 is better than scs1
        if (is_any_reference_type(scs1.dest) 
                && is_any_reference_type(scs2.dest))
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
    // S1 and S2 are reference bindings and neither refers to an
    // implicit object parameter of a non-static member function
    // declared without a ref-qualifier, and S1 binds an rvalue
    // reference to an rvalue and S2 binds an lvalue reference.
    // FIXME - Not checking the implicit case
    else if (is_rvalue_reference_type(scs1.dest)
            && !is_lvalue_reference_type(scs1.orig) // a rvalue (either a non-reference or a rvalue reference)
            && is_lvalue_reference_type(scs2.dest))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Standard conversion SCS1 is better "
                    "than SCS2 because the first binds an rvalue to an rvalue reference and "
                    "the second binds to an lvalue reference\n");
        }
        return 1;
    }
    // S1 and S2 are reference bindings and S1 binds an lvalue
    // reference to a function lvalue and S2 binds an rvalue reference
    // to a function lvalue
    else if (is_lvalue_reference_type(scs1.orig)
            && is_function_type(no_ref(scs1.orig))
            && is_lvalue_reference_type(scs1.dest)

            && is_lvalue_reference_type(scs2.orig)
            && is_function_type(no_ref(scs2.orig))
            && is_rvalue_reference_type(scs2.dest))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Standard conversion SCS1 is better "
                    "than SCS2 because the first has better rank than the second\n");
        }
        return 1;
    }
    // Cases including "const int*/int*", "const int&/int&" and "const int&&/int&&"
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

    if (ics1.kind == ICSK_USER_DEFINED
            && ics2.kind == ICSK_ELLIPSIS)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: ICS1 is better than ICS2 because it is a "
                    "user-defined conversion against an ellipsis one\n");
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
            && ics2.kind == ICSK_USER_DEFINED
            && (ics_is_ambiguous(ics1)
                || ics_is_ambiguous(ics2)))
    {
        // Ambiguous ICS are always indistinguishable from other user-defined
        // conversions
        return 0;
    }
    else if (ics1.kind == ICSK_USER_DEFINED
            && ics2.kind == ICSK_USER_DEFINED)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "ICS: ICS1 and ICS2 are both user-defined conversions\n");
        }
        // User-defined conversion sequence U1 is a better conversion sequence
        // than another user-defined conversion sequence U2 if they contain
        // the same user-defined conversion function or constructor or they
        // initialize the same class in an aggregate initialization and in
        // either case the second standard conversion sequence of U1 is better
        // than the second standard conversion sequence of U2
        if ((((ics1.conversor == ics2.conversor) && ics1.conversor != NULL)
                    || (ics1.is_aggregate_ics
                        && ics2.is_aggregate_ics
                        && equivalent_types(ics1.first_sc.dest, ics2.first_sc.dest)))
                && standard_conversion_is_better(ics1.second_sc, ics2.second_sc))
            return 1;

        if (ics1.is_list_ics
                && ics2.is_list_ics)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "ICS: ICS1 and ICS2 are both (user-defined) list-conversion sequences\n");
            }
            scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(
                    CURRENT_COMPILED_FILE->global_decl_context,
                    make_locus("", 0, 0), /* mandatory */ 0);

            type_t* std_initializer_list_template_type = NULL;
            if (std_initializer_list_template != NULL)
                std_initializer_list_template_type = std_initializer_list_template->type_information;

            // ics1 converts to std::initializer_list<X> for some X and ics2 does not
            if (std_initializer_list_template_type != NULL
                    && is_template_specialized_type(ics1.first_sc.dest)
                    && equivalent_types(
                        template_specialized_type_get_related_template_type(ics1.first_sc.dest),
                        std_initializer_list_template_type)
                    && !(is_template_specialized_type(ics2.first_sc.dest)
                        || !equivalent_types(
                            template_specialized_type_get_related_template_type(ics2.first_sc.dest),
                            std_initializer_list_template_type)))
            {
                return 1;
            }
            // or if not that ics1 converts to type "array of N1 T", ics2 converts to type "array of N2 T", and N1 is smaller than N2
            else if (is_array_type(ics1.first_sc.dest)
                    && is_array_type(ics2.first_sc.dest)
                    && nodecl_is_constant(array_type_get_array_size_expr(ics1.first_sc.dest))
                    && nodecl_is_constant(array_type_get_array_size_expr(ics2.first_sc.dest))
                    && (const_value_cast_to_signed_int(
                            nodecl_get_constant(array_type_get_array_size_expr(ics1.first_sc.dest)))
                        <
                        const_value_cast_to_signed_int(
                            nodecl_get_constant(array_type_get_array_size_expr(ics2.first_sc.dest)))))
            {
                return 1;
            }
        }
    }
    else if (ics1.kind == ICSK_USER_DEFINED
            && ics2.kind == ICSK_USER_DEFINED
            // Both are list ICS
            && ics1.is_list_ics
            && ics2.is_list_ics)
    {
    }

    return 0;
}

static char can_be_called_with_number_of_arguments_ovl(scope_entry_t* entry, int num_arguments)
{
    if (symbol_entity_specs_get_is_member(entry)
            && !symbol_entity_specs_get_is_constructor(entry))
        num_arguments--;
    return can_be_called_with_number_of_arguments(entry, num_arguments);
}

static overload_entry_list_t* compute_viable_functions(
        candidate_t* candidate_functions,
        const decl_context_t* decl_context,
        char no_user_defined_conversions,
        const locus_t* locus)
{
    overload_entry_list_t *result = NULL;

    for (candidate_t* it = candidate_functions;
            it != NULL;
            it = it->next)
    {
        scope_entry_t* orig_candidate = it->entry;
        int num_arguments = it->num_args;
        type_t** argument_types = it->args;

        scope_entry_t* candidate = entry_advance_aliases(orig_candidate);

        if (is_error_type(candidate->type_information))
            continue;

        ERROR_CONDITION(!is_function_type(candidate->type_information),
                "This is not a function", 0);

        if (can_be_called_with_number_of_arguments_ovl(candidate, num_arguments))
        {
            implicit_conversion_sequence_t *ics_arguments = NULL;
            int num_ics_arguments = 0;

            int num_parameters =
                function_type_get_num_parameters(candidate->type_information);
            if (function_type_get_has_ellipsis(candidate->type_information))
                num_parameters--;

            char still_viable = 1;
            int i = 0;
            char requires_ambiguous_conversion = 0;

            // Static members have their implicit argument simply ignored
            if (symbol_entity_specs_get_is_member(candidate)
                    && symbol_entity_specs_get_is_static(candidate))
            {
                P_LIST_ADD(ics_arguments, num_ics_arguments, invalid_ics);
                i = 1;
            }

            for (; (i < num_arguments) && still_viable; i++)
            {
                int argument_number = i;

                implicit_conversion_sequence_t ics_to_candidate;
                if (i == 0
                        && symbol_entity_specs_get_is_member(candidate)
                        && !symbol_entity_specs_get_is_constructor(candidate))
                {
                    if (argument_types[0] != NULL)
                    {
                        // Set 'this' parameter (not argument!)
                        type_t* member_object_type = NULL;

                        // Using entities use the class where they are used, not
                        // their original type
                        if (orig_candidate->kind == SK_USING)
                        {
                            member_object_type = symbol_entity_specs_get_class_type(orig_candidate);
                        }
                        else
                        {
                            member_object_type = symbol_entity_specs_get_class_type(candidate);
                        }

                        ref_qualifier_t ref_qualifier =
                            function_type_get_ref_qualifier(candidate->type_information);

                        member_object_type = get_cv_qualified_type(member_object_type,
                                get_cv_qualifier(candidate->type_information));

                        if (ref_qualifier == REF_QUALIFIER_NONE
                                || ref_qualifier == REF_QUALIFIER_LVALUE)
                        {
                            member_object_type = get_lvalue_reference_type(member_object_type);
                        }
                        else if (ref_qualifier == REF_QUALIFIER_RVALUE)
                        {
                            member_object_type = get_rvalue_reference_type(member_object_type);
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }

                        compute_ics_flags(argument_types[0],
                                member_object_type,
                                decl_context,
                                &ics_to_candidate,
                                no_user_defined_conversions,
                                /* is_implicit_argument */ 1,
                                /* needs_contextual_conversion */ 0,
                                ref_qualifier,
                                locus);
                    }
                    else
                    {
                        // Make sure we mark it is as invalid, lest we attempt anything with it
                        P_LIST_ADD(ics_arguments, num_ics_arguments, invalid_ics);
                        continue;
                    }
                }
                else
                {
                    // The implicit is not counted in the function type, so skew it
                    if (symbol_entity_specs_get_is_member(candidate)
                            && !symbol_entity_specs_get_is_constructor(candidate))
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

                    char needs_contextual_conversion =
                        builtin_needs_contextual_conversion(candidate,
                                argument_number,
                                parameter_type);

                    compute_ics_flags(argument_types[i],
                            parameter_type,
                            decl_context,
                            &ics_to_candidate,
                            no_user_defined_conversions,
                            /* is_implicit_argument */ 0,
                            needs_contextual_conversion,
                            REF_QUALIFIER_NONE,
                            locus);
                }

                if (ics_to_candidate.kind == ICSK_INVALID)
                {
                    still_viable = 0;
                }
                else
                {
                    requires_ambiguous_conversion |= ics_is_ambiguous(ics_to_candidate);
                    P_LIST_ADD(ics_arguments, num_ics_arguments, ics_to_candidate);
                }
            }

            if (still_viable)
            {
                overload_entry_list_t* new_result = NEW0(overload_entry_list_t);
                new_result->candidate = it;
                new_result->next = result;
                new_result->requires_ambiguous_ics = requires_ambiguous_conversion;
                new_result->num_ics_arguments = num_ics_arguments;
                new_result->ics_arguments = ics_arguments;
                result = new_result;
            }
            else
            {
                DELETE(ics_arguments);
            }
        }
    }

    return result;
}

static char is_better_function_despite_equal_ics(
        scope_entry_t* f,
        scope_entry_t* g,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // flags
        int num_arguments)
{
    if (!is_template_specialized_type(f->type_information)
            && is_template_specialized_type(g->type_information))
    {
        // 13.3.3 [over.match.best]
        // F1 is not a function template specialization and F2 is a function template specialization, 
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Found that [%s, %s] IS better than [%s, %s] because "
                    "the first is not a template-specialization and the second is\n",
                    print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                    locus_to_str(f->locus),
                    print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                    locus_to_str(g->locus));
        }
        return 1;
    }

    if (is_template_specialized_type(f->type_information)
            && is_template_specialized_type(g->type_information))
    {
        // 13.3.3 [over.match.best]
        // or, if not that,
        // F1 and F2 are function template specializations, and the function template for F1 is more specialized
        //     than the template for F2 according to the partial ordering rules described in 14.5.6.2.
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Found that [%s, %s] and [%s, %s] are template functions "
                    "so we have to check which one is more specialized\n",
                    print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                    locus_to_str(f->locus),
                    print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                    locus_to_str(g->locus));
        }

        if (is_more_specialized_template_function_in_overload(
                    // Why is it so convoluted to get the type of the primary specialization ?
                    named_type_get_symbol(template_type_get_primary_type(
                            template_specialized_type_get_related_template_type(f->type_information))),
                    named_type_get_symbol(template_type_get_primary_type(
                            template_specialized_type_get_related_template_type(g->type_information))), 
                    decl_context,
                    // TODO - Should we pass them?
                    /* explicit_template_arguments */ NULL,
                    locus,
                    // Flags
                    num_arguments,
                    symbol_entity_specs_get_is_conversion(f)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Found that template-function [%s, %s] is more "
                        "specialized than template-function [%s, %s]\n",
                        print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                        locus_to_str(f->locus),
                        print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                        locus_to_str(g->locus));
            }
            return 1;
        }
    }

    return 0;
}

static char is_same_kind_of_reference(type_t* t1, type_t* t2)
{
    return (is_lvalue_reference_type(t1)
            && is_lvalue_reference_type(t2))
        || (is_rvalue_reference_type(t1)
                && is_rvalue_reference_type(t2));
}

// States whether f is better than g
static
char is_better_function(
        overload_entry_list_t* ovl_f,
        overload_entry_list_t* ovl_g,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        type_t* dest,
        const locus_t* locus)
{
    scope_entry_t* f = ovl_f->candidate->entry;
    scope_entry_t* g = ovl_g->candidate->entry;

    // A function is better if all ICS are equal or best, so if any is not
    // better or equal, then it is not better
    //
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Checking if [%s, %s] is better than [%s, %s]\n",
                print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                locus_to_str(f->locus),
                print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                locus_to_str(g->locus));
    }

    int first_type = 0;

    if (symbol_entity_specs_get_is_member(f)
            && symbol_entity_specs_get_is_member(g)
            && symbol_entity_specs_get_is_static(f))
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
        // 13.3.3 [over.match.best]
        // A viable function F1 is defined to be a better function than another viable function
        // F2 if for all arguments i, ICSi(F1) is not a worse conversion sequence than ICSi(F2), and then
        if (some_is_better)
        {
            // 13.3.3 [over.match.best]
            // for some argument j, ICSj(F1) is a better conversion sequence than ICSj(F2), or, if not that,
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Found that [%s, %s] IS better"
                        " than [%s, %s] because some argument in the first"
                        " function has a better ICS than the respective one in the second\n",
                        print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                        locus_to_str(f->locus),
                        print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                        locus_to_str(g->locus));
            }
            return 1;
        }

        if ((initialization_kind & IK_BY_USER_DEFINED_CONVERSION)
                && function_type_get_return_type(f->type_information) != NULL
                && function_type_get_return_type(g->type_information) != NULL)
        {
            ERROR_CONDITION(dest == NULL, "No destination type", 0);
            standard_conversion_t scs1;
            standard_conversion_between_types_for_overload(
                    &scs1,
                    function_type_get_return_type(f->type_information),
                    dest,
                    locus);
            standard_conversion_t scs2;
            standard_conversion_between_types_for_overload(
                    &scs2,
                    function_type_get_return_type(g->type_information),
                    dest,
                    locus);

            if (!standard_conversion_is_invalid(scs1)
                    && !standard_conversion_is_invalid(scs2)
                    && standard_conversion_is_better(scs1, scs2))
            {
                // 13.3.3 [over.match.best]
                // the context is an initialization by user-defined conversion (see 8.5, 13.3.1.5, and 13.3.1.6) and the
                // standard conversion sequence from the return type of F1 to the destination type (i.e., the type of the
                // entity being initialized) is a better conversion sequence than the standard conversion sequence from
                // the return type of F2 to the destination type
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: In the context of a user directed conversion, "
                            "the target of the standard conversion of ICS1 is better than ICS2, "
                            " so [%s, %s] is better than [%s, %s]\n",
                            print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                            locus_to_str(f->locus),
                            print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                            locus_to_str(g->locus));
                }
                return 1;
            }
        }

        if ((initialization_kind & IK_BY_DIRECT_REFERENCE_BINDING)
                && function_type_get_return_type(f->type_information) != NULL
                && function_type_get_return_type(g->type_information) != NULL
                && is_any_reference_type(dest)
                && is_function_type(no_ref(dest)))
        {
            type_t* return_f = function_type_get_return_type(f->type_information);
            type_t* return_g = function_type_get_return_type(g->type_information);

            if (is_same_kind_of_reference(dest, return_f)
                    && !is_same_kind_of_reference(dest, return_g))
            {
                // 13.3.3 [over.match.best]
                // the context is an initialization by conversion function for
                // direct reference binding (13.3.1.6) of a reference to
                // function type, the return type of F1 is the same kind of
                // reference (i.e. lvalue or rvalue) as the reference being
                // initialized, and the return type of F2 is not
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: In the context of a direct reference binding to a "
                            "function reference, the target of the standard conversion of ICS1 is better than ICS2, "
                            " so [%s, %s] is better than [%s, %s]\n",
                            print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                            locus_to_str(f->locus),
                            print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                            locus_to_str(g->locus));
                }
                return 1;
            }
        }

        int num_arguments_of_call = ovl_f->candidate->num_args;
        if (symbol_entity_specs_get_is_member(f)
                && !symbol_entity_specs_get_is_constructor(f))
            num_arguments_of_call--;

        if (is_better_function_despite_equal_ics(f, g, decl_context, locus, num_arguments_of_call))
            return 1;
    }

    // It is not better (it might be equally good, though)
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Found that [%s, %s] is NOT better than [%s, %s]\n",
                print_decl_type_str(f->type_information, f->decl_context, f->symbol_name),
                locus_to_str(f->locus),
                print_decl_type_str(g->type_information, g->decl_context, g->symbol_name),
                locus_to_str(g->locus));
    }
    return 0;
}

/*
 * num_arguments includes the implicit argument so it should never be zero, at least 1
 */
static scope_entry_t* solve_overload_(candidate_t* candidate_set,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        type_t* dest,
        const locus_t* locus,
        // Out
        char *is_ambiguous)
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
                fprintf(stderr, "OVERLOAD: Candidate %d: %s, %s [%s] %s\n",
                        i,
                        entry->symbol_name,
                        locus_to_str(entry->locus),
                        print_declarator(entry->type_information),
                        (symbol_entity_specs_get_is_builtin(entry) ? "<builtin function>" : ""));

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
                                && symbol_entity_specs_get_is_member(entry)
                                && !symbol_entity_specs_get_is_constructor(entry))
                        {
                            fprintf(stderr, "[[implicit argument]] ");
                        }
                        if (it->args[j] != NULL)
                        {
                            fprintf(stderr, "%s", print_declarator(it->args[j]) );
                        }
                        else
                        {
                            fprintf(stderr, "<<NULL>>");
                        }
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
            decl_context, !!(initialization_kind & IK_NO_MORE_USER_DEFINED_CONVERSIONS), locus);

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
                fprintf(stderr, "OVERLOAD:    %s: %s %s\n",
                        locus_to_str(entry->locus),
                        print_decl_type_str(
                            entry->type_information,
                            entry->decl_context,
                            entry->symbol_name),
                        symbol_entity_specs_get_is_builtin(entry) ? "<builtin>" : "");

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
                    decl_context,
                    initialization_kind,
                    dest,
                    locus))
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
        
        scope_entry_t* sym_current = entry_advance_aliases(current->candidate->entry);
        scope_entry_t* sym_best_viable = entry_advance_aliases(best_viable->candidate->entry);

        // Do not compare to the same symbol
        if (sym_best_viable != sym_current)
        {
            if (!is_better_function(best_viable, current, 
                        decl_context,
                        initialization_kind,
                        dest,
                        locus))
            {
                DEBUG_CODE()
                {
                    scope_entry_t* entry = entry_advance_aliases(current->candidate->entry);
                    fprintf(stderr, "OVERLOAD: Current function '%s' is better than the one we thought it was the best\n",
                            print_decl_type_str(
                                entry->type_information,
                                entry->decl_context,
                                entry->symbol_name));
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
            fprintf(stderr, "OVERLOAD: Best function so far '%s' is ambiguous\n",
                    print_decl_type_str(entry->type_information,
                        entry->decl_context,
                        entry->symbol_name
                       ));
            fprintf(stderr, "OVERLOAD: There is no best function\n");
        }
        *is_ambiguous = 1;
        return NULL;
    }
    else
    {
        if (best_viable->requires_ambiguous_ics)
        {
            DEBUG_CODE()
            {
                scope_entry_t* entry = entry_advance_aliases(best_viable->candidate->entry);
                fprintf(stderr, "OVERLOAD: Call to '%s' requires ambiguous conversion\n",
                        print_decl_type_str(
                            entry->type_information,
                            entry->decl_context,
                            entry->symbol_name ));
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

    // Note that this function effectively returns SK_FUNCTION or SK_USING.
    // It is up to the caller to advance the alias and not to lose track
    // of the SK_USING
    scope_entry_t* best_viable_function = best_viable->candidate->entry;

    // Cleanup: overload_entry_list_t is an incredibly big structure
    // because of a big array of MCXX_MAX_FUNCTION_CALL_ARGUMENTS
    it = viable_functions;
    while (it != NULL)
    {
        overload_entry_list_t* next = it->next;
        DELETE(it->ics_arguments);
        DELETE(it);
        it = next;
    }

    return best_viable_function;
}

scope_entry_t* solve_overload(candidate_t* candidate_set,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    char is_ambiguous = 0; // Unused
    return solve_overload_(candidate_set,
            decl_context,
            /* initialization_kind */ IK_INVALID,
            /* dest */ NULL,
            locus,
            // Out
            &is_ambiguous);
}

scope_entry_t* address_of_overloaded_function(
        scope_entry_list_t* overload_set,
        template_parameter_list_t* explicit_template_arguments,
        type_t* target_type,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Solving the address of overload function-name '%s' at '%s'\n",
                /* use the first in the set */
                overload_set != NULL ? entry_list_head(overload_set)->symbol_name : "<overload set empty>",
                locus_to_str(locus));
    }

    // Check sanity of the target type
    if (!is_pointer_type(target_type)
            && !is_pointer_to_member_type(target_type)
            && !is_lvalue_reference_type(target_type)
            && !is_rvalue_reference_type(target_type)
            && !is_function_type(target_type))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Type '%s' is not a valid function type\n", print_declarator(target_type));
        }
        return NULL;
    }

    type_t* functional_type = NULL;
    // type_t *class_type = NULL;

    functional_type = no_ref(target_type);
    if (is_pointer_to_function_type(functional_type))
    {
        functional_type = pointer_type_get_pointee_type(functional_type);
    }
    else if (is_pointer_to_member_type(functional_type))
    {
        // class_type = pointer_to_member_type_get_class_type(functional_type);
        functional_type = pointer_type_get_pointee_type(functional_type);
    }
    else if (is_function_type(functional_type))
    {
        // functional_type = functional_type;
    }

    if (!is_function_type(functional_type))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Type '%s' is not a valid function type\n", print_declarator(target_type));
        }
        return NULL;
    }

    char there_are_templates = 0;
    char there_are_specializations = 0;
    char there_are_non_templates = 0;

    scope_entry_list_t* potential_valid = NULL;

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_fun = entry_advance_aliases(entry_list_iterator_current(it));

        scope_entry_t* considered_function = NULL;

        if (current_fun->kind == SK_FUNCTION)
        {
            there_are_non_templates = 1;
            if (equivalent_types(current_fun->type_information, functional_type))
            {
                considered_function = current_fun;
            }
        }
        else if (current_fun->kind == SK_TEMPLATE)
        {
            there_are_templates = 1;

            scope_entry_t* primary_symbol =
                named_type_get_symbol(
                        template_type_get_primary_type(current_fun->type_information));

            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Deducing arguments of function '%s' against target type '%s'\n",
                        print_declarator(primary_symbol->type_information),
                        print_declarator(functional_type));
            }

            template_parameter_list_t* deduced_template_arguments = NULL;
            deduction_result_t deduction_result =
                deduce_template_arguments_from_address_of_a_function_template(
                        functional_type,
                        primary_symbol->type_information,
                        template_specialized_type_get_template_parameters(
                            primary_symbol->type_information),
                        template_type_get_template_parameters(
                            current_fun->type_information),
                        explicit_template_arguments,
                        decl_context,
                        locus,
                        &deduced_template_arguments);

            if (deduction_result == DEDUCTION_OK)
            {
                type_t* named_specialization_type = template_type_get_specialized_type(
                        current_fun->type_information,
                        deduced_template_arguments,
                        decl_context,
                        locus);
                free_template_parameter_list(deduced_template_arguments);

                if (named_specialization_type != NULL)
                {
                    considered_function = named_type_get_symbol(named_specialization_type);
                }
            }
            DEBUG_CODE()
            {
                if (considered_function == NULL)
                {
                    fprintf(stderr, "OVERLOAD: Failure when deducing arguments of function '%s' against target type '%s'\n",
                            print_declarator(primary_symbol->type_information),
                            print_declarator(functional_type));
                }
                else
                {
                    fprintf(stderr, "OVERLOAD: Deduction yields function '%s' against target type '%s'\n",
                            print_declarator(considered_function->type_information),
                            print_declarator(functional_type));
                }
            }

            if (considered_function != NULL)
                there_are_specializations = 1;
        }

        if (considered_function == NULL)
            continue;

        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Checking with function '%s' against target type '%s'\n",
                    print_declarator(considered_function->type_information),
                    print_declarator(functional_type));
        }

        // Now check feasibility
        if (((!symbol_entity_specs_get_is_member(considered_function)
                        || symbol_entity_specs_get_is_static(considered_function))
                    && (is_pointer_to_function_type(target_type)
                        || is_function_type(no_ref(target_type))))
                || (symbol_entity_specs_get_is_member(considered_function)
                    && !symbol_entity_specs_get_is_static(considered_function)
                    && is_pointer_to_member_type(no_ref(target_type))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Function '%s' DOES match target type '%s'\n",
                        print_declarator(considered_function->type_information),
                        print_declarator(target_type));
            }
            // non-members and static data members match functions and pointers to functions
            potential_valid = entry_list_add(potential_valid, considered_function);
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "OVERLOAD: Function '%s' DOES NOT match target type '%s'\n",
                        print_declarator(considered_function->type_information),
                        print_declarator(target_type));
            }
        }
    }
    entry_list_iterator_free(it);

    if (explicit_template_arguments != NULL
            && !there_are_templates)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: Failure because there are explicit template arguments but no template-names in the overload-set\n");
        }
        entry_list_free(potential_valid);
        return NULL;
    }

    if (entry_list_size(potential_valid) == 1)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "OVERLOAD: There is only a single match '%s'\n",
                    print_declarator(entry_list_head(potential_valid)->type_information));
        }
        scope_entry_t* result = entry_list_head(potential_valid);
        entry_list_free(potential_valid);
        return result;
    }
    else if (entry_list_size(potential_valid) > 1)
    {
        if (there_are_non_templates)
        {
            if (there_are_specializations)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: Filtering templates from potential list set\n");
                }
                scope_entry_list_t* nontemplates = NULL;
                for (it = entry_list_iterator_begin(potential_valid);
                        !entry_list_iterator_end(it);
                        entry_list_iterator_next(it))
                {
                    scope_entry_t* current_fun = entry_list_iterator_current(it);
                    if (!is_template_specialized_type(current_fun->type_information))
                    {
                        nontemplates = entry_list_add(nontemplates, current_fun);
                    }
                }
                entry_list_iterator_free(it);
                entry_list_free(potential_valid);

                potential_valid = nontemplates;
            }

            if (entry_list_size(potential_valid) != 1)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: More than one non-template matches\n");
                }
                entry_list_free(potential_valid);
                return NULL;
            }
            else
            {
                scope_entry_t* result = entry_list_head(potential_valid);
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: Only one nontemplate matches\n");
                }
                entry_list_free(potential_valid);
                return result;
            }
        }
        else
        {
            // All remaining functions are template-specialized, order them
            scope_entry_t* more_specialized = entry_list_head(potential_valid);
            for (it = entry_list_iterator_begin(potential_valid);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current_fun = entry_list_iterator_current(it);
                if (current_fun == more_specialized)
                    continue;

                if (is_more_specialized_template_function_in_function_address(
                            current_fun,
                            more_specialized,
                            decl_context,
                            // TODO: Should we pass them?
                            /* explicit_template_arguments */ NULL,
                            locus,
                            /* is_conversion */ symbol_entity_specs_get_is_conversion(current_fun)))
                {
                    more_specialized = current_fun;
                }
            }
            entry_list_iterator_free(it);

            // Check
            for (it = entry_list_iterator_begin(potential_valid);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current_fun = entry_list_iterator_current(it);
                if (current_fun == more_specialized)
                    continue;

                if (is_more_specialized_template_function_in_function_address(
                            current_fun,
                            more_specialized,
                            decl_context,
                            // TODO: Should we pass them?
                            /* explicit_template_arguments */ NULL,
                            locus,
                            /* is_conversion */ symbol_entity_specs_get_is_conversion(current_fun)))
                {
                    more_specialized = NULL;
                    break;
                }
            }
            entry_list_iterator_free(it);
            entry_list_free(potential_valid);

            if (more_specialized != NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "OVERLOAD: There is a more specialized function\n");
                }
                return more_specialized;
            }
        }
    }

    scope_entry_t* fallback = unresolved_overloaded_type_simplify_unpacked(
            overload_set,
            explicit_template_arguments,
            decl_context,
            locus);
    if (fallback != NULL)
    {
        return fallback;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Failure when trying to determine the address of a function\n");
    }
    return NULL;
}

static scope_entry_list_t* constructor_candidates_initialization_of_class_type(
        type_t* dest,
        const decl_context_t* decl_context,
        enum initialization_kind initialization_kind,
        char init_constructors_only,
        const locus_t* locus)
{
    // 13.3.1.3 [over.match.ctor]
    // When objects of class type are direct-initialized (8.5), or copy-initialized from an expression of the same or
    // a derived class type (8.5), overload resolution selects the constructor.

    // 13.3.1.4 [over.match.copy]
    // Under the conditions specified in 8.5, as part of a copy-initialization of an object of class type, a user-defined
    // conversion can be invoked to convert an initializer expression to the type of the object being initialized.
    // Overload resolution is used to select the user-defined conversion to be invoked.
    scope_entry_list_t* candidate_list = NULL;

    scope_entry_list_t* constructors = class_type_get_constructors(get_actual_class_type(dest));
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
    {
        scope_entry_t* constructor
            = entry_list_iterator_current(it);

        // 13.3.1.3 [over.match.ctor]
        // For direct-initialization, the candidate functions are all the
        // constructors of the class of the object being initialized.
        if ((initialization_kind & IK_COPY_INITIALIZATION)
                && symbol_entity_specs_get_is_explicit(constructor))
            continue;

        // 13.3.1.3 [over.match.ctor]
        // For copy-initialization, the candidate functions are all the converting
        // constructors (12.3.1) of that class.  The argument list is the
        // expression-list or assignment-expression of the initializer.
        if ((initialization_kind & IK_COPY_INITIALIZATION)
                && (initialization_kind & IK_BY_CONSTRUCTOR)
                && !symbol_entity_specs_get_is_conversor_constructor(constructor))
            continue;

        // 13.3.1.4 [over.match.copy]
        // - The converting constructors (12.3.1) of T are candidate functions.
        if ((initialization_kind & IK_COPY_INITIALIZATION)
                && (initialization_kind & IK_BY_USER_DEFINED_CONVERSION)
                && !symbol_entity_specs_get_is_conversor_constructor(constructor))
            continue;

        // Filter init constructors only
        if (init_constructors_only)
        {
            scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(decl_context, locus, /* mandatory */ 1);

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

        candidate_list = entry_list_add(candidate_list, constructor);
    }
    entry_list_iterator_free(it);
    entry_list_free(constructors);

    return candidate_list;
}

static scope_entry_t* solve_constructor_(type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        enum initialization_kind initialization_kind,
        const decl_context_t* decl_context,
        char init_constructors_only,
        const locus_t* locus,
        // Output arguments
        scope_entry_list_t** candidates,
        char *is_ambiguous)
{
    // 13.3.1.3 [over.match.ctor]
    // 13.3.1.4 [over.match.copy]
    ERROR_CONDITION(!is_named_class_type(class_type), "This is not a named class type", 0);

    class_type_complete_if_possible(named_type_get_symbol(class_type), decl_context, locus);

    // 13.3.1.3 [over.match.ctor]
    // 13.3.1.4 [over.match.copy]
    scope_entry_list_t* candidate_list =
        constructor_candidates_initialization_of_class_type(
                class_type,
                decl_context,
                initialization_kind,
                init_constructors_only,
                locus);

    if (((initialization_kind & IK_COPY_INITIALIZATION)
                || (initialization_kind & IK_DIRECT_INITIALIZATION))
            && (initialization_kind & IK_BY_USER_DEFINED_CONVERSION)
            && !init_constructors_only)
    {
        // 13.3.1.4 [over.match.copy]
        if (num_arguments == 1
                && is_class_type(no_ref(argument_types[0])))
        {
            candidate_list = entry_list_concat(candidate_list,
                    conversion_function_candidates_initialization_of_class_type(
                        no_ref(argument_types[0]),
                        class_type,
                        decl_context,
                        initialization_kind,
                        locus));
        }

    }

    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(candidate_list,
            NULL, argument_types, num_arguments,
            decl_context,
            locus, /* explicit_template_arguments */ NULL);
    entry_list_free(candidate_list);

    candidate_t* candidate_set = NULL;
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        candidate_set = candidate_set_add(candidate_set,
                entry_list_iterator_current(it),
                num_arguments,
                argument_types);
    }
    entry_list_iterator_free(it);

    // Store the candidates here
    *candidates = overload_set;

    // Now we have all the constructors, perform an overload resolution on them
    scope_entry_t* overload_resolution = solve_overload_(candidate_set,
            decl_context,
            initialization_kind,
            class_type,
            locus,
            // Out
            is_ambiguous);
    candidate_set_free(&candidate_set);

    return overload_resolution;
}

static char solve_initialization_of_class_type_(
        type_t* class_type,
        type_t** argument_types,
        int num_arguments,
        enum initialization_kind initialization_kind,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // Out
        scope_entry_t** constructor,
        scope_entry_list_t** candidates,
        char *is_ambiguous)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "OVERLOAD: Solving initialization of class type '%s' ",
                print_declarator(class_type));
        if (num_arguments > 0)
        {
            fprintf(stderr, "using %d types with argument types:\n",
                    num_arguments);
            int i;
            for (i = 0; i < num_arguments; i++)
            {
                fprintf(stderr, "OVERLOAD:    Argument %d: %s\n", i, print_declarator(argument_types[i]));
            }
            fprintf(stderr, "OVERLOAD: No more argument types\n");
        }
        else
        {
            fprintf(stderr, "using 0 types as arguments\n");
        }
    }
    *constructor = solve_constructor_(class_type,
            argument_types,
            num_arguments,
            initialization_kind,
            decl_context,
            /* init_constructors_only */ 0,
            locus,
            // Out
            candidates,
            is_ambiguous);

    DEBUG_CODE()
    {
        if (*constructor != NULL)
        {
            fprintf(stderr, "OVERLOAD: Solving initialization of class type '%s' succeeded using constructor '%s' at '%s'\n",
                    print_declarator(class_type),
                    get_qualified_symbol_name(*constructor, (*constructor)->decl_context),
                    locus_to_str((*constructor)->locus));
        }
        else
        {
            fprintf(stderr, "OVERLOAD: Solving initialization of class type '%s' failed\n",
                    print_declarator(class_type));
        }
    }

    return (*constructor != NULL);
}

char solve_initialization_of_class_type(
        type_t* class_type,
        type_t** argument_types,
        int num_arguments,
        enum initialization_kind initialization_kind,
        const decl_context_t* decl_context,
        const locus_t* locus,
        scope_entry_t** constructor,
        scope_entry_list_t** candidates)
{
    char is_ambiguous = 0;
    return solve_initialization_of_class_type_(
            class_type,
            argument_types,
            num_arguments,
            initialization_kind,
            decl_context,
            locus,
            // Out
            constructor,
            candidates,
            &is_ambiguous);
}

static char solve_list_initialization_of_class_type_(
        type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        enum initialization_kind initialization_kind,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // Out
        scope_entry_t** constructor,
        scope_entry_list_t** candidates,
        char *is_ambiguous)
{
    scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(
            decl_context, 
            locus,
            /* mandatory */ 0);

    // 13.3.1.7 [over.match.list]
    //
    // When objects of non-aggregate class type T are list-initialized (8.5.4), overload resolution selects the con-
    // structor in two phases:
    // — Initially, the candidate functions are the initializer-list constructors (8.5.4) of the class T and the
    // argument list consists of the initializer list as a single argument.
    // — If no viable initializer-list constructor is found, overload resolution is performed again, where the
    // candidate functions are all the constructors of the class T and the argument list consists of the elements
    // of the initializer list.

    // If the initializer list has no elements and T has a default constructor, the first phase is omitted.
    char omit_first_phase = 0;
    if (num_arguments == 0
            && class_type_get_default_constructor(class_type) != NULL)
    {
        omit_first_phase = 1;
    }

    scope_entry_list_t* candidate_list = NULL;
    scope_entry_list_t* all_constructors = class_type_get_constructors(get_actual_class_type(class_type));

    scope_entry_list_iterator_t* it = NULL;
    scope_entry_list_t* list_initializer_constructors = NULL;

    *candidates = NULL;

    if (!omit_first_phase
            // If std::initializer_list is not available there should not be
            // any initializer-list-constructor
            && std_initializer_list_template != NULL)
    {
        for (it = entry_list_iterator_begin(all_constructors);
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
                    if (is_template_specialized_type(entry->type_information))
                    {
                        type_t* template_type =
                            template_specialized_type_get_related_template_type(
                                    entry->type_information);
                        entry = template_type_get_related_symbol(template_type);
                    }
                    list_initializer_constructors = entry_list_add(list_initializer_constructors,
                            entry);
                }
            }
        }
        entry_list_iterator_free(it);


        type_t* braced_list_type = get_braced_list_type(num_arguments, argument_types);

        scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(list_initializer_constructors,
                NULL, &braced_list_type, 1,
                decl_context,
                locus, /* explicit_template_arguments */ NULL);
        entry_list_free(list_initializer_constructors);

        candidate_t* candidate_set = NULL;
        for (it = entry_list_iterator_begin(overload_set);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            candidate_set = candidate_set_add(candidate_set,
                    entry_list_iterator_current(it),
                    1,
                    &braced_list_type);
        }
        entry_list_iterator_free(it);

        *candidates = entry_list_concat(*candidates, overload_set);

        // Now we have all the constructors, perform an overload resolution on them
        scope_entry_t* overload_resolution = solve_overload_(candidate_set,
                decl_context,
                initialization_kind,
                class_type,
                locus,
                // Out
                is_ambiguous);
        candidate_set_free(&candidate_set);

        *constructor = overload_resolution;

        if (overload_resolution != NULL)
        {
            // In copy-list-initialization, if an explicit constructor is
            // chosen, the initialization is ill-formed.
            if ((initialization_kind & IK_COPY_INITIALIZATION)
                    && symbol_entity_specs_get_is_explicit(overload_resolution))
            {
                *constructor = 0;
                return 0;
            }

            // We are done
            return 1;
        }
    }

    for (it = entry_list_iterator_begin(all_constructors);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_constructor
            = entry_list_iterator_current(it);

        // For template specialized types, use the template symbol
        if (is_template_specialized_type(current_constructor->type_information))
        {
            type_t* template_type =
                template_specialized_type_get_related_template_type(
                        current_constructor->type_information);
            current_constructor = template_type_get_related_symbol(template_type);
        }

        candidate_list = entry_list_add(candidate_list, current_constructor);
    }
    entry_list_iterator_free(it);

    // Second phase (only if the first was not done or failed)

    // Now use this candidate_list
    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(candidate_list,
            NULL, argument_types, num_arguments,
            decl_context,
            locus, /* explicit_template_arguments */ NULL);

    candidate_t* candidate_set = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        candidate_set = candidate_set_add(candidate_set,
                entry_list_iterator_current(it),
                num_arguments,
                argument_types);
    }
    entry_list_iterator_free(it);

    *candidates = entry_list_concat(*candidates, overload_set);

    // Now we have all the constructors, perform an overload resolution on them
    scope_entry_t* overload_resolution = solve_overload_(candidate_set,
            decl_context,
            initialization_kind,
            class_type,
            locus,
            // Out
            is_ambiguous);
    candidate_set_free(&candidate_set);

    *constructor = overload_resolution;

    // In copy-list-initialization, if an explicit constructor is
    // chosen, the initialization is ill-formed.
    if (overload_resolution != NULL
            && (initialization_kind & IK_COPY_INITIALIZATION)
            && symbol_entity_specs_get_is_explicit(overload_resolution))
    {
        *constructor = 0;
        return 0;
    }

    return (overload_resolution != NULL);
}

char solve_list_initialization_of_class_type(
        type_t* class_type,
        type_t** argument_types, 
        int num_arguments,
        enum initialization_kind initialization_kind,
        const decl_context_t* decl_context,
        const locus_t* locus,
        scope_entry_t** constructor,
        scope_entry_list_t** candidates)
{
    char is_ambiguous = 0;
    return solve_list_initialization_of_class_type_(
            class_type,
            argument_types,
            num_arguments,
            initialization_kind,
            decl_context,
            locus,
            // Out
            constructor,
            candidates,
            &is_ambiguous);
}


candidate_t* candidate_set_add(candidate_t* candidate_set,
        scope_entry_t* entry,
        int num_args,
        type_t** args)
{
    if (is_error_type(entry_advance_aliases(entry)->type_information))
        return candidate_set;

    candidate_t* result = NEW0(candidate_t);

    result->next = candidate_set;

    result->entry = entry;

    result->num_args = num_args;
    result->args = args;

    // For members ignore the implicit argument which we allow to be NULL
    int i = 0;
    if (symbol_entity_specs_get_is_member(entry))
        i = 1;

    for (; i < result->num_args; i++)
    {
        // Sanity check
        ERROR_CONDITION(result->args[i] == NULL, "An argument type (except the implicit argument object) cannot be NULL", 0);
    }

    return result;
}

void candidate_set_free(candidate_t** p_candidate_set)
{
    static int i = 0;
    i++;

    ERROR_CONDITION(p_candidate_set == NULL, "This cannot be NULL", 0);
    candidate_t* candidate_set = *p_candidate_set;
    while (candidate_set != NULL)
    {
        candidate_t* next = candidate_set->next;
        DELETE(candidate_set);
        candidate_set = next;
    }

    // Early detection of bugs
    *p_candidate_set = NULL;
}
