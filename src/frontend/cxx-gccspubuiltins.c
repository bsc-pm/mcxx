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




#include <stdio.h>
#include <string.h>
#include "cxx-gccspubuiltins.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"

// Bits of this code come from GCC
//
enum spu_builtin_type_index
{
  SPU_BTI_END_OF_PARAMS,

  /* We create new type nodes for these. */
  SPU_BTI_V16QI,
  SPU_BTI_V8HI,
  SPU_BTI_V4SI,
  SPU_BTI_V2DI,
  SPU_BTI_V4SF,
  SPU_BTI_V2DF,
  SPU_BTI_UV16QI,
  SPU_BTI_UV8HI,
  SPU_BTI_UV4SI,
  SPU_BTI_UV2DI,

  /* A 16-byte type. (Implemented with V16QI_type_node) */
  SPU_BTI_QUADWORD,

  /* These all correspond to intSI_type_node */
  SPU_BTI_7,
  SPU_BTI_S7,
  SPU_BTI_U7,
  SPU_BTI_S10,
  SPU_BTI_S10_4,
  SPU_BTI_U14,
  SPU_BTI_16,
  SPU_BTI_S16,
  SPU_BTI_S16_2,
  SPU_BTI_U16,
  SPU_BTI_U16_2,
  SPU_BTI_U18,

  /* These correspond to the standard types */
  SPU_BTI_INTQI, 
  SPU_BTI_INTHI, 
  SPU_BTI_INTSI, 
  SPU_BTI_INTDI, 

  SPU_BTI_UINTQI,
  SPU_BTI_UINTHI,
  SPU_BTI_UINTSI,
  SPU_BTI_UINTDI,

  SPU_BTI_FLOAT, 
  SPU_BTI_DOUBLE,

  SPU_BTI_VOID,   
  SPU_BTI_PTR,   

  SPU_BTI_MAX
};

#define V16QI_type_node               (spu_builtin_types[SPU_BTI_V16QI])
#define V8HI_type_node                (spu_builtin_types[SPU_BTI_V8HI])
#define V4SI_type_node                (spu_builtin_types[SPU_BTI_V4SI])
#define V2DI_type_node                (spu_builtin_types[SPU_BTI_V2DI])
#define V4SF_type_node                (spu_builtin_types[SPU_BTI_V4SF])
#define V2DF_type_node                (spu_builtin_types[SPU_BTI_V2DF])
#define unsigned_V16QI_type_node      (spu_builtin_types[SPU_BTI_UV16QI])
#define unsigned_V8HI_type_node       (spu_builtin_types[SPU_BTI_UV8HI])
#define unsigned_V4SI_type_node       (spu_builtin_types[SPU_BTI_UV4SI])
#define unsigned_V2DI_type_node       (spu_builtin_types[SPU_BTI_UV2DI])

static type_t* spu_builtin_types[SPU_BTI_MAX];

// These are hardcoded for CELL SysV ABI
#define intQI_type_node (get_signed_char_type())
#define intHI_type_node (get_signed_short_int_type())
#define intSI_type_node (get_signed_int_type())
#define intDI_type_node (get_signed_long_long_int_type())
#define float_type_node (get_float_type())
#define double_type_node (get_double_type())

#define unsigned_intQI_type_node (get_unsigned_char_type())
#define unsigned_intHI_type_node (get_unsigned_short_int_type())
#define unsigned_intSI_type_node (get_unsigned_int_type())
#define unsigned_intDI_type_node (get_unsigned_long_long_int_type())

#define build_vector_type get_vector_type_by_bytes

static void init_spu_types(void)
{
  V16QI_type_node = build_vector_type (intQI_type_node, 16);
  V8HI_type_node = build_vector_type (intHI_type_node, 16);
  V4SI_type_node = build_vector_type (intSI_type_node, 16);
  V2DI_type_node = build_vector_type (intDI_type_node, 16);
  V4SF_type_node = build_vector_type (float_type_node, 16);
  V2DF_type_node = build_vector_type (double_type_node, 16);

  unsigned_V16QI_type_node = build_vector_type (unsigned_intQI_type_node, 16);
  unsigned_V8HI_type_node = build_vector_type (unsigned_intHI_type_node, 16);
  unsigned_V4SI_type_node = build_vector_type (unsigned_intSI_type_node, 16);
  unsigned_V2DI_type_node = build_vector_type (unsigned_intDI_type_node, 16);

  spu_builtin_types[SPU_BTI_QUADWORD] = V16QI_type_node;

  spu_builtin_types[SPU_BTI_7] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_S7] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_U7] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_S10] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_S10_4] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_U14] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_16] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_S16] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_S16_2] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_U16] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_U16_2] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_U18] = get_signed_int_type();

  spu_builtin_types[SPU_BTI_INTQI] = get_signed_char_type();
  spu_builtin_types[SPU_BTI_INTHI] = get_signed_short_int_type();
  spu_builtin_types[SPU_BTI_INTSI] = get_signed_int_type();
  spu_builtin_types[SPU_BTI_INTDI] = get_signed_long_long_int_type();
  spu_builtin_types[SPU_BTI_UINTQI] = get_unsigned_char_type();
  spu_builtin_types[SPU_BTI_UINTHI] = get_unsigned_short_int_type();
  spu_builtin_types[SPU_BTI_UINTSI] = get_unsigned_int_type();
  spu_builtin_types[SPU_BTI_UINTDI] = get_unsigned_long_long_int_type();

  spu_builtin_types[SPU_BTI_FLOAT] = get_float_type();
  spu_builtin_types[SPU_BTI_DOUBLE] = get_double_type();

  spu_builtin_types[SPU_BTI_VOID] = get_void_type();

  spu_builtin_types[SPU_BTI_PTR] = 
      get_cv_qualified_type(get_pointer_type(get_void_type()),
              CV_CONST | CV_VOLATILE);
}

static type_t* adjust_type_for_parameter_type(type_t* orig)
{
    type_t* result = get_unqualified_type(orig);

    if (is_function_type(result))
    {
        result = get_pointer_type(result);
    }
    else if (is_array_type(result))
    {
        result = get_pointer_type(array_type_get_element_type(result));
    }

    return result;
}

// These functions will create lots of stupid and repeated types but it is a
// bit cumbersome to avoid repeated types here like we did for normal gcc
// builtins

static type_t* build_function_type_2(type_t* ret, type_t* arg1)
{
    type_t* result = NULL;
    parameter_info_t _param_info[1]; 
    memset(_param_info, 0, sizeof(_param_info));
    _param_info[0].is_ellipsis = 0; 
    _param_info[0].type_info = arg1;
    _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); 
    result =  get_new_function_type(ret, _param_info, 1, REF_QUALIFIER_NONE); 

    return result;
}

static type_t* build_function_type_3(type_t* ret, type_t* arg1, type_t* arg2)
{
    type_t* result = NULL;
    parameter_info_t _param_info[2]; 
    memset(_param_info, 0, sizeof(_param_info));
    _param_info[0].is_ellipsis = 0; 
    _param_info[0].type_info = arg1;
    _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); 
    _param_info[1].is_ellipsis = 0; 
    _param_info[1].type_info = arg2;
    _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); 
    result =  get_new_function_type(ret, _param_info, 2, REF_QUALIFIER_NONE); 
    return result;
}

static type_t* build_function_type_4(type_t* ret, type_t* arg1, type_t* arg2, type_t* arg3)
{
    type_t* result = NULL;
    parameter_info_t _param_info[3]; 
    memset(_param_info, 0, sizeof(_param_info));
    _param_info[0].is_ellipsis = 0; 
    _param_info[0].type_info = arg1;
    _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); 
    _param_info[1].is_ellipsis = 0; 
    _param_info[1].type_info = arg2;
    _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); 
    _param_info[2].is_ellipsis = 0; 
    _param_info[2].type_info = arg3;
    _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); 
    result =  get_new_function_type(ret, _param_info, 3, REF_QUALIFIER_NONE); 
    return result;
}

#define _A1(ret) (get_new_function_type(spu_builtin_types[ret], NULL, 0, REF_QUALIFIER_NONE))
#define _A2(ret, arg1) (build_function_type_2(spu_builtin_types[ret], spu_builtin_types[arg1]))
#define _A3(ret, arg1, arg2) (build_function_type_3(spu_builtin_types[ret], spu_builtin_types[arg1], spu_builtin_types[arg2]))
#define _A4(ret, arg1, arg2, arg3) (build_function_type_4(spu_builtin_types[ret], spu_builtin_types[arg1], spu_builtin_types[arg2], spu_builtin_types[arg3]))

enum spu_builtin_types {
    B_INSN,
    B_JUMP,
    B_BISLED,
    B_CALL,
    B_HINT,
    B_OVERLOAD, 
    B_INTERNAL
};

static scope_entry_t* solve_spu_overload_name(scope_entry_t* overloaded_function, 
        type_t** types, 
        nodecl_t *arguments UNUSED_PARAMETER, 
        int num_arguments,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Why people insists on having overload in C?
    char name[MCXX_MAX_C_OVERLOADS];

    // From gcc source at maximum 16 are defined
    const int max_valid_overloads = 24;

    char found_match = 0;
    scope_entry_t* result = NULL;

    DEBUG_CODE()
    {
        fprintf(stderr, "SPU-BUILTIN: Trying to figure out the exact version of '%s' given the following %d types\n",
                overloaded_function->symbol_name,
                num_arguments);
        int j;
        for (j = 0; j < num_arguments; j++)
        {
            fprintf(stderr, "SPU-BUILTIN:     [%d] %s\n", j,
                    print_declarator(types[j]));
        }
    }

    int i;
    for (i = 0; (i < max_valid_overloads) && !found_match; i++)
    {
        snprintf(name, 255, "%s_%d", overloaded_function->symbol_name, i);
        name[255] = '\0';
        scope_entry_list_t *entry_list = query_name_str(overloaded_function->decl_context, name, NULL);

        // Let's assume no more overloads have been defined
        if (entry_list == NULL)
        {
            break;
        }

        scope_entry_t* current_entry = entry_list_head(entry_list);
        entry_list_free(entry_list);

        type_t* current_function_type = current_entry->type_information;

        DEBUG_CODE()
        {
            fprintf(stderr, "SPU-BUILTIN: Checking with builtin '%s' of type '%s'\n",
                    current_entry->symbol_name,
                    print_declarator(current_function_type));
        }

        if (!is_function_type(current_function_type))
        {
            internal_error("spu builtin '%s' without function type\n", current_entry);
        }

        // Don't know if this case is considered but let's be kind with this crazy SDK
        if (num_arguments != function_type_get_num_parameters(current_function_type))
        {
            continue;
        }

        int j;
        char all_arguments_matched = 1;
        for (j = 0; (j < num_arguments) && all_arguments_matched; j++)
        {
            type_t* argument_type = types[j];
            type_t* parameter_type = function_type_get_parameter_type_num(current_function_type, j);

            // Fix the parameter

            //  We try to mimic this
            //
            // if ((!SCALAR_TYPE_P (param_type)
            //             || !SCALAR_TYPE_P (arg_type)
            //             || ((fcode == SPU_SPLATS || fcode == SPU_PROMOTE
            //                     || fcode == SPU_HCMPEQ || fcode == SPU_HCMPGT
            //                     || fcode == SPU_MASKB || fcode == SPU_MASKH
            //                     || fcode == SPU_MASKW) && p == 0))
            //         && !comptypes (TYPE_MAIN_VARIANT (param_type),
            //             TYPE_MAIN_VARIANT (arg_type)))
            //
            // standard_conversion_t scs;
            // if ((!is_scalar_type (parameter_type)
            //             || !is_scalar_type (argument_type)
            //             || ((strcmp(overloaded_function->symbol_name, "__builtin_spu_splats") == 0 
            //                     || strcmp(overloaded_function->symbol_name, "__builtin_spu_promote") == 0
            //                     || strcmp(overloaded_function->symbol_name, "__builtin_spu_hcmpeq") == 0 
            //                     || strcmp(overloaded_function->symbol_name, "__builtin_spu_hcmpgt") == 0
            //                     || strcmp(overloaded_function->symbol_name, "__builtin_spu_maskb") == 0 
            //                     || strcmp(overloaded_function->symbol_name, "__builtin_spu_maskh") == 0
            //                     || strcmp(overloaded_function->symbol_name, "__builtin_spu_maskw") == 0) 
            //                 && j == 0))
            //         && !standard_conversion_between_types(&scs, main_variant(argument_type),
            //                 main_variant(parameter_type), overloaded_function->decl_context))
            // {
            //     all_arguments_matched = 0;
            // }
            all_arguments_matched = all_arguments_matched 
                // We want vector types to be identic
                && (equivalent_types(get_unqualified_type(argument_type),
                            parameter_type));
        }

        if (all_arguments_matched)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SPU-BUILTIN: Builtin '%s' of type '%s' matched!\n",
                        current_entry->symbol_name,
                        print_declarator(current_function_type));
            }
            result = current_entry;
            found_match = 1;
        }
    }

    return result;
}


#define DEF_BUILTIN(UNUSED1, UNUSED2, NAME, KIND, TYPE) \
{ \
    scope_entry_t* new_spu_builtin = new_symbol(global_context, global_context->global_scope, "__builtin_" NAME);  \
    new_spu_builtin->kind = SK_FUNCTION; \
    if ((KIND) == B_OVERLOAD) \
    { \
        new_spu_builtin->type_information = spu_solve_overload_type; \
    } \
    else \
    { \
        new_spu_builtin->type_information = TYPE; \
    } \
    symbol_entity_specs_set_is_builtin(new_spu_builtin, 1); \
    new_spu_builtin->do_not_print = 1; \
}

void gcc_sign_in_spu_builtins(const decl_context_t* global_context)
{
    init_spu_types();

    type_t* spu_solve_overload_type = get_computed_function_type(solve_spu_overload_name);

#include "cxx-gccspubuiltins.def"
}
