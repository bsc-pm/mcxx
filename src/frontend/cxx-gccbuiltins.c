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

#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include <string.h>

/* Copyright(C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or(at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* This header provides a declarative way of describing the types that
   are used when declaring builtin functions.

   Before including this header, you must define the following macros:

   DEF_PRIMITIVE_TYPE(ENUM, TYPE)

     The ENUM is an identifier indicating which type is being defined.
     TYPE is an expression for a `tree' that represents the type.

   DEF_FUNCTION_TYPE_0(ENUM, RETURN)
   DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1)
   DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2)
   DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3)
   DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4)
   DEF_FUNCTION_TYPE_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5)

     These macros describe function types.  ENUM is as above.  The
     RETURN type is one of the enumerals already defined.  ARG1, ARG2,
     and ARG3 give the types of the arguments, similarly.

   DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN)
   DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1)
   DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2)
   DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, ARG1, ARG2, ARG3)
   DEF_FUNCTION_TYPE_VAR_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4)

     Similar, but for function types that take variable arguments.
     For example:

       DEF_FUNCTION_TYPE_1(BT_INT_DOUBLE, BT_INT, BT_DOUBLE)

     describes the type `int()(double)', using the enumeral
     BT_INT_DOUBLE, whereas:

       DEF_FUNCTION_TYPE_VAR_1(BT_INT_DOUBLE_VAR, BT_INT, BT_DOUBLE)

     describes the type `int()(double, ...)'.
 
  DEF_POINTER_TYPE(ENUM, TYPE)

    This macro describes a pointer type.  ENUM is as above; TYPE is
    the type pointed to.  */

#define UNUSED_FUNCTION __attribute__((unused))

#define DEF_PRIMITIVE_TYPE(NAME, TYPE_T_VALUE) \
    UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
    { \
        static type_t* result = NULL; \
        if(result == NULL) \
        { \
           result = TYPE_T_VALUE; \
        } \
        return result; \
    }

#define DEF_POINTER_TYPE(NAME, TYPE_T_VALUE) \
    UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
    { \
        static type_t* result = NULL; \
        if(result == NULL) \
        { \
           result = get_pointer_type((__mcxx_builtin_type__##TYPE_T_VALUE)()); \
        } \
        return result; \
    }

#define DEF_POINTER_VOLATILE_TYPE(NAME, TYPE_T_VALUE) \
    UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
    { \
        static type_t* result = NULL; \
        if(result == NULL) \
        { \
           result = get_pointer_type(get_volatile_qualified_type((__mcxx_builtin_type__##TYPE_T_VALUE)())); \
        } \
        return result; \
    }

// This is required for the sync overloaded functions
static scope_entry_t* solve_gcc_sync_builtins_overload_name(scope_entry_t* overloaded_function, type_t** arguments, int num_arguments);
DEF_PRIMITIVE_TYPE(BT_FN_SYNC_OVERLOAD, get_computed_function_type(solve_gcc_sync_builtins_overload_name))

// Basic types
DEF_PRIMITIVE_TYPE(BT_VOID, get_void_type())
DEF_PRIMITIVE_TYPE(BT_BOOL, get_bool_type())
DEF_PRIMITIVE_TYPE(BT_INT, get_signed_int_type())
DEF_PRIMITIVE_TYPE(BT_UINT, get_unsigned_int_type())
DEF_PRIMITIVE_TYPE(BT_LONG, get_signed_long_int_type())
DEF_PRIMITIVE_TYPE(BT_ULONG, get_unsigned_long_int_type())
DEF_PRIMITIVE_TYPE(BT_LONGLONG, get_signed_long_long_int_type())
DEF_PRIMITIVE_TYPE(BT_ULONGLONG, get_unsigned_long_long_int_type())
// FIXME using type environment information
DEF_PRIMITIVE_TYPE(BT_INTMAX, get_signed_long_int_type())
DEF_PRIMITIVE_TYPE(BT_UINTMAX, get_unsigned_int_type())

// FIXME  using type environment information
DEF_PRIMITIVE_TYPE(BT_WORD, get_signed_int_type())
DEF_PRIMITIVE_TYPE(BT_FLOAT, get_float_type())
DEF_PRIMITIVE_TYPE(BT_DOUBLE, get_double_type())
DEF_PRIMITIVE_TYPE(BT_LONGDOUBLE, get_long_double_type())
DEF_PRIMITIVE_TYPE(BT_COMPLEX_FLOAT, get_complex_type(get_float_type()))
DEF_PRIMITIVE_TYPE(BT_COMPLEX_DOUBLE, get_complex_type(get_double_type()))
DEF_PRIMITIVE_TYPE(BT_COMPLEX_LONGDOUBLE, get_complex_type(get_long_double_type()))

DEF_PRIMITIVE_TYPE(BT_PTR, get_pointer_type(get_void_type()))

DEF_PRIMITIVE_TYPE(BT_DFLOAT32, get_float_type())
DEF_PRIMITIVE_TYPE(BT_DFLOAT64, get_double_type())
DEF_PRIMITIVE_TYPE(BT_DFLOAT128, get_long_double_type())

// How to define this nicely ?
DEF_PRIMITIVE_TYPE(BT_FILEPTR, get_pointer_type(get_void_type()))
DEF_PRIMITIVE_TYPE(BT_CONST_PTR, get_pointer_type(get_const_qualified_type(get_void_type())))
DEF_PRIMITIVE_TYPE(BT_VOLATILE_PTR, get_pointer_type(get_volatile_qualified_type(get_void_type())))

// What is this ? :)
// DEF_PRIMITIVE_TYPE(BT_PTRMODE,(*lang_hooks.types.type_for_mode)(ptr_mode, 0))
DEF_PRIMITIVE_TYPE(BT_PTRMODE, get_pointer_type(get_void_type()))

DEF_PRIMITIVE_TYPE(BT_INT_PTR, get_pointer_type(get_signed_int_type()))
DEF_PRIMITIVE_TYPE(BT_FLOAT_PTR, get_pointer_type(get_float_type()))
DEF_PRIMITIVE_TYPE(BT_DOUBLE_PTR, get_pointer_type(get_double_type()))
DEF_PRIMITIVE_TYPE(BT_LONGDOUBLE_PTR, get_pointer_type(get_long_double_type()))
// FIXME - These depend on the exact environment
DEF_PRIMITIVE_TYPE(BT_PID, get_signed_int_type())
DEF_PRIMITIVE_TYPE(BT_SIZE, get_unsigned_int_type())
DEF_PRIMITIVE_TYPE(BT_SSIZE, get_signed_int_type())
// FIXME - These depend on the exact environment
// This is roughly the same as wchar_t
DEF_PRIMITIVE_TYPE(BT_WINT, get_unsigned_short_int_type())
DEF_PRIMITIVE_TYPE(BT_STRING, get_pointer_type(get_char_type()))
DEF_PRIMITIVE_TYPE(BT_CONST_STRING, get_pointer_type(get_const_qualified_type(get_char_type())))

// Review this type
DEF_PRIMITIVE_TYPE(BT_VALIST_REF, get_gcc_builtin_va_list_type())
DEF_PRIMITIVE_TYPE(BT_VALIST_ARG, get_gcc_builtin_va_list_type())

// FIXME - These depend on the exact environment
DEF_PRIMITIVE_TYPE(BT_I1, get_signed_char_type())
DEF_PRIMITIVE_TYPE(BT_I2, get_signed_short_int_type())
DEF_PRIMITIVE_TYPE(BT_I4, get_signed_int_type())
DEF_PRIMITIVE_TYPE(BT_I8, get_signed_long_long_int_type())
DEF_PRIMITIVE_TYPE(BT_I16, get_signed_long_long_int_type())

DEF_PRIMITIVE_TYPE(BT_UNWINDWORD, get_signed_int_type())

DEF_POINTER_TYPE (BT_PTR_CONST_STRING, BT_CONST_STRING)
DEF_POINTER_TYPE (BT_PTR_LONG, BT_LONG)
DEF_POINTER_TYPE (BT_PTR_ULONGLONG, BT_ULONGLONG)
DEF_POINTER_TYPE (BT_PTR_PTR, BT_PTR)

DEF_PRIMITIVE_TYPE(BT_UINT16, get_unsigned_short_int_type())
DEF_PRIMITIVE_TYPE(BT_UINT32, get_unsigned_int_type())
DEF_PRIMITIVE_TYPE(BT_UINT64, get_unsigned_long_long_int_type())

// Volatile pointers to basic integers (used in sync functions)
DEF_POINTER_VOLATILE_TYPE(BT_VPTR_I1, BT_I1)
DEF_POINTER_VOLATILE_TYPE(BT_VPTR_I2, BT_I2)
DEF_POINTER_VOLATILE_TYPE(BT_VPTR_I4, BT_I4)
DEF_POINTER_VOLATILE_TYPE(BT_VPTR_I8, BT_I8)

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


#define DEF_FUNCTION_TYPE_0(NAME, RESULT_TYPE) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
          result = get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), NULL, 0); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_1(NAME, RESULT_TYPE, PARAM_TYPE1) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
          parameter_info_t _param_info[1]; \
          memset(_param_info, 0, sizeof(_param_info)); \
          _param_info[0].is_ellipsis = 0; \
          _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
          _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 1); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_2(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
         parameter_info_t _param_info[2]; \
         memset(_param_info, 0, sizeof(_param_info)); \
         _param_info[0].is_ellipsis = 0; \
         _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
         _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
         _param_info[1].is_ellipsis = 0; \
         _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
         _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
         result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 2); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_3(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
         parameter_info_t _param_info[3]; \
         memset(_param_info, 0, sizeof(_param_info)); \
         _param_info[0].is_ellipsis = 0; \
         _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
         _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
         _param_info[1].is_ellipsis = 0; \
         _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
         _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
         _param_info[2].is_ellipsis = 0; \
         _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
         _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
         result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 3); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_4(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3, PARAM_TYPE4) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
          parameter_info_t _param_info[4]; \
          memset(_param_info, 0, sizeof(_param_info)); \
          _param_info[0].is_ellipsis = 0; \
          _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
          _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
          _param_info[1].is_ellipsis = 0; \
          _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
          _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
          _param_info[2].is_ellipsis = 0; \
          _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
          _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
          _param_info[3].is_ellipsis = 0; \
          _param_info[3].type_info =(__mcxx_builtin_type__##PARAM_TYPE4)(); \
          _param_info[3].type_info = adjust_type_for_parameter_type(_param_info[3].type_info); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 4); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_5(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3, PARAM_TYPE4, PARAM_TYPE5) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
          parameter_info_t _param_info[5]; \
          memset(_param_info, 0, sizeof(_param_info)); \
          _param_info[0].is_ellipsis = 0; \
          _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
          _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
          _param_info[1].is_ellipsis = 0; \
          _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
          _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
          _param_info[2].is_ellipsis = 0; \
          _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
          _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
          _param_info[3].is_ellipsis = 0; \
          _param_info[3].type_info =(__mcxx_builtin_type__##PARAM_TYPE4)(); \
          _param_info[3].type_info = adjust_type_for_parameter_type(_param_info[3].type_info); \
          _param_info[4].is_ellipsis = 0; \
          _param_info[4].type_info =(__mcxx_builtin_type__##PARAM_TYPE5)(); \
          _param_info[4].type_info = adjust_type_for_parameter_type(_param_info[4].type_info); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 5); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_6(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3, PARAM_TYPE4, PARAM_TYPE5, PARAM_TYPE6) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[6]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 0; \
           _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
           _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
           _param_info[1].is_ellipsis = 0; \
           _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
           _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
           _param_info[2].is_ellipsis = 0; \
           _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
           _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
           _param_info[3].is_ellipsis = 0; \
           _param_info[3].type_info =(__mcxx_builtin_type__##PARAM_TYPE4)(); \
           _param_info[3].type_info = adjust_type_for_parameter_type(_param_info[3].type_info); \
           _param_info[4].is_ellipsis = 0; \
           _param_info[4].type_info =(__mcxx_builtin_type__##PARAM_TYPE5)(); \
           _param_info[4].type_info = adjust_type_for_parameter_type(_param_info[4].type_info); \
           _param_info[5].is_ellipsis = 0; \
           _param_info[5].type_info =(__mcxx_builtin_type__##PARAM_TYPE6)(); \
           _param_info[5].type_info = adjust_type_for_parameter_type(_param_info[5].type_info); \
           result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 6); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_7(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3, PARAM_TYPE4, PARAM_TYPE5, PARAM_TYPE6, PARAM_TYPE7) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[6]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 0; \
           _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
           _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
           _param_info[1].is_ellipsis = 0; \
           _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
           _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
           _param_info[2].is_ellipsis = 0; \
           _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
           _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
           _param_info[3].is_ellipsis = 0; \
           _param_info[3].type_info =(__mcxx_builtin_type__##PARAM_TYPE4)(); \
           _param_info[3].type_info = adjust_type_for_parameter_type(_param_info[3].type_info); \
           _param_info[4].is_ellipsis = 0; \
           _param_info[4].type_info =(__mcxx_builtin_type__##PARAM_TYPE5)(); \
           _param_info[4].type_info = adjust_type_for_parameter_type(_param_info[4].type_info); \
           _param_info[5].is_ellipsis = 0; \
           _param_info[5].type_info =(__mcxx_builtin_type__##PARAM_TYPE6)(); \
           _param_info[5].type_info = adjust_type_for_parameter_type(_param_info[5].type_info); \
           _param_info[6].is_ellipsis = 0; \
           _param_info[6].type_info =(__mcxx_builtin_type__##PARAM_TYPE7)(); \
           _param_info[6].type_info = adjust_type_for_parameter_type(_param_info[6].type_info); \
           result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 7); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_VAR_0(NAME, RESULT_TYPE) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[1]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 1; \
           _param_info[0].type_info = get_ellipsis_type(); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 1); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_VAR_1(NAME, RESULT_TYPE, PARAM_TYPE1) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[2]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 0; \
           _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
           _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
           _param_info[1].is_ellipsis = 1; \
           _param_info[1].type_info = get_ellipsis_type(); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 2); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_VAR_2(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[3]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 0; \
           _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
           _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
           _param_info[1].is_ellipsis = 0; \
           _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
           _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
           _param_info[2].is_ellipsis = 1; \
           _param_info[2].type_info = get_ellipsis_type(); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 3); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_VAR_3(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[4]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 0; \
           _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
           _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
           _param_info[1].is_ellipsis = 0; \
           _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
           _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
           _param_info[2].is_ellipsis = 0; \
           _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
           _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
           _param_info[3].is_ellipsis = 1; \
           _param_info[3].type_info = get_ellipsis_type(); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 4); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_VAR_4(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3, PARAM_TYPE4) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[5]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 0; \
           _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
           _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
           _param_info[1].is_ellipsis = 0; \
           _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
           _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
           _param_info[2].is_ellipsis = 0; \
           _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
           _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
           _param_info[3].is_ellipsis = 0; \
           _param_info[3].type_info =(__mcxx_builtin_type__##PARAM_TYPE4)(); \
           _param_info[3].type_info = adjust_type_for_parameter_type(_param_info[3].type_info); \
           _param_info[4].is_ellipsis = 1; \
           _param_info[4].type_info = get_ellipsis_type(); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 5); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_VAR_5(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3, PARAM_TYPE4, PARAM_TYPE5) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[6]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 0; \
           _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
           _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
           _param_info[1].is_ellipsis = 0; \
           _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
           _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
           _param_info[2].is_ellipsis = 0; \
           _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
           _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
           _param_info[3].is_ellipsis = 0; \
           _param_info[3].type_info =(__mcxx_builtin_type__##PARAM_TYPE4)(); \
           _param_info[3].type_info = adjust_type_for_parameter_type(_param_info[3].type_info); \
           _param_info[4].is_ellipsis = 0; \
           _param_info[4].type_info =(__mcxx_builtin_type__##PARAM_TYPE5)(); \
           _param_info[4].type_info = adjust_type_for_parameter_type(_param_info[4].type_info); \
           _param_info[5].is_ellipsis = 1; \
           _param_info[5].type_info = get_ellipsis_type(); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 6); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_VAR_6(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3, PARAM_TYPE4, PARAM_TYPE5, PARAM_TYPE6) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[7]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 0; \
           _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
           _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
           _param_info[1].is_ellipsis = 0; \
           _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
           _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
           _param_info[2].is_ellipsis = 0; \
           _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
           _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
           _param_info[3].is_ellipsis = 0; \
           _param_info[3].type_info =(__mcxx_builtin_type__##PARAM_TYPE4)(); \
           _param_info[3].type_info = adjust_type_for_parameter_type(_param_info[3].type_info); \
           _param_info[4].is_ellipsis = 0; \
           _param_info[4].type_info =(__mcxx_builtin_type__##PARAM_TYPE5)(); \
           _param_info[4].type_info = adjust_type_for_parameter_type(_param_info[4].type_info); \
           _param_info[5].is_ellipsis = 0; \
           _param_info[5].type_info =(__mcxx_builtin_type__##PARAM_TYPE6)(); \
           _param_info[5].type_info = adjust_type_for_parameter_type(_param_info[5].type_info); \
           _param_info[6].is_ellipsis = 1; \
           _param_info[6].type_info = get_ellipsis_type(); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 7); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_VAR_7(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3, PARAM_TYPE4, PARAM_TYPE5, PARAM_TYPE6, PARAM_TYPE_7) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
           parameter_info_t _param_info[8]; \
           memset(_param_info, 0, sizeof(_param_info)); \
           _param_info[0].is_ellipsis = 0; \
           _param_info[0].type_info =(__mcxx_builtin_type__##PARAM_TYPE1)(); \
           _param_info[0].type_info = adjust_type_for_parameter_type(_param_info[0].type_info); \
           _param_info[1].is_ellipsis = 0; \
           _param_info[1].type_info =(__mcxx_builtin_type__##PARAM_TYPE2)(); \
           _param_info[1].type_info = adjust_type_for_parameter_type(_param_info[1].type_info); \
           _param_info[2].is_ellipsis = 0; \
           _param_info[2].type_info =(__mcxx_builtin_type__##PARAM_TYPE3)(); \
           _param_info[2].type_info = adjust_type_for_parameter_type(_param_info[2].type_info); \
           _param_info[3].is_ellipsis = 0; \
           _param_info[3].type_info =(__mcxx_builtin_type__##PARAM_TYPE4)(); \
           _param_info[3].type_info = adjust_type_for_parameter_type(_param_info[3].type_info); \
           _param_info[4].is_ellipsis = 0; \
           _param_info[4].type_info =(__mcxx_builtin_type__##PARAM_TYPE5)(); \
           _param_info[4].type_info = adjust_type_for_parameter_type(_param_info[4].type_info); \
           _param_info[5].is_ellipsis = 0; \
           _param_info[5].type_info =(__mcxx_builtin_type__##PARAM_TYPE6)(); \
           _param_info[5].type_info = adjust_type_for_parameter_type(_param_info[5].type_info); \
           _param_info[6].is_ellipsis = 0; \
           _param_info[6].type_info =(__mcxx_builtin_type__##PARAM_TYPE7)(); \
           _param_info[6].type_info = adjust_type_for_parameter_type(_param_info[5].type_info); \
           _param_info[7].is_ellipsis = 1; \
           _param_info[7].type_info = get_ellipsis_type(); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 8); \
       } \
       return result; \
   } 


DEF_FUNCTION_TYPE_0 (BT_FN_VOID, BT_VOID)
DEF_FUNCTION_TYPE_0 (BT_FN_BOOL, BT_BOOL)
DEF_FUNCTION_TYPE_0 (BT_FN_PTR, BT_PTR)
DEF_FUNCTION_TYPE_0 (BT_FN_PID, BT_PID)
DEF_FUNCTION_TYPE_0 (BT_FN_INT, BT_INT)
DEF_FUNCTION_TYPE_0 (BT_FN_UINT, BT_UINT)
DEF_FUNCTION_TYPE_0 (BT_FN_FLOAT, BT_FLOAT)
DEF_FUNCTION_TYPE_0 (BT_FN_DOUBLE, BT_DOUBLE)
/* For "long double" we use LONGDOUBLE (not LONG_DOUBLE) to
   distinguish it from two types in sequence, "long" followed by
   "double".  */
DEF_FUNCTION_TYPE_0 (BT_FN_LONGDOUBLE, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_0 (BT_FN_DFLOAT32, BT_DFLOAT32)
DEF_FUNCTION_TYPE_0 (BT_FN_DFLOAT64, BT_DFLOAT64)
DEF_FUNCTION_TYPE_0 (BT_FN_DFLOAT128, BT_DFLOAT128)

DEF_FUNCTION_TYPE_1 (BT_FN_LONG_LONG, BT_LONG, BT_LONG)
DEF_FUNCTION_TYPE_1 (BT_FN_LONGLONG_LONGLONG, BT_LONGLONG, BT_LONGLONG)
DEF_FUNCTION_TYPE_1 (BT_FN_INTMAX_INTMAX, BT_INTMAX, BT_INTMAX)
DEF_FUNCTION_TYPE_1 (BT_FN_FLOAT_FLOAT, BT_FLOAT, BT_FLOAT)
DEF_FUNCTION_TYPE_1 (BT_FN_DOUBLE_DOUBLE, BT_DOUBLE, BT_DOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_LONGDOUBLE_LONGDOUBLE,
		     BT_LONGDOUBLE, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT,
		     BT_COMPLEX_FLOAT, BT_COMPLEX_FLOAT)
DEF_FUNCTION_TYPE_1 (BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE,
		     BT_COMPLEX_DOUBLE, BT_COMPLEX_DOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE,
		     BT_COMPLEX_LONGDOUBLE, BT_COMPLEX_LONGDOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_FLOAT_COMPLEX_FLOAT,
		     BT_FLOAT, BT_COMPLEX_FLOAT)
DEF_FUNCTION_TYPE_1 (BT_FN_DOUBLE_COMPLEX_DOUBLE,
		     BT_DOUBLE, BT_COMPLEX_DOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_LONGDOUBLE_COMPLEX_LONGDOUBLE,
		     BT_LONGDOUBLE, BT_COMPLEX_LONGDOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_COMPLEX_FLOAT_FLOAT,
		     BT_COMPLEX_FLOAT, BT_FLOAT)
DEF_FUNCTION_TYPE_1 (BT_FN_COMPLEX_DOUBLE_DOUBLE,
		     BT_COMPLEX_DOUBLE, BT_DOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_COMPLEX_LONGDOUBLE_LONGDOUBLE,
		     BT_COMPLEX_LONGDOUBLE, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_PTR_UINT, BT_PTR, BT_UINT)
DEF_FUNCTION_TYPE_1 (BT_FN_PTR_SIZE, BT_PTR, BT_SIZE)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_INT, BT_INT, BT_INT)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_UINT, BT_INT, BT_UINT)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_LONG, BT_INT, BT_LONG)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_ULONG, BT_INT, BT_ULONG)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_LONGLONG, BT_INT, BT_LONGLONG)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_ULONGLONG, BT_INT, BT_ULONGLONG)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_INTMAX, BT_INT, BT_INTMAX)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_UINTMAX, BT_INT, BT_UINTMAX)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_PTR, BT_INT, BT_PTR)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_FLOAT, BT_INT, BT_FLOAT)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_DOUBLE, BT_INT, BT_DOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_LONGDOUBLE, BT_INT, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_DFLOAT32, BT_INT, BT_DFLOAT32)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_DFLOAT64, BT_INT, BT_DFLOAT64)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_DFLOAT128, BT_INT, BT_DFLOAT128)
DEF_FUNCTION_TYPE_1 (BT_FN_LONG_FLOAT, BT_LONG, BT_FLOAT)
DEF_FUNCTION_TYPE_1 (BT_FN_LONG_DOUBLE, BT_LONG, BT_DOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_LONG_LONGDOUBLE, BT_LONG, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_LONGLONG_FLOAT, BT_LONGLONG, BT_FLOAT)
DEF_FUNCTION_TYPE_1 (BT_FN_LONGLONG_DOUBLE, BT_LONGLONG, BT_DOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_LONGLONG_LONGDOUBLE, BT_LONGLONG, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_1 (BT_FN_VOID_PTR, BT_VOID, BT_PTR)
DEF_FUNCTION_TYPE_1 (BT_FN_SIZE_CONST_STRING, BT_SIZE, BT_CONST_STRING)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_CONST_STRING, BT_INT, BT_CONST_STRING)
DEF_FUNCTION_TYPE_1 (BT_FN_PTR_PTR, BT_PTR, BT_PTR)
DEF_FUNCTION_TYPE_1 (BT_FN_VOID_VALIST_REF, BT_VOID, BT_VALIST_REF)
DEF_FUNCTION_TYPE_1 (BT_FN_VOID_INT, BT_VOID, BT_INT)
DEF_FUNCTION_TYPE_1 (BT_FN_FLOAT_CONST_STRING, BT_FLOAT, BT_CONST_STRING)
DEF_FUNCTION_TYPE_1 (BT_FN_DOUBLE_CONST_STRING, BT_DOUBLE, BT_CONST_STRING)
DEF_FUNCTION_TYPE_1 (BT_FN_LONGDOUBLE_CONST_STRING,
		     BT_LONGDOUBLE, BT_CONST_STRING)
DEF_FUNCTION_TYPE_1 (BT_FN_DFLOAT32_CONST_STRING, BT_DFLOAT32, BT_CONST_STRING)
DEF_FUNCTION_TYPE_1 (BT_FN_DFLOAT64_CONST_STRING, BT_DFLOAT64, BT_CONST_STRING)
DEF_FUNCTION_TYPE_1 (BT_FN_DFLOAT128_CONST_STRING,
		     BT_DFLOAT128, BT_CONST_STRING)
DEF_FUNCTION_TYPE_1 (BT_FN_STRING_CONST_STRING, BT_STRING, BT_CONST_STRING)
DEF_FUNCTION_TYPE_1 (BT_FN_UNWINDWORD_PTR, BT_UNWINDWORD, BT_PTR)
DEF_FUNCTION_TYPE_1 (BT_FN_INT_WINT, BT_INT, BT_WINT)
DEF_FUNCTION_TYPE_1 (BT_FN_WINT_WINT, BT_WINT, BT_WINT)
DEF_FUNCTION_TYPE_1 (BT_FN_DFLOAT32_DFLOAT32, BT_DFLOAT32, BT_DFLOAT32)
DEF_FUNCTION_TYPE_1 (BT_FN_DFLOAT64_DFLOAT64, BT_DFLOAT64, BT_DFLOAT64)
DEF_FUNCTION_TYPE_1 (BT_FN_DFLOAT128_DFLOAT128, BT_DFLOAT128, BT_DFLOAT128)
DEF_FUNCTION_TYPE_1 (BT_FN_VOID_VPTR, BT_VOID, BT_VOLATILE_PTR)
DEF_FUNCTION_TYPE_1 (BT_FN_VOID_PTRPTR, BT_VOID, BT_PTR_PTR)
DEF_FUNCTION_TYPE_1 (BT_FN_UINT_UINT, BT_UINT, BT_UINT)
DEF_FUNCTION_TYPE_1 (BT_FN_ULONG_ULONG, BT_ULONG, BT_ULONG)
DEF_FUNCTION_TYPE_1 (BT_FN_ULONGLONG_ULONGLONG, BT_ULONGLONG, BT_ULONGLONG)
DEF_FUNCTION_TYPE_1 (BT_FN_UINT32_UINT32, BT_UINT32, BT_UINT32)
DEF_FUNCTION_TYPE_1 (BT_FN_UINT64_UINT64, BT_UINT64, BT_UINT64)

DEF_POINTER_TYPE (BT_PTR_FN_VOID_PTR, BT_FN_VOID_PTR)

DEF_FUNCTION_TYPE_2 (BT_FN_VOID_PTR_INT, BT_VOID, BT_PTR, BT_INT)
DEF_FUNCTION_TYPE_2 (BT_FN_STRING_STRING_CONST_STRING,
		     BT_STRING, BT_STRING, BT_CONST_STRING)
DEF_FUNCTION_TYPE_2 (BT_FN_INT_CONST_STRING_CONST_STRING,
		     BT_INT, BT_CONST_STRING, BT_CONST_STRING)
DEF_FUNCTION_TYPE_2 (BT_FN_STRING_CONST_STRING_CONST_STRING,
		     BT_STRING, BT_CONST_STRING, BT_CONST_STRING)
DEF_FUNCTION_TYPE_2 (BT_FN_SIZE_CONST_STRING_CONST_STRING,
		     BT_SIZE, BT_CONST_STRING, BT_CONST_STRING)
DEF_FUNCTION_TYPE_2 (BT_FN_STRING_CONST_STRING_INT,
		     BT_STRING, BT_CONST_STRING, BT_INT)
DEF_FUNCTION_TYPE_2 (BT_FN_STRING_CONST_STRING_SIZE,
		     BT_STRING, BT_CONST_STRING, BT_SIZE)
DEF_FUNCTION_TYPE_2 (BT_FN_INT_CONST_STRING_FILEPTR,
		     BT_INT, BT_CONST_STRING, BT_FILEPTR)
DEF_FUNCTION_TYPE_2 (BT_FN_INT_INT_FILEPTR,
		     BT_INT, BT_INT, BT_FILEPTR)
DEF_FUNCTION_TYPE_2 (BT_FN_VOID_PTRMODE_PTR,
		     BT_VOID, BT_PTRMODE, BT_PTR)
DEF_FUNCTION_TYPE_2 (BT_FN_VOID_VALIST_REF_VALIST_ARG,
		     BT_VOID, BT_VALIST_REF, BT_VALIST_ARG)
DEF_FUNCTION_TYPE_2 (BT_FN_LONG_LONG_LONG,
		     BT_LONG, BT_LONG, BT_LONG)
DEF_FUNCTION_TYPE_2 (BT_FN_INT_PTR_CONST_STRING,
		     BT_INT, BT_PTR, BT_CONST_STRING)
DEF_FUNCTION_TYPE_2 (BT_FN_VOID_PTR_SIZE,
		     BT_VOID, BT_PTR, BT_SIZE)
DEF_FUNCTION_TYPE_2 (BT_FN_FLOAT_FLOAT_FLOAT,
		     BT_FLOAT, BT_FLOAT, BT_FLOAT)
DEF_FUNCTION_TYPE_2 (BT_FN_DOUBLE_DOUBLE_DOUBLE,
		     BT_DOUBLE, BT_DOUBLE, BT_DOUBLE)
DEF_FUNCTION_TYPE_2 (BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE,
		     BT_LONGDOUBLE, BT_LONGDOUBLE, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_2 (BT_FN_FLOAT_FLOAT_FLOATPTR,
		     BT_FLOAT, BT_FLOAT, BT_FLOAT_PTR)
DEF_FUNCTION_TYPE_2 (BT_FN_DOUBLE_DOUBLE_DOUBLEPTR,
		     BT_DOUBLE, BT_DOUBLE, BT_DOUBLE_PTR)
DEF_FUNCTION_TYPE_2 (BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLEPTR,
		     BT_LONGDOUBLE, BT_LONGDOUBLE, BT_LONGDOUBLE_PTR)
DEF_FUNCTION_TYPE_2 (BT_FN_FLOAT_FLOAT_LONGDOUBLE,
		     BT_FLOAT, BT_FLOAT, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_2 (BT_FN_DOUBLE_DOUBLE_LONGDOUBLE,
		     BT_DOUBLE, BT_DOUBLE, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_2 (BT_FN_FLOAT_FLOAT_INT,
		     BT_FLOAT, BT_FLOAT, BT_INT)
DEF_FUNCTION_TYPE_2 (BT_FN_DOUBLE_DOUBLE_INT,
		     BT_DOUBLE, BT_DOUBLE, BT_INT)
DEF_FUNCTION_TYPE_2 (BT_FN_LONGDOUBLE_LONGDOUBLE_INT,
		     BT_LONGDOUBLE, BT_LONGDOUBLE, BT_INT)
DEF_FUNCTION_TYPE_2 (BT_FN_FLOAT_FLOAT_INTPTR,
		     BT_FLOAT, BT_FLOAT, BT_INT_PTR)
DEF_FUNCTION_TYPE_2 (BT_FN_DOUBLE_DOUBLE_INTPTR,
		     BT_DOUBLE, BT_DOUBLE, BT_INT_PTR)
DEF_FUNCTION_TYPE_2 (BT_FN_LONGDOUBLE_LONGDOUBLE_INTPTR,
		     BT_LONGDOUBLE, BT_LONGDOUBLE, BT_INT_PTR)
DEF_FUNCTION_TYPE_2 (BT_FN_FLOAT_INT_FLOAT,
		     BT_FLOAT, BT_INT, BT_FLOAT)
DEF_FUNCTION_TYPE_2 (BT_FN_DOUBLE_INT_DOUBLE,
		     BT_DOUBLE, BT_INT, BT_DOUBLE)
DEF_FUNCTION_TYPE_2 (BT_FN_LONGDOUBLE_INT_LONGDOUBLE,
		     BT_LONGDOUBLE, BT_INT, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_2 (BT_FN_FLOAT_FLOAT_LONG,
		     BT_FLOAT, BT_FLOAT, BT_LONG)
DEF_FUNCTION_TYPE_2 (BT_FN_DOUBLE_DOUBLE_LONG,
		     BT_DOUBLE, BT_DOUBLE, BT_LONG)
DEF_FUNCTION_TYPE_2 (BT_FN_LONGDOUBLE_LONGDOUBLE_LONG,
		     BT_LONGDOUBLE, BT_LONGDOUBLE, BT_LONG)
DEF_FUNCTION_TYPE_2 (BT_FN_INT_CONST_STRING_VALIST_ARG,
		     BT_INT, BT_CONST_STRING, BT_VALIST_ARG)
DEF_FUNCTION_TYPE_2 (BT_FN_PTR_SIZE_SIZE,
		     BT_PTR, BT_SIZE, BT_SIZE)
DEF_FUNCTION_TYPE_2 (BT_FN_PTR_PTR_SIZE,
		     BT_PTR, BT_PTR, BT_SIZE)
DEF_FUNCTION_TYPE_2 (BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT_COMPLEX_FLOAT,
		     BT_COMPLEX_FLOAT, BT_COMPLEX_FLOAT, BT_COMPLEX_FLOAT)
DEF_FUNCTION_TYPE_2 (BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE_COMPLEX_DOUBLE,
		     BT_COMPLEX_DOUBLE, BT_COMPLEX_DOUBLE, BT_COMPLEX_DOUBLE)
DEF_FUNCTION_TYPE_2 (BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE,
		     BT_COMPLEX_LONGDOUBLE, BT_COMPLEX_LONGDOUBLE, BT_COMPLEX_LONGDOUBLE)
DEF_FUNCTION_TYPE_2 (BT_FN_VOID_PTR_PTR, BT_VOID, BT_PTR, BT_PTR)
DEF_FUNCTION_TYPE_2 (BT_FN_INT_CONST_STRING_PTR_CONST_STRING,
		     BT_INT, BT_CONST_STRING, BT_PTR_CONST_STRING)
DEF_FUNCTION_TYPE_2 (BT_FN_SIZE_CONST_PTR_INT, BT_SIZE, BT_CONST_PTR, BT_INT)
DEF_FUNCTION_TYPE_2 (BT_FN_I1_VPTR_I1, BT_I1, BT_VOLATILE_PTR, BT_I1)
DEF_FUNCTION_TYPE_2 (BT_FN_I2_VPTR_I2, BT_I2, BT_VOLATILE_PTR, BT_I2)
DEF_FUNCTION_TYPE_2 (BT_FN_I4_VPTR_I4, BT_I4, BT_VOLATILE_PTR, BT_I4)
DEF_FUNCTION_TYPE_2 (BT_FN_I8_VPTR_I8, BT_I8, BT_VOLATILE_PTR, BT_I8)
DEF_FUNCTION_TYPE_2 (BT_FN_I16_VPTR_I16, BT_I16, BT_VOLATILE_PTR, BT_I16)
DEF_FUNCTION_TYPE_2 (BT_FN_BOOL_LONGPTR_LONGPTR,
		     BT_BOOL, BT_PTR_LONG, BT_PTR_LONG)
DEF_FUNCTION_TYPE_2 (BT_FN_BOOL_ULONGLONGPTR_ULONGLONGPTR,
		     BT_BOOL, BT_PTR_ULONGLONG, BT_PTR_ULONGLONG)

DEF_POINTER_TYPE (BT_PTR_FN_VOID_PTR_PTR, BT_FN_VOID_PTR_PTR)

DEF_FUNCTION_TYPE_3 (BT_FN_STRING_STRING_CONST_STRING_SIZE,
		     BT_STRING, BT_STRING, BT_CONST_STRING, BT_SIZE)
DEF_FUNCTION_TYPE_3 (BT_FN_INT_CONST_STRING_CONST_STRING_SIZE,
		     BT_INT, BT_CONST_STRING, BT_CONST_STRING, BT_SIZE)
DEF_FUNCTION_TYPE_3 (BT_FN_PTR_PTR_CONST_PTR_SIZE,
		     BT_PTR, BT_PTR, BT_CONST_PTR, BT_SIZE)
DEF_FUNCTION_TYPE_3 (BT_FN_INT_CONST_PTR_CONST_PTR_SIZE,
		     BT_INT, BT_CONST_PTR, BT_CONST_PTR, BT_SIZE)
DEF_FUNCTION_TYPE_3 (BT_FN_PTR_PTR_INT_SIZE,
		     BT_PTR, BT_PTR, BT_INT, BT_SIZE)
DEF_FUNCTION_TYPE_3 (BT_FN_VOID_PTR_INT_INT,
		     BT_VOID, BT_PTR, BT_INT, BT_INT)
DEF_FUNCTION_TYPE_3 (BT_FN_VOID_CONST_PTR_PTR_SIZE,
		     BT_VOID, BT_CONST_PTR, BT_PTR, BT_SIZE)
DEF_FUNCTION_TYPE_3 (BT_FN_INT_STRING_CONST_STRING_VALIST_ARG,
		     BT_INT, BT_STRING, BT_CONST_STRING, BT_VALIST_ARG)
DEF_FUNCTION_TYPE_3 (BT_FN_INT_CONST_STRING_CONST_STRING_VALIST_ARG,
		     BT_INT, BT_CONST_STRING, BT_CONST_STRING, BT_VALIST_ARG)
DEF_FUNCTION_TYPE_3 (BT_FN_INT_FILEPTR_CONST_STRING_VALIST_ARG,
		     BT_INT, BT_FILEPTR, BT_CONST_STRING, BT_VALIST_ARG)
DEF_FUNCTION_TYPE_3 (BT_FN_STRING_CONST_STRING_CONST_STRING_INT,
		     BT_STRING, BT_CONST_STRING, BT_CONST_STRING, BT_INT)
DEF_FUNCTION_TYPE_3 (BT_FN_FLOAT_FLOAT_FLOAT_FLOAT,
		     BT_FLOAT, BT_FLOAT, BT_FLOAT, BT_FLOAT)
DEF_FUNCTION_TYPE_3 (BT_FN_DOUBLE_DOUBLE_DOUBLE_DOUBLE,
		     BT_DOUBLE, BT_DOUBLE, BT_DOUBLE, BT_DOUBLE)
DEF_FUNCTION_TYPE_3 (BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE,
		     BT_LONGDOUBLE, BT_LONGDOUBLE, BT_LONGDOUBLE, BT_LONGDOUBLE)
DEF_FUNCTION_TYPE_3 (BT_FN_FLOAT_FLOAT_FLOAT_INTPTR,
		     BT_FLOAT, BT_FLOAT, BT_FLOAT, BT_INT_PTR)
DEF_FUNCTION_TYPE_3 (BT_FN_DOUBLE_DOUBLE_DOUBLE_INTPTR,
		     BT_DOUBLE, BT_DOUBLE, BT_DOUBLE, BT_INT_PTR)
DEF_FUNCTION_TYPE_3 (BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE_INTPTR,
		     BT_LONGDOUBLE, BT_LONGDOUBLE, BT_LONGDOUBLE, BT_INT_PTR)
DEF_FUNCTION_TYPE_3 (BT_FN_VOID_FLOAT_FLOATPTR_FLOATPTR,
		     BT_VOID, BT_FLOAT, BT_FLOAT_PTR, BT_FLOAT_PTR)
DEF_FUNCTION_TYPE_3 (BT_FN_VOID_DOUBLE_DOUBLEPTR_DOUBLEPTR,
		     BT_VOID, BT_DOUBLE, BT_DOUBLE_PTR, BT_DOUBLE_PTR)
DEF_FUNCTION_TYPE_3 (BT_FN_VOID_LONGDOUBLE_LONGDOUBLEPTR_LONGDOUBLEPTR,
		     BT_VOID, BT_LONGDOUBLE, BT_LONGDOUBLE_PTR, BT_LONGDOUBLE_PTR)
DEF_FUNCTION_TYPE_3 (BT_FN_VOID_PTR_PTR_PTR, BT_VOID, BT_PTR, BT_PTR, BT_PTR)
DEF_FUNCTION_TYPE_3 (BT_FN_INT_CONST_STRING_PTR_CONST_STRING_PTR_CONST_STRING,
		     BT_INT, BT_CONST_STRING, BT_PTR_CONST_STRING, BT_PTR_CONST_STRING)
DEF_FUNCTION_TYPE_3 (BT_FN_INT_INT_CONST_STRING_VALIST_ARG,
		     BT_INT, BT_INT, BT_CONST_STRING, BT_VALIST_ARG)
DEF_FUNCTION_TYPE_3 (BT_FN_BOOL_VPTR_I1_I1, BT_BOOL, BT_VOLATILE_PTR,
		     BT_I1, BT_I1)
DEF_FUNCTION_TYPE_3 (BT_FN_BOOL_VPTR_I2_I2, BT_BOOL, BT_VOLATILE_PTR,
		     BT_I2, BT_I2)
DEF_FUNCTION_TYPE_3 (BT_FN_BOOL_VPTR_I4_I4, BT_BOOL, BT_VOLATILE_PTR,
		     BT_I4, BT_I4)
DEF_FUNCTION_TYPE_3 (BT_FN_BOOL_VPTR_I8_I8, BT_BOOL, BT_VOLATILE_PTR,
		     BT_I8, BT_I8)
DEF_FUNCTION_TYPE_3 (BT_FN_BOOL_VPTR_I16_I16, BT_BOOL, BT_VOLATILE_PTR,
		     BT_I16, BT_I16)
DEF_FUNCTION_TYPE_3 (BT_FN_I1_VPTR_I1_I1, BT_I1, BT_VOLATILE_PTR, BT_I1, BT_I1)
DEF_FUNCTION_TYPE_3 (BT_FN_I2_VPTR_I2_I2, BT_I2, BT_VOLATILE_PTR, BT_I2, BT_I2)
DEF_FUNCTION_TYPE_3 (BT_FN_I4_VPTR_I4_I4, BT_I4, BT_VOLATILE_PTR, BT_I4, BT_I4)
DEF_FUNCTION_TYPE_3 (BT_FN_I8_VPTR_I8_I8, BT_I8, BT_VOLATILE_PTR, BT_I8, BT_I8)
DEF_FUNCTION_TYPE_3 (BT_FN_I16_VPTR_I16_I16, BT_I16, BT_VOLATILE_PTR,
		     BT_I16, BT_I16)
DEF_FUNCTION_TYPE_3 (BT_FN_VOID_OMPFN_PTR_UINT, BT_VOID, BT_PTR_FN_VOID_PTR,
		     BT_PTR, BT_UINT)
DEF_FUNCTION_TYPE_3 (BT_FN_PTR_CONST_PTR_INT_SIZE, BT_PTR,
		     BT_CONST_PTR, BT_INT, BT_SIZE)

DEF_FUNCTION_TYPE_4 (BT_FN_SIZE_CONST_PTR_SIZE_SIZE_FILEPTR,
		     BT_SIZE, BT_CONST_PTR, BT_SIZE, BT_SIZE, BT_FILEPTR)
DEF_FUNCTION_TYPE_4 (BT_FN_INT_STRING_SIZE_CONST_STRING_VALIST_ARG,
		BT_INT, BT_STRING, BT_SIZE, BT_CONST_STRING, BT_VALIST_ARG)
DEF_FUNCTION_TYPE_4 (BT_FN_SIZE_STRING_SIZE_CONST_STRING_CONST_PTR,
		BT_SIZE, BT_STRING, BT_SIZE, BT_CONST_STRING, BT_CONST_PTR)
DEF_FUNCTION_TYPE_4 (BT_FN_PTR_PTR_CONST_PTR_SIZE_SIZE,
		     BT_PTR, BT_PTR, BT_CONST_PTR, BT_SIZE, BT_SIZE)
DEF_FUNCTION_TYPE_4 (BT_FN_PTR_PTR_INT_SIZE_SIZE,
		     BT_PTR, BT_PTR, BT_INT, BT_SIZE, BT_SIZE)
DEF_FUNCTION_TYPE_4 (BT_FN_STRING_STRING_CONST_STRING_SIZE_SIZE,
		     BT_STRING, BT_STRING, BT_CONST_STRING, BT_SIZE, BT_SIZE)
DEF_FUNCTION_TYPE_4 (BT_FN_INT_FILEPTR_INT_CONST_STRING_VALIST_ARG,
		     BT_INT, BT_FILEPTR, BT_INT, BT_CONST_STRING, BT_VALIST_ARG)
DEF_FUNCTION_TYPE_4 (BT_FN_VOID_OMPFN_PTR_UINT_UINT,
		     BT_VOID, BT_PTR_FN_VOID_PTR, BT_PTR, BT_UINT, BT_UINT)
DEF_FUNCTION_TYPE_4 (BT_FN_VOID_PTR_WORD_WORD_PTR,
		     BT_VOID, BT_PTR, BT_WORD, BT_WORD, BT_PTR)

DEF_FUNCTION_TYPE_5 (BT_FN_INT_STRING_INT_SIZE_CONST_STRING_VALIST_ARG,
		     BT_INT, BT_STRING, BT_INT, BT_SIZE, BT_CONST_STRING,
		     BT_VALIST_ARG)
DEF_FUNCTION_TYPE_5 (BT_FN_BOOL_LONG_LONG_LONG_LONGPTR_LONGPTR,
		     BT_BOOL, BT_LONG, BT_LONG, BT_LONG,
		     BT_PTR_LONG, BT_PTR_LONG)

DEF_FUNCTION_TYPE_6 (BT_FN_INT_STRING_SIZE_INT_SIZE_CONST_STRING_VALIST_ARG,
		     BT_INT, BT_STRING, BT_SIZE, BT_INT, BT_SIZE,
		     BT_CONST_STRING, BT_VALIST_ARG)
DEF_FUNCTION_TYPE_6 (BT_FN_BOOL_LONG_LONG_LONG_LONG_LONGPTR_LONGPTR,
		     BT_BOOL, BT_LONG, BT_LONG, BT_LONG, BT_LONG,
		     BT_PTR_LONG, BT_PTR_LONG)
DEF_FUNCTION_TYPE_6 (BT_FN_VOID_OMPFN_PTR_UINT_LONG_LONG_LONG,
		     BT_VOID, BT_PTR_FN_VOID_PTR, BT_PTR, BT_UINT,
		     BT_LONG, BT_LONG, BT_LONG)
DEF_FUNCTION_TYPE_6 (BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		     BT_BOOL, BT_BOOL, BT_ULONGLONG, BT_ULONGLONG,
		     BT_ULONGLONG, BT_PTR_ULONGLONG, BT_PTR_ULONGLONG)

DEF_FUNCTION_TYPE_7 (BT_FN_VOID_OMPFN_PTR_UINT_LONG_LONG_LONG_LONG,
		     BT_VOID, BT_PTR_FN_VOID_PTR, BT_PTR, BT_UINT,
		     BT_LONG, BT_LONG, BT_LONG, BT_LONG)
DEF_FUNCTION_TYPE_7 (BT_FN_VOID_OMPFN_PTR_OMPCPYFN_LONG_LONG_BOOL_UINT,
		     BT_VOID, BT_PTR_FN_VOID_PTR, BT_PTR,
		     BT_PTR_FN_VOID_PTR_PTR, BT_LONG, BT_LONG,
		     BT_BOOL, BT_UINT)
DEF_FUNCTION_TYPE_7 (BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		     BT_BOOL, BT_BOOL, BT_ULONGLONG, BT_ULONGLONG,
		     BT_ULONGLONG, BT_ULONGLONG,
		     BT_PTR_ULONGLONG, BT_PTR_ULONGLONG)

DEF_FUNCTION_TYPE_VAR_0 (BT_FN_VOID_VAR, BT_VOID)
DEF_FUNCTION_TYPE_VAR_0 (BT_FN_INT_VAR, BT_INT)
DEF_FUNCTION_TYPE_VAR_0 (BT_FN_PTR_VAR, BT_PTR)

DEF_FUNCTION_TYPE_VAR_1 (BT_FN_VOID_VALIST_REF_VAR,
			 BT_VOID, BT_VALIST_REF)
DEF_FUNCTION_TYPE_VAR_1 (BT_FN_VOID_CONST_PTR_VAR,
			 BT_VOID, BT_CONST_PTR)
DEF_FUNCTION_TYPE_VAR_1 (BT_FN_INT_CONST_STRING_VAR,
			 BT_INT, BT_CONST_STRING)

DEF_FUNCTION_TYPE_VAR_2 (BT_FN_INT_FILEPTR_CONST_STRING_VAR,
			 BT_INT, BT_FILEPTR, BT_CONST_STRING)
DEF_FUNCTION_TYPE_VAR_2 (BT_FN_INT_STRING_CONST_STRING_VAR,
			 BT_INT, BT_STRING, BT_CONST_STRING)
DEF_FUNCTION_TYPE_VAR_2 (BT_FN_INT_CONST_STRING_CONST_STRING_VAR,
			 BT_INT, BT_CONST_STRING, BT_CONST_STRING)
DEF_FUNCTION_TYPE_VAR_2 (BT_FN_INT_INT_CONST_STRING_VAR,
			 BT_INT, BT_INT, BT_CONST_STRING)

DEF_FUNCTION_TYPE_VAR_3 (BT_FN_INT_STRING_SIZE_CONST_STRING_VAR,
			 BT_INT, BT_STRING, BT_SIZE, BT_CONST_STRING)
DEF_FUNCTION_TYPE_VAR_3 (BT_FN_SSIZE_STRING_SIZE_CONST_STRING_VAR,
			 BT_SSIZE, BT_STRING, BT_SIZE, BT_CONST_STRING)
DEF_FUNCTION_TYPE_VAR_3 (BT_FN_INT_FILEPTR_INT_CONST_STRING_VAR,
			 BT_INT, BT_FILEPTR, BT_INT, BT_CONST_STRING)

DEF_FUNCTION_TYPE_VAR_4 (BT_FN_INT_STRING_INT_SIZE_CONST_STRING_VAR,
			 BT_INT, BT_STRING, BT_INT, BT_SIZE, BT_CONST_STRING)

DEF_FUNCTION_TYPE_VAR_5 (BT_FN_INT_STRING_SIZE_INT_SIZE_CONST_STRING_VAR,
			 BT_INT, BT_STRING, BT_SIZE, BT_INT, BT_SIZE,
			 BT_CONST_STRING)

DEF_FUNCTION_TYPE_VAR_5 (BT_FN_INT_INT_INT_INT_INT_INT_VAR,
			 BT_INT, BT_INT, BT_INT, BT_INT, BT_INT, BT_INT)

DEF_POINTER_TYPE (BT_PTR_FN_VOID_VAR, BT_FN_VOID_VAR)
DEF_FUNCTION_TYPE_3 (BT_FN_PTR_PTR_FN_VOID_VAR_PTR_SIZE,
		     BT_PTR, BT_PTR_FN_VOID_VAR, BT_PTR, BT_SIZE)

/*
 * Bultins
 */

/* 
   From GCC
   --------

   Before including this file, you should define a macro:

     DEF_BUILTIN (ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P,
                  FALLBACK_P, NONANSI_P, ATTRS, IMPLICIT)

   This macro will be called once for each builtin function.  The
   ENUM will be of type `enum built_in_function', and will indicate
   which builtin function is being processed.  The NAME of the builtin
   function (which will always start with `__builtin_') is a string
   literal.  The CLASS is of type `enum built_in_class' and indicates
   what kind of builtin is being processed.

   Some builtins are actually two separate functions.  For example,
   for `strcmp' there are two builtin functions; `__builtin_strcmp'
   and `strcmp' itself.  Both behave identically.  Other builtins
   define only the `__builtin' variant.  If BOTH_P is TRUE, then this
   builtin has both variants; otherwise, it is has only the first
   variant.

   TYPE indicates the type of the function.  The symbols correspond to
   enumerals from builtin-types.def.  If BOTH_P is true, then LIBTYPE
   is the type of the non-`__builtin_' variant.  Otherwise, LIBTYPE
   should be ignored.

   If FALLBACK_P is true then, if for some reason, the compiler cannot
   expand the builtin function directly, it will call the
   corresponding library function (which does not have the
   `__builtin_' prefix.

   If NONANSI_P is true, then the non-`__builtin_' variant is not an
   ANSI/ISO library function, and so we should pretend it does not
   exist when compiling in ANSI conformant mode.

   ATTRs is an attribute list as defined in builtin-attrs.def that
   describes the attributes of this builtin function.  

   IMPLICIT specifies condition when the builtin can be produced by
   compiler.  For instance C90 reserves floorf function, but does not
   define it's meaning.  When user uses floorf we may assume that the
   floorf has the meaning we expect, but we can't produce floorf by
   simplifying floor((double)float) since the runtime need not implement
   it.  */

// DEF_PRIMITIVE_TYPE(BT_LAST, get_void_type())
DEF_FUNCTION_TYPE_0(0, BT_VOID)

static default_argument_info_t** empty_default_argument_info(int num_parameters)
{
    // FIXME - Not counted!
    return calloc(sizeof(default_argument_info_t*), num_parameters);
}

#define  DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P, \
                   FALLBACK_P, NONANSI_P, ATTRS, IMPLICIT, _X) \
  { \
      scope_entry_t* new_builtin = new_symbol(global_context, global_context.global_scope, NAME); \
      new_builtin->kind = SK_FUNCTION; \
      new_builtin->type_information = (__mcxx_builtin_type__##TYPE)(); \
      new_builtin->entity_specs.is_builtin = 1; \
      new_builtin->do_not_print = 1; \
      new_builtin->file = "(builtin-function)"; \
      new_builtin->line = 0; \
      new_builtin->entity_specs.num_parameters = function_type_get_num_parameters(new_builtin->type_information); \
      new_builtin->entity_specs.default_argument_info = empty_default_argument_info(new_builtin->entity_specs.num_parameters); \
      /* DEBUG_CODE() */ \
      /* { */ \
      /*     fprintf(stderr, "GCC-BUILTIN: Registered gcc-builtin '%s' with type '%s'\n", NAME, */ \
      /*             print_declarator(new_builtin->type_information, global_context)); */ \
      /* } */ \
  }

/* A GCC builtin(like __builtin_saveregs) is provided by the
   compiler, but does not correspond to a function in the standard
   library.  */
#undef DEF_GCC_BUILTIN
#define DEF_GCC_BUILTIN(ENUM, NAME, TYPE, ATTRS)        \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, BT_LAST,  \
               false, false, false, ATTRS, true, true)

/* Like DEF_GCC_BUILTIN, except we don't prepend "__builtin_".  */
#undef DEF_SYNC_BUILTIN
#define DEF_SYNC_BUILTIN(ENUM, NAME, TYPE, ATTRS)       \
  DEF_BUILTIN(ENUM, NAME, BUILT_IN_NORMAL, TYPE, BT_LAST,   \
               false, false, false, ATTRS, true, true)

/* A library builtin(like __builtin_strchr) is a builtin equivalent
   of an ANSI/ISO standard library function.  In addition to the
   `__builtin' version, we will create an ordinary version(e.g,
   `strchr') as well.  If we cannot compute the answer using the
   builtin function, we will fall back to the standard library
   version.  */
#undef DEF_LIB_BUILTIN                  
#define DEF_LIB_BUILTIN(ENUM, NAME, TYPE, ATTRS)    \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           true, true, false, ATTRS, true, true)

/* Like DEF_LIB_BUILTIN, except that the function is not one that is
   specified by ANSI/ISO C.  So, when we're being fully conformant we
   ignore the version of these builtins that does not begin with
   __builtin.  */
#undef DEF_EXT_LIB_BUILTIN              
#define DEF_EXT_LIB_BUILTIN(ENUM, NAME, TYPE, ATTRS)    \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           true, true, true, ATTRS, false, true)

/* Like DEF_LIB_BUILTIN, except that the function is only a part of
   the standard in C94 or above.  */
#undef DEF_C94_BUILTIN                  
#define DEF_C94_BUILTIN(ENUM, NAME, TYPE, ATTRS)    \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           true, true, !flag_isoc94, ATTRS, TARGET_C99_FUNCTIONS, true)

/* Like DEF_LIB_BUILTIN, except that the function is only a part of
   the standard in C99 or above.  */
#undef DEF_C99_BUILTIN                  
#define DEF_C99_BUILTIN(ENUM, NAME, TYPE, ATTRS)    \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           true, true, !flag_isoc99, ATTRS, TARGET_C99_FUNCTIONS, true)

/* Builtin that is specified by C99 and C90 reserve the name for future use.
   We can still recognize the builtin in C90 mode but we can't produce it
   implicitly.  */
#undef DEF_C99_C90RES_BUILTIN                   
#define DEF_C99_C90RES_BUILTIN(ENUM, NAME, TYPE, ATTRS) \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           true, true, !flag_isoc99, ATTRS, TARGET_C99_FUNCTIONS, true)

/* Builtin that C99 reserve the name for future use. We can still recognize
   the builtin in C99 mode but we can't produce it implicitly.  */
#undef DEF_EXT_C99RES_BUILTIN
#define DEF_EXT_C99RES_BUILTIN(ENUM, NAME, TYPE, ATTRS)        \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE,   \
              true, true, true, ATTRS, false, true)

/* Allocate the enum and the name for a builtin, but do not actually
   define it here at all.  */
#undef DEF_BUILTIN_STUB
#define DEF_BUILTIN_STUB(ENUM, NAME) \
  DEF_BUILTIN(ENUM, NAME, BUILT_IN_NORMAL, 0, 0, false, false, \
           false, 0, false, false)

/* Define an attribute list for math functions that are normally
   "impure" because some of them may write into global memory for
   `errno'.  If !flag_errno_math they are instead "const".  */
#undef ATTR_MATHFN_ERRNO
#define ATTR_MATHFN_ERRNO (1 << 0)
// #define ATTR_MATHFN_ERRNO(flag_errno_math ? ATTR_NOTHROW_LIST : ATTR_CONST_NOTHROW_LIST)

/* Define an attribute list for math functions that are normally
   "pure" but if flag_unsafe_math_optimizations is set they are
   instead "const".  This distinction accounts for the fact that some
   math functions check the rounding mode which is akin to examining
   global memory.  In "unsafe" mode we can be less careful.  */
#undef ATTR_MATHFN_FPROUNDING
#define ATTR_MATHFN_FPROUNDING (1 << 1)
// #define ATTR_MATHFN_FPROUNDING(flag_unsafe_math_optimizations ? ATTR_CONST_NOTHROW_LIST : ATTR_PURE_NOTHROW_NOVOPS_LIST)

/* Define an attribute list for math functions that are normally
   "impure" because some of them may write into global memory for
   `errno'.  If !flag_errno_math, we can possibly use "pure" or
   "const" depending on whether we care about FP rounding.  */
#undef ATTR_MATHFN_FPROUNDING_ERRNO
#define ATTR_MATHFN_FPROUNDING_ERRNO  (1 << 2)
// #define ATTR_MATHFN_FPROUNDING_ERRNO(flag_errno_math ? ATTR_NOTHROW_LIST : ATTR_MATHFN_FPROUNDING)

/* Define an attribute list for math functions that need to mind FP
   rounding, but because they store into memory they are never "const"
   or "pure".  Use of this macro is mainly for documentation and
   maintenance purposes.  */
#undef ATTR_MATHFN_FPROUNDING_STORE
#define ATTR_MATHFN_FPROUNDING_STORE ATTR_NOTHROW_LIST

void gcc_sign_in_builtins(decl_context_t global_context)
{ 

/* Category: math builtins.  */
DEF_LIB_BUILTIN        (BUILT_IN_ACOS, "acos", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ACOSF, "acosf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ACOSH, "acosh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ACOSHF, "acoshf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ACOSHL, "acoshl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ACOSL, "acosl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_ASIN, "asin", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ASINF, "asinf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ASINH, "asinh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_ASINHF, "asinhf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_ASINHL, "asinhl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ASINL, "asinl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_ATAN, "atan", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_LIB_BUILTIN        (BUILT_IN_ATAN2, "atan2", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ATAN2F, "atan2f", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ATAN2L, "atan2l", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ATANF, "atanf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_ATANH, "atanh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ATANHF, "atanhf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ATANHL, "atanhl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ATANL, "atanl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CBRT, "cbrt", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CBRTF, "cbrtf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CBRTL, "cbrtl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_LIB_BUILTIN        (BUILT_IN_CEIL, "ceil", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_C90RES_BUILTIN (BUILT_IN_CEILF, "ceilf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_C90RES_BUILTIN (BUILT_IN_CEILL, "ceill", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_COPYSIGN, "copysign", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_COPYSIGNF, "copysignf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_COPYSIGNL, "copysignl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_COS, "cos", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_C90RES_BUILTIN (BUILT_IN_COSF, "cosf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_LIB_BUILTIN        (BUILT_IN_COSH, "cosh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_COSHF, "coshf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_COSHL, "coshl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_COSL, "cosl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DREM, "drem", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DREMF, "dremf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DREML, "dreml", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ERF, "erf", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_ERFC, "erfc", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ERFCF, "erfcf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ERFCL, "erfcl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ERFF, "erff", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_ERFL, "erfl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_LIB_BUILTIN        (BUILT_IN_EXP, "exp", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_EXP10, "exp10", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_EXP10F, "exp10f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_EXP10L, "exp10l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_EXP2, "exp2", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_EXP2F, "exp2f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_EXP2L, "exp2l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_EXPF, "expf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_EXPL, "expl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_EXPM1, "expm1", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_EXPM1F, "expm1f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_EXPM1L, "expm1l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_FABS, "fabs", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FABSF, "fabsf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FABSL, "fabsl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_FDIM, "fdim", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_FDIMF, "fdimf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_FDIML, "fdiml", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_FLOOR, "floor", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FLOORF, "floorf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FLOORL, "floorl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_FMA, "fma", BT_FN_DOUBLE_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_FMAF, "fmaf", BT_FN_FLOAT_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_FMAL, "fmal", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_FMAX, "fmax", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_FMAXF, "fmaxf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_FMAXL, "fmaxl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_FMIN, "fmin", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_FMINF, "fminf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_FMINL, "fminl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_FMOD, "fmod", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FMODF, "fmodf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FMODL, "fmodl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_FREXP, "frexp", BT_FN_DOUBLE_DOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FREXPF, "frexpf", BT_FN_FLOAT_FLOAT_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FREXPL, "frexpl", BT_FN_LONGDOUBLE_LONGDOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMA, "gamma", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMAF, "gammaf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMAL, "gammal", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMA_R, "gamma_r", BT_FN_DOUBLE_DOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMAF_R, "gammaf_r", BT_FN_FLOAT_FLOAT_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMAL_R, "gammal_r", BT_FN_LONGDOUBLE_LONGDOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_GCC_BUILTIN        (BUILT_IN_HUGE_VAL, "huge_val", BT_FN_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_HUGE_VALF, "huge_valf", BT_FN_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_HUGE_VALL, "huge_vall", BT_FN_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_HYPOT, "hypot", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_HYPOTF, "hypotf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_HYPOTL, "hypotl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ILOGB, "ilogb", BT_FN_INT_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ILOGBF, "ilogbf", BT_FN_INT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ILOGBL, "ilogbl", BT_FN_INT_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_GCC_BUILTIN        (BUILT_IN_INF, "inf", BT_FN_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_INFF, "inff", BT_FN_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_INFL, "infl", BT_FN_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN	       (BUILT_IN_INFD32, "infd32", BT_FN_DFLOAT32, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_INFD64, "infd64", BT_FN_DFLOAT64, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_INFD128, "infd128", BT_FN_DFLOAT128, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J0, "j0", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J0F, "j0f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J0L, "j0l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J1, "j1", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J1F, "j1f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J1L, "j1l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_JN, "jn", BT_FN_DOUBLE_INT_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_JNF, "jnf", BT_FN_FLOAT_INT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_JNL, "jnl", BT_FN_LONGDOUBLE_INT_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_GCC_BUILTIN        (BUILT_IN_LCEIL, "lceil", BT_FN_LONG_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LCEILF, "lceilf", BT_FN_LONG_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LCEILL, "lceill", BT_FN_LONG_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_LDEXP, "ldexp", BT_FN_DOUBLE_DOUBLE_INT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LDEXPF, "ldexpf", BT_FN_FLOAT_FLOAT_INT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LDEXPL, "ldexpl", BT_FN_LONGDOUBLE_LONGDOUBLE_INT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_GCC_BUILTIN        (BUILT_IN_LFLOOR, "lfloor", BT_FN_LONG_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LFLOORF, "lfloorf", BT_FN_LONG_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LFLOORL, "lfloorl", BT_FN_LONG_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_LGAMMA, "lgamma", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_BUILTIN        (BUILT_IN_LGAMMAF, "lgammaf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_BUILTIN        (BUILT_IN_LGAMMAL, "lgammal", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_LGAMMA_R, "lgamma_r", BT_FN_DOUBLE_DOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_LGAMMAF_R, "lgammaf_r", BT_FN_FLOAT_FLOAT_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_LGAMMAL_R, "lgammal_r", BT_FN_LONGDOUBLE_LONGDOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_GCC_BUILTIN        (BUILT_IN_LLCEIL, "llceil", BT_FN_LONGLONG_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LLCEILF, "llceilf", BT_FN_LONGLONG_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LLCEILL, "llceill", BT_FN_LONGLONG_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LLFLOOR, "llfloor", BT_FN_LONGLONG_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LLFLOORF, "llfloorf", BT_FN_LONGLONG_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LLFLOORL, "llfloorl", BT_FN_LONGLONG_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_LLRINT, "llrint", BT_FN_LONGLONG_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LLRINTF, "llrintf", BT_FN_LONGLONG_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LLRINTL, "llrintl", BT_FN_LONGLONG_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LLROUND, "llround", BT_FN_LONGLONG_DOUBLE, ATTR_MATHFN_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LLROUNDF, "llroundf", BT_FN_LONGLONG_FLOAT, ATTR_MATHFN_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LLROUNDL, "llroundl", BT_FN_LONGLONG_LONGDOUBLE, ATTR_MATHFN_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_LOG, "log", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_LOG10, "log10", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LOG10F, "log10f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LOG10L, "log10l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LOG1P, "log1p", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LOG1PF, "log1pf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LOG1PL, "log1pl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LOG2, "log2", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LOG2F, "log2f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LOG2L, "log2l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LOGB, "logb", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LOGBF, "logbf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LOGBL, "logbl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LOGF, "logf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LOGL, "logl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LRINT, "lrint", BT_FN_LONG_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LRINTF, "lrintf", BT_FN_LONG_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LRINTL, "lrintl", BT_FN_LONG_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LROUND, "lround", BT_FN_LONG_DOUBLE, ATTR_MATHFN_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LROUNDF, "lroundf", BT_FN_LONG_FLOAT, ATTR_MATHFN_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_LROUNDL, "lroundl", BT_FN_LONG_LONGDOUBLE, ATTR_MATHFN_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_MODF, "modf", BT_FN_DOUBLE_DOUBLE_DOUBLEPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_C90RES_BUILTIN (BUILT_IN_MODFF, "modff", BT_FN_FLOAT_FLOAT_FLOATPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_C90RES_BUILTIN (BUILT_IN_MODFL, "modfl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLEPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_BUILTIN        (BUILT_IN_NAN, "nan", BT_FN_DOUBLE_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL)
DEF_C99_BUILTIN        (BUILT_IN_NANF, "nanf", BT_FN_FLOAT_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL)
DEF_C99_BUILTIN        (BUILT_IN_NANL, "nanl", BT_FN_LONGDOUBLE_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL)
DEF_GCC_BUILTIN        (BUILT_IN_NAND32, "nand32", BT_FN_DFLOAT32_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL)
DEF_GCC_BUILTIN        (BUILT_IN_NAND64, "nand64", BT_FN_DFLOAT64_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL)
DEF_GCC_BUILTIN        (BUILT_IN_NAND128, "nand128", BT_FN_DFLOAT128_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL)
DEF_GCC_BUILTIN        (BUILT_IN_NANS, "nans", BT_FN_DOUBLE_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL)
DEF_GCC_BUILTIN        (BUILT_IN_NANSF, "nansf", BT_FN_FLOAT_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL)
DEF_GCC_BUILTIN        (BUILT_IN_NANSL, "nansl", BT_FN_LONGDOUBLE_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL)
DEF_C99_BUILTIN        (BUILT_IN_NEARBYINT, "nearbyint", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_NEARBYINTF, "nearbyintf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_NEARBYINTL, "nearbyintl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_NEXTAFTER, "nextafter", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_NEXTAFTERF, "nextafterf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_NEXTAFTERL, "nextafterl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_NEXTTOWARD, "nexttoward", BT_FN_DOUBLE_DOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_NEXTTOWARDF, "nexttowardf", BT_FN_FLOAT_FLOAT_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_NEXTTOWARDL, "nexttowardl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_POW, "pow", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_POW10, "pow10", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_POW10F, "pow10f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_POW10L, "pow10l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_POWF, "powf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_GCC_BUILTIN        (BUILT_IN_POWI, "powi", BT_FN_DOUBLE_DOUBLE_INT, ATTR_MATHFN_FPROUNDING)
DEF_GCC_BUILTIN        (BUILT_IN_POWIF, "powif", BT_FN_FLOAT_FLOAT_INT, ATTR_MATHFN_FPROUNDING)
DEF_GCC_BUILTIN        (BUILT_IN_POWIL, "powil", BT_FN_LONGDOUBLE_LONGDOUBLE_INT, ATTR_MATHFN_FPROUNDING)
DEF_C99_C90RES_BUILTIN (BUILT_IN_POWL, "powl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_REMAINDER, "remainder", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_REMAINDERF, "remainderf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_REMAINDERL, "remainderl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_REMQUO, "remquo", BT_FN_DOUBLE_DOUBLE_DOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_BUILTIN        (BUILT_IN_REMQUOF, "remquof", BT_FN_FLOAT_FLOAT_FLOAT_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_BUILTIN        (BUILT_IN_REMQUOL, "remquol", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_BUILTIN        (BUILT_IN_RINT, "rint", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_RINTF, "rintf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_RINTL, "rintl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_ROUND, "round", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_ROUNDF, "roundf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_ROUNDL, "roundl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SCALB, "scalb", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SCALBF, "scalbf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SCALBL, "scalbl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_SCALBLN, "scalbln", BT_FN_DOUBLE_DOUBLE_LONG, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_SCALBLNF, "scalblnf", BT_FN_FLOAT_FLOAT_LONG, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_SCALBLNL, "scalblnl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONG, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_SCALBN, "scalbn", BT_FN_DOUBLE_DOUBLE_INT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_SCALBNF, "scalbnf", BT_FN_FLOAT_FLOAT_INT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_SCALBNL, "scalbnl", BT_FN_LONGDOUBLE_LONGDOUBLE_INT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBIT, "signbit", BT_FN_INT_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITF, "signbitf", BT_FN_INT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITL, "signbitl", BT_FN_INT_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITD32, "signbitd32", BT_FN_INT_DFLOAT32, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITD64, "signbitd64", BT_FN_INT_DFLOAT64, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITD128, "signbitd128", BT_FN_INT_DFLOAT128, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNIFICAND, "significand", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNIFICANDF, "significandf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNIFICANDL, "significandl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_SIN, "sin", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SINCOS, "sincos", BT_FN_VOID_DOUBLE_DOUBLEPTR_DOUBLEPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SINCOSF, "sincosf", BT_FN_VOID_FLOAT_FLOATPTR_FLOATPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SINCOSL, "sincosl", BT_FN_VOID_LONGDOUBLE_LONGDOUBLEPTR_LONGDOUBLEPTR, ATTR_MATHFN_FPROUNDING_STORE)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SINF, "sinf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_LIB_BUILTIN        (BUILT_IN_SINH, "sinh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SINHF, "sinhf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SINHL, "sinhl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SINL, "sinl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_LIB_BUILTIN        (BUILT_IN_SQRT, "sqrt", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SQRTF, "sqrtf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SQRTL, "sqrtl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_LIB_BUILTIN        (BUILT_IN_TAN, "tan", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_C90RES_BUILTIN (BUILT_IN_TANF, "tanf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_LIB_BUILTIN        (BUILT_IN_TANH, "tanh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_C90RES_BUILTIN (BUILT_IN_TANHF, "tanhf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_C90RES_BUILTIN (BUILT_IN_TANHL, "tanhl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_C90RES_BUILTIN (BUILT_IN_TANL, "tanl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_TGAMMA, "tgamma", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_TGAMMAF, "tgammaf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_TGAMMAL, "tgammal", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_C99_BUILTIN        (BUILT_IN_TRUNC, "trunc", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_TRUNCF, "truncf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_TRUNCL, "truncl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y0, "y0", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y0F, "y0f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y0L, "y0l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y1, "y1", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y1F, "y1f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y1L, "y1l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_YN, "yn", BT_FN_DOUBLE_INT_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_YNF, "ynf", BT_FN_FLOAT_INT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_YNL, "ynl", BT_FN_LONGDOUBLE_INT_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO)

/* Category: _Complex math builtins.  */
DEF_C99_BUILTIN        (BUILT_IN_CABS, "cabs", BT_FN_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CABSF, "cabsf", BT_FN_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CABSL, "cabsl", BT_FN_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CACOS, "cacos", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CACOSF, "cacosf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CACOSH, "cacosh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CACOSHF, "cacoshf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CACOSHL, "cacoshl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CACOSL, "cacosl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CARG, "carg", BT_FN_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CARGF, "cargf", BT_FN_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CARGL, "cargl", BT_FN_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CASIN, "casin", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CASINF, "casinf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CASINH, "casinh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CASINHF, "casinhf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CASINHL, "casinhl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CASINL, "casinl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CATAN, "catan", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CATANF, "catanf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CATANH, "catanh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CATANHF, "catanhf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CATANHL, "catanhl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CATANL, "catanl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CCOS, "ccos", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CCOSF, "ccosf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CCOSH, "ccosh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CCOSHF, "ccoshf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CCOSHL, "ccoshl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CCOSL, "ccosl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CEXP, "cexp", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CEXPF, "cexpf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CEXPL, "cexpl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_GCC_BUILTIN        (BUILT_IN_CEXPI, "cexpi", BT_FN_COMPLEX_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_GCC_BUILTIN        (BUILT_IN_CEXPIF, "cexpif", BT_FN_COMPLEX_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_GCC_BUILTIN        (BUILT_IN_CEXPIL, "cexpil", BT_FN_COMPLEX_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CIMAG, "cimag", BT_FN_DOUBLE_COMPLEX_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_CIMAGF, "cimagf", BT_FN_FLOAT_COMPLEX_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_CIMAGL, "cimagl", BT_FN_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_CLOG, "clog", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CLOGF, "clogf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CLOGL, "clogl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_EXT_C99RES_BUILTIN (BUILT_IN_CLOG10, "clog10", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_EXT_C99RES_BUILTIN (BUILT_IN_CLOG10F, "clog10f", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_EXT_C99RES_BUILTIN (BUILT_IN_CLOG10L, "clog10l", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CONJ, "conj", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_CONJF, "conjf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_CONJL, "conjl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_CPOW, "cpow", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CPOWF, "cpowf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CPOWL, "cpowl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CPROJ, "cproj", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CPROJF, "cprojf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CPROJL, "cprojl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CREAL, "creal", BT_FN_DOUBLE_COMPLEX_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_CREALF, "crealf", BT_FN_FLOAT_COMPLEX_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_CREALL, "creall", BT_FN_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_CSIN, "csin", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CSINF, "csinf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CSINH, "csinh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CSINHF, "csinhf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CSINHL, "csinhl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CSINL, "csinl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CSQRT, "csqrt", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CSQRTF, "csqrtf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CSQRTL, "csqrtl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CTAN, "ctan", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CTANF, "ctanf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CTANH, "ctanh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CTANHF, "ctanhf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CTANHL, "ctanhl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)
DEF_C99_BUILTIN        (BUILT_IN_CTANL, "ctanl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING)

/* Category: string/memory builtins.  */
/* bcmp, bcopy and bzero have traditionally accepted NULL pointers
   when the length parameter is zero, so don't apply attribute "nonnull".  */
DEF_EXT_LIB_BUILTIN    (BUILT_IN_BCMP, "bcmp", BT_FN_INT_CONST_PTR_CONST_PTR_SIZE, ATTR_PURE_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_BCOPY, "bcopy", BT_FN_VOID_CONST_PTR_PTR_SIZE, ATTR_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_BZERO, "bzero", BT_FN_VOID_PTR_SIZE, ATTR_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_INDEX, "index", BT_FN_STRING_CONST_STRING_INT, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_MEMCHR, "memchr", BT_FN_PTR_CONST_PTR_INT_SIZE, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_MEMCMP, "memcmp", BT_FN_INT_CONST_PTR_CONST_PTR_SIZE, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_MEMCPY, "memcpy", BT_FN_PTR_PTR_CONST_PTR_SIZE, ATTR_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_MEMMOVE, "memmove", BT_FN_PTR_PTR_CONST_PTR_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMPCPY, "mempcpy", BT_FN_PTR_PTR_CONST_PTR_SIZE, ATTR_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_MEMSET, "memset", BT_FN_PTR_PTR_INT_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_RINDEX, "rindex", BT_FN_STRING_CONST_STRING_INT, ATTR_PURE_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STPCPY, "stpcpy", BT_FN_STRING_STRING_CONST_STRING, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STPNCPY, "stpncpy", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRCASECMP, "strcasecmp", BT_FN_INT_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRCAT, "strcat", BT_FN_STRING_STRING_CONST_STRING, ATTR_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRCHR, "strchr", BT_FN_STRING_CONST_STRING_INT, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRCMP, "strcmp", BT_FN_INT_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRCPY, "strcpy", BT_FN_STRING_STRING_CONST_STRING, ATTR_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRCSPN, "strcspn", BT_FN_SIZE_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRDUP, "strdup", BT_FN_STRING_CONST_STRING, ATTR_MALLOC_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRNDUP, "strndup", BT_FN_STRING_CONST_STRING_SIZE, ATTR_MALLOC_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRLEN, "strlen", BT_FN_SIZE_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRNCASECMP, "strncasecmp", BT_FN_INT_CONST_STRING_CONST_STRING_SIZE, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRNCAT, "strncat", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRNCMP, "strncmp", BT_FN_INT_CONST_STRING_CONST_STRING_SIZE, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRNCPY, "strncpy", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRPBRK, "strpbrk", BT_FN_STRING_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRRCHR, "strrchr", BT_FN_STRING_CONST_STRING_INT, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRSPN, "strspn", BT_FN_SIZE_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL)
DEF_LIB_BUILTIN        (BUILT_IN_STRSTR, "strstr", BT_FN_STRING_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL)

/* Category: stdio builtins.  */
DEF_LIB_BUILTIN        (BUILT_IN_FPRINTF, "fprintf", BT_FN_INT_FILEPTR_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_2_3)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FPRINTF_UNLOCKED, "fprintf_unlocked", BT_FN_INT_FILEPTR_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_2_3)
DEF_LIB_BUILTIN        (BUILT_IN_PUTC, "putc", BT_FN_INT_INT_FILEPTR, ATTR_NONNULL_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PUTC_UNLOCKED, "putc_unlocked", BT_FN_INT_INT_FILEPTR, ATTR_NONNULL_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_FPUTC, "fputc", BT_FN_INT_INT_FILEPTR, ATTR_NONNULL_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FPUTC_UNLOCKED, "fputc_unlocked", BT_FN_INT_INT_FILEPTR, ATTR_NONNULL_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_FPUTS, "fputs", BT_FN_INT_CONST_STRING_FILEPTR, ATTR_NONNULL_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FPUTS_UNLOCKED, "fputs_unlocked", BT_FN_INT_CONST_STRING_FILEPTR, ATTR_NONNULL_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_FSCANF, "fscanf", BT_FN_INT_FILEPTR_CONST_STRING_VAR, ATTR_FORMAT_SCANF_2_3)
DEF_LIB_BUILTIN        (BUILT_IN_FWRITE, "fwrite", BT_FN_SIZE_CONST_PTR_SIZE_SIZE_FILEPTR, ATTR_NONNULL_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FWRITE_UNLOCKED, "fwrite_unlocked", BT_FN_SIZE_CONST_PTR_SIZE_SIZE_FILEPTR, ATTR_NONNULL_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_PRINTF, "printf", BT_FN_INT_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_1_2)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PRINTF_UNLOCKED, "printf_unlocked", BT_FN_INT_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_1_2)
DEF_LIB_BUILTIN        (BUILT_IN_PUTCHAR, "putchar", BT_FN_INT_INT, ATTR_NULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PUTCHAR_UNLOCKED, "putchar_unlocked", BT_FN_INT_INT, ATTR_NULL)
DEF_LIB_BUILTIN        (BUILT_IN_PUTS, "puts", BT_FN_INT_CONST_STRING, ATTR_NONNULL_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PUTS_UNLOCKED, "puts_unlocked", BT_FN_INT_CONST_STRING, ATTR_NONNULL_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_SCANF, "scanf", BT_FN_INT_CONST_STRING_VAR, ATTR_FORMAT_SCANF_1_2)
DEF_C99_BUILTIN        (BUILT_IN_SNPRINTF, "snprintf", BT_FN_INT_STRING_SIZE_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_NOTHROW_3_4)
DEF_LIB_BUILTIN        (BUILT_IN_SPRINTF, "sprintf", BT_FN_INT_STRING_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_NOTHROW_2_3)
DEF_LIB_BUILTIN        (BUILT_IN_SSCANF, "sscanf", BT_FN_INT_CONST_STRING_CONST_STRING_VAR, ATTR_FORMAT_SCANF_NOTHROW_2_3)
DEF_LIB_BUILTIN        (BUILT_IN_VFPRINTF, "vfprintf", BT_FN_INT_FILEPTR_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_2_0)
DEF_C99_BUILTIN        (BUILT_IN_VFSCANF, "vfscanf", BT_FN_INT_FILEPTR_CONST_STRING_VALIST_ARG, ATTR_FORMAT_SCANF_2_0)
DEF_LIB_BUILTIN        (BUILT_IN_VPRINTF, "vprintf", BT_FN_INT_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_1_0)
DEF_C99_BUILTIN        (BUILT_IN_VSCANF, "vscanf", BT_FN_INT_CONST_STRING_VALIST_ARG, ATTR_FORMAT_SCANF_1_0)
DEF_C99_BUILTIN        (BUILT_IN_VSNPRINTF, "vsnprintf", BT_FN_INT_STRING_SIZE_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_NOTHROW_3_0)
DEF_LIB_BUILTIN        (BUILT_IN_VSPRINTF, "vsprintf", BT_FN_INT_STRING_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_NOTHROW_2_0)
DEF_C99_BUILTIN        (BUILT_IN_VSSCANF, "vsscanf", BT_FN_INT_CONST_STRING_CONST_STRING_VALIST_ARG, ATTR_FORMAT_SCANF_NOTHROW_2_0)

/* Category: ctype builtins.  */
DEF_LIB_BUILTIN        (BUILT_IN_ISALNUM, "isalnum", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISALPHA, "isalpha", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISASCII, "isascii", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_ISBLANK, "isblank", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISCNTRL, "iscntrl", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISDIGIT, "isdigit", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISGRAPH, "isgraph", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISLOWER, "islower", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISPRINT, "isprint", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISPUNCT, "ispunct", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISSPACE, "isspace", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISUPPER, "isupper", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ISXDIGIT, "isxdigit", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_TOASCII, "toascii", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_TOLOWER, "tolower", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_TOUPPER, "toupper", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LIST)

/* Category: wctype builtins.  */
DEF_C94_BUILTIN        (BUILT_IN_ISWALNUM, "iswalnum", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWALPHA, "iswalpha", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_ISWBLANK, "iswblank", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWCNTRL, "iswcntrl", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWDIGIT, "iswdigit", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWGRAPH, "iswgraph", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWLOWER, "iswlower", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWPRINT, "iswprint", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWPUNCT, "iswpunct", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWSPACE, "iswspace", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWUPPER, "iswupper", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_ISWXDIGIT, "iswxdigit", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_TOWLOWER, "towlower", BT_FN_WINT_WINT, ATTR_PURE_NOTHROW_LIST)
DEF_C94_BUILTIN        (BUILT_IN_TOWUPPER, "towupper", BT_FN_WINT_WINT, ATTR_PURE_NOTHROW_LIST)

/* Category: miscellaneous builtins.  */
DEF_LIB_BUILTIN        (BUILT_IN_ABORT, "abort", BT_FN_VOID, ATTR_NORETURN_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_ABS, "abs", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_AGGREGATE_INCOMING_ADDRESS, "aggregate_incoming_address", BT_FN_PTR_VAR, ATTR_NULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ALLOCA, "alloca", BT_FN_PTR_SIZE, ATTR_MALLOC_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_APPLY, "apply", BT_FN_PTR_PTR_FN_VOID_VAR_PTR_SIZE, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_APPLY_ARGS, "apply_args", BT_FN_PTR_VAR, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_ARGS_INFO, "args_info", BT_FN_INT_INT, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_BSWAP32, "bswap32", BT_FN_UINT32_UINT32, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_BSWAP64, "bswap64", BT_FN_UINT64_UINT64, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_CLEAR_CACHE, "__clear_cache", BT_FN_VOID_PTR_PTR, ATTR_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_CALLOC, "calloc", BT_FN_PTR_SIZE_SIZE, ATTR_MALLOC_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_CLASSIFY_TYPE, "classify_type", BT_FN_INT_VAR, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_CLZ, "clz", BT_FN_INT_UINT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_CLZIMAX, "clzimax", BT_FN_INT_UINTMAX, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_CLZL, "clzl", BT_FN_INT_ULONG, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_CLZLL, "clzll", BT_FN_INT_ULONGLONG, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_CONSTANT_P, "constant_p", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_CTZ, "ctz", BT_FN_INT_UINT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_CTZIMAX, "ctzimax", BT_FN_INT_UINTMAX, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_CTZL, "ctzl", BT_FN_INT_ULONG, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_CTZLL, "ctzll", BT_FN_INT_ULONGLONG, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DCGETTEXT, "dcgettext", BT_FN_STRING_CONST_STRING_CONST_STRING_INT, ATTR_FORMAT_ARG_2)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DGETTEXT, "dgettext", BT_FN_STRING_CONST_STRING_CONST_STRING, ATTR_FORMAT_ARG_2)
DEF_GCC_BUILTIN        (BUILT_IN_DWARF_CFA, "dwarf_cfa", BT_FN_PTR, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_DWARF_SP_COLUMN, "dwarf_sp_column", BT_FN_UINT, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_EH_RETURN, "eh_return", BT_FN_VOID_PTRMODE_PTR, ATTR_NORETURN_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_EH_RETURN_DATA_REGNO, "eh_return_data_regno", BT_FN_INT_INT, ATTR_NULL)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECL, "execl", BT_FN_INT_CONST_STRING_CONST_STRING_VAR, ATTR_SENTINEL_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECLP, "execlp", BT_FN_INT_CONST_STRING_CONST_STRING_VAR, ATTR_SENTINEL_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECLE, "execle", BT_FN_INT_CONST_STRING_CONST_STRING_VAR, ATTR_NOTHROW_SENTINEL_1)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECV, "execv", BT_FN_INT_CONST_STRING_PTR_CONST_STRING, ATTR_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECVP, "execvp", BT_FN_INT_CONST_STRING_PTR_CONST_STRING, ATTR_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECVE, "execve", BT_FN_INT_CONST_STRING_PTR_CONST_STRING_PTR_CONST_STRING, ATTR_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_EXIT, "exit", BT_FN_VOID_INT, ATTR_NORETURN_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_EXPECT, "expect", BT_FN_LONG_LONG_LONG, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_EXTEND_POINTER, "extend_pointer", BT_FN_UNWINDWORD_PTR, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_EXTRACT_RETURN_ADDR, "extract_return_addr", BT_FN_PTR_PTR, ATTR_NULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FFS, "ffs", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FFSIMAX, "ffsimax", BT_FN_INT_INTMAX, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FFSL, "ffsl", BT_FN_INT_LONG, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FFSLL, "ffsll", BT_FN_INT_LONGLONG, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_FORK, "fork", BT_FN_PID, ATTR_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_FRAME_ADDRESS, "frame_address", BT_FN_PTR_UINT, ATTR_NULL)
DEF_LIB_BUILTIN        (BUILT_IN_FREE, "free", BT_FN_VOID_PTR, ATTR_NOTHROW_LIST) 
DEF_GCC_BUILTIN        (BUILT_IN_FROB_RETURN_ADDR, "frob_return_addr", BT_FN_PTR_PTR, ATTR_NULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GETTEXT, "gettext", BT_FN_STRING_CONST_STRING, ATTR_FORMAT_ARG_1)
DEF_C99_BUILTIN        (BUILT_IN_IMAXABS, "imaxabs", BT_FN_INTMAX_INTMAX, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_INIT_DWARF_REG_SIZES, "init_dwarf_reg_size_table", BT_FN_VOID_PTR, ATTR_NULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITE, "finite", BT_FN_INT_DOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITEF, "finitef", BT_FN_INT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITEL, "finitel", BT_FN_INT_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITED32, "finited32", BT_FN_INT_DFLOAT32, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITED64, "finited64", BT_FN_INT_DFLOAT64, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITED128, "finited128", BT_FN_INT_DFLOAT128, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_FPCLASSIFY, "fpclassify", BT_FN_INT_INT_INT_INT_INT_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_GCC_BUILTIN        (BUILT_IN_ISFINITE, "isfinite", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_GCC_BUILTIN        (BUILT_IN_ISINF_SIGN, "isinf_sign", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ISINF, "isinf", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFF, "isinff", BT_FN_INT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFL, "isinfl", BT_FN_INT_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFD32, "isinfd32", BT_FN_INT_DFLOAT32, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFD64, "isinfd64", BT_FN_INT_DFLOAT64, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFD128, "isinfd128", BT_FN_INT_DFLOAT128, ATTR_CONST_NOTHROW_LIST)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ISNAN, "isnan", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNANF, "isnanf", BT_FN_INT_FLOAT, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNANL, "isnanl", BT_FN_INT_LONGDOUBLE, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNAND32, "isnand32", BT_FN_INT_DFLOAT32, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNAND64, "isnand64", BT_FN_INT_DFLOAT64, ATTR_CONST_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNAND128, "isnand128", BT_FN_INT_DFLOAT128, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_ISNORMAL, "isnormal", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_GCC_BUILTIN        (BUILT_IN_ISGREATER, "isgreater", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_GCC_BUILTIN        (BUILT_IN_ISGREATEREQUAL, "isgreaterequal", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_GCC_BUILTIN        (BUILT_IN_ISLESS, "isless", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_GCC_BUILTIN        (BUILT_IN_ISLESSEQUAL, "islessequal", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_GCC_BUILTIN        (BUILT_IN_ISLESSGREATER, "islessgreater", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_GCC_BUILTIN        (BUILT_IN_ISUNORDERED, "isunordered", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC)
DEF_LIB_BUILTIN        (BUILT_IN_LABS, "labs", BT_FN_LONG_LONG, ATTR_CONST_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN_LLABS, "llabs", BT_FN_LONGLONG_LONGLONG, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_LONGJMP, "longjmp", BT_FN_VOID_PTR_INT, ATTR_NORETURN_NOTHROW_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_MALLOC, "malloc", BT_FN_PTR_SIZE, ATTR_MALLOC_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_NEXT_ARG, "next_arg", BT_FN_PTR_VAR, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_PARITY, "parity", BT_FN_INT_UINT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_PARITYIMAX, "parityimax", BT_FN_INT_UINTMAX, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_PARITYL, "parityl", BT_FN_INT_ULONG, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_PARITYLL, "parityll", BT_FN_INT_ULONGLONG, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_POPCOUNT, "popcount", BT_FN_INT_UINT, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_POPCOUNTIMAX, "popcountimax", BT_FN_INT_UINTMAX, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_POPCOUNTL, "popcountl", BT_FN_INT_ULONG, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_POPCOUNTLL, "popcountll", BT_FN_INT_ULONGLONG, ATTR_CONST_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_PREFETCH, "prefetch", BT_FN_VOID_CONST_PTR_VAR, ATTR_NOVOPS_LIST)
DEF_LIB_BUILTIN        (BUILT_IN_REALLOC, "realloc", BT_FN_PTR_PTR_SIZE, ATTR_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_RETURN, "return", BT_FN_VOID_PTR, ATTR_NORETURN_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_RETURN_ADDRESS, "return_address", BT_FN_PTR_UINT, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_SAVEREGS, "saveregs", BT_FN_PTR_VAR, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_SETJMP, "setjmp", BT_FN_INT_PTR, ATTR_NULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRFMON, "strfmon", BT_FN_SSIZE_STRING_SIZE_CONST_STRING_VAR, ATTR_FORMAT_STRFMON_NOTHROW_3_4)
DEF_LIB_BUILTIN        (BUILT_IN_STRFTIME, "strftime", BT_FN_SIZE_STRING_SIZE_CONST_STRING_CONST_PTR, ATTR_FORMAT_STRFTIME_NOTHROW_3_0)
DEF_GCC_BUILTIN        (BUILT_IN_TRAP, "trap", BT_FN_VOID, ATTR_NORETURN_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_UNWIND_INIT, "unwind_init", BT_FN_VOID, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_UPDATE_SETJMP_BUF, "update_setjmp_buf", BT_FN_VOID_PTR_INT, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_VA_COPY, "va_copy", BT_FN_VOID_VALIST_REF_VALIST_ARG, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_VA_END, "va_end", BT_FN_VOID_VALIST_REF, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_VA_START, "va_start", BT_FN_VOID_VALIST_REF_VAR, ATTR_NULL)
DEF_GCC_BUILTIN        (BUILT_IN_VA_ARG_PACK, "va_arg_pack", BT_FN_INT, ATTR_PURE_NOTHROW_LIST)
DEF_GCC_BUILTIN        (BUILT_IN_VA_ARG_PACK_LEN, "va_arg_pack_len", BT_FN_INT, ATTR_PURE_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN__EXIT, "_exit", BT_FN_VOID_INT, ATTR_NORETURN_NOTHROW_LIST)
DEF_C99_BUILTIN        (BUILT_IN__EXIT2, "_Exit", BT_FN_VOID_INT, ATTR_NORETURN_NOTHROW_LIST)

/* Implementing nested functions.  */
DEF_BUILTIN_STUB (BUILT_IN_INIT_TRAMPOLINE, "__builtin_init_trampoline")
DEF_BUILTIN_STUB (BUILT_IN_ADJUST_TRAMPOLINE, "__builtin_adjust_trampoline")
DEF_BUILTIN_STUB (BUILT_IN_NONLOCAL_GOTO, "__builtin_nonlocal_goto")

/* Implementing __builtin_setjmp.  */
DEF_BUILTIN_STUB (BUILT_IN_SETJMP_SETUP, "__builtin_setjmp_setup")
DEF_BUILTIN_STUB (BUILT_IN_SETJMP_DISPATCHER, "__builtin_setjmp_dispatcher")
DEF_BUILTIN_STUB (BUILT_IN_SETJMP_RECEIVER, "__builtin_setjmp_receiver")

/* Implementing variable sized local variables.  */
DEF_BUILTIN_STUB (BUILT_IN_STACK_SAVE, "__builtin_stack_save")
DEF_BUILTIN_STUB (BUILT_IN_STACK_RESTORE, "__builtin_stack_restore")

/* Object size checking builtins.  */
DEF_GCC_BUILTIN	       (BUILT_IN_OBJECT_SIZE, "object_size", BT_FN_SIZE_CONST_PTR_INT, ATTR_PURE_NOTHROW_LIST)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMCPY_CHK, "__memcpy_chk", BT_FN_PTR_PTR_CONST_PTR_SIZE_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMMOVE_CHK, "__memmove_chk", BT_FN_PTR_PTR_CONST_PTR_SIZE_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMPCPY_CHK, "__mempcpy_chk", BT_FN_PTR_PTR_CONST_PTR_SIZE_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMSET_CHK, "__memset_chk", BT_FN_PTR_PTR_INT_SIZE_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STPCPY_CHK, "__stpcpy_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRCAT_CHK, "__strcat_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRCPY_CHK, "__strcpy_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRNCAT_CHK, "__strncat_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRNCPY_CHK, "__strncpy_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE_SIZE, ATTR_NOTHROW_NONNULL)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SNPRINTF_CHK, "__snprintf_chk", BT_FN_INT_STRING_SIZE_INT_SIZE_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_NOTHROW_5_6)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SPRINTF_CHK, "__sprintf_chk", BT_FN_INT_STRING_INT_SIZE_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_NOTHROW_4_5)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_VSNPRINTF_CHK, "__vsnprintf_chk", BT_FN_INT_STRING_SIZE_INT_SIZE_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_NOTHROW_5_0)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_VSPRINTF_CHK, "__vsprintf_chk", BT_FN_INT_STRING_INT_SIZE_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_NOTHROW_4_0)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FPRINTF_CHK, "__fprintf_chk", BT_FN_INT_FILEPTR_INT_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_3_4)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PRINTF_CHK, "__printf_chk", BT_FN_INT_INT_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_2_3)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_VFPRINTF_CHK, "__vfprintf_chk", BT_FN_INT_FILEPTR_INT_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_3_0)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_VPRINTF_CHK, "__vprintf_chk", BT_FN_INT_INT_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_2_0)

/* Profiling hooks.  */
DEF_BUILTIN_STUB (BUILT_IN_PROFILE_FUNC_ENTER, "profile_func_enter")
DEF_BUILTIN_STUB (BUILT_IN_PROFILE_FUNC_EXIT, "profile_func_exit")

/* TLS emulation.  */
// DEF_BUILTIN (BUILT_IN_EMUTLS_GET_ADDRESS, targetm.emutls.get_address,
// 	     BUILT_IN_NORMAL,
// 	     BT_FN_PTR_PTR,  BT_FN_PTR_PTR,
// 	     true, true, true, ATTR_CONST_NOTHROW_NONNULL, false,
// 	     !targetm.have_tls)
// DEF_BUILTIN (BUILT_IN_EMUTLS_REGISTER_COMMON,
// 	     targetm.emutls.register_common, BUILT_IN_NORMAL,
// 	     BT_FN_VOID_PTR_WORD_WORD_PTR, BT_FN_VOID_PTR_WORD_WORD_PTR,
// 	     true, true, true, ATTR_NOTHROW_LIST, false,
// 	     !targetm.have_tls)

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_ADD_N, "__sync_fetch_and_add",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_ADD_1, "__sync_fetch_and_add_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_ADD_2, "__sync_fetch_and_add_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_ADD_4, "__sync_fetch_and_add_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_ADD_8, "__sync_fetch_and_add_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_ADD_16, "__sync_fetch_and_add_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_SUB_N, "__sync_fetch_and_sub",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_SUB_1, "__sync_fetch_and_sub_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_SUB_2, "__sync_fetch_and_sub_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_SUB_4, "__sync_fetch_and_sub_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_SUB_8, "__sync_fetch_and_sub_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_SUB_16, "__sync_fetch_and_sub_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_OR_N, "__sync_fetch_and_or",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_OR_1, "__sync_fetch_and_or_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_OR_2, "__sync_fetch_and_or_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_OR_4, "__sync_fetch_and_or_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_OR_8, "__sync_fetch_and_or_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_OR_16, "__sync_fetch_and_or_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_AND_N, "__sync_fetch_and_and",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_AND_1, "__sync_fetch_and_and_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_AND_2, "__sync_fetch_and_and_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_AND_4, "__sync_fetch_and_and_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_AND_8, "__sync_fetch_and_and_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_AND_16, "__sync_fetch_and_and_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_XOR_N, "__sync_fetch_and_xor",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_XOR_1, "__sync_fetch_and_xor_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_XOR_2, "__sync_fetch_and_xor_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_XOR_4, "__sync_fetch_and_xor_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_XOR_8, "__sync_fetch_and_xor_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_XOR_16, "__sync_fetch_and_xor_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_NAND_N, "__sync_fetch_and_nand",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_NAND_1, "__sync_fetch_and_nand_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_NAND_2, "__sync_fetch_and_nand_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_NAND_4, "__sync_fetch_and_nand_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_NAND_8, "__sync_fetch_and_nand_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_NAND_16, "__sync_fetch_and_nand_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_ADD_AND_FETCH_N, "__sync_add_and_fetch",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_ADD_AND_FETCH_1, "__sync_add_and_fetch_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_ADD_AND_FETCH_2, "__sync_add_and_fetch_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_ADD_AND_FETCH_4, "__sync_add_and_fetch_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_ADD_AND_FETCH_8, "__sync_add_and_fetch_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_ADD_AND_FETCH_16, "__sync_add_and_fetch_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_SUB_AND_FETCH_N, "__sync_sub_and_fetch",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_SUB_AND_FETCH_1, "__sync_sub_and_fetch_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_SUB_AND_FETCH_2, "__sync_sub_and_fetch_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_SUB_AND_FETCH_4, "__sync_sub_and_fetch_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_SUB_AND_FETCH_8, "__sync_sub_and_fetch_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_SUB_AND_FETCH_16, "__sync_sub_and_fetch_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_OR_AND_FETCH_N, "__sync_or_and_fetch",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_OR_AND_FETCH_1, "__sync_or_and_fetch_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_OR_AND_FETCH_2, "__sync_or_and_fetch_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_OR_AND_FETCH_4, "__sync_or_and_fetch_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_OR_AND_FETCH_8, "__sync_or_and_fetch_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_OR_AND_FETCH_16, "__sync_or_and_fetch_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_AND_AND_FETCH_N, "__sync_and_and_fetch",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_AND_AND_FETCH_1, "__sync_and_and_fetch_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_AND_AND_FETCH_2, "__sync_and_and_fetch_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_AND_AND_FETCH_4, "__sync_and_and_fetch_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_AND_AND_FETCH_8, "__sync_and_and_fetch_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_AND_AND_FETCH_16, "__sync_and_and_fetch_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_XOR_AND_FETCH_N, "__sync_xor_and_fetch",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_XOR_AND_FETCH_1, "__sync_xor_and_fetch_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_XOR_AND_FETCH_2, "__sync_xor_and_fetch_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_XOR_AND_FETCH_4, "__sync_xor_and_fetch_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_XOR_AND_FETCH_8, "__sync_xor_and_fetch_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_XOR_AND_FETCH_16, "__sync_xor_and_fetch_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_NAND_AND_FETCH_N, "__sync_nand_and_fetch",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_NAND_AND_FETCH_1, "__sync_nand_and_fetch_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_NAND_AND_FETCH_2, "__sync_nand_and_fetch_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_NAND_AND_FETCH_4, "__sync_nand_and_fetch_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_NAND_AND_FETCH_8, "__sync_nand_and_fetch_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_NAND_AND_FETCH_16, "__sync_nand_and_fetch_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_BOOL_COMPARE_AND_SWAP_N,
		  "__sync_bool_compare_and_swap",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_BOOL_COMPARE_AND_SWAP_1,
		  "__sync_bool_compare_and_swap_1",
		  BT_FN_BOOL_VPTR_I1_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_BOOL_COMPARE_AND_SWAP_2,
		  "__sync_bool_compare_and_swap_2",
		  BT_FN_BOOL_VPTR_I2_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_BOOL_COMPARE_AND_SWAP_4,
		  "__sync_bool_compare_and_swap_4",
		  BT_FN_BOOL_VPTR_I4_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_BOOL_COMPARE_AND_SWAP_8,
		  "__sync_bool_compare_and_swap_8",
		  BT_FN_BOOL_VPTR_I8_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_BOOL_COMPARE_AND_SWAP_16,
		  "__sync_bool_compare_and_swap_16",
		  BT_FN_BOOL_VPTR_I16_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_VAL_COMPARE_AND_SWAP_N,
		  "__sync_val_compare_and_swap",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_VAL_COMPARE_AND_SWAP_1,
		  "__sync_val_compare_and_swap_1",
		  BT_FN_I1_VPTR_I1_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_VAL_COMPARE_AND_SWAP_2,
		  "__sync_val_compare_and_swap_2",
		  BT_FN_I2_VPTR_I2_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_VAL_COMPARE_AND_SWAP_4,
		  "__sync_val_compare_and_swap_4",
		  BT_FN_I4_VPTR_I4_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_VAL_COMPARE_AND_SWAP_8,
		  "__sync_val_compare_and_swap_8",
		  BT_FN_I8_VPTR_I8_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_VAL_COMPARE_AND_SWAP_16,
		  "__sync_val_compare_and_swap_16",
		  BT_FN_I16_VPTR_I16_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_LOCK_TEST_AND_SET_N, "__sync_lock_test_and_set",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_TEST_AND_SET_1, "__sync_lock_test_and_set_1",
		  BT_FN_I1_VPTR_I1, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_TEST_AND_SET_2, "__sync_lock_test_and_set_2",
		  BT_FN_I2_VPTR_I2, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_TEST_AND_SET_4, "__sync_lock_test_and_set_4",
		  BT_FN_I4_VPTR_I4, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_TEST_AND_SET_8, "__sync_lock_test_and_set_8",
		  BT_FN_I8_VPTR_I8, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_TEST_AND_SET_16, "__sync_lock_test_and_set_16",
		  BT_FN_I16_VPTR_I16, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_LOCK_RELEASE_N, "__sync_lock_release",
		  BT_FN_VOID_VAR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_RELEASE_1, "__sync_lock_release_1",
		  BT_FN_VOID_VPTR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_RELEASE_2, "__sync_lock_release_2",
		  BT_FN_VOID_VPTR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_RELEASE_4, "__sync_lock_release_4",
		  BT_FN_VOID_VPTR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_RELEASE_8, "__sync_lock_release_8",
		  BT_FN_VOID_VPTR, ATTR_NOTHROW_LIST)
DEF_SYNC_BUILTIN (BUILT_IN_LOCK_RELEASE_16, "__sync_lock_release_16",
		  BT_FN_VOID_VPTR, ATTR_NOTHROW_LIST)

DEF_SYNC_BUILTIN (BUILT_IN_SYNCHRONIZE, "__sync_synchronize",
		  BT_FN_VOID, ATTR_NOTHROW_LIST)

}

static scope_entry_t* solve_gcc_sync_builtins_overload_name(scope_entry_t* overloaded_function, type_t** arguments, int num_arguments)
{
    // Why people insists on having overload in C?
    char name[256];

    // We will iterate from 1 to 32
    const int max_valid_overloads = 5;

    char found_match = 0;
    scope_entry_t* result = NULL;

    DEBUG_CODE()
    {
        fprintf(stderr, "GCC-BUILTIN: Trying to figure out the exact version of '%s' given the following %d arguments\n",
                overloaded_function->symbol_name,
                num_arguments);
        int j;
        for (j = 0; j < num_arguments; j++)
        {
            if (arguments[j] != NULL)
            {
                fprintf(stderr, "GCC-BUILTIN:     [%d] %s\n", j,
                        print_declarator(arguments[j]));
            }
            else
            {
                fprintf(stderr, "GCC-BUILTIN:     [%d] <<NULL>>\n", j);
            }
        }
    }

    int i;
    int current_bit_size = 1;
    for (i = 0; (i < max_valid_overloads) && !found_match; i++)
    {
        snprintf(name, 255, "%s_%d", overloaded_function->symbol_name, current_bit_size);
        name[255] = '\0';
        scope_entry_list_t *entry_list = query_unqualified_name_str(overloaded_function->decl_context, name);

        // Let's assume no more overloads have been defined
        if (entry_list == NULL)
        {
            break;
        }

        scope_entry_t* current_entry = entry_list->entry;

        type_t* current_function_type = current_entry->type_information;

        DEBUG_CODE()
        {
            fprintf(stderr, "GCC-BUILTIN: Checking with builtin '%s' of type '%s'\n",
                    current_entry->symbol_name,
                    print_declarator(current_function_type));
        }

        if (!is_function_type(current_function_type))
        {
            internal_error("gcc builtin '%s' without function type\n", current_entry);
        }

        if (num_arguments != function_type_get_num_parameters(current_function_type))
        {
            // Ignore this case
            continue;
        }

        int j;
        char all_arguments_matched = 1;
        for (j = 0; (j < num_arguments) && all_arguments_matched; j++)
        {
            type_t* argument_type = arguments[j];
            type_t* parameter_type = function_type_get_parameter_type_num(current_function_type, j);

            standard_conversion_t scs;

            if (all_arguments_matched)
            {
                if (is_pointer_type(argument_type)
                        && is_pointer_type(parameter_type))
                {
                    // Do not rely on standard conversion between types because
                    // it allows too many conversions in C
                    all_arguments_matched = pointer_types_can_be_converted(argument_type, parameter_type);
                }
                else
                {
                    all_arguments_matched = standard_conversion_between_types(&scs, argument_type, parameter_type);
                }
            }
        }

        if (all_arguments_matched)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "GCC-BUILTIN: Builtin '%s' of type '%s' matched!\n",
                        current_entry->symbol_name,
                        print_declarator(current_function_type));
            }
            result = current_entry;
            found_match = 1;
        }

        current_bit_size = 2*current_bit_size;
    }

    return result;
}
