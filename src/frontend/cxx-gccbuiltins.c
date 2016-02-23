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




#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-entrylist.h"
#include "cxx-cexpr.h"
#include "cxx-diagnostic.h"
#include "cxx-intelsupport.h"
#include <string.h>
#include <math.h>

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
// DEF_PRIMITIVE_TYPE(BT_WORD, get_signed_int_type())
DEF_PRIMITIVE_TYPE(BT_FLOAT, get_float_type())
#ifdef HAVE_QUADMATH_H
DEF_PRIMITIVE_TYPE(BT_FLOAT128, get_float128_type())
#endif
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
// DEF_PRIMITIVE_TYPE(BT_VOLATILE_PTR, get_pointer_type(get_volatile_qualified_type(get_void_type())))

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

// DEF_PRIMITIVE_TYPE(BT_I1, get_signed_char_type())
// DEF_PRIMITIVE_TYPE(BT_I2, get_signed_short_int_type())
// DEF_PRIMITIVE_TYPE(BT_I4, get_signed_int_type())
// DEF_PRIMITIVE_TYPE(BT_I8,
//         (CURRENT_CONFIGURATION->type_environment->sizeof_signed_long == 8
//          ?  get_signed_long_int_type()
//          : get_signed_long_long_int_type()))
// This may not be available...
// DEF_PRIMITIVE_TYPE(BT_I16, get_signed_long_long_int_type())

DEF_PRIMITIVE_TYPE(BT_UNWINDWORD, get_signed_int_type())

DEF_POINTER_TYPE (BT_PTR_CONST_STRING, BT_CONST_STRING)
DEF_POINTER_TYPE (BT_PTR_LONG, BT_LONG)
DEF_POINTER_TYPE (BT_PTR_ULONGLONG, BT_ULONGLONG)
DEF_POINTER_TYPE (BT_PTR_PTR, BT_PTR)
DEF_POINTER_TYPE (BT_PTR_INT, BT_INT)
// DEF_POINTER_TYPE (BT_PTR_I1, BT_I1)
// DEF_POINTER_TYPE (BT_PTR_I2, BT_I2)
// DEF_POINTER_TYPE (BT_PTR_I4, BT_I4)
// DEF_POINTER_TYPE (BT_PTR_I8, BT_I8)
// DEF_POINTER_TYPE (BT_PTR_I16, BT_I16)

DEF_PRIMITIVE_TYPE(BT_UINT16, get_unsigned_short_int_type())
DEF_PRIMITIVE_TYPE(BT_UINT32, get_unsigned_int_type())
DEF_PRIMITIVE_TYPE(BT_UINT64, get_unsigned_long_long_int_type())

// Volatile pointers to basic integers (used in sync functions)
// DEF_POINTER_VOLATILE_TYPE(BT_VPTR_I1, BT_VOID)
// DEF_POINTER_VOLATILE_TYPE(BT_VPTR_I2, BT_VOID)
// DEF_POINTER_VOLATILE_TYPE(BT_VPTR_I4, BT_VOID)
// DEF_POINTER_VOLATILE_TYPE(BT_VPTR_I8, BT_VOID)
// DEF_POINTER_VOLATILE_TYPE(BT_VPTR_I16, BT_VOID)
DEF_POINTER_VOLATILE_TYPE(BT_VPTR_INT, BT_VOID)

static type_t* adjust_type_for_parameter_type(type_t* orig)
{
    type_t* result = orig;

    if (is_function_type(result))
    {
        result = get_pointer_type(result);
    }
    else if (is_array_type(result))
    {
        result = get_pointer_type(array_type_get_element_type(result));
    }

    return get_unqualified_type(result);
}


#define DEF_FUNCTION_TYPE_0(NAME, RESULT_TYPE) \
   UNUSED_FUNCTION static type_t* __mcxx_builtin_type__##NAME(void) \
   { \
       static type_t* result = NULL; \
       if(result == NULL) \
       { \
          result = get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), NULL, 0, REF_QUALIFIER_NONE); \
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
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 1, REF_QUALIFIER_NONE); \
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
         result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 2, REF_QUALIFIER_NONE); \
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
         result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 3, REF_QUALIFIER_NONE); \
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
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 4, REF_QUALIFIER_NONE); \
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
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 5, REF_QUALIFIER_NONE); \
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
           result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 6, REF_QUALIFIER_NONE); \
       } \
       return result; \
   } 

#define DEF_FUNCTION_TYPE_7(NAME, RESULT_TYPE, PARAM_TYPE1, PARAM_TYPE2, PARAM_TYPE3, PARAM_TYPE4, PARAM_TYPE5, PARAM_TYPE6, PARAM_TYPE7) \
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
           _param_info[6].is_ellipsis = 0; \
           _param_info[6].type_info =(__mcxx_builtin_type__##PARAM_TYPE7)(); \
           _param_info[6].type_info = adjust_type_for_parameter_type(_param_info[6].type_info); \
           result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 7, REF_QUALIFIER_NONE); \
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
           _param_info[0].type_info = get_generic_type(0); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 1, REF_QUALIFIER_NONE); \
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
           _param_info[1].type_info = get_generic_type(0); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 2, REF_QUALIFIER_NONE); \
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
           _param_info[2].type_info = get_generic_type(0); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 3, REF_QUALIFIER_NONE); \
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
           _param_info[3].type_info = get_generic_type(0); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 4, REF_QUALIFIER_NONE); \
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
           _param_info[4].type_info = get_generic_type(0); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 5, REF_QUALIFIER_NONE); \
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
           _param_info[5].type_info = get_generic_type(0); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 6, REF_QUALIFIER_NONE); \
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
           _param_info[6].type_info = get_generic_type(0); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 7, REF_QUALIFIER_NONE); \
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
           _param_info[7].type_info = get_generic_type(0); \
          result =  get_new_function_type((__mcxx_builtin_type__##RESULT_TYPE)(), _param_info, 8, REF_QUALIFIER_NONE); \
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
#ifdef HAVE_QUADMATH_H
DEF_FUNCTION_TYPE_0 (BT_FN_FLOAT128, BT_FLOAT128)
#endif
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
DEF_FUNCTION_TYPE_1 (BT_FN_INT_UINT16, BT_INT, BT_UINT16)
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
// DEF_FUNCTION_TYPE_1 (BT_FN_DFLOAT32_DFLOAT32, BT_DFLOAT32, BT_DFLOAT32)
// DEF_FUNCTION_TYPE_1 (BT_FN_DFLOAT64_DFLOAT64, BT_DFLOAT64, BT_DFLOAT64)
// DEF_FUNCTION_TYPE_1 (BT_FN_DFLOAT128_DFLOAT128, BT_DFLOAT128, BT_DFLOAT128)
// DEF_FUNCTION_TYPE_1 (BT_FN_VOID_VPTR, BT_VOID, BT_VOLATILE_PTR)
DEF_FUNCTION_TYPE_1 (BT_FN_VOID_PTRPTR, BT_VOID, BT_PTR_PTR)
DEF_FUNCTION_TYPE_1 (BT_FN_UINT_UINT, BT_UINT, BT_UINT)
// DEF_FUNCTION_TYPE_1 (BT_FN_ULONG_ULONG, BT_ULONG, BT_ULONG)
// DEF_FUNCTION_TYPE_1 (BT_FN_ULONGLONG_ULONGLONG, BT_ULONGLONG, BT_ULONGLONG)
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
// DEF_FUNCTION_TYPE_2 (BT_FN_INT_PTR_CONST_STRING,
// 		     BT_INT, BT_PTR, BT_CONST_STRING)
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
// DEF_FUNCTION_TYPE_3 (BT_FN_VOID_PTR_INT_INT,
// 		     BT_VOID, BT_PTR, BT_INT, BT_INT)
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
// DEF_FUNCTION_TYPE_3 (BT_FN_VOID_PTR_PTR_PTR, BT_VOID, BT_PTR, BT_PTR, BT_PTR)
DEF_FUNCTION_TYPE_3 (BT_FN_INT_CONST_STRING_PTR_CONST_STRING_PTR_CONST_STRING,
		     BT_INT, BT_CONST_STRING, BT_PTR_CONST_STRING, BT_PTR_CONST_STRING)
DEF_FUNCTION_TYPE_3 (BT_FN_INT_INT_CONST_STRING_VALIST_ARG,
		     BT_INT, BT_INT, BT_CONST_STRING, BT_VALIST_ARG)
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
// DEF_FUNCTION_TYPE_4 (BT_FN_VOID_PTR_WORD_WORD_PTR,
// 		     BT_VOID, BT_PTR, BT_WORD, BT_WORD, BT_PTR)

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

DEF_POINTER_TYPE(BT_PTR_VOID, BT_VOID)
DEF_POINTER_TYPE(BT_PTR_BOOL, BT_BOOL)

DEF_POINTER_VOLATILE_TYPE(BT_VPTR_VOID, BT_VOID)
DEF_POINTER_VOLATILE_TYPE(BT_VPTR_BOOL, BT_BOOL)

DEF_FUNCTION_TYPE_2(BT_FN_BOOL_VPTR_VOID_INT, BT_BOOL, BT_VPTR_VOID, BT_INT)
DEF_FUNCTION_TYPE_2(BT_FN_VOID_VPTR_BOOL_INT, BT_VOID, BT_VPTR_BOOL, BT_INT)


DEF_PRIMITIVE_TYPE(BT_GENERIC_0, get_generic_type(0))
DEF_POINTER_VOLATILE_TYPE(BT_VPTR_GENERIC_0, BT_GENERIC_0)
DEF_POINTER_TYPE(BT_PTR_GENERIC_0, BT_GENERIC_0)

DEF_FUNCTION_TYPE_2(BT_FUN_ATOMIC_LOAD_N, BT_GENERIC_0, BT_VPTR_GENERIC_0, BT_INT)
DEF_FUNCTION_TYPE_3(BT_FUN_ATOMIC_LOAD,   BT_VOID, BT_VPTR_GENERIC_0, BT_PTR_GENERIC_0, BT_INT)

DEF_FUNCTION_TYPE_3(BT_FUN_ATOMIC_STORE_N, BT_VOID, BT_VPTR_GENERIC_0, BT_GENERIC_0, BT_INT)
DEF_FUNCTION_TYPE_3(BT_FUN_ATOMIC_STORE,   BT_VOID, BT_VPTR_GENERIC_0, BT_PTR_GENERIC_0, BT_INT)

DEF_FUNCTION_TYPE_3(BT_FUN_ATOMIC_EXCHANGE_N, BT_GENERIC_0, BT_VPTR_GENERIC_0, BT_GENERIC_0, BT_INT)
DEF_FUNCTION_TYPE_3(BT_FUN_ATOMIC_EXCHANGE,   BT_VOID, BT_VPTR_GENERIC_0, BT_PTR_GENERIC_0, BT_INT)

DEF_FUNCTION_TYPE_6(BT_FUN_ATOMIC_COMPARE_EXCHANGE_N, BT_BOOL, BT_VPTR_GENERIC_0, BT_PTR_GENERIC_0, BT_GENERIC_0, BT_BOOL, BT_INT, BT_INT)
DEF_FUNCTION_TYPE_6(BT_FUN_ATOMIC_COMPARE_EXCHANGE,   BT_BOOL, BT_VPTR_GENERIC_0, BT_PTR_GENERIC_0, BT_PTR_GENERIC_0, BT_BOOL, BT_INT, BT_INT)

DEF_FUNCTION_TYPE_3(BT_FUN_ATOMIC_BIN_OP, BT_GENERIC_0, BT_VPTR_GENERIC_0, BT_GENERIC_0, BT_INT)

static scope_entry_t* solve_gcc_atomic_builtins_overload_name_generic(scope_entry_t* overloaded_function,
        type_t** types,
        nodecl_t *arguments,
        int num_arguments,
        const_value_t** const_value,
        type_t* function_type);

#define ATOMIC_OVERLOAD_FUN(X) \
static scope_entry_t* solve_gcc_atomic_builtins_overload_name##X(scope_entry_t* overloaded_function,  \
        type_t** types,  \
        nodecl_t *arguments UNUSED_PARAMETER, \
        int num_arguments, \
        const_value_t** const_value UNUSED_PARAMETER) \
{ \
    return solve_gcc_atomic_builtins_overload_name_generic(overloaded_function, types, arguments, num_arguments, const_value, \
            (__mcxx_builtin_type__##X)()); \
}

ATOMIC_OVERLOAD_FUN(BT_FUN_ATOMIC_EXCHANGE)
ATOMIC_OVERLOAD_FUN(BT_FUN_ATOMIC_EXCHANGE_N)
ATOMIC_OVERLOAD_FUN(BT_FUN_ATOMIC_LOAD)
ATOMIC_OVERLOAD_FUN(BT_FUN_ATOMIC_LOAD_N)
ATOMIC_OVERLOAD_FUN(BT_FUN_ATOMIC_COMPARE_EXCHANGE)
ATOMIC_OVERLOAD_FUN(BT_FUN_ATOMIC_COMPARE_EXCHANGE_N)
ATOMIC_OVERLOAD_FUN(BT_FUN_ATOMIC_STORE)
ATOMIC_OVERLOAD_FUN(BT_FUN_ATOMIC_STORE_N)
ATOMIC_OVERLOAD_FUN(BT_FUN_ATOMIC_BIN_OP)

#define DEF_ATOMIC_FUNCTION_TYPE(NAME, X) DEF_PRIMITIVE_TYPE(NAME, get_computed_function_type(solve_gcc_atomic_builtins_overload_name##X))

DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_EXCHANGE, BT_FUN_ATOMIC_EXCHANGE)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_EXCHANGE_N, BT_FUN_ATOMIC_EXCHANGE_N)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_ATOMIC_LOAD, BT_FUN_ATOMIC_LOAD)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_ATOMIC_LOAD_N, BT_FUN_ATOMIC_LOAD_N)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_COMPARE_EXCHANGE, BT_FUN_ATOMIC_COMPARE_EXCHANGE)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_COMPARE_EXCHANGE_N, BT_FUN_ATOMIC_COMPARE_EXCHANGE_N)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_ATOMIC_STORE, BT_FUN_ATOMIC_STORE)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_ATOMIC_STORE_N, BT_FUN_ATOMIC_STORE_N)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_OP_FETCH, BT_FUN_ATOMIC_BIN_OP)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_ATOMIC_OVERLOAD_FETCH_OP, BT_FUN_ATOMIC_BIN_OP)

static scope_entry_t* solve_gcc_builtin_shuffle(scope_entry_t* overloaded_function,
        type_t** types,
        nodecl_t *arguments UNUSED_PARAMETER,
        int num_arguments,
        const_value_t** const_value UNUSED_PARAMETER);

DEF_PRIMITIVE_TYPE(BT_FN_BUILTIN_SHUFFLE, get_computed_function_type(solve_gcc_builtin_shuffle))

DEF_FUNCTION_TYPE_2(BT_FN_BOOL_SIZE_CONST_VPTR, BT_BOOL, BT_SIZE, BT_PTR_VOID)

// Old sync support
DEF_FUNCTION_TYPE_2(BT_FN_SYNC_FETCH_AND_OP, BT_GENERIC_0, BT_PTR_GENERIC_0, BT_GENERIC_0)
DEF_FUNCTION_TYPE_2(BT_FN_SYNC_OP_AND_FETCH, BT_GENERIC_0, BT_PTR_GENERIC_0, BT_GENERIC_0)
DEF_FUNCTION_TYPE_3(BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, BT_BOOL, BT_PTR_GENERIC_0, BT_GENERIC_0, BT_GENERIC_0)
DEF_FUNCTION_TYPE_3(BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, BT_GENERIC_0, BT_PTR_GENERIC_0, BT_GENERIC_0, BT_GENERIC_0)
DEF_FUNCTION_TYPE_2(BT_FN_SYNC_LOCK_TEST_AND_SET, BT_GENERIC_0, BT_PTR_GENERIC_0, BT_GENERIC_0)
DEF_FUNCTION_TYPE_1(BT_FN_SYNC_LOCK_RELEASE, BT_VOID, BT_PTR_GENERIC_0)

ATOMIC_OVERLOAD_FUN(BT_FN_SYNC_FETCH_AND_OP)
ATOMIC_OVERLOAD_FUN(BT_FN_SYNC_OP_AND_FETCH)
ATOMIC_OVERLOAD_FUN(BT_FN_SYNC_COMPARE_AND_SWAP_BOOL)
ATOMIC_OVERLOAD_FUN(BT_FN_SYNC_COMPARE_AND_SWAP_VALUE)
ATOMIC_OVERLOAD_FUN(BT_FN_SYNC_LOCK_TEST_AND_SET)
ATOMIC_OVERLOAD_FUN(BT_FN_SYNC_LOCK_RELEASE)

DEF_ATOMIC_FUNCTION_TYPE(BT_FN_SYNC_FETCH_AND_OP_OVERLOAD, BT_FN_SYNC_FETCH_AND_OP)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_SYNC_OP_AND_FETCH_OVERLOAD, BT_FN_SYNC_OP_AND_FETCH)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_SYNC_COMPARE_AND_SWAP_BOOL_OVERLOAD, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_SYNC_COMPARE_AND_SWAP_VALUE_OVERLOAD, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_SYNC_LOCK_TEST_AND_SET_OVERLOAD, BT_FN_SYNC_LOCK_TEST_AND_SET)
DEF_ATOMIC_FUNCTION_TYPE(BT_FN_SYNC_LOCK_RELEASE_OVERLOAD, BT_FN_SYNC_LOCK_RELEASE)

/*
 * Bultins
 */

/* 
   From GCC
   --------

   Before including this file, you should define a macro:

     DEF_BUILTIN (ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P,
                  FALLBACK_P, NONANSI_P, ATTRS, IMPLICIT, COND)

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
   enumerals from builtin-types.def.  If BOTH_P is 1, then LIBTYPE
   is the type of the non-`__builtin_' variant.  Otherwise, LIBTYPE
   should be ignored.

   If FALLBACK_P is 1 then, if for some reason, the compiler cannot
   expand the builtin function directly, it will call the
   corresponding library function (which does not have the
   `__builtin_' prefix.

   If NONANSI_P is 1, then the non-`__builtin_' variant is not an
   ANSI/ISO library function, and so we should pretend it does not
   exist when compiling in ANSI conformant mode.

   ATTRs is an attribute list as defined in builtin-attrs.def that
   describes the attributes of this builtin function.  

   IMPLICIT specifies condition when the builtin can be produced by
   compiler.  For instance C90 reserves floorf function, but does not
   define it's meaning.  When user uses floorf we may assume that the
   floorf has the meaning we expect, but we can't produce floorf by
   simplifying floor((double)float) since the runtime need not implement
   it.  
 
   The builtins is registered only if COND is 1. */

// DEF_PRIMITIVE_TYPE(BT_LAST, get_void_type())
DEF_FUNCTION_TYPE_0(0, BT_VOID)

#define  DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P, \
                   FALLBACK_P, NONANSI_P, ATTRS, IMPLICIT, COND, EXPAND) \
  { \
      if (COND) \
      { \
      scope_entry_t* new_builtin = new_symbol(global_context, global_context->global_scope, uniquestr(NAME)); \
      new_builtin->kind = SK_FUNCTION; \
      new_builtin->type_information = (__mcxx_builtin_type__##TYPE)(); \
      symbol_entity_specs_set_is_builtin(new_builtin, 1); \
      new_builtin->do_not_print = 1; \
      new_builtin->locus = make_locus("(gcc-builtin)", 0, 0); \
      if (is_function_type(new_builtin->type_information)) \
      { \
          symbol_entity_specs_reserve_default_argument_info(new_builtin, symbol_entity_specs_get_num_parameters(new_builtin)); \
      } \
      symbol_entity_specs_set_simplify_function(new_builtin, EXPAND); \
      /* DEBUG_CODE() */ \
      /* { */ \
      /*     fprintf(stderr, "GCC-BUILTIN: Registered gcc-builtin '%s' with type '%s'\n", NAME, */ \
      /*             print_declarator(new_builtin->type_information, global_context)); */ \
      /* } */ \
      } \
  }

/* A GCC builtin(like __builtin_saveregs) is provided by the
   compiler, but does not correspond to a function in the standard
   library.  */
#undef DEF_GCC_BUILTIN
#define DEF_GCC_BUILTIN(ENUM, NAME, TYPE, ATTRS, EXPAND)        \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, BT_LAST,  \
               0, 0, 0, ATTRS, 1, 1, EXPAND)

/* Like DEF_GCC_BUILTIN, except we don't prepend "__builtin_".  */
#undef DEF_SYNC_BUILTIN
#define DEF_SYNC_BUILTIN(ENUM, NAME, TYPE, ATTRS, EXPAND)       \
  DEF_BUILTIN(ENUM, NAME, BUILT_IN_NORMAL, TYPE, BT_LAST,   \
               0, 0, 0, ATTRS, 1, 1, EXPAND)

/* A library builtin(like __builtin_strchr) is a builtin equivalent
   of an ANSI/ISO standard library function.  In addition to the
   `__builtin' version, we will create an ordinary version(e.g,
   `strchr') as well.  If we cannot compute the answer using the
   builtin function, we will fall back to the standard library
   version.  */
#undef DEF_LIB_BUILTIN                  
#define DEF_LIB_BUILTIN(ENUM, NAME, TYPE, ATTRS, EXPAND)    \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           1, 1, 0, ATTRS, 1, 1, EXPAND)

/* Like DEF_LIB_BUILTIN, except that the function is not one that is
   specified by ANSI/ISO C.  So, when we're being fully conformant we
   ignore the version of these builtins that does not begin with
   __builtin.  */
#undef DEF_EXT_LIB_BUILTIN              
#define DEF_EXT_LIB_BUILTIN(ENUM, NAME, TYPE, ATTRS, EXPAND)    \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           1, 1, 1, ATTRS, 0, 1, EXPAND)

/* Like DEF_LIB_BUILTIN, except that the function is only a part of
   the standard in C94 or above.  */
#undef DEF_C94_BUILTIN                  
#define DEF_C94_BUILTIN(ENUM, NAME, TYPE, ATTRS, EXPAND)    \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           1, 1, !flag_isoc94, ATTRS, TARGET_C99_FUNCTIONS, 1, EXPAND)

/* Like DEF_LIB_BUILTIN, except that the function is only a part of
   the standard in C99 or above.  */
#undef DEF_C99_BUILTIN                  
#define DEF_C99_BUILTIN(ENUM, NAME, TYPE, ATTRS, EXPAND)    \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           1, 1, !flag_isoc99, ATTRS, TARGET_C99_FUNCTIONS, 1, EXPAND)

/* Builtin that is specified by C99 and C90 reserve the name for future use.
   We can still recognize the builtin in C90 mode but we can't produce it
   implicitly.  */
#undef DEF_C99_C90RES_BUILTIN                   
#define DEF_C99_C90RES_BUILTIN(ENUM, NAME, TYPE, ATTRS, EXPAND) \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE, \
           1, 1, !flag_isoc99, ATTRS, TARGET_C99_FUNCTIONS, 1, EXPAND)

/* Builtin that C99 reserve the name for future use. We can still recognize
   the builtin in C99 mode but we can't produce it implicitly.  */
#undef DEF_EXT_C99RES_BUILTIN
#define DEF_EXT_C99RES_BUILTIN(ENUM, NAME, TYPE, ATTRS, EXPAND)        \
  DEF_BUILTIN(ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE,   \
              1, 1, 1, ATTRS, 0, 1, EXPAND)

/* Allocate the enum and the name for a builtin, but do not actually
   define it here at all.  */
#undef DEF_BUILTIN_STUB
#define DEF_BUILTIN_STUB(ENUM, NAME, EXPAND) \
  DEF_BUILTIN(ENUM, NAME, BUILT_IN_NORMAL, 0, 0, 0, 0, \
           0, 0, 0, 0, EXPAND)

/* Builtin used by the implementation of GNU OpenMP.  None of these are
   actually implemented in the compiler; they're all in libgomp.  */
#undef DEF_GOMP_BUILTIN
#define DEF_GOMP_BUILTIN(ENUM, NAME, TYPE, ATTRS, EXPAND) \
  DEF_BUILTIN (ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE,    \
               0, 1, 1, ATTRS, 0, \
          CURRENT_CONFIGURATION->enable_openmp, EXPAND)
	       // (flag_openmp || flag_tree_parallelize_loops))


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

#define NO_EXPAND_FUN (0)

#define SIMPLIFY_BUILTIN_FUN0(builtin_name, return_type) \
static nodecl_t simplify_##builtin_name(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments UNUSED_PARAMETER) \
{ \
    if (num_arguments == 0) \
    { \
        return const_value_to_nodecl( \
                const_value_get_##return_type( \
                    __builtin_##builtin_name() \
                    )); \
    } \
    return nodecl_null(); \
}

#define SIMPLIFY_BUILTIN_FUN1(builtin_name, return_type, type_0) \
static nodecl_t simplify_##builtin_name(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments) \
{ \
    if (num_arguments == 1 \
            && nodecl_is_constant(arguments[0])) \
    { \
        return const_value_to_nodecl( \
                const_value_get_##return_type( \
                    __builtin_##builtin_name( \
                        const_value_cast_to_##type_0( \
                            nodecl_get_constant(arguments[0]))))); \
    } \
    return nodecl_null(); \
}

#define SIMPLIFY_BUILTIN_FUN2(builtin_name, return_type, type_0, type_1) \
static nodecl_t simplify_##builtin_name(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments) \
{ \
    if (num_arguments == 2 \
            && nodecl_is_constant(arguments[0]) \
            && nodecl_is_constant(arguments[1])) \
    { \
        return const_value_to_nodecl( \
                const_value_get_##return_type( \
                    __builtin_##builtin_name( \
                        const_value_cast_to_##type_0( \
                            nodecl_get_constant(arguments[0])), \
                        const_value_cast_to_##type_1( \
                            nodecl_get_constant(arguments[1])) \
                        ))); \
    } \
    return nodecl_null(); \
}

#define SIMPLIFY_BUILTIN_FUN3(builtin_name, return_type, type_0, type_1, type_2) \
static nodecl_t simplify_##builtin_name(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments) \
{ \
    if (num_arguments == 3 \
            && nodecl_is_constant(arguments[0]) \
            && nodecl_is_constant(arguments[1]) \
            && nodecl_is_constant(arguments[2])) \
    { \
        return const_value_to_nodecl( \
                const_value_get_##return_type( \
                    __builtin_##builtin_name( \
                        const_value_cast_to_##type_0( \
                            nodecl_get_constant(arguments[0])), \
                        const_value_cast_to_##type_1( \
                            nodecl_get_constant(arguments[1])), \
                        const_value_cast_to_##type_2( \
                            nodecl_get_constant(arguments[2])) \
                        ))); \
    } \
    return nodecl_null(); \
}

/* Simplification functions */
SIMPLIFY_BUILTIN_FUN1(clz, signed_int, unsigned_int);
SIMPLIFY_BUILTIN_FUN1(clzl, signed_int, unsigned_long_int);
SIMPLIFY_BUILTIN_FUN1(clzll, signed_int, unsigned_long_long_int);

SIMPLIFY_BUILTIN_FUN1(ctz, signed_int, unsigned_int);
SIMPLIFY_BUILTIN_FUN1(ctzl, signed_int, unsigned_long_int);
SIMPLIFY_BUILTIN_FUN1(ctzll, signed_int, unsigned_long_long_int);

SIMPLIFY_BUILTIN_FUN1(fabsf, float, float);
SIMPLIFY_BUILTIN_FUN1(fabs, double, double);
SIMPLIFY_BUILTIN_FUN1(fabsl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(sinf, float, float);
SIMPLIFY_BUILTIN_FUN1(sin, double, double);
SIMPLIFY_BUILTIN_FUN1(sinl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(cosf, float, float);
SIMPLIFY_BUILTIN_FUN1(cos, double, double);
SIMPLIFY_BUILTIN_FUN1(cosl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(tanf, float, float);
SIMPLIFY_BUILTIN_FUN1(tan, double, double);
SIMPLIFY_BUILTIN_FUN1(tanl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(sinhf, float, float);
SIMPLIFY_BUILTIN_FUN1(sinh, double, double);
SIMPLIFY_BUILTIN_FUN1(sinhl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(coshf, float, float);
SIMPLIFY_BUILTIN_FUN1(cosh, double, double);
SIMPLIFY_BUILTIN_FUN1(coshl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(tanhf, float, float);
SIMPLIFY_BUILTIN_FUN1(tanh, double, double);
SIMPLIFY_BUILTIN_FUN1(tanhl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(asinf, float, float);
SIMPLIFY_BUILTIN_FUN1(asin, double, double);
SIMPLIFY_BUILTIN_FUN1(asinl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(acosf, float, float);
SIMPLIFY_BUILTIN_FUN1(acos, double, double);
SIMPLIFY_BUILTIN_FUN1(acosl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(atanf, float, float);
SIMPLIFY_BUILTIN_FUN1(atan, double, double);
SIMPLIFY_BUILTIN_FUN1(atanl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(asinhf, float, float);
SIMPLIFY_BUILTIN_FUN1(asinh, double, double);
SIMPLIFY_BUILTIN_FUN1(asinhl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(acoshf, float, float);
SIMPLIFY_BUILTIN_FUN1(acosh, double, double);
SIMPLIFY_BUILTIN_FUN1(acoshl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(atanhf, float, float);
SIMPLIFY_BUILTIN_FUN1(atanh, double, double);
SIMPLIFY_BUILTIN_FUN1(atanhl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(atan2f, float, float, float);
SIMPLIFY_BUILTIN_FUN2(atan2, double, double, double);
SIMPLIFY_BUILTIN_FUN2(atan2l, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(ceilf, float, float);
SIMPLIFY_BUILTIN_FUN1(ceil, double, double);
SIMPLIFY_BUILTIN_FUN1(ceill, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(floorf, float, float);
SIMPLIFY_BUILTIN_FUN1(floor, double, double);
SIMPLIFY_BUILTIN_FUN1(floorl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(fmodf, float, float, float);
SIMPLIFY_BUILTIN_FUN2(fmod, double, double, double);
SIMPLIFY_BUILTIN_FUN2(fmodl, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(expf, float, float);
SIMPLIFY_BUILTIN_FUN1(exp, double, double);
SIMPLIFY_BUILTIN_FUN1(expl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(ldexpf, float, float, signed_int);
SIMPLIFY_BUILTIN_FUN2(ldexp, double, double, signed_int);
SIMPLIFY_BUILTIN_FUN2(ldexpl, long_double, long_double, signed_int);

SIMPLIFY_BUILTIN_FUN1(log10f, float, float);
SIMPLIFY_BUILTIN_FUN1(log10, double, double);
SIMPLIFY_BUILTIN_FUN1(log10l, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(logf, float, float);
SIMPLIFY_BUILTIN_FUN1(log, double, double);
SIMPLIFY_BUILTIN_FUN1(logl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(log1pf, float, float);
SIMPLIFY_BUILTIN_FUN1(log1p, double, double);
SIMPLIFY_BUILTIN_FUN1(log1pl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(log2f, float, float);
SIMPLIFY_BUILTIN_FUN1(log2, double, double);
SIMPLIFY_BUILTIN_FUN1(log2l, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(logbf, float, float);
SIMPLIFY_BUILTIN_FUN1(logb, double, double);
SIMPLIFY_BUILTIN_FUN1(logbl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(powf, float, float, float);
SIMPLIFY_BUILTIN_FUN2(pow, double, double, double);
SIMPLIFY_BUILTIN_FUN2(powl, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(sqrtf, float, float);
SIMPLIFY_BUILTIN_FUN1(sqrt, double, double);
SIMPLIFY_BUILTIN_FUN1(sqrtl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(cbrtf, float, float);
SIMPLIFY_BUILTIN_FUN1(cbrt, double, double);
SIMPLIFY_BUILTIN_FUN1(cbrtl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(copysignf, float, float, float);
SIMPLIFY_BUILTIN_FUN2(copysign, double, double, double);
SIMPLIFY_BUILTIN_FUN2(copysignl, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(erff, float, float);
SIMPLIFY_BUILTIN_FUN1(erf, double, double);
SIMPLIFY_BUILTIN_FUN1(erfl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(erfcf, float, float);
SIMPLIFY_BUILTIN_FUN1(erfc, double, double);
SIMPLIFY_BUILTIN_FUN1(erfcl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(exp2f, float, float);
SIMPLIFY_BUILTIN_FUN1(exp2, double, double);
SIMPLIFY_BUILTIN_FUN1(exp2l, long_double, long_double);

#ifdef HAVE__BUILTIN_EXP10F
SIMPLIFY_BUILTIN_FUN1(exp10f, float, float);
#else
#define simplify_exp10f NO_EXPAND_FUN
#endif

#ifdef HAVE___BUILTIN_EXP10
SIMPLIFY_BUILTIN_FUN1(exp10, double, double);
#else
#define simplify_exp10 NO_EXPAND_FUN
#endif

#ifdef HAVE__BUILTIN_EXP10L
SIMPLIFY_BUILTIN_FUN1(exp10l, long_double, long_double);
#else
#define simplify_exp10l NO_EXPAND_FUN
#endif


SIMPLIFY_BUILTIN_FUN1(expm1f, float, float);
SIMPLIFY_BUILTIN_FUN1(expm1, double, double);
SIMPLIFY_BUILTIN_FUN1(expm1l, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(fdimf, float, float, float);
SIMPLIFY_BUILTIN_FUN2(fdim, double, double, double);
SIMPLIFY_BUILTIN_FUN2(fdiml, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN3(fmaf, float, float, float, float);
SIMPLIFY_BUILTIN_FUN3(fma, double, double, double, double);
SIMPLIFY_BUILTIN_FUN3(fmal, long_double, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(fmaxf, float, float, float);
SIMPLIFY_BUILTIN_FUN2(fmax, double, double, double);
SIMPLIFY_BUILTIN_FUN2(fmaxl, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(fminf, float, float, float);
SIMPLIFY_BUILTIN_FUN2(fmin, double, double, double);
SIMPLIFY_BUILTIN_FUN2(fminl, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN0(huge_valf, float);
SIMPLIFY_BUILTIN_FUN0(huge_val, double);
SIMPLIFY_BUILTIN_FUN0(huge_vall, long_double);

#ifdef HAVE_QUADMATH_H
SIMPLIFY_BUILTIN_FUN0(huge_valq, float128);
#endif

SIMPLIFY_BUILTIN_FUN2(hypotf, float, float, float);
SIMPLIFY_BUILTIN_FUN2(hypot, double, double, double);
SIMPLIFY_BUILTIN_FUN2(hypotl, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(ilogbf, signed_int, float);
SIMPLIFY_BUILTIN_FUN1(ilogb, signed_int, double);
SIMPLIFY_BUILTIN_FUN1(ilogbl, signed_int, long_double);

SIMPLIFY_BUILTIN_FUN1(abs, signed_int, signed_int);
SIMPLIFY_BUILTIN_FUN1(labs, signed_long_int, signed_long_int);
SIMPLIFY_BUILTIN_FUN1(llabs, signed_long_long_int, signed_long_long_int);

SIMPLIFY_BUILTIN_FUN1(lgammaf, float, float);
SIMPLIFY_BUILTIN_FUN1(lgamma, double, double);
SIMPLIFY_BUILTIN_FUN1(lgammal, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(llrintf, signed_long_long_int, float);
SIMPLIFY_BUILTIN_FUN1(llrint, signed_long_long_int, double);
SIMPLIFY_BUILTIN_FUN1(llrintl, signed_long_long_int, long_double);

SIMPLIFY_BUILTIN_FUN1(lrintf, signed_long_int, float);
SIMPLIFY_BUILTIN_FUN1(lrint, signed_long_int, double);
SIMPLIFY_BUILTIN_FUN1(lrintl, signed_long_int, long_double);

SIMPLIFY_BUILTIN_FUN1(llroundf, signed_long_long_int, float);
SIMPLIFY_BUILTIN_FUN1(llround, signed_long_long_int, double);
SIMPLIFY_BUILTIN_FUN1(llroundl, signed_long_long_int, long_double);

SIMPLIFY_BUILTIN_FUN1(lroundf, signed_long_int, float);
SIMPLIFY_BUILTIN_FUN1(lround, signed_long_int, double);
SIMPLIFY_BUILTIN_FUN1(lroundl, signed_long_int, long_double);

SIMPLIFY_BUILTIN_FUN1(nearbyintf, float, float);
SIMPLIFY_BUILTIN_FUN1(nearbyint, double, double);
SIMPLIFY_BUILTIN_FUN1(nearbyintl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(nextafterf, float, float, float);
SIMPLIFY_BUILTIN_FUN2(nextafter, double, double, double);
SIMPLIFY_BUILTIN_FUN2(nextafterl, long_double, long_double, long_double);

#ifdef HAVE__BUILTIN_NEXTTOWARDF
SIMPLIFY_BUILTIN_FUN2(nexttowardf, float, float, long_double);
#else
#define simplify_nexttowardf NO_EXPAND_FUN
#endif
#ifdef HAVE__BUILTIN_NEXTTOWARD
SIMPLIFY_BUILTIN_FUN2(nexttoward, double, double, long_double);
#else
#define simplify_nexttoward NO_EXPAND_FUN
#endif
#ifdef HAVE__BUILTIN_NEXTTOWARDL
SIMPLIFY_BUILTIN_FUN2(nexttowardl, long_double, long_double, long_double);
#else
#define simplify_nexttowardl NO_EXPAND_FUN
#endif

SIMPLIFY_BUILTIN_FUN1(popcount, signed_int, unsigned_int);
SIMPLIFY_BUILTIN_FUN1(popcountl, signed_int, unsigned_long_int);
SIMPLIFY_BUILTIN_FUN1(popcountll, signed_int, unsigned_long_long_int);

SIMPLIFY_BUILTIN_FUN2(remainderf, float, float, float);
SIMPLIFY_BUILTIN_FUN2(remainder, double, double, double);
SIMPLIFY_BUILTIN_FUN2(remainderl, long_double, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(rintf, float, float);
SIMPLIFY_BUILTIN_FUN1(rint, double, double);
SIMPLIFY_BUILTIN_FUN1(rintl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(roundf, float, float);
SIMPLIFY_BUILTIN_FUN1(round, double, double);
SIMPLIFY_BUILTIN_FUN1(roundl, long_double, long_double);

SIMPLIFY_BUILTIN_FUN2(scalbnf, float, float, signed_int);
SIMPLIFY_BUILTIN_FUN2(scalbn, double, double, signed_int);
SIMPLIFY_BUILTIN_FUN2(scalbnl, long_double, long_double, signed_int);

SIMPLIFY_BUILTIN_FUN2(scalblnf, float, float, signed_long_int);
SIMPLIFY_BUILTIN_FUN2(scalbln, double, double, signed_long_int);
SIMPLIFY_BUILTIN_FUN2(scalblnl, long_double, long_double, signed_long_int);

SIMPLIFY_BUILTIN_FUN1(tgammaf, float, float);
SIMPLIFY_BUILTIN_FUN1(tgamma, double, double);
SIMPLIFY_BUILTIN_FUN1(tgammal, long_double, long_double);

SIMPLIFY_BUILTIN_FUN1(truncf, float, float);
SIMPLIFY_BUILTIN_FUN1(trunc, double, double);
SIMPLIFY_BUILTIN_FUN1(truncl, long_double, long_double);

#ifdef HAVE__BUILTIN_FPCLASSIFY
static nodecl_t simplify_fpclassify(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    // GCC 4.3 does not implement '__builtin_fpclassify'
    if (num_arguments == 6
            && nodecl_is_constant(arguments[0])
            && nodecl_is_constant(arguments[1])
            && nodecl_is_constant(arguments[2])
            && nodecl_is_constant(arguments[3])
            && nodecl_is_constant(arguments[4])
            && nodecl_is_constant(arguments[5])
            // arguments[5] goes unchecked, so verify that it is a floating point
            && const_value_is_floating(nodecl_get_constant(arguments[5])))
    {
        const_value_t* v = nodecl_get_constant(arguments[5]);
        if (const_value_is_float(v))
        {
            return const_value_to_nodecl(
                    const_value_get_signed_int(
                        __builtin_fpclassify(
                            FP_NAN, FP_INFINITE, FP_NORMAL, FP_SUBNORMAL, FP_ZERO,
                            const_value_cast_to_float(v))));
        }
        else if (const_value_is_double(v))
        {
            return const_value_to_nodecl(
                    const_value_get_signed_int(
                        __builtin_fpclassify(
                            FP_NAN, FP_INFINITE, FP_NORMAL, FP_SUBNORMAL, FP_ZERO,
                            const_value_cast_to_double(v))));
        }
        else if (const_value_is_long_double(v))
        {
            return const_value_to_nodecl(
                    const_value_get_signed_int(
                        __builtin_fpclassify(
                            FP_NAN, FP_INFINITE, FP_NORMAL, FP_SUBNORMAL, FP_ZERO,
                            const_value_cast_to_long_double(v))));
        }
    }
    return nodecl_null();
}
#else
static nodecl_t simplify_fpclassify(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments UNUSED_PARAMETER)
{
    return nodecl_null();
}
#endif


#ifdef HAVE__BUILTIN_NAN
static nodecl_t simplify_nan(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    if (num_arguments == 1
            && nodecl_is_constant(arguments[0])
            && const_value_is_string(nodecl_get_constant(arguments[0])))
    {
        char is_null_ended = 0;
        return const_value_to_nodecl(
                const_value_get_double(
                __builtin_nan(const_value_string_unpack_to_string(nodecl_get_constant(arguments[0]), &is_null_ended))
                ));
    }

    return nodecl_null();
}
#else
static nodecl_t simplify_nan(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments UNUSED_PARAMETER)
{
    return nodecl_null();
}
#endif

#ifdef HAVE__BUILTIN_NANF
static nodecl_t simplify_nanf(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    if (num_arguments == 1
            && nodecl_is_constant(arguments[0])
            && const_value_is_string(nodecl_get_constant(arguments[0])))
    {
        char is_null_ended = 0;
        return const_value_to_nodecl(
                const_value_get_float(
                __builtin_nanf(const_value_string_unpack_to_string(nodecl_get_constant(arguments[0]), &is_null_ended))
                ));
    }

    return nodecl_null();
}
#else
static nodecl_t simplify_nanf(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments UNUSED_PARAMETER)
{
    return nodecl_null();
}
#endif

#ifdef HAVE__BUILTIN_NANL
static nodecl_t simplify_nanl(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    if (num_arguments == 1
            && nodecl_is_constant(arguments[0])
            && const_value_is_string(nodecl_get_constant(arguments[0])))
    {
        char is_null_ended = 0;
        return const_value_to_nodecl(
                const_value_get_long_double(
                    __builtin_nanl(const_value_string_unpack_to_string(nodecl_get_constant(arguments[0]), &is_null_ended))
                    ));
    }

    return nodecl_null();
}
#else
static nodecl_t simplify_nanl(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments UNUSED_PARAMETER)
{
    return nodecl_null();
}
#endif

static nodecl_t simplify_nans(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    if (num_arguments == 1
            && nodecl_is_constant(arguments[0])
            && const_value_is_string(nodecl_get_constant(arguments[0])))
    {
        return const_value_to_nodecl(
                const_value_get_double(
                    // GCC does not implement this builtin but for an "" argument
                    __builtin_nans("")
                    ));
    }

    return nodecl_null();
}

static nodecl_t simplify_nansf(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    if (num_arguments == 1
            && nodecl_is_constant(arguments[0])
            && const_value_is_string(nodecl_get_constant(arguments[0])))
    {
        return const_value_to_nodecl(
                const_value_get_float(
                    // GCC does not implement this builtin but for an "" argument
                    __builtin_nansf("")
                    ));
    }

    return nodecl_null();
}

static nodecl_t simplify_nansl(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    if (num_arguments == 1
            && nodecl_is_constant(arguments[0])
            && const_value_is_string(nodecl_get_constant(arguments[0])))
    {
        return const_value_to_nodecl(
                const_value_get_long_double(
                    // GCC does not implement this builtin but for an "" argument
                    __builtin_nansl("")
                    ));
    }

    return nodecl_null();
}

SIMPLIFY_BUILTIN_FUN1(signbitf, signed_int, float);
SIMPLIFY_BUILTIN_FUN1(signbitl, signed_int, long_double);

#ifdef HAVE__BUILTIN_SIGNBIT
static nodecl_t simplify_signbit(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    if (num_arguments == 1
            && nodecl_is_constant(arguments[0])
            && const_value_is_floating(nodecl_get_constant(arguments[0])))
    {
        const_value_t* v = nodecl_get_constant(arguments[0]);

        if (const_value_is_float(v))
        {
            return const_value_to_nodecl(
                    const_value_get_signed_int(
                        __builtin_signbit(const_value_cast_to_float(v))
                        ));
        }
        else if (const_value_is_double(v))
        {
            return const_value_to_nodecl(
                    const_value_get_signed_int(
                        __builtin_signbit(const_value_cast_to_double(v))
                        ));
        }
        else if (const_value_is_long_double(v))
        {
            return const_value_to_nodecl(
                    const_value_get_signed_int(
                        __builtin_signbit(const_value_cast_to_long_double(v))
                        ));
        }
    }

    return nodecl_null();
}
#else
static nodecl_t simplify_signbit(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments UNUSED_PARAMETER)
{
    return nodecl_null();
}
#endif

#define SIMPLIFY_GENERIC_FLOAT_TEST1(builtin_name) \
static nodecl_t simplify_##builtin_name(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments) \
{ \
    if (num_arguments == 1 \
            && nodecl_is_constant(arguments[0]) \
            && const_value_is_floating(nodecl_get_constant(arguments[0]))) \
    { \
        const_value_t* v = nodecl_get_constant(arguments[0]); \
        if (const_value_is_float(v)) \
        { \
            return const_value_to_nodecl( \
                    const_value_get_signed_int( \
                        __builtin_##builtin_name( \
                            const_value_cast_to_float(v) \
                            ))); \
        } \
        else if (const_value_is_double(v)) \
        { \
            return const_value_to_nodecl( \
                    const_value_get_signed_int( \
                        __builtin_##builtin_name( \
                            const_value_cast_to_double(v) \
                            ))); \
        } \
        else if (const_value_is_long_double(v)) \
        { \
            return const_value_to_nodecl( \
                    const_value_get_signed_int( \
                        __builtin_##builtin_name( \
                            const_value_cast_to_long_double(v) \
                            ))); \
        } \
    } \
 \
    return nodecl_null(); \
}

SIMPLIFY_GENERIC_FLOAT_TEST1(isfinite)

SIMPLIFY_GENERIC_FLOAT_TEST1(isnan)
#ifdef HAVE__BUILTIN_ISNANF
SIMPLIFY_BUILTIN_FUN1(isnanf, signed_int, float);
#else
#define simplify_isnanf NO_EXPAND_FUN
#endif
#ifdef HAVE__BUILTIN_ISNANL
SIMPLIFY_BUILTIN_FUN1(isnanl, signed_int, long_double);
#else
#define simplify_isnanl NO_EXPAND_FUN
#endif

SIMPLIFY_GENERIC_FLOAT_TEST1(isnormal)

SIMPLIFY_GENERIC_FLOAT_TEST1(isinf)
#ifdef HAVE__BUILTIN_ISINFF
SIMPLIFY_BUILTIN_FUN1(isinff, signed_int, float);
#else
#define simplify_isinff NO_EXPAND_FUN
#endif
#ifdef HAVE__BUILTIN_ISINFF
SIMPLIFY_BUILTIN_FUN1(isinfl, signed_int, long_double);
#else
#define simplify_isinfl NO_EXPAND_FUN
#endif

#define SIMPLIFY_GENERIC_FLOAT_TEST2(builtin_name) \
static nodecl_t simplify_##builtin_name(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments) \
{ \
    if (num_arguments == 1 \
            && nodecl_is_constant(arguments[0]) \
            && nodecl_is_constant(arguments[1]) \
            && const_value_is_floating(nodecl_get_constant(arguments[0])) \
            && const_value_is_floating(nodecl_get_constant(arguments[1]))) \
    { \
        const_value_t* v0 = nodecl_get_constant(arguments[0]); \
        const_value_t* v1 = nodecl_get_constant(arguments[1]); \
        if (const_value_is_float(v0)) \
        { \
            if (const_value_is_float(v1)) \
            { \
                return const_value_to_nodecl( \
                        const_value_get_signed_int( \
                            __builtin_##builtin_name( \
                                const_value_cast_to_float(v0), \
                                const_value_cast_to_float(v1) \
                                ) \
                            )); \
            } \
            else if (const_value_is_double(v1)) \
            { \
                return const_value_to_nodecl( \
                        const_value_get_signed_int( \
                            __builtin_##builtin_name( \
                                const_value_cast_to_float(v0), \
                                const_value_cast_to_double(v1) \
                                ) \
                            )); \
            } \
            else if (const_value_is_long_double(v1)) \
            { \
                return const_value_to_nodecl( \
                        const_value_get_signed_int( \
                            __builtin_##builtin_name( \
                                const_value_cast_to_float(v0), \
                                const_value_cast_to_long_double(v1) \
                                ) \
                            )); \
            } \
            else return nodecl_null(); \
        } \
        else if (const_value_is_double(v0)) \
        { \
            if (const_value_is_float(v1)) \
            { \
                return const_value_to_nodecl( \
                        const_value_get_signed_int( \
                            __builtin_##builtin_name( \
                                const_value_cast_to_double(v0), \
                                const_value_cast_to_float(v1) \
                                ) \
                            )); \
            } \
            else if (const_value_is_double(v1)) \
            { \
                return const_value_to_nodecl( \
                        const_value_get_signed_int( \
                            __builtin_##builtin_name( \
                                const_value_cast_to_double(v0), \
                                const_value_cast_to_double(v1) \
                                ) \
                            )); \
            } \
            else if (const_value_is_long_double(v1)) \
            { \
                return const_value_to_nodecl( \
                        const_value_get_signed_int( \
                            __builtin_##builtin_name( \
                                const_value_cast_to_double(v0), \
                                const_value_cast_to_long_double(v1) \
                                ) \
                            )); \
            } \
            else return nodecl_null(); \
        } \
        else if (const_value_is_long_double(v0)) \
        { \
            if (const_value_is_float(v1)) \
            { \
                return const_value_to_nodecl( \
                        const_value_get_signed_int( \
                            __builtin_##builtin_name( \
                                const_value_cast_to_long_double(v0), \
                                const_value_cast_to_float(v1) \
                                ) \
                            )); \
            } \
            else if (const_value_is_double(v1)) \
            { \
                return const_value_to_nodecl( \
                        const_value_get_signed_int( \
                            __builtin_##builtin_name( \
                                const_value_cast_to_long_double(v0), \
                                const_value_cast_to_double(v1) \
                                ) \
                            )); \
            } \
            else if (const_value_is_long_double(v1)) \
            { \
                return const_value_to_nodecl( \
                        const_value_get_signed_int( \
                            __builtin_##builtin_name( \
                                const_value_cast_to_long_double(v0), \
                                const_value_cast_to_long_double(v1) \
                                ) \
                            )); \
            } \
            else return nodecl_null(); \
        } \
        else return nodecl_null(); \
    } \
 \
    return nodecl_null(); \
}

SIMPLIFY_GENERIC_FLOAT_TEST2(isgreater)
SIMPLIFY_GENERIC_FLOAT_TEST2(isgreaterequal)
SIMPLIFY_GENERIC_FLOAT_TEST2(isless)
SIMPLIFY_GENERIC_FLOAT_TEST2(islessequal)
SIMPLIFY_GENERIC_FLOAT_TEST2(islessgreater)
SIMPLIFY_GENERIC_FLOAT_TEST2(isunordered)

static scope_entry_t* solve_gcc_builtin_shuffle(scope_entry_t* overloaded_function,
        type_t** types,
        nodecl_t *arguments UNUSED_PARAMETER,
        int num_arguments,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* synthesized_function_type = NULL;
    switch (num_arguments)
    {
        case 2:
            {
                type_t* t0 = no_ref(types[0]);
                type_t* t1 = no_ref(types[1]);
                if (!is_vector_type(no_ref(t0))
                        || !is_vector_type(no_ref(t1))
                        || !is_integral_type(vector_type_get_element_type(t1))
                        || (vector_type_get_num_elements(t0) != vector_type_get_num_elements(t1)))
                        return NULL;


                parameter_info_t param_info[2];
                memset(param_info, 0, sizeof(param_info));
                param_info[0].type_info = t0;
                param_info[1].type_info = t1;
                synthesized_function_type = get_new_function_type(t0, param_info, 2, REF_QUALIFIER_NONE);
            }
            break;
        case 3:
            {
                type_t* t0 = no_ref(types[0]);
                type_t* t1 = no_ref(types[1]);
                type_t* t2 = no_ref(types[2]);
                if (!is_vector_type(no_ref(t0))
                        || !is_vector_type(no_ref(t1))
                        || !is_vector_type(no_ref(t2))
                        || (vector_type_get_num_elements(t0) != vector_type_get_num_elements(t1))
                        || (vector_type_get_num_elements(t0) != vector_type_get_num_elements(t2))
                        || !is_integral_type(vector_type_get_element_type(t2)))
                        return NULL;

                parameter_info_t param_info[3];
                memset(param_info, 0, sizeof(param_info));
                param_info[0].type_info = t0;
                param_info[1].type_info = t1;
                param_info[2].type_info = t2;
                synthesized_function_type = get_new_function_type(t0, param_info, 3, REF_QUALIFIER_NONE);
            }
            break;
        default:
            return NULL;
    }
    ERROR_CONDITION(synthesized_function_type == NULL, "Invalid synthesized function type", 0);

    const char* builtin_name = strappend(".", overloaded_function->symbol_name);
    scope_entry_list_t* entry_list = query_name_str(overloaded_function->decl_context,
            builtin_name, NULL);

    scope_entry_t* result = NULL;
    scope_entry_list_iterator_t* it;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* existing_entry = entry_list_iterator_current(it);
        if (equivalent_types(synthesized_function_type, existing_entry->type_information))
        {
            result = existing_entry;
            break;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(entry_list);

    if (result == NULL)
    {
        result = new_symbol(overloaded_function->decl_context, 
                overloaded_function->decl_context->current_scope,
                builtin_name);
        result->locus = overloaded_function->locus;
        result->symbol_name = overloaded_function->symbol_name;
        result->kind = SK_FUNCTION;

        result->type_information = synthesized_function_type;

        result->do_not_print = 1;
        symbol_entity_specs_set_is_builtin(result, 1);
    }

    return result;
}

static void gcc_sign_in_builtins_0(const decl_context_t* global_context)
{
/* Category: math builtins.  */
DEF_LIB_BUILTIN        (BUILT_IN_ACOS, "acos", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_acos)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ACOSF, "acosf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_acosf)
DEF_C99_BUILTIN        (BUILT_IN_ACOSH, "acosh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_acosh)
DEF_C99_BUILTIN        (BUILT_IN_ACOSHF, "acoshf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_acoshf)
DEF_C99_BUILTIN        (BUILT_IN_ACOSHL, "acoshl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_acoshl)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ACOSL, "acosl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_acosl)
DEF_LIB_BUILTIN        (BUILT_IN_ASIN, "asin", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_asin)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ASINF, "asinf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_asinf)
DEF_C99_BUILTIN        (BUILT_IN_ASINH, "asinh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, simplify_asinh)
DEF_C99_BUILTIN        (BUILT_IN_ASINHF, "asinhf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, simplify_asinhf)
DEF_C99_BUILTIN        (BUILT_IN_ASINHL, "asinhl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, simplify_asinhl)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ASINL, "asinl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_asinl)
DEF_LIB_BUILTIN        (BUILT_IN_ATAN, "atan", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, simplify_atan)
DEF_LIB_BUILTIN        (BUILT_IN_ATAN2, "atan2", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_atan2)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ATAN2F, "atan2f", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_atan2f)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ATAN2L, "atan2l", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_atan2l)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ATANF, "atanf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, simplify_atanf)
DEF_C99_BUILTIN        (BUILT_IN_ATANH, "atanh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_atanh)
DEF_C99_BUILTIN        (BUILT_IN_ATANHF, "atanhf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_atanhf)
DEF_C99_BUILTIN        (BUILT_IN_ATANHL, "atanhl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_atanhl)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ATANL, "atanl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, simplify_atanl)
DEF_C99_BUILTIN        (BUILT_IN_CBRT, "cbrt", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, simplify_cbrt)
DEF_C99_BUILTIN        (BUILT_IN_CBRTF, "cbrtf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, simplify_cbrtf)
DEF_C99_BUILTIN        (BUILT_IN_CBRTL, "cbrtl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, simplify_cbrtl)
DEF_LIB_BUILTIN        (BUILT_IN_CEIL, "ceil", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_ceil)
DEF_C99_C90RES_BUILTIN (BUILT_IN_CEILF, "ceilf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_ceilf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_CEILL, "ceill", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_ceill)
DEF_C99_BUILTIN        (BUILT_IN_COPYSIGN, "copysign", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_copysign)
DEF_C99_BUILTIN        (BUILT_IN_COPYSIGNF, "copysignf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_copysignf)
DEF_C99_BUILTIN        (BUILT_IN_COPYSIGNL, "copysignl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_copysignl)
DEF_LIB_BUILTIN        (BUILT_IN_COS, "cos", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, simplify_cos)
DEF_C99_C90RES_BUILTIN (BUILT_IN_COSF, "cosf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, simplify_cosf)
DEF_LIB_BUILTIN        (BUILT_IN_COSH, "cosh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_cosh)
DEF_C99_C90RES_BUILTIN (BUILT_IN_COSHF, "coshf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_coshf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_COSHL, "coshl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_coshl)
DEF_C99_C90RES_BUILTIN (BUILT_IN_COSL, "cosl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, simplify_cosl)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DREM, "drem", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DREMF, "dremf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DREML, "dreml", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_ERF, "erf", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, simplify_erf)
DEF_C99_BUILTIN        (BUILT_IN_ERFC, "erfc", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_erfc)
DEF_C99_BUILTIN        (BUILT_IN_ERFCF, "erfcf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_erfcf)
DEF_C99_BUILTIN        (BUILT_IN_ERFCL, "erfcl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_erfcl)
DEF_C99_BUILTIN        (BUILT_IN_ERFF, "erff", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, simplify_erff)
DEF_C99_BUILTIN        (BUILT_IN_ERFL, "erfl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, simplify_erfl)
DEF_LIB_BUILTIN        (BUILT_IN_EXP, "exp", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_exp)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_EXP10, "exp10", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_exp10)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_EXP10F, "exp10f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_exp10f)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_EXP10L, "exp10l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_exp10l)
DEF_C99_BUILTIN        (BUILT_IN_EXP2, "exp2", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_exp2)
DEF_C99_BUILTIN        (BUILT_IN_EXP2F, "exp2f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_exp2f)
DEF_C99_BUILTIN        (BUILT_IN_EXP2L, "exp2l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_exp2l)
DEF_C99_C90RES_BUILTIN (BUILT_IN_EXPF, "expf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_expf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_EXPL, "expl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_expl)
DEF_C99_BUILTIN        (BUILT_IN_EXPM1, "expm1", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_expm1)
DEF_C99_BUILTIN        (BUILT_IN_EXPM1F, "expm1f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_expm1f)
DEF_C99_BUILTIN        (BUILT_IN_EXPM1L, "expm1l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_expm1l)
DEF_LIB_BUILTIN        (BUILT_IN_FABS, "fabs", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_fabs)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FABSF, "fabsf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_fabsf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FABSL, "fabsl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_fabsl)
DEF_C99_BUILTIN        (BUILT_IN_FDIM, "fdim", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_fdim)
DEF_C99_BUILTIN        (BUILT_IN_FDIMF, "fdimf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_fdimf)
DEF_C99_BUILTIN        (BUILT_IN_FDIML, "fdiml", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_fdiml)
DEF_LIB_BUILTIN        (BUILT_IN_FLOOR, "floor", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_floor)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FLOORF, "floorf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_floorf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FLOORL, "floorl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_floorl)
DEF_C99_BUILTIN        (BUILT_IN_FMA, "fma", BT_FN_DOUBLE_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, simplify_fma)
DEF_C99_BUILTIN        (BUILT_IN_FMAF, "fmaf", BT_FN_FLOAT_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, simplify_fmaf)
DEF_C99_BUILTIN        (BUILT_IN_FMAL, "fmal", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, simplify_fmal)
DEF_C99_BUILTIN        (BUILT_IN_FMAX, "fmax", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_fmax)
DEF_C99_BUILTIN        (BUILT_IN_FMAXF, "fmaxf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_fmaxf)
DEF_C99_BUILTIN        (BUILT_IN_FMAXL, "fmaxl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_fmaxl)
DEF_C99_BUILTIN        (BUILT_IN_FMIN, "fmin", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_fmin)
DEF_C99_BUILTIN        (BUILT_IN_FMINF, "fminf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_fminf)
DEF_C99_BUILTIN        (BUILT_IN_FMINL, "fminl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_fminl)
DEF_LIB_BUILTIN        (BUILT_IN_FMOD, "fmod", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_fmod)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FMODF, "fmodf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_fmodf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FMODL, "fmodl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_fmodl)
DEF_LIB_BUILTIN        (BUILT_IN_FREXP, "frexp", BT_FN_DOUBLE_DOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FREXPF, "frexpf", BT_FN_FLOAT_FLOAT_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_C99_C90RES_BUILTIN (BUILT_IN_FREXPL, "frexpl", BT_FN_LONGDOUBLE_LONGDOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMA, "gamma", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMAF, "gammaf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMAL, "gammal", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMA_R, "gamma_r", BT_FN_DOUBLE_DOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMAF_R, "gammaf_r", BT_FN_FLOAT_FLOAT_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GAMMAL_R, "gammal_r", BT_FN_LONGDOUBLE_LONGDOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_HUGE_VAL, "huge_val", BT_FN_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_huge_val)
DEF_GCC_BUILTIN        (BUILT_IN_HUGE_VALF, "huge_valf", BT_FN_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_huge_valf)
DEF_GCC_BUILTIN        (BUILT_IN_HUGE_VALL, "huge_vall", BT_FN_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_huge_vall)
#ifdef HAVE_QUADMATH_H
DEF_GCC_BUILTIN        (BUILT_IN_HUGE_VALL, "huge_valq", BT_FN_FLOAT128, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_huge_valq)
#endif
DEF_C99_BUILTIN        (BUILT_IN_HYPOT, "hypot", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_hypot)
DEF_C99_BUILTIN        (BUILT_IN_HYPOTF, "hypotf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_hypotf)
DEF_C99_BUILTIN        (BUILT_IN_HYPOTL, "hypotl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_hypotl)
DEF_C99_BUILTIN        (BUILT_IN_ILOGB, "ilogb", BT_FN_INT_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_ilogb)
DEF_C99_BUILTIN        (BUILT_IN_ILOGBF, "ilogbf", BT_FN_INT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_ilogbf)
DEF_C99_BUILTIN        (BUILT_IN_ILOGBL, "ilogbl", BT_FN_INT_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_ilogbl)
DEF_GCC_BUILTIN        (BUILT_IN_INF, "inf", BT_FN_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_INFF, "inff", BT_FN_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_INFL, "infl", BT_FN_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN	       (BUILT_IN_INFD32, "infd32", BT_FN_DFLOAT32, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_INFD64, "infd64", BT_FN_DFLOAT64, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_INFD128, "infd128", BT_FN_DFLOAT128, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J0, "j0", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J0F, "j0f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J0L, "j0l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J1, "j1", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J1F, "j1f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_J1L, "j1l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_JN, "jn", BT_FN_DOUBLE_INT_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_JNF, "jnf", BT_FN_FLOAT_INT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_JNL, "jnl", BT_FN_LONGDOUBLE_INT_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LCEIL, "lceil", BT_FN_LONG_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LCEILF, "lceilf", BT_FN_LONG_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LCEILL, "lceill", BT_FN_LONG_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_LDEXP, "ldexp", BT_FN_DOUBLE_DOUBLE_INT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_ldexp)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LDEXPF, "ldexpf", BT_FN_FLOAT_FLOAT_INT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_ldexpf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LDEXPL, "ldexpl", BT_FN_LONGDOUBLE_LONGDOUBLE_INT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_ldexpl)
DEF_GCC_BUILTIN        (BUILT_IN_LFLOOR, "lfloor", BT_FN_LONG_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LFLOORF, "lfloorf", BT_FN_LONG_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LFLOORL, "lfloorl", BT_FN_LONG_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_LGAMMA, "lgamma", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_STORE, simplify_lgamma)
DEF_C99_BUILTIN        (BUILT_IN_LGAMMAF, "lgammaf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_STORE, simplify_lgammaf)
DEF_C99_BUILTIN        (BUILT_IN_LGAMMAL, "lgammal", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_STORE, simplify_lgammal)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_LGAMMA_R, "lgamma_r", BT_FN_DOUBLE_DOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_LGAMMAF_R, "lgammaf_r", BT_FN_FLOAT_FLOAT_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_LGAMMAL_R, "lgammal_r", BT_FN_LONGDOUBLE_LONGDOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LLCEIL, "llceil", BT_FN_LONGLONG_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LLCEILF, "llceilf", BT_FN_LONGLONG_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LLCEILL, "llceill", BT_FN_LONGLONG_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LLFLOOR, "llfloor", BT_FN_LONGLONG_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LLFLOORF, "llfloorf", BT_FN_LONGLONG_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_LLFLOORL, "llfloorl", BT_FN_LONGLONG_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_LLRINT, "llrint", BT_FN_LONGLONG_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_llrint)
DEF_C99_BUILTIN        (BUILT_IN_LLRINTF, "llrintf", BT_FN_LONGLONG_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_llrintf)
DEF_C99_BUILTIN        (BUILT_IN_LLRINTL, "llrintl", BT_FN_LONGLONG_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_llrintl)
DEF_C99_BUILTIN        (BUILT_IN_LLROUND, "llround", BT_FN_LONGLONG_DOUBLE, ATTR_MATHFN_ERRNO, simplify_llround)
DEF_C99_BUILTIN        (BUILT_IN_LLROUNDF, "llroundf", BT_FN_LONGLONG_FLOAT, ATTR_MATHFN_ERRNO, simplify_llroundf)
DEF_C99_BUILTIN        (BUILT_IN_LLROUNDL, "llroundl", BT_FN_LONGLONG_LONGDOUBLE, ATTR_MATHFN_ERRNO, simplify_llroundl)
DEF_LIB_BUILTIN        (BUILT_IN_LOG, "log", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log)
DEF_LIB_BUILTIN        (BUILT_IN_LOG10, "log10", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log10)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LOG10F, "log10f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log10f)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LOG10L, "log10l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log10l)
DEF_C99_BUILTIN        (BUILT_IN_LOG1P, "log1p", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log1p)
DEF_C99_BUILTIN        (BUILT_IN_LOG1PF, "log1pf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log1pf)
DEF_C99_BUILTIN        (BUILT_IN_LOG1PL, "log1pl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log1pl)
DEF_C99_BUILTIN        (BUILT_IN_LOG2, "log2", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log2)
DEF_C99_BUILTIN        (BUILT_IN_LOG2F, "log2f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log2f)
DEF_C99_BUILTIN        (BUILT_IN_LOG2L, "log2l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_log2l)
DEF_C99_BUILTIN        (BUILT_IN_LOGB, "logb", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_logb)
DEF_C99_BUILTIN        (BUILT_IN_LOGBF, "logbf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_logbf)
DEF_C99_BUILTIN        (BUILT_IN_LOGBL, "logbl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_logbl)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LOGF, "logf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_logf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_LOGL, "logl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_logl)
DEF_C99_BUILTIN        (BUILT_IN_LRINT, "lrint", BT_FN_LONG_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_lrint)
DEF_C99_BUILTIN        (BUILT_IN_LRINTF, "lrintf", BT_FN_LONG_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_lrintf)
DEF_C99_BUILTIN        (BUILT_IN_LRINTL, "lrintl", BT_FN_LONG_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_lrintl)
DEF_C99_BUILTIN        (BUILT_IN_LROUND, "lround", BT_FN_LONG_DOUBLE, ATTR_MATHFN_ERRNO, simplify_lround)
DEF_C99_BUILTIN        (BUILT_IN_LROUNDF, "lroundf", BT_FN_LONG_FLOAT, ATTR_MATHFN_ERRNO, simplify_lroundf)
DEF_C99_BUILTIN        (BUILT_IN_LROUNDL, "lroundl", BT_FN_LONG_LONGDOUBLE, ATTR_MATHFN_ERRNO, simplify_lroundl)
DEF_LIB_BUILTIN        (BUILT_IN_MODF, "modf", BT_FN_DOUBLE_DOUBLE_DOUBLEPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_C99_C90RES_BUILTIN (BUILT_IN_MODFF, "modff", BT_FN_FLOAT_FLOAT_FLOATPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_C99_C90RES_BUILTIN (BUILT_IN_MODFL, "modfl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLEPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_NAN, "nan", BT_FN_DOUBLE_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL, simplify_nan)
DEF_C99_BUILTIN        (BUILT_IN_NANF, "nanf", BT_FN_FLOAT_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL, simplify_nanf)
DEF_C99_BUILTIN        (BUILT_IN_NANL, "nanl", BT_FN_LONGDOUBLE_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL, simplify_nanl)
DEF_GCC_BUILTIN        (BUILT_IN_NAND32, "nand32", BT_FN_DFLOAT32_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_NAND64, "nand64", BT_FN_DFLOAT64_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_NAND128, "nand128", BT_FN_DFLOAT128_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_NANS, "nans", BT_FN_DOUBLE_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL, simplify_nans)
DEF_GCC_BUILTIN        (BUILT_IN_NANSF, "nansf", BT_FN_FLOAT_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL, simplify_nansf)
DEF_GCC_BUILTIN        (BUILT_IN_NANSL, "nansl", BT_FN_LONGDOUBLE_CONST_STRING, ATTR_CONST_NOTHROW_NONNULL, simplify_nansl)
DEF_C99_BUILTIN        (BUILT_IN_NEARBYINT, "nearbyint", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_nearbyint)
DEF_C99_BUILTIN        (BUILT_IN_NEARBYINTF, "nearbyintf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_nearbyintf)
DEF_C99_BUILTIN        (BUILT_IN_NEARBYINTL, "nearbyintl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_nearbyintl)
DEF_C99_BUILTIN        (BUILT_IN_NEXTAFTER, "nextafter", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_nextafter)
DEF_C99_BUILTIN        (BUILT_IN_NEXTAFTERF, "nextafterf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_nextafterf)
DEF_C99_BUILTIN        (BUILT_IN_NEXTAFTERL, "nextafterl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_nextafterl)
DEF_C99_BUILTIN        (BUILT_IN_NEXTTOWARD, "nexttoward", BT_FN_DOUBLE_DOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_nexttoward)
DEF_C99_BUILTIN        (BUILT_IN_NEXTTOWARDF, "nexttowardf", BT_FN_FLOAT_FLOAT_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_nexttowardf)
DEF_C99_BUILTIN        (BUILT_IN_NEXTTOWARDL, "nexttowardl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_nexttowardl)
DEF_LIB_BUILTIN        (BUILT_IN_POW, "pow", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_pow)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_POW10, "pow10", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_POW10F, "pow10f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_POW10L, "pow10l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_C99_C90RES_BUILTIN (BUILT_IN_POWF, "powf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_powf)
DEF_GCC_BUILTIN        (BUILT_IN_POWI, "powi", BT_FN_DOUBLE_DOUBLE_INT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_POWIF, "powif", BT_FN_FLOAT_FLOAT_INT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_POWIL, "powil", BT_FN_LONGDOUBLE_LONGDOUBLE_INT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_C90RES_BUILTIN (BUILT_IN_POWL, "powl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_powl)
DEF_C99_BUILTIN        (BUILT_IN_REMAINDER, "remainder", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_remainder)
DEF_C99_BUILTIN        (BUILT_IN_REMAINDERF, "remainderf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_remainderf)
DEF_C99_BUILTIN        (BUILT_IN_REMAINDERL, "remainderl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_remainderl)
DEF_C99_BUILTIN        (BUILT_IN_REMQUO, "remquo", BT_FN_DOUBLE_DOUBLE_DOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_REMQUOF, "remquof", BT_FN_FLOAT_FLOAT_FLOAT_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_REMQUOL, "remquol", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE_INTPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_RINT, "rint", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_rint)
DEF_C99_BUILTIN        (BUILT_IN_RINTF, "rintf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_rintf)
DEF_C99_BUILTIN        (BUILT_IN_RINTL, "rintl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_rintl)
DEF_C99_BUILTIN        (BUILT_IN_ROUND, "round", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_round)
DEF_C99_BUILTIN        (BUILT_IN_ROUNDF, "roundf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_roundf)
DEF_C99_BUILTIN        (BUILT_IN_ROUNDL, "roundl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_roundl)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SCALB, "scalb", BT_FN_DOUBLE_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SCALBF, "scalbf", BT_FN_FLOAT_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SCALBL, "scalbl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_SCALBLN, "scalbln", BT_FN_DOUBLE_DOUBLE_LONG, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_scalbln)
DEF_C99_BUILTIN        (BUILT_IN_SCALBLNF, "scalblnf", BT_FN_FLOAT_FLOAT_LONG, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_scalblnf)
DEF_C99_BUILTIN        (BUILT_IN_SCALBLNL, "scalblnl", BT_FN_LONGDOUBLE_LONGDOUBLE_LONG, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_scalblnl)
DEF_C99_BUILTIN        (BUILT_IN_SCALBN, "scalbn", BT_FN_DOUBLE_DOUBLE_INT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_scalbn)
DEF_C99_BUILTIN        (BUILT_IN_SCALBNF, "scalbnf", BT_FN_FLOAT_FLOAT_INT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_scalbnf)
DEF_C99_BUILTIN        (BUILT_IN_SCALBNL, "scalbnl", BT_FN_LONGDOUBLE_LONGDOUBLE_INT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_scalbnl)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBIT, "signbit", BT_FN_INT_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_signbit)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITF, "signbitf", BT_FN_INT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_signbitf)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITL, "signbitl", BT_FN_INT_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_signbitl)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITD32, "signbitd32", BT_FN_INT_DFLOAT32, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITD64, "signbitd64", BT_FN_INT_DFLOAT64, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNBITD128, "signbitd128", BT_FN_INT_DFLOAT128, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNIFICAND, "significand", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNIFICANDF, "significandf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SIGNIFICANDL, "significandl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_SIN, "sin", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, simplify_sin)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SINCOS, "sincos", BT_FN_VOID_DOUBLE_DOUBLEPTR_DOUBLEPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SINCOSF, "sincosf", BT_FN_VOID_FLOAT_FLOATPTR_FLOATPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SINCOSL, "sincosl", BT_FN_VOID_LONGDOUBLE_LONGDOUBLEPTR_LONGDOUBLEPTR, ATTR_MATHFN_FPROUNDING_STORE, NO_EXPAND_FUN)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SINF, "sinf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, simplify_sinf)
DEF_LIB_BUILTIN        (BUILT_IN_SINH, "sinh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_sinh)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SINHF, "sinhf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_sinhf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SINHL, "sinhl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_sinhl)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SINL, "sinl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, simplify_sinl)
DEF_LIB_BUILTIN        (BUILT_IN_SQRT, "sqrt", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_sqrt)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SQRTF, "sqrtf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_sqrtf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_SQRTL, "sqrtl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_sqrtl)
DEF_LIB_BUILTIN        (BUILT_IN_TAN, "tan", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, simplify_tan)
DEF_C99_C90RES_BUILTIN (BUILT_IN_TANF, "tanf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, simplify_tanf)
DEF_LIB_BUILTIN        (BUILT_IN_TANH, "tanh", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, simplify_tanh)
DEF_C99_C90RES_BUILTIN (BUILT_IN_TANHF, "tanhf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, simplify_tanhf)
DEF_C99_C90RES_BUILTIN (BUILT_IN_TANHL, "tanhl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, simplify_tanhl)
DEF_C99_C90RES_BUILTIN (BUILT_IN_TANL, "tanl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, simplify_tanl)
DEF_C99_BUILTIN        (BUILT_IN_TGAMMA, "tgamma", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_tgamma)
DEF_C99_BUILTIN        (BUILT_IN_TGAMMAF, "tgammaf", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_tgammaf)
DEF_C99_BUILTIN        (BUILT_IN_TGAMMAL, "tgammal", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, simplify_tgammal)
DEF_C99_BUILTIN        (BUILT_IN_TRUNC, "trunc", BT_FN_DOUBLE_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_trunc)
DEF_C99_BUILTIN        (BUILT_IN_TRUNCF, "truncf", BT_FN_FLOAT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_truncf)
DEF_C99_BUILTIN        (BUILT_IN_TRUNCL, "truncl", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_truncl)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y0, "y0", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y0F, "y0f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y0L, "y0l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y1, "y1", BT_FN_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y1F, "y1f", BT_FN_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_Y1L, "y1l", BT_FN_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_YN, "yn", BT_FN_DOUBLE_INT_DOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_YNF, "ynf", BT_FN_FLOAT_INT_FLOAT, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_YNL, "ynl", BT_FN_LONGDOUBLE_INT_LONGDOUBLE, ATTR_MATHFN_FPROUNDING_ERRNO, NO_EXPAND_FUN)

/* Category: _Complex math builtins.  */
DEF_C99_BUILTIN        (BUILT_IN_CABS, "cabs", BT_FN_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CABSF, "cabsf", BT_FN_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CABSL, "cabsl", BT_FN_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CACOS, "cacos", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CACOSF, "cacosf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CACOSH, "cacosh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CACOSHF, "cacoshf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CACOSHL, "cacoshl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CACOSL, "cacosl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CARG, "carg", BT_FN_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CARGF, "cargf", BT_FN_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CARGL, "cargl", BT_FN_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CASIN, "casin", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CASINF, "casinf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CASINH, "casinh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CASINHF, "casinhf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CASINHL, "casinhl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CASINL, "casinl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CATAN, "catan", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CATANF, "catanf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CATANH, "catanh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CATANHF, "catanhf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CATANHL, "catanhl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CATANL, "catanl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CCOS, "ccos", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CCOSF, "ccosf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CCOSH, "ccosh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CCOSHF, "ccoshf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CCOSHL, "ccoshl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CCOSL, "ccosl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CEXP, "cexp", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CEXPF, "cexpf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CEXPL, "cexpl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_CEXPI, "cexpi", BT_FN_COMPLEX_DOUBLE_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_CEXPIF, "cexpif", BT_FN_COMPLEX_FLOAT_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_CEXPIL, "cexpil", BT_FN_COMPLEX_LONGDOUBLE_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CIMAG, "cimag", BT_FN_DOUBLE_COMPLEX_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CIMAGF, "cimagf", BT_FN_FLOAT_COMPLEX_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CIMAGL, "cimagl", BT_FN_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CLOG, "clog", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CLOGF, "clogf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CLOGL, "clogl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_EXT_C99RES_BUILTIN (BUILT_IN_CLOG10, "clog10", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_EXT_C99RES_BUILTIN (BUILT_IN_CLOG10F, "clog10f", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_EXT_C99RES_BUILTIN (BUILT_IN_CLOG10L, "clog10l", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CONJ, "conj", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CONJF, "conjf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CONJL, "conjl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CPOW, "cpow", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CPOWF, "cpowf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CPOWL, "cpowl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CPROJ, "cproj", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CPROJF, "cprojf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CPROJL, "cprojl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CREAL, "creal", BT_FN_DOUBLE_COMPLEX_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CREALF, "crealf", BT_FN_FLOAT_COMPLEX_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CREALL, "creall", BT_FN_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CSIN, "csin", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CSINF, "csinf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CSINH, "csinh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CSINHF, "csinhf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CSINHL, "csinhl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CSINL, "csinl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CSQRT, "csqrt", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CSQRTF, "csqrtf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CSQRTL, "csqrtl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CTAN, "ctan", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CTANF, "ctanf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CTANH, "ctanh", BT_FN_COMPLEX_DOUBLE_COMPLEX_DOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CTANHF, "ctanhf", BT_FN_COMPLEX_FLOAT_COMPLEX_FLOAT, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CTANHL, "ctanhl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_CTANL, "ctanl", BT_FN_COMPLEX_LONGDOUBLE_COMPLEX_LONGDOUBLE, ATTR_MATHFN_FPROUNDING, NO_EXPAND_FUN)

/* Category: string/memory builtins.  */
/* bcmp, bcopy and bzero have traditionally accepted NULL pointers
   when the length parameter is zero, so don't apply attribute "nonnull".  */
DEF_EXT_LIB_BUILTIN    (BUILT_IN_BCMP, "bcmp", BT_FN_INT_CONST_PTR_CONST_PTR_SIZE, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_BCOPY, "bcopy", BT_FN_VOID_CONST_PTR_PTR_SIZE, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_BZERO, "bzero", BT_FN_VOID_PTR_SIZE, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_INDEX, "index", BT_FN_STRING_CONST_STRING_INT, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_MEMCHR, "memchr", BT_FN_PTR_CONST_PTR_INT_SIZE, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_MEMCMP, "memcmp", BT_FN_INT_CONST_PTR_CONST_PTR_SIZE, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_MEMCPY, "memcpy", BT_FN_PTR_PTR_CONST_PTR_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_MEMMOVE, "memmove", BT_FN_PTR_PTR_CONST_PTR_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMPCPY, "mempcpy", BT_FN_PTR_PTR_CONST_PTR_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_MEMSET, "memset", BT_FN_PTR_PTR_INT_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_RINDEX, "rindex", BT_FN_STRING_CONST_STRING_INT, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STPCPY, "stpcpy", BT_FN_STRING_STRING_CONST_STRING, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STPNCPY, "stpncpy", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRCASECMP, "strcasecmp", BT_FN_INT_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRCAT, "strcat", BT_FN_STRING_STRING_CONST_STRING, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRCHR, "strchr", BT_FN_STRING_CONST_STRING_INT, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRCMP, "strcmp", BT_FN_INT_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRCPY, "strcpy", BT_FN_STRING_STRING_CONST_STRING, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRCSPN, "strcspn", BT_FN_SIZE_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRDUP, "strdup", BT_FN_STRING_CONST_STRING, ATTR_MALLOC_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRNDUP, "strndup", BT_FN_STRING_CONST_STRING_SIZE, ATTR_MALLOC_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRLEN, "strlen", BT_FN_SIZE_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRNCASECMP, "strncasecmp", BT_FN_INT_CONST_STRING_CONST_STRING_SIZE, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRNCAT, "strncat", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRNCMP, "strncmp", BT_FN_INT_CONST_STRING_CONST_STRING_SIZE, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRNCPY, "strncpy", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRPBRK, "strpbrk", BT_FN_STRING_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRRCHR, "strrchr", BT_FN_STRING_CONST_STRING_INT, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRSPN, "strspn", BT_FN_SIZE_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRSTR, "strstr", BT_FN_STRING_CONST_STRING_CONST_STRING, ATTR_PURE_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)

/* Category: stdio builtins.  */
DEF_LIB_BUILTIN        (BUILT_IN_FPRINTF, "fprintf", BT_FN_INT_FILEPTR_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_2_3, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FPRINTF_UNLOCKED, "fprintf_unlocked", BT_FN_INT_FILEPTR_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_2_3, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_PUTC, "putc", BT_FN_INT_INT_FILEPTR, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PUTC_UNLOCKED, "putc_unlocked", BT_FN_INT_INT_FILEPTR, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_FPUTC, "fputc", BT_FN_INT_INT_FILEPTR, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FPUTC_UNLOCKED, "fputc_unlocked", BT_FN_INT_INT_FILEPTR, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_FPUTS, "fputs", BT_FN_INT_CONST_STRING_FILEPTR, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FPUTS_UNLOCKED, "fputs_unlocked", BT_FN_INT_CONST_STRING_FILEPTR, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_FSCANF, "fscanf", BT_FN_INT_FILEPTR_CONST_STRING_VAR, ATTR_FORMAT_SCANF_2_3, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_FWRITE, "fwrite", BT_FN_SIZE_CONST_PTR_SIZE_SIZE_FILEPTR, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FWRITE_UNLOCKED, "fwrite_unlocked", BT_FN_SIZE_CONST_PTR_SIZE_SIZE_FILEPTR, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_PRINTF, "printf", BT_FN_INT_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_1_2, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PRINTF_UNLOCKED, "printf_unlocked", BT_FN_INT_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_1_2, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_PUTCHAR, "putchar", BT_FN_INT_INT, ATTR_NULL, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PUTCHAR_UNLOCKED, "putchar_unlocked", BT_FN_INT_INT, ATTR_NULL, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_PUTS, "puts", BT_FN_INT_CONST_STRING, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PUTS_UNLOCKED, "puts_unlocked", BT_FN_INT_CONST_STRING, ATTR_NONNULL_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_SCANF, "scanf", BT_FN_INT_CONST_STRING_VAR, ATTR_FORMAT_SCANF_1_2, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_SNPRINTF, "snprintf", BT_FN_INT_STRING_SIZE_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_NOTHROW_3_4, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_SPRINTF, "sprintf", BT_FN_INT_STRING_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_NOTHROW_2_3, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_SSCANF, "sscanf", BT_FN_INT_CONST_STRING_CONST_STRING_VAR, ATTR_FORMAT_SCANF_NOTHROW_2_3, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_VFPRINTF, "vfprintf", BT_FN_INT_FILEPTR_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_2_0, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_VFSCANF, "vfscanf", BT_FN_INT_FILEPTR_CONST_STRING_VALIST_ARG, ATTR_FORMAT_SCANF_2_0, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_VPRINTF, "vprintf", BT_FN_INT_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_1_0, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_VSCANF, "vscanf", BT_FN_INT_CONST_STRING_VALIST_ARG, ATTR_FORMAT_SCANF_1_0, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_VSNPRINTF, "vsnprintf", BT_FN_INT_STRING_SIZE_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_NOTHROW_3_0, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_VSPRINTF, "vsprintf", BT_FN_INT_STRING_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_NOTHROW_2_0, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_VSSCANF, "vsscanf", BT_FN_INT_CONST_STRING_CONST_STRING_VALIST_ARG, ATTR_FORMAT_SCANF_NOTHROW_2_0, NO_EXPAND_FUN)

/* Category: ctype builtins.  */
DEF_LIB_BUILTIN        (BUILT_IN_ISALNUM, "isalnum", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISALPHA, "isalpha", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISASCII, "isascii", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_ISBLANK, "isblank", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISCNTRL, "iscntrl", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISDIGIT, "isdigit", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISGRAPH, "isgraph", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISLOWER, "islower", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISPRINT, "isprint", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISPUNCT, "ispunct", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISSPACE, "isspace", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISUPPER, "isupper", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ISXDIGIT, "isxdigit", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_TOASCII, "toascii", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_TOLOWER, "tolower", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_TOUPPER, "toupper", BT_FN_INT_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

/* Category: wctype builtins.  */
DEF_C94_BUILTIN        (BUILT_IN_ISWALNUM, "iswalnum", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWALPHA, "iswalpha", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_ISWBLANK, "iswblank", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWCNTRL, "iswcntrl", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWDIGIT, "iswdigit", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWGRAPH, "iswgraph", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWLOWER, "iswlower", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWPRINT, "iswprint", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWPUNCT, "iswpunct", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWSPACE, "iswspace", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWUPPER, "iswupper", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_ISWXDIGIT, "iswxdigit", BT_FN_INT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_TOWLOWER, "towlower", BT_FN_WINT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C94_BUILTIN        (BUILT_IN_TOWUPPER, "towupper", BT_FN_WINT_WINT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

/* Category: miscellaneous builtins.  */
DEF_LIB_BUILTIN        (BUILT_IN_ABORT, "abort", BT_FN_VOID, ATTR_NORETURN_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_ABS, "abs", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_abs)
DEF_GCC_BUILTIN        (BUILT_IN_AGGREGATE_INCOMING_ADDRESS, "aggregate_incoming_address", BT_FN_PTR_VAR, ATTR_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ALLOCA, "alloca", BT_FN_PTR_SIZE, ATTR_MALLOC_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_APPLY, "apply", BT_FN_PTR_PTR_FN_VOID_VAR_PTR_SIZE, ATTR_NULL, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_APPLY_ARGS, "apply_args", BT_FN_PTR_VAR, ATTR_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_BSWAP32, "bswap32", BT_FN_UINT32_UINT32, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_BSWAP64, "bswap64", BT_FN_UINT64_UINT64, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_CLEAR_CACHE, "__clear_cache", BT_FN_VOID_PTR_PTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_CALLOC, "calloc", BT_FN_PTR_SIZE_SIZE, ATTR_MALLOC_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_CLASSIFY_TYPE, "classify_type", BT_FN_INT_VAR, ATTR_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_CLZ, "clz", BT_FN_INT_UINT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_clz)
DEF_GCC_BUILTIN        (BUILT_IN_CLZIMAX, "clzimax", BT_FN_INT_UINTMAX, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_CLZL, "clzl", BT_FN_INT_ULONG, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_clzl)
DEF_GCC_BUILTIN        (BUILT_IN_CLZLL, "clzll", BT_FN_INT_ULONGLONG, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_clzll)
DEF_GCC_BUILTIN        (BUILT_IN_CLZS, "clzs", BT_FN_INT_UINT16, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_CONSTANT_P, "constant_p", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_CTZ, "ctz", BT_FN_INT_UINT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_ctz)
DEF_GCC_BUILTIN        (BUILT_IN_CTZIMAX, "ctzimax", BT_FN_INT_UINTMAX, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_CTZL, "ctzl", BT_FN_INT_ULONG, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_ctzl)
DEF_GCC_BUILTIN        (BUILT_IN_CTZLL, "ctzll", BT_FN_INT_ULONGLONG, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_ctzll)
DEF_GCC_BUILTIN        (BUILT_IN_CLZS, "ctzs", BT_FN_INT_UINT16, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DCGETTEXT, "dcgettext", BT_FN_STRING_CONST_STRING_CONST_STRING_INT, ATTR_FORMAT_ARG_2, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_DGETTEXT, "dgettext", BT_FN_STRING_CONST_STRING_CONST_STRING, ATTR_FORMAT_ARG_2, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_DWARF_CFA, "dwarf_cfa", BT_FN_PTR, ATTR_NULL, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_DWARF_SP_COLUMN, "dwarf_sp_column", BT_FN_UINT, ATTR_NULL, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_EH_RETURN, "eh_return", BT_FN_VOID_PTRMODE_PTR, ATTR_NORETURN_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_EH_RETURN_DATA_REGNO, "eh_return_data_regno", BT_FN_INT_INT, ATTR_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECL, "execl", BT_FN_INT_CONST_STRING_CONST_STRING_VAR, ATTR_SENTINEL_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECLP, "execlp", BT_FN_INT_CONST_STRING_CONST_STRING_VAR, ATTR_SENTINEL_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECLE, "execle", BT_FN_INT_CONST_STRING_CONST_STRING_VAR, ATTR_NOTHROW_SENTINEL_1, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECV, "execv", BT_FN_INT_CONST_STRING_PTR_CONST_STRING, ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECVP, "execvp", BT_FN_INT_CONST_STRING_PTR_CONST_STRING, ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_EXECVE, "execve", BT_FN_INT_CONST_STRING_PTR_CONST_STRING_PTR_CONST_STRING, ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_EXIT, "exit", BT_FN_VOID_INT, ATTR_NORETURN_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_EXPECT, "expect", BT_FN_LONG_LONG_LONG, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_EXTEND_POINTER, "extend_pointer", BT_FN_UNWINDWORD_PTR, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_EXTRACT_RETURN_ADDR, "extract_return_addr", BT_FN_PTR_PTR, ATTR_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FFS, "ffs", BT_FN_INT_INT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FFSIMAX, "ffsimax", BT_FN_INT_INTMAX, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FFSL, "ffsl", BT_FN_INT_LONG, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FFSLL, "ffsll", BT_FN_INT_LONGLONG, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN        (BUILT_IN_FORK, "fork", BT_FN_PID, ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_FRAME_ADDRESS, "frame_address", BT_FN_PTR_UINT, ATTR_NULL, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_FREE, "free", BT_FN_VOID_PTR, ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_FROB_RETURN_ADDR, "frob_return_addr", BT_FN_PTR_PTR, ATTR_NULL, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_GETTEXT, "gettext", BT_FN_STRING_CONST_STRING, ATTR_FORMAT_ARG_1, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN_IMAXABS, "imaxabs", BT_FN_INTMAX_INTMAX, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_INIT_DWARF_REG_SIZES, "init_dwarf_reg_size_table", BT_FN_VOID_PTR, ATTR_NULL, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITE, "finite", BT_FN_INT_DOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITEF, "finitef", BT_FN_INT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITEL, "finitel", BT_FN_INT_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITED32, "finited32", BT_FN_INT_DFLOAT32, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITED64, "finited64", BT_FN_INT_DFLOAT64, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FINITED128, "finited128", BT_FN_INT_DFLOAT128, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_FPCLASSIFY, "fpclassify", BT_FN_INT_INT_INT_INT_INT_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_fpclassify)
DEF_GCC_BUILTIN        (BUILT_IN_ISFINITE, "isfinite", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_isfinite)
DEF_GCC_BUILTIN        (BUILT_IN_ISINF_SIGN, "isinf_sign", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, NO_EXPAND_FUN)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ISINF, "isinf", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC, simplify_isinf)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFF, "isinff", BT_FN_INT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_isinff)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFL, "isinfl", BT_FN_INT_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_isinfl)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFD32, "isinfd32", BT_FN_INT_DFLOAT32, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFD64, "isinfd64", BT_FN_INT_DFLOAT64, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISINFD128, "isinfd128", BT_FN_INT_DFLOAT128, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_C90RES_BUILTIN (BUILT_IN_ISNAN, "isnan", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_isnan)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNANF, "isnanf", BT_FN_INT_FLOAT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_isnanf)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNANL, "isnanl", BT_FN_INT_LONGDOUBLE, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_isnanl)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNAND32, "isnand32", BT_FN_INT_DFLOAT32, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNAND64, "isnand64", BT_FN_INT_DFLOAT64, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_ISNAND128, "isnand128", BT_FN_INT_DFLOAT128, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_ISNORMAL, "isnormal", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_isnormal)
DEF_GCC_BUILTIN        (BUILT_IN_ISGREATER, "isgreater", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_isgreater)
DEF_GCC_BUILTIN        (BUILT_IN_ISGREATEREQUAL, "isgreaterequal", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_isgreaterequal)
DEF_GCC_BUILTIN        (BUILT_IN_ISLESS, "isless", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_isless)
DEF_GCC_BUILTIN        (BUILT_IN_ISLESSEQUAL, "islessequal", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_islessequal)
DEF_GCC_BUILTIN        (BUILT_IN_ISLESSGREATER, "islessgreater", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_islessgreater)
DEF_GCC_BUILTIN        (BUILT_IN_ISUNORDERED, "isunordered", BT_FN_INT_VAR, ATTR_CONST_NOTHROW_TYPEGENERIC_LEAF, simplify_isunordered)
DEF_LIB_BUILTIN        (BUILT_IN_LABS, "labs", BT_FN_LONG_LONG, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_labs)
DEF_C99_BUILTIN        (BUILT_IN_LLABS, "llabs", BT_FN_LONGLONG_LONGLONG, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_llabs)
DEF_GCC_BUILTIN        (BUILT_IN_LONGJMP, "longjmp", BT_FN_VOID_PTR_INT, ATTR_NORETURN_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_MALLOC, "malloc", BT_FN_PTR_SIZE, ATTR_MALLOC_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_NEXT_ARG, "next_arg", BT_FN_PTR_VAR, ATTR_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_PARITY, "parity", BT_FN_INT_UINT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_PARITYIMAX, "parityimax", BT_FN_INT_UINTMAX, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_PARITYL, "parityl", BT_FN_INT_ULONG, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_PARITYLL, "parityll", BT_FN_INT_ULONGLONG, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_POPCOUNT, "popcount", BT_FN_INT_UINT, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_popcount)
DEF_GCC_BUILTIN        (BUILT_IN_POPCOUNTIMAX, "popcountimax", BT_FN_INT_UINTMAX, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_POPCOUNTL, "popcountl", BT_FN_INT_ULONG, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_popcountl)
DEF_GCC_BUILTIN        (BUILT_IN_POPCOUNTLL, "popcountll", BT_FN_INT_ULONGLONG, ATTR_CONST_NOTHROW_LEAF_LIST, simplify_popcountll)
DEF_GCC_BUILTIN        (BUILT_IN_PREFETCH, "prefetch", BT_FN_VOID_CONST_PTR_VAR, ATTR_NOVOPS_LEAF_LIST, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_REALLOC, "realloc", BT_FN_PTR_PTR_SIZE, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_RETURN, "return", BT_FN_VOID_PTR, ATTR_NORETURN_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_RETURN_ADDRESS, "return_address", BT_FN_PTR_UINT, ATTR_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_SAVEREGS, "saveregs", BT_FN_PTR_VAR, ATTR_NULL, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_SETJMP, "setjmp", BT_FN_INT_PTR, ATTR_NULL, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRFMON, "strfmon", BT_FN_SSIZE_STRING_SIZE_CONST_STRING_VAR, ATTR_FORMAT_STRFMON_NOTHROW_3_4, NO_EXPAND_FUN)
DEF_LIB_BUILTIN        (BUILT_IN_STRFTIME, "strftime", BT_FN_SIZE_STRING_SIZE_CONST_STRING_CONST_PTR, ATTR_FORMAT_STRFTIME_NOTHROW_3_0, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_TRAP, "trap", BT_FN_VOID, ATTR_NORETURN_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_UNREACHABLE, "unreachable", BT_FN_VOID, ATTR_NORETURN_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_UNWIND_INIT, "unwind_init", BT_FN_VOID, ATTR_NULL, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_UPDATE_SETJMP_BUF, "update_setjmp_buf", BT_FN_VOID_PTR_INT, ATTR_NULL, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_VA_COPY, "va_copy", BT_FN_VOID_VALIST_REF_VALIST_ARG, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_VA_END, "va_end", BT_FN_VOID_VALIST_REF, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_VA_START, "va_start", BT_FN_VOID_VALIST_REF_VAR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_VA_ARG_PACK, "va_arg_pack", BT_FN_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GCC_BUILTIN        (BUILT_IN_VA_ARG_PACK_LEN, "va_arg_pack_len", BT_FN_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN__EXIT, "_exit", BT_FN_VOID_INT, ATTR_NORETURN_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_C99_BUILTIN        (BUILT_IN__EXIT2, "_Exit", BT_FN_VOID_INT, ATTR_NORETURN_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

/* Implementing nested functions.  */
DEF_BUILTIN_STUB (BUILT_IN_INIT_TRAMPOLINE, "__builtin_init_trampoline", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_ADJUST_TRAMPOLINE, "__builtin_adjust_trampoline", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_NONLOCAL_GOTO, "__builtin_nonlocal_goto", NO_EXPAND_FUN)

/* Implementing __builtin_setjmp.  */
DEF_BUILTIN_STUB (BUILT_IN_SETJMP_SETUP, "__builtin_setjmp_setup", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_SETJMP_DISPATCHER, "__builtin_setjmp_dispatcher", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_SETJMP_RECEIVER, "__builtin_setjmp_receiver", NO_EXPAND_FUN)

/* Implementing variable sized local variables.  */
DEF_BUILTIN_STUB (BUILT_IN_STACK_SAVE, "__builtin_stack_save", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_STACK_RESTORE, "__builtin_stack_restore", NO_EXPAND_FUN)

/* Object size checking builtins.  */
DEF_GCC_BUILTIN	       (BUILT_IN_OBJECT_SIZE, "object_size", BT_FN_SIZE_CONST_PTR_INT, ATTR_PURE_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMCPY_CHK, "__memcpy_chk", BT_FN_PTR_PTR_CONST_PTR_SIZE_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMMOVE_CHK, "__memmove_chk", BT_FN_PTR_PTR_CONST_PTR_SIZE_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMPCPY_CHK, "__mempcpy_chk", BT_FN_PTR_PTR_CONST_PTR_SIZE_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_MEMSET_CHK, "__memset_chk", BT_FN_PTR_PTR_INT_SIZE_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STPCPY_CHK, "__stpcpy_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRCAT_CHK, "__strcat_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRCPY_CHK, "__strcpy_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRNCAT_CHK, "__strncat_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_STRNCPY_CHK, "__strncpy_chk", BT_FN_STRING_STRING_CONST_STRING_SIZE_SIZE, ATTR_NOTHROW_NONNULL_LEAF, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SNPRINTF_CHK, "__snprintf_chk", BT_FN_INT_STRING_SIZE_INT_SIZE_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_NOTHROW_5_6, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_SPRINTF_CHK, "__sprintf_chk", BT_FN_INT_STRING_INT_SIZE_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_NOTHROW_4_5, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_VSNPRINTF_CHK, "__vsnprintf_chk", BT_FN_INT_STRING_SIZE_INT_SIZE_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_NOTHROW_5_0, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_VSPRINTF_CHK, "__vsprintf_chk", BT_FN_INT_STRING_INT_SIZE_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_NOTHROW_4_0, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_FPRINTF_CHK, "__fprintf_chk", BT_FN_INT_FILEPTR_INT_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_3_4, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_PRINTF_CHK, "__printf_chk", BT_FN_INT_INT_CONST_STRING_VAR, ATTR_FORMAT_PRINTF_2_3, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_VFPRINTF_CHK, "__vfprintf_chk", BT_FN_INT_FILEPTR_INT_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_3_0, NO_EXPAND_FUN)
DEF_EXT_LIB_BUILTIN    (BUILT_IN_VPRINTF_CHK, "__vprintf_chk", BT_FN_INT_INT_CONST_STRING_VALIST_ARG, ATTR_FORMAT_PRINTF_2_0, NO_EXPAND_FUN)

/* Profiling hooks.  */
DEF_BUILTIN_STUB (BUILT_IN_PROFILE_FUNC_ENTER, "profile_func_enter", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_PROFILE_FUNC_EXIT, "profile_func_exit", NO_EXPAND_FUN)

/* TLS emulation.  */
//
// Not supported in Mercurium: need to know the function for _tls and the register
//
// DEF_BUILTIN (BUILT_IN_EMUTLS_GET_ADDRESS, targetm.emutls.get_address,
// 	     BUILT_IN_NORMAL,
// 	     BT_FN_PTR_PTR,  BT_FN_PTR_PTR,
// 	     1, 1, 1, ATTR_CONST_NOTHROW_NONNULL_LEAF, 0,
// 	     !targetm.have_tls)
// DEF_BUILTIN (BUILT_IN_EMUTLS_REGISTER_COMMON,
// 	     targetm.emutls.register_common, BUILT_IN_NORMAL,
// 	     BT_FN_VOID_PTR_WORD_WORD_PTR, BT_FN_VOID_PTR_WORD_WORD_PTR,
// 	     1, 1, 1, ATTR_NOTHROW_LEAF_LIST, 0,
// 	     !targetm.have_tls)

/* Exception support.  */
DEF_BUILTIN_STUB (BUILT_IN_UNWIND_RESUME, "__builtin_unwind_resume", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_CXA_END_CLEANUP, "__builtin_cxa_end_cleanup", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_EH_POINTER, "__builtin_eh_pointer", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_EH_FILTER, "__builtin_eh_filter", NO_EXPAND_FUN)
DEF_BUILTIN_STUB (BUILT_IN_EH_COPY_VALUES, "__builtin_eh_copy_values", NO_EXPAND_FUN)

// Old __sync_XXX support
#define REGISTER_SYNC_SPECIALIZATION(generic_name, bytes, def_type, pred, ...) \
{ \
    if (pred) \
    { \
        const char* generic_name_str = UNIQUESTR_LITERAL(generic_name); \
    scope_entry_list_t* generic_list = query_name_str(global_context, generic_name_str, NULL); \
    ERROR_CONDITION(generic_list == NULL, "Generic '" generic_name "' not found", 0); \
    scope_entry_t* generic = entry_list_head(generic_list); \
    entry_list_free(generic_list); \
    type_t *types[] = { __VA_ARGS__ }; \
    int num_args = sizeof(types) / sizeof(*types); \
    nodecl_t nodecl_args[num_args]; \
    int i; \
    for (i = 0; i < num_args; i++) \
    { \
        nodecl_args[i] = nodecl_make_type(types[i], NULL); \
    } \
    const_value_t* cval = NULL; \
    scope_entry_t* specific = solve_gcc_atomic_builtins_overload_name_generic( \
            generic, types, nodecl_args, num_args, &cval, (__mcxx_builtin_type__##def_type)()); \
    ERROR_CONDITION(specific == NULL, "Invalid specific symbol for '" generic_name "'", 0); \
    for (i = 0; i < num_args; i++) \
    { \
        nodecl_free(nodecl_args[i]); \
    } \
    const char* alias_str = UNIQUESTR_LITERAL(generic_name "_" #bytes); \
    insert_alias(global_context->current_scope, specific, alias_str); \
    if (!CURRENT_CONFIGURATION->xl_compatibility) \
    { /* We use the specific name always, except under XL compatibility */ \
        specific->symbol_name = alias_str; \
    } \
    } \
}

#define REGISTER_SYNC_FETCH_AND_OP_SPECIALIZATIONS(generic_name) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_FETCH_AND_OP, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_char_type())), \
            get_signed_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_FETCH_AND_OP, 1, \
            get_pointer_type((get_unsigned_char_type())), \
            get_unsigned_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_FETCH_AND_OP, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_short_int_type())), \
            get_signed_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_FETCH_AND_OP, 1, \
            get_pointer_type((get_unsigned_short_int_type())), \
            get_unsigned_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_FETCH_AND_OP, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_int_type())), \
            get_signed_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_FETCH_AND_OP, 1, \
            get_pointer_type((get_unsigned_int_type())), \
            get_unsigned_int_type()) \
    if (type_get_size(get_signed_long_int_type()) == 4) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_FETCH_AND_OP, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_long_int_type())), \
            get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_FETCH_AND_OP, IS_CXX_LANGUAGE, \
            get_pointer_type((get_unsigned_long_int_type())), \
            get_unsigned_long_int_type()) \
    } else if (type_get_size(get_signed_long_int_type()) == 8) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_FETCH_AND_OP, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_long_int_type())), \
            get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_FETCH_AND_OP, 1, \
            get_pointer_type((get_unsigned_long_int_type())), \
            get_unsigned_long_int_type()) \
    } else internal_error("Code unreachable", 0); \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_FETCH_AND_OP, \
            IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_long_long_int_type())), \
            get_signed_long_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_FETCH_AND_OP, \
            IS_CXX_LANGUAGE || type_get_size(get_signed_long_int_type()) == 4, \
            get_pointer_type((get_unsigned_long_long_int_type())), \
            get_unsigned_long_long_int_type())

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_ADD_N, "__sync_fetch_and_add",
		  BT_FN_SYNC_FETCH_AND_OP_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_FETCH_AND_OP_SPECIALIZATIONS("__sync_fetch_and_add")

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_SUB_N, "__sync_fetch_and_sub",
		  BT_FN_SYNC_FETCH_AND_OP_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_FETCH_AND_OP_SPECIALIZATIONS("__sync_fetch_and_sub")

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_OR_N, "__sync_fetch_and_or",
		  BT_FN_SYNC_FETCH_AND_OP_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_FETCH_AND_OP_SPECIALIZATIONS("__sync_fetch_and_or")

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_AND_N, "__sync_fetch_and_and",
		  BT_FN_SYNC_FETCH_AND_OP_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_FETCH_AND_OP_SPECIALIZATIONS("__sync_fetch_and_and")

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_XOR_N, "__sync_fetch_and_xor",
		  BT_FN_SYNC_FETCH_AND_OP_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_FETCH_AND_OP_SPECIALIZATIONS("__sync_fetch_and_xor")

DEF_SYNC_BUILTIN (BUILT_IN_FETCH_AND_NAND_N, "__sync_fetch_and_nand",
		  BT_FN_SYNC_FETCH_AND_OP_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_FETCH_AND_OP_SPECIALIZATIONS("__sync_fetch_and_nand")

#define REGISTER_SYNC_OP_AND_FETCH_SPECIALIZATIONS(generic_name) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_OP_AND_FETCH, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_char_type())), \
            get_signed_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_OP_AND_FETCH, 1, \
            get_pointer_type((get_unsigned_char_type())), \
            get_unsigned_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_OP_AND_FETCH, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_short_int_type())), \
            get_signed_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_OP_AND_FETCH, 1, \
            get_pointer_type((get_unsigned_short_int_type())), \
            get_unsigned_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_OP_AND_FETCH, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_int_type())), \
            get_signed_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_OP_AND_FETCH, 1, \
            get_pointer_type((get_unsigned_int_type())), \
            get_unsigned_int_type()) \
    if (type_get_size(get_signed_long_int_type()) == 4) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_OP_AND_FETCH, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_long_int_type())), \
            get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_OP_AND_FETCH, IS_CXX_LANGUAGE, \
            get_pointer_type((get_unsigned_long_int_type())), \
            get_unsigned_long_int_type()) \
    } else if (type_get_size(get_signed_long_int_type()) == 8) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_OP_AND_FETCH, IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_long_int_type())), \
            get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_OP_AND_FETCH, 1, \
            get_pointer_type((get_unsigned_long_int_type())), \
            get_unsigned_long_int_type()) \
    } else internal_error("Code unreachable", 0); \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_OP_AND_FETCH, \
            IS_CXX_LANGUAGE, \
            get_pointer_type((get_signed_long_long_int_type())), \
            get_signed_long_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_OP_AND_FETCH, \
            IS_CXX_LANGUAGE || type_get_size(get_signed_long_int_type()) == 4, \
            get_pointer_type((get_unsigned_long_long_int_type())), \
            get_unsigned_long_long_int_type())

DEF_SYNC_BUILTIN (BUILT_IN_ADD_AND_FETCH_N, "__sync_add_and_fetch",
		  BT_FN_SYNC_OP_AND_FETCH_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_OP_AND_FETCH_SPECIALIZATIONS("__sync_add_and_fetch")

DEF_SYNC_BUILTIN (BUILT_IN_SUB_AND_FETCH_N, "__sync_sub_and_fetch",
		  BT_FN_SYNC_OP_AND_FETCH_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_OP_AND_FETCH_SPECIALIZATIONS("__sync_sub_and_fetch")

DEF_SYNC_BUILTIN (BUILT_IN_OR_AND_FETCH_N, "__sync_or_and_fetch",
		  BT_FN_SYNC_OP_AND_FETCH_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_OP_AND_FETCH_SPECIALIZATIONS("__sync_or_and_fetch")

DEF_SYNC_BUILTIN (BUILT_IN_AND_AND_FETCH_N, "__sync_and_and_fetch",
		  BT_FN_SYNC_OP_AND_FETCH_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_OP_AND_FETCH_SPECIALIZATIONS("__sync_and_and_fetch")

DEF_SYNC_BUILTIN (BUILT_IN_XOR_AND_FETCH_N, "__sync_xor_and_fetch",
		  BT_FN_SYNC_OP_AND_FETCH_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_OP_AND_FETCH_SPECIALIZATIONS("__sync_xor_and_fetch")

DEF_SYNC_BUILTIN (BUILT_IN_NAND_AND_FETCH_N, "__sync_nand_and_fetch",
		  BT_FN_SYNC_OP_AND_FETCH_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_OP_AND_FETCH_SPECIALIZATIONS("__sync_nand_and_fetch")

#define REGISTER_SYNC_COMPARE_AND_SWAP_BOOL_SPECIALIZATIONS(generic_name) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_char_type())), \
            get_signed_char_type(), get_signed_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_char_type())), \
            get_unsigned_char_type(), get_unsigned_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_short_int_type())), \
            get_signed_short_int_type(), get_signed_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_short_int_type())), \
            get_unsigned_short_int_type(), get_unsigned_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_int_type())), \
            get_signed_int_type(), get_signed_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_int_type())), \
            get_unsigned_int_type(), get_unsigned_int_type()) \
    if (type_get_size(get_signed_long_int_type()) == 4) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_int_type())), \
            get_signed_long_int_type(), get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_int_type())), \
            get_unsigned_long_int_type(), get_unsigned_long_int_type()) \
    } else if (type_get_size(get_signed_long_int_type()) == 8) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_int_type())), \
            get_signed_long_int_type(), get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_int_type())), \
            get_unsigned_long_int_type(), get_unsigned_long_int_type()) \
    } else internal_error("Code unreachable", 0); \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, \
            IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_long_int_type())), \
            get_signed_long_long_int_type(), get_signed_long_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_COMPARE_AND_SWAP_BOOL, \
            IS_CXX_LANGUAGE || type_get_size(get_signed_long_int_type()) == 4, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_long_int_type())), \
            get_unsigned_long_long_int_type(), get_unsigned_long_long_int_type())

DEF_SYNC_BUILTIN (BUILT_IN_BOOL_COMPARE_AND_SWAP_N,
		  "__sync_bool_compare_and_swap",
		  BT_FN_SYNC_COMPARE_AND_SWAP_BOOL_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_COMPARE_AND_SWAP_BOOL_SPECIALIZATIONS("__sync_bool_compare_and_swap")

#define REGISTER_SYNC_COMPARE_AND_SWAP_VALUE_SPECIALIZATIONS(generic_name) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_char_type())), \
            get_signed_char_type(), get_signed_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_char_type())), \
            get_unsigned_char_type(), get_unsigned_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_short_int_type())), \
            get_signed_short_int_type(), get_signed_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_short_int_type())), \
            get_unsigned_short_int_type(), get_unsigned_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_int_type())), \
            get_signed_int_type(), get_signed_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_int_type())), \
            get_unsigned_int_type(), get_unsigned_int_type()) \
    if (type_get_size(get_signed_long_int_type()) == 4) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_int_type())), \
            get_signed_long_int_type(), get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_int_type())), \
            get_unsigned_long_int_type(), get_unsigned_long_int_type()) \
    } else if (type_get_size(get_signed_long_int_type()) == 8) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_int_type())), \
            get_signed_long_int_type(), get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_int_type())), \
            get_unsigned_long_int_type(), get_unsigned_long_int_type()) \
    } else internal_error("Code unreachable", 0); \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, \
            IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_long_int_type())), \
            get_signed_long_long_int_type(), get_signed_long_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, \
            IS_CXX_LANGUAGE || type_get_size(get_signed_long_int_type()) == 4, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_long_int_type())), \
            get_unsigned_long_long_int_type(), get_unsigned_long_long_int_type()) \
    if (type_get_size(get_pointer_type(get_void_type())) == 4) { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_pointer_type(get_void_type()))), \
            get_pointer_type(get_void_type()), get_pointer_type(get_void_type())) \
    } else if (type_get_size(get_pointer_type(get_void_type())) == 8) { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_COMPARE_AND_SWAP_VALUE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_pointer_type(get_void_type()))), \
            get_pointer_type(get_void_type()), get_pointer_type(get_void_type())) \
    } else internal_error("Code unreachable", 0);

DEF_SYNC_BUILTIN (BUILT_IN_VAL_COMPARE_AND_SWAP_N,
		  "__sync_val_compare_and_swap",
		  BT_FN_SYNC_COMPARE_AND_SWAP_VALUE_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_COMPARE_AND_SWAP_VALUE_SPECIALIZATIONS("__sync_val_compare_and_swap")

#define REGISTER_SYNC_LOCK_TEST_AND_SET_SPECIALIZATIONS(generic_name) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_LOCK_TEST_AND_SET, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_char_type())), \
            get_signed_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_LOCK_TEST_AND_SET, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_char_type())), \
            get_unsigned_char_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_LOCK_TEST_AND_SET, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_short_int_type())), \
            get_signed_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_LOCK_TEST_AND_SET, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_short_int_type())), \
            get_unsigned_short_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_LOCK_TEST_AND_SET, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_int_type())), \
            get_signed_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_LOCK_TEST_AND_SET, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_int_type())), \
            get_unsigned_int_type()) \
    if (type_get_size(get_signed_long_int_type()) == 4) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_LOCK_TEST_AND_SET, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_int_type())), \
            get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_LOCK_TEST_AND_SET, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_int_type())), \
            get_unsigned_long_int_type()) \
    } else if (type_get_size(get_signed_long_int_type()) == 8) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_LOCK_TEST_AND_SET, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_int_type())), \
            get_signed_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_LOCK_TEST_AND_SET, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_int_type())), \
            get_unsigned_long_int_type()) \
    } else internal_error("Code unreachable", 0); \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_LOCK_TEST_AND_SET, \
            IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_long_int_type())), \
            get_signed_long_long_int_type()) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_LOCK_TEST_AND_SET, \
            IS_CXX_LANGUAGE || type_get_size(get_signed_long_int_type()) == 4, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_long_int_type())), \
            get_unsigned_long_long_int_type())

DEF_SYNC_BUILTIN (BUILT_IN_LOCK_TEST_AND_SET_N, "__sync_lock_test_and_set",
		  BT_FN_SYNC_LOCK_TEST_AND_SET_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_LOCK_TEST_AND_SET_SPECIALIZATIONS("__sync_lock_test_and_set")

#define REGISTER_SYNC_LOCK_RELEASE_SPECIALIZATIONS(generic_name) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_LOCK_RELEASE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_char_type()))) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 1, BT_FN_SYNC_LOCK_RELEASE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_char_type()))) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_LOCK_RELEASE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_short_int_type()))) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 2, BT_FN_SYNC_LOCK_RELEASE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_short_int_type()))) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_LOCK_RELEASE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_int_type()))) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_LOCK_RELEASE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_int_type()))) \
    if (type_get_size(get_signed_long_int_type()) == 4) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_LOCK_RELEASE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_int_type()))) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 4, BT_FN_SYNC_LOCK_RELEASE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_int_type()))) \
    } else if (type_get_size(get_signed_long_int_type()) == 8) \
    { \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_LOCK_RELEASE, IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_int_type()))) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_LOCK_RELEASE, 1, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_int_type()))) \
    } else internal_error("Code unreachable", 0); \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_LOCK_RELEASE, \
            IS_CXX_LANGUAGE, \
            get_pointer_type(get_volatile_qualified_type(get_signed_long_long_int_type()))) \
    REGISTER_SYNC_SPECIALIZATION(generic_name, 8, BT_FN_SYNC_LOCK_RELEASE, \
            IS_CXX_LANGUAGE || type_get_size(get_signed_long_int_type()) == 4, \
            get_pointer_type(get_volatile_qualified_type(get_unsigned_long_long_int_type())))

DEF_SYNC_BUILTIN (BUILT_IN_LOCK_RELEASE_N, "__sync_lock_release",
		  BT_FN_SYNC_LOCK_RELEASE_OVERLOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
REGISTER_SYNC_LOCK_RELEASE_SPECIALIZATIONS("__sync_lock_release")

DEF_SYNC_BUILTIN (BUILT_IN_SYNCHRONIZE, "__sync_synchronize",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

// GNU OMP
DEF_GOMP_BUILTIN (BUILT_IN_OMP_GET_THREAD_NUM, "omp_get_thread_num",
		  BT_FN_INT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_OMP_GET_NUM_THREADS, "omp_get_num_threads",
		  BT_FN_INT, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_GOMP_BUILTIN (BUILT_IN_GOMP_ATOMIC_START, "GOMP_atomic_start",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_ATOMIC_END, "GOMP_atomic_end",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_BARRIER, "GOMP_barrier",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_TASKWAIT, "GOMP_taskwait",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_CRITICAL_START, "GOMP_critical_start",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_CRITICAL_END, "GOMP_critical_end",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_CRITICAL_NAME_START,
		  "GOMP_critical_name_start",
		  BT_FN_VOID_PTRPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_CRITICAL_NAME_END,
		  "GOMP_critical_name_end",
		  BT_FN_VOID_PTRPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
/* NOTE: Do not change the order of BUILT_IN_GOMP_LOOP_*_START.  They
   are used in index arithmetic with enum omp_clause_schedule_kind
   in omp-low.c.  */
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_STATIC_START,
		  "GOMP_loop_static_start",
		  BT_FN_BOOL_LONG_LONG_LONG_LONG_LONGPTR_LONGPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_DYNAMIC_START,
		  "GOMP_loop_dynamic_start",
		  BT_FN_BOOL_LONG_LONG_LONG_LONG_LONGPTR_LONGPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_GUIDED_START,
		  "GOMP_loop_guided_start",
		  BT_FN_BOOL_LONG_LONG_LONG_LONG_LONGPTR_LONGPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_RUNTIME_START,
		  "GOMP_loop_runtime_start",
		  BT_FN_BOOL_LONG_LONG_LONG_LONGPTR_LONGPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ORDERED_STATIC_START,
		  "GOMP_loop_ordered_static_start",
		  BT_FN_BOOL_LONG_LONG_LONG_LONG_LONGPTR_LONGPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ORDERED_DYNAMIC_START,
		  "GOMP_loop_ordered_dynamic_start",
		  BT_FN_BOOL_LONG_LONG_LONG_LONG_LONGPTR_LONGPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ORDERED_GUIDED_START,
		  "GOMP_loop_ordered_guided_start",
		  BT_FN_BOOL_LONG_LONG_LONG_LONG_LONGPTR_LONGPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ORDERED_RUNTIME_START,
		  "GOMP_loop_ordered_runtime_start",
		  BT_FN_BOOL_LONG_LONG_LONG_LONGPTR_LONGPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_STATIC_NEXT, "GOMP_loop_static_next",
		  BT_FN_BOOL_LONGPTR_LONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_DYNAMIC_NEXT, "GOMP_loop_dynamic_next",
		  BT_FN_BOOL_LONGPTR_LONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_GUIDED_NEXT, "GOMP_loop_guided_next",
		  BT_FN_BOOL_LONGPTR_LONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_RUNTIME_NEXT, "GOMP_loop_runtime_next",
		  BT_FN_BOOL_LONGPTR_LONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ORDERED_STATIC_NEXT,
		  "GOMP_loop_ordered_static_next",
		  BT_FN_BOOL_LONGPTR_LONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ORDERED_DYNAMIC_NEXT,
		  "GOMP_loop_ordered_dynamic_next",
		  BT_FN_BOOL_LONGPTR_LONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ORDERED_GUIDED_NEXT,
		  "GOMP_loop_ordered_guided_next",
		  BT_FN_BOOL_LONGPTR_LONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ORDERED_RUNTIME_NEXT,
		  "GOMP_loop_ordered_runtime_next",
		  BT_FN_BOOL_LONGPTR_LONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_STATIC_START,
		  "GOMP_loop_ull_static_start",
		  BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_DYNAMIC_START,
		  "GOMP_loop_ull_dynamic_start",
		  BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_GUIDED_START,
		  "GOMP_loop_ull_guided_start",
		  BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_RUNTIME_START,
		  "GOMP_loop_ull_runtime_start",
		  BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_ORDERED_STATIC_START,
		  "GOMP_loop_ull_ordered_static_start",
		  BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_ORDERED_DYNAMIC_START,
		  "GOMP_loop_ull_ordered_dynamic_start",
		  BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_ORDERED_GUIDED_START,
		  "GOMP_loop_ull_ordered_guided_start",
		  BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_ORDERED_RUNTIME_START,
		  "GOMP_loop_ull_ordered_runtime_start",
		  BT_FN_BOOL_BOOL_ULL_ULL_ULL_ULLPTR_ULLPTR,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_STATIC_NEXT, "GOMP_loop_ull_static_next",
		  BT_FN_BOOL_ULONGLONGPTR_ULONGLONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_DYNAMIC_NEXT, "GOMP_loop_ull_dynamic_next",
		  BT_FN_BOOL_ULONGLONGPTR_ULONGLONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_GUIDED_NEXT, "GOMP_loop_ull_guided_next",
		  BT_FN_BOOL_ULONGLONGPTR_ULONGLONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_RUNTIME_NEXT, "GOMP_loop_ull_runtime_next",
		  BT_FN_BOOL_ULONGLONGPTR_ULONGLONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_ORDERED_STATIC_NEXT,
		  "GOMP_loop_ull_ordered_static_next",
		  BT_FN_BOOL_ULONGLONGPTR_ULONGLONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_ORDERED_DYNAMIC_NEXT,
		  "GOMP_loop_ull_ordered_dynamic_next",
		  BT_FN_BOOL_ULONGLONGPTR_ULONGLONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_ORDERED_GUIDED_NEXT,
		  "GOMP_loop_ull_ordered_guided_next",
		  BT_FN_BOOL_ULONGLONGPTR_ULONGLONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_ULL_ORDERED_RUNTIME_NEXT,
		  "GOMP_loop_ull_ordered_runtime_next",
		  BT_FN_BOOL_ULONGLONGPTR_ULONGLONGPTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
/* NOTE: Do not change the order of BUILT_IN_GOMP_PARALLEL_LOOP_*_START.
   They are used in index arithmetic with enum omp_clause_schedule_kind
   in omp-low.c.  */
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_PARALLEL_LOOP_STATIC_START,
		  "GOMP_parallel_loop_static_start",
		  BT_FN_VOID_OMPFN_PTR_UINT_LONG_LONG_LONG_LONG,
		  ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_PARALLEL_LOOP_DYNAMIC_START,
		  "GOMP_parallel_loop_dynamic_start",
		  BT_FN_VOID_OMPFN_PTR_UINT_LONG_LONG_LONG_LONG,
		  ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_PARALLEL_LOOP_GUIDED_START,
		  "GOMP_parallel_loop_guided_start",
		  BT_FN_VOID_OMPFN_PTR_UINT_LONG_LONG_LONG_LONG,
		  ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_PARALLEL_LOOP_RUNTIME_START,
		  "GOMP_parallel_loop_runtime_start",
		  BT_FN_VOID_OMPFN_PTR_UINT_LONG_LONG_LONG,
		  ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_END, "GOMP_loop_end",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_LOOP_END_NOWAIT, "GOMP_loop_end_nowait",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_ORDERED_START, "GOMP_ordered_start",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_ORDERED_END, "GOMP_ordered_end",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_PARALLEL_START, "GOMP_parallel_start",
		  BT_FN_VOID_OMPFN_PTR_UINT, ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_PARALLEL_END, "GOMP_parallel_end",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_TASK, "GOMP_task",
		  BT_FN_VOID_OMPFN_PTR_OMPCPYFN_LONG_LONG_BOOL_UINT,
		  ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_SECTIONS_START, "GOMP_sections_start",
		  BT_FN_UINT_UINT, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_SECTIONS_NEXT, "GOMP_sections_next",
		  BT_FN_UINT, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_PARALLEL_SECTIONS_START,
		  "GOMP_parallel_sections_start",
		  BT_FN_VOID_OMPFN_PTR_UINT_UINT, ATTR_NOTHROW_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_SECTIONS_END, "GOMP_sections_end",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_SECTIONS_END_NOWAIT,
		  "GOMP_sections_end_nowait",
		  BT_FN_VOID, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_SINGLE_START, "GOMP_single_start",
		  BT_FN_BOOL, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_SINGLE_COPY_START, "GOMP_single_copy_start",
		  BT_FN_PTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_GOMP_BUILTIN (BUILT_IN_GOMP_SINGLE_COPY_END, "GOMP_single_copy_end",
		  BT_FN_VOID_PTR, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

/* Atomic builtins for the C++ memory model.  */

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_TEST_AND_SET, "__atomic_test_and_set",
		  BT_FN_BOOL_VPTR_VOID_INT, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_CLEAR, "__atomic_clear",
        BT_FN_VOID_VPTR_BOOL_INT, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_EXCHANGE,
		  "__atomic_exchange",
		  BT_FN_ATOMIC_OVERLOAD_EXCHANGE, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_EXCHANGE_N,
		  "__atomic_exchange_n",
		  BT_FN_ATOMIC_OVERLOAD_EXCHANGE_N, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_LOAD,
		  "__atomic_load",
          BT_FN_ATOMIC_OVERLOAD_ATOMIC_LOAD, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_LOAD_N,
		  "__atomic_load_n",
		  BT_FN_ATOMIC_OVERLOAD_ATOMIC_LOAD_N, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_COMPARE_EXCHANGE,
		  "__atomic_compare_exchange",
		  BT_FN_ATOMIC_OVERLOAD_COMPARE_EXCHANGE,
		  ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_COMPARE_EXCHANGE_N,
		  "__atomic_compare_exchange_n",
		  BT_FN_ATOMIC_OVERLOAD_COMPARE_EXCHANGE_N, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_STORE,
		  "__atomic_store",
		  BT_FN_ATOMIC_OVERLOAD_ATOMIC_STORE, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_STORE_N,
		  "__atomic_store_n",
		  BT_FN_ATOMIC_OVERLOAD_ATOMIC_STORE_N, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_ADD_FETCH_N,
		  "__atomic_add_fetch",
		  BT_FN_ATOMIC_OVERLOAD_OP_FETCH, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_SUB_FETCH_N,
		  "__atomic_sub_fetch",
		  BT_FN_ATOMIC_OVERLOAD_OP_FETCH, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_AND_FETCH_N,
		  "__atomic_and_fetch",
		  BT_FN_ATOMIC_OVERLOAD_OP_FETCH, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_NAND_FETCH_N,
		  "__atomic_nand_fetch",
		  BT_FN_ATOMIC_OVERLOAD_OP_FETCH, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_XOR_FETCH_N,
		  "__atomic_xor_fetch",
		  BT_FN_ATOMIC_OVERLOAD_OP_FETCH, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_OR_FETCH_N,
		  "__atomic_or_fetch",
		  BT_FN_ATOMIC_OVERLOAD_OP_FETCH, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_FETCH_ADD_N,
		  "__atomic_fetch_add",
		  BT_FN_ATOMIC_OVERLOAD_FETCH_OP, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_FETCH_SUB_N,
		  "__atomic_fetch_sub",
		  BT_FN_ATOMIC_OVERLOAD_FETCH_OP, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_FETCH_AND_N,
		  "__atomic_fetch_and",
		  BT_FN_ATOMIC_OVERLOAD_FETCH_OP, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_FETCH_NAND_N,
		  "__atomic_fetch_nand",
		  BT_FN_ATOMIC_OVERLOAD_FETCH_OP, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_FETCH_XOR_N,
		  "__atomic_fetch_xor",
		  BT_FN_ATOMIC_OVERLOAD_FETCH_OP, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_FETCH_OR_N,
		  "__atomic_fetch_or",
		  BT_FN_ATOMIC_OVERLOAD_FETCH_OP, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_ALWAYS_LOCK_FREE,
		  "__atomic_always_lock_free",
		  BT_FN_BOOL_SIZE_CONST_VPTR, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_IS_LOCK_FREE,
		  "__atomic_is_lock_free",
		  BT_FN_BOOL_SIZE_CONST_VPTR, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)
DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_THREAD_FENCE,
		  "__atomic_thread_fence",
		  BT_FN_VOID_INT, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

DEF_SYNC_BUILTIN (BUILT_IN_ATOMIC_SIGNAL_FENCE,
		  "__atomic_signal_fence",
		  BT_FN_VOID_INT, ATTR_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)


DEF_GCC_BUILTIN        (BUILT_IN_SHUFFLE, "shuffle", BT_FN_BUILTIN_SHUFFLE, ATTR_CONST_NOTHROW_LEAF_LIST, NO_EXPAND_FUN)

}

void gcc_sign_in_builtins(const decl_context_t* global_context)
{
    gcc_sign_in_builtins_0(global_context);

    // Target specific builtins
    if (CURRENT_CONFIGURATION->type_environment->gcc_target_specific_builtins != NULL)
    {
        (CURRENT_CONFIGURATION->type_environment->gcc_target_specific_builtins)(global_context);
    }
}

static type_t* replace_generic_0_with_type(type_t* t, type_t* replacement)
{
    cv_qualifier_t cv_qualif = get_cv_qualifier(t);
    type_t* updated_type = t;

    if (is_generic_type(t))
    {
        ERROR_CONDITION(generic_type_get_num(t) != 0, "Unexpected generic type found\n", 0);
        updated_type = replacement;
    }
    else if (is_pointer_type(t))
    {
        updated_type = get_pointer_type(
                replace_generic_0_with_type(
                    pointer_type_get_pointee_type(t),
                    replacement));
    }
    else if (is_function_type(t))
    {
        type_t* result = function_type_get_return_type(t);
        result = replace_generic_0_with_type(result, replacement);

        int i, N = function_type_get_num_parameters(t);

        parameter_info_t param_info[N];
        memset(param_info, 0, sizeof(param_info));

        for (i = 0; i < N; i++)
        {
            param_info[i].type_info = 
                replace_generic_0_with_type(
                        function_type_get_parameter_type_num(t, i),
                        replacement);
        }

        updated_type = get_new_function_type(result, param_info, N, REF_QUALIFIER_NONE);
    }
    else
    {
        // Do not replace anything
    }

    updated_type =  get_cv_qualified_type(updated_type, cv_qualif);

    return updated_type;
}

static scope_entry_t* solve_gcc_atomic_builtins_overload_name_generic(
        scope_entry_t* overloaded_function,
        type_t** types,
        nodecl_t *arguments UNUSED_PARAMETER,
        int num_arguments,
        const_value_t** const_value UNUSED_PARAMETER,
        type_t* function_type)
{
    type_t* integer_types[] =
    {
        get_char_type(),
        get_signed_char_type(),
        get_unsigned_char_type(),
        get_signed_short_int_type(),
        get_unsigned_short_int_type(),
        get_signed_int_type(),
        get_unsigned_int_type(),
        get_signed_long_int_type(),
        get_unsigned_long_int_type(),
        get_signed_long_long_int_type(),
        get_unsigned_long_long_int_type(),
        get_pointer_type(get_void_type()),
        NULL
    };

    int i;
    for (i = 0; integer_types[i] != NULL; i++)
    {
        type_t* current_function_type = replace_generic_0_with_type(function_type, integer_types[i]);

        if (num_arguments != function_type_get_num_parameters(current_function_type))
        {
            // Ignore this case
            continue;
        }

        int j;
        char all_arguments_matched = 1;
        for (j = 0; (j < num_arguments) && all_arguments_matched; j++)
        {
            type_t* argument_type = types[j];
            type_t* parameter_type = function_type_get_parameter_type_num(current_function_type, j);

            if (is_pointer_type(no_ref(argument_type))
                    && !is_void_type(pointer_type_get_pointee_type(no_ref(argument_type)))
                    && is_pointer_type(parameter_type)
                    && !is_void_type(pointer_type_get_pointee_type(parameter_type)))
            {
                // Use sizes instead of types
                argument_type = pointer_type_get_pointee_type(no_ref(argument_type));
                parameter_type = pointer_type_get_pointee_type(parameter_type);

                all_arguments_matched = ((is_integral_type(argument_type)
                            && is_integral_type(parameter_type))
                        || (is_pointer_type(argument_type)
                            && is_void_type(pointer_type_get_pointee_type(argument_type))
                            && is_pointer_type(parameter_type)
                            && is_void_type(pointer_type_get_pointee_type(parameter_type))))
                    && equivalent_types(get_unqualified_type(argument_type),
                            get_unqualified_type(parameter_type));

            }
            else
            {
                // We do not have locus
                const locus_t* locus = make_locus("", 0, 0);
                // Allow conversions here
                standard_conversion_t scs;
                all_arguments_matched = standard_conversion_between_types(&scs,
                        argument_type,
                        parameter_type,
                        locus);
            }
        }

        if (all_arguments_matched)
        {
            // Check if already created
            // Note that we do not use the name of the builtin because we do not want
            // plain lookups to find them
            const char* builtin_name = strappend(".", overloaded_function->symbol_name);
            scope_entry_list_t* entry_list = query_name_str(overloaded_function->decl_context,
                    builtin_name, NULL);

            parameter_info_t parameter_info[num_arguments + 1];
            for (j = 0; j < num_arguments; j++)
            {
                parameter_info[j].is_ellipsis = 0;
                parameter_info[j].type_info = get_unqualified_type(no_ref(types[j]));
                parameter_info[j].nonadjusted_type_info = NULL;
            }

            type_t* deduced_function_type = get_new_function_type(
                    function_type_get_return_type(current_function_type),
                    parameter_info,
                    num_arguments,
                    function_type_get_ref_qualifier(current_function_type));

            scope_entry_t* matching_entry = NULL;
            scope_entry_list_iterator_t* it;
            for (it = entry_list_iterator_begin(entry_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* existing_entry = entry_list_iterator_current(it);

                if (equivalent_types(existing_entry->type_information, deduced_function_type))
                {
                    matching_entry = existing_entry;
                    break;
                }
            }

            entry_list_iterator_free(it);
            entry_list_free(entry_list);

            if (matching_entry != NULL)
                return matching_entry;

            // Craft a symbol here
            scope_entry_t* return_symbol = new_symbol(overloaded_function->decl_context, 
                    overloaded_function->decl_context->current_scope,
                    builtin_name);
            return_symbol->locus = overloaded_function->locus;
            return_symbol->symbol_name = overloaded_function->symbol_name;
            return_symbol->kind = SK_FUNCTION;


            return_symbol->type_information = deduced_function_type;

            return_symbol->do_not_print = 1;
            symbol_entity_specs_set_is_builtin(return_symbol, 1);

            return return_symbol;
        }
    }

    return NULL;
}

static void sign_in_gcc_simd_builtins(const decl_context_t* decl_context)
{
    // Intel architecture gcc builtins
    const locus_t* builtins_locus = make_locus("(gcc-builtin-ia32)", 0, 0);
#include "cxx-gccbuiltins-ia32.h"
}

static void sign_in_simd_builtins(const decl_context_t* decl_context)
{
    sign_in_gcc_simd_builtins(decl_context);
    
    sign_in_intel_simd_types(decl_context);
}

extern void gcc_builtins_i386(const decl_context_t* global_context)
{
    sign_in_simd_builtins(global_context);

    if (CURRENT_CONFIGURATION->enable_intel_intrinsics)
    {
        warn_printf_at(NULL, "Intel intrinsics are not supported for i386 yet\n");
    }
}

extern void gcc_builtins_x86_64(const decl_context_t* global_context)
{
    sign_in_simd_builtins(global_context);

    if (CURRENT_CONFIGURATION->enable_intel_intrinsics)
    {
        sign_in_icc_intrinsics(global_context);
    }
}

#ifndef HAVE_INT128
// Kind of a fallback
static type_t* get_unsigned_16x8_int(void)
{
    return get_vector_type_by_elements(get_unsigned_char_type(), 16);
}
#endif

static void gcc_builtins_neon(const decl_context_t* decl_context)
{

#define GENERATE_NEON_VECTOR_BUILTINS \
    GENERATE_NEON_VECTOR(128, __simd128_float32_t,    get_float_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_int16_t,      get_signed_short_int_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_int32_t,      get_signed_int_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_int64_t,      get_signed_long_long_int_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_int8_t,       get_signed_char_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_uint16_t,     get_unsigned_short_int_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_uint32_t,     get_unsigned_int_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_uint64_t,     get_unsigned_long_long_int_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_uint8_t,      get_unsigned_char_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_float16_t,     get_float16_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_float32_t,     get_float_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_int16_t,       get_signed_short_int_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_int32_t,       get_signed_int_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_int8_t,        get_signed_char_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_uint16_t,      get_unsigned_short_int_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_uint32_t,      get_unsigned_int_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_uint8_t,       get_unsigned_char_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_poly8_t,       get_signed_char_type()) \
    GENERATE_NEON_VECTOR(64,  __simd64_poly16_t,      get_signed_short_int_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_poly8_t,      get_signed_char_type()) \
    GENERATE_NEON_VECTOR(128, __simd128_poly16_t,     get_signed_short_int_type()) \
    GENERATE_NEON_VECTOR(64,  __builtin_neon_poly8,   get_signed_char_type()) \
    GENERATE_NEON_VECTOR(64,  __builtin_neon_poly16,  get_signed_short_int_type()) \
    GENERATE_NEON_VECTOR(128, __builtin_neon_poly64,  get_unsigned_long_long_int_type()) \
    GENERATE_NEON_VECTOR(128, __builtin_neon_poly128, get_unsigned_char_type())

#define GENERATE_NEON_VECTOR(bits, elem_typename, elem_type) \
    { \
        scope_entry_t* sym = new_symbol(decl_context, decl_context->current_scope, UNIQUESTR_LITERAL(#elem_typename )); \
        sym->locus = make_locus("(builtin-simd-type)", 0, 0); \
        sym->kind = SK_TYPEDEF; \
        sym->type_information = get_vector_type_by_elements(elem_type, bits / (type_get_size(elem_type) * 8)); \
        sym->defined = 1; \
        sym->do_not_print = 1; \
        symbol_entity_specs_set_is_user_declared(sym, 1); \
    }
    GENERATE_NEON_VECTOR_BUILTINS
#undef GENERATE_NEON_VECTOR
#undef GENERATE_NEON_VECTOR_BUILTINS
    // Aliases
    int i, N;
    struct
    {
        const char* name;
        type_t* (*fun)(void);
    } aliased_names[] =
    {
        { "__builtin_neon_qi", get_signed_char_type },
        { "__builtin_neon_uqi", get_unsigned_char_type },

        { "__builtin_neon_hi", get_signed_short_int_type },
        { "__builtin_neon_uhi", get_unsigned_short_int_type },

        { "__builtin_neon_si", get_signed_int_type },
        { "__builtin_neon_usi", get_unsigned_int_type },

        { "__builtin_neon_di", get_signed_long_long_int_type },
        { "__builtin_neon_udi", get_unsigned_long_int_type },

        { "__builtin_neon_sf", get_float_type },
        { "__builtin_neon_df", get_double_type },
        { "__builtin_neon_poly8",   get_signed_char_type },
        { "__builtin_neon_poly16",  get_signed_short_int_type },
        { "__builtin_neon_poly64",  get_unsigned_long_int_type },
#ifdef HAVE_INT128
        { "__builtin_neon_poly128", get_unsigned_int128_type },
#else
        { "__builtin_neon_poly128", get_unsigned_16x8_int },
#endif
    };
    for (i = 0, N = STATIC_ARRAY_LENGTH(aliased_names); i < N; i++)
    {
        scope_entry_t* sym = new_symbol(decl_context, decl_context->current_scope, uniquestr(aliased_names[i].name));
        sym->locus = make_locus("(builtin-simd-type)", 0, 0);
        sym->kind = SK_TYPEDEF;
        sym->type_information = (aliased_names[i].fun)();
        sym->defined = 1;
        sym->do_not_print = 1;
        symbol_entity_specs_set_is_builtin(sym, 1);
    }

    // Special ones
    struct
    {
        const char* name;
        int num_elements;
    } opaque_neon_builtin_types[] =
    {
        {"ti", 2},
        {"ei", 3},
        {"oi", 4},
        {"ci", 6},
        {"xi", 8},
    };
    for (i = 0, N = STATIC_ARRAY_LENGTH(opaque_neon_builtin_types); i < N; i++)
    {
        const char* c = NULL;
        uniquestr_sprintf(&c, "__builtin_neon_%s", opaque_neon_builtin_types[i].name);
        scope_entry_t* sym = new_symbol(decl_context, decl_context->current_scope, c);
        sym->locus = make_locus("(builtin-simd-type)", 0, 0);
        sym->kind = SK_TYPEDEF;
        sym->type_information =
            get_vector_type_by_elements(
                    get_signed_long_long_int_type(),
                    opaque_neon_builtin_types[i].num_elements);
        sym->defined = 1;
        sym->do_not_print = 1; \
        symbol_entity_specs_set_is_builtin(sym, 1);
    }

    const locus_t* builtins_locus = make_locus("(gcc-builtin-arm)", 0, 0);
#include "cxx-gccbuiltins-arm-neon.h"
}

extern void gcc_builtins_arm(const decl_context_t* global_context)
{
    gcc_builtins_neon(global_context);
}

static void gcc_builtins_neon_arm64(const decl_context_t* decl_context)
{
#define GENERATE_NEON_VECTOR_BUILTINS \
    GENERATE_NEON_VECTOR(64,  __Int8x8_t,     get_signed_char_type()) \
    GENERATE_NEON_VECTOR(64,  __Int16x4_t,    get_signed_short_int_type()) \
    GENERATE_NEON_VECTOR(64,  __Int32x2_t,    get_signed_int_type()) \
    GENERATE_NEON_VECTOR(64,  __Int64x1_t,    get_signed_long_int_type()) \
    GENERATE_NEON_VECTOR(64,  __Float32x2_t,  get_float_type()) \
    GENERATE_NEON_VECTOR(64,  __Poly8x8_t,    get_signed_char_type()) \
    GENERATE_NEON_VECTOR(64,  __Poly16x4_t,   get_signed_short_int_type()) \
    GENERATE_NEON_VECTOR(64,  __Uint8x8_t,    get_unsigned_char_type()) \
    GENERATE_NEON_VECTOR(64,  __Uint16x4_t,   get_unsigned_short_int_type()) \
    GENERATE_NEON_VECTOR(64,  __Uint32x2_t,   get_unsigned_int_type()) \
    GENERATE_NEON_VECTOR(64,  __Float64x1_t,  get_double_type()) \
    GENERATE_NEON_VECTOR(64,  __Uint64x1_t,   get_unsigned_long_int_type()) \
    GENERATE_NEON_VECTOR(128, __Int8x16_t,    get_signed_char_type()) \
    GENERATE_NEON_VECTOR(128, __Int16x8_t,    get_signed_short_int_type()) \
    GENERATE_NEON_VECTOR(128, __Int32x4_t,    get_signed_int_type()) \
    GENERATE_NEON_VECTOR(128, __Int64x2_t,    get_signed_long_int_type()) \
    GENERATE_NEON_VECTOR(128, __Float32x4_t,  get_float_type()) \
    GENERATE_NEON_VECTOR(128, __Float64x2_t,  get_double_type()) \
    GENERATE_NEON_VECTOR(128, __Poly8x16_t,   get_signed_char_type()) \
    GENERATE_NEON_VECTOR(128, __Poly16x8_t,   get_signed_short_int_type()) \
    GENERATE_NEON_VECTOR(128, __Poly64x2_t,   get_unsigned_long_int_type()) \
    GENERATE_NEON_VECTOR(128, __Uint8x16_t,   get_unsigned_char_type()) \
    GENERATE_NEON_VECTOR(128, __Uint16x8_t,   get_unsigned_short_int_type()) \
    GENERATE_NEON_VECTOR(128, __Uint32x4_t,   get_unsigned_int_type()) \
    GENERATE_NEON_VECTOR(128, __Uint64x2_t,   get_unsigned_long_int_type()) \

#define GENERATE_NEON_VECTOR(bits, elem_typename, elem_type) \
    { \
        scope_entry_t* sym = new_symbol(decl_context, decl_context->current_scope, UNIQUESTR_LITERAL(#elem_typename)); \
        sym->locus = make_locus("(builtin-simd-type)", 0, 0); \
        sym->kind = SK_TYPEDEF; \
        sym->type_information = get_vector_type_by_elements(elem_type, bits / (type_get_size(elem_type) * 8)); \
        sym->defined = 1; \
        sym->do_not_print = 1; \
        symbol_entity_specs_set_is_user_declared(sym, 1); \
    }
    GENERATE_NEON_VECTOR_BUILTINS
#undef GENERATE_NEON_VECTOR

    int i, N;

    // Aliases
    struct
    {
        const char* name;
        type_t* (*fun)(void);
    } aliased_names[] =
    {
        {"__Poly8_t",    get_signed_char_type   },
        {"__Poly16_t",   get_signed_short_int_type   },
        {"__Poly64_t",   get_unsigned_long_int_type },
#ifdef HAVE_INT128
        {"__Poly128_t",  get_unsigned_int128_type },
#else
        {"__Poly128_t",  get_unsigned_16x8_int      },
#endif
        {"__builtin_aarch64_simd_qi", get_signed_char_type       },
        {"__builtin_aarch64_simd_hi", get_signed_short_int_type  },
        {"__builtin_aarch64_simd_si", get_signed_int_type        },
        {"__builtin_aarch64_simd_di", get_signed_long_int_type   },
        {"__builtin_aarch64_simd_sf", get_float_type             },
        {"__builtin_aarch64_simd_df", get_double_type            },
    };
    for (i = 0, N = STATIC_ARRAY_LENGTH(aliased_names); i < N; i++)
    {
        scope_entry_t* sym = new_symbol(decl_context, decl_context->current_scope, uniquestr(aliased_names[i].name));
        sym->locus = make_locus("(builtin-simd-type)", 0, 0);
        sym->kind = SK_TYPEDEF;
        sym->type_information = (aliased_names[i].fun)();
        sym->defined = 1;
        sym->do_not_print = 1;
        symbol_entity_specs_set_is_builtin(sym, 1);
    }

    // Special ones
    struct
    {
        const char* name;
        int num_elements;
    } opaque_neon_builtin_types[] =
    {
        {"ti", 2},
        {"ei", 3},
        {"oi", 4},
        {"ci", 6},
        {"xi", 8},
    };
    for (i = 0, N = STATIC_ARRAY_LENGTH(opaque_neon_builtin_types); i < N; i++)
    {
        const char* c = NULL;
        uniquestr_sprintf(&c, "__builtin_aarch64_simd_%s", opaque_neon_builtin_types[i].name);
        scope_entry_t* sym = new_symbol(decl_context, decl_context->current_scope, c);
        sym->locus = make_locus("(builtin-simd-type)", 0, 0);
        sym->kind = SK_TYPEDEF;
        sym->type_information =
            get_vector_type_by_elements(
                    get_signed_long_int_type(),
                    opaque_neon_builtin_types[i].num_elements);
        sym->defined = 1;
        sym->do_not_print = 1; \
        symbol_entity_specs_set_is_builtin(sym, 1);
    }

    const locus_t* builtins_locus = make_locus("(gcc-builtin-aarch64)", 0, 0);
#include "cxx-gccbuiltins-arm64-neon.h"
}

extern void gcc_builtins_arm64(const decl_context_t* global_context)
{
    gcc_builtins_neon_arm64(global_context);
}

void prepend_intel_vector_typedefs(nodecl_t* nodecl_output)
{
    ERROR_CONDITION(!IS_CXX_LANGUAGE, "This function is only for C++", 0);

    scope_entry_t* (*fun_list[])(void) = {
        get_m64_typedef,

        get_m128_typedef,
        get_m128d_typedef,
        get_m128i_typedef,

        get_m256_typedef,
        get_m256d_typedef,
        get_m256i_typedef,

        get_m512_typedef,
        get_m512d_typedef,
        get_m512i_typedef,
        NULL
    };

    nodecl_t nodecl_vector_defs = nodecl_null();

    int i;
    for (i = 0; fun_list[i] != NULL; i++)
    {
        scope_entry_t* sym = (fun_list[i])();

        type_t* struct_type = advance_over_typedefs(sym->type_information);

        if (is_complete_type(struct_type))
        {
            nodecl_vector_defs =
                nodecl_append_to_list(
                        nodecl_vector_defs,
                        nodecl_make_cxx_def(
                            nodecl_null(),
                            sym,
                            NULL));
        }
    }

    *nodecl_output = nodecl_concat_lists(
            nodecl_vector_defs,
            *nodecl_output);
}
