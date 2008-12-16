#include <stdlib.h>
#include "cxx-typeenviron.h"
#include "cxx-typeutils.h"
#include "cxx-cexpr.h"
#include "cxx-utils.h"

/*
   Generic environment definitions
   System V mainly
 */

void system_v_array_sizeof(type_t* t)
{
    type_t* element_type = array_type_get_element_type(t);
    AST expr = array_type_get_array_size_expr(t);
    decl_context_t decl_context = array_type_get_array_size_expr_context(t);

    _size_t element_size = type_get_size(t);
    _size_t element_align = type_get_alignment(t);

    if (is_constant_expression(expr, decl_context))
    {
        literal_value_t l = evaluate_constant_expression(expr, decl_context);
        char valid = 0;

        int size = literal_value_to_uint(l, &valid);

        if (valid)
        {
            type_set_size(t, size * element_size);
            type_set_alignment(t, element_align);
            type_set_valid_size(t, 1);
            return;
        }
    }
    internal_error("Cannot compute the size of the array type '%s'!", print_declarator(t));
}

void system_v_union_sizeof(type_t* t UNUSED_PARAMETER)
{
}

void system_v_struct_sizeof(type_t* t UNUSED_PARAMETER)
{
}

void system_v_generic_sizeof(type_t* t)
{
    CXX_LANGUAGE()
    {
        internal_error("Not yet implemented sizeof for C++", 0);
    }

    // FIXME - Named types like in the case below are not well handled
    // typedef __attribute__((aligned(16))) int T;
    if (is_array_type(t))
    {
        system_v_array_sizeof(t);
    }
    else if (is_union_type(t))
    {
        system_v_union_sizeof(t);
    }
    else if (is_class_type(t))
    {
        system_v_struct_sizeof(t);
    }
}

/*
   Specific architecture environment definitions and routines
 */
// Linux IA-32
// Nothing is aligned more than 4 here
static type_environment_t type_environment_linux_ia32_ = 
{
    // '_Bool' in C99
    // 'bool' in C++
    .sizeof_bool = 1,
    .alignof_bool = 1,

    // ?? Check this one with itanium abi
    .sizeof_wchar_t = 2,
    .alignof_wchar_t = 2,

    .sizeof_unsigned_short = 2,
    .alignof_unsigned_short = 2,

    .sizeof_signed_short = 2,
    .alignof_signed_short = 2,

    .sizeof_unsigned_int = 4,
    .alignof_unsigned_int = 4,

    .sizeof_signed_int = 4,
    .alignof_signed_int = 4,

    .sizeof_unsigned_long = 4,
    .alignof_unsigned_long = 4,

    .sizeof_signed_long = 4,
    .alignof_signed_long = 4,

    .sizeof_unsigned_long_long = 8,
    .alignof_unsigned_long_long = 4,

    .sizeof_signed_long_long = 8,
    .alignof_signed_long_long = 4, 

    .sizeof_float = 4,
    .alignof_float = 4,

    .sizeof_double = 8,
    .alignof_double = 4,

    .sizeof_long_double = 12,
    .alignof_long_double = 4,

    .sizeof_pointer = 4,
    .alignof_pointer = 4,

    // ?? Check this one with itanium abi
    .sizeof_pointer_to_data_member = 4,
    .alignof_pointer_to_data_member = 4,

    .sizeof_function_pointer = 4,
    .alignof_function_pointer = 4,

    // ?? Check this one with itanium abi
    .sizeof_pointer_to_member_function = 4,
    .alignof_pointer_to_member_function = 4,

    .compute_sizeof = NULL,

    .type_of_sizeof = get_unsigned_int_type,

    // In IA32 'char' == 'signed char'
    .char_type = get_signed_char_type
};

type_environment_t* type_environment_linux_ia32 = &type_environment_linux_ia32_;
