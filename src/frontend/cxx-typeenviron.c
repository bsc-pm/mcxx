#include <stdlib.h>
#include <string.h>
#include "cxx-typeenviron.h"
#include "cxx-typeutils.h"
#include "cxx-cexpr.h"
#include "cxx-utils.h"

static long long unsigned int _bytes_due_to_type_environment = 0;

long long unsigned int type_environment_used_memory(void)
{
    return _bytes_due_to_type_environment;
}

/* Utility functions */
static void next_offset_with_align(_size_t* current_offset, _size_t required_align)
{
    // FIXME - This is a bit stupid, it can be implemented better with
    // arithmetic, but I felt lazy when I wrote that
    while ((*current_offset) % required_align != 0)
        (*current_offset)++;
}

static int round_to_upper_byte(_size_t bit_offset)
{
    while (bit_offset % 8 == 0)
        bit_offset++;

    return bit_offset / 8;
}

/*
   Generic environment definitions
   System V mainly

   C
 */

static void system_v_array_sizeof(type_t* t)
{
    type_t* element_type = array_type_get_element_type(t);
    AST expr = array_type_get_array_size_expr(t);
    decl_context_t decl_context = array_type_get_array_size_expr_context(t);

    _size_t element_size = type_get_size(element_type);
    _size_t element_align = type_get_alignment(element_type);

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

static void system_v_union_sizeof(type_t* t UNUSED_PARAMETER)
{
}

static void system_v_struct_sizeof(type_t* t)
{
    // offset is used only for non-bitfields and when bitfields
    // cause an effective advance of the offset, otherwise bitfields only
    // use current_bit_within_storage
    _size_t offset = 0;
    _size_t whole_align = 1;

    // bitfields do not use offset except as needed
    _size_t current_bit_within_storage = 0;

    char previous_was_bitfield = 0;

    int i;
    int num_fields = class_type_get_num_nonstatic_data_members(t);

    for (i = 0; i < num_fields; i++)
    {
        scope_entry_t* field = class_type_get_nonstatic_data_member_num(t, i);
        type_t* field_type = field->type_information;

        _size_t field_size = type_get_size(field_type);
        _size_t field_align = type_get_alignment(field_type);

        if (field->entity_specs.is_bitfield)
        {
            // Named bitfields

            // Bitfields are very special, otherwise all this stuff would be
            // extremely easy
            unsigned int bitsize = 0;
            {
                literal_value_t literal = evaluate_constant_expression(
                        field->entity_specs.bitfield_expr,
                        field->entity_specs.bitfield_expr_context);

                char valid = 0;
                bitsize = literal_value_to_uint(literal, &valid);
                if (!valid)
                {
                    internal_error("Invalid bitfield expression", 0);
                }
            }

            if (bitsize == 0)
            {
                // FIXME - Frontend should check that unnamed bitfields are the only
                // ones that can have 0 width
                if (!previous_was_bitfield)
                {
                    // If the previous was not a bitfield then we have
                    // to adjust the current bit within storage just after
                    // the previous field
                    current_bit_within_storage = (offset % field_align) * 8;
                }

                // Just fill all the remaining bits
                current_bit_within_storage += (field_size * 8 - current_bit_within_storage);
            }
            else
            {
                if (!previous_was_bitfield)
                {
                    // If the previous was not a bitfield then we have
                    // to adjust the current bit within storage just after
                    // the previous field
                    current_bit_within_storage = (offset % field_align) * 8;
                }

                // In System V bytes are 8 bits :)
                ERROR_CONDITION(bitsize > (field_size * 8),
                        "This bitsize (%d) is greater than the related field type (%d)", 
                        bitsize, (field_size * 8));

                // Not enough remaining bits in this storage unit, just advance as usual.
                if ((field_size * 8 - current_bit_within_storage) <
                        bitsize)
                {
                    // Example (assume shorts are 16 bit wide)
                    // struct A 
                    // {
                    //    char c;
                    //    short :9;
                    // };
                    //
                    // We would start laying out the bitfield :9 just after
                    // 'c', but this would mean starting at bit 8 of the
                    // current storage and 8 + 9 does not fit in
                    // field_size * 8. No overlapping applies in
                    // System V, so advance to the next storage unit suitable
                    // for the bitfield type ('short' in this example)

                    next_offset_with_align(&offset, field_align);

                    current_bit_within_storage = 0;
                }

                // Now move within the current storage
                current_bit_within_storage += bitsize;

                if (!field->entity_specs.is_unnamed_bitfield)
                {
                    // Named bitfields DO contribute to the align of the whole struct
                    // Update the whole align, this is needed for the tail padding
                    if (whole_align < field_align)
                        whole_align = field_align;
                }
            }

            // This is a bitfield
            previous_was_bitfield = 1;
        }
        else
        {
            if (previous_was_bitfield)
            {
                // Offset is in bytes, but for bitfields we have bits, and we have 
                // to round to the next byte
                offset += round_to_upper_byte(current_bit_within_storage);
            }

            // Update the whole align, this is needed for the tail padding
            if (whole_align < field_align)
                whole_align = field_align;

            // Advance the offset for the current field alignment This computes
            // the "internal padding"
            next_offset_with_align(&offset, field_align);

            // Store the offset for this field
            field->entity_specs.field_offset = offset;

            // Now update the offset, we have to advance at least field_size
            offset += field_size;

            // This is not a bitfield
            previous_was_bitfield = 0;
        }
    }

    // Round the offset to upper byte if the last field was a bitfield
    if (previous_was_bitfield)
    {
        offset += round_to_upper_byte(current_bit_within_storage);
    }
     
    // Compute tail padding, just ensure that the next laid out entity
    // would satisfy the alignment 
    next_offset_with_align(&offset, whole_align);

    // If it remains 1, it means that the struct was empty
    // Make it like an int
    if (offset == 0)
    {
        offset = CURRENT_CONFIGURATION(type_environment)->sizeof_signed_int;
        whole_align = CURRENT_CONFIGURATION(type_environment)->alignof_signed_int;
    }

    type_set_size(t, offset);
    type_set_alignment(t, whole_align);
    type_set_valid_size(t, 1);
}

static void system_v_generic_sizeof(type_t* t)
{
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
    else
    {
        internal_error("Unhandled type size '%s'", print_declarator(t));
    }
}

/* 
   Itanium ABI for C++ (the one followed by most vendors of C++)
 */
static void cxx_abi_array_sizeof(type_t* t)
{
    type_t* element_type = array_type_get_element_type(t);
    AST expr = array_type_get_array_size_expr(t);
    decl_context_t decl_context = array_type_get_array_size_expr_context(t);

    _size_t element_size = type_get_size(element_type);
    _size_t element_align = type_get_alignment(element_type);

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

static void cxx_abi_union_sizeof(type_t* t)
{
    // FIXME - Unions with pointer to members *may* fail miserably
    system_v_union_sizeof(t);
}

#define MAX_BASES 32

typedef
struct base_info_preorder_tag
{
    scope_entry_t* entry;
    char is_virtual;
} base_info_preorder_t;

static void class_type_preorder_all_bases_rec(type_t* t,
        base_info_preorder_t base_info[MAX_BASES], int *num_elems)
{
    type_t* class_type = get_actual_class_type(t);
    ERROR_CONDITION((*num_elems) == MAX_BASES, 
            "Too many bases (%d)\n", (*num_elems));

    int num_bases = class_type_get_num_bases(class_type);
    int i;

    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        scope_entry_t* current_base = 
            class_type_get_base_num(class_type, i, &is_virtual);

        char is_found = 0;
        int j;
        for (j = 0; (j < (*num_elems)) && !is_found; j++)
        {
            if (base_info[j].entry == current_base
                    && base_info[j].is_virtual 
                    && is_virtual)
                is_found = 1;
        }

        if (!is_found)
        {
            base_info[(*num_elems)].entry = current_base;
            base_info[(*num_elems)].is_virtual = is_virtual;
            (*num_elems)++;

            class_type_preorder_all_bases_rec(current_base->type_information,
                    base_info, num_elems);
        }
    }
}

static void class_type_preorder_all_bases(type_t* t,
        base_info_preorder_t base_info[MAX_BASES],
        int *num_elems)
{
    *num_elems = 0;
    class_type_preorder_all_bases_rec(t, base_info, num_elems);
}

// Returns null if no primary base class. is_virtual is only valid if non null
// was returned
static scope_entry_t* cxx_abi_class_type_get_primary_base_class(type_t* t, char *is_virtual)
{
    type_t* class_type = get_actual_class_type(t);
    ERROR_CONDITION(!is_named_class_type(t),
            "Invalid class type", 0);

    base_info_preorder_t base_info[MAX_BASES];
    int num_elems = 0;

    class_type_preorder_all_bases_rec(class_type, base_info, &num_elems);

    // 1. Identify all _virtual_ base classes, direct or indirect, that are
    // primary base classes for some other direct or indirect base class
    int i;

    int num_primaries = 0;
    base_info_preorder_t primary_bases[MAX_BASES];

    for (i = 0; i < num_elems; i++)
    {
        char current_base_is_virtual = 0;
        // Recursion
        scope_entry_t* entry = cxx_abi_class_type_get_primary_base_class(
                base_info[i].entry->type_information, 
                &current_base_is_virtual);

        if (entry != NULL
                && current_base_is_virtual)
        {
            char is_found = 0;
            int j;
            for (j = 0; (j < num_primaries) && !is_found; j++)
            {
                if (primary_bases[j].entry == entry
                        && primary_bases[j].is_virtual
                        /* && current_base_is_virtual */)
                    is_found = 1;
            }

            if (!is_found)
            {
                ERROR_CONDITION(num_primaries == MAX_BASES,
                        "Too many primaries (%d)!", num_primaries);
                primary_bases[num_primaries].entry = entry;
                // This is always 1 since we are only interested in virtual ones
                primary_bases[num_primaries].is_virtual = 1;
                num_primaries++;
            }
        }
    }

    // 2. If C has a dynamic base class, attempt to choose a primary base classB.
    // a) It is the first (in direct base class order) non-virtual dynamic base class
    int num_direct_bases = class_type_get_num_bases(class_type);
    for (i = 0; i < num_direct_bases; i++)
    {
        char current_base_is_virtual = 0;
        scope_entry_t* current_direct_base = class_type_get_base_num(class_type, i, 
                &current_base_is_virtual);

        if (!current_base_is_virtual
                && class_type_is_dynamic(current_direct_base->type_information))
        {
            *is_virtual = 0;
            return current_direct_base;
        }
    }

    // b) Otherwise it is a nearly empty virtual base class, the first one in (preorder)
    // inheritance graph which is not an indirect primary base class if any exist or 
    // just the first one if they are all indirect primaries
    *is_virtual = 1;

    scope_entry_t* first_ne_virtual_base = NULL;

    for (i = 0; i < num_elems; i++)
    {
        if (base_info[i].is_virtual
                && class_type_is_nearly_empty(base_info[i].entry->type_information))
        {
            if (first_ne_virtual_base == NULL)
            {
                first_ne_virtual_base = base_info[i].entry;
            }

            char is_found = 0;
            int j;
            for (j = 0; (j < num_primaries) && !is_found; j++)
            {
                if (primary_bases[j].entry == base_info[i].entry)
                    is_found = 1;
            }

            if (!is_found)
            {
                return base_info[i].entry;
            }
        }
    }

    // Return the first one. It might be null if neither a) nor b) returned anything
    return first_ne_virtual_base;
}

static void cxx_abi_class_sizeof(type_t* t)
{
    if (is_pod_type_layout(t))
    {
        system_v_struct_sizeof(t);
        return;
    }

    type_t* class_type = get_actual_class_type(t);

    // Initialization: sizeof to 0, align to 1, dsize to 0
    type_set_size(t, 0);
    type_set_alignment(t, 1);
    type_set_data_size(t, 0);

    if (class_type_is_dynamic(class_type))
    {
        char primary_base_class_is_virtual = 0;
        scope_entry_t* primary_base_class = 
            cxx_abi_class_type_get_primary_base_class(class_type, 
                    &primary_base_class_is_virtual);

        if (primary_base_class == NULL)
        {
            // c) If C has no primary base class, allocate the virtual table pointer
            // for C at offset zero and set sizeof(C), align(C) and dsize(C) to the
            // appropriate values for a pointer

            type_set_size(t, CURRENT_CONFIGURATION(type_environment)->sizeof_pointer);
            type_set_alignment(t, CURRENT_CONFIGURATION(type_environment)->alignof_pointer);
            type_set_data_size(t, CURRENT_CONFIGURATION(type_environment)->sizeof_pointer);
        }

        // For each data component D (first the primary base of C, if any, then
        // the non-primary non-virtual direct base classes in declaration
        // orderd, then the non-static data members and unnamed bitfields in
        // declaration order
    }
}

static void cxx_abi_generic_sizeof(type_t* t)
{
    // FIXME - Named types like in the case below are not well handled
    // typedef __attribute__((aligned(16))) int T;
    if (is_array_type(t))
    {
        cxx_abi_array_sizeof(t);
    }
    else if (is_union_type(t))
    {
        cxx_abi_union_sizeof(t);
    }
    else if (is_class_type(t))
    {
        cxx_abi_class_sizeof(t);
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        cxx_abi_generic_sizeof(reference_type_get_referenced_type(t));
    }
    else
    {
        internal_error("Unhandled type size '%s'", print_declarator(t));
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

    // One 'ptrdiff_t'
    .sizeof_pointer_to_data_member = 4,
    .alignof_pointer_to_data_member = 4,

    .sizeof_function_pointer = 4,
    .alignof_function_pointer = 4,

    // Two 'ptrdiff_t'
    .sizeof_pointer_to_member_function = 8,
    .alignof_pointer_to_member_function = 4,

    .compute_sizeof = NULL,

    .type_of_sizeof = get_unsigned_int_type,

    // In IA32 'char' == 'signed char'
    .char_type = get_signed_char_type
};

type_environment_t* type_environment_linux_ia32 = &type_environment_linux_ia32_;
