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



#include <stdlib.h>
#include <string.h>
#include "cxx-typeenviron.h"
#include "cxx-typeutils.h"
#include "cxx-exprtype.h"
#include "cxx-cexpr.h"
#include "cxx-utils.h"
#include "cxx-entrylist.h"

#ifdef MAX
  #warning MAX already defined here! Overriding
#endif

#undef MAX
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))

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
    while (bit_offset % 8 != 0)
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

    _size_t element_size = type_get_size(element_type);
    _size_t element_align = type_get_alignment(element_type);

    if (expr != NULL
            && expression_is_constant(expr))
    {
        int size = const_value_cast_to_4(expression_get_constant(expr));

        type_set_size(t, size * element_size);
        type_set_alignment(t, element_align);
        type_set_valid_size(t, 1);
    }
    else if (expr == NULL)
    {
        internal_error("The compiler tried to evaluate the size of an unbounded array", 0);
    }
}

static void system_v_field_layout(scope_entry_t* field,
        _size_t *offset,
        _size_t *whole_align,
        _size_t *current_bit_within_storage,
        char *previous_was_bitfield,
        char is_last_field)
{
    type_t* field_type = field->type_information;

    _size_t field_size = 0;

    // gcc flexible arrays support
    if (is_array_type(field_type)
            && array_type_get_array_size_expr(field_type) == NULL)
    {
        if (!is_last_field)
        {
            running_error("Invalid unbounded array found when computing type of struct\n", 0);
        }
        else
        {
            // Perform a special computation for this array
            type_t* element_type = array_type_get_element_type(field_type);
            _size_t element_align = type_get_alignment(element_type);

            type_set_size(field_type, 0);
            type_set_alignment(field_type, element_align);
            type_set_valid_size(field_type, 1);
        }
    }
    else
    {
        field_size = type_get_size(field_type);
    }
    _size_t field_align = type_get_alignment(field_type);

    if (field->entity_specs.is_bitfield)
    {
        _size_t initial_bit = 0;
        _size_t filled_bits = 0;
        // Named bitfields

        // Bitfields are very special, otherwise all this stuff would be
        // extremely easy
        unsigned int bitsize = const_value_cast_to_4(
                expression_get_constant(field->entity_specs.bitfield_expr)
                );

        if (!(*previous_was_bitfield))
        {
            // If the previous was not a bitfield then we have
            // to adjust the current bit within storage just after
            // the previous field
            initial_bit = ((*offset) % field_align) * 8;
        }
        else
        {
            // Fix the offset since we are one byte ahed
            if ((*offset) > 0)
            {
               (*offset) = (*offset) - 1; 
            }

            // Use the previous bit
            initial_bit = (*current_bit_within_storage);
        }

        if (bitsize == 0)
        {
            // FIXME - Frontend should check that unnamed bitfields are the only
            // ones that can have 0 width

            // Just fill all the remaining bits
            filled_bits = field_size * 8 - initial_bit;
        }
        else
        {
            // In System V bytes are 8 bits :)
            ERROR_CONDITION(bitsize > (field_size * 8),
                    "This bitsize (%d) is greater than the related field type (%d)", 
                    bitsize, (field_size * 8));

            // Not enough remaining bits in this storage unit, just advance as usual.
            if ((field_size * 8 - initial_bit) <
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

                // Update the offset here
                next_offset_with_align(&(*offset), field_align);

                initial_bit = 0;
            }

            // Now move within the current storage
            filled_bits = bitsize;

            if (!field->entity_specs.is_unnamed_bitfield)
            {
                // Named bitfields DO contribute to the align of the whole struct
                // Update the whole align, this is needed for the tail padding
                if ((*whole_align) < field_align)
                    (*whole_align) = field_align;
            }
        }

        // Now update current bit
        (*current_bit_within_storage) = initial_bit + filled_bits;

        // Advance always the offset (we will go backwards for consecutive bitfields)
        (*offset) += round_to_upper_byte(filled_bits);

        // This is a bitfield
        (*previous_was_bitfield) = 1;
    }
    else
    {
        // Update the whole align, this is needed for the tail padding
        if ((*whole_align) < field_align)
            (*whole_align) = field_align;

        // Advance the (*offset) for the current field alignment This computes
        // the "internal padding"
        next_offset_with_align(&(*offset), field_align);

        // Store the (*offset) for this field
        field->entity_specs.field_offset = (*offset);

        // Now update the (*offset), we have to advance at least field_size
        (*offset) += field_size;

        // This is not a bitfield
        (*previous_was_bitfield) = 0;
    }
}

static void system_v_union_sizeof(type_t* class_type)
{
    // offset is used only for non-bitfields and when bitfields
    // cause an effective advance of the offset, otherwise bitfields only
    // use current_bit_within_storage
    _size_t offset, max_offset = 0;
    _size_t whole_align = 1;

    // bitfields do not use offset except as needed
    _size_t current_bit_within_storage = 0;

    char previous_was_bitfield = 0;

    int i;
    int num_fields = class_type_get_num_nonstatic_data_members(class_type);

    for (i = 0; i < num_fields; i++)
    {
        // For a union its fields are always in offset 0
        offset = 0;
        current_bit_within_storage = 0;
        previous_was_bitfield = 0;
        
        scope_entry_t* field = class_type_get_nonstatic_data_member_num(class_type, i);

        char is_last_field = (i == (num_fields - 1));

        system_v_field_layout(
                field,
                &offset,
                &whole_align,
                &current_bit_within_storage,
                &previous_was_bitfield,
                is_last_field);

        max_offset = MAX(max_offset, offset);
    }

    // Compute tail padding, just ensure that the next laid out entity
    // will satisfy the alignment 
    next_offset_with_align(&max_offset, whole_align);

    // If it remains 0, it means that the struct was empty
    // Make it like an int
    if (max_offset == 0)
    {
        max_offset = CURRENT_CONFIGURATION->type_environment->sizeof_signed_int;
        whole_align = CURRENT_CONFIGURATION->type_environment->alignof_signed_int;
    }

    type_set_size(class_type, max_offset);
    type_set_alignment(class_type, whole_align);
    type_set_valid_size(class_type, 1);
}

static void system_v_struct_sizeof(type_t* class_type)
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
    int num_fields = class_type_get_num_nonstatic_data_members(class_type);

    for (i = 0; i < num_fields; i++)
    {
        scope_entry_t* field = class_type_get_nonstatic_data_member_num(class_type, i);

        char is_last_field = (i == (num_fields - 1));

        system_v_field_layout(
                field,
                &offset,
                &whole_align,
                &current_bit_within_storage,
                &previous_was_bitfield,
                is_last_field);
    }

    // Compute tail padding, just ensure that the next laid out entity
    // would satisfy the alignment 
    next_offset_with_align(&offset, whole_align);

    // If it remains 0, it means that the struct was empty
    // In C++ classes can be empty but have sizeof 1
    // In C, structs cannot be empty, so leave them as being sizeof(X) == 0 
    CXX_LANGUAGE()
    {
        if (offset == 0)
        {
            offset = 1;
            whole_align = 1;
        }
    }

    type_set_size(class_type, offset);
    type_set_alignment(class_type, whole_align);
    type_set_valid_size(class_type, 1);
}

static void c_or_cxx_enum_sizeof(type_t* t)
{
    type_t* underlying = enum_type_get_underlying_type(t);

    type_set_size(t, type_get_size(underlying));
    CXX_LANGUAGE()
    {
        type_set_data_size(t, type_get_size(underlying));
    }
    type_set_alignment(t, type_get_alignment(underlying));
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
    // It must be unnamed
    else if (is_union_type(t))
    {
        system_v_union_sizeof(t);
    }
    else if (is_unnamed_class_type(t))
    {
        system_v_struct_sizeof(t);
    }
    else if (is_enum_type(t))
    {
        c_or_cxx_enum_sizeof(t);
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

    _size_t element_size = type_get_size(element_type);
    _size_t element_align = type_get_alignment(element_type);

    if (expr != NULL
            && expression_is_constant(expr))
    {
        int size = const_value_cast_to_4(expression_get_constant(expr));

        type_set_size(t, size * element_size);
        type_set_alignment(t, element_align);
        type_set_valid_size(t, 1);

        return;
    }
    internal_error("Cannot compute the size of the array type '%s'!", print_declarator(t));
}

static void cxx_abi_union_sizeof(type_t* t)
{
    system_v_union_sizeof(t);
}

typedef
struct base_info_preorder_tag
{
    scope_entry_t* entry;
    char is_virtual;
} base_info_preorder_t;

static void class_type_preorder_all_bases_rec(type_t* t,
        base_info_preorder_t base_info[MCXX_MAX_CLASS_BASES], int *num_elems)
{
    type_t* class_type = get_actual_class_type(t);
    ERROR_CONDITION((*num_elems) == MCXX_MAX_CLASS_BASES, 
            "Too many bases (%d)\n", (*num_elems));

    int num_bases = class_type_get_num_bases(class_type);
    int i;

    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        scope_entry_t* current_base = 
            class_type_get_base_num(class_type, i, &is_virtual, &is_dependent, /* access_specifier */ NULL);

        if (is_dependent)
            continue;

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
        base_info_preorder_t base_info[MCXX_MAX_CLASS_BASES],
        int *num_elems)
{
    *num_elems = 0;
    class_type_preorder_all_bases_rec(t, base_info, num_elems);
}

static scope_entry_t* cxx_abi_class_type_get_primary_base_class(type_t* t, char *is_virtual);

static void class_type_get_indirect_virtual_primary_bases(
        base_info_preorder_t bases[MCXX_MAX_CLASS_BASES],
        int num_bases, 
        base_info_preorder_t indirect_primary_bases[MCXX_MAX_CLASS_BASES],
        int *num_primaries)
{
    (*num_primaries) = 0;

    int i;
    for (i = 0; i < num_bases; i++)
    {
        char current_base_is_virtual = 0;
        // Recursion
        scope_entry_t* entry = cxx_abi_class_type_get_primary_base_class(
                bases[i].entry->type_information, 
                &current_base_is_virtual);

        if (entry != NULL
                && current_base_is_virtual)
        {
            char is_found = 0;
            int j;
            for (j = 0; (j < (*num_primaries)) && !is_found; j++)
            {
                if (indirect_primary_bases[j].entry == entry
                        && indirect_primary_bases[j].is_virtual
                        /* && current_base_is_virtual */)
                    is_found = 1;
            }

            if (!is_found)
            {
                ERROR_CONDITION((*num_primaries) == MCXX_MAX_CLASS_BASES,
                        "Too many primaries (%d)!", (*num_primaries));
                indirect_primary_bases[(*num_primaries)].entry = entry;
                // This is always 1 since we are only interested in virtual ones
                indirect_primary_bases[(*num_primaries)].is_virtual = 1;
                (*num_primaries)++;
            }
        }
    }

}

// Returns null if no primary base class. is_virtual is only valid if non null
// was returned
static scope_entry_t* cxx_abi_class_type_get_primary_base_class(type_t* t, char *is_virtual)
{
    type_t* class_type = get_actual_class_type(t);

    int num_bases = 0;
    base_info_preorder_t base_info[MCXX_MAX_CLASS_BASES];

    class_type_preorder_all_bases(class_type, base_info, &num_bases);

    // 1. Identify all _virtual_ base classes, direct or indirect, that are
    // primary base classes for some other direct or indirect base class

    int num_primaries = 0;
    base_info_preorder_t indirect_primary_bases[MCXX_MAX_CLASS_BASES];

    class_type_get_indirect_virtual_primary_bases(base_info, num_bases,
            // indirect primary bases will be stored in 'indirect_primary_bases'
            indirect_primary_bases, &num_primaries);

    // 2. If C has a dynamic base class, attempt to choose a primary base classB.
    // a) It is the first (in direct base class order) non-virtual dynamic base class
    int i;
    int num_direct_bases = class_type_get_num_bases(class_type);
    for (i = 0; i < num_direct_bases; i++)
    {
        char current_base_is_virtual = 0;
        char current_base_is_dependent = 0;
        scope_entry_t* current_direct_base = class_type_get_base_num(class_type, i, 
                &current_base_is_virtual, &current_base_is_dependent, /* access_specifier */ NULL);

        if (current_base_is_dependent)
            continue;

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

    for (i = 0; i < num_bases; i++)
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
                if (indirect_primary_bases[j].entry == base_info[i].entry)
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

typedef
struct offset_info_tag
{
    _size_t offset;
    scope_entry_list_t* subobjects;
    struct offset_info_tag* next;
} offset_info_t;

typedef
struct layout_info_tag
{
    _size_t dsize;
    _size_t size;
    _size_t align;
    _size_t nvalign;
    _size_t nvsize;
    offset_info_t* offsets;

    char previous_was_bitfield;
    _size_t bit_within_bitfield;

    char previous_was_base;
    scope_entry_t* previous_base;
} layout_info_t;

static void cxx_abi_register_entity_offset(layout_info_t* layout_info,
        scope_entry_t* entry,
        _size_t offset)
{
    offset_info_t* previous_offset = NULL;
    offset_info_t* current_offset = layout_info->offsets;

    while (current_offset != NULL)
    {
        if (current_offset->offset == offset)
        {
            scope_entry_list_t* new_entry_list = entry_list_new(entry);

            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(current_offset->subobjects);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                new_entry_list = entry_list_add(new_entry_list, 
                        entry_list_iterator_current(it));
            }
            entry_list_iterator_free(it);

            current_offset->subobjects = new_entry_list;

            // Do nothing else
            return;
        }
        else if (current_offset->offset > offset)
        {
            // This offset does not exist!, we have to add it
            break;
        }

        previous_offset = current_offset;
        current_offset = current_offset->next;
    }

    // Cases: previous_offset == NULL means we are at the beginning
    // current_offset == NULL means we are the largest offset

    offset_info_t* new_offset_info = counted_calloc(1, 
            sizeof(*new_offset_info), &_bytes_due_to_type_environment);
    new_offset_info->offset = offset;

    scope_entry_list_t* new_entry_list = entry_list_new(entry);

    new_offset_info->subobjects = new_entry_list;
    new_offset_info->next = current_offset;

    if (previous_offset == NULL)
    {
        layout_info->offsets = new_offset_info;
    }
    else
    {
        previous_offset->next = new_offset_info;
    }
}

static void cxx_abi_print_layout(layout_info_t* layout_info)
{
    fprintf(stderr, "--- Layout of class ---\n");

    offset_info_t* current_offset = layout_info->offsets;
    while (current_offset != NULL)
    {
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(current_offset->subobjects);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            fprintf(stderr, "@%04zu:", current_offset->offset);
            fprintf(stderr, " %s\n", get_qualified_symbol_name(entry, 
                        entry->decl_context));
        }
        entry_list_iterator_free(it);

        current_offset = current_offset->next;
    }

    fprintf(stderr, "--- End of layout ---\n");
}

static void cxx_abi_register_subobject_offset(layout_info_t* layout_info,
        scope_entry_t* member,
        _size_t chosen_offset)
{
    // If the member is a class it means that it is a base
    if (member->kind == SK_CLASS)
    {
        type_t* class_type = get_actual_class_type(member->type_information);

        int num_bases = class_type_get_num_bases(class_type);
        int i;

        for (i = 0; i < num_bases; i++)
        {
            char is_virtual;
            char is_dependent;
            scope_entry_t* base_class = class_type_get_base_num(class_type, i, 
                    &is_virtual, &is_dependent, /* access_specifier */ NULL);

            if (is_dependent)
                continue;

            _size_t base_offset = 0;
            // Do not register virtual bases here, only nonvirtual bases
            if (!is_virtual)
            {
                base_offset = class_type_get_offset_direct_base(class_type, base_class);
                cxx_abi_register_subobject_offset(layout_info, 
                        base_class,
                        chosen_offset + base_offset);
            }
        }

        // Nonstatic data members
        int num_nonstatic_data_members 
            = class_type_get_num_nonstatic_data_members(class_type);

        for (i = 0; i < num_nonstatic_data_members; i++)
        {
            scope_entry_t* nonstatic_data_member 
                = class_type_get_nonstatic_data_member_num(class_type, i);

            // Bitfields do not have offset!
            if (!nonstatic_data_member->entity_specs.is_bitfield)
            {
                _size_t field_offset = nonstatic_data_member->entity_specs.field_offset;

                // Recurse
                cxx_abi_register_subobject_offset(layout_info,
                        nonstatic_data_member,
                        chosen_offset + field_offset);
            }
        }
    }

    cxx_abi_register_entity_offset(layout_info, member, chosen_offset);
}

static char cxx_abi_conflicting_member(layout_info_t* layout_info,
        scope_entry_t* member,
        _size_t candidate_offset)
{
    offset_info_t* current_offset = layout_info->offsets;

    DEBUG_SIZEOF_CODE()
    {
        fprintf(stderr, "Checking if offset %zu is valid for '%s'\n",
                candidate_offset, member->symbol_name);
    }

    while (current_offset != NULL)
    {
        if (current_offset->offset == candidate_offset)
        {
            // Check all elements linked to this offset (there can be more than
            // one if these are empty bases)

            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(current_offset->subobjects);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* subobject_entry = entry_list_iterator_current(it);

                if (equivalent_types(subobject_entry->type_information,
                            member->type_information))
                {
                    DEBUG_SIZEOF_CODE()
                    {
                        fprintf(stderr, "Conflict detected at offset %zu with '%s'!\n",
                                current_offset->offset,
                                subobject_entry->symbol_name);
                    }
                    return 1;
                }
            }
            entry_list_iterator_free(it);
        }
        current_offset = current_offset->next;
    }

    if (is_class_type(member->type_information))
    {
        // Check nonvirtual bases first

        type_t* class_type = get_actual_class_type(member->type_information);

        int num_bases = class_type_get_num_bases(class_type);
        int i;
        
        for (i = 0; i < num_bases; i++)
        {
            char is_virtual = 0;
            char is_dependent = 0;
            scope_entry_t* base = class_type_get_base_num(class_type, i, 
                    &is_virtual, &is_dependent, /* access_specifier */ NULL);

            if (is_dependent)
                continue;

            // Only non virtual classes are relevant here
            if (!is_virtual)
            {
                _size_t base_offset = class_type_get_offset_direct_base(class_type, base);

                if (cxx_abi_conflicting_member(layout_info, base, candidate_offset + base_offset))
                    return 1;
            }
        }

        // Check virtual bases
        int virtual_bases = class_type_get_num_virtual_bases_with_offset(class_type);

        for (i = 0; i < virtual_bases; i++)
        {
            scope_entry_t* base = NULL;
            _size_t base_offset = 0;
            class_type_get_virtual_base_with_offset_num(class_type, i, &base, &base_offset);

            if (cxx_abi_conflicting_member(layout_info, base, candidate_offset + base_offset))
                return 1;
        }
    }

    return 0;
}

static type_t* get_largest_integral_with_bits(unsigned int bits)
{
    // Try in order: unsigned long long, signed long long, unsigned long,
    // signed long, unsigned int, int, unsigned short, short, unsigned char, char

    type_t* integral_types[] = {
        get_unsigned_long_long_int_type(),
        get_signed_long_long_int_type(),
        get_unsigned_long_int_type(),
        get_signed_long_int_type(),
        get_unsigned_int_type(),
        get_signed_int_type(),
        get_unsigned_short_int_type(),
        get_signed_short_int_type(),
        get_unsigned_char_type(),
        get_signed_char_type(),
    };

    unsigned int i;
    unsigned int max_elems = STATIC_ARRAY_LENGTH(integral_types);
    for (i = 0; i < max_elems; i++)
    {
        if ((type_get_size(integral_types[i]) * 8) <= bits)
                return integral_types[i];
    }

    // if we reach here what? :)
    return get_char_type();
}

static void cxx_abi_lay_bitfield(type_t* t UNUSED_PARAMETER, 
        scope_entry_t* member, 
        layout_info_t* layout_info)
{
    unsigned int bitsize 
        = const_value_cast_to_4(expression_get_constant(member->entity_specs.bitfield_expr));

    _size_t size_of_member = type_get_size(member->type_information);
    _size_t initial_bit = 0;
    _size_t filled_bits = 0;

    // At least one byte is always used
    _size_t used_bytes = 0;

    // Starting offset, we may move it if it is not suitable
    // for the bitfield being laid out
    _size_t offset = layout_info->dsize;

    if (size_of_member * 8 >= bitsize)
    {
        _size_t member_align = type_get_alignment(member->type_information);

        if (layout_info->previous_was_base)
        {
            // Allocate like the underlying ABI says constrained that a bitfield
            // is never placed in the tail padding of a base class of C
            next_offset_with_align(&offset, 
                   type_get_alignment(layout_info->previous_base->type_information));
        }

        if (layout_info->previous_was_bitfield)
        {
            // Next available bits in this offset
            // if dsize(C) > 0 and dsize(C) - 1 is partially filled by a
            // bitfield and that bitfield is also a data member declared in C, 
            // next available bits are the unfilled bits in dsize(C) - 1,
            if (offset > 0)
            {
                // Fix the offset
                offset = offset - 1;
            }

            initial_bit = layout_info->bit_within_bitfield;
        }
        else
        {
            // otherwise bits available start at the offset
            initial_bit = (offset % member_align) * 8;
        }

        if (bitsize == 0)
        {
            // Just fill all the remaining bits
            filled_bits = size_of_member * 8 - initial_bit;
        }
        else
        {
            if ((size_of_member * 8 - initial_bit) <
                    bitsize)
            {
                // No overlapping in SysV
                next_offset_with_align(&offset, member_align);
                initial_bit = 0;
            }

            filled_bits = bitsize;
        }

        // Update align if not unnamed
        if (!member->entity_specs.is_unnamed_bitfield)
            layout_info->align = MAX(layout_info->align, member_align);
    }
    else
    {
        // GCC gives a warning for these cases 
        type_t* align_integral_type = get_largest_integral_with_bits(bitsize);
        _size_t integral_type_align = type_get_alignment(align_integral_type);

        next_offset_with_align(&offset, integral_type_align);

        // Advance the required amount of bytes
        used_bytes += bitsize / 8;
        // And we save the bit in the last (partially) filled byte
        initial_bit = 0;
        filled_bits = bitsize % 8;

        // Update align if not unnamed
        if (!member->entity_specs.is_unnamed_bitfield)
            layout_info->align = MAX(layout_info->align, integral_type_align);
    }

    // Note that filled_bits can be zero only for bitfields like
    //
    //    T c : (sizeof(T) * k)
    //
    // where k > 1
    used_bytes += round_to_upper_byte(filled_bits);

    layout_info->dsize = offset + used_bytes;
    layout_info->size = MAX(layout_info->size, layout_info->dsize);

    layout_info->previous_was_bitfield = 1;
    layout_info->bit_within_bitfield = initial_bit + filled_bits;
}

static void cxx_abi_lay_member_out(type_t* t, 
        scope_entry_t* member, 
        layout_info_t* layout_info,
        char is_base_class,
        char is_virtual_base_class)
{
    if (member->entity_specs.is_bitfield)
    {
        cxx_abi_lay_bitfield(t, member, layout_info);
    }
    else
    {
        if (is_base_class
                && class_type_is_empty(member->type_information))
        {
            // Start attempting to put this class at offset zero
            _size_t offset = 0;
            /*
               Surprisingly, even an empty class can have a sizeof different than 1

               Example

               struct A { };
               struct B : A { };
               struct C : A { };
               struct D : B, C { };

               D is empty but its sizeof(D) is 2 since there are two A's that must be allocated
               in different offsets:

               D          <-- 0
               D::B       <-- 0
               D::B::A    <-- 0
               D::C       <-- 1
               D::C::A    <-- 1 (this causes the conflict)
             */

            _size_t size_of_base = type_get_size(member->type_information);
            _size_t non_virtual_align = class_type_get_non_virtual_align(member->type_information);

            if (cxx_abi_conflicting_member(layout_info, member, offset))
            {
                // If unsuccessful due to a component type conflict, proceed
                // with attempts at dsize(C)
                offset = layout_info->dsize;

                // Increment by nvalign(D)
                while (cxx_abi_conflicting_member(layout_info, member, offset))
                {
                    offset += non_virtual_align;
                }
            }

            if (!is_virtual_base_class)
            {
                class_type_set_offset_direct_base(t, member, offset);
            }
            else
            {
                class_type_set_offset_virtual_base(t, member, offset);
            }

            cxx_abi_register_subobject_offset(layout_info, member, offset);

            // Once offset(D) has been chosen, update sizeof(C) to max(sizeof(C), offset(D) + sizeof(D))
            layout_info->size = MAX(layout_info->size, offset + size_of_base);

            DEBUG_SIZEOF_CODE()
            {
                fprintf(stderr, "Laying out base class '%s' at offset %zu\n",
                        get_qualified_symbol_name(member, member->decl_context), 
                        offset);
            }
        }
        else // Plain data member or non empty base class
        {
            // Start at offset dsize(C) incremented for alignment to nvalign(D)
            // or align(D) for data members
            _size_t offset = layout_info->dsize;
            // Compute this this to ensure that this member has its sizeof already computed
            _size_t sizeof_member = type_get_size(member->type_information);
            if (is_base_class)
            {
                next_offset_with_align(&offset, 
                        class_type_get_non_virtual_align(member->type_information));
            }
            else
            {
                next_offset_with_align(&offset, 
                        type_get_alignment(member->type_information));
            }

            while (cxx_abi_conflicting_member(layout_info, member, offset))
            {
                if (is_base_class)
                {
                    offset += class_type_get_non_virtual_align(member->type_information);
                }
                else
                {
                    offset += type_get_alignment(member->type_information);
                }
            }

            if (is_base_class)
            {
                // If D is a base class update sizeof(C) to max (sizeof(C), offset(D) + nvsize(D))
                _size_t non_virtual_size = class_type_get_non_virtual_size(member->type_information);
                _size_t non_virtual_align = class_type_get_non_virtual_align(member->type_information);
                layout_info->size = MAX(layout_info->size, offset + non_virtual_size);

                if (!is_virtual_base_class)
                {
                    class_type_set_offset_direct_base(t, member, offset);
                }
                else
                {
                    class_type_set_offset_virtual_base(t, member, offset);
                }
                cxx_abi_register_subobject_offset(layout_info, member, offset);

                // Update dsize(C) to offset(D) + nvsize(D) and align(C) to max(align(C), nvalign(D))
                layout_info->dsize = offset + non_virtual_size;
                layout_info->align = MAX(layout_info->align, non_virtual_align);

                DEBUG_SIZEOF_CODE()
                {
                    fprintf(stderr, "Laying out base class '%s' at offset %zu\n",
                            get_qualified_symbol_name(member, member->decl_context),
                            offset);
                }
            }
            else
            {
                member->entity_specs.field_offset = offset;
                cxx_abi_register_subobject_offset(layout_info, member, offset);

                // Otherwise, if D is a data member, update sizeof(C) to max(sizeof(C), offset(D) + sizeof(D))
                layout_info->size = MAX(layout_info->size, offset + sizeof_member);

                _size_t alignment = type_get_alignment(member->type_information);

                // Update dsize(C) to offset(D) + sizeof(D), align(C) to max(align(C), align(D))
                layout_info->dsize = offset + sizeof_member;
                layout_info->align = MAX(layout_info->align, alignment);

                DEBUG_SIZEOF_CODE()
                {
                    fprintf(stderr, "Laying out member '%s' at offset %zu\n",
                            get_qualified_symbol_name(member, member->decl_context),
                            offset);
                }
            }
        }

        // This is not a bitfield
        layout_info->previous_was_bitfield = 0;
    }

    // This is needed for bitfields
    layout_info->previous_was_base = is_base_class;
    layout_info->previous_base = is_base_class ? member : NULL;
}

static char is_pod_type_layout(type_t* t)
{
    /*
       POD for the purpose of layout

       In general, a type is considered a POD for the purposes of layout if it is
       a POD type (in the sense of ISO C++ [basic.types]). However, a POD-struct
       or POD-union (in the sense of ISO C++ [class]) with a bitfield member whose
       declared width is wider than the declared type of the bitfield is not a POD
       for the purpose of layout. Similarly, an array type is not a POD for the
       purpose of layout if the element type of the array is not a POD for the
       purpose of layout. 

     */
    if (!is_class_type(t)
            && !is_array_type(t))
    {
        return is_pod_type(t);
    }
    else if (is_array_type(t))
    {
        return is_pod_type_layout(array_type_get_element_type(t));
    }
    else if (is_class_type(t))
    {
        if (!is_pod_type(t))
            return 0;

        type_t* class_type = get_actual_class_type(t);
        int i;
        for (i = 0; i < class_type_get_num_nonstatic_data_members(class_type); i++)
        {
            scope_entry_t* data_member = class_type_get_nonstatic_data_member_num(class_type, i);

            if (data_member->entity_specs.is_bitfield)
            {
                _size_t bits_of_bitfield = 
                    const_value_cast_to_8(
                            expression_get_constant(data_member->entity_specs.bitfield_expr)
                            );

                _size_t bits_of_base_type = type_get_size(data_member->type_information) * 8;

                if (bits_of_bitfield > bits_of_base_type)
                    return 0;
            }
        }

        return 1;
    }

    internal_error("Unhandled type", 0);
}

static void cxx_abi_class_sizeof(type_t* class_type)
{
    // Initialization: sizeof to 0, align to 1, dsize to 0
    layout_info_t layout_info;
    memset(&layout_info, 0, sizeof(layout_info));

    layout_info.size = 0;
    layout_info.align = 1;
    layout_info.dsize = 0;

    // For each data component D (first the primary base of C, if any, then
    // the non-primary non-virtual direct base classes in declaration
    // orderd, then the non-static data members and unnamed bitfields in
    // declaration order

    scope_entry_t* primary_base_class = NULL;

    if (class_type_is_dynamic(class_type))
    {
        char primary_base_class_is_virtual = 0;
        primary_base_class = 
            cxx_abi_class_type_get_primary_base_class(class_type, 
                    &primary_base_class_is_virtual);


        if (primary_base_class == NULL)
        {
            DEBUG_SIZEOF_CODE()
            {
                fprintf(stderr, "No primary base class\n");
            }
            // c) If C has no primary base class, allocate the virtual table pointer
            // for C at offset zero and set sizeof(C), align(C) and dsize(C) to the
            // appropriate values for a pointer
            layout_info.size = CURRENT_CONFIGURATION->type_environment->sizeof_pointer;
            layout_info.align = CURRENT_CONFIGURATION->type_environment->alignof_pointer;
            layout_info.dsize = CURRENT_CONFIGURATION->type_environment->sizeof_pointer;

        }
        else
        {
            // First primary base class (if any)
            DEBUG_SIZEOF_CODE()
            {
                fprintf(stderr, "Laying out primary base class '%s'\n",
                        primary_base_class->symbol_name);
            }
            cxx_abi_lay_member_out(class_type,
                    primary_base_class,
                    &layout_info, 
                    /* is_base_class */ 1,
                    /* is_virtual_base_class */ primary_base_class_is_virtual);
            DEBUG_SIZEOF_CODE()
            {
                fprintf(stderr, "Finished laying out primary base class '%s'\n",
                        primary_base_class->symbol_name);
            }
        }
    }

    // Non primary non virtual direct bases
    {
        int num_bases = class_type_get_num_bases(class_type);
        int i;

        for (i = 0; i < num_bases; i++)
        {
            char is_virtual;
            char is_dependent;
            scope_entry_t* base_class = class_type_get_base_num(class_type, i, 
                    &is_virtual, &is_dependent, /* access_specifier */ NULL);

            if (!is_virtual
                    && primary_base_class != base_class)
            {
                cxx_abi_lay_member_out(class_type,
                        base_class,
                        &layout_info, 
                        /* is_base_class */ 1,
                        /* is_virtual_base_class */ 0);
            }
        }
    }

    // Non static data members and unnamed bitfields
    {
        // Nonstatic data members
        int i;
        int num_nonstatic_data_members 
            = class_type_get_num_nonstatic_data_members(class_type);

        for (i = 0; i < num_nonstatic_data_members; i++)
        {
            scope_entry_t* nonstatic_data_member 
                = class_type_get_nonstatic_data_member_num(class_type, i);

            cxx_abi_lay_member_out(class_type,
                    nonstatic_data_member,
                    &layout_info, 
                    /* is_base_class */ 0,
                    /* is_virtual_base_class */ 0);
        }
    }

    // After all such components have been allocated set nvalign(C) = align(C)
    layout_info.nvalign = layout_info.align;
    layout_info.nvsize = layout_info.size;


    {
        // Virtual bases allocation
        int num_bases = 0;
        base_info_preorder_t base_info[MCXX_MAX_CLASS_BASES];

        class_type_preorder_all_bases(class_type, base_info, &num_bases);

        int i;

        DEBUG_SIZEOF_CODE()
        {
            fprintf(stderr, "Bases: ");
            for (i = 0; i < num_bases; i++)
            {
                fprintf(stderr, "%s, ", base_info[i].entry->symbol_name);
            }
            fprintf(stderr, "\n");
        }

        // Get all the indirect primary bases since we won'class_type allocate these here
        int num_primaries = 0;
        base_info_preorder_t indirect_primary_bases[MCXX_MAX_CLASS_BASES];

        class_type_get_indirect_virtual_primary_bases(base_info, num_bases,
                // indirect primary bases will be stored in 'indirect_primary_bases'
                indirect_primary_bases, &num_primaries);

        for (i = 0; i < num_bases; i++)
        {
            if (base_info[i].is_virtual)
            {
                char found = 0;

                // If it is the primary base class
                if (base_info[i].entry == primary_base_class)
                    found = 1;

                // or an indirect base class, do not lay it out again
                int j;
                for (j = 0; (j < num_primaries) && !found; j++)
                {
                    if (base_info[i].entry == indirect_primary_bases[i].entry)
                        found = 1;
                }

                // It is not an indirect primary base, allocate it
                if (!found)
                {
                    DEBUG_SIZEOF_CODE()
                    {
                        fprintf(stderr, "Laying out virtual base '%s'\n", base_info[i].entry->symbol_name);
                    }
                    cxx_abi_lay_member_out(class_type, base_info[i].entry,
                            &layout_info, 
                            /* is_base_class */ 1,
                            /* is_virtual_base_class */ 1);
                    DEBUG_SIZEOF_CODE()
                    {
                        fprintf(stderr, "Finished laying out virtual base '%s'\n", base_info[i].entry->symbol_name);
                    }
                }
            }
        }
    }

    // dsize
    type_set_data_size(class_type, layout_info.size);

    // Finalization, round sizeof(C) up to a non zero multiple of align(C)
    next_offset_with_align(&layout_info.size, layout_info.align);

    if (layout_info.size == 0)
        layout_info.size = 1;

    // If it is POD, but not for the purpose of layout
    // set nvsize(C) = sizeof(C)
    if (is_pod_type(class_type)
            && !is_pod_type_layout(class_type))
    {
        layout_info.nvsize = layout_info.size;
    }

    // sizeof
    type_set_size(class_type, layout_info.size);
    // align
    type_set_alignment(class_type, layout_info.align);

    // nvsize
    class_type_set_non_virtual_size(class_type, layout_info.nvsize);
    // nvalign
    class_type_set_non_virtual_align(class_type, layout_info.nvalign);

    // Valid size
    type_set_valid_size(class_type, 1);

    DEBUG_SIZEOF_CODE()
    {
        fprintf(stderr, "Final layout of class '%s'\n\n", print_declarator(class_type));
        cxx_abi_print_layout(&layout_info);
        fprintf(stderr, "sizeof == %zu\n", type_get_size(class_type));
        fprintf(stderr, "dsize == %zu\n", type_get_data_size(class_type));
        fprintf(stderr, "nvsizeof == %zu\n", class_type_get_non_virtual_size(class_type));
        fprintf(stderr, "nvalign == %zu\n", class_type_get_non_virtual_align(class_type));
    }
}


static void cxx_abi_generic_sizeof(type_t* t)
{
    // FIXME - Named types like in the case below are not well handled
    // typedef __attribute__((aligned(16))) int T;
    if (is_array_type(t))
    {
        cxx_abi_array_sizeof(t);

        // This is like a POD
        type_set_data_size(t, type_get_size(t));
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        // Use the referenced type
        type_t* referenced_type = reference_type_get_referenced_type(t);

        // align
        type_set_alignment(t, type_get_alignment(referenced_type));
        // sizeof
        type_set_size(t, type_get_size(referenced_type));
        // dsize
        type_set_data_size(t, type_get_size(referenced_type));
        // Valid size
        type_set_valid_size(t, 1);
    }
    // Unnamed from now
    else if (is_union_type(t))
    {
        cxx_abi_union_sizeof(t);

        // This should be a POD all the time
        type_set_data_size(t, type_get_size(t));
        // nvsize
        class_type_set_non_virtual_size(t, type_get_size(t));
        // nvalign
        class_type_set_non_virtual_align(t, type_get_alignment(t));
    }
    else if (is_class_type(t))
    {
        cxx_abi_class_sizeof(t);
    }
    else if (is_enum_type(t))
    {
        c_or_cxx_enum_sizeof(t);
    }
    else
    {
        internal_error("Unhandled type size '%s'", print_declarator(t));
    }
}

void generic_system_v_sizeof(type_t* t)
{
    C_LANGUAGE()
    {
        system_v_generic_sizeof(t);
        return;
    }

    CXX_LANGUAGE()
    {
        cxx_abi_generic_sizeof(t);
        return;
    }

    internal_error("Unreachable code", 0);
}

/*
   Specific architecture environment definitions and routines

    1. Define a struct of type 'struct type_environment_t' for every
    architecture. And fill all the fields.
    2. Add the pointer to the list of environments at the end of the file
 */
// ***************
// Linux IA-32
// ***************
// Nothing is aligned more than 4 here
static type_environment_t type_environment_linux_ia32_ = 
{
    .environ_id = "linux-i386",
    .environ_name = "Linux IA32",

    // '_Bool' in C99
    // 'bool' in C++
    .sizeof_bool = 1,
    .alignof_bool = 1,

    .sizeof_wchar_t = 4,
    .alignof_wchar_t = 4,

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

    // Valid both for C and C++
    .compute_sizeof = generic_system_v_sizeof,

    .type_of_sizeof = get_unsigned_int_type,

    // In IA32 'char' == 'signed char'
    .char_type = get_signed_char_type,

    // __builtin_va_list
    .sizeof_builtin_va_list = 4,
    .alignof_builtin_va_list = 4,
};


type_environment_t* type_environment_linux_ia32 = &type_environment_linux_ia32_;

// ***************
// Linux IA64
// ***************
static type_environment_t type_environment_linux_ia64_ = 
{
    .environ_id = "linux-ia64",
    .environ_name = "Linux Itanium",

    // '_Bool' in C99
    // 'bool' in C++
    .sizeof_bool = 1,
    .alignof_bool = 1,

    .sizeof_wchar_t = 4,
    .alignof_wchar_t = 4,

    .sizeof_unsigned_short = 2,
    .alignof_unsigned_short = 2,

    .sizeof_signed_short = 2,
    .alignof_signed_short = 2,

    .sizeof_unsigned_int = 4,
    .alignof_unsigned_int = 4,

    .sizeof_signed_int = 4,
    .alignof_signed_int = 4,

    .sizeof_unsigned_long = 8,
    .alignof_unsigned_long = 8,

    .sizeof_signed_long = 8,
    .alignof_signed_long = 8,

    .sizeof_unsigned_long_long = 8,
    .alignof_unsigned_long_long = 8,

    .sizeof_signed_long_long = 8,
    .alignof_signed_long_long = 8, 

    .sizeof_float = 4,
    .alignof_float = 4,

    .sizeof_double = 8,
    .alignof_double = 4,

    .sizeof_long_double = 16,
    .alignof_long_double = 16,

    .sizeof_pointer = 8,
    .alignof_pointer = 8,

    // One 'ptrdiff_t'
    .sizeof_pointer_to_data_member = 8,
    .alignof_pointer_to_data_member = 8,

    .sizeof_function_pointer = 8,
    .alignof_function_pointer = 8,

    // Two 'ptrdiff_t'
    .sizeof_pointer_to_member_function = 16,
    .alignof_pointer_to_member_function = 8,

    // Valid both for C and C++
    .compute_sizeof = generic_system_v_sizeof,

    // In IA64 a size_t is an unsigned long 
    .type_of_sizeof = get_unsigned_long_int_type,

    // In IA64 'char' == 'signed char'
    .char_type = get_signed_char_type,

    // __builtin_va_list
    .sizeof_builtin_va_list = 8,
    .alignof_builtin_va_list = 8,
};

// ****************
// Linux PowerPC 32
// ****************
static type_environment_t type_environment_linux_ppc32_ = 
{
    .environ_id = "linux-ppc32",
    .environ_name = "Linux PowerPC 32",

    // '_Bool' in C99
    // 'bool' in C++
    .sizeof_bool = 1,
    .alignof_bool = 1,

    .sizeof_wchar_t = 4,
    .alignof_wchar_t = 4,

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
    .alignof_unsigned_long_long = 8,

    .sizeof_signed_long_long = 8,
    .alignof_signed_long_long = 8, 

    .sizeof_float = 4,
    .alignof_float = 4,

    .sizeof_double = 8,
    .alignof_double = 8,

    .sizeof_long_double = 16,
    .alignof_long_double = 16,

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

    // Valid both for C and C++
    .compute_sizeof = generic_system_v_sizeof,

    .type_of_sizeof = get_unsigned_int_type,

    // In PPC32 'char' == 'unsigned char'
    .char_type = get_unsigned_char_type,

    // __builtin_va_list
    .sizeof_builtin_va_list = 12,
    .alignof_builtin_va_list = 4,
};

// ****************
// Linux PowerPC 64
// ****************
static type_environment_t type_environment_linux_ppc64_ = 
{
    .environ_id = "linux-ppc64",
    .environ_name = "Linux PowerPC 64",

    // '_Bool' in C99
    // 'bool' in C++
    .sizeof_bool = 1,
    .alignof_bool = 1,

    .sizeof_wchar_t = 4,
    .alignof_wchar_t = 4,

    .sizeof_unsigned_short = 2,
    .alignof_unsigned_short = 2,

    .sizeof_signed_short = 2,
    .alignof_signed_short = 2,

    .sizeof_unsigned_int = 4,
    .alignof_unsigned_int = 4,

    .sizeof_signed_int = 4,
    .alignof_signed_int = 4,

    .sizeof_unsigned_long = 8,
    .alignof_unsigned_long = 8,

    .sizeof_signed_long = 8,
    .alignof_signed_long = 8,

    .sizeof_unsigned_long_long = 8,
    .alignof_unsigned_long_long = 8,

    .sizeof_signed_long_long = 8,
    .alignof_signed_long_long = 8, 

    .sizeof_float = 4,
    .alignof_float = 4,

    .sizeof_double = 8,
    .alignof_double = 4,

    .sizeof_long_double = 16,
    .alignof_long_double = 16,

    .sizeof_pointer = 8,
    .alignof_pointer = 8,

    // One 'ptrdiff_t'
    .sizeof_pointer_to_data_member = 8,
    .alignof_pointer_to_data_member = 8,

    .sizeof_function_pointer = 8,
    .alignof_function_pointer = 8,

    // Two 'ptrdiff_t'
    .sizeof_pointer_to_member_function = 16,
    .alignof_pointer_to_member_function = 8,

    // Valid both for C and C++
    .compute_sizeof = generic_system_v_sizeof,

    // In PPC64 a size_t is an unsigned long 
    .type_of_sizeof = get_unsigned_long_int_type,

    // In PPC64 'char' == 'unsigned char'
    .char_type = get_unsigned_char_type,

    // __builtin_va_list
    .sizeof_builtin_va_list = 8,
    .alignof_builtin_va_list = 8,
};

// ****************
// Linux AMD 64
// ****************
static type_environment_t type_environment_linux_amd64_ = 
{
    .environ_id = "linux-x86_64",
    .environ_name = "Linux AMD64/EMT64",

    // '_Bool' in C99
    // 'bool' in C++
    .sizeof_bool = 1,
    .alignof_bool = 1,

    .sizeof_wchar_t = 4,
    .alignof_wchar_t = 4,

    .sizeof_unsigned_short = 2,
    .alignof_unsigned_short = 2,

    .sizeof_signed_short = 2,
    .alignof_signed_short = 2,

    .sizeof_unsigned_int = 4,
    .alignof_unsigned_int = 4,

    .sizeof_signed_int = 4,
    .alignof_signed_int = 4,

    .sizeof_unsigned_long = 8,
    .alignof_unsigned_long = 8,

    .sizeof_signed_long = 8,
    .alignof_signed_long = 8,

    .sizeof_unsigned_long_long = 8,
    .alignof_unsigned_long_long = 8,

    .sizeof_signed_long_long = 8,
    .alignof_signed_long_long = 8, 

    .sizeof_float = 4,
    .alignof_float = 4,

    .sizeof_double = 8,
    .alignof_double = 8,

    .sizeof_long_double = 16,
    .alignof_long_double = 16,

    .sizeof_pointer = 8,
    .alignof_pointer = 8,

    // One 'ptrdiff_t'
    .sizeof_pointer_to_data_member = 8,
    .alignof_pointer_to_data_member = 8,

    .sizeof_function_pointer = 8,
    .alignof_function_pointer = 8,

    // Two 'ptrdiff_t'
    .sizeof_pointer_to_member_function = 16,
    .alignof_pointer_to_member_function = 8,

    // Valid both for C and C++
    .compute_sizeof = generic_system_v_sizeof,

    // In PPC64 a size_t is an unsigned long 
    .type_of_sizeof = get_unsigned_long_int_type,

    // In PPC64 'char' == 'unsigned char'
    .char_type = get_unsigned_char_type,

    // __builtin_va_list
    .sizeof_builtin_va_list = 24,
    .alignof_builtin_va_list = 8,
};

// ****************
// Linux SPU
// ****************
static type_environment_t type_environment_linux_spu_ = 
{
    .environ_id = "linux-spu",
    .environ_name = "Linux SPU",

    // '_Bool' in C99
    // 'bool' in C++
    .sizeof_bool = 1,
    .alignof_bool = 1,

    .sizeof_wchar_t = 4,
    .alignof_wchar_t = 4,

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
    .alignof_unsigned_long_long = 8,

    .sizeof_signed_long_long = 8,
    .alignof_signed_long_long = 8, 

    .sizeof_float = 4,
    .alignof_float = 4,

    .sizeof_double = 8,
    .alignof_double = 8,

    .sizeof_long_double = 16,
    .alignof_long_double = 16,

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

    // Valid both for C and C++
    .compute_sizeof = generic_system_v_sizeof,

    .type_of_sizeof = get_unsigned_int_type,

    // In PPC32 'char' == 'unsigned char'
    .char_type = get_unsigned_char_type
};

// 

// ****************
// Solaris SPARCv9
// ****************
static type_environment_t type_environment_solaris_sparcv9_ = 
{
    .environ_id = "solaris-sparcv9",
    .environ_name = "Solaris SPARCv9",

    // '_Bool' in C99
    // 'bool' in C++
    .sizeof_bool = 1,
    .alignof_bool = 1,

    .sizeof_wchar_t = 4,
    .alignof_wchar_t = 4,

    .sizeof_unsigned_short = 2,
    .alignof_unsigned_short = 2,

    .sizeof_signed_short = 2,
    .alignof_signed_short = 2,

    .sizeof_unsigned_int = 4,
    .alignof_unsigned_int = 4,

    .sizeof_signed_int = 4,
    .alignof_signed_int = 4,

    .sizeof_unsigned_long = 8,
    .alignof_unsigned_long = 8,

    .sizeof_signed_long = 8,
    .alignof_signed_long = 8,

    .sizeof_unsigned_long_long = 8,
    .alignof_unsigned_long_long = 8,

    .sizeof_signed_long_long = 8,
    .alignof_signed_long_long = 8, 

    .sizeof_float = 4,
    .alignof_float = 4,

    .sizeof_double = 8,
    // Required 4, recommended 8
    .alignof_double = 8,

    .sizeof_long_double = 16,
    // Required 4, recommended 16
    .alignof_long_double = 16,

    // Size of pointers
    .sizeof_pointer = 8,
    .alignof_pointer = 8,

    // One 'ptrdiff_t'
    .sizeof_pointer_to_data_member = 8,
    .alignof_pointer_to_data_member = 8,

    // Size of pointer to function
    .sizeof_function_pointer = 8,
    .alignof_function_pointer = 8,

    // Two 'ptrdiff_t'
    .sizeof_pointer_to_member_function = 16,
    .alignof_pointer_to_member_function = 8,

    // Valid both for C and C++
    .compute_sizeof = generic_system_v_sizeof,

    // In SparcV9 a size_t is an unsigned long 
    .type_of_sizeof = get_unsigned_long_int_type,

    // In SparcV9 'char' == 'signed char'
    .char_type = get_signed_char_type,

    // __builtin_va_list is lots of times a pointer
    // (check this)
    .sizeof_builtin_va_list = 8,
    .alignof_builtin_va_list = 8,
};

// ****************
// Linux ARM
// ****************
static type_environment_t type_environment_linux_arm_ = 
{
    .environ_id = "linux-arm",
    .environ_name = "Linux ARM (GNUEABI)",

    // '_Bool' in C99
    // 'bool' in C++
    .sizeof_bool = 1,
    .alignof_bool = 1,

    .sizeof_wchar_t = 4,
    .alignof_wchar_t = 4,

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
    .alignof_unsigned_long_long = 8,

    .sizeof_signed_long_long = 8,
    .alignof_signed_long_long = 8, 

    .sizeof_half = 2,
    .alignof_half = 2,

    .sizeof_float = 4,
    .alignof_float = 4,

    .sizeof_double = 8,
    .alignof_double = 8,

    .sizeof_long_double = 16,
    .alignof_long_double = 16,

    // Data pointer
    .sizeof_pointer = 4,
    .alignof_pointer = 4,

    // Code pointer
    .sizeof_function_pointer = 4,
    .alignof_function_pointer = 4,

    // One 'ptrdiff_t'
    .sizeof_pointer_to_data_member = 4,
    .alignof_pointer_to_data_member = 4,

    // Two 'ptrdiff_t'
    .sizeof_pointer_to_member_function = 8,
    .alignof_pointer_to_member_function = 4,

    // Valid both for C and C++
    .compute_sizeof = generic_system_v_sizeof,

    .type_of_sizeof = get_unsigned_int_type,

    // In ARM 'char' == 'unsigned char'
    // ?check this?
    .char_type = get_unsigned_char_type,

    // __builtin_va_list is a struct containing a void* in ARM
    .sizeof_builtin_va_list = 4,
    .alignof_builtin_va_list = 4,
};

/*
   NULL ended list of type environments

   Add your environments here. The driver will enumerate them using this array
 */
type_environment_t* type_environment_list[] = {
    &type_environment_linux_ia32_,
    &type_environment_linux_ia64_,
    &type_environment_linux_ppc32_,
    &type_environment_linux_ppc64_,
    &type_environment_linux_amd64_,
    &type_environment_linux_spu_,
    &type_environment_linux_arm_,
    &type_environment_solaris_sparcv9_,
    NULL, /* last */
};


type_environment_t* default_environment = NULL;
