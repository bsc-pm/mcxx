/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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


#include "fortran03-cexpr.h"

int fortran_flatten_array_count_elements(const_value_t* v)
{
    if (const_value_is_array(v))
    {
        int r = 0;
        int i, N = const_value_get_num_elements(v);
        for (i = 0; i < N; i++)
        {
            r += fortran_flatten_array_count_elements(const_value_get_element_num(v, i));
        }

        return r;
    }
    else
    {
        return 1;
    }
}

static void fortran_flatten_array_rec(const_value_t* v, const_value_t*** scalar_item)
{
    if (const_value_is_array(v))
    {
        int i, N = const_value_get_num_elements(v);
        for (i = 0; i < N; i++)
        {
            fortran_flatten_array_rec(const_value_get_element_num(v, i), scalar_item);
        }
    }
    else
    {
        (**scalar_item) = v;
        (*scalar_item)++;
    }
}

const_value_t* fortran_flatten_array(const_value_t* v)
{
    int N = fortran_flatten_array_count_elements(v);
    const_value_t** flattened_items = NEW_VEC0(const_value_t*, N);

    const_value_t** pos = flattened_items;
    fortran_flatten_array_rec(v, &pos);

    const_value_t* result = const_value_make_array(N, flattened_items);

    DELETE(flattened_items);

    return result;
}

int fortran_flatten_array_count_elements_with_mask(const_value_t* v, const_value_t* mask)
{
    if (const_value_is_array(v) != const_value_is_array(mask))
            return -1;

    if (const_value_is_array(v))
    {
        int r = 0;
        int i, N = const_value_get_num_elements(v);
        for (i = 0; i < N; i++)
        {
            r += fortran_flatten_array_count_elements_with_mask(
                    const_value_get_element_num(v, i),
                    const_value_get_element_num(mask, i));
        }

        return r;
    }
    else 
    {
        if (const_value_is_nonzero(mask))
            return 1;
        else
            return 0;
    }
}

static void fortran_flatten_array_mask_rec(const_value_t* v, const_value_t* mask, const_value_t*** scalar_item)
{
    if (const_value_is_array(v))
    {
        int i, N = const_value_get_num_elements(v);
        for (i = 0; i < N; i++)
        {
            fortran_flatten_array_mask_rec(
                    const_value_get_element_num(v, i), 
                    const_value_get_element_num(mask, i), 
                    scalar_item);
        }
    }
    else
    {
        if (const_value_is_nonzero(mask))
        {
            (**scalar_item) = v;
            (*scalar_item)++;
        }
    }
}

const_value_t* fortran_flatten_array_with_mask(const_value_t* v, const_value_t* mask)
{
    if (mask == NULL)
        return fortran_flatten_array(v);

    int N = fortran_flatten_array_count_elements_with_mask(v, mask);
    if (N < 0)
        return NULL;

    const_value_t** flattened_items = NEW_VEC0(const_value_t*, N);

    const_value_t** pos = flattened_items;
    fortran_flatten_array_mask_rec(v, mask, &pos);

    const_value_t* result = const_value_make_array(N, flattened_items);

    DELETE(flattened_items);

    return result;
}

const_value_t* fortran_const_value_rank_zero(const_value_t* v)
{
    while (const_value_is_array(v))
    {
        v = const_value_get_element_num(v, 0);
    }

    return v;
}
