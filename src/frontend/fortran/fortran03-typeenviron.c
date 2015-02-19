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

#include "fortran03-typeenviron.h"

#include "cxx-driver.h"
#include "cxx-typeutils.h"
#include "cxx-typeenviron.h"

#define MAX(A, B) (A > B ? A : B)

/*

Gfortran array descriptor up to 4.7
===================================

typedef struct descriptor_dimension
{
  index_type _stride;
  index_type lower_bound;
  index_type _ubound;
}

descriptor_dimension;

#define GFC_ARRAY_DESCRIPTOR(r, type) \
struct {\
  type *base_addr;\
  size_t offset;\
  index_type dtype;\
  descriptor_dimension dim[r];\
}

*/

static void gfortran_array_descriptor_info(_size_t *size, _size_t *align, int rank)
{
    _size_t base_addr_size = CURRENT_CONFIGURATION->type_environment->sizeof_pointer;
    _size_t base_addr_align = CURRENT_CONFIGURATION->type_environment->alignof_pointer;

    _size_t offset_size = type_get_size(CURRENT_CONFIGURATION->type_environment->type_of_sizeof());
    _size_t offset_align = type_get_alignment(CURRENT_CONFIGURATION->type_environment->type_of_sizeof());

    _size_t dtype_size = type_get_size(CURRENT_CONFIGURATION->type_environment->type_of_ptrdiff_t());
    _size_t dtype_align = type_get_alignment(CURRENT_CONFIGURATION->type_environment->type_of_ptrdiff_t());

    // Auxiliar
    _size_t descriptor_dimension_size = 3 * type_get_size(CURRENT_CONFIGURATION->type_environment->type_of_ptrdiff_t());
    _size_t descriptor_dimension_align = type_get_alignment(CURRENT_CONFIGURATION->type_environment->type_of_ptrdiff_t());

    _size_t dim_size = rank * descriptor_dimension_size;
    _size_t dim_align = descriptor_dimension_align;

    *size = base_addr_size + offset_size + dtype_size + dim_size;
    *align = MAX(base_addr_align, MAX(offset_align, MAX(dtype_align, MAX(dim_align, 0))));
}

static _size_t gfortran_array_descriptor_get_size(type_t* t UNUSED_PARAMETER, int rank)
{
    _size_t size, align;
    gfortran_array_descriptor_info(&size, &align, rank);

    return size;
}

static _size_t gfortran_array_descriptor_get_alignment(type_t* t UNUSED_PARAMETER, int rank)
{
    _size_t size, align;
    gfortran_array_descriptor_info(&size, &align, rank);

    return align;
}

static fortran_array_descriptor_t gfortran_array_descriptor =
{
    "gfortran", "GNU Fortran compiler",
    gfortran_array_descriptor_get_size,
    gfortran_array_descriptor_get_alignment,
};

/*
 * Intel Fortran array descriptor
 * (Taken from Chasm)
 */
#if 0
typedef struct dope_vec_Intel_ {
  char*   base_addr;		  /* base address of the array		*/
  long    elem_size;		  /* size of a single element           */
  long    offset;		  /* base_addr + offset is array start  */

  unsigned int    assoc     :  1; /* 1 = has been associated		*/
  unsigned int    ptr_alloc :  1; /* 1 = has been allocated		*/
  unsigned int    p_or_a    :  2; /* 1 = pointer, 2 = allocatable	*/
  unsigned int    non_contig:  1; /* 0 = is contiguous			*/
  unsigned int    reserved_1: 11;
  unsigned int    reserved_2: 16;
#ifdef CHASM_ARCH_64
  unsigned int    reserved64: 16;
#endif
  long rank;                      /* number of dimensions		*/
  long reserved_3;

  struct {
    long extent;       /* number of elements for a given dimension	*/
    long stride_mult;  /* distance between successive elements (bytes)  */
    long lower_bound;  /* first array index for a given dimension	*/
  } dim[7];

} dope_vec_Intel;
#endif

static _size_t ifort_array_descriptor_get_size(type_t* t UNUSED_PARAMETER, int rank)
{
    // 64 bit
    if (CURRENT_CONFIGURATION->type_environment->sizeof_pointer == 8)
    {
        return (48 + 24 * rank);
    }
    // 32 bit
    else if (CURRENT_CONFIGURATION->type_environment->sizeof_pointer == 4)
    {
        return (24 + 12 * rank);
    }
    else
    {
        internal_error("Unsupported environment", 0);
    }
}

static _size_t ifort_array_descriptor_get_alignment(type_t* t UNUSED_PARAMETER, int rank UNUSED_PARAMETER)
{
    // 64 bit
    if (CURRENT_CONFIGURATION->type_environment->sizeof_pointer == 8)
    {
        return 8;
    }
    // 32 bit
    else if (CURRENT_CONFIGURATION->type_environment->sizeof_pointer == 4)
    {
        return 4;
    }
    else
    {
        internal_error("Unsupported environment", 0);
    }
}

static fortran_array_descriptor_t ifort_array_descriptor =
{
    "ifort", "Intel Fortran (version 8 or later)",
    ifort_array_descriptor_get_size,
    ifort_array_descriptor_get_alignment,
};

/*
 * IBM XL Array descriptor
 * (Taken from Chasm)
 */
#if 0
 typedef struct dope_vec_IBMXL_ {
  void* base_addr;            /* base address of the array               */
  unsigned int p_or_a    : 8; /* pointer (=1) or array (=3)              */
  unsigned int type_code : 8; /* integer id representing the datatype    */
  unsigned int not_ptr   : 1; /* pointer (=0 if pointer)                 */
  int cookie             : 3; /* 4 (rank=0), 5 otherwise                 */
  unsigned int zero      : 4; /* always 0                                */
  unsigned int flag64    : 1; /* 1 if 64 bit object files (-q64)         */
  unsigned int one       : 7; /* always 1                                */
#ifdef CHASM_ARCH_64
  unsigned int       rank;       /* number of dimensions                    */
  unsigned long_type elem_size;  /* size of datatype                        */
#else
  unsigned long_type elem_size;  /* size of datatype                        */
  unsigned int       rank;       /* number of dimensions                    */
#endif
  long_type          sum_d;      /* -sumof(lower*mult)                      */

  /* array bounds information */
  struct {
    long_type lower_bound;  /* first array index for a given dimension      */
    long_type extent;       /* number of elements for a given dimension     */
    long_type stride_mult;  /* distance between successive elements (bytes) */
  } dim[7];
} dope_vec_IBMXL;
#endif

static _size_t xlf_array_descriptor_get_size(type_t* t UNUSED_PARAMETER, int rank)
{
    // 64 bit
    if (CURRENT_CONFIGURATION->type_environment->sizeof_pointer == 8)
    {
        return (32 + 24 * rank);
    }
    // 32 bit
    else if (CURRENT_CONFIGURATION->type_environment->sizeof_pointer == 4)
    {
        return (20 + 12 * rank);
    }
    else
    {
        internal_error("Unsupported environment", 0);
    }
}

static _size_t xlf_array_descriptor_get_alignment(type_t* t UNUSED_PARAMETER, int rank UNUSED_PARAMETER)
{
    // 64 bit
    if (CURRENT_CONFIGURATION->type_environment->sizeof_pointer == 8)
    {
        return 8;
    }
    // 32 bit
    else if (CURRENT_CONFIGURATION->type_environment->sizeof_pointer == 4)
    {
        return 4;
    }
    else
    {
        internal_error("Unsupported environment", 0);
    }
}

static fortran_array_descriptor_t xlf_array_descriptor =
{
    "xlf", "IBM XL Fortran",
    xlf_array_descriptor_get_size,
    xlf_array_descriptor_get_alignment,
};

// Common code

_size_t fortran_size_of_array_descriptor(type_t* t, int rank)
{
    return (CURRENT_CONFIGURATION->fortran_array_descriptor->get_size)(t, rank);
}

_size_t fortran_alignment_of_array_descriptor(type_t* t, int rank)
{
    return (CURRENT_CONFIGURATION->fortran_array_descriptor->get_alignment)(t, rank);
}

fortran_array_descriptor_t* default_fortran_array_descriptor = &gfortran_array_descriptor;

fortran_array_descriptor_t* fortran_array_descriptor_list[] =
{
    &gfortran_array_descriptor,
    &ifort_array_descriptor,
    &xlf_array_descriptor,
    NULL
};
