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


/*
<testinfo>
test_generator=config/mercurium-nanos4
test_CFLAGS=--variable=new_udr:0
</testinfo>
*/
#include <string.h>

void add_vec(float*, float*, int);

void add_mat(float*, float*, int, int);

#pragma omp declare reduction(add_vec : float) dimensions(1)

#pragma omp declare reduction(add_mat : float) dimensions(2)

#define VECTOR_SIZE (100)
#define NUM_VECTORS (100)

void f1(void)
{
    float array_of_vec[NUM_VECTORS][VECTOR_SIZE];
    float vec_sum[VECTOR_SIZE];

    memset(vec_sum, 0, sizeof(vec_sum));
    
    int i; 
#pragma omp parallel for reduction(add_vec : vec_sum)
    for (i = 0; i < NUM_VECTORS; i++)
    {
        add_vec(&vec_sum[0], &array_of_vec[i][0], VECTOR_SIZE);
    }
}

#define NUM_MATS (100)

void f2(void)
{
    float array_of_mat[NUM_MATS][VECTOR_SIZE][VECTOR_SIZE];
    float mat_sum[VECTOR_SIZE][VECTOR_SIZE];

    memset(mat_sum, 0, sizeof(mat_sum));
    
    int i; 
#pragma omp parallel for reduction(add_mat : mat_sum)
    for (i = 0; i < NUM_MATS; i++)
    {
        add_mat(&mat_sum[0][0], &array_of_mat[i][0][0], VECTOR_SIZE, VECTOR_SIZE);
    }
}
