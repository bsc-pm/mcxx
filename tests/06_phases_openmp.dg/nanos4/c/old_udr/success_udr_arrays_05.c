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
test_ignore=yes
</testinfo>
*/
#include <string.h>

void add_vec(int, float*, float*);

void add_mat(int, int, float*, float*);

#pragma omp declare reduction(add_vec : float) dimensions(1)

#pragma omp declare reduction(add_mat : float) dimensions(2)

void f1(int num_vectors, int vector_size)
{
    float array_of_vec[num_vectors][vector_size];
    float vec_sum[vector_size];

    memset(vec_sum, 0, sizeof(vec_sum));
    
    int i; 
#pragma omp parallel for reduction(add_vec : vec_sum)
    for (i = 0; i < num_vectors; i++)
    {
        add_vec(vector_size, &vec_sum[0], &array_of_vec[i][0]);
    }
}

void f2(int num_mats, int vector_size)
{
    float array_of_mat[num_mats][vector_size][vector_size];
    float mat_sum[vector_size][vector_size];

    memset(mat_sum, 0, sizeof(mat_sum));
    
    int i; 
#pragma omp parallel for reduction(add_mat : mat_sum)
    for (i = 0; i < num_mats; i++)
    {
        add_mat(vector_size, vector_size, &mat_sum[0][0], &array_of_mat[i][0][0]);
    }
}
