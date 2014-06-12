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

/*
<testinfo>
test_generator=config/mercurium-serial-simd-avx2
</testinfo>
*/

#include <stdio.h>
#include <stdlib.h>

#define VECTOR_SIZE 64

void __attribute__((noinline)) saxpy(float *x, float *y, float *z, float a, int N)
{
    int j;
#pragma omp simd 
        for (j=0; j<N; j++)
        {
            z[j] = a * x[j] + y[j];
        }
}


int main (int argc, char * argv[])
{
    const int N = 16;
    const int iters = 1;

    float *x, *y, *z; 
    
    posix_memalign((void **)&x, VECTOR_SIZE, N*sizeof(float));
    posix_memalign((void **)&y, VECTOR_SIZE, N*sizeof(float));
    posix_memalign((void **)&z, VECTOR_SIZE, N*sizeof(float));
    
    float a = 0.93f;

    int i, j;

    for (i=0; i<N; i++)
    {
        x[i] = i+1;
        y[i] = i-1;
        z[i] = 0.0f;
    }

    for (i=0; i<iters; i++)
    {
        saxpy(x, y, z, a, N);
    }

    for (i=0; i<N; i++)
    {
        if (z[i] != (a * x[i] + y[i]))
        {
            printf("Error\n");
            return (1);
        }
    }

    printf("SUCCESS!\n");
    return 0;
}

