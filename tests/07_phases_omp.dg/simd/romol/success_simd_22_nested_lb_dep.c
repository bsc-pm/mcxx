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
test_CFLAGS="--variable=prefetch_distance:4,1 --prefetch-in-place"
test_generator=config/mercurium-serial-simd-romol
</testinfo>
*/

#include <stdio.h>

#define VECTOR_SIZE 64

void foo_simd(float * __restrict__ __attribute__((__aligned__(VECTOR_SIZE))) b,
        int N)
{
    int i, j;

#pragma omp simd aligned(b:64)
    for(i=N-16; i<N; i++)
    {
        int j;
        float tmp = 0.0f;

        for(j=i; j<N; j+=1)
        {
            tmp += b[i+j];
        }

        b[i] = tmp / (N-i);
    }
}

void foo_scalar(float * __restrict__ __attribute__((__aligned__(VECTOR_SIZE))) b,
        int N)
{
    int i, j;

#pragma novec
    for(i=N-16; i<N; i++)
    {
        int j;
        float tmp = 0.0f;

        for(j=i; j<N; j+=1)
        {
            tmp += b[i+j];
        }

        b[i] = tmp / (N-i);
    }
}


int main()
{
    float __attribute__((__aligned__(VECTOR_SIZE))) a[64];
    float __attribute__((__aligned__(VECTOR_SIZE))) b[64];

    int N = 19;
    int i;

    for(i=0; i<64; i++)
    {
        a[i] = b[i] = i;
    }

    foo_simd(a, N);
    foo_scalar(b, N);

    for(i=0; i<64; i++)
    {
        if(a[i] != b[i])
        {
            printf("Error at %d: %f != %f\n", i, a[i], b[i]);
            return 1;
        }
    }

    printf("SUCCESS!\n");

    return 0;
}


