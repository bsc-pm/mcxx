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
test_CFLAGS=--only-adjacent-accesses
test_generator=config/mercurium-serial-simd-romol
</testinfo>
*/

#include <stdio.h>
#include <stdlib.h>

#define VECTOR_SIZE 64

void test(void * z, float N)
{
    float *_z = (float *) z;
    int i;

    for (i=0; i<N; i++)
    {
        if (_z[i] == 1)
        {
            printf("Error\n");
            exit (1);
        }
    }
}

void __attribute__((noinline)) lt_float(float *x, float *y, float *z, int N)
{
    int j;
#pragma omp simd
        for (j=0; j<N; j++)
        {
            z[j] = (x[j] < y[j]) ? 0 : 1;
        }
}

void __attribute__((noinline)) le_float(float *x, float *y, float *z, int N)
{
    int j;
#pragma omp simd
        for (j=0; j<N; j++)
        {
            z[j] = (x[j] <= y[j]) ? 0 : 1;
        }
}

void __attribute__((noinline)) gt_float(float *x, float *y, float *z, int N)
{
    int j;
#pragma omp simd
        for (j=0; j<N; j++)
        {
            z[j] = (x[j] > y[j]) ? 0 : 1;
        }
}

void __attribute__((noinline)) ge_float(float *x, float *y, float *z, int N)
{
    int j;
#pragma omp simd
        for (j=0; j<N; j++)
        {
            z[j] = (x[j] >= y[j]) ? 0 : 1;
        }
}

void __attribute__((noinline)) eq_float(float *x, float *y, float *z, int N)
{
    int j;
#pragma omp simd
        for (j=0; j<N; j++)
        {
            z[j] = (x[j] == y[j]) ? 0 : 1;
        }
}

void __attribute__((noinline)) diff_float(float *x, float *y, float *z, int N)
{
    int j;
#pragma omp simd
        for (j=0; j<N; j++)
        {
            z[j] = (x[j] != y[j]) ? 0 : 1;
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

    int i, j;

    for (i=0; i<N; i++)
    {
        x[i] = i;
        y[i] = i+1;
        z[i] = 0.0f;
    }

    lt_float(x, y, z, N);
    test((void *)z, N);

    gt_float(y, x, z, N);
    test((void *)z, N);

    le_float(x, y, z, N);
    test((void *)z, N);

    ge_float(y, x, z, N);
    test((void *)z, N);

    diff_float(y, x, z, N);
    test((void *)z, N);

    for (i=0; i<N; i++)
    {
        x[i] = i;
        y[i] = i;
    }

    eq_float(y, x, z, N);
    test((void *)z, N);

    printf("SUCCESS!\n");
    return 0;
}

