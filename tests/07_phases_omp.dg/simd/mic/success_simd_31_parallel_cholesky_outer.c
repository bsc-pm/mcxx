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
test_CFLAGS="--variable=prefetch_distance:4,1 --prefetch-in-place --only-aligned-accesses"
test_generator=config/mercurium-parallel-simd-mic
</testinfo>
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>



// This is a left looking cholesky in place
void __attribute((noinline)) cholesky_in_place(int N, float (*a)[N])
{
    float a_jj = 0.0f;

#pragma omp parallel firstprivate(N, a)
{   
    for (int j = 0; j < N; j++)
    {
        int i1;
#pragma omp simd for aligned(a:64) suitable(N)
        for (i1 = j + 1; i1 < N; i1++)
        {
            int k;
            float a_ij = a[i1][j];

            for (k = 0; k < j; k++)
            {
                a_ij -=  a[i1][k] * a[j][k];
            }

            a[i1][j] = a_ij;
        }

#pragma omp single 
        {
            a_jj = a[j][j];
        }

        int i2;

#pragma omp simd for private(i2) reduction(-:a_jj) aligned(a:64) suitable(N)
        for (i2 = 0; i2 < j; i2++)
        {
            float tmp = a[j][i2];
            a_jj -= tmp * tmp;
        }

        float local_a_jj = sqrtf(a_jj);

#pragma omp single nowait
        {
            a[j][j] = local_a_jj;
        }

        int i3;

#pragma omp simd for private(i3) aligned(a:64) suitable(N)
        for (i3 = j + 1; i3 < N; i3++)
        {
            a[i3][j] = a[i3][j] / local_a_jj;
        }
    }
}
}


//#define VERBOSE
#define TEST
#define ERROR 1e-2

const int ALIGNMENT = 64;

static void init_matrix(int N, float (*a)[N])
{
    int i, j;

    for (i = 0; i < N; i++)
    {
        for (j = 0; j <= i; j++)
        {
            a[i][j] = (float)random() / (float)RAND_MAX;
            a[j][i] = a[i][j];
        }
    }

    // Make it positive defined
    for (i = 0; i < N; i++)
    {
        a[i][i] += N;
    }
}

void print_matrix(int N, float (*a)[N])
{
#ifdef VERBOSE
    int i, j;
    for (i = 0; i < N; i++)
    {
        for (j = 0; j < N; j++)
        {
            fprintf(stderr, "%8.2f ", a[i][j]);
        }
        fprintf(stderr, "\n");
    }
#endif
}


static void test_cholesky_in_place(int N, float (*a)[N], 
        float (*ref)[N])
{
    float (*lower)[N] = malloc(sizeof(lower[0][0]) * N * N);
    float (*upper)[N] = malloc(sizeof(lower[0][0]) * N * N);
        
    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j <= i; j++)
        {
            lower[j][i] = 0.0f;
            lower[i][j] = a[i][j];

            upper[i][j] = 0.0f;
            upper[j][i] = a[i][j];
        }
    }

    fprintf(stderr, "LOWER MATRIX\n");
    print_matrix(N, lower);
    fprintf(stderr, "UPPER MATRIX\n");
    print_matrix(N, upper);
    fprintf(stderr, "============\n");

    // We have L and L^T
    // MATMUL!
    int k;

    float (*test)[N] = malloc(sizeof(test[0][0]) * N * N);

    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j < N; j++)
        {
            test[i][j] = 0.0f;
            for (k = 0; k < N; k++)
            {
                test[i][j] += lower[i][k] * upper[k][j];
            }
        }
    }
    char verif = 1;
    float max_err = 0.0f;
    float min_err = HUGE_VALF;
    for (int i = 0; i < N; i++)
    {
#ifdef VERBOSE
        // Make it match print_matrix
        fprintf(stderr, "%8.2f | ", 0.0f);
#endif
        for (int j = 0; j < N; j++)
        {
            float diff = fabsf(test[i][j] - ref[i][j]);

#ifdef VERBOSE
            if (diff < ERROR)
            {
                fprintf(stderr, "%8.2f ", test[i][j]);
            }
            else
            {
                fprintf(stderr, "%7.2f* ", test[i][j]);
            }
#endif

            max_err = fmaxf(max_err, diff);
            min_err = fminf(min_err, diff);

            verif = verif && (diff < ERROR);

        }
#ifdef VERBOSE
        fprintf(stderr, "\n");
#endif
    }
    if (!verif)
    {
        fprintf(stderr, "Verification: UNSUCCESSFUL\n");
    }
    else
    {
        fprintf(stderr, "Verification: Successful\n");
    }

    fprintf(stderr, "Minimum error: %f\n", min_err);
    fprintf(stderr, "Maximum error: %f\n", max_err);

    free(test);
    free(lower);
    free(upper);
}


int main(int argc, char *argv[])
{
    const int N = 160;
    const int iters = 1;;

    float (*a)[N];

    if (posix_memalign((void **)&a, ALIGNMENT, sizeof(float) * N * N) != 0)
    {
        fprintf(stderr, "ERROR allocating 'a'\n");
        return 1;
    }


    init_matrix(N, a);

#ifdef TEST
    float (*ref)[N] = malloc(sizeof(ref[0][0]) * N * N);
    memcpy(ref, a, sizeof(ref[0][0]) * N * N);
#endif

#ifdef VERBOSE
    print_matrix(N, a);
#endif

    cholesky_in_place(N, a);

#ifdef VERBOSE
    fprintf(stderr, "============================================================\n");
    fprintf(stderr, "RESULT\n");
    print_matrix(N, a);
#endif

//    fprintf(stderr, "Time Program: %f seconds\n", time_sec);
#ifdef TEST
    test_cholesky_in_place(N, a, ref);
#else
    fprintf(stderr, "Verification: RESULT NOT VERIFIED\n");
#endif

#ifdef TEST
    free(ref);
#endif
    free(a);

    return 0;
}

