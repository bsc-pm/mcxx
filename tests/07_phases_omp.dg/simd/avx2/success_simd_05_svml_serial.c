#include <math.h>
#include <malloc.h>
#include <stdlib.h>

/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
test_generator="config/mercurium-serial-simd-avx2 svml"
</testinfo>
*/

#include <stdlib.h>


void __attribute__((noinline)) test_vec(float *x, float *y)
{
    int j;
#pragma omp simd
    for (j=0; j<4; j++)
    {
        y[j] = sinf(x[j]);
    }

#pragma omp simd
    for (j=4; j<8; j++)
    {
        y[j] = expf(x[j]);
    }

#pragma omp simd
    for (j=8; j<12; j++)
    {
        y[j] = logf(x[j]);
    }

#pragma omp simd
    for (j=12; j<16; j++)
    {
        y[j] = fabsf(x[j]);
    }

#pragma omp simd
    for (j=16; j<20; j++)
    {
        y[j] = sqrtf(x[j]);
    }
}

void __attribute__((noinline)) test_sc(float *x, float *y)
{
    int j;
    for (j=0; j<4; j++)
    {
        y[j] = sinf(x[j]);
    }

    for (j=4; j<8; j++)
    {
        y[j] = expf(x[j]);
    }

    for (j=8; j<12; j++)
    {
        y[j] = logf(x[j]);
    }

    for (j=12; j<16; j++)
    {
        y[j] = fabsf(x[j]);
    }

    for (j=16; j<20; j++)
    {
        y[j] = sqrtf(x[j]);
    }
}

int main (int argc, char* argv[])
{
    const int N = 10 * 4;

    float* input, *output, *input_sc, *output_sc;
   
    if(posix_memalign((void **) &input, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &output, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &input_sc, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &output_sc, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }
 
    int i;
    for (i=0; i<N; i++)
    {
        input[i] = (i*0.9f)/(i+1);
        input_sc[i] = (i*0.9f)/(i+1);
    }

    test_vec(input, output);
    test_sc(input_sc, output_sc);

#define ERROR 0.01

    for (i=0; i<N; i++)
    {
        if(fabsf(input_sc[i] - input[i]) > ERROR)
        {
            printf("ERROR: %f != %f\n", input_sc[i], input[i]);
            exit(1);
        }
        if(fabsf(output_sc[i] - output[i]) > ERROR)
        {
            printf("ERROR: %f != %f\n", output_sc[i], output[i]);
            exit(1);
        }
    }
    printf("SUCCESS\n");

    return 0;
}

