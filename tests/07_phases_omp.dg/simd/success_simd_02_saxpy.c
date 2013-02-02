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
test_generator=config/mercurium-simd
</testinfo>
*/


#include <stdio.h>

void __attribute__((noinline)) saxpy(float *x, float *y, float a, int N)
{
    int j;
#pragma omp simd
    for (j=0; j<N; j++)
    {
        y[j] = a * x[j] + y[j];
    }
}


int main ()
{
    const int N = 80;
    const int iters = 2;

    float * x = malloc(N*sizeof(float));
    float * y = malloc(N*sizeof(float));
    float a = 0.93f;

    int i, j;

    for (i=0; i<N; i++)
    {
        x[i] = i+1;
        y[i] = i-1;
    }

    for (i=0; i<iters; i++)
    {
        saxpy(x, y, a, N);
    }

    printf("%f\n", y[10]);
    
    return 0;
}

