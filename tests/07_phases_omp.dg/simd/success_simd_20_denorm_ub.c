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
test_generator=config/mercurium-serial-simd
</testinfo>
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>


#define VECTOR_SIZE 64
typedef int * __restrict__ F;

__attribute__((noinline)) int foo(
        int * __restrict__ __attribute__((__aligned__(VECTOR_SIZE))) b, 
        int N)
{
    int i,j;

    int tmp = 0.0f;

#pragma omp simd for reduction(+:tmp)
        for(i=0; i<(N-16); i++)
        {
            tmp += b[i] * 

                (((b[i+1] + 
                  b[i+2] * 
                   b[i+3]) + 

                    (b[i+4] + 
                     b[i+5] *
                      b[i+6]) *

                       ((b[i+7] + 
                        b[i+8] * 
                         b[i+9]) + 

                          (b[i+10] +
                           b[i+11] *
                            b[i+12]) *

                             (b[i+13] +
                              b[i+14] *
                               b[i+15]))));
        }

    return tmp;
}

int main (int argc, char* argv[])
{
    int* b; 
    int result, i;
    int N = 16000; 

    if(posix_memalign((void **) &b, VECTOR_SIZE, N * sizeof(int)) != 0)
    {
        return 1;
    }

    for (i=0; i<N; i++)
    {
        b[i] = 5;
    }

    result = 0;
    N=7;

    for(i=0; i<N-16; i++)
    {
        result += b[i] * 

            (((b[i+1] + 
               b[i+2] * 
               b[i+3]) + 

              (b[i+4] + 
               b[i+5] *
               b[i+6]) *

              ((b[i+7] + 
                b[i+8] * 
                b[i+9]) + 

               (b[i+10] +
                b[i+11] *
                b[i+12]) *

               (b[i+13] +
                b[i+14] *
                b[i+15]))));
    }

    int r = foo(b,N);
    printf("TEST1: %d and %d\n",  r, result);

    if(r != result)
    {
        printf("ERROR %d != %d\n", r, result);
        return 1;
    }

    printf("SUCCESS\n");


    result = 0;
    N = 1600;
    for(i=0; i<N-16; i++)
    {
        result += b[i] * 

            (((b[i+1] + 
               b[i+2] * 
               b[i+3]) + 

              (b[i+4] + 
               b[i+5] *
               b[i+6]) *

              ((b[i+7] + 
                b[i+8] * 
                b[i+9]) + 

               (b[i+10] +
                b[i+11] *
                b[i+12]) *

               (b[i+13] +
                b[i+14] *
                b[i+15]))));
    }

    r = foo(b,N);
    printf("TEST2: %d and %d\n",  r, result);

    if(r != result)
    {
        printf("ERROR %d != %d\n", r, result);
        return 1;
    }
    printf("SUCCESS\n");

    return 0;
}

