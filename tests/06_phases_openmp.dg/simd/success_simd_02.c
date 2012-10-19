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
#include <stdlib.h>
#include <malloc.h>

int main(int argc, char * argv[])
{

#if __GNUC__ == 4 && __GNUC_MINOR__ >= 4

    const int vec_size = 9000;
    float time;

    float *a = (float *) memalign(16, vec_size*sizeof(float));
    float *b = (float *) memalign(16, vec_size*sizeof(float));
    float *c = (float *) memalign(16, vec_size*sizeof(float));

    int i;

#pragma omp simd
    for (i=0; i<vec_size; i++)
    {
        a[i] = 5.0f;
        b[i] = 6.0f + a[i] + 4.0f;
    }

#pragma omp simd
    for (i=0; i<vec_size; i++)
    {
        float tmp = a[i]+b[i] * 3;
        float tmp2 = 1+6*6;

        c[i] = tmp * tmp + 6 + 20.0f + 40.0;
        c[i] = c[i] + tmp2;
        c[i] = c[i] * c[i];
    }

    printf("%f %f\n", c[0], c[vec_size-1]);

#else
  #warning "This compiler is not supported"
#endif
    return 0;
}

