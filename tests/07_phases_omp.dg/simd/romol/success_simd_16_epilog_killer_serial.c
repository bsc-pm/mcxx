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
test_CFLAGS=--only-adjacent-accesses
test_generator="config/mercurium-serial-simd-romol"
</testinfo>
*/


#include <math.h>
#include <malloc.h>
#include <stdlib.h>


int main (int argc, char* argv[])
{
    const int width = 40;
    const int height = 40;

    int* input;
   
    if(posix_memalign((void **) &input, 64, 1024*sizeof(int)) != 0)
    {
        exit(1);
    }

    int i;

    for (i=0; i<1024; i++)
    {
        input[i] = -5;
    }


#pragma omp simd
    for (i=0; i<5; i++)
    {
        input[i] = i*i;
    }

    for (i=0; i<5; i++)
    {
        if (input[i] != i*i)
        {
            printf("1st fails!!\n");
            return 1;
        }
    }
    for (; i<32; i++)
    {
        if (input[i] != -5)
        {
            printf("2nd fails!!\n");
            return 1;
        }
    }




#pragma omp simd
    for (i=0; i<16; i++)
    {
        input[i] = i;
    }

    for (i=0; i<16; i++)
    {
        if (input[i] != i)
        {
            printf("3rd fails!!\n");
            return 1;
        }
    }
    for (; i<32; i++)
    {
        if (input[i] != -5)
        {
            printf("4th fails!!\n");
            return 1;
        }
    }

#pragma omp simd
    for (i=0; i<17; i++)
    {
        input[i] = -i;
    }

    for (i=0; i<17; i++)
    {
        if (input[i] != -i)
        {
            printf("5th fails!!\n");
            return 1;
        }
    }
    for (; i<32; i++)
    {
        if (input[i] != -5)
        {
            printf("6th fails!!\n");
            return 1;
        }
    }

#pragma omp simd
    for (i=0; i<18; i++)
    {
        input[i] = i*i;
    }

    for (i=0; i<18; i++)
    {
        if (input[i] != i*i)
        {
            printf("7th fails!!\n");
            return 1;
        }
    }
    for (; i<32; i++)
    {
        if (input[i] != -5)
        {
            printf("8th fails!!\n");
            return 1;
        }
    }


    for (i=0; i<1024; i++)
    {
        input[i] = -5;
    }
    
    int j;
    for(j=0; j<16; j++)
    {
#pragma omp simd
        for (i=0; i<j; i++)
        {
            input[i] = -i-i;
        }

        for (i=0; i<j; i++)
        {
            if (input[i] != -i-i)
            {
                printf("1* fails!!\n");
                return 1;
            }
        }
        for (; i<32; i++)
        {
            if (input[i] != -5)
            {
                printf("2* fails!!\n");
                return 1;
            }
        }
    }

    return 0;
}

