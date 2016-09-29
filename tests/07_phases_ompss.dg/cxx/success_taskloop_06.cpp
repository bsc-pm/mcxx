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
test_generator=config/mercurium-ompss
</testinfo>
*/

#include <assert.h>

void loop_1(int *v, int start, int end, int step, int offset, int chunk)
{
    #pragma omp taskloop num_tasks(chunk)
    for (int i = start; i < end; i += step) {
        v[i+offset]++;
    }
}

void loop_2(int *v, int start, int end, int step, int offset, int chunk)
{
    #pragma omp taskloop num_tasks(chunk)
    for (int i = start; i > end; i += step) {
        v[offset-i]++;
    }
}


void check_results(int *v, int start, int end, int step, int chunk_values)
{
    for (int i = start; i < end; i += step)
        assert(v[i] == chunk_values);
}


void init(int *v, int size)
{
    for(int i = 0; i < size; ++i)
        v[i] = 0;
}

int main(int argc, char *argv[]) {
    const int n = 2000;
    int v[n];


    //printf("first config\n");
    {
        int start  =     0;
        int end    =  2000;
        int step   =    10;
        int offset =     0;

        int chunk_values = 20;
        init(v, n);
        for (int i = 1; i <= chunk_values; i++)
            loop_1(v, start, end, step, offset, i);

        check_results(v, 0, n, step, 20);
    }

    //printf("second config\n");
    {
        int start  =  2000;
        int end    =     0;
        int step   =   -10;
        int offset =  2000;

        int chunk_values = 20;
        init(v, n);
        for (int i = 1; i <= chunk_values; i++)
            loop_2(v, start, end, step, offset, i);

        check_results(v, 0, n, -step, 20);
    }

    //printf("third config\n");
    {
        int start  = -2000;
        int end    =     0;
        int step   =    10;
        int offset =  2000;

        int chunk_values = 20;
        init(v, n);
        for (int i = 1; i <= chunk_values; i++)
            loop_1(v, start, end, step, offset, i);

        check_results(v, 0, n, step, 20);
    }


    //printf("fourth config\n");
    {
        int start  =     0;
        int end    = -2000;
        int step   =   -10;
        int offset =     0;

        int chunk_values = 20;
        init(v, n);
        for (int i = 1; i <= chunk_values; i++)
            loop_2(v, start, end, step, offset, i);

        check_results(v, 0, n, -step, 20);
    }


    //printf("fifth config\n");
    {
        int start  = -1000;
        int end    =  1000;
        int step   =    10;
        int offset =  1000;

        int chunk_values = 20;
        init(v, n);
        for (int i = 1; i <= chunk_values; i++)
            loop_1(v, start, end, step, offset, i);

        check_results(v, 0, n, step, 20);
    }

    //printf("sixth config\n");
    {
        int start  =  1000;
        int end    = -1000;
        int step   =   -10;
        int offset =  1000;

        int chunk_values = 20;
        init(v, n);
        for (int i = 1; i <= chunk_values; i++)
            loop_2(v, start, end, step, offset, 10);

        check_results(v, 0, n, -step, chunk_values);
    }

    return 0;
}
