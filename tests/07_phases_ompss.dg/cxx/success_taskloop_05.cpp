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
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcxx=yes
</testinfo>
*/
#include<assert.h>
#include<stdio.h>
#define N 1000
#define NUM_TASKS 20

void strictly_increasing_loop(int l, int u, int s, int ntasks)
{
    int var = 0;
    char first_time = 1;
    #pragma omp taskloop num_tasks(ntasks) firstprivate(first_time) shared(var)
    for (int j = l; j < u; j += s)
    {

        if (first_time)
        {
            #pragma omp atomic
            var++;
            first_time = 0;
        }
    }
    assert(var == ntasks);
}

void strictly_decreasing_loop(int l, int u, int s, int ntasks)
{
    int var = 0;
    char first_time = 1;
    #pragma omp taskloop num_tasks(ntasks) firstprivate(first_time) shared(var)
    for (int j = l; j >= u; j += s)
    {
        if (first_time)
        {
            #pragma omp atomic
            var++;
            first_time = 0;
        }
    }
    assert(var == ntasks);
}


int main(int argc, char* argv[])
{
    int var;
    for(int ntasks = 1; ntasks <= 20; ++ntasks)
    {
        strictly_increasing_loop(0, 100,  1, ntasks);
        strictly_decreasing_loop(99,  0, -1, ntasks);

        strictly_increasing_loop(-100,  0,  1, ntasks);
        strictly_decreasing_loop(-1, -100, -1, ntasks);

        strictly_increasing_loop(-100, 100,  1, ntasks);
        strictly_decreasing_loop(99,  -100, -1, ntasks);
    }
    return 0;
}
