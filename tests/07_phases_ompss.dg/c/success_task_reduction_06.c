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
test_compile_fail_nanos6_imcc=yes
</testinfo>
*/
#include<stdio.h>
#include<time.h>
#include<stdlib.h>
#include<assert.h>

#define N 1000

int main()
{
    int v[N];
    int serial_max_val = 0, max_val = 0;
    int i;

    srand(time(NULL));
    for(i = 0; i < N; i++)
    {
        v[i] = rand() % N;
        if (serial_max_val < v[i])
        {
            serial_max_val = v[i];
        }
    }

    for(i = 0;i < N; i++)
    {
        #pragma omp task reduction(max : max_val) firstprivate(i) shared(v)
        {
            if(v[i] > max_val)
            {
                max_val = v[i];
            }
        }
    }
#pragma omp taskwait
    assert(serial_max_val == max_val);
}
