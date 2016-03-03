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
#include<assert.h>
#include<stdio.h>
#define N 5

int power(int base, int exp)
{
    int result = 1;
    for (int i = 0; i < exp; ++i)
    {
        result *= base;
    }
    return result;
}

int backtrack(int n)
{
    if (n == 0)
    {
        return 1;
    }
    else
    {
        int local_counter = 0;
        for (int i = 0; i < n; ++i)
        {
            #pragma omp task reduction(+: local_counter)  firstprivate(i)
            {
                int res = backtrack(i);
                local_counter += res;
            }
        }
        #pragma omp taskwait
        return local_counter;
    }
}

int main()
{
    int x = backtrack(N);
    assert(x == power(2,N-1));
}
