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

int global_var;

// This function returns 1 if the current node is a leaf. Otherwise returns 0.
int backtrack(int n)
{
    if (n == 0)
    {
        return 1;
    }
    else
    {
        for (int i = 0; i < n; ++i)
        {
            #pragma omp task reduction(+: global_var)  firstprivate(i)
            {
                //  The function call to backtrack may modify the value of global_var, so firstly we
                //  have to execute the task and then load the value of global_var
                // global_var += backtrack(i); //KO
                int res = backtrack(i);
                global_var += res;
            }
        }
        #pragma omp taskwait
        return 0;
    }
}

int main()
{
    global_var = 0;
    backtrack(N);
    #pragma omp taskwait
    int x = global_var;
    assert(x == power(2,N-1));
}
