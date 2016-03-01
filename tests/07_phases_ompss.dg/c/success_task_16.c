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
test_generator="config/mercurium-ompss"
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcc=yes
</testinfo>
*/

#include<assert.h>
#include<omp.h>

#define N 1000

__thread int res = 0;

int main()
{
    int result = 0;
    int i;
    int v[N];

    for (i = 0; i < N; ++i) v[i] = i+1;

    for (i = 0; i < N; ++i)
    {
        #pragma omp task shared(v) firstprivate(i)
        {
            // printf("begining the task(%d): %d, %d, %p\n", v[i], omp_get_thread_num(), res, &res);
            res += v[i];
            // printf("ending the task(%d): %d, %d, %p\n", v[i], omp_get_thread_num(), res, &res);
        }
    }
    #pragma omp taskwait

    // Note: I came up with this idea to reduce the thread local copies,
    //       I'm not sure if there are other ways to do it!
    #pragma omp for reduction(+:result)
    for (i = 0; i < omp_get_max_threads(); ++i)
    {
        // printf("%d: %d -> %p\n", omp_get_thread_num(),res, &res);
        result += res;
    }

    assert(result == ( ( N * (N+1) ) /2) );
    // printf("ok!\n");
}
