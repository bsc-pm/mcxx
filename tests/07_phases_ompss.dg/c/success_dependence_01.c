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
test_ENV="PERISH_TIMEOUT_MINUTES=5"
</testinfo>
*/
#include<unistd.h>
#include<assert.h>

#define N 10
#define M 100

struct A
{
    int x[M];
};

int main()
{
    struct A a[N];
    int i, j;
    for (i = 0; i < N; ++i)
    {
        for (j = 0; j < M; ++j)
        {
            a[i].x[j] = -1;
        }
    }

    int z = 3, ub = 10, lb = 0;
    int *p = &(a[z].x[lb]);
    #pragma omp task out(a[z].x[lb:ub])
    {
        sleep(1);
        int k;
        for (k = lb; k <= ub; k++)
            a[z].x[k] = 2;
    }

    #pragma omp task in(*p)
    {
        int i;
        for (i = 0; i <= (ub - lb); i++)
            assert(p[i] == 2);
    }
    #pragma omp taskwait
    return 0;
}
