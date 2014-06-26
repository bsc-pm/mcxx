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
test_generator=config/mercurium-omp
</testinfo>
*/

#include <stdio.h>
#include <string.h>
#include <assert.h>

void foo(int n)
{
    int v[n][n];

    memset(v, 0, sizeof(v));

    #pragma omp  parallel for firstprivate(v)
    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < n; ++j)
        {
//            printf("1. v[i][j]: %d\n", v[i][j]);
            assert(v[i][j] == 0);
            v[i][j]++;
        }
    }

    #pragma omp parallel for shared(v)
    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < n; ++j)
        {
//            printf("2. v[i][j]: %d\n", v[i][j]);
            assert(v[i][j] == 0);
            v[i][j]++;
        }
    }

    #pragma omp parallel for firstprivate(v)
    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < n; ++j)
        {
//            printf("3. v[i][j]: %d\n", v[i][j]);
            assert(v[i][j] == 1);
            v[i][j]++;
        }
    }
}


int main()
{
    foo(10);
    return 0;
}
