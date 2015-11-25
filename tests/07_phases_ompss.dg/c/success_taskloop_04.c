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
#include <stdlib.h>
#include <stdio.h>

enum { N = 10000 };

char check[N] = { };

void f(int *a, int n)
{
    int i;
    #pragma omp taskloop out(a[i]) grainsize(10) shared(check) nogroup
    for (i = 0; i < n; i++)
    {
        if (check[i] != 0)
            abort();
        a[i] = i;
        check[i] = 1;
    }

#pragma omp taskloop inout(a[i]) grainsize(10) shared(check) nogroup
    for (i = 0; i < n; i++)
    {
        if (check[i] != 1)
            abort();
        a[i]++;
        check[i] = 0;
    }

#pragma omp taskloop in(a[i]) grainsize(10) shared(check)
    for (i = 0; i < n; i++)
    {
        if (check[i] != 0)
            abort();
    }
}

int w[N];

int main(int argc, char* argv[])
{
    int i;
    for (i = 0; i < N; i++)
    {
        w[i] = i;
    }

    f(w, N);

    return 0;
}
