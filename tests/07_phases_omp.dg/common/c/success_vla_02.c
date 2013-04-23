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

#include <stdlib.h>
#include <stdio.h>

void f(void)
{
    int n = 10, m = 20;
    int v[n + 1][m * 2];

#pragma omp task shared(v)
    {
        v[n-1][m-1] = 3;
    }
#pragma omp taskwait

    if (v[9][19] != 3)
    {
        fprintf(stderr, "v[9][19] != 3\n");
        abort();
    }
}

void g(void)
{
    int n = 10, m = 20;
    int v[n + 1][m * 2];

#pragma omp parallel shared(v) firstprivate(n, m)
    {
        v[n-1][m-1] = 4;
    }

    if (v[9][19] != 4)
    {
        fprintf(stderr, "v[9][19] != 4\n");
        abort();
    }
}

int main(int argc, char *argv[])
{
    f();
    g();

    return 0;
}
