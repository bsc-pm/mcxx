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
test_generator=config/mercurium-ompss
</testinfo>
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#pragma omp task
void f(int n, int m, int v[n][m]);

int g(int n1, int m1)
{
    int v[n1][m1];

    memset(v, 0, sizeof(v));

    fprintf(stderr, "g: %p -> [%d][%d] -> %p\n", v, n1-1, m1-1, &v[n1-1][m1-1]);

    f(n1, m1, v);
#pragma omp taskwait
    if (v[n1-1][m1-1] != 42)
    {
        fprintf(stderr, "%d != 42\n", v[n1-1][m1-1]);
        abort();
    }
}

void f(int n1, int m1, int v[n1][m1])
{
    fprintf(stderr, "f: %p -> [%d][%d] -> %p\n", v, n1-1, m1-1, &v[n1-1][m1-1]);
    v[n1-1][m1-1] = 42;
}

int main(int argc, char* argv[])
{
    g(10, 20);
    return 0;
}
