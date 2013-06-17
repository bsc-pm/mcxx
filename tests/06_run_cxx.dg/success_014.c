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
test_generator=config/mercurium-run
</testinfo>
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void *global = 0;

void f(int @ref@n, int @ref@m, int (*@ref@v)[m])
{
    // fprintf(stderr, "(1) n = %d | m = %d | %p\n", n, m, &(v[n-1][m-1]));
    global = &(v[n-1][m-1]);
    v[n-1][m-1] = 42;
}

struct A
{
    int n1;
    int m1;
    void *p;
};

void g(struct A@ref@ a)
{
    f(a.n1, a.m1, (int (*@ref@)[a.m1]) a.p);
}

void h(int n, int m)
{
    struct A a;

    int b[n][m];
    memset(b, 0, sizeof(b));
    fprintf(stderr, "SIZEOF -> %zd\n", sizeof(b));

    a.n1 = n;
    a.m1 = m;
    a.p = b;

    g(a);

    // fprintf(stderr, "(2) n = %d | m = %d | %p\n\n", n, m, &(b[n-1][m-1]));

    if (b[n-1][m-1] != 42)
        abort();

    if (global != &(b[n-1][m-1]))
        abort();
}

int main(int argc, char *argv[])
{
    h(11, 21);
    h(12, 22);
    h(11, 22);
    h(23, 11);
    h(12, 22);
    h(23, 23);
    h(24, 24);

    return 0;
}
