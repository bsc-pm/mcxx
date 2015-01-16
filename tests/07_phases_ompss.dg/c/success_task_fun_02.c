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
test_CFLAGS="--no-copy-deps"
</testinfo>
*/

#include <stdlib.h>

#pragma omp task inout(*a)
void foo1(int *a)
{
    *a = 3 + *a;
}

#pragma omp task inout([1] a)
void foo2(int *a)
{
    a[0] = 3 + a[0];
}

#pragma omp task inout(a[3;1])
void foo3(int *a)
{
    a[3] = a[3] + 10;
    a[4] = a[4] * 12;
}

#pragma omp task inout(([10] a)[3;1])
void foo4(int *a)
{
    a[3] = a[3] + 10;
    a[4] = a[4] * 12;
}

int main(int argc, char *argv[])
{
    int a[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

    foo1(a);
#pragma omp taskwait
    if (a[0] != 3)
        abort();
    a[0] = 0;

    foo2(a);
#pragma omp taskwait
    if (a[0] != 3)
        abort();
    a[0] = 0;

    foo3(a);
#pragma omp taskwait
    if (a[3] != 13
            || a[4] != 48)
        abort();
    a[3] = 3;
    a[4] = 4;

    foo4(a);
#pragma omp taskwait
    if (a[3] != 13
            || a[4] != 48)
        abort();

    return 0;
}
