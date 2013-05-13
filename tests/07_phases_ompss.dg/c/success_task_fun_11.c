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

#pragma omp target device(smp) copy_deps
#pragma omp task inout(a[0;10])
void f(int *a)
{
    int i;
    for (i = 0; i < 10; i++)
        a[i]++;
}

int main(int argc, char *argv[])
{
    int v[10];

    int i;
    for (i = 0; i < 10; i++)
        v[i] = i;

    f(v);
#pragma omp taskwait

    for (i = 0; i < 10; i++)
    {
        if (v[i] != (i+1))
            abort();
    }

    return 0;
}
