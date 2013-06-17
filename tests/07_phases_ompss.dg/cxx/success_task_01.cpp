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

#include <cstdlib>
#include <cstdio>

template <typename T>
void copy(T* dst, T* src, int N)
{
    for(int i = 0; i < N; i++)
    {
#pragma omp task shared(dst, src)
        dst[i] = src[i];
    }
#pragma omp taskwait
}

int main(int argc, char *argv[])
{
    int a[10], b[10];

    copy(a, b, 10);

    for (int i = 0; i < 10; i++)
    {
        if (a[i] != b[i])
        {
            fprintf(stderr, "a[%d] != b[%d]\n", i, i);
            abort();
        }
    }

    return 0;
}
