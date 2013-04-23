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
test_generator=config/mercurium-omp
</testinfo>
*/

#include <stdlib.h>
#include <string.h>

#define N 1024

int a[N];

int main(int argc, char *argv[])
{
    int i;
    int BS = 64;

    memset(a, 0, sizeof(a));

#pragma omp task shared(a)
    {
#pragma omp for
        for (i = 0; i < N; i += BS)
        {
            int j;
            for (j = i; j < i+ BS; j++)
            {
                a[j] = i;
            }
        }
    }
#pragma omp taskwait

    for (i = 0; i < N; i += BS)
    {
        int j;
        for (j = i; j < i+ BS; j++)
        {
            if (a[j] != i) abort();
        }
    }

    return 0;
}
