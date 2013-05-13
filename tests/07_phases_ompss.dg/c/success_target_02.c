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

#define M 20
const int m = M;

int b[M];

int main (int argc, char *argv[])
{
    int *a = b;

    {
        int k;
        for (k = 0; k < m; k++)
        {
            a[k] = -(k + 1);
        }
    }

#pragma omp task output([m] a)
    {
        int k;
        for (k = 0; k < m; k++)
        {
            a[k] = k;
        }
    }

#pragma omp taskwait

    int i;
    for (i = 0; i < m; i++)
    {
        if (a[i] != i)
        {
            fprintf(stderr, "a[%d] == %d != %d\n", i, a[i], i);
            abort();
        }
    }

    return 0;
}

