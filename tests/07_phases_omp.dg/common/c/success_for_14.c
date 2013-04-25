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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ELEMS 100

int main(int argc, char *argv[])
{
    int c[100 + MAX_ELEMS + 100];
    int i;

    memset(c, 0, sizeof(c));

#pragma omp for
    for (i = MAX_ELEMS; i > 0; i--)
    {
        c[i + 100 - 1] = i - 1;
    }

    for (i = 0; i < MAX_ELEMS; i++)
    {
        if (c[i + 100] != i)
        {
            fprintf(stderr, "c[%d] == %d != %d\n", i + 100, c[i + 100], i);
            abort();
        }
    }
    for (i = 0; i < 100; i++)
    {
        if (c[i] != 0)
        {
            fprintf(stderr, "c[%d] == %d != %d\n", i, c[i], 0);
            abort();
        }
        if (c[100 + MAX_ELEMS + i] != 0)
        {
            fprintf(stderr, "c[%d] == %d != %d\n", 100 + MAX_ELEMS + i, c[100 + MAX_ELEMS + i], 0);
            abort();
        }
    }


    return 0;
}
