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
test_generator=config/mercurium-serial-simd
</testinfo>
*/

#include <stdio.h>

int __attribute__((noinline)) test (int sizey,
        int *u)
{
    int sum =0;
    int j;

#pragma omp simd reduction(+:sum) suitable(sizey)
                for (j=1; j <= sizey-2; j++)
                {
                    sum += u[j];
                }

    return sum;
}


int main( int argc, char *argv[] )
{
    int i, result;
    int a[101];

    for(i=0; i<101; i++)
    {
        a[i] = 1;
    }

    result = test(96, a);

    if (result != 94)
    {
        fprintf(stderr, "Error: result %d != 96\n", result);
        return 1;
    }

    result = test(64, a);

    if (result != 62)
    {
        fprintf(stderr, "Error: result %d != 62\n", result);
        return 1;
    }

    result = test(16, a);

    if (result != 14)
    {
        fprintf(stderr, "Error: result %d != 14\n", result);
        return 1;
    }

    result = test(32, a);

    if (result != 30)
    {
        fprintf(stderr, "Error: result %d != 30\n", result);
        return 1;
    }

    return 0;
}
