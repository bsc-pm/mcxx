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

#if !defined(__ICC) || (__ICC >= 1400)

#include <stdlib.h>
#include <stdio.h>

void foo (unsigned sizex, unsigned sizey, int u[sizex][sizey])
{
    int (*check)[sizey] = calloc(sizex * sizey, sizeof(check[0][0]));

    int i, j;
    for (i = 0; i < sizex; i++)
    {
        for (j = 0; j < sizey; j++)
        {
            u[i][j] = j - i;
            check[i][j] = j - i;
        }
    }

    for (i = sizex-1; i >= 1; i--)
    {
        for (j = sizey-1; j >= 1; j--)
        {
#pragma omp task in(u[i-1][j-1]) inout(u[i][j])
            {
                u[i][j] += u[i-1][j-1];
            }
        }
    }
#pragma omp taskwait

    for (i = 1; j < sizex; i++)
    {
        for (j = 1; j < sizey; j++)
        {
            if (u[i][j] != (check[i][j] + check[i-1][j-1]))
            {
                fprintf(stderr, "%d != %d\n", u[i][j], (check[i][j] + check[i-1][j-1]));
                abort();
            }
        }
    }

    free(check);
}

int main(int argc, char *argv[])
{
    int (*k)[10] = calloc(10 * 10, sizeof(*k));

    foo(10, 10, k);

    free(k);

    return 0;
}

#else
// ICC <14.0 miscompiles VLA address calculations
int main(int argc, char *argv[])
{
    return 0;
}
#endif
