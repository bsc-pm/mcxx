/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
test_generator=config/mercurium-ss2omp
</testinfo>
*/

#include <stdlib.h>

#pragma css task input(n) inout(m)
void f(int n, int m[n][n])
{
    int i, j;
    for (i = 0; i < n; i++)
    {
        for (j = 0; j < n; j++)
        {
            m[i][j]++;
        }
    }
}

void g(int m, int a[m][m])
{
    f(m, a);
}

int main(int argc, char *argv[])
{
    int k = 10;

    int a[k][k];

    int i, j;
    for (i = 0; i < k; i++)
    {
        for (j = 0; j < k; j++)
        {
            a[i][j] = i + j;
        }
    }

    g(k, a);
#pragma omp taskwait

    for (i = 0; i < k; i++)
    {
        for (j = 0; j < k; j++)
        {
            if (a[i][j] != (i + j + 1)) abort();
        }
    }

    return 0;
}
