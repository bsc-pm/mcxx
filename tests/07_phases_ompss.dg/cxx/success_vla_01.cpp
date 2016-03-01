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
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcxx=yes
</testinfo>
*/
#include <stdlib.h>

void f(int n)
{
    if (n <= 0)
        return;

    int x[n][n];
    int y[n][n];

    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            x[i][j] = j - i;
            y[i][j] = j - i;
        }
    }

#pragma omp task shared(x) firstprivate(y) no_copy_deps
    {
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < n; j++)
            {
                x[i][j] *= 2;
                y[i][j] *= 2;
            }
        }
    }

#pragma omp taskwait

    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            if (x[i][j] != (2*(j - i)))
            {
                abort();
            }
            if (y[i][j] != (j - i))
            {
                abort();
            }
        }
    }
}

int main(int argc, char *argv[])
{
    f(10);

    return 0;
}
