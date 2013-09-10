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

#ifndef __ICC

#include <stdlib.h>

void f(int N1, int N2, int a[N1][N2])
{
    int i;
#pragma omp parallel for
    for (i = 0; i < N1; i++)
    {
        int j;
        for (j = 0; j < N2; j++)
        {
            a[i][j] = i - j;
        }
    }
}

int main(int argc, char* argv[])
{
    int a[10][20];

    f(10, 20, a);

    int i;
    for (i = 0; i < 10; i++)
    {
        int j;
        for (j = 0; j < 20; j++)
        {
            if (a[i][j] != (i-j))
            {
                abort();
            }
        }
    }

    return 0;
}

#else

int main(int argc, char* argv[])
{
    return 0;
}

#endif
