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
test_nolink=yes
</testinfo>
*/

#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[])
{
    int a[5] = { 1, 2, 3, 4, 5 };
    int i;

#pragma omp task inout(a[2])
    a[2]++;

#pragma omp task input(a[2])
    {
        // { 1, 2, 4, 4, 5 }
        if (a[2] != 4)
        {
            abort();
        }
    }

#pragma omp task inout([5] a)
    {
        for ( i = 0; i < 5; i ++ )
            a[i]++;
    }

#pragma omp task input([5] a)
    {
        int b[5] = { 2, 3, 5, 5, 6 };
        for ( i = 0; i < 5; i ++ )
        {
            if (a[i] != b[i])
            {
                abort();
            }
        }
    }

#pragma omp taskwait

    return 0;
}
