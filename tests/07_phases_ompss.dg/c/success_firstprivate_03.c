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

#include<assert.h>

int v(int n)
{
    int v1[n];
    int v2[n];
    for (int i = 0; i < n; ++i)
    {
        v1[i] = 0;
        v2[i] = 0;
    }

    #pragma omp task firstprivate(v1, v2)
    {
        for (int i = 0; i < n; ++i)
        {
            v1[i]++;
            v2[i]++;
        }
    }
    #pragma omp taskwait
    for (int i = 0; i < n; ++i)
    {
        assert(v1[i] == 0);
        assert(v2[i] == 0);
    }
}

int main(int argc, char*argv[])
{
    v(1000);
    return 0;
}
