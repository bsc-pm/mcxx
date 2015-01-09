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

void f(int *x, int n)
{

    int i;
    for (i = 0; i < n; i++)
    {
#pragma omp task depend(out : x[i]) firstprivate(i)
        {
            x[i] = i;
        }
#pragma omp task depend(inout : x[i]) firstprivate(i)
        {
            if (x[i] != i)
            {
                abort();
            }
            x[i]++;
        }
#pragma omp task depend(in : x[i]) firstprivate(i)
        {
            if (x[i] != (i+1))
            {
                abort();
            }
        }
    }

#pragma omp taskwait
}

int main(int argc, char *argv[])
{

    #pragma omp parallel
    {
        #pragma omp single
        {
            int c[100];
            f(c, 100);
        }
    }

    return 0;
}
