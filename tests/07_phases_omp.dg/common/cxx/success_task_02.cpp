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

extern "C" int fib(int a)
{
    if (a == 0 || a == 1)
    {
        return 1;
    }
    else
    {
        int n1 = 0, n2 = 0;
#pragma omp task firstprivate(a) shared(n1)
        n1 = fib(a - 1);
#pragma omp task firstprivate(a) shared(n2)
        n2 = fib(a - 2);
#pragma omp taskwait
        return n1 + n2;
    }
}

int fibo_seq[] = { 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, -1 };

int main(int argc, char *argv[])
{
    int i = 0;

    while (fibo_seq[i] > 0)
    {
        fprintf(stderr, "Computing fibonacci %d\n", i);

        int c;
#pragma omp parallel
#pragma omp single
        {
            c = fib(i);
        }

        if (c != fibo_seq[i])
        {
            fprintf(stderr, "fib(%d) == %d != %d\n", i, c, fibo_seq[i]);
            abort();
        }

        i++;
    }

    return 0;
}
