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
test_generator=config/mercurium-omp

test_exec_fail_nanox_2thread=yes
test_exec_faulty_nanox_2thread=yes
test_exec_fail_nanox_4thread=yes
test_exec_faulty_nanox_4thread=yes
</testinfo>
*/
#include <stdlib.h>
#include "omp.h"

int d;
#pragma omp threadprivate(d)

void f1(void)
{
    d = 10;
    int *p = &d;
#pragma omp parallel copyin(d) firstprivate(p)
    {
        if (p == &d
                && omp_get_thread_num() != 0)
            abort();

        if (d != 10)
            abort();
    }
}

int c[2];
#pragma omp threadprivate(c)

void f2(void)
{
    int *p = c;
    c[1] = 10;
#pragma omp parallel copyin(c) firstprivate(p)
    {
        if (p == c
                && omp_get_thread_num() != 0)
            abort();

        if (c[1] != 10)
            abort();
    }
}

int main(int argc, char *argv[])
{
    f1();
    f2();
    return 0;
}
