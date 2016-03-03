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
test_generator="config/mercurium-ompss"
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcc=yes
test_CFLAGS=--variable=enable_input_by_value_dependences:1
</testinfo>
*/
#include<assert.h>

#pragma omp task in(x)
void consumer(int x)
{
    assert(x == 2);
}
#pragma omp task out(*x)
void producer(int*x)
{
    *x = 1;
}

int main()
{
    int x = -1;
    int v[5] = {0, 1, 2, 3, 4};

    producer(&x);

    // This taskwait is needed because the following task call introduces a
    // dependence over v[x] and for compute this dependence Mercurium needs
    // the right value of x.
    #pragma omp taskwait on(x)

    consumer(v[x] + x);
#pragma omp taskwait
    assert(x == 1);
}
