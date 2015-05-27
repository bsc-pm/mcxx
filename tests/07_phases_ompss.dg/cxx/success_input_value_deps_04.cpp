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
test_CXXFLAGS=--variable=enable_input_by_value_dependences:1
test_compile_faulty=yes
test_compile_fail=yes
</testinfo>
*/
#include<assert.h>

#pragma omp task out(*z)
void produce_z(int*z)
{
    for (int i = 0; i < 1000; i++)
    {
        for(int j = 0; j < 1000; ++j)
        {
        }
    }
    *z = 2;
}

#pragma omp task out(*y)
void produce_y(int*y)
{
for (int i = 0; i < 1000; i++)
    {
        for(int j = 0; j < 1000; ++j)
        {
        }
    }
    *y = 1;
}

#pragma omp task out([3]v)
void produce_v(int* v)
{
    v[0] = 0;
    v[1] = 1;
    v[2] = 2;
}

#pragma omp task in(n) inout([n]arr)
void consumer(int n, int* arr)
{
    assert(n == 3);
    assert(arr[0] == 0);
    assert(arr[1] == 1);
    assert(arr[2] == 2);

}

int main()
{
    int y = -1, z = -1;
    int v[3] = { -1 };
    produce_y(&y);
    produce_z(&z);
    produce_v(v);
    consumer(z + y, v);
#pragma omp taskwait

    assert(y == 1);
    assert(z == 2);

    assert(v[0] == 0);
    assert(v[1] == 1);
    assert(v[2] == 2);
}
