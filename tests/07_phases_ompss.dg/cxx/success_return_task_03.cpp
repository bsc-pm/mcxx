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
test_CXXFLAGS="--variable=enable_input_by_value_dependences:1 --variable=enable_nonvoid_function_tasks:1"
</testinfo>
*/
#include<assert.h>

#pragma omp task in(x)
int foo(int x)
{
    return x + 1;
}

int bar(int x)
{
    return x + 7;
}

int main()
{
    int valor = 0;
    int x = 0;

    x = foo(foo(valor));

#pragma omp taskwait

    assert(x == 2);

    x = bar(foo(valor));

#pragma omp taskwait
    assert(x == 8);
}
