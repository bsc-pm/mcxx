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
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_CXXFLAGS=--variable=enable_nonvoid_function_tasks:1
test_compile_fail_nanos6_mercurium=yes
test_compile_end_signal_nanos6_mercurium=yes
test_compile_fail_nanos6_imcxx=yes
test_compile_end_signal_nanos6_imcxx=yes
</testinfo>
*/
#include<assert.h>

#pragma omp task
int foo()
{
    return 2;
}
#pragma omp task
int bar()
{
    int x = (1) ? foo() : 1;
#pragma omp taskwait on(x)
    return x;
}


int main()
{
    int x = (1) ? bar() : 0;

#pragma omp taskwait

    assert(x == 2);
}
