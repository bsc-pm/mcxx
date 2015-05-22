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
test_CXXFLAGS=--variable=enable_input_by_value_dependences:1
</testinfo>
*/
#include<assert.h>
#include<stdlib.h>

#pragma omp task
int* task_producer(int n)
{
    int *buf = (int*) malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i)
    {
        buf[i] = i;
    }
    return buf;
}


#pragma omp task in(buf)
void task_consumer(int n, int* buf)
{
    for (int i = 0; i < n; ++i)
        assert(buf[i] == i);

}

int main()
{
    int* p = 0;
    p = task_producer(5);
    task_consumer(5, p);
#pragma omp taskwait
}
