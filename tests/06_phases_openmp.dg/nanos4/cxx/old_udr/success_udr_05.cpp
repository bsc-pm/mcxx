/*
<testinfo>
test_generator=config/mercurium-nanos4
test_CXXFLAGS=--variable=new_udr:0
</testinfo>
*/
/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

struct my_int
{
    int _n;
    my_int() : _n ( 0 ) { }
    my_int(int n) : _n(n) { }
};

void fun(my_int*, my_int*);

#pragma omp declare reduction(fun:my_int) identity(constructor)

void g()
{
    my_int s;

#pragma omp parallel for reduction(fun : s)
    for (int i = 0; i < 100; i++)
    {
        my_int k(i);
        fun(&s, &k);
    }
}
