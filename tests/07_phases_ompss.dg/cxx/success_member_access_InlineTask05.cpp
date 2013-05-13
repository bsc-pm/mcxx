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
</testinfo>
*/

#include<assert.h>


struct A
{
    int m,n;

    A(int _n, int _m) : n(_n), m(_m) { }


    void foo(int &a, int &b)
    {
        //d must be firstprivate 
        int d = 0;
        #pragma omp task inout(a,b) 
        {
            d = 1;
            a+=d;
            b+=d;
            n+=d;
            m+=d;
        }
        #pragma omp taskwait
        assert(d == 0);

    }
};

void foo()
{
    A a(1, 2);

    int n1 = 3, m1 = 4;

    a.foo(n1, m1);
    //#pragma omp taskwait
    assert(n1 == 4);
    assert(m1 == 5);
    assert(a.n == 2);
    assert(a.m == 3);
}

int main() 
{
    foo();    
}
