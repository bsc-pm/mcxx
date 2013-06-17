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

struct Test 
{
        static int n;
        static int m;
        int x; 
        #pragma omp task inout(n,m,x,s)
        void foo(int & s) 
        {
            n++;
            m++;
            x++;
            s++;
        }

    
};
void inc(int * ptr) 
{
    (*ptr)++;
}
int Test::n = 1;
int Test::m = 1;

int main() 
{
    Test a,b;
    int * ptr_n = &(Test::n);


    assert(Test::n == 1 && Test::m == 1);
    a.foo(Test::m);    
#pragma omp taskwait
    assert(Test::n == 2 && Test::m == 3);
    b.foo(Test::m);    
#pragma omp taskwait
    assert(Test::n == 3 && Test::m == 5);
    inc(ptr_n);
    assert(Test::n == 4 && Test::m == 5);
}
