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
test_exec_faulty=config/mercurium-ompss
</testinfo>
*/


#include<assert.h>

struct C
{
    int n;
};

struct B
{
    C * c;
    int n;
};

struct A
{
    int n;
    B* b;

#pragma omp task inout(n,b->n, b->c->n)
    void f()
    {
        n++;
        b->n++;
        b->c->n++;
    }
};

int main()
{
    A a;
    B b;
    C c;
    c.n = 1;
    b.n = 2;
    b.c = &c;

    a.n = 3;
    a.b = &b;
    a.f();
    #pragma omp taskwait
    assert(a.n == 4);
    assert(a.b->n == 3);
    assert(a.b->c->n == 2);
}
