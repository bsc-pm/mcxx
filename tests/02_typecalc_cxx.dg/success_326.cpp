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
test_generator=config/mercurium
</testinfo>
*/

struct A
{};

template < typename _T >
struct C
{
    C(_T y);
};

template < typename _T >
struct B
{
    B(C<_T>* c);
};

struct D
{
    D(C<A>* a);
};

void bar()
{
    A* a1 = new A;
    A* a2(new A);
    A* a3 = new A();
    A* a4 (new A());

    int *t = new int();

    D* d = new D(new C<A>(*a1));
}

template < typename _T >
void foo()
{
    _T x;

    A* a1 = new A;
    A* a2(new A);
    A* a3 = new A();
    A* a4 (new A());

    C<_T>* ptrC1 = new C<_T>(x);
    C<_T>* ptrC2(new C<_T>(x));

    B<_T> b1 = B<_T>(new C<_T>(x));
    B<_T> b2 (B<_T>(new C<_T>(x)));
    int *t = new int();
}
