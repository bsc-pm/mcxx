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
test_CXXFLAGS="--instantiate"
</testinfo>
*/
template <typename T>
void m(T t)
{
    T p;
    p = t;
}

template <typename T>
struct A
{
    void f(T t)
    {
        T p;
        p = t;
    }

    void g(T t);

    template <typename Q>
        void h(T t, Q q)
        {
            T p1;
            Q p2;

            p1 = t;
            p2 = q;
        }

    template <typename Q>
        void i(T, Q);
};

template <typename T1>
void A<T1>::g(T1 t)
{
    T1 p;
    p = t;
}

template <typename T1>
template <typename Q1>
void A<T1>::i(T1 t, Q1 q)
{
    T1 p1;
    Q1 p2;

    p1 = t;
    p2 = q;
}

void h()
{
    m(3);
    m(4.0);

    A<int> a;

    a.f(3);
    a.f(4.0);

    a.g(3);
    a.g(4.0);

    a.h(3, 3);
    a.h(3, 4.0);

    a.i(3, 3);
    a.i(3, 4.0);
}
