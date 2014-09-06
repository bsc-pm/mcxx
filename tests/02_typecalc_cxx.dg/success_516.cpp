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
struct empty;

template < int N, typename T0 = empty, typename T1 = empty, typename T2 = empty,
                  typename T3 = empty, typename T4 = empty, typename T5 = empty >
struct Helper;


template < int N >
struct Helper< N, empty, empty, empty,
                  empty, empty, empty >
{
    typedef void ElementType;
};

template < int N, typename T0, typename T1, typename T2,
                  typename T3, typename T4, typename T5 >
struct Helper : Helper<N+1, T1, T2, T3, T4, T5>
{
    typedef T0 &ElementType;
    T0 datum;
};


template < typename T0 = empty, typename T1 = empty, typename T2 = empty,
           typename T3 = empty, typename T4 = empty, typename T5 = empty >
struct MyTuple : Helper<0, T0, T1, T2, T3, T4, T5>
{
};

template < int N,
           typename T0, typename T1, typename T2,
           typename T3, typename T4, typename T5 >
typename Helper<N, T0, T1, T2, T3, T4, T5>::ElementType get(Helper<N, T0, T1, T2, T3, T4, T5> &h)
{
    return h.datum;
}

void foo()
{
    MyTuple<int, float, double> m;
    Helper<0, int, float, double> h1;

    int& m0 = get<0>(m);
    float& m2 = get<1>(m);
    double& m3 = get<2>(m);
}
