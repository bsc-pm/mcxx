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
template <typename T>
struct A
{
    typedef T* Q;
};

template <typename T>
struct B
{
    enum E { V = 1 };
};

template <typename S>
int f1()
{
    typedef typename A<S>::Q M1;

    return B<M1>::V;
}

template <typename S>
int f2()
{
    typedef typename A<S>::Q M2;

    return B<M2>::V;
}

template <typename S>
int f3()
{
    typedef typename A<S>::Q M1;
    typename B<M1>::V x1;
}

template <typename S>
int f4()
{
    typedef typename A<S>::Q M2;
    struct B<M2>::V x2;
}
