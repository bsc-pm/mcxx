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

template < bool b1, bool b2, bool b3, bool b4 = false >
struct A;

template < bool c1, bool c2, bool c3 = false, bool c4 >
struct A;

template < bool d1, bool d2 = false, bool d3, bool d4 >
struct A;



template < bool b1, bool b2, bool b3, bool b4 = false >
struct B;

template < bool c1, bool c2, bool c3 = false, bool c4 >
struct B;

template < bool d1, bool d2 = false, bool d3, bool d4 >
struct B;

template < bool e1 = false , bool e2, bool e3, bool e4 >
struct B
{
    static const bool b = true;
};



template < int i = 1 >
struct C
{
    template < int j = 2 >
    struct D
    {

    };
};



template < int i = 1 >
class E
{
    template < int j = 2 >
    class F;
};

template <int i>
template <int j>
class E<i>::F
{};
