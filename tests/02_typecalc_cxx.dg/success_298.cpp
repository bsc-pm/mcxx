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
{
    explicit A(int t)
    {
    }
};

template < typename _T >
struct B
{
    explicit B(_T t)
    {
    }
};

void foo()
{
    A _a1(1);
    A _a2 = A(2);
    A _a3 = (A) 3;
    A _a4 = static_cast<A>(4);
    A *_a5 = new A(5);

    B < int > _b1(1);
    B < int > _b2 = B<int>(2);
    B < int > _b3 = (B<int>) 3;
    B < int > _b4 = static_cast< B < int > >(4); 
    B < int > *_b5 = new B < int >(4); 
};
