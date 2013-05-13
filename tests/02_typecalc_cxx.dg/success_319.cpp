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


template < bool a1, bool a2, bool a3 >
struct A;

template < bool b1, bool b2, bool b3 >
struct A
{

};
A < true, true, true > a1;
//A < true, true > a2; error
//A < true> a3;        error
//A <> a4;             error

template < bool c1, bool c2, bool c3 = true >
struct A;
A < true, true, true > b1;
A < true, true > b2;
//A < true> b3;        error
//A <> b4;             error


template < bool d1, bool d2 = true, bool d3 >
struct A;
A < true, true, true > c1;
A < true, true > c2;
A < true> c3;
//A <> c4;            error


template < bool e1 = true, bool e2, bool e3 >
struct A;
A < true, true, true > d1;
A < true, true > d2;
A < true> d3;
A <> d4;
