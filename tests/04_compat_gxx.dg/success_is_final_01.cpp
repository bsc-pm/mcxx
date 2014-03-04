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
test_generator=config/mercurium-cxx11
</testinfo>
*/



struct A1 final
{
    int x;
};

// This caused an ambiguity
struct A2 final { };

struct A3
{
    int x;
};

struct A4
{
};

template <bool B1, bool B2>
struct A {
};

template <bool B1>
struct A<B1, B1> {
    typedef int Type;
};

A<__is_final(A1), true>::Type test1;
A<__is_final(A2), true>::Type test2;
A<__is_final(A3), false>::Type test3;
A<__is_final(A4), false>::Type test4;
