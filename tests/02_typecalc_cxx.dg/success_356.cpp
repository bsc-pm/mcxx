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
test_ignore=yes
test_ignore_reason="convoluted friend declarations"
</testinfo>
*/
template < typename T1, typename T2>
void faa(T1, T2) {}


class C
{
    template < typename T1, typename T2>
        friend void fee(T1, T2) {}
};


template < typename T>
class D
{
    template < typename T1, typename T2>
        friend void fii(T1, T2) {}

    template < typename T1, typename T2>
        friend void foo(T, T1, T2) {}
};

int main()
{
    faa(1, 1);
    fee(1, 1);

    D<int> d;

    fii(1, 1);
    foo(1, 1, 1);
}
