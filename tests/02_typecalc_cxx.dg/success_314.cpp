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

template <bool x> struct STATIC_ASSERTION_FAILURE;
template <> struct STATIC_ASSERTION_FAILURE<true> { enum { value = 1 }; };
template<int x> struct static_assert_test{};

template <typename A, typename B>
struct is_interoperable
{
    static const int value = 1;
};


template < typename Derived1, typename Derived2>
void foo()
{
    typedef static_assert_test< sizeof(STATIC_ASSERTION_FAILURE<is_interoperable<Derived1, Derived2>::value == 0 ? false : true>)> blah;
}

template <typename A, typename B>
struct is_convertible
{
    static const int value = 1;
};

template < typename Boo1>
void bar()
{
    typedef static_assert_test< sizeof(STATIC_ASSERTION_FAILURE<is_convertible<Boo1, Boo1>::value == 0 ? false : true>)> blah;
}
