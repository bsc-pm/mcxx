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

#if ((__GNUC__ < 4) \
        || (( __GNUC__ == 4) && __GNUC_MINOR__ < 6))
    #define IGNORE_TEST
#endif

#ifndef IGNORE_TEST
template <int N>
struct A
{
    static int x __attribute__((aligned(N)));
    static int __attribute__((aligned(64))) (__attribute__((aligned(16))) y), z;
};

template <int N, int M>
struct EqualTest { };

template <int N>
struct EqualTest<N, N> { typedef int Test; };

void foo()
{
    /* 1 */ EqualTest<32, __alignof__(A<32>::x)>::Test t1;
    /* 2 */ EqualTest<4, __alignof__(__typeof__(A<32>::x))>::Test t2;

    /* 3 */ EqualTest<16, __alignof__(__typeof__(A<32>::y))>::Test t3;
    /* 4 */ EqualTest<4, __alignof__(__typeof__(A<32>::z))>::Test t4;

    /* 5 */ EqualTest<64, __alignof__(A<32>::y)>::Test t5;
    /* 6 */ EqualTest<64, __alignof__(A<32>::z)>::Test t6;
}
#endif
