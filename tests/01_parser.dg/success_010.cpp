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

template <class T>
struct A
{
    typedef int P;
};

template <class T>
struct A<T*>
{
    typedef float P;
};

template <class T>
struct A<T(*)()>
{
    typedef char P;
};

template <class T, class Q>
struct A<T(*)(Q)>
{
    typedef Q P;
};

typedef int T1;
typedef A<int>::P T1;
typedef A<float>::P T1;
typedef A<char>::P T1;
typedef A<double>::P T1;
typedef A<wchar_t>::P T1;
typedef A<bool>::P T1;
typedef A<A<int> >::P T1;

typedef float T2;
typedef A<int*>::P T2;
typedef A<float*>::P T2;
typedef A<char*>::P T2;
typedef A<double*>::P T2;
typedef A<wchar_t*>::P T2;
typedef A<bool*>::P T2;
typedef A<A<int>* >::P T2;

typedef char T3;
typedef A<int(*)()>::P T3;
typedef A<float(*)()>::P T3;
typedef A<char(*)()>::P T3;
typedef A<double(*)()>::P T3;
typedef A<wchar_t(*)()>::P T3;
typedef A<bool(*)()>::P T3;
typedef A<A<int>(*)()>::P T3;

typedef A<int(*)(int)>::P T1;
typedef A<int(*)(float)>::P T2;
typedef A<int(*)(char)>::P T3;
