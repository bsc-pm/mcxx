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

struct __false_type { };
struct __true_type { };

template<bool>
struct __truth_type
{ typedef __false_type __type; };

template<>
struct __truth_type<true>
{ typedef __true_type __type; };



template<class _Sp, class _Tp>
struct __traitor
{
    enum { __value = bool(_Sp::__value) || bool(_Tp::__value) };
    typedef typename __truth_type<__value>::__type __type;
};

struct A
{
    enum { __value = true };
};

struct B
{
    enum { __value = false };
};

typedef __true_type C;
typedef __traitor<A, B>::__type C;
typedef __traitor<A, A>::__type C;

typedef __false_type D;
typedef __traitor<B, B>::__type D;
