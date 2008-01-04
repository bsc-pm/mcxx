/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
