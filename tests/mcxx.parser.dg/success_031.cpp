/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
template <class _T1>
struct A
{
	enum { __value = false };
	typedef _T1 T1;
};

template <>
struct A<int>
{
	enum { __value = true };
	typedef int *T1;
};

template<
          typename _T2, 
          typename _S = typename A<_T2>::T1, 
          bool _B = A<_T2>::__value
        >
struct M
{
	typedef _S S;
};

typedef M<int>::S P;
typedef int *P;
