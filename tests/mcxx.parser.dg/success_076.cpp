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
template <int _N1, typename _T>
struct A
{
};

template <int _N2>
struct A<_N2, int>
{
    typedef int T1;
};

template <int _N3>
struct A<_N3, const int>
{
    typedef int T2;
};

template <int _N4>
struct A<_N4, const volatile int>
{
    typedef int T3;
};

A<3, int>::T1 t1;
A<3, const int>::T2 t2;
A<3, const volatile int>::T3 t3;
