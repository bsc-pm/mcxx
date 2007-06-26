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
template <int _N>
struct A
{
};

template <typename _T>
struct B
{
    typedef int K;
};

template <int _N>
struct B<A<_N + 1> >
{
    typedef int K;
};

template <int _N>
struct B<A<_N> >
{
    typedef float K;
};

template <int _Q>
struct C : A<_Q>
{
};

typedef B<A<3> >::K P1;
typedef float P1;

typedef B<float>::K P2;
typedef int P2;
