/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
template <typename _T5, typename _S5, typename _P5>
struct A
{
    typedef _P5 T;
};

template <typename _T1, typename _S1, typename _P1>
struct A;

template <typename _T2, typename _S2, typename _P2 = _S2*>
struct A;

template <typename _T3, typename _S3 = _T3*, typename _P3>
struct A;

template <typename _T4 = int, typename _S4, typename _P4>
struct A;

A<> t;

A<float> t2;
