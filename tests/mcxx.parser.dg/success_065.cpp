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
namespace NS2
{
    template <typename _T, typename _Q>
    struct D
    {
    };
}

namespace NS1
{
    template <typename _T, int _N>
    struct C
    {
    };

    template <typename _T, typename _Q>
    struct F
    {
    };
    
    template <>
    struct F<int, float>
    {
        template <typename _T, typename _Q>
        struct B : C<
                     typename NS2::D<
                                typename _T::T, 
                                typename _Q::T
                               >::T,
                     (_T::V + _Q::V)
                    >
        {
        };
    };
    
    template <typename _T, typename _Q>
    struct G
    {
    };
    
    template <>
    struct G<int, float>
    {
        template <typename _T, typename _Q>
        struct B : C<
                     typename NS2::D<
                                typename _T::T, 
                                typename _Q::T
                               >::T,
                     (_T::V - _Q::V)
                    >
        {
        };
    };
}
