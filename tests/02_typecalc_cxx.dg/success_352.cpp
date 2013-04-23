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

template<typename _Tp>
struct complex
{
    _Tp _M_real;
    _Tp _M_imag;

    complex(const _Tp& __r = _Tp(), const _Tp& __i = _Tp())
        : _M_real(__r), _M_imag(__i) { }
};

namespace N
{
    template < typename _T1, typename _T2>
        struct __my_promote_2
        {
            typedef complex<double> __type;
        };

    template < bool b, typename _T >
        struct __my_enable_if
        {
            typedef _T __type;
        };
}

template < typename _T>
struct __my_is_arithmetic
{
    enum { __value = true };
};


namespace my_std
{
    template<typename _Tp, typename _Up>
        inline
        typename N::__my_promote_2<
        typename N::__my_enable_if<__my_is_arithmetic<_Tp>::__value
        && __my_is_arithmetic<_Up>::__value,
        _Tp>::__type, _Up>::__type
            my_pow(_Tp __x, _Up __y)
            {
            }
}

void foo()
{
    complex<double> a(1.0,2.0);
    complex<double> b(1.5,0.5);
    complex<double> c;


    c = my_std::my_pow(a,b);
}
