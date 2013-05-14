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

template <typename _T>
struct my_is_integer
{
    enum { __value = false };
};

template <>
struct my_is_integer<int>
{
    enum { __value = false };
};

template<typename _Tp, bool = my_is_integer<_Tp>::__value>
struct __promote
{ typedef double __type; };

template<typename _Tp>
struct __promote<_Tp, false>
{ typedef _Tp __type; };

template<typename _Tp, typename _Up>
struct __promote_2
{
    private:
        typedef typename __promote<_Tp>::__type __type1;
        typedef typename __promote<_Up>::__type __type2;

    public:
        typedef __typeof__(__type1() + __type2()) __type;
};

template <typename _T>
struct my_complex
{
};


template<typename _Tp, typename _Up>
struct __promote_2<my_complex<_Tp>, _Up>
{
    public:
        typedef my_complex<typename __promote_2<_Tp, _Up>::__type> __type;
};

template<typename _Tp, typename _Up>
struct __promote_2<_Tp, my_complex<_Up> >
{
    public:
        typedef my_complex<typename __promote_2<_Tp, _Up>::__type> __type;
};

template<typename _Tp, typename _Up>
struct __promote_2<my_complex<_Tp>, my_complex<_Up> >
{
    public:
        typedef my_complex<typename __promote_2<_Tp, _Up>::__type> __type;
};
