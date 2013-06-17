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

typedef long unsigned int size_t;
typedef long int ptrdiff_t;


template<class T>
struct prime_list_template
{
    static size_t const value[];
    static ptrdiff_t const length;
};



template<class T>
size_t const prime_list_template<T>::value[] =
{ 
    1610612741ul, 3221225473ul, 4294967291ul
};

template<class T>
ptrdiff_t const prime_list_template<T>::length = 40;





template < typename T>
struct A
{
    struct B
    {
        static size_t const value[];
    };
};


template < typename T>
size_t const A<T>::B::value[] =
{
    1, 2, 3
};

