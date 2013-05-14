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

namespace std
{
typedef unsigned int size_t;
}

template< typename T > struct is_array
{ 
};

template< typename T, std::size_t N > struct is_array< T[N] > 
{ 
    typedef T A;
};

template< typename T, std::size_t N > struct is_array< T const[N] > 
{ 
    typedef T AC;
};

template< typename T, std::size_t N > struct is_array< T volatile[N] > 
{ 
    typedef T AV;
};

template< typename T, std::size_t N > struct is_array< T const volatile[N] >  
{ 
    typedef T ACV;
};

template< typename T> struct is_array< T[] > 
{ 
};

template< typename T> struct is_array< T const[] > 
{ 
};

template< typename T> struct is_array< T volatile[] > 
{ 
};

template< typename T> struct is_array< T const volatile[] >  
{ 
};

typedef is_array<int[10]>::A P;
typedef is_array<const int[10]>::AC P;
typedef is_array<volatile int[10]>::AV P;
typedef is_array<const volatile int[10]>::ACV P;
