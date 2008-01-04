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
