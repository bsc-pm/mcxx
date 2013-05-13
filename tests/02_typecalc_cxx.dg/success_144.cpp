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
struct type_trait
{
    typedef _T* pointer;
};

template <class T_base, class T_derived>
struct is_base_and_derived
{
private:
  struct big {
    char memory[64];
  };
  static big is_base_class_(...);
  static char is_base_class_(typename type_trait<T_base>::pointer);
public:
  static const bool value =
    sizeof(is_base_class_(reinterpret_cast<typename type_trait<T_derived>::pointer>(0))) ==
    sizeof(char);
  void avoid_gcc3_warning_();
};

struct A
{
};

struct B : A
{
};

template <bool _B>
struct test { };

template <>
struct test<true>
{ 
    typedef int T;
};

typedef test<is_base_and_derived<A, B>::value>::T P;
typedef int P;
