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

typedef unsigned int size_t;
typedef unsigned int ptrdiff_t;

namespace std
{

template<typename _Alloc>
  class allocator;

template<class _CharT>
  struct char_traits
  {
      typedef _CharT char_type;
  };

template<>
    struct char_traits<char>
    {
        typedef char char_type;
        typedef int int_type;
    };

template<typename _CharT, typename _Traits = char_traits<_CharT>,
         typename _Alloc = allocator<_CharT> >
  class basic_string;


typedef basic_string<char> string;

namespace __gnu_cxx
{
  template<typename _Tp>
    class new_allocator
    {
    public:
      typedef size_t size_type;
      typedef ptrdiff_t difference_type;
      typedef _Tp* pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp& reference;
      typedef const _Tp& const_reference;
      typedef _Tp value_type;
      template<typename _Tp1>
        struct rebind
        { 
            typedef new_allocator<_Tp1> other; 
        };
    };

  template<typename _Iterator, typename _Container>
    class __normal_iterator
    {
    };
  
}

template<typename _Tp>
  class allocator;
template<>
  class allocator<void>
  {
  public:
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;
    typedef void* pointer;
    typedef const void* const_pointer;
    typedef void value_type;
    template<typename _Tp1>
      struct rebind
      { typedef allocator<_Tp1> other; };
  };


template<typename _Tp>
  class allocator: public __gnu_cxx::new_allocator<_Tp>
  {
 public:
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;
    typedef _Tp* pointer;
    typedef const _Tp* const_pointer;
    typedef _Tp& reference;
    typedef const _Tp& const_reference;
    typedef _Tp value_type;
    template<typename _Tp1>
      struct rebind
      { 
          typedef allocator<_Tp1> other; 
      };
  };

template<typename _CharT, typename _Traits, typename _Alloc>
class basic_string
{
    typedef typename _Alloc::template rebind<_CharT>::other _CharT_alloc_type;
    public:
    typedef _Traits traits_type;
    typedef typename _Traits::char_type value_type;
    typedef _Alloc allocator_type;
    typedef typename _CharT_alloc_type::size_type size_type;
    typedef typename _CharT_alloc_type::difference_type difference_type;
    typedef typename _CharT_alloc_type::reference reference;
    typedef typename _CharT_alloc_type::const_reference const_reference;
    typedef typename _CharT_alloc_type::pointer pointer;
    typedef typename _CharT_alloc_type::const_pointer const_pointer;
    typedef __gnu_cxx::__normal_iterator<pointer, basic_string> iterator;
    typedef __gnu_cxx::__normal_iterator<const_pointer, basic_string>
        const_iterator;
};

}

template <typename _T>
struct A
{
    _T t;
    void f();
};

void f()
{
    A<std::string::iterator> c;

    c.f();
}

