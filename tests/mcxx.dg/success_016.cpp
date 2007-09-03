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
typedef int ptrdiff_t;
typedef unsigned int size_t;
namespace A
{
  using ::ptrdiff_t;
  using ::size_t;
}


namespace A
{
  template<typename _Alloc>
    struct allocator;
  template<class _CharT>
    struct char_traits;
  template<typename _CharT, typename _Traits = char_traits<_CharT>,
           typename _Alloc = allocator<_CharT> >
    struct basic_string;
  template<> struct char_traits<char>;
  typedef basic_string<char> string;
  template<> struct char_traits<wchar_t>;
  typedef basic_string<wchar_t> wstring;
}

namespace A
{
  template<typename _CharT, typename _Traits, typename _Alloc>
    struct basic_string
    {
        typedef typename _Alloc::template rebind<_CharT>::other _CharT_alloc_type;
        typedef typename _CharT_alloc_type::size_type size_type;
    };
}

namespace B
{
  template<typename _Tp1>
    class new_allocator
    {
    };
}

namespace A
{
	template<typename _Tp2>
		struct allocator;

	template<>
		struct allocator<void>
		{
			public:
				typedef size_t size_type;
				template<typename _Tp3>
					struct rebind
					{ 
						typedef allocator<_Tp3> other; 
					};
		};

	template<typename _Tp4>
		struct allocator: public B::new_allocator<_Tp4>
		{
			public:
				typedef size_t size_type;
				template<typename _Tp5>
					struct rebind
					{ 
						typedef allocator<_Tp5> other; 
					};
				allocator(const allocator& __a) throw()
					: B::new_allocator<_Tp4>(__a) { }
		};
}

namespace A
{
    void g()
    {
         typedef string::size_type size_type;
    }
}
