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
  typedef long unsigned int size_t;
  typedef long int ptrdiff_t;
  typedef ptrdiff_t streamsize;
}

namespace std __attribute__ ((__visibility__ ("default")))
{
    class ios_base
    {
        public:
            streamsize precision(streamsize __prec)
            {
                return 1;
            }
    };

    template<typename _CharT>
        class basic_ios : public ios_base
    {
    };
    template<typename _CharT>
        class basic_ostream : virtual public basic_ios<_CharT>
    {
    };


    template<typename _CharT>
        class basic_istream : virtual public basic_ios<_CharT>
    {
    };


    template<typename _CharT>
        class basic_iostream
        : public basic_istream<_CharT>,
        public basic_ostream<_CharT>
    {
    };
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template<typename _CharT >
    class basic_fstream : public basic_iostream<_CharT>
    {
    };
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  typedef basic_fstream<char> fstream;
}


void foo(std::fstream& t)
{
    t.precision(8);
}
