/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#ifndef TL_COUNTERS_HPP
#define TL_COUNTERS_HPP

#include <iostream>
#include <string>
#include "tl-objectlist.hpp"

namespace TL
{
    struct Counter
    {
        private:
            int _n;

            Counter(const Counter& c)
                : _n(c._n)
            {
            }
        public:

            Counter()
                : _n(0)
            {
            }

            explicit Counter(int n) 
                : _n(n)
            {
            }

            Counter& operator=(int n)
            {
                _n = n;
                return *this;
            }

            Counter& operator+=(int n)
            {
                _n += n;
                return *this;
            }

            Counter& operator++(int)
            {
                return this->operator++();
            }

            Counter& operator++()
            {
                ++_n;
                return *this;
            }

            operator int() const
            {
                return _n;
            }

            friend std::ostream& operator<<(std::ostream& os, Counter c);
    };

    struct CounterManager
    {
        public:
            static Counter& get_counter(const std::string& str);
            static ObjectList<std::string> counters();
    };
}

#endif // TL_COUNTERS_HPP
