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




#include "tl-counters.hpp"
#include <map>
#include <algorithm>
#include <iterator>

namespace TL
{
    std::ostream& operator<<(std::ostream& os, Counter c)
    {
        return (os << c._n);
    }

    typedef std::map<std::string, Counter*> counter_map_t;
    typedef counter_map_t* counter_map_ptr_t;

    static counter_map_ptr_t _counter_map = NULL;

    Counter& CounterManager::get_counter(const std::string& str)
    {
        if (_counter_map == NULL)
        {
            _counter_map = new counter_map_t();
        }

        counter_map_t& counter_map(*_counter_map);
        Counter* p_counter = counter_map[str];
        if (p_counter == NULL)
        {
            p_counter = counter_map[str] = new Counter();
        }

        return *p_counter;
    }

    static std::string get_first(std::pair<std::string, Counter*> p)
    {
        return p.first;
    }

    ObjectList<std::string> CounterManager::counters()
    {
        ObjectList<std::string> result;
        if (_counter_map == NULL)
            return result;

        counter_map_t& counter_map(*_counter_map);

        std::transform(counter_map.begin(), counter_map.end(), 
                std::back_inserter(result), get_first);

        return result;
    }
}
