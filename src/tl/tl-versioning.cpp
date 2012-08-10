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

#include "tl-versioning.hpp"
#include <list>

namespace TL
{
    template <class V>
        TL::Version<V>::Version(const V& value, const int kindness) : 
            _value(value), _kindness(kindness)
    {
    }

    template <class V>
        V TL::Version<V>::get_value()
        {
            return _value;
        }

    template <class V>
        bool TL::Version<V>::is_better_than(const TL::Version<V>& version)
        {
            return _kindness > version._kindness;
        }


    template <class K, class V>
        TL::Versioning<K,V>::Versioning() : _versions()
    {

    }

    template <class K, class V>
        void TL::Versioning<K, V>::add_version(const K& key, Version<V>& version)
        {
            bool is_new_version = true;

            for (typename versions_map_t::iterator it = _versions.find(key);
                    it != versions_map_t::end();
                    it++)
            {
                if (it->second() == version)
                {
                    is_new_version = false;
                }
            }

            if (is_new_version)
            {
                _versions.insert(std::make_pair(key, version));
            }
            else 
            {
                /*
                DEBUG_CODE()
                {
                    std::cerr << "Versioning: Version is already in the map." << std::endl;
                }
                */
            }
        }

    template <class K, class V>
        V TL::Versioning<K, V>::get_best_version(const K& key)
        {
            typename versions_map_t::iterator it = _versions.find(key);

            ERROR_CONDITION(it == versions_map_t::end(), 
                    "Version key does not exist", 0);

            V best_version = it->second;
            it++;

            for (;it != versions_map_t::end();
                    it++)
            {
                if (it->second.is_better(best_version))
                {
                    best_version = it->second;
                }
            }

            return best_version;
        } 

    template <class K, class V>
        V TL::Versioning<K, V>::get_best_version(const K& key, const Version<V>& filter)
        {
            typedef std::list<V&> filtered_list_t;
            filtered_list_t filtered_versions;

            for (typename versions_map_t::iterator it = _versions.find(key);
                    it != versions_map_t::end();
                    it++)
            {
                if (it->second.is_similar_to(filter))
                {
                    filtered_versions.push_back(it->second);
                }
            }

            typename filtered_list_t::iterator it = filtered_versions.begin();

            ERROR_CONDITION(it == filtered_list_t::end(), 
                    "Filtered version key does not exist", 0);

            V best_version = *it;
            it++;

            for (;it != filtered_list_t::end();
                    it++)
            {
                if (it->is_better_than(best_version))
                {
                    best_version = *it;
                }
            }

            return best_version;
        }
}

