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

#ifndef TL_VERSIONING_HPP
#define TL_VERSIONING_HPP

#include <map>

namespace TL
{
    template <class V>
    class Version
    {
        protected:
            const V _value;
            int _kindness;

        public:
            Version(const V& value, const int kindness);
            
            virtual bool is_better_than(const Version<V>& version);
            virtual bool passes_filter(const Version<V>& filter) = 0;
            V get_value();
    };

    template < class K, class V>
    class Versioning
    {
        private:
            typedef std::map< K, Version<V>& > versions_map_t;
            versions_map_t _versions;

        public:
            Versioning();

            void add_version(const K& key, Version<V>& value);

            V get_best_version(const K& key);
            V get_best_version(const K& key, const Version<V>& filter);
    };
}

#endif //TL_VERSIONING_HPP


