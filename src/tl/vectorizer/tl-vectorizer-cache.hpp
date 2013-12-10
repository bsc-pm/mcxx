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

#ifndef TL_VECTORIZER_CACHE_HPP
#define TL_VECTORIZER_CACHE_HPP

#include "tl-nodecl.hpp"
#include "tl-vectorizer.hpp"
#include <map>
#include <vector>

using namespace Nodecl;

namespace TL 
{ 
    namespace Vectorization
    {
        class VectorizerCache;

        class CacheInfo
        {
            private:
                NodeclBase _lower_bound;
                NodeclBase _upper_bound;
                NodeclBase _stride;

                std::vector<TL::Symbol> _register_list;

            public:
                CacheInfo(const NodeclBase& lower_bound,
                       const NodeclBase& upper_bound,
                       const NodeclBase& stride);

            friend VectorizerCache;

        };

        class VectorizerCache
        {
            private:
                typedef std::map<TL::Symbol, CacheInfo> cache_map_t; //Second will be a list
                typedef std::pair<TL::Symbol, CacheInfo> cache_pair_t; //Second will be a list

                cache_map_t _cache_map;

            public:
                VectorizerCache(const TL::ObjectList<Nodecl::NodeclBase>& cached_expressions);

                void declare_cache_symbols(TL::Scope scope, 
                        const VectorizerEnvironment& environment);
                
                Nodecl::List get_init_statements(VectorizerEnvironment& environment) const;
                Nodecl::List get_iteration_update(VectorizerEnvironment& environment) const;
        };
    }
}

#endif // TL_VECTORIZER_CACHE_HPP
