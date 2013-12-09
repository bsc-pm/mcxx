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

#include "tl-vectorizer-cache.hpp"

namespace TL 
{
    namespace Vectorization
    {
        CacheInfo::CacheInfo(const NodeclBase& lower_bound, const NodeclBase& upper_bound,
                const NodeclBase& stride)
            : _lower_bound(lower_bound), _upper_bound(upper_bound), _stride(stride)
        {
        }


        VectorizerCache::VectorizerCache(const ObjectList<Nodecl::NodeclBase>& cached_expressions)
        {
            for(ObjectList<Nodecl::NodeclBase>::const_iterator it = cached_expressions.begin();
                    it != cached_expressions.end();
                    it++)
            {
                ERROR_CONDITION(!it->is<Nodecl::ArraySubscript>(), 
                        "VECTORIZER: cache clause does not contain an ArraySubscript", 0);
    
                Nodecl::ArraySubscript arr_it = it->as<Nodecl::ArraySubscript>();
                Nodecl::Range range_it = arr_it.get_subscripts().as<Nodecl::List>().front().as<Nodecl::Range>();

                TL::Symbol key = arr_it.get_subscripted().as<Nodecl::Symbol>().get_symbol();
                Nodecl::NodeclBase lower_bound = range_it.get_lower();
                Nodecl::NodeclBase upper_bound = range_it.get_upper();
                Nodecl::NodeclBase stride = range_it.get_stride();

                _cache_map.insert(cache_pair_t(key, 
                            CacheInfo(upper_bound, lower_bound, stride)));
            }
        }

        void VectorizerCache::declare_cache_symbols(TL::Scope scope)
        {
            for(cache_map_t::iterator it = _cache_map.begin();
                    it != _cache_map.end();
                    it++)
            {
                // TODO # registers
                for (int i=0; i<2; i++)
                {
                    std::stringstream new_sym_name;
                    new_sym_name << "__cache_" << it->first.get_name() << "_" << i;

                    TL::Symbol new_sym = scope.new_symbol(new_sym_name.str());
                    new_sym.get_internal_symbol()->kind = SK_VARIABLE;
                    new_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                    new_sym.set_type(it->first.get_type());

                    it->second._register_list.push_back(new_sym);
                }
            } 
        } 

        Nodecl::List VectorizerCache::get_init_statements()
        {
            Nodecl::List result_list;

            for(cache_map_t::iterator it = _cache_map.begin();
                    it != _cache_map.end();
                    it++)
            {
                std::vector<TL::Symbol>& register_list = it->second._register_list;
                const int size = register_list.size();

                for(int i=0; i < size; i++)
                {
                    result_list.append(Nodecl::ExpressionStatement::make(Nodecl::Add::make(
                                register_list[i].make_nodecl(), 
                                register_list[i].make_nodecl(),
                                register_list[i].get_type())));
                } 
            }
            
            return result_list;
        } 
    }
}

