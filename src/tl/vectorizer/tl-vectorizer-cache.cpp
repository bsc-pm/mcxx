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

#include "cxx-cexpr.h"
#include "tl-vectorizer-cache.hpp"
#include "tl-vectorizer-visitor-statement.hpp"

namespace TL 
{
    namespace Vectorization
    {
        CacheInfo::CacheInfo(
                const NodeclBase &lower_bound,
                const NodeclBase& upper_bound,
                const NodeclBase& stride)
            : _lower_bound(lower_bound), _upper_bound(upper_bound), _stride(stride)
        {
        }

        VectorizerCache::VectorizerCache(const ObjectList<NodeclBase>& cached_expressions)
        {
            for(ObjectList<NodeclBase>::const_iterator it = cached_expressions.begin();
                    it != cached_expressions.end();
                    it++)
            {
                ERROR_CONDITION(!it->is<ArraySubscript>(), 
                        "VECTORIZER: cache clause does not contain an ArraySubscript", 0);
    
                ArraySubscript arr_it = it->as<ArraySubscript>();
                NodeclBase list_front = arr_it.get_subscripts().as<Nodecl::List>().front();

                ERROR_CONDITION(!list_front.is<Range>(), 
                        "VECTORIZER: Range not found in cache clause", 0);
 
                Range range_it = list_front.as<Range>();


                TL::Symbol key = arr_it.get_subscripted().as<Nodecl::Symbol>().get_symbol();

                NodeclBase lower_bound = range_it.get_lower();
                NodeclBase upper_bound = range_it.get_upper();
                NodeclBase stride = range_it.get_stride();

                _cache_map.insert(cache_pair_t(key, 
                            CacheInfo(lower_bound, upper_bound, stride)));
            }
        }

        void VectorizerCache::declare_cache_symbols(TL::Scope scope, 
                const VectorizerEnvironment& environment)
        {
            // Map of caches
            for(cache_map_t::iterator it = _cache_map.begin();
                    it != _cache_map.end();
                    it++)
            {
                // TODO # registers
                // Add registers to each cache
                for (int i=0; i<2; i++)
                {
                    std::stringstream new_sym_name;
                    new_sym_name << "__cache_" << it->first.get_name() << "_" << i;

                    TL::Symbol new_sym = scope.new_symbol(new_sym_name.str());
                    new_sym.get_internal_symbol()->kind = SK_VARIABLE;
                    new_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                    new_sym.set_type(it->first.get_type().basic_type().
                            get_vector_of_elements(environment._unroll_factor));

                    std::cerr << it->first.get_name() << ": " << it->first.get_type().get_simple_declaration(scope, " ") 
                              << it->first.get_type().basic_type().get_simple_declaration(scope, " ")
                              << it->first.get_type().basic_type().get_vector_of_elements(environment._unroll_factor).get_simple_declaration(scope, " ")
                              << std::endl;

                    it->second._register_list.push_back(new_sym);
                }
            } 
        } 

        Nodecl::List VectorizerCache::get_init_statements(VectorizerEnvironment& environment) const
        {
            Nodecl::List result_list;

            for(cache_map_t::const_iterator it = _cache_map.begin();
                    it != _cache_map.end();
                    it++)
            {
                const std::vector<TL::Symbol>& register_list = it->second._register_list;
                const int size = register_list.size();

                //TODO: different strategies
                for(int i=1; i < size; i++)
                {
                    // __cache_X_1 = a[i];
                    ExpressionStatement exp_stmt = 
                        ExpressionStatement::make(
                                Assignment::make(
                                    register_list[i].make_nodecl(/* type = */ true), 
                                    ArraySubscript::make(
                                        it->first.make_nodecl(/* type = */true),
                                        Nodecl::List::make( //TODO: + VL*(i-1)
                                            it->second._lower_bound.shallow_copy()),
                                        it->first.get_type().basic_type()),
                                    register_list[i].get_type().basic_type()));

                    VectorizerVisitorStatement stmt_vectorizer(environment, /* cached disabled */ false);
                    stmt_vectorizer.walk(exp_stmt);

                    result_list.prepend(exp_stmt);
                } 
            }
            
            return result_list;
        } 

        Nodecl::List VectorizerCache::get_iteration_update(VectorizerEnvironment& environment) const
        {
            Nodecl::List result_list;

            for(cache_map_t::const_iterator it = _cache_map.begin();
                    it != _cache_map.end();
                    it++)
            {
                const std::vector<TL::Symbol>& register_list = it->second._register_list;
                const int size = register_list.size();

                //TODO: different strategies
                int i;
                for(i=0; i < (size-1); i++)
                {
                    // __cache_X_0 = __cache_X_1;
                    ExpressionStatement exp_stmt = 
                        ExpressionStatement::make(
                                Assignment::make(
                                    register_list[i].make_nodecl(/* type = */ true), 
                                    register_list[i+1].make_nodecl(/* type = */ true), 
                                    register_list[i].get_type().basic_type()));

                    result_list.prepend(exp_stmt);
                } 

                // __cache_X_0 = load(a[i + VL]) // TODO: VL*;
                ExpressionStatement exp_stmt = 
                    ExpressionStatement::make(
                            Assignment::make(
                                register_list[i].make_nodecl(/* type = */ true), 
                                ArraySubscript::make(
                                    it->first.make_nodecl(/* type = */true),
                                    Nodecl::List::make(
                                        Add::make(
                                            it->second._lower_bound.shallow_copy(),
                                            const_value_to_nodecl(const_value_get_signed_int(environment._unroll_factor)),
                                            TL::Type::get_int_type())),
                                    it->first.get_type().basic_type()),
                                register_list[i].get_type().basic_type()));

                VectorizerVisitorStatement stmt_vectorizer(environment, /* cache disabled */ false);
                stmt_vectorizer.walk(exp_stmt);

                result_list.prepend(exp_stmt);

            }
            
            return result_list;
        } 

        bool VectorizerCache::is_cached_access(const ArraySubscript& n) const
        {
            NodeclBase subscripted = Nodecl::Utils::advance_conversions(n.get_subscripted());

            if(subscripted.is<Nodecl::Symbol>())
            {
                TL::Symbol sym = subscripted.as<Nodecl::Symbol>().get_symbol();
                if (_cache_map.find(sym) != _cache_map.end())
                {
                    return true;
                }
            }

            return false;
        }

        NodeclBase VectorizerCache::get_load_access(const ArraySubscript& n) const
        {
            NodeclBase subscripted = Nodecl::Utils::advance_conversions(n.get_subscripted());

            if(subscripted.is<Nodecl::Symbol>())
            {
                TL::Symbol sym = subscripted.as<Nodecl::Symbol>().get_symbol();

                // Find cache
                cache_map_t::const_iterator cache_it = _cache_map.find(sym); 
                if (cache_it != _cache_map.end())
                {
                    const CacheInfo& cache = cache_it->second;

                    // Compare subscripts
                    NodeclBase subscript = n.get_subscripts().as<Nodecl::List>().front();

                    if (Nodecl::Utils::equal_nodecls(cache._lower_bound, subscript, 
                                /* skip conversions */ true))
                    {
                        return cache._register_list[0].make_nodecl(true);
                    }
                    else
                    {
                        return Nodecl::VectorShiftRight2::make(
                                cache._register_list[1].make_nodecl(true),
                                cache._register_list[0].make_nodecl(true),
                                const_value_to_nodecl(const_value_get_one(4, 1)),
                                Vectorization::Utils::get_null_mask(),
                                cache._register_list[0].get_type());
                    }
                }
            }

            return n;
        }
    }
}

