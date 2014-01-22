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

#include "tl-vectorizer-analysis.hpp"
#include <algorithm>

namespace TL 
{ 
    namespace Vectorization 
    {
        VectorizerAnalysisStaticInfo::VectorizerAnalysisStaticInfo(
                const Nodecl::NodeclBase& n, Analysis::WhichAnalysis analysis_mask,
                Analysis::WhereAnalysis nested_analysis_mask, int nesting_level) 
            :
            VectorizerAnalysisMaps(),
            AnalysisStaticInfo::AnalysisStaticInfo(copy_function_code(n.as<Nodecl::FunctionCode>()), 
                    analysis_mask, nested_analysis_mask, nesting_level),
            _original_node(n) 
        {
            //Fill inverse maps
            for(Nodecl::Utils::NodeclDeepCopyMap::const_iterator it = _orig_to_copy_nodes.begin();
                    it != _orig_to_copy_nodes.end();
                    it ++)
            {
                //std::cerr << "-> " << it->first.prettyprint() << ": " << &(it->first) << std::endl;

                std::pair<Nodecl::Utils::NodeclDeepCopyMap::const_iterator, bool> ret_insert = 
                    _copy_to_orig_nodes.insert(
                            std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(it->second, it->first));

                if (!(ret_insert.second))
                {
                    internal_error("VectorizerAnalysis: Error inserting Nodecl in reverse map. Nodecl already exists?", 0);
                }
            }

            for(Nodecl::Utils::SymbolDeepCopyMap::const_iterator it = _orig_to_copy_symbols.begin();
                    it != _orig_to_copy_symbols.end();
                    it ++)
            {
                std::pair<Nodecl::Utils::SymbolDeepCopyMap::const_iterator, bool> ret_insert = 
                    _copy_to_orig_symbols.insert(
                            std::pair<TL::Symbol, TL::Symbol>(it->second, it->first));

                if (!(ret_insert.second))
                {
                    internal_error("VectorizerAnalysis: Error inserting Symbol in reverse map. Nodecl already exists?", 0);
                }
            }
        }

        Nodecl::FunctionCode VectorizerAnalysisStaticInfo::copy_function_code(const Nodecl::FunctionCode& n)
        {
            TL::Symbol func_sym = n.get_symbol();
            std::string orig_func_name = func_sym.get_name();

            TL::Symbol new_func_sym = func_sym.get_scope().
                new_symbol("__" + orig_func_name + "_copy");
            new_func_sym.get_internal_symbol()->kind = SK_FUNCTION;

            Nodecl::Utils::SimpleSymbolMap func_sym_map;
            func_sym_map.add_map(func_sym, new_func_sym);

            return Nodecl::Utils::deep_copy(n, n, func_sym_map, _orig_to_copy_nodes, 
                    _orig_to_copy_symbols).as<Nodecl::FunctionCode>();
        }

        Nodecl::NodeclBase VectorizerAnalysisStaticInfo::translate_input(const Nodecl::NodeclBase& n) 
        {
            Nodecl::Utils::NodeclDeepCopyMap::const_iterator it = _orig_to_copy_nodes.find(n);

            if (it == _orig_to_copy_nodes.end())
            {
                std::cerr << "From O to C: " <<  n.prettyprint() << ": " << &(it->first) << std::endl;

                Nodecl::Utils::NodeclDeepCopyMap::const_iterator it2 = _copy_to_orig_nodes.find(n);
                if (it2 != _copy_to_orig_nodes.end())
                    internal_error("VectorizerAnalysis: Error translating Nodecl from origin to copy. NODE ALREADY TRANSLATED", 0);

                register_node(n);

                // Get the registered node
                it = _orig_to_copy_nodes.find(n);

                //internal_error("VectorizerAnalysis: Error translating Nodecl from origin to copy", 0);
            }

            return it->second;
        }

        TL::ObjectList<Nodecl::NodeclBase> VectorizerAnalysisStaticInfo::translate_input(
                const TL::ObjectList<Nodecl::NodeclBase>& list) 
        {
            TL::ObjectList<Nodecl::NodeclBase> result_list;

            for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                result_list.append(translate_input(*it));
            }

            return result_list;
        }

        TL::Symbol VectorizerAnalysisStaticInfo::translate_input(const TL::Symbol& n) const
        {
            Nodecl::Utils::SymbolDeepCopyMap::const_iterator it = _orig_to_copy_symbols.find(n);

            if (it == _orig_to_copy_symbols.end())
            {
                internal_error("VectorizerAnalysis: Error translating Symbol from origin to copy", 0);
            }

            return it->second;
        }

        Nodecl::NodeclBase VectorizerAnalysisStaticInfo::translate_output(const Nodecl::NodeclBase& n) const
        {
            Nodecl::Utils::NodeclDeepCopyMap::const_iterator it = _copy_to_orig_nodes.find(n);

            if (it == _copy_to_orig_nodes.end())
            {
                std::cerr << "From C to O: " << n.prettyprint() << ": " << &(it->first) <<  std::endl;
                internal_error("VectorizerAnalysis: Error translating Nodecl from copy to origin", 0);
                return n;
            }

            return it->second;
        }

        TL::ObjectList<Nodecl::NodeclBase> VectorizerAnalysisStaticInfo::translate_output(const TL::ObjectList<Nodecl::NodeclBase>& list) const
        {
            TL::ObjectList<Nodecl::NodeclBase> result_list;

            for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                result_list.append(translate_output(*it));
            }

            return result_list;
        }

        TL::Symbol VectorizerAnalysisStaticInfo::translate_output(const TL::Symbol& n) const
        {
            Nodecl::Utils::SymbolDeepCopyMap::const_iterator it = _copy_to_orig_symbols.find(n);

            if (it == _copy_to_orig_symbols.end())
            {
                internal_error("VectorizerAnalysis: Error translating Symbol from copy to origin", 0);
            }

            return it->second;
        }

        Nodecl::Utils::NodeclDeepCopyMap::iterator VectorizerAnalysisStaticInfo::find_equal_nodecl(
                const Nodecl::NodeclBase& n,
                Nodecl::Utils::NodeclDeepCopyMap& map)
        {
            for (Nodecl::Utils::NodeclDeepCopyMap::iterator it = map.begin();
                    it != map.end();
                    it ++)
            {
                if(Nodecl::Utils::equal_nodecls(n, it->first, false /*Do no skip conversions*/))
                    return it;
            }

            return map.end();
        }
        
        Nodecl::NodeclBase VectorizerAnalysisStaticInfo::get_translated_copy(const Nodecl::NodeclBase& n)
        {
            Nodecl::Utils::NodeclDeepCopyMap::const_iterator it = 
                find_equal_nodecl(n, _orig_to_copy_nodes);

            // New node already exists.
            // Get the copy, then shallow copy it
            if (it != _orig_to_copy_nodes.end())
            {
                return it->second.shallow_copy();
            }
            else
            {
                Nodecl::NodeclBase new_node = n.shallow_copy();

                TL::ObjectList<Nodecl::NodeclBase> children_list = new_node.children();

                for(TL::ObjectList<Nodecl::NodeclBase>::iterator children_it = children_list.begin();
                        children_it != children_list.end();
                        children_it++)
                {
                    if(!children_it->is_null())
                        children_it->replace(get_translated_copy(*children_it));
                }

                return new_node;
            }
        }

        void VectorizerAnalysisStaticInfo::register_node(const Nodecl::NodeclBase& n)
        {
            if (_orig_to_copy_nodes.find(n) != _orig_to_copy_nodes.end())
                 internal_error("VectorizerAnalysis: Node already registered", 0);

            
            Nodecl::Utils::NodeclDeepCopyMap::iterator it = 
                find_equal_nodecl(n, _orig_to_copy_nodes);

            // There is equal node in origin and, therefore, in copy
            // Shallow copy it and insert the new copy
            if (it != _orig_to_copy_nodes.end())
            {
                std::cerr << "Shallow copy " << n.prettyprint() << std::endl;

                Nodecl::NodeclBase sc_copy = it->second.shallow_copy();

                _orig_to_copy_nodes.insert(
                        std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(n, sc_copy));

                _copy_to_orig_nodes.insert(
                        std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(sc_copy, n));
            }
            // There is NO equal node in origin
            // Insert it and create a "translated copy" of it and its children
            else
            {
                std::cerr << "Making up " << n.prettyprint() << std::endl;
                Nodecl::NodeclBase translated_n = get_translated_copy(n);

                _orig_to_copy_nodes.insert(
                        std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(n, translated_n));

                _copy_to_orig_nodes.insert(
                        std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(translated_n, n));

            }

            _registered_nodes.push_back(n);
/*
            // Register also children
            TL::ObjectList<Nodecl::NodeclBase> children_list = n.children();
            for(TL::ObjectList<Nodecl::NodeclBase>::iterator children_it = children_list.begin();
                    children_it != children_list.end();
                    children_it++)
            {
                if(!children_it->is_null())
                    register_node(*children_it);
            }
*/            
        }

        //WARNING: if you use this function out of unregister_nodeS you should remove the
        // unregistered_node from the _registered_nodes list
        void VectorizerAnalysisStaticInfo::unregister_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Delete " << n.prettyprint() << std::endl;

            Nodecl::Utils::NodeclDeepCopyMap::iterator it = _orig_to_copy_nodes.find(n);
            Nodecl::Utils::NodeclDeepCopyMap::iterator it2 = _copy_to_orig_nodes.find(it->second);

            _orig_to_copy_nodes.erase(it);
            _copy_to_orig_nodes.erase(it2);
/*
            // Unregister also children
            TL::ObjectList<Nodecl::NodeclBase> children_list = n.children();
            for(TL::ObjectList<Nodecl::NodeclBase>::iterator children_it = children_list.begin();
                    children_it != children_list.end();
                    children_it++)
            {
                if(!children_it->is_null())
                    unregister_node(*children_it);
            }
*/            
        }

        void VectorizerAnalysisStaticInfo::unregister_nodes()
        {
            for(std::list<Nodecl::NodeclBase>::iterator it = _registered_nodes.begin();
                    it != _registered_nodes.end();
                    it++)
            {
                unregister_node(*it);
            }
            _registered_nodes.clear();
        }
 
        bool VectorizerAnalysisStaticInfo::is_constant(const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
        {
            bool result = Analysis::AnalysisStaticInfo::is_constant(
                    translate_input(scope), translate_input(n));

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::has_been_defined(const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n, 
                const Nodecl::NodeclBase& s)
        {
            bool result = Analysis::AnalysisStaticInfo::has_been_defined(
                    translate_input(scope), translate_input(n), translate_input(s));

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::is_induction_variable(const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
        {
            bool result = Analysis::AnalysisStaticInfo::is_induction_variable(
                    translate_input(scope), translate_input(n));

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::is_basic_induction_variable(const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
        {
            bool result = Analysis::AnalysisStaticInfo::is_basic_induction_variable(
                    translate_input(scope), translate_input(n));

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::is_non_reduction_basic_induction_variable(const Nodecl::NodeclBase& scope, 
                const Nodecl::NodeclBase& n)
        {
            bool result = Analysis::AnalysisStaticInfo::is_non_reduction_basic_induction_variable(
                    translate_input(scope), translate_input(n));

            unregister_nodes();

            return result;
        }
 
        Nodecl::NodeclBase VectorizerAnalysisStaticInfo::get_induction_variable_increment(const Nodecl::NodeclBase& scope,
                const Nodecl::NodeclBase& n ) 
        {
            Nodecl::NodeclBase return_nodecl = Analysis::AnalysisStaticInfo::get_induction_variable_increment(
                    translate_input(scope), translate_input(n));

            unregister_nodes();

            if (return_nodecl.is<Nodecl::IntegerLiteral>())
                return return_nodecl;
            
            return translate_output(return_nodecl);
        }
        
        ObjectList<Nodecl::NodeclBase> VectorizerAnalysisStaticInfo::get_induction_variable_increment_list(
                const Nodecl::NodeclBase& scope,
                const Nodecl::NodeclBase& n )
        {
            ObjectList<Nodecl::NodeclBase> result = 
                translate_output(Analysis::AnalysisStaticInfo::get_induction_variable_increment_list(
                    translate_input(scope), translate_input(n)));

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::is_induction_variable_increment_one( const Nodecl::NodeclBase& scope, 
                const Nodecl::NodeclBase& n )
        {
            bool result = Analysis::AnalysisStaticInfo::is_induction_variable_increment_one(
                    translate_input(scope), translate_input(n));

            unregister_nodes();

            return result;
        }

        ObjectList<Analysis::Utils::InductionVariableData*> VectorizerAnalysisStaticInfo::get_induction_variables(
                const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) 
        {
            std::cerr << "INDUCTIONVARIABLEDATA" << std::endl;

            ObjectList<Analysis::Utils::InductionVariableData*> result = 
                Analysis::AnalysisStaticInfo::get_induction_variables(
                        translate_input(scope), translate_input(n));

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::is_adjacent_access(const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
        {
            bool result = Analysis::AnalysisStaticInfo::is_adjacent_access(
                    translate_input(scope), translate_input(n));

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::is_induction_variable_dependent_access(
                const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
        {
            bool result = Analysis::AnalysisStaticInfo::is_induction_variable_dependent_access(
                    translate_input(scope), translate_input(n));

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::is_constant_access(const Nodecl::NodeclBase& scope, 
                const Nodecl::NodeclBase& n) 
        {
            bool result = Analysis::AnalysisStaticInfo::is_constant_access(
                    translate_input(scope), translate_input(n));

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::is_simd_aligned_access(const Nodecl::NodeclBase& scope, 
                const Nodecl::NodeclBase& n, 
                const ObjectList<Nodecl::NodeclBase>* suitable_expressions,
                int unroll_factor, int alignment) 
        {
            bool result;

            if (suitable_expressions != NULL)
            {
                ObjectList<Nodecl::NodeclBase> translated_suitable_expressions =
                    translate_input(*suitable_expressions);

                result = Analysis::AnalysisStaticInfo::is_simd_aligned_access(
                        translate_input(scope), translate_input(n), 
                        &translated_suitable_expressions,
                        unroll_factor, alignment);
            }
            else
            {
                result = Analysis::AnalysisStaticInfo::is_simd_aligned_access(
                        translate_input(scope), translate_input(n), 
                        suitable_expressions,
                        unroll_factor, alignment);
            }

            unregister_nodes();

            return result;
        }

        bool VectorizerAnalysisStaticInfo::is_suitable_expression(const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n, 
                const ObjectList<Nodecl::NodeclBase>* suitable_expressions,
                int unroll_factor, int alignment, int& vector_size_module)
        {
            bool result;

            if (suitable_expressions != NULL)
            {
                ObjectList<Nodecl::NodeclBase> translated_suitable_expressions =
                    translate_input(*suitable_expressions);

                result = Analysis::AnalysisStaticInfo::is_suitable_expression(
                        translate_input(scope), translate_input(n),
                        &translated_suitable_expressions,
                        unroll_factor, alignment, vector_size_module);
            }
            else
            {
                result = Analysis::AnalysisStaticInfo::is_suitable_expression(
                        translate_input(scope), translate_input(n),
                        suitable_expressions,
                        unroll_factor, alignment, vector_size_module);
            }

            unregister_nodes();

            return result;
        }
    }
}


