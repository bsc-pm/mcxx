/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include "tl-vectorization-analysis-interface.hpp"

#include "tl-vectorization-analysis-internals.hpp"
#include "tl-extensible-graph.hpp"

#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
    VectorizationAnalysisInterface::VectorizationAnalysisInterface(
            const Nodecl::NodeclBase& n, 
            const Analysis::WhichAnalysis analysis_mask)
        : VectorizationAnalysisCopyMaps(),
        Analysis::AnalysisInterface::AnalysisInterface(
                copy_function_code(n),
                analysis_mask, 
                /*ompss_enabled*/false),
        _original_node(n)
    {
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: Computing new analysis\n");
        }

        //Fill inverse maps
        for(Nodecl::Utils::NodeclDeepCopyMap::const_iterator it =
                _orig_to_copy_nodes.begin();
                it != _orig_to_copy_nodes.end();
                it ++)
        {
            /*
            if (it->second.is<Nodecl::Symbol>())
            std::cerr << "Inserting " << it->second.prettyprint() << ": " << &(it->second.get_internal_nodecl()) << std::endl 
                << it->first.prettyprint() << ": " << &(it->first.get_internal_nodecl()) << std::endl;
            */

            std::pair<Nodecl::Utils::NodeclDeepCopyMap::const_iterator, bool>
                ret_insert = _copy_to_orig_nodes.insert(
                        std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(
                            it->second, it->first));

            if (!(ret_insert.second))
            {
                internal_error("VectorizerAnalysis: Error inserting Nodecl "\
                        "in reverse map. Nodecl already exists?", 0);
            }
        }

        for(Nodecl::Utils::SymbolDeepCopyMap::const_iterator it =
                _orig_to_copy_symbols.begin();
                it != _orig_to_copy_symbols.end();
                it ++)
        {
            /*
            std::cerr << "Inserting symbol " << it->first.get_name() << ": "
               << it->first.get_internal_symbol() << std::endl;
            */
            std::pair<Nodecl::Utils::SymbolDeepCopyMap::const_iterator, bool>
                ret_insert = _copy_to_orig_symbols.insert(
                        std::pair<TL::Symbol, TL::Symbol>(it->second, it->first));

            if (!(ret_insert.second))
            {
                internal_error("VectorizerAnalysis: Error inserting Symbol "\
                        "in reverse map. Nodecl already exists?", 0);
            }
        }
    }

    VectorizationAnalysisInterface::~VectorizationAnalysisInterface()
    {
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: Finalizing analysis\n");
        }
        //AnalysisInterface::~AnalysisInterface();
    }

    Nodecl::FunctionCode VectorizationAnalysisInterface::copy_function_code(
            const Nodecl::NodeclBase& n)
    {
        Nodecl::FunctionCode func_code;

        if (n.is<Nodecl::OpenMP::SimdFunction>())
            func_code = n.as<Nodecl::OpenMP::SimdFunction>().
                get_statement().as<Nodecl::FunctionCode>();
        else
            func_code = n.as<Nodecl::FunctionCode>();

        TL::Symbol func_sym = func_code.get_symbol();
        std::string orig_func_name = func_sym.get_name();

        TL::Symbol new_func_sym = func_sym.get_scope().
            new_symbol("__" + orig_func_name + "_copy");
        new_func_sym.get_internal_symbol()->kind = SK_FUNCTION;

        Nodecl::Utils::SimpleSymbolMap func_sym_map;
        func_sym_map.add_map(func_sym, new_func_sym);

        return Nodecl::Utils::deep_copy(n, n, func_sym_map, _orig_to_copy_nodes,
                _orig_to_copy_symbols).as<Nodecl::FunctionCode>();
    }

    Nodecl::NodeclBase VectorizationAnalysisInterface::translate_input(
            const Nodecl::NodeclBase& n)
    {
        Nodecl::Utils::NodeclDeepCopyMap::const_iterator it =
            _orig_to_copy_nodes.find(n);

        
        // for(Nodecl::Utils::NodeclDeepCopyMap::const_iterator it3 = _orig_to_copy_nodes.begin();
        //         it3 != _orig_to_copy_nodes.end();
        //         it3++)
        // {
        //     std::cerr << "Origin node " << nodecl_get_ast(it3->first.get_internal_nodecl()) /* << ": " << it3->first.prettyprint() */ << std::endl;
        // }

        //std::cerr << "Request to orig_to_copy=" << &_orig_to_copy_nodes << std::endl;
        //std::cerr << "Requested node " << nodecl_get_ast(n.get_internal_nodecl()) /* << ": " << it3->first.prettyprint() */ << std::endl;

        if (it == _orig_to_copy_nodes.end())
        {
            //std::cerr << "From O to C: " <<  n.prettyprint() << ": " << &(it->first) << std::endl;

            Nodecl::Utils::NodeclDeepCopyMap::const_iterator it2 =
                _copy_to_orig_nodes.find(n);
            if (it2 != _copy_to_orig_nodes.end())
            {
                //return n;
                internal_error("VectorizerAnalysis: Error translating Nodecl "
                "from origin to copy. NODE ALREADY TRANSLATED", 0);
            }

            //return n;
            //register_node(n);

            // Get the registered node
            //it = _orig_to_copy_nodes.find(n);
            /*
            for(Nodecl::Utils::NodeclDeepCopyMap::const_iterator it3 = _orig_to_copy_nodes.begin();
                    it3 != _orig_to_copy_nodes.end();
                    it3++)
            {
                std::cerr << "Origin node " << &(it3->first.get_internal_nodecl()) << ": " << it3->first.prettyprint() << std::endl;
            }
            */

            Nodecl::NodeclBase n_pointer = n;
            while (it == _orig_to_copy_nodes.end())
            {
                n_pointer  = n_pointer.get_parent();
                std::cerr << "Parent: " << n_pointer.prettyprint() << std::endl;
                it = _orig_to_copy_nodes.find(n_pointer);
            }

            std::cerr << "FOUND!" << std::endl;

            internal_error("VectorizerAnalysis: Error translating Nodecl from origin to copy, %p %s",
                    nodecl_get_ast(n.get_internal_nodecl()), n.prettyprint().c_str());
        }
        //std::cerr << "Translation from O to C: " <<  n.prettyprint() << ": " << &(it->first) << std::endl;

        return it->second;
    }

    objlist_nodecl_t VectorizationAnalysisInterface::translate_input(
            const objlist_nodecl_t& list)
    {
        objlist_nodecl_t result_list;

        for(objlist_nodecl_t::const_iterator it = list.begin();
                it != list.end();
                it++)
        {
            result_list.append(translate_input(*it));
        }

        return result_list;
    }

    TL::Symbol VectorizationAnalysisInterface::translate_input(
            const TL::Symbol& n) const
    {
        Nodecl::Utils::SymbolDeepCopyMap::const_iterator it =
            _orig_to_copy_symbols.find(n);

        if (it == _orig_to_copy_symbols.end())
        {
            internal_error("VectorizerAnalysis: Error translating "\
                    "Symbol '%s' from origin to copy",
                    n.get_name().c_str());
        }

        return it->second;
    }

    objlist_tlsym_t VectorizationAnalysisInterface::translate_input(
            const objlist_tlsym_t& list) const
    {
        objlist_tlsym_t result_list;

        for(objlist_tlsym_t::const_iterator it = list.begin();
                it != list.end();
                it++)
        {
            /*
            std::cerr << "Translating symbol " << it->get_name() << ": "
               << it->get_internal_symbol() << std::endl;
            */
            result_list.append(translate_input(*it));
        }

        return result_list;
    }

    std::map<TL::Symbol, int> VectorizationAnalysisInterface::translate_input(
            const std::map<TL::Symbol, int>& map)
    {
        std::map<TL::Symbol, int> result_map;

        for(std::map<TL::Symbol, int>::const_iterator it = map.begin();
                it != map.end();
                it++)
        {
            std::pair<TL::Symbol, int> it_pair(translate_input(it->first),
                    it->second);

            result_map.insert(it_pair);
        }

        return result_map;
    }

    Nodecl::NodeclBase VectorizationAnalysisInterface::translate_output(
            const Nodecl::NodeclBase& n)
    {
        Nodecl::Utils::NodeclDeepCopyMap::iterator it =
            _copy_to_orig_nodes.find(n);

        if (it == _copy_to_orig_nodes.end())
        {
            //std::cerr << "From C to O: " << n.prettyprint() << ": " << &(it->first) <<  std::endl;

            Nodecl::Utils::NodeclDeepCopyMap::const_iterator it2 =
                _orig_to_copy_nodes.find(n);
            if (it2 != _orig_to_copy_nodes.end())
            {
                //return n;
                internal_error("VectorizerAnalysis: Error translating Nodecl "
                "from copy to copy. NODE ALREADY TRANSLATED", 0);
            }

            Nodecl::NodeclBase n_pointer = n;
            while (it == _copy_to_orig_nodes.end())
            {
                n_pointer  = n_pointer.get_parent();
                std::cerr << "Parent: " << n_pointer.prettyprint() << std::endl;
                it = _copy_to_orig_nodes.find(n_pointer);
            }

            std::cerr << "FOUND!" << std::endl;

            internal_error("VectorizerAnalysis: Error translating "\
                    "Nodecl from copy to origin %p %s",
                    nodecl_get_ast(n.get_internal_nodecl()), n.prettyprint().c_str());

            return n;
        }

        return it->second;
    }

    objlist_nodecl_t VectorizationAnalysisInterface::translate_output(
            const objlist_nodecl_t& list)
    {
        objlist_nodecl_t result_list;

        for(objlist_nodecl_t::const_iterator it = list.begin();
                it != list.end();
                it++)
        {
            result_list.append(translate_output(*it));
        }

        return result_list;
    }

    TL::Symbol VectorizationAnalysisInterface::translate_output(
            const TL::Symbol& n) const
    {
        Nodecl::Utils::SymbolDeepCopyMap::const_iterator it =
            _copy_to_orig_symbols.find(n);

        if (it == _copy_to_orig_symbols.end())
        {
            internal_error("VectorizerAnalysis: Error translating "\
                    "Symbol from copy to origin", 0);
        }

        return it->second;
    }


    bool VectorizationAnalysisInterface::is_uniform(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& stmt,
            const Nodecl::NodeclBase& n)
    {
        Nodecl::NodeclBase translated_n =
            translate_input(n);
        Nodecl::NodeclBase translated_scope =
            translate_input(scope);

        map_scope_analysis_info_t::iterator scope_it =
            _scope_analysis_info.find(
                    translated_scope);

        // Already computed
        if (scope_it != _scope_analysis_info.end())
        {
            const map_node_bool_t& uniform_nodes =
                scope_it->second.uniform_nodes;

            map_node_bool_t::const_iterator node_it =
                uniform_nodes.find(translated_n);

            if (node_it != uniform_nodes.end())
            {
                return node_it->second;
            }
        }
        else
        {
            scope_it = _scope_analysis_info.insert(
                    pair_scope_analysis_info_t(
                        translated_scope,
                        VectorizationAnalysisInfo())).first;
        }

        // New 
        bool result = Analysis::AnalysisInterface::is_uniform(
                translated_scope, translate_input(stmt),
                translated_n);

        scope_it->second.uniform_nodes.insert(
                pair_node_bool_t(translated_n, result));
        
        return result;
    }

    bool VectorizationAnalysisInterface::is_linear(
        const Nodecl::NodeclBase& scope,
        const Nodecl::NodeclBase& n)
    {
        Nodecl::NodeclBase translated_n =
            translate_input(n);
        Nodecl::NodeclBase translated_scope =
            translate_input(scope);

        map_scope_analysis_info_t::iterator scope_it =
            _scope_analysis_info.find(
                    translated_scope);

        // Already computed
        if (scope_it != _scope_analysis_info.end())
        {
            const map_node_bool_t& linear_nodes =
                scope_it->second.linear_nodes;

            map_node_bool_t::const_iterator node_it =
                linear_nodes.find(translated_n);

            if (node_it != linear_nodes.end())
            {
                return node_it->second;
            }
        }
        else
        {
            scope_it = _scope_analysis_info.insert(
                    pair_scope_analysis_info_t(
                        translated_scope,
                        VectorizationAnalysisInfo())).first;
        }

        // New 
        bool result = Analysis::AnalysisInterface::is_linear(
                translated_scope, translated_n);

        scope_it->second.linear_nodes.insert(
                pair_node_bool_t(translated_n, result));

        return result;
    }

    Nodecl::NodeclBase VectorizationAnalysisInterface::get_linear_step(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n)
    {
        Nodecl::NodeclBase iv_step = Analysis::AnalysisInterface::
                get_linear_variable_increment(translate_input(scope),
                        translate_input(n));
    
        if (iv_step.is<Nodecl::IntegerLiteral>())
            return iv_step;

        return translate_output(iv_step);
    }

    objlist_nodecl_t VectorizationAnalysisInterface::get_linear_nodecls(
                const Nodecl::NodeclBase& scope )
    {
        Analysis::Utils::InductionVarList linear_data_list =
            Analysis::AnalysisInterface::get_linear_variables(
                    translate_input(scope));

        objlist_nodecl_t result;

        for(Analysis::Utils::InductionVarList::const_iterator it =
                linear_data_list.begin();
                it != linear_data_list.end();
                it ++)
        {
            result.append((*it)->get_variable());
        }

        return translate_output(result);
    }
    
    bool VectorizationAnalysisInterface::has_been_defined(
            const Nodecl::NodeclBase& n) 
    {
        return Analysis::AnalysisInterface::has_been_defined(
                translate_input(n));
    }

    
    bool VectorizationAnalysisInterface::is_induction_variable(
            const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
    {
        Nodecl::NodeclBase translated_n = translate_input(n);
        Nodecl::NodeclBase translated_scope =
            translate_input(scope);

        map_scope_analysis_info_t::iterator scope_it =
            _scope_analysis_info.find(
                    translated_scope);

        // Already computed
        if (scope_it != _scope_analysis_info.end())
        {
            const map_node_bool_t& ivs_nodes =
                scope_it->second.ivs_nodes;

            map_node_bool_t::const_iterator node_it =
                ivs_nodes.find(translated_n);

            if (node_it != ivs_nodes.end())
            {
                return node_it->second;
            }
        }
        else
        {
            scope_it = _scope_analysis_info.insert(
                    pair_scope_analysis_info_t(scope,
                        VectorizationAnalysisInfo())).first;
        }

        // New 
        bool result = Analysis::AnalysisInterface::is_induction_variable(
                translate_input(scope), translated_n);

        scope_it->second.ivs_nodes.insert(
                pair_node_bool_t(n, result));
 
        return result;
    }
    
    /*
    bool VectorizationAnalysisInterface::
        is_non_reduction_basic_induction_variable(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n)
    {
        bool result;
        Nodecl::NodeclBase translated_n = translate_input(n);
        map_node_bool_t::iterator it = non_red_iv_nodes.find(translated_n);
 
        if (it == non_red_iv_nodes.end())
        {
            result = Analysis::AnalysisInterface::
                is_non_reduction_basic_induction_variable(
                        translate_input(scope), translated_n);

            non_red_iv_nodes.insert(pair_node_bool_t(translated_n, result));
        }
        else
        {
            result = it->second;
        }

        return result;
    }
    */

    Nodecl::NodeclBase VectorizationAnalysisInterface::get_induction_variable_lower_bound(
        const Nodecl::NodeclBase& scope,
        const Nodecl::NodeclBase& n)
    {
        std::set<Nodecl::NodeclBase, Nodecl::Utils::Nodecl_structural_less> lower_bounds
                = Analysis::AnalysisInterface::get_induction_variable_lower_bound_list(
                        translate_input(scope), translate_input(n));

        if (lower_bounds.size() != 1)
            return Nodecl::NodeclBase::null();

        //ERROR_CONDITION(lower_bounds.size() != 1,
        //                "Induction variable '%s' has %d lower bounds. "
        //                "Only 1 lower bound supported.\n",
        //                n.prettyprint().c_str(), lower_bounds.size());

        Nodecl::NodeclBase return_nodecl = *lower_bounds.begin();

        return translate_output(return_nodecl);
    }

    /*
    Nodecl::NodeclBase
        VectorizationAnalysisInterface::get_induction_variable_increment(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n )
    {
        Nodecl::NodeclBase return_nodecl =
            Analysis::AnalysisInterface::get_induction_variable_increment(
                translate_input(scope), translate_input(n));

        return translate_output(return_nodecl);
    }
    */

    /*
    objlist_nodecl_t VectorizationAnalysisInterface::get_ivs_nodecls(
                const Nodecl::NodeclBase& scope )
    {
        Analysis::Utils::InductionVarList iv_data_list =
            Analysis::AnalysisInterface::get_induction_variables(
                    translate_input(scope));

        objlist_nodecl_t result;

        for(Analysis::Utils::InductionVarList::const_iterator it = iv_data_list.begin();
                it != iv_data_list.end();
                it ++)
        {
            result.append((*it)->get_variable());
        }

        return translate_output(result);
    }
    */
 
    bool VectorizationAnalysisInterface::is_simd_aligned_access(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n,
            const map_nodecl_int_t& aligned_expressions,
            const objlist_nodecl_t& suitable_expressions,
            int vec_factor, int alignment,
            int& alignment_output)
    {
        //DO NOT TRANSLATE
        map_scope_analysis_info_t::iterator scope_it =
            _scope_analysis_info.find(scope);

        // Already computed
        if (scope_it != _scope_analysis_info.end())
        {
            const map_node_boolint_t& simd_aligned_nodes =
                scope_it->second.simd_aligned_nodes;

            map_node_boolint_t::const_iterator node_it =
                simd_aligned_nodes.find(n);

            if (node_it != simd_aligned_nodes.end())
            {
                alignment_output = node_it->second.second;
                return node_it->second.first;
            }
        }
        else
        {
            scope_it = _scope_analysis_info.insert(
                    pair_scope_analysis_info_t(scope,
                        VectorizationAnalysisInfo())).first;
        }

        // New 
        bool result = is_simd_aligned_access_internal(
                    scope, n, aligned_expressions,
                    suitable_expressions, vec_factor,
                    alignment, alignment_output, this);

        scope_it->second.simd_aligned_nodes.insert(
                pair_node_boolint_t(n, pair_bool_int_t(
                        result, alignment_output)));
        
        return result;
    }

    int VectorizationAnalysisInterface::get_assume_aligned_attribute(
        const Nodecl::NodeclBase& scope,
        const Nodecl::Symbol& n)
    {
        Nodecl::Symbol translated_n =
            translate_input(n).as<Nodecl::Symbol>();
        Nodecl::Symbol translated_scope =
            translate_input(scope).as<Nodecl::Symbol>();

        return Analysis::AnalysisInterface::get_assume_aligned_attribute(
                translated_scope, translated_n);
    }

    bool VectorizationAnalysisInterface::is_suitable_expression(
        const Nodecl::NodeclBase &scope,
        const Nodecl::NodeclBase &n,
        const objlist_nodecl_t &suitable_expressions,
        unsigned int suitable_factor,
        unsigned int vec_factor,
        int &suitable_module)
    {
        // Do not translate n!
        return is_suitable_expression_internal(scope,
                                               n,
                                               suitable_expressions,
                                               suitable_factor,
                                               vec_factor,
                                               suitable_module,
                                               this);
    }

    bool VectorizationAnalysisInterface::is_adjacent_access(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n) 
    {
        Nodecl::NodeclBase translated_n =
            translate_input(n);
        Nodecl::NodeclBase translated_scope =
            translate_input(scope);

        map_scope_analysis_info_t::iterator scope_it =
            _scope_analysis_info.find(
                    translated_scope);

        // Already computed
        if (scope_it != _scope_analysis_info.end())
        {
            const map_node_bool_t& adjacent_nodes =
                scope_it->second.adjacent_nodes;

            map_node_bool_t::const_iterator node_it =
                adjacent_nodes.find(translated_n);

            if (node_it != adjacent_nodes.end())
            {
                return node_it->second;
            }
        }
        else
        {
            scope_it = _scope_analysis_info.insert(
                    pair_scope_analysis_info_t(
                        translated_scope,
                        VectorizationAnalysisInfo())).first;
        }

        // New 
        // Retrieve PCFG
        Analysis::ExtensibleGraph* pcfg = retrieve_pcfg_from_func(
                translated_scope);
        ERROR_CONDITION(pcfg==NULL, "No PCFG found for nodecl %s\n",
                n.prettyprint().c_str());

        // Retrieve nodes from PCFG
        Analysis::Node* n_node = pcfg->find_nodecl_pointer(translated_n);
        ERROR_CONDITION(n_node==NULL, "No PCFG node found for nodecl '%s:%s'. \n", 
                n.get_locus_str().c_str(), n.prettyprint().c_str());
        Analysis::Node* scope_node = pcfg->find_nodecl_pointer(
                translated_scope);
        ERROR_CONDITION(scope_node==NULL, "No PCFG node found for nodecl '%s:%s'. \n",
                scope.get_locus_str().c_str(), scope.prettyprint().c_str());

        bool result = is_adjacent_access_internal(
                scope_node, n_node, translated_n, pcfg);

        scope_it->second.adjacent_nodes.insert(
                pair_node_bool_t(translated_n, result));

        return result;
    }

    void VectorizationAnalysisInterface::register_copy_base(
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& n_copy)
    {
        // Skip List translation
        if (!n.is<Nodecl::List>())
        {
            Nodecl::Utils::NodeclDeepCopyMap::iterator it =
                _orig_to_copy_nodes.find(n);

            // There is equal node in origin and, therefore, in copy
            // Insert the new copy
            if (it != _orig_to_copy_nodes.end())
            {
                _orig_to_copy_nodes.insert(
                        std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(
                            n_copy, it->second));

                // We don't insert a pair in copy_to_origin because it->second
                // is already binded to the original 'n'
                //_copy_to_orig_nodes.insert(
                //        std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(
                //            it->second, n_copy));
            }
            // There is NO equal node in origin
            else
            {
                //internal_error("VectorizerAnalysis: Original node doesn't exist in the copy", 0);
            }
        }
    }

    void VectorizationAnalysisInterface::register_identical_copy(
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& n_copy)
    {
        // Register shallow_copy in the maps
        register_copy_base(n, n_copy);

        // Register also children
        Nodecl::NodeclBase::Children children_list = n.children();
        Nodecl::NodeclBase::Children children_copy_list = n_copy.children();

        for(Nodecl::NodeclBase::Children::iterator children_it = children_list.begin(),
                children_copy_it = children_copy_list.begin();
                children_it != children_list.end();
                children_it++, children_copy_it++)
        {
            if(!children_it->is_null())
            {
                register_identical_copy(*children_it, *children_copy_it);
            }
        }

        // ObjectInit initialization. Special case.
        if (n.is<Nodecl::ObjectInit>())
        {
            TL::Symbol sym = n.get_symbol();
            Nodecl::NodeclBase init = sym.get_value();

            // Register initialization
            if(!init.is_null())
                register_identical_copy(
                        init, n_copy.get_symbol().get_value());
        }
    }

    Nodecl::NodeclBase VectorizationAnalysisInterface::shallow_copy(
            const Nodecl::NodeclBase& n)
    {
        Nodecl::NodeclBase n_copy = n.shallow_copy();
        
        register_identical_copy(n, n_copy);

        return n_copy;
    }
 
    Nodecl::NodeclBase VectorizationAnalysisInterface::deep_copy(
            const Nodecl::NodeclBase& n,
            TL::ReferenceScope ref_scope)
    {
        Nodecl::Utils::SimpleSymbolMap empty_sym_map;
        Nodecl::Utils::NodeclDeepCopyMap new_origin_to_copy_nodes;
        Nodecl::Utils::SymbolDeepCopyMap new_orig_to_copy_symbols;

        Nodecl::NodeclBase n_copy = Nodecl::Utils::deep_copy(n, ref_scope,
                empty_sym_map, new_origin_to_copy_nodes, new_orig_to_copy_symbols);
        
        // Register new Nodecl::Symbols
        register_identical_copy(n, n_copy);

        // Register new TL::Symbols
        for(Nodecl::Utils::SymbolDeepCopyMap::iterator it = 
                new_orig_to_copy_symbols.begin(); 
                it != new_orig_to_copy_symbols.end();
                it++)
        {
            Nodecl::Utils::SymbolDeepCopyMap::iterator found_it =
                _orig_to_copy_symbols.find(it->first);

            // There is equal symbol in origin and, therefore, in copy
            // Insert the new copy
            if (found_it != _orig_to_copy_symbols.end())
            {
                _orig_to_copy_symbols.insert(
                        std::pair<TL::Symbol, TL::Symbol>(
                            it->second, found_it->first));
            }
            // There is NO equal node in origin
            else
            {
                //internal_error("VectorizerAnalysis: Original node doesn't exist in the copy (Deep Copy)", 0);
            }
        }

        return n_copy;
    }

    /*
    bool VectorizationAnalysisInterface::
        is_nested_induction_variable_dependent_access(
            const VectorizerEnvironment& environment,
            const Nodecl::NodeclBase& n)
    {
        for(std::reverse_iterator<stdlist_nodecl_t::const_iterator>
                current_scope(environment._analysis_scopes.end());
                current_scope !=
                std::reverse_iterator<stdlist_nodecl_t::const_iterator>(
                    environment._analysis_scopes.begin());
                current_scope++)
        {
            if((*current_scope) == environment._analysis_simd_scope)
                return false;

            if((*current_scope).is<Nodecl::ForStatement>() ||
                    (*current_scope).is<Nodecl::IfElseStatement>() ||
                    (*current_scope).is<Nodecl::FunctionCode>())
            {
                if(_vectorizer_analysis->
                        is_induction_variable_dependent_expression(
                            *current_scope,
                            n))
                {
                    return true;
                }
            }

        }

        return false;
    }

    bool VectorizationAnalysisInterface::
        is_nested_non_reduction_basic_induction_variable(
            const VectorizerEnvironment& environment,
            const Nodecl::NodeclBase& n)
    {
        for(std::reverse_iterator<stdlist_nodecl_t::const_iterator>
                current_scope(environment._analysis_scopes.end());
                current_scope != std::reverse_iterator<stdlist_nodecl_t::
                const_iterator>(environment._analysis_scopes.begin());
                current_scope++)
        {
            if((*current_scope) == environment._analysis_simd_scope)
                return false;

            if((*current_scope).is<Nodecl::ForStatement>() ||
                    (*current_scope).is<Nodecl::IfElseStatement>() ||
                    (*current_scope).is<Nodecl::FunctionCode>())
            {
                if(_vectorizer_analysis->is_non_reduction_basic_induction_variable(
                            *current_scope,
                            n))
                {
                    return true;
                }
            }
        }

        return false;
    }

    bool VectorizationAnalysisInterface::iv_lb_depends_on_ivs_from_scope(
            const Nodecl::NodeclBase& n_scope,
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& ivs_scope)
    {
        Nodecl::NodeclBase lb_analysis_copy = Analysis::AnalysisInterface::
            get_induction_variable_lower_bound(translate_input(n_scope),
                    translate_input(n));

        bool result = Analysis::AnalysisInterface::
            is_induction_variable_dependent_expression(
                    translate_input(ivs_scope),
                    lb_analysis_copy);

        //unregister_nodes();

        return result;
    }

    bool VectorizationAnalysisInterface::iv_ub_depends_on_ivs_from_scope(
            const Nodecl::NodeclBase& n_scope,
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& ivs_scope)
    {
        Nodecl::NodeclBase ub_analysis_copy = Analysis::AnalysisInterface::
            get_induction_variable_upper_bound(translate_input(n_scope),
                    translate_input(n));

        bool result = Analysis::AnalysisInterface::
            is_induction_variable_dependent_expression(
                    translate_input(ivs_scope),
                    ub_analysis_copy);

        //unregister_nodes();

        return result;
    }

    bool VectorizationAnalysisInterface::iv_step_depends_on_ivs_from_scope(
            const Nodecl::NodeclBase& n_scope,
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& ivs_scope)
    {
        Nodecl::NodeclBase step_analysis_copy = Analysis::AnalysisInterface::
            get_induction_variable_increment(translate_input(n_scope),
                    translate_input(n));

        bool result = Analysis::AnalysisInterface::
            is_induction_variable_dependent_expression(
                    translate_input(ivs_scope),
                    step_analysis_copy);

        //unregister_nodes();

        return result;
    }

    bool VectorizationAnalysisInterface::is_constant(
            const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
    {
        bool result = Analysis::AnalysisInterface::is_constant(
                translate_input(scope), translate_input(n));

        return result;
    }

    bool VectorizationAnalysisInterface::has_been_defined(
            const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& s)
    {
        bool result = Analysis::AnalysisInterface::has_been_defined(
                translate_input(scope), translate_input(n), translate_input(s));

        return result;
    }


    */

#if 0
    DEPRECATED Nodecl::Utils::NodeclDeepCopyMap::iterator
        VectorizationAnalysisInterface::find_equal_nodecl(
            const Nodecl::NodeclBase& n,
            Nodecl::Utils::NodeclDeepCopyMap& map)
    {
        for (Nodecl::Utils::NodeclDeepCopyMap::iterator it = map.begin();
                it != map.end();
                it ++)
        {
            if(Nodecl::Utils::structurally_equal_nodecls(n, it->first,
                        false /*Do no skip conversions*/))
            {
                return it;
            }
        }

        return map.end();
    }

    DEPRECATED Nodecl::NodeclBase VectorizationAnalysisInterface::get_translated_copy(
            const Nodecl::NodeclBase& n)
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

            objlist_nodecl_t children_list = new_node.children();

            for(objlist_nodecl_t::iterator children_it = children_list.begin();
                    children_it != children_list.end();
                    children_it++)
            {
                if(!children_it->is_null())
                    children_it->replace(get_translated_copy(*children_it));
            }

            return new_node;
        }
    }

    DEPRECATED void VectorizationAnalysisInterface::register_node(
            const Nodecl::NodeclBase& n)
    {
        if (_orig_to_copy_nodes.find(n) != _orig_to_copy_nodes.end())
            internal_error("VectorizerAnalysis: Node already registered", 0);

        //TODO: What if we use find_equal_nodecl before registering a node?
        // Why do we need to shallow_copy it?
        // I'm using this approach in translate_output
        Nodecl::Utils::NodeclDeepCopyMap::iterator it =
            find_equal_nodecl(n, _orig_to_copy_nodes);

        // There is equal node in origin and, therefore, in copy
        // Shallow copy it and insert the new copy
        if (it != _orig_to_copy_nodes.end())
        {
            //std::cerr << "Shallow copy " << n.prettyprint() << std::endl;

            Nodecl::NodeclBase sc_copy = it->second.shallow_copy();

            _orig_to_copy_nodes.insert(
                    std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(
                        n, sc_copy));
            _copy_to_orig_nodes.insert(
                    std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(
                        sc_copy, n));
        }
        // There is NO equal node in origin
        // Insert it and create a "translated copy" of it and its children
        else
        {
            //std::cerr << "Making up " << n.prettyprint() << std::endl;
            Nodecl::NodeclBase translated_n = get_translated_copy(n);

            _orig_to_copy_nodes.insert(
                    std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(
                        n, translated_n));
            _copy_to_orig_nodes.insert(
                    std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>(
                        translated_n, n));
        }

        _registered_nodes.push_back(n);
        /*
        // Register also children
        objlist_nodecl_t children_list = n.children();
        for(objlist_nodecl_t::iterator children_it = children_list.begin();
        children_it != children_list.end();
        children_it++)
        {
        if(!children_it->is_null())
        register_node(*children_it);
        }
         */
    }

    // WARNING: if you use this function out of unregister_nodes you should remove the
    // unregistered_node from the _registered_nodes list
    void VectorizationAnalysisInterface::unregister_node(const Nodecl::NodeclBase& n)
    {
        //std::cerr << "Delete " << n.prettyprint() << std::endl;

        Nodecl::Utils::NodeclDeepCopyMap::iterator it =
            _orig_to_copy_nodes.find(n);
        Nodecl::Utils::NodeclDeepCopyMap::iterator it2 =
            _copy_to_orig_nodes.find(it->second);

        _orig_to_copy_nodes.erase(it);
        _copy_to_orig_nodes.erase(it2);
        /*
        // Unregister also children
        objlist_nodecl_t children_list = n.children();
        for(objlist_nodecl_t::iterator children_it = children_list.begin();
        children_it != children_list.end();
        children_it++)
        {
        if(!children_it->is_null())
        unregister_node(*children_it);
        }
         */
    }

    DEPRECATED void VectorizationAnalysisInterface::unregister_nodes()
    {
        for(std::list<Nodecl::NodeclBase>::iterator it =
                _registered_nodes.begin();
                it != _registered_nodes.end();
                it++)
        {
            unregister_node(*it);
        }
        _registered_nodes.clear();
    }
#endif
/*
    bool VectorizationAnalysisInterface::is_basic_induction_variable(
            const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
    {
        bool result = Analysis::AnalysisInterface::is_basic_induction_variable(
                translate_input(scope), translate_input(n));

        return result;
    }

    Nodecl::NodeclBase
        VectorizationAnalysisInterface::get_induction_variable_upper_bound(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n )
    {
        Nodecl::NodeclBase return_nodecl = Analysis::AnalysisInterface::
            get_induction_variable_upper_bound(
                    translate_input(scope), translate_input(n));

        return translate_output(return_nodecl);
    }

    objlist_nodecl_t VectorizationAnalysisInterface::
        get_induction_variable_increment_list(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n )
    {
        objlist_nodecl_t result =
            translate_output(Analysis::AnalysisInterface::
                    get_induction_variable_increment_list(
                        translate_input(scope), translate_input(n)));

        return result;
    }

    bool VectorizationAnalysisInterface::is_induction_variable_increment_one(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n)
    {
        bool result = Analysis::AnalysisInterface::
            is_induction_variable_increment_one(
                translate_input(scope), translate_input(n));

        return result;
    }

    ObjectList<Analysis::Utils::InductionVariableData*>
        VectorizationAnalysisInterface::get_induction_variables(
                const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n )
    {
        internal_error("VectorizerAnalysis: get_induction_variables"\
                "cannot be called from here", 0);

        ObjectList<Analysis::Utils::InductionVariableData*> result =
            Analysis::AnalysisInterface::get_induction_variables(
                    translate_input(scope), translate_input(n));

        return result;
    }

   

    bool VectorizationAnalysisInterface::
        is_induction_variable_dependent_expression(
            const Nodecl::NodeclBase& ivs_scope,
            const Nodecl::NodeclBase& n)
    {
        bool result = Analysis::AnalysisInterface::
            is_induction_variable_dependent_expression(
                    translate_input(ivs_scope), translate_input(n));

        //unregister_nodes();

        return result;
    }

    bool VectorizationAnalysisInterface::contains_induction_variable(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n)
    {
        bool result = Analysis::AnalysisInterface::
            contains_induction_variable(
                translate_input(scope), translate_input(n));

        //unregister_nodes();

        return result;
    }

    bool VectorizationAnalysisInterface::is_constant_access(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n)
    {
        bool result = Analysis::AnalysisInterface::is_constant_access(
                translate_input(scope), translate_input(n));

        //unregister_nodes();

        return result;
    }
    */
}
}
