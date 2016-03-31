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
 
#include "tl-analysis-interface.hpp"

#include "tl-analysis-internals.hpp"
#include "tl-expression-reduction.hpp"
#include "tl-tribool.hpp"

//#include "tl-induction-variables-data.hpp"
//#include "cxx-process.h"
//#include "tl-analysis-utils.hpp"
//#include "tl-analysis-static-info.hpp"
//#include "tl-expression-reduction.hpp"
//#include "tl-use-def.hpp"
 
namespace TL  {
namespace Analysis {

    // ********************************************************************************************* //
    // ********************** Class to define which analysis are to be done ************************ //

    WhichAnalysis::WhichAnalysis( Analysis_tag a )
            : _which_analysis( a )
    {}

    WhichAnalysis::WhichAnalysis( int a )
            : _which_analysis( Analysis_tag( a ) )
    {}

    WhichAnalysis WhichAnalysis::operator|( WhichAnalysis a )
    {
        return WhichAnalysis( int( this->_which_analysis ) | int( a._which_analysis ) );
    }

    // ******************** END class to define which analysis are to be done ********************** //
    // ********************************************************************************************* //

    // ********************************************************************************************* //
    // **************************** User interface for analysis ***************************** //

    AnalysisInterface::AnalysisInterface( ){ }

    AnalysisInterface::AnalysisInterface(
            const Nodecl::NodeclBase& n,
            WhichAnalysis analysis_mask, 
            bool ompss_mode_enabled)
    {
        TL::Analysis::AnalysisBase analysis(ompss_mode_enabled);

        // Compute "dynamic" analysis
        // Do it in such an order that the first is the most complete analysis and the last is the simplest one
        if( analysis_mask._which_analysis & WhichAnalysis::AUTO_SCOPING )
        {
            analysis.auto_scoping(n);
        }
        if( analysis_mask._which_analysis & WhichAnalysis::REACHING_DEFS_ANALYSIS )
        {
            analysis.reaching_definitions(n, /*propagate_graph_nodes*/ false);
        }
        if( analysis_mask._which_analysis & WhichAnalysis::INDUCTION_VARS_ANALYSIS )
        {
            analysis.induction_variables(n, /*propagate_graph_nodes*/ false);
        }
        if( analysis_mask._which_analysis & WhichAnalysis::LIVENESS_ANALYSIS )
        {
            analysis.liveness(n, /*propagate_graph_nodes*/ false);
        }
        if( analysis_mask._which_analysis & ( WhichAnalysis::USAGE_ANALYSIS |
                                              WhichAnalysis::CONSTANTS_ANALYSIS ) )
        {
            analysis.use_def(n, /*propagate_graph_nodes*/ false);
        }
        if( analysis_mask._which_analysis & WhichAnalysis::PCFG_ANALYSIS )
        {
            analysis.parallel_control_flow_graph(n);
        }

        if (debug_options.print_pcfg ||
            debug_options.print_pcfg_w_context ||
            debug_options.print_pcfg_w_analysis ||
            debug_options.print_pcfg_full)
            analysis.print_all_pcfg();

        // Fill nodecl to pcfg map
        const ObjectList<ExtensibleGraph*>& pcfgs = analysis.get_pcfgs();
        for(ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            Nodecl::NodeclBase func_nodecl = (*it)->get_nodecl();

            // If SimdFunction, we better associate nested
            // FunctionCode with PCFG
            if (func_nodecl.is<Nodecl::OpenMP::SimdFunction>())
                func_nodecl = func_nodecl.as<Nodecl::OpenMP::SimdFunction>().
                    get_statement();

            _func_to_pcfg_map[func_nodecl] = *it;
        }
    }

    AnalysisInterface::~AnalysisInterface( ) {}

    Node* AnalysisInterface::retrieve_scope_node_from_nodecl(
            const Nodecl::NodeclBase& scope,
            ExtensibleGraph* pcfg) 
    {
        nodecl_to_node_map_t::const_iterator it = _scope_nodecl_to_node_map.find(scope);

        if(it != _scope_nodecl_to_node_map.end())
        {
            return it->second;
        }
        else // Insert new scope in the map
        {
            Node* scope_node = pcfg->find_nodecl_pointer(scope);
            ERROR_CONDITION(scope_node==NULL, "No PCFG node found for scope Nodecl (%p)'%s:%s'. \n",
                    nodecl_get_ast(scope.get_internal_nodecl()), scope.get_locus_str().c_str(),
                    scope.prettyprint().c_str());

            _scope_nodecl_to_node_map.insert(nodecl_to_node_pair_t(scope, scope_node));

            return scope_node;
        }
    }

    ExtensibleGraph* AnalysisInterface::retrieve_pcfg_from_func(const Nodecl::NodeclBase& n) const
    {
        TL::Symbol func_sym = Nodecl::Utils::get_enclosing_function(n);

        if(func_sym.is_valid() && n.is<Nodecl::FunctionCode>())
        {
            WARNING_MESSAGE("FunctionCode nested in another FunctionCode. Retrieving enclosing FunctionCode PCFG Node.\n", 0);
        }

        // If n is a FunctionCode, maybe could be FunctionCode nested in another FunctionCode
        if(!func_sym.is_valid() && n.is<Nodecl::FunctionCode>())
            func_sym = n.as<Nodecl::FunctionCode>().get_symbol();

        if (!func_sym.is_valid())
        {
            func_sym = Nodecl::Utils::get_enclosing_function(n);
        }
 
        ERROR_CONDITION(!func_sym.is_valid(), 
                "Invalid Nodecl '%s' on expecting non top-level nodecls\n", n.prettyprint().c_str());

        Nodecl::NodeclBase func = func_sym.get_function_code();

        // TODO Check 'func' is a valid nodecl

        nodecl_to_pcfg_map_t::const_iterator it = _func_to_pcfg_map.find(func);
        ERROR_CONDITION(it == _func_to_pcfg_map.end(), 
                "No PCFG found corresponding to function %s\n", func.get_symbol().get_name().c_str());
        return it->second;
    }

    bool AnalysisInterface::is_uniform(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& stmt,
            const Nodecl::NodeclBase& n)
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);

        // Retrieve stmt node
        Node* stmt_node = pcfg->find_nodecl_pointer(stmt);
        ERROR_CONDITION(stmt_node==NULL, "No PCFG node found for statement '%s:%s'. \n",
                stmt.get_locus_str().c_str(), stmt.prettyprint().c_str());

        // Retrieve node
        Node* n_node = pcfg->find_nodecl_pointer(n);
        ERROR_CONDITION(n_node==NULL, "No PCFG node found for n Nodecl '%s:%s'. \n",
                n.get_locus_str().c_str(), n.prettyprint().c_str());

        std::set<Nodecl::NodeclBase> visited_nodes;

        //has_property implements is_uniform so far
        return is_uniform_internal(scope_node, stmt_node, n,
                pcfg, visited_nodes);
    }

    bool AnalysisInterface::is_linear(
            const Nodecl::NodeclBase& scope, 
            const Nodecl::NodeclBase& n)
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);
        
        return is_linear_internal(scope_node, n);
    }
    
    bool AnalysisInterface::is_induction_variable(
            const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);

        return is_iv_internal(scope_node, n);
    }


    bool AnalysisInterface::is_non_reduction_basic_induction_variable(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n ) 
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);

        return is_non_reduction_basic_iv_internal(scope_node, n);
    }

    NodeclSet AnalysisInterface::get_induction_variable_lower_bound_list(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n )
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);

        return get_iv_lower_bound_internal(scope_node, n);
    }

    Nodecl::NodeclBase AnalysisInterface::get_induction_variable_increment(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n )
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);

        return get_iv_increment_internal(scope_node, n);
    }

    Utils::InductionVarList AnalysisInterface::get_induction_variables(
            const Nodecl::NodeclBase& scope)
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);

        return scope_node->get_induction_variables();
    }

    Utils::InductionVarList AnalysisInterface::get_linear_variables(
        const Nodecl::NodeclBase& scope)
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);
        
        return get_linear_variables_internal(scope_node);
    }
    
    NodeclSet AnalysisInterface::get_linear_variable_lower_bound(
            const NBase& scope, 
            const NBase& n)
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);
        
        return get_linear_variable_lower_bound_internal(scope_node, n);
    }
    
    NBase AnalysisInterface::get_linear_variable_increment(
            const NBase& scope, 
            const NBase& n)
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve scope
        Node* scope_node = retrieve_scope_node_from_nodecl(scope, pcfg);
        
        return get_linear_variable_increment_internal(scope_node, n);
    }

    int AnalysisInterface::get_assume_aligned_attribute(
            const NBase& scope, 
            const Nodecl::Symbol& n) 
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        // Retrieve node
        Node* n_node = pcfg->find_nodecl_pointer(n);
        ERROR_CONDITION(n_node==NULL, "No PCFG node found for n Nodecl '%s:%s'. \n",
                n.get_locus_str().c_str(), n.prettyprint().c_str());


        return get_assume_aligned_attribute_internal(n_node, n);
    }
    
    static bool nodecl_calls_outline_task( const Nodecl::NodeclBase& n, std::shared_ptr<OmpSs::FunctionTaskSet> function_tasks )
    {
        if( n.is_null( ) )
            return false;

        bool result = false;

        // Check the current node
        if( n.is<Nodecl::FunctionCall>( ) )
        {
            Symbol s( n.as<Nodecl::FunctionCall>( ).get_called( ).get_symbol( ) );
            if( s.is_valid( ) && function_tasks->is_function_task( s ) )
                result = true;
        }

        // Check its children
        Nodecl::NodeclBase::Children children = n.children( );
        for( Nodecl::NodeclBase::Children::iterator it = children.begin( ); it != children.end( ) && !result; ++it )
        {
            result = nodecl_calls_outline_task( *it, function_tasks );
        }

        return result;
    }

    static bool ompss_reduction_rhs_uses_lhs( const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& lhs,
                                              std::shared_ptr<OmpSs::FunctionTaskSet> function_tasks )
    {
        if( n.is_null( ) || n.is<Nodecl::ArraySubscript>( ) ||
            ( n.is<Nodecl::FunctionCall>( ) && ( !n.as<Nodecl::FunctionCall>( ).get_called( ).get_symbol( ).is_valid( ) ||
                                                 !function_tasks->is_function_task( n.as<Nodecl::FunctionCall>( ).get_called( ).get_symbol( ) ) ) ) )
            return false;

        // Check the current node
        if( Nodecl::Utils::structurally_equal_nodecls( n, lhs, /*skip conversion nodes*/ true ) )
            return true;

        // Check the children
        bool result = false;
        Nodecl::NodeclBase::Children children = n.children( );
        for( Nodecl::NodeclBase::Children::iterator it = children.begin( ); it != children.end( ) && !result; ++it )
        {
            result = ompss_reduction_rhs_uses_lhs( *it, lhs, function_tasks );
        }
        return result;
    }

    bool AnalysisInterface::is_ompss_reduction( const Nodecl::NodeclBase& n, std::shared_ptr<OmpSs::FunctionTaskSet> function_tasks ) const
    {
        bool result = false;

        if( n.is<Nodecl::Assignment>( ) ||
            n.is<Nodecl::AddAssignment>( ) || n.is<Nodecl::MinusAssignment>( ) ||
            n.is<Nodecl::DivAssignment>( ) || n.is<Nodecl::MulAssignment>( ) || n.is<Nodecl::ModAssignment>( ) ||
            n.is<Nodecl::BitwiseShlAssignment>( ) || n.is<Nodecl::BitwiseShrAssignment>( ) || n.is<Nodecl::ArithmeticShrAssignment>( ) ||
            n.is<Nodecl::BitwiseAndAssignment>( ) || n.is<Nodecl::BitwiseOrAssignment>( ) || n.is<Nodecl::BitwiseXorAssignment>( ) )
        {
            Nodecl::Assignment n_assig = n.as<Nodecl::Assignment>( );
            result = nodecl_calls_outline_task( n_assig.get_rhs( ), function_tasks );

            if( result && n.is<Nodecl::Assignment>( ) )
            {   // Check also if the LHS also contains the RHS
                Nodecl::NodeclBase rhs_c = n_assig.get_rhs( ).shallow_copy( );
                //TODO: This should be applied before calling the analysis 
                // if PCFG is used at some point in the future.
                TL::Optimizations::ReduceExpressionVisitor rev;
                rev.walk( rhs_c );
                if( !ompss_reduction_rhs_uses_lhs( rhs_c, n_assig.get_lhs( ), function_tasks ) )
                    result = false;
            }
        }

        return result;
    }

    bool AnalysisInterface::has_been_defined( const Nodecl::NodeclBase& n )
    {
        // Retrieve pcfg
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(n);

        // Retrieve node
        Node* n_node = pcfg->find_nodecl_pointer(n);
        ERROR_CONDITION(n_node==NULL, "No PCFG node found for Nodecl '%s'. \n",
                n.get_locus_str().c_str());

        return has_been_defined_internal(n_node, n, pcfg->get_global_variables());
    }
}
}
