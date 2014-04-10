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
 
#include "cxx-process.h"
#include "tl-analysis-utils.hpp"
#include "tl-analysis-static-info.hpp"
#include "tl-expression-reduction.hpp"
#include "tl-use-def.hpp"
 
namespace TL  {
namespace Analysis {
 
    /* Example:
     *
     * if( i < 5 ) k = N;
     * else k = M;
     *
     * k; <--- target
     *
     * reach_defs_depends_on_iv analyzes reaching definitions, 'N' and 'M' in the example,
     * and their enclosing scopes, 'if( i < 5 )' in the example, to determine if any IV
     * is involved in the evolution of 'k'.
     *
     * In this example, being 'i' and IV, the result is 'true'.
     * Note the different between this query and reach_defs_contain_iv, where the result
     * would be 'false'.
     */
 
    // Beter name: evolution_depends_on_iv??
    static bool reach_defs_depend_on_iv_rec(const Nodecl::NodeclBase& n, const ObjectList<Nodecl::NodeclBase>& ivs, ExtensibleGraph* pcfg)
    {
        // Get reaching definitions for 'n' in its corresponding node
        Node* n_node = pcfg->find_nodecl_pointer(n);
        Utils::ext_sym_map reach_defs = n_node->get_reaching_definitions_in(n);
        
        // Get the PCFG nodes where the reaching definitions where produced
        ObjectList<Node*> reach_defs_nodes;
        for(Utils::ext_sym_map::iterator it = reach_defs.begin(); it != reach_defs.end(); ++it)
            reach_defs_nodes.append(pcfg->find_nodecl_pointer(it->second));
        
        // For each reaching definition node:
        // 1.- check whether any of its outer nodes depend on an induction variable
        bool depends_on_iv = false;
        for(ObjectList<Node*>::iterator it = reach_defs_nodes.begin(); it != reach_defs_nodes.end(); ++it)
        {
            Node* outer_node = (*it)->get_outer_node();
            while(outer_node!=NULL)
            {
                if(outer_node->is_ifelse_statement() || outer_node->is_loop_node())
                {
                    Node* cond = outer_node->get_condition_node();
                    ObjectList<Nodecl::NodeclBase> stmts = cond->get_statements();
                    for(ObjectList<Nodecl::NodeclBase>::iterator itt = stmts.begin(); itt != stmts.end(); ++itt)
                    {
                        for(ObjectList<Nodecl::NodeclBase>::const_iterator ittt = ivs.begin(); ittt != ivs.end(); ++ittt)
                        {
                            if(Nodecl::Utils::stmtexpr_contains_nodecl_structurally(*itt, *ittt))
                            {
                                depends_on_iv = true;
                                goto end_depends;
                            }
                        }
                    }
                }
            }
        }
        // 2.- otherwise, recursively check the variables in the RHS of the reaching definition
        for(Utils::ext_sym_map::iterator it = reach_defs.begin(); it != reach_defs.end(); ++it)
        {
            ObjectList<Nodecl::NodeclBase> vars = Nodecl::Utils::get_all_memory_accesses(it->second);
            for(ObjectList<Nodecl::NodeclBase>::iterator itt = vars.begin(); itt != vars.end(); ++itt)
            {
                if(reach_defs_depend_on_iv_rec(*itt, ivs, pcfg))
                {
                    depends_on_iv = true;
                    goto end_depends;
                }
            }
        }
        
end_depends:
        return depends_on_iv;
    }
 
    DEPRECATED bool AnalysisStaticInfo::reach_defs_depend_on_iv(const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
    {
        bool result = false;
 
        Nodecl::NodeclBase func = Nodecl::Utils::get_enclosing_function(scope).get_function_code();
        // TODO Check 'func' is a valid nodecl
        ERROR_CONDITION(_nodecl_to_pcfg_map.find(func)== _nodecl_to_pcfg_map.end(), 
                        "Node PCFG found corresponding to function %s\n", func.get_symbol().get_name().c_str());
        ExtensibleGraph* pcfg = _nodecl_to_pcfg_map[func];
        
        // Get the induction variables involved in the scope
        Node* scope_node = pcfg->find_nodecl_pointer(scope);
        if(!scope_node->is_loop_node())
            return result;
        ObjectList<Utils::InductionVariableData*> ivs = scope_node->get_induction_variables();
        if(ivs.empty())
            return result;
        ObjectList<Nodecl::NodeclBase> nodecl_ivs;
        for(ObjectList<Utils::InductionVariableData*>::iterator it = ivs.begin(); it != ivs.end(); ++it)
            nodecl_ivs.append((*it)->get_variable().get_nodecl());
        
        result = reach_defs_depend_on_iv_rec(n, nodecl_ivs, pcfg);
        
        return result;
    }
    
    bool AnalysisStaticInfo::variable_is_constant_at_statement(const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n)
    {
        Node* stmt_node = pcfg->find_nodecl_pointer(n);
        ERROR_CONDITION(stmt_node==NULL, "No PCFG node found for nodecl '%s:%s'. \n", n.get_locus_str().c_str(), n.prettyprint().c_str());
        Node* scope_node = pcfg->find_nodecl_pointer(scope);
        ERROR_CONDITION(scope_node==NULL, "No PCFG node found for nodecl '%s:%s'. \n", scope.get_locus_str().c_str(), scope.prettyprint().c_str());
        
        Utils::ext_sym_map reach_defs_in = stmt_node->get_reaching_definitions_in();
        ERROR_CONDITION(reach_defs_in.find(n)==reach_defs_in.end(), 
                        "Node reaching definition arrives for nodecl %s.\n", n.prettyprint().c_str());
        
        std::pair<Utils::ext_sym_map::iterator, Utils::ext_sym_map::iterator> bounds = reach_defs_in.equal_range(n);
        Utils::ext_sym_map::iterator rd_it = bounds.first;
        while(rd_it != bounds.second)
        {
            // Get the PCFG nodes where the reaching definitions where produced
            Node* reach_defs_node = pcfg->find_nodecl_pointer(reach_defs_in[n]);
            if(ExtensibleGraph::node_contains_node(scope_node, stmt_node))
            {
                Node* control_structure = get_enclosing_control_structure(reach_defs_node);
                if((control_structure != NULL) && (control_structure != scope_node))
                {
                    Node* cond_node = control_structure->get_condition_node();
                    ObjectList<Nodecl::NodeclBase> stmts = cond_node->get_statements();
                    for(ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                    {
                        ObjectList<Nodecl::NodeclBase> mem_accesses = Nodecl::Utils::get_all_memory_accesses()
                        for(ObjectList<Nodecl::NodeclBase>::iterator itt = mem_accesses.begin(); it != mem_accesses.end(); ++itt)
                        {
                            if(!is_constant(scope, *itt) || !variable_is_constant_at_statement(scope, *itt))
                                return false;
                        }
                    }
                }
            }

            ++rd_it;
        }
        
        return true;
    }
}
}