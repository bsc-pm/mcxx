/*--------------------------------------------------------------------
(C) Copyright 2006-2013 Barcelona Supercomputing Center             *
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
#include "tl-range-analysis.hpp"

#include <algorithm>

namespace TL {
namespace Analysis {
    
    //! This maps stores the relationship between each variable in a given node and 
    //! the last identifier used to create a constraint for that variable
    std::map<Nodecl::NodeclBase, unsigned int, Nodecl::Utils::NodeclLess> var_to_last_constraint_id;
    
    // **************************************************************************************************** //
    // ******************************** Class implementing range analysis ********************************* //

namespace {
    unsigned int get_next_id(const Nodecl::NodeclBase& n)
    {
        unsigned int next_id = 0;
        if(var_to_last_constraint_id.find(n) != var_to_last_constraint_id.end())
            next_id = var_to_last_constraint_id[n] + 1;
        var_to_last_constraint_id[n] = next_id;
        return next_id;
    }
}
    
    ConstraintBuilderVisitor::ConstraintBuilderVisitor(Utils::ConstraintMap input_constraints, 
                                                       Utils::ConstraintMap current_constraints)
        : _input_constraints(input_constraints), _output_constraints(current_constraints), 
          _output_true_constraints(), _output_false_constraints()
    {}
    
    void ConstraintBuilderVisitor::compute_constraints(const Nodecl::NodeclBase& n)
    {
        walk(n);
    }
    
    Utils::ConstraintMap ConstraintBuilderVisitor::get_output_constraints()
    {
        return _output_constraints;
    }

    Utils::ConstraintMap ConstraintBuilderVisitor::get_output_true_constraints()
    {
        return _output_true_constraints;
    }
    
    Utils::ConstraintMap ConstraintBuilderVisitor::get_output_false_constraints()
    {
        return _output_false_constraints;
    }
    
    Utils::Constraint ConstraintBuilderVisitor::join_list(TL::ObjectList<Utils::Constraint>& list)
    {
        Utils::Constraint result = list[0];
        WARNING_MESSAGE("join_list of a list of constraint is not yet supported. Doing nothing.", 0);
        return result;    
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::Assignment& n)
    {
        Utils::Constraint c;
        
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if(rhs.is_constant()) 
        {   // x = c;    -->    X1 = c
            // Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(lhs);
            std::string constr_name = Utils::get_nodecl_base(lhs).get_symbol().get_name() + "_" + ss.str();
            Symbol s(n.retrieve_context().new_symbol(constr_name));
            c = Utils::Constraint(s, rhs);
        }
        else 
        {
            ObjectList<Nodecl::NodeclBase> rhs_mem_access = Nodecl::Utils::get_all_memory_accesses(rhs);
            for(ObjectList<Nodecl::NodeclBase>::iterator it = rhs_mem_access.begin(); it != rhs_mem_access.end(); ++it)
            {
                if(_input_constraints.find(*it) != _input_constraints.end())
                {
                    // TODO Replace *it in constr_rhs with constraints[0]
                    Utils::Constraint constraint = _input_constraints[*it];
                    internal_error("Constraints found for variable %s. We should replace the variable but this is not yet implemented.", 
                                    it->prettyprint().c_str());
                }
                else
                {
                    std::stringstream ss; ss << get_next_id(lhs);
                    std::string constr_name = Utils::get_nodecl_base(lhs).get_symbol().get_name() + "_" + ss.str();
                    Symbol s(n.retrieve_context().new_symbol(constr_name));
                    c = Utils::Constraint(s, rhs);
                }
            }
        }

        _input_constraints[lhs] = c;
        _output_constraints[lhs] = c;
        
        return c;
    }
    
    // x < c;    ---TRUE-->    X1 = X0 ∩ [-∞, c-1]
    //           --FALSE-->    X1 = X0 ∩ [ c,  +∞]
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::LowerThan& n)
    {
        Utils::Constraint c;
        
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        
        ERROR_CONDITION(_input_constraints.find(lhs) == _input_constraints.end(), 
                        "Some input constraint required for the RHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));

        // TODO Check whether we have more than one input constraint for RHS
        if(rhs.is_constant()) 
        {   // x = c;    -->    X1 = c            
            Utils::Constraint constraint = _input_constraints[lhs];
            
            // Compute the constraint that corresponds to the current node
            std::stringstream ss; ss << get_next_id(lhs);
            std::string constr_name = Utils::get_nodecl_base(lhs).get_symbol().get_name() + "_" + ss.str();
            Symbol s(n.retrieve_context().new_symbol(constr_name));
            Nodecl::NodeclBase val = Nodecl::Symbol::make(constraint.get_symbol());
            c = Utils::Constraint(s, val);
            
            // Compute the constraint that corresponds to the true branch taken from this node
            std::stringstream ss_true; ss_true << get_next_id(lhs);
            std::string constr_name_true = Utils::get_nodecl_base(lhs).get_symbol().get_name() + "_" + ss_true.str();
            Symbol s_true(n.retrieve_context().new_symbol(constr_name_true));
            Nodecl::NodeclBase val_true = 
                Nodecl::Analysis::RangeIntersection::make(
                    Nodecl::Symbol::make(s), 
                    Nodecl::Analysis::Range::make(Nodecl::Analysis::MinusInfinity::make(),  
                                                  const_value_to_nodecl(const_value_sub(rhs.get_constant(), const_value_get_one(/*num_bytes*/ 4, /*sign*/1))),
                                                  lhs.get_type()), 
                    lhs.get_type());
            Utils::Constraint c_true = Utils::Constraint(s_true, val_true);
            _output_true_constraints[lhs] = c_true;
            
            // Compute the constraint that corresponds to the false branch taken from this node
            std::stringstream ss_false; ss_false << get_next_id(lhs);
            std::string constr_name_false = Utils::get_nodecl_base(lhs).get_symbol().get_name() + "_" + ss_false.str();
            Symbol s_false(n.retrieve_context().new_symbol(constr_name_false));
            Nodecl::NodeclBase val_false = 
                Nodecl::Analysis::RangeIntersection::make(
                    Nodecl::Symbol::make(s), 
                    Nodecl::Analysis::Range::make(rhs.shallow_copy(),
                                                  Nodecl::Analysis::PlusInfinity::make(),
                                                  lhs.get_type()), 
                    lhs.get_type());
            Utils::Constraint c_false = Utils::Constraint(s_false, val_false);
            _output_false_constraints[lhs] = c_false;
        }
        else
        {
            ObjectList<Nodecl::NodeclBase> rhs_mem_access = Nodecl::Utils::get_all_memory_accesses(rhs);
            for(ObjectList<Nodecl::NodeclBase>::iterator it = rhs_mem_access.begin(); it != rhs_mem_access.end(); ++it)
            {
                if(_input_constraints.find(*it) != _input_constraints.end())
                {
                    // TODO
                }
            }
        }
        
        _input_constraints[lhs] = c;
        _output_constraints[lhs] = c;
        
        return c;
    }
    
    // ++x;    -->    X1 = X0 + 1
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::Preincrement& n)
    {
        Utils::Constraint c;
        
        Nodecl::NodeclBase rhs = n.get_rhs();
        ERROR_CONDITION(_input_constraints.find(rhs) == _input_constraints.end(), 
                        "Some input constraint required for the RHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        // TODO Check whether we have more than one input constraint for RHS
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(rhs);
        std::string constr_name = Utils::get_nodecl_base(rhs).get_symbol().get_name() + "_" + ss.str();
        Symbol s(n.retrieve_context().new_symbol(constr_name));
        
        Utils::Constraint constraint = _input_constraints[rhs];
        Nodecl::NodeclBase val = Nodecl::Add::make(Nodecl::Symbol::make(constraint.get_symbol()), 
                                                   const_value_to_nodecl(const_value_get_one(/*num_bytes*/ 4, /*sign*/1)), 
                                                   rhs.get_type());
        
        c = Utils::Constraint(s, val);
        _input_constraints[rhs] = c;
        _output_constraints[rhs] = c;
        
        return c;
    }


    RangeAnalysis::RangeAnalysis(ExtensibleGraph* graph)
        : _graph(graph)
    {}
    
    void RangeAnalysis::compute_range_analysis()
    {
        // Compute the constraints of the current graph
        Node* entry = _graph->get_graph()->get_graph_entry_node();
        compute_initial_constraints(entry);
        ExtensibleGraph::clear_visits(entry);
        propagate_constraints_from_backwards_edges(entry);
        ExtensibleGraph::clear_visits(entry);
    }
    
    void RangeAnalysis::compute_initial_constraints(Node* n)
    {
        if(n->is_visited())
            return;
        
        n->set_visited(true);
        
        // Treat the current node
        if(n->is_graph_node())
        {
            // recursively compute the constraints for the inner nodes
            compute_initial_constraints(n->get_graph_entry_node());
            
            // propagate constraint from the inner nodes (summarized in the exit node) to the graph node
            n->set_propagated_constraints(n->get_graph_exit_node()->get_propagated_constraints());
        }
        else
        {
            // Collect the constraints computed from all the parents
            Utils::ConstraintMap input_constraints;
            ObjectList<Node*> parents = (n->is_entry_node() ? n->get_outer_node()->get_parents() : n->get_parents());
            for(ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            {
                Utils::ConstraintMap it_constraints = (*it)->get_all_constraints();
                for(Utils::ConstraintMap::iterator itt = it_constraints.begin(); itt != it_constraints.end(); ++itt)
                {
                    if(input_constraints.find(itt->first)==input_constraints.end())
                    {   // No constraints already found for variable itt->first
                        input_constraints[itt->first] = itt->second;
                    }
                    else
                    {   // Merge the constraint that already existed with the new one
                        Nodecl::NodeclBase constraint = input_constraints[itt->first].get_constraint();
                        if(constraint.is<Nodecl::Analysis::Phi>())
                        {   // Attach a new element to the list inside the node Phi
                            Nodecl::List expressions = constraint.as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                            expressions.append(itt->second.get_constraint());
                            constraint.as<Nodecl::Analysis::Phi>().set_expressions(expressions);
                        }
                        else
                        {   // Create a new node Phi with the combination of the old constraint and the new one
                            Nodecl::List expressions = Nodecl::List::make(constraint.shallow_copy(), itt->second.get_constraint().shallow_copy());
                            constraint = Nodecl::Analysis::Phi::make(expressions, itt->second.get_constraint().get_type());
                        }
                        input_constraints[itt->first] = Utils::Constraint(input_constraints[itt->first].get_symbol(), constraint);
                    }
                }
            }
            
            // Propagate constraints from parent nodes to the current node
            n->set_propagated_constraints(input_constraints);
            
            // Compute the constraints of the current node
            // Note: take into account the constraints the node may already has (if it is the TRUE or FALSE child of a conditional)
            n->set_propagated_constraints(input_constraints);
            Utils::ConstraintMap current_constraints = n->get_constraints();
            ConstraintBuilderVisitor cbv(input_constraints, current_constraints);
            ObjectList<Nodecl::NodeclBase> stmts = n->get_statements();
            for(ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                cbv.compute_constraints(*it);
            n->set_constraints(cbv.get_output_constraints());
            
            // Set true/false output constraints to current children, if applies
            ObjectList<Edge*> exits = n->get_exit_edges();
            if(exits.size()==2 &&
               ((exits[0]->is_true_edge() && exits[1]->is_false_edge()) || (exits[1]->is_true_edge() && exits[0]->is_false_edge())))
            {
                Utils::ConstraintMap out_true_constraints = cbv.get_output_true_constraints();
                Utils::ConstraintMap out_false_constraints = cbv.get_output_false_constraints();
                
                Node* true_node = (exits[0]->is_true_edge() ? exits[0]->get_target() : exits[1]->get_target());
                while(true_node->is_exit_node())
                    true_node = true_node->get_outer_node()->get_children()[0];
                Node* false_node = (exits[0]->is_true_edge() ? exits[1]->get_target() : exits[0]->get_target());
                while(false_node->is_exit_node())
                    false_node = false_node->get_outer_node()->get_children()[0];
                
                true_node->add_constraints(out_true_constraints);
                false_node->add_constraints(out_false_constraints);
            }
        }
        
        // Treat the children
        ObjectList<Node*> children = n->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            compute_initial_constraints(*it);
    }
    
    void RangeAnalysis::propagate_constraints_from_backwards_edges(Node* n)
    {
        if(n->is_visited())
            return;
        n->set_visited(true);
        
        if(n->is_graph_node())
            propagate_constraints_from_backwards_edges(n->get_graph_entry_node());
        
        // Check the exit edges looking for back edges
        ObjectList<Edge*> exit_edges = n->get_exit_edges();
        for(ObjectList<Edge*>::iterator it = exit_edges.begin(); it != exit_edges.end(); ++it)
        {
            if((*it)->is_back_edge())
                recompute_node_constraints((*it)->get_target(), n->get_constraints());
        }
        
        // Recursively treat the children
        ObjectList<Node*> children = n->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            propagate_constraints_from_backwards_edges(*it);
    }

    void RangeAnalysis::recompute_node_constraints(Node* n, Utils::ConstraintMap new_constraint_map)
    {
        Utils::ConstraintMap current_constraint_map = n->get_constraints();
        for(Utils::ConstraintMap::iterator it = new_constraint_map.begin(); it != new_constraint_map.end(); ++it)
        {
            if(current_constraint_map.find(it->first) != current_constraint_map.end())
            {
                Nodecl::NodeclBase c1 = current_constraint_map[it->first].get_constraint().shallow_copy();
                Nodecl::NodeclBase c2 = Nodecl::Symbol::make(it->second.get_symbol());
                Nodecl::List expressions = Nodecl::List::make(c1, c2);
                Nodecl::NodeclBase c = Nodecl::Analysis::Phi::make(expressions, it->first.get_type());
                // Set the new constraint
                current_constraint_map[it->first] = Utils::Constraint(current_constraint_map[it->first].get_symbol(), c);
            }
        }
        n->set_constraints(current_constraint_map);
    }
    
    // ****************************** End class implementing range analysis ******************************* //
    // **************************************************************************************************** //

}
}