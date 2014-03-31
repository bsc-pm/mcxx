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
#include "cxx-process.h"
#include "tl-range-analysis.hpp"

#include <algorithm>

namespace TL {
namespace Analysis {
    
    // **************************************************************************************************** //
    // **************************** Visitor implementing constraint building ****************************** //
    
    namespace {
        //! This maps stores the relationship between each variable in a given node and 
        //! the last identifier used to create a constraint for that variable
        std::map<Nodecl::NodeclBase, unsigned int, Nodecl::Utils::Nodecl_structural_less> var_to_last_constraint_id;
        
        unsigned int get_next_id(const Nodecl::NodeclBase& n)
        {
            unsigned int next_id = 0;
            if(var_to_last_constraint_id.find(n) != var_to_last_constraint_id.end())
                next_id = var_to_last_constraint_id[n] + 1;
            var_to_last_constraint_id[n] = next_id;
            return next_id;
        }
    }
    
    ConstraintReplacement::ConstraintReplacement(Utils::ConstraintMap constraints)
        : _constraints(constraints)
    {}
    
    void ConstraintReplacement::visit(const Nodecl::ArraySubscript& n)
    {
        if(_constraints.find(n) != _constraints.end())
            n.replace(Nodecl::Symbol::make(_constraints[n].get_symbol()));
        else
        {
            walk(n.get_subscripted());
            walk(n.get_subscripts());
        }
    }
    
    void ConstraintReplacement::visit(const Nodecl::ClassMemberAccess& n)
    {
        if(_constraints.find(n) != _constraints.end())
            n.replace(Nodecl::Symbol::make(_constraints[n].get_symbol()));
        else
        {
            walk(n.get_lhs());
            walk(n.get_member());
        }
    }
    
    void ConstraintReplacement::visit(const Nodecl::Symbol& n)
    {
        ERROR_CONDITION(_constraints.find(n) == _constraints.end(),
                        "No constraints found for symbol %s in locus %s. "
                        "We should replace the variable with the corresponding constraint.", 
                        n.prettyprint().c_str(), n.get_locus_str().c_str());
        
        n.replace(Nodecl::Symbol::make(_constraints[n].get_symbol()));
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
    
    Utils::Constraint ConstraintBuilderVisitor::visit_assignment(const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs)
    {
        Utils::Constraint c;
        
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(lhs);
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        std::string constr_name = orig_s.get_name() + "_" + ss.str();
        Symbol s(lhs.retrieve_context().new_symbol(constr_name));
        s.set_type(orig_s.get_type());
        // Build the value of the constraint
        Nodecl::NodeclBase val;
        if(rhs.is_constant())       // x = c;    -->    X1 = c
            val = rhs;
        else 
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints);
            val = rhs.shallow_copy();
            cr.walk(val);
        }
        
        // Build the constraint and insert it in the corresponding maps
        c = Utils::Constraint(s, val);
        _input_constraints[lhs] = c;
        _output_constraints[lhs] = c;
        
        return c;
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::AddAssignment& n)
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        Nodecl::NodeclBase new_rhs = Nodecl::Assignment::make(lhs.shallow_copy(), 
                                                              Nodecl::Add::make(lhs.shallow_copy(), rhs.shallow_copy(), rhs.get_type()), 
                                                              lhs.get_type());
        n.replace(new_rhs);
        return visit_assignment(n.get_lhs(), n.get_rhs());
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::Assignment& n)
    {
        return visit_assignment(n.get_lhs(), n.get_rhs());
    }
    
    // x < c;    ---TRUE-->    X1 = X0 ∩ [-∞, c-1]
    //           --FALSE-->    X1 = X0 ∩ [ c,  +∞]
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::LowerThan& n)
    {
        Utils::Constraint c;
        Nodecl::NodeclBase val;
        
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        
        // Check the input is something we expect: LHS has a constraint or is a parameter
        ERROR_CONDITION(_input_constraints.find(lhs) == _input_constraints.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));

        // Get a new symbol for the new constraint
        std::stringstream ss; ss << get_next_id(lhs);
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        std::string constr_name = orig_s.get_name() + "_" + ss.str();
        Symbol s(n.retrieve_context().new_symbol(constr_name));
        s.set_type(orig_s.get_type());
        
        // Compute the constraint that corresponds to the current node
        // x < c;    -->    X1 = x0
        Symbol val_sym(_input_constraints[lhs].get_symbol());
        val = Nodecl::Symbol::make(val_sym);
        val.set_type(val_sym.get_type());
        val = Nodecl::Analysis::RangeIntersection::make(val.shallow_copy(), 
                                                        Nodecl::Analysis::Range::make(Nodecl::Analysis::MinusInfinity::make(), 
                                                                                      rhs.shallow_copy(), lhs.get_type()),
                                                        lhs.get_type());
        c = Utils::Constraint(s, val);
        _input_constraints[lhs] = c;
        _output_constraints[lhs] = c;
        
        // Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints);
            cr.walk(val);
        }
        // 1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < x;       --TRUE-->       X1 = X0 ∩ [-∞, x-1]
        // 1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol orig_s_true(Utils::get_nodecl_base(lhs).get_symbol());
        std::string constr_name_true = orig_s_true.get_name() + "_" + ss_true.str();
        Symbol s_true(n.retrieve_context().new_symbol(constr_name_true));
        s_true.set_type(orig_s_true.get_type());
        // 1.2.- Build the TRUE constraint value
        const_value_t* one = const_value_get_one(/*num_bytes*/ 4, /*sign*/1);
        Nodecl::NodeclBase ub = (rhs.is_constant() ? const_value_to_nodecl(const_value_sub(rhs.get_constant(), one)) 
                                                   : Nodecl::Minus::make(val.shallow_copy(), const_value_to_nodecl(one), lhs.get_type()));
        Nodecl::NodeclBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                Nodecl::Symbol::make(s), 
                Nodecl::Analysis::Range::make(Nodecl::Analysis::MinusInfinity::make(), ub, lhs.get_type()),
                lhs.get_type());
        // 1.3.- Build the TRUE constraint and store it
        Utils::Constraint c_true = Utils::Constraint(s_true, val_true);
        _output_true_constraints[lhs] = c_true;
        
        // 2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ [ c, +∞]
        // 2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol orig_s_false(Utils::get_nodecl_base(lhs).get_symbol());
        std::string constr_name_false = orig_s_false.get_name() + "_" + ss_false.str();
        Symbol s_false(n.retrieve_context().new_symbol(constr_name_false));
        s_false.set_type(orig_s_false.get_type());
        // 2.2.- Build the FALSE constraint value
        Nodecl::NodeclBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                Nodecl::Symbol::make(s), 
                Nodecl::Analysis::Range::make(val.shallow_copy(),
                                              Nodecl::Analysis::PlusInfinity::make(),
                                              lhs.get_type()), 
                lhs.get_type());
        // 2.3.- Build the FALSE constraint and store it
        Utils::Constraint c_false = Utils::Constraint(s_false, val_false);
        _output_false_constraints[lhs] = c_false;
        
        return c;
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::ObjectInit& n)
    {
        Nodecl::Symbol lhs = Nodecl::Symbol::make(n.get_symbol(), n.get_locus());
        Nodecl::NodeclBase rhs = n.get_symbol().get_value();
        return visit_assignment(lhs, rhs);
    }
    
    // ++x;    -->    X1 = X0 + 1
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::Preincrement& n)
    {
        Utils::Constraint c;
        
        Nodecl::NodeclBase rhs = n.get_rhs();
        ERROR_CONDITION(_input_constraints.find(rhs) == _input_constraints.end(), 
                        "Some input constraint required for the RHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(rhs);
        Symbol orig_s(Utils::get_nodecl_base(rhs).get_symbol());
        std::string constr_name = orig_s.get_name() + "_" + ss.str();
        Symbol s(n.retrieve_context().new_symbol(constr_name));
        s.set_type(orig_s.get_type());
        
        Utils::Constraint constraint = _input_constraints[rhs];
        Nodecl::NodeclBase val = Nodecl::Add::make(Nodecl::Symbol::make(constraint.get_symbol()), 
                                                   const_value_to_nodecl(const_value_get_one(/*num_bytes*/ 4, /*sign*/1)), 
                                                   rhs.get_type());
        
        c = Utils::Constraint(s, val);
        _input_constraints[rhs] = c;
        _output_constraints[rhs] = c;
        
        return c;
    }
    
    // ************************** END Visitor implementing constraint building **************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ******************************** Class implementing range analysis ********************************* //
    
    RangeAnalysis::RangeAnalysis(ExtensibleGraph* graph)
        : _graph(graph)
    {}
    
    void RangeAnalysis::compute_range_analysis()
    {   // Compute the constraints of the current graph
        
        set_parameters_constraints();
        
        Node* entry = _graph->get_graph()->get_graph_entry_node();
        compute_initial_constraints(entry);
        ExtensibleGraph::clear_visits(entry);
        
        propagate_constraints_from_backwards_edges(entry);
        ExtensibleGraph::clear_visits(entry);
    }
    
    // Set an constraint to the graph entry node for each parameter of the function
    void RangeAnalysis::set_parameters_constraints()
    {
        Symbol func_sym = _graph->get_function_symbol();
        if(!func_sym.is_valid())    // The PCFG have been built for something other than a FunctionCode
            return;
        
        Node* entry = _graph->get_graph()->get_graph_entry_node();
        Utils::ConstraintMap constraints;
        const ObjectList<Symbol> params = func_sym.get_function_parameters();
        for(ObjectList<Symbol>::const_iterator it = params.begin(); it != params.end(); ++it)
        {
            // Get the value for the constraint (that is the value of the parameter)
            Nodecl::Symbol val = Nodecl::Symbol::make(*it);
            
            // Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(val);
            std::string constr_name = it->get_name() + "_" + ss.str();
            Symbol s(it->get_scope().new_symbol(constr_name));
            s.set_type(it->get_type());
            
            // Build the constraint and insert it in the constraints map
            Utils::Constraint c = Utils::Constraint(s, val);
            constraints[val] = c;
        }
        // Attach the constraints to the entry node of the graph
        entry->set_constraints(constraints);
    }
    
    // This is a breadth-first search because for a given node we need all its parents 
    // (except from those that come from back edges, for they will be recalculated later 'propagate_constraints_from_backwards_edges')
    // to be computed before propagating their information to the node
    void RangeAnalysis::compute_initial_constraints(Node* entry)
    {
        ERROR_CONDITION(!entry->is_entry_node(), 
                        "Expected ENTRY node but found %s node.", entry->get_type_as_string().c_str());
        
        ObjectList<Node*> currents(1, entry);
        while(!currents.empty())
        {
            ObjectList<Node*>::iterator it = currents.begin();
            while(it != currents.end())
            {
                Node* c = *it;
                if((*it)->is_visited())
                {
                    currents.erase(it);
                    continue;
                }
                else
                {
                    (*it)->set_visited(true);
                    ++it;
                }
                
                if(c->is_graph_node())
                {
                    // recursively compute the constraints for the inner nodes
                    compute_initial_constraints(c->get_graph_entry_node());
                    // propagate constraint from the inner nodes (summarized in the exit node) to the graph node
                    c->set_propagated_constraints(c->get_graph_exit_node()->get_propagated_constraints());
                }
                else
                {
                    // Collect the constraints computed from all the parents
                    Utils::ConstraintMap input_constraints;
                    ObjectList<Node*> parents = (c->is_entry_node() ? c->get_outer_node()->get_parents() : c->get_parents());
                    for(ObjectList<Node*>::iterator itt = parents.begin(); itt != parents.end(); ++itt)
                    {
                        Utils::ConstraintMap it_constraints = (*itt)->get_all_constraints();
                        for(Utils::ConstraintMap::iterator ittt = it_constraints.begin(); ittt != it_constraints.end(); ++ittt)
                        {
                            if(input_constraints.find(ittt->first)==input_constraints.end())
                            {   // No constraints already found for variable ittt->first
                                input_constraints[ittt->first] = ittt->second;
                            }
                            else
                            {   // Merge the constraint that already existed with the new one
                                Nodecl::NodeclBase old_constraint = input_constraints[ittt->first].get_constraint();
                                Nodecl::NodeclBase current_constraint = ittt->second.get_constraint();
                                TL::Symbol current_symbol = ittt->second.get_symbol();
                                Nodecl::NodeclBase new_constraint;
                                if(!Nodecl::Utils::structurally_equal_nodecls(old_constraint, current_constraint, /*skip_conversion_nodes*/true))
                                {   // Build a new constraint
                                    if(old_constraint.is<Nodecl::Analysis::Phi>())
                                    {   // Attach a new element to the list inside the node Phi
                                        Nodecl::List expressions = old_constraint.as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                                        expressions.append(ittt->first);
                                        new_constraint = Nodecl::Analysis::Phi::make(expressions, ittt->first.get_type());
                                    }
                                    else
                                    {   // Create a new node Phi with the combination of the old constraint and the new one
                                        Symbol tmp_s1(input_constraints[ittt->first].get_symbol());
                                        Nodecl::Symbol tmp1 = Nodecl::Symbol::make(tmp_s1);
                                        tmp1.set_type(tmp_s1.get_type());
                                        
                                        Symbol tmp_s2(ittt->second.get_symbol());
                                        Nodecl::Symbol tmp2 = Nodecl::Symbol::make(tmp_s2);
                                        tmp2.set_type(tmp_s2.get_type());
                                        
                                        Nodecl::List expressions = Nodecl::List::make(tmp1, tmp2);
                                        new_constraint = Nodecl::Analysis::Phi::make(expressions, ittt->second.get_constraint().get_type());
                                    }
                                    // Get a new symbol for the new constraint
                                    Nodecl::NodeclBase lhs = ittt->first;
                                    std::stringstream ss; ss << get_next_id(lhs);
                                    Symbol orig_s(lhs.get_symbol());
                                    std::string constr_name = orig_s.get_name() + "_" + ss.str();
                                    Symbol s(lhs.retrieve_context().new_symbol(constr_name));
                                    s.set_type(orig_s.get_type());
                                    input_constraints[ittt->first] = Utils::Constraint(s, new_constraint);
                                }
                                else
                                {   // Propagate the already existing constraint
                                    input_constraints[ittt->first] = ittt->second;
                                }
                            }
                        }
                    }
                    
                    // Propagate constraints from parent nodes to the current node
                    c->set_propagated_constraints(input_constraints);
                    
                    // Compute the constraints of the current node
                    // Note: take into account the constraints the node may already have (if it is the TRUE or FALSE child of a conditional)
                    Utils::ConstraintMap current_constraints = c->get_constraints();
                    ConstraintBuilderVisitor cbv(input_constraints, current_constraints);
                    ObjectList<Nodecl::NodeclBase> stmts = c->get_statements();
                    for(ObjectList<Nodecl::NodeclBase>::iterator itt = stmts.begin(); itt != stmts.end(); ++itt)
                        cbv.compute_constraints(*itt);
                    c->set_constraints(cbv.get_output_constraints());
                    Utils::ConstraintMap out_constraints = cbv.get_output_constraints();
                    
                    // Set true/false output constraints to current children, if applies
                    ObjectList<Edge*> exits = c->get_exit_edges();
                    if(exits.size()==2 &&
                       ((exits[0]->is_true_edge() && exits[1]->is_false_edge()) || (exits[1]->is_true_edge() && exits[0]->is_false_edge())))
                    {
                        Utils::ConstraintMap out_true_constraints = cbv.get_output_true_constraints();
                        Utils::ConstraintMap out_false_constraints = cbv.get_output_false_constraints();
                        
                        // Distinguish here if_else conditionals and loop conditionals
                        Node* n_outer = c->get_outer_node();
                        // We always propagate to the TRUE edge
                        Node* true_node = (exits[0]->is_true_edge() ? exits[0]->get_target() : exits[1]->get_target());
                        while(true_node->is_exit_node())
                            true_node = true_node->get_outer_node()->get_children()[0];
                        // For the if_else cases, we only propagate to the FALSE edge when it contains statements ('else' statements)
                        Node* false_node = (exits[0]->is_true_edge() ? exits[1]->get_target() : exits[0]->get_target());
                        if(!n_outer->is_ifelse_statement())
                            while(false_node->is_exit_node())
                                false_node = false_node->get_outer_node()->get_children()[0];
                        
                        true_node->add_constraints(out_true_constraints);
                        if(!false_node->is_exit_node())
                            false_node->add_constraints(out_false_constraints);
                    }
                }
            }
            
            ObjectList<Node*> next_currents;
            for(ObjectList<Node*>::iterator itt = currents.begin(); itt != currents.end(); ++itt)
                next_currents.append((*itt)->get_children());
            currents = next_currents;
        }
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
