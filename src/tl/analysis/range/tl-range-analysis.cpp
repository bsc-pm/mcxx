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
        
        // #define RANGES_DEBUG
        
        unsigned int non_sym_constraint_id = 0;
        
        //! This maps stores the relationship between each variable in a given node and 
        //! the last identifier used to create a constraint for that variable
        std::map<Nodecl::NodeclBase, unsigned int, Nodecl::Utils::Nodecl_structural_less> var_to_last_constraint_id;
        
        unsigned int get_next_id(const Nodecl::NodeclBase& n)
        {
            unsigned int next_id = 0;
            if(!n.is_null())
            {
                if(var_to_last_constraint_id.find(n) != var_to_last_constraint_id.end())
                    next_id = var_to_last_constraint_id[n] + 1;
                var_to_last_constraint_id[n] = next_id;
            }
            else
            {
                next_id = ++non_sym_constraint_id;
            }
            return next_id;
        }
    }
    
    ConstraintReplacement::ConstraintReplacement(Utils::ConstraintMap constraints_map)
        : _constraints_map(constraints_map)
    {}
    
    void ConstraintReplacement::visit(const Nodecl::ArraySubscript& n)
    {
        if(_constraints_map.find(n) != _constraints_map.end())
            n.replace(Nodecl::Symbol::make(_constraints_map[n].get_symbol()));
        else
        {
            walk(n.get_subscripted());
            walk(n.get_subscripts());
        }
    }
    
    void ConstraintReplacement::visit(const Nodecl::ClassMemberAccess& n)
    {
        if(_constraints_map.find(n) != _constraints_map.end())
            n.replace(Nodecl::Symbol::make(_constraints_map[n].get_symbol()));
        else
        {
            walk(n.get_lhs());
            walk(n.get_member());
        }
    }
    
    void ConstraintReplacement::visit(const Nodecl::Symbol& n)
    {
        ERROR_CONDITION(_constraints_map.find(n) == _constraints_map.end(),
                        "No constraints found for symbol %s in locus %s. "
                        "We should replace the variable with the corresponding constraint.", 
                        n.prettyprint().c_str(), n.get_locus_str().c_str());
        n.replace(Nodecl::Symbol::make(_constraints_map[n].get_symbol()));
    }
    
    ConstraintBuilderVisitor::ConstraintBuilderVisitor(Utils::ConstraintMap input_constraints_map, 
                                                       Utils::ConstraintMap current_constraints)
        : _input_constraints_map(input_constraints_map), _output_constraints_map(current_constraints), 
          _output_true_constraints_map(), _output_false_constraints_map()
    {}
    
    void ConstraintBuilderVisitor::compute_constraints(const Nodecl::NodeclBase& n)
    {
        walk(n);
    }
    
    Utils::ConstraintMap ConstraintBuilderVisitor::get_output_constraints_map()
    {
        return _output_constraints_map;
    }

    Utils::ConstraintMap ConstraintBuilderVisitor::get_output_true_constraints_map()
    {
        return _output_true_constraints_map;
    }
    
    Utils::ConstraintMap ConstraintBuilderVisitor::get_output_false_constraints_map()
    {
        return _output_false_constraints_map;
    }
    
    Utils::Constraint ConstraintBuilderVisitor::join_list(TL::ObjectList<Utils::Constraint>& list)
    {
        Utils::Constraint result = list[0];
        WARNING_MESSAGE("join_list of a list of constraint is not yet supported. Doing nothing.", 0);
        return result;    
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit_assignment(const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs)
    {
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(lhs);
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        std::string constr_name = orig_s.get_name() + "_" + ss.str();
        Symbol s(lhs.retrieve_context().new_symbol(constr_name));
        Type t = orig_s.get_type();
        s.set_type(t);
        // Build the value of the constraint
        Nodecl::NodeclBase val;
        if(rhs.is_constant())       // x = c;    -->    X1 = c
            val = Nodecl::Analysis::Range::make(rhs.shallow_copy(), rhs.shallow_copy(), t);
        else 
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            val = rhs.shallow_copy();
            cr.walk(val);
        }
        
        // Build the constraint and insert it in the corresponding maps
        Utils::Constraint c(s, val);
#ifdef RANGES_DEBUG
        std::cerr << "Assignment Constraint " << s.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        _input_constraints_map[lhs] = c;
        _output_constraints_map[lhs] = c;
        
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
        ERROR_CONDITION(_input_constraints_map.find(lhs) == _input_constraints_map.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        Type t = orig_s.get_type();
        
        // 1. Compute the first constraint that corresponds to the current node: x < c
        // -->    X1 = [0, 1]
        // 1.1 Get a new symbol for the constraint
        std::stringstream ss; ss << get_next_id(Nodecl::NodeclBase::null());
        Symbol s_x(n.retrieve_context().new_symbol("_x_" + ss.str()));
        s_x.set_type(t);
        // 1.2 Build the value of the constraints
        const_value_t* zero = const_value_get_zero(/*num_bytes*/ 4, /*sign*/1);
        const_value_t* one = const_value_get_one(/*num_bytes*/ 4, /*sign*/1);
        val = Nodecl::Analysis::Range::make(const_value_to_nodecl(zero), 
                                            const_value_to_nodecl(one), t);
        // 1.3 Build the actual constraint and insert it in the corresponding map
#ifdef RANGES_DEBUG
        std::cerr << "LowerThan Constraint " << s_x.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        c = Utils::Constraint(s_x, val);
        Symbol var_s(n.retrieve_context().new_symbol("_x"));
        var_s.set_type(t);
        Nodecl::Symbol var = Nodecl::Symbol::make(var_s);
        var.set_type(t);
        _output_constraints_map[var] = c;
        
        // 2. Compute the second constraint that corresponds to the current node: x < c
        // -->    X1 = X0
        // 2.1 Get a new symbol for the constraint
        std::stringstream ss_tmp; ss_tmp << get_next_id(lhs);
        Symbol s(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_tmp.str()));
        s.set_type(t);
        // 2.2 Build the value of the constraints
        val = Nodecl::Symbol::make(_input_constraints_map[lhs].get_symbol());
        // 2.3 Build the actual constraint and insert it in the corresponding map
#ifdef RANGES_DEBUG
        std::cerr << "LowerThan Constraint " << s.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        c = Utils::Constraint(s, val);
        _input_constraints_map[lhs] = c;
        _output_constraints_map[lhs] = c;
        
        // 3. Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            cr.walk(val);
        }
        // 3.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < x;       --TRUE-->       X1 = X0 ∩ [-∞, x-1]
        // 3.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_true.str()));
        s_true.set_type(t);
        // 3.1.2.- Build the TRUE constraint value
        Nodecl::NodeclBase ub = (rhs.is_constant() ? const_value_to_nodecl(const_value_sub(rhs.get_constant(), one)) 
                                                   : Nodecl::Minus::make(val.shallow_copy(), const_value_to_nodecl(one), t));
        Nodecl::NodeclBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                Nodecl::Symbol::make(s), 
                Nodecl::Analysis::Range::make(Nodecl::Analysis::MinusInfinity::make(), ub, t),
                t);
        // 3.1.3.- Build the TRUE constraint and store it
#ifdef RANGES_DEBUG
        std::cerr << "LowerThan TRUE Constraint " << s_true.get_name() << " = " << val_true.prettyprint() << std::endl;
#endif
        _output_true_constraints_map[lhs] = Utils::Constraint(s_true, val_true);
        
        // 3.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ [ c, +∞]
        // 3.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_false.str()));
        s_false.set_type(t);
        // 3.2.2.- Build the FALSE constraint value
        Nodecl::NodeclBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                Nodecl::Symbol::make(s), 
                Nodecl::Analysis::Range::make(val.shallow_copy(),
                                              Nodecl::Analysis::PlusInfinity::make(),
                                              t), 
                t);
        // 3.2.3.- Build the FALSE constraint and store it
#ifdef RANGES_DEBUG
        std::cerr << "LowerThan FALSE Constraint " << s_false.get_name() << " = " << val_false.prettyprint() << std::endl;
#endif
        _output_false_constraints_map[lhs] = Utils::Constraint(s_false, val_false);
        
        return c;
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::Mod& n)
    {
        // Build the constraint symbol
        std::stringstream ss; ss << get_next_id(Nodecl::NodeclBase::null());
        std::string sym_name = "_x_" + ss.str();
        Symbol s(n.retrieve_context().new_symbol(sym_name));
        Type t = n.get_lhs().get_type();
        s.set_type(t);
        
        // Build the constraint value
        const_value_t* zero = const_value_get_zero(/*num_bytes*/ 4, /*sign*/1);
        const_value_t* one = const_value_get_one(/*num_bytes*/ 4, /*sign*/1);
        Nodecl::NodeclBase rhs = n.get_rhs();
        Nodecl::NodeclBase ub = 
                (rhs.is_constant() ? const_value_to_nodecl(const_value_sub(rhs.get_constant(), one)) 
                                   : Nodecl::Minus::make(rhs.shallow_copy(), const_value_to_nodecl(one), rhs.get_type()));
        Nodecl::NodeclBase val = Nodecl::Analysis::Range::make(const_value_to_nodecl(zero), ub, t);
        
        // Build the constraint
#ifdef RANGES_DEBUG
        std::cerr << "Mod Constraint " << s.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        Utils::Constraint c(s, val);
        Nodecl::Symbol var = Nodecl::Symbol::make(TL::Symbol(n.retrieve_context().new_symbol("_x")));
        var.set_type(t);
        _output_constraints_map[var] = c;
        
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
        Nodecl::NodeclBase rhs = n.get_rhs();
        ERROR_CONDITION(_input_constraints_map.find(rhs) == _input_constraints_map.end(), 
                        "Some input constraint required for the RHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(rhs);
        Symbol orig_s(Utils::get_nodecl_base(rhs).get_symbol());
        std::string constr_name = orig_s.get_name() + "_" + ss.str();
        Symbol s(n.retrieve_context().new_symbol(constr_name));
        s.set_type(orig_s.get_type());
        
        Nodecl::NodeclBase val = Nodecl::Add::make(Nodecl::Symbol::make(_input_constraints_map[rhs].get_symbol()), 
                                                   const_value_to_nodecl(const_value_get_one(/*num_bytes*/ 4, /*sign*/1)), 
                                                   rhs.get_type());
#ifdef RANGES_DEBUG
        std::cerr << "Preincrement Constraint " << s.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        Utils::Constraint c(s, val);
        _input_constraints_map[rhs] = c;
        _output_constraints_map[rhs] = c;
        
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
    // Since this node does not have statements, the constraint will be propagated to the descendants as 'propagated constraint'
    // but we need it as a "full rights" constraint, so we look for the first node with statements and set it there too
    void RangeAnalysis::set_parameters_constraints()
    {
        Symbol func_sym = _graph->get_function_symbol();
        if(!func_sym.is_valid())    // The PCFG have been built for something other than a FunctionCode
            return;
        
        Node* entry = _graph->get_graph()->get_graph_entry_node();
        // Look for the first node with statements
        Node* first_stmt_node = NULL;
        Node* current = entry;
        while(first_stmt_node == NULL)
        {
            if(current->is_graph_node())
                current = current->get_graph_entry_node();
            else if(current->is_exit_node())
                current = current->get_outer_node()->get_children()[0];
            else
                current = current->get_children()[0];
            if(current->has_statements())
                first_stmt_node = current;
        }
        
        // Set the constraints for each parameter to both nodes
        Utils::ConstraintMap constraints;
        const ObjectList<Symbol> params = func_sym.get_function_parameters();
        for(ObjectList<Symbol>::const_iterator it = params.begin(); it != params.end(); ++it)
        {
            Type t = it->get_type();
            Nodecl::Symbol param_s = Nodecl::Symbol::make(*it);
            param_s.set_type(t);
            
            // Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(param_s);
            std::string constr_name = it->get_name() + "_" + ss.str();
            Symbol s(it->get_scope().new_symbol(constr_name));
            s.set_type(t);
            
            // Get the value for the constraint
            Nodecl::NodeclBase val = Nodecl::Analysis::Range::make(Nodecl::Analysis::MinusInfinity::make(), Nodecl::Analysis::PlusInfinity::make(), t);
            
            // Build the constraint and insert it in the constraints map
            constraints[param_s] = Utils::Constraint(s, val);
        }
        // Attach the constraints to the entry node of the graph
        entry->set_constraints_map(constraints);
//         first_stmt_node->set_constraints_map(constraints);
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
                    c->set_propagated_constraints_map(c->get_graph_exit_node()->get_propagated_constraints_map());
                    c->add_propagated_constraints_map(c->get_graph_exit_node()->get_constraints_map());
                }
                else
                {
                    // Collect the constraints computed from all the parents
                    Utils::ConstraintMap input_constraints_map, new_input_constraints;
                    ObjectList<Node*> parents = (c->is_entry_node() ? c->get_outer_node()->get_parents() : c->get_parents());
                    for(ObjectList<Node*>::iterator itt = parents.begin(); itt != parents.end(); ++itt)
                    {
                        Utils::ConstraintMap it_constraints_map = (*itt)->get_all_constraints_map();
                        for(Utils::ConstraintMap::iterator ittt = it_constraints_map.begin(); 
                            ittt != it_constraints_map.end(); ++ittt)
                        {
                            if(input_constraints_map.find(ittt->first)==input_constraints_map.end() && 
                               new_input_constraints.find(ittt->first)==new_input_constraints.end())
                            {   // No constraints already found for variable ittt->first
                                input_constraints_map[ittt->first] = ittt->second;
                            }
                            else
                            {   // Merge the constraint that already existed with the new one
                                Utils::Constraint old_constraint;
                                if(input_constraints_map.find(ittt->first)!=input_constraints_map.end())
                                {
                                    old_constraint = input_constraints_map[ittt->first];
                                    input_constraints_map.erase(input_constraints_map.find(ittt->first));
                                }
                                else
                                    old_constraint = new_input_constraints[ittt->first];
                                Nodecl::NodeclBase current_constraint = ittt->second.get_constraint();
                                TL::Symbol current_symbol = ittt->second.get_symbol();
                                if(!Nodecl::Utils::structurally_equal_nodecls(old_constraint.get_constraint(), current_constraint, 
                                                                            /*skip_conversion_nodes*/true))
                                {
                                    // Get a new symbol for the new constraint
                                    Nodecl::NodeclBase lhs = ittt->first;
                                    std::stringstream ss; ss << get_next_id(lhs);
                                    Symbol orig_s(lhs.get_symbol());
                                    std::string constr_name = orig_s.get_name() + "_" + ss.str();
                                    Symbol s(lhs.retrieve_context().new_symbol(constr_name));
                                    s.set_type(orig_s.get_type());
                                    
                                    // Build a the value of the new constraint
                                    Nodecl::NodeclBase new_constraint_val;
                                    if(old_constraint.get_constraint().is<Nodecl::Analysis::Phi>())
                                    {   // Attach a new element to the list inside the node Phi
                                        Nodecl::List expressions = old_constraint.get_constraint().as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                                        expressions.append(ittt->first);
                                        new_constraint_val = Nodecl::Analysis::Phi::make(expressions, ittt->first.get_type());
                                    }
                                    else
                                    {   // Create a new node Phi with the combination of the old constraint and the new one
                                        Symbol tmp_s1(old_constraint.get_symbol());
                                        Nodecl::Symbol tmp1 = Nodecl::Symbol::make(tmp_s1);
                                        tmp1.set_type(tmp_s1.get_type());
                                        
                                        Symbol tmp_s2(ittt->second.get_symbol());
                                        Nodecl::Symbol tmp2 = Nodecl::Symbol::make(tmp_s2);
                                        tmp2.set_type(tmp_s2.get_type());
                                        
                                        Nodecl::List expressions = Nodecl::List::make(tmp1, tmp2);
                                        new_constraint_val = Nodecl::Analysis::Phi::make(expressions, ittt->second.get_constraint().get_type());
                                    }
                                    
                                    // Build the actual constraint and insert it in the proper list
                                    new_input_constraints[ittt->first] = Utils::Constraint(s, new_constraint_val);
                                }
                            }
                        }
                    }
                    
                    // Propagate constraints from parent nodes to the current node
                    c->set_propagated_constraints_map(input_constraints_map);
                    c->add_constraints_map(new_input_constraints);
                    if(c->has_statements())
                    {
                        // Compute the constraints of the current node
                        // Note: take into account the constraints the node may already have (if it is the TRUE or FALSE child of a conditional)
                        Utils::ConstraintMap current_constraints_map = c->get_constraints_map();
#ifdef RANGES_DEBUG
                        std::cerr << "NODE " << c->get_id() << std::endl;
#endif
                        ConstraintBuilderVisitor cbv(input_constraints_map, current_constraints_map);
                        ObjectList<Nodecl::NodeclBase> stmts = c->get_statements();
                        for(ObjectList<Nodecl::NodeclBase>::iterator itt = stmts.begin(); itt != stmts.end(); ++itt)
                            cbv.compute_constraints(*itt);
                        c->add_constraints_map(cbv.get_output_constraints_map());
                        
                        // Set true/false output constraints to current children, if applies
                        ObjectList<Edge*> exits = c->get_exit_edges();
                        if(exits.size()==2 &&
                            ((exits[0]->is_true_edge() && exits[1]->is_false_edge()) || (exits[1]->is_true_edge() && exits[0]->is_false_edge())))
                        {
                            Utils::ConstraintMap out_true_constraints_map = cbv.get_output_true_constraints_map();
                            Utils::ConstraintMap out_false_constraints_map = cbv.get_output_false_constraints_map();
                            
                            // Distinguish here if_else conditionals and loop conditionals
                            Node* n_outer = c->get_outer_node();
                            // We always propagate to the TRUE edge
                            Node* true_node = (exits[0]->is_true_edge() ? exits[0]->get_target() : exits[1]->get_target());
                            while(true_node->is_exit_node())
                                true_node = true_node->get_outer_node()->get_children()[0];
                            if(true_node->is_graph_node())
                                true_node = true_node->get_graph_entry_node();
                            true_node->add_constraints_map(out_true_constraints_map);
                            // For the if_else cases, we only propagate to the FALSE edge when it contains statements ('else' statements)
                            Node* false_node = (exits[0]->is_true_edge() ? exits[1]->get_target() : exits[0]->get_target());
                            if(!n_outer->is_ifelse_statement())
                            {
                                ObjectList<Node*> children;
                                while(false_node->is_exit_node())
                                {
                                    children = false_node->get_outer_node()->get_children();
                                    if(!children.empty())
                                        false_node = children[0];
                                    else
                                    {
                                        false_node = NULL;
                                        break;
                                    }
                                }
                                if(false_node!=NULL && false_node->is_graph_node())
                                    false_node = false_node->get_graph_entry_node();                            
                            }
                            if(false_node!=NULL && !false_node->is_exit_node())
                                false_node->add_constraints_map(out_false_constraints_map);
                        }
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
                recompute_node_constraints((*it)->get_target(), n->get_all_constraints_map());
        }
        
        // Recursively treat the children
        ObjectList<Node*> children = n->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            propagate_constraints_from_backwards_edges(*it);
    }

    void RangeAnalysis::recompute_node_constraints(Node* n, Utils::ConstraintMap new_constraint_map)
    {
        Utils::ConstraintMap current_constraint_map = n->get_constraints_map();
        for(Utils::ConstraintMap::iterator it = new_constraint_map.begin(); it != new_constraint_map.end(); ++it)
        {
            if((current_constraint_map.find(it->first) != current_constraint_map.end()) && 
                (current_constraint_map[it->first] != it->second))
            {
                Nodecl::NodeclBase c1 = current_constraint_map[it->first].get_constraint().shallow_copy();
                Nodecl::NodeclBase c2 = Nodecl::Symbol::make(it->second.get_symbol());
                Nodecl::List expressions = Nodecl::List::make(c1, c2);
                Nodecl::NodeclBase c_val = Nodecl::Analysis::Phi::make(expressions, Utils::get_nodecl_base(it->first).get_symbol().get_type());
                // Set the new constraint
                current_constraint_map[it->first] = Utils::Constraint(current_constraint_map[it->first].get_symbol(), c_val);
            }
        }
        n->set_constraints_map(current_constraint_map);
    }
    
    // ****************************** End class implementing range analysis ******************************* //
    // **************************************************************************************************** //
}
}
