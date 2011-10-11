/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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
#include "cxx-codegen.h"
#include "cxx-process.h"
#include "tl-extensible-graph.hpp"
#include "tl-loop-analysis.hpp"

namespace TL
{
    LoopAnalysis::LoopAnalysis()
        : _induction_vars()
    {}
    
    void LoopAnalysis::traverse_loop_init(Nodecl::NodeclBase init)
    {
        if (init.is<Nodecl::Comma>())
        {
            Nodecl::Comma init_ = init.as<Nodecl::Comma>();
            traverse_loop_init(init_.get_rhs());
            traverse_loop_init(init_.get_lhs());
        }
        else if (init.is<Nodecl::ObjectInit>())
        {
            Nodecl::ObjectInit init_ = init.as<Nodecl::ObjectInit>();
            Symbol def_var = init_.get_symbol();
            Nodecl::NodeclBase def_expr = def_var.get_initialization();
            
            InductionVarInfo* ind = new InductionVarInfo(def_var, def_expr);
            _induction_vars.append(ind);
        }
        else if (init.is<Nodecl::Assignment>())
        {
            Nodecl::Assignment init_ = init.as<Nodecl::Assignment>();
            Symbol def_var = init_.get_lhs().get_symbol();
            Nodecl::NodeclBase def_expr = init_.get_rhs();
            
            InductionVarInfo* ind = new InductionVarInfo(def_var, def_expr);
            _induction_vars.append(ind);
        }
        else
        {
            internal_error("Node kind '%s' while analysing the induction variables in loop init expression not yet implemented",
                ast_print_node_type(init.get_kind()));
        }
    }
    
    InductionVarInfo* LoopAnalysis::induction_vars_l_contains_symbol(Symbol s)
    {
        for (ObjectList<InductionVarInfo*>::iterator it = _induction_vars.begin(); it != _induction_vars.end(); ++it)
        {
            if ((*it)->get_symbol() == s)
            {
                return *it;
            }
        }
        return NULL;
    }
    
    void LoopAnalysis::traverse_loop_cond(Nodecl::NodeclBase cond)
    {
        // Logical Operators
        if (cond.is<Nodecl::LogicalAnd>())
        {
//             Nodecl::LogicalAnd cond_ = cond.as<Nodecl::LogicalAnd>();
//             
//             result = traverse_loop_cond(init_info_l, cond_.get_lhs());
//             
//             // To mix the values is not trivial, we have to get the bigger or the smaller, depending on the step (positive or negative)
//             cond_values.insert(traverse_loop_cond(init_info_l, cond_.get_rhs()));
            internal_error("Combined && expressions as loop condition not yet implemented", 0);
        }
        else if (cond.is<Nodecl::LogicalOr>())
        {
            internal_error("Combined || expressions as loop condition not yet implemented", 0);
        }
        else if (cond.is<Nodecl::LogicalNot>())
        {
            internal_error("Combined ! expressions as loop condition not yet implemented", 0);
        }
        // Relational Operators
        else if (cond.is<Nodecl::LowerThan>())
        {
            Nodecl::LowerThan cond_ = cond.as<Nodecl::LowerThan>();
            Symbol def_var = cond_.get_lhs().get_symbol();
            Nodecl::NodeclBase def_expr = cond_.get_rhs();           
            
            // The upper bound will be the rhs minus 1
            nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
            Nodecl::NodeclBase ub = Nodecl::Minus::make(def_expr, Nodecl::IntegerLiteral(one), def_var.get_type(), 
                                                        cond.get_filename(), cond.get_line());
            
            InductionVarInfo* loop_info_var;
            if ( (loop_info_var = induction_vars_l_contains_symbol(def_var)) != NULL )
            {
                loop_info_var->set_ub(ub);
            }
            else
            {
                internal_error("Analysis of loops without an init expression not yet implemented", 0);
                // Look for the lb of the value!!!
//                 loop_info_var = new LoopAnalysis(def_var, );
//                 result[def_var] = def_expr;
            }
        }
        else if (cond.is<Nodecl::LowerOrEqualThan>())
        {
            Nodecl::LowerThan cond_ = cond.as<Nodecl::LowerThan>();
            Symbol def_var = cond_.get_lhs().get_symbol();
            Nodecl::NodeclBase def_expr = cond_.get_rhs();            
            
            InductionVarInfo* loop_info_var;
            if ( (loop_info_var = induction_vars_l_contains_symbol(def_var)) != NULL )
            {
                loop_info_var->set_ub(def_expr);
            }
            else
            {
                internal_error("Analysis of loops without an init expression not yet implemented", 0);
            }
            
        }
        else if (cond.is<Nodecl::GreaterThan>())
        {
            Nodecl::GreaterThan cond_ = cond.as<Nodecl::GreaterThan>();
            Symbol def_var = cond_.get_lhs().get_symbol();
            Nodecl::NodeclBase def_expr = cond_.get_rhs();
            
            // This is not the UB, is the LB: the lower bound will be the rhs plus 1
            nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
            Nodecl::NodeclBase lb = Nodecl::Add::make(def_expr, Nodecl::IntegerLiteral(one), def_var.get_type(), 
                                                      cond.get_filename(), cond.get_line());
            
            InductionVarInfo* loop_info_var;
            if ( (loop_info_var = induction_vars_l_contains_symbol(def_var)) != NULL )
            {
                loop_info_var->set_ub(loop_info_var->get_lb());
                loop_info_var->set_lb(lb);
            }
            else
            {
                internal_error("Analysis of loops without an init expression not yet implemented", 0);
            }
        }
        else if (cond.is<Nodecl::GreaterOrEqualThan>())
        {
            Nodecl::GreaterThan cond_ = cond.as<Nodecl::GreaterThan>();
            Symbol def_var = cond_.get_lhs().get_symbol();
            Nodecl::NodeclBase def_expr = cond_.get_rhs();

            InductionVarInfo* loop_info_var;
            if ( (loop_info_var = induction_vars_l_contains_symbol(def_var)) != NULL )
            {
                loop_info_var->set_ub(loop_info_var->get_lb());
                loop_info_var->set_lb(def_expr);
            }
            else
            {
                internal_error("Analysis of loops without an init expression not yet implemented", 0);
            }
        }
        else if (cond.is<Nodecl::Different>())
        {
            internal_error("Analysis of loops with DIFFERENT condition expression not yet implemented", 0);
        }
        else if (cond.is<Nodecl::Equal>())
        {
            internal_error("Analysis of loops with EQUAL condition expression not yet implemented", 0);
        }
        else
        {
            internal_error("Node kind '%s' while analysing the induction variables in loop cond expression not yet implemented",
                ast_print_node_type(cond.get_kind()));
        }
    }
    
    void LoopAnalysis::traverse_loop_step(Nodecl::NodeclBase step)
    {
        if (step.is<Nodecl::Preincrement>())
        {
            Nodecl::Preincrement step_ = step.as<Nodecl::Preincrement>();
            Nodecl::NodeclBase rhs = step_.get_rhs();
            
            Symbol s = rhs.get_symbol();
            if (s.is_valid())
            {
                InductionVarInfo* loop_info_var;
                if ( (loop_info_var = induction_vars_l_contains_symbol(s)) != NULL )
                {
                    loop_info_var->set_step(step);
                    loop_info_var->set_step_is_one(true);
                }
                else
                {
                    internal_error("Analysis of loops without an init expression not yet implemented", 0);
                }
            }
            else 
            {
                internal_error("Analysis of loop step which is not a symbol not yet implemented", 0);
            }
        }
        else if (step.is<Nodecl::Postincrement>())
        {
            Nodecl::Preincrement step_ = step.as<Nodecl::Preincrement>();
            Nodecl::NodeclBase rhs = step_.get_rhs();
            
            Symbol s = rhs.get_symbol();
            if (s.is_valid())
            {
                InductionVarInfo* loop_info_var;
                if ( (loop_info_var = induction_vars_l_contains_symbol(s)) != NULL )
                {
                    loop_info_var->set_step(step);
                    loop_info_var->set_step_is_one(true);
                }
                else
                {
                    internal_error("Analysis of loops without an init expression not yet implemented", 0);
                }
            }
            else 
            {
                internal_error("Analysis of loop step which is not a symbol not yet implemented", 0);
            }            
        }
        else
        {
            internal_error("Node kind '%s' while analysing the induction variables in loop step expression not yet implemented",
                ast_print_node_type(step.get_kind()));            
        }
    }
    
    void LoopAnalysis::prettyprint_induction_var_info(InductionVarInfo* var_info)
    {
        std::cerr << "***** Symbol: " << var_info->get_symbol().get_name() << std::endl;
        std::cerr << "          LB = " << c_cxx_codegen_to_str(var_info->get_lb().get_internal_nodecl()) << std::endl;
        std::cerr << "          UB = " << c_cxx_codegen_to_str(var_info->get_ub().get_internal_nodecl()) << std::endl;
        std::cerr << "          STEP = " << c_cxx_codegen_to_str(var_info->get_step().get_internal_nodecl()) << std::endl;
        std::cerr << "          IS_ONE = " << (var_info->step_is_one() == 1) << std::endl;
    }
    

    char LoopAnalysis::induction_vars_are_defined_in_node(Node* node)
    {
        if (node->is_visited())
        {
            node->set_visited(true);
            
            Node_type ntype = node->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                if(ntype == BASIC_NORMAL_NODE || ntype == BASIC_LABELED_NODE 
                        || ntype == BASIC_FUNCTION_CALL_NODE || ntype == GRAPH_NODE)
                {   // The node has Use-Def
                    ext_sym_set killed_vars = node->get_killed_vars();
                    for (ext_sym_set::iterator it = killed_vars.begin(); it != killed_vars.end(); ++it)
                    {
                        if (induction_vars_l_contains_symbol(it->get_symbol()) != NULL)
                        {
                            return '1';
                        }
                    }
                    ext_sym_set undef_behaviour_vars = node->get_undefined_behaviour_vars();
                    for (ext_sym_set::iterator it = undef_behaviour_vars.begin(); it != undef_behaviour_vars.end(); ++it)
                    {
                        if (induction_vars_l_contains_symbol(it->get_symbol()) != NULL)
                        {
                            return '2';
                        }
                    }                    
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    induction_vars_are_defined_in_node(*it);
                }
            }
            else
            {
                return '0';
            }
        }
    }
    
    void LoopAnalysis::compute_induction_vars_from_loop_control(Nodecl::LoopControl loop_control, Node* loop_node)
    {
        // Compute loop control info
        traverse_loop_init(loop_control.get_init());
        traverse_loop_cond(loop_control.get_cond());
        traverse_loop_step(loop_control.get_next());
        
        // Check whether the statements within the loop modify the induction variables founded in the loop control
        induction_vars_are_defined_in_node(loop_node);
        
        // Print induction variables info
        std::cerr << "Info computed about the induction variables" << std::endl;
        for(ObjectList<InductionVarInfo*>::iterator it = _induction_vars.begin(); it != _induction_vars.end(); ++it)
        {
            prettyprint_induction_var_info(*it);
        }
    }

    void LoopAnalysis::compute_induction_varaibles_info(Node* node)
    {
        if (!node->is_visited())
        {
            std::cerr << "compute_induction_varaibles_info - node: " << node->get_id() << std::endl;
            node->set_visited(true);
            
            Node_type ntype = node->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                if (ntype == GRAPH_NODE)
                {
                    if (node->get_data<Graph_type>(_GRAPH_TYPE) == LOOP)
                    {
                        // Get the info about induction variables in the loop control
                        Nodecl::LoopControl loop_control = node->get_data<Nodecl::NodeclBase>(_NODE_LABEL).as<Nodecl::LoopControl>();
                        compute_induction_vars_from_loop_control(loop_control, node);
                    }
                    compute_induction_varaibles_info(node->get_data<Node*>(_ENTRY_NODE));
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    compute_induction_varaibles_info(*it);
                }
            }
            else
            {
                return;
            }
        }
    }
    
    static void substitue_index_per_range(Nodecl::ArraySubscript& subscript, InductionVarInfo* ind_var)
    {
        Nodecl::List subscripts = subscript.get_subscripts().as<Nodecl::List>();
        
    }
    
    Nodecl::ArraySection LoopAnalysis::set_array_access_range(Node* node, Nodecl::ArraySubscript subscript, 
                                                              ExtensibleSymbol ei, char use_type)
    {
        Nodecl::List subscript_l = subscript.get_subscripts().as<Nodecl::List>();
        Nodecl::ArraySection new_array_access;
        
        for (std::vector<Nodecl::NodeclBase>::iterator its = subscript_l.begin();
            its != subscript_l.end(); ++its)
        {
            if (its->is<Nodecl::Symbol>())
            {
                Symbol s = its->get_symbol();
                InductionVarInfo* ind_var;
                if ( (ind_var = induction_vars_l_contains_symbol(s)) != NULL )
                {
                    std::cerr << "Array accessed by an induction varaible!!" << std::endl;
                    
                    // Unset the old extensible symbol from the proper list
                    if (use_type == '0')
                    {   
                        node->unset_ue_var(ei);
                    }
                    else if (use_type == '1')
                    {
                        std::cerr << "Deleting symbol " << ei.get_symbol().get_name() 
                                  << " from killed vars in node " << node->get_id() << std::endl;
                        node->unset_killed_var(ei);
                    }
                    else
                    {
                        internal_error("Unexpected type of variable use '%s' in node '%d'", use_type, node->get_id());
                    }
                    
                    // Set the new symbol to the proper list
                    if (ind_var->step_is_one())
                    {
                        // FIXME This should be done from right to left traversing the subscripts
                        new_array_access = Nodecl::ArraySection::make(subscript.get_subscripted(), ind_var->get_lb(), ind_var->get_ub(),
                                                                      subscript.get_type(), 
                                                                      subscript.get_filename(), subscript.get_line());
                        ExtensibleSymbol new_ei(new_array_access);
                        if (use_type == '0')
                        {   
                            node->set_ue_var(new_ei);
                        }
                        else
                        {   // use_type = '1'
                            std::cerr << "setting new  symbol " << new_ei.get_symbol().get_name() 
                                      << " from killed vars in node " << node->get_id() << std::endl;                        
                            node->set_killed_var(new_ei);
                        }
                    }
                    else
                    {
                        node->set_undefined_behaviour_var(ei);
                    }
                }
                else
                {
                    // TODO We cannot say what can we do with this array!
                }
            }
            else
            {
                internal_error("Non symbol subscripts not yet implemented for array analysis within loops", 0);
            }
        }
        
        return new_array_access;
    }
    
    void LoopAnalysis::set_array_access_range_in_list(Node* node, ext_sym_set ext_syms_l, char use_type)
    {
        for(ext_sym_set::iterator it = ext_syms_l.begin(); it != ext_syms_l.end(); ++it)
        {
            if (it->is_array())
            {
                Nodecl::NodeclBase array_access_ = it->get_nodecl();
                if (array_access_.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript array_access = array_access_.as<Nodecl::ArraySubscript>();
                    Nodecl::ArraySection new_array_access = set_array_access_range(node, array_access, *it, use_type);
                }
                else
                {
                    internal_error("Array sections not yet implemented in array analysis within loops", 0);
                }
            }
            else
            {   // nothing to do
            }
        }
    }
    
    void LoopAnalysis::compute_arrays_info_in_loop(Node* node)
    {
        if (!node->is_visited())
        {
            node->set_visited(true);
            
            Node_type ntype = node->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                if (ntype == GRAPH_NODE)
                {
                    compute_arrays_info_in_loop(node->get_data<Node*>(_ENTRY_NODE));
                }
                else if (ntype == BASIC_NORMAL_NODE || ntype == BASIC_LABELED_NODE)
                {   // Check for arrays in that are used in some way within the BB statements
                    set_array_access_range_in_list(node, node->get_ue_vars(), /* use type */ '0');
                    set_array_access_range_in_list(node, node->get_killed_vars(), /* use type */ '1');
                }
                else if (ntype == BASIC_FUNCTION_CALL_NODE)
                {   // Check for arrays in the arguments
                    // TODO
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    compute_arrays_info_in_loop(*it);
                }
            }
            else
            {
                return;
            }
        }
    }
    
    void LoopAnalysis::compute_arrays_info(Node* node)
    {
        if (!node->is_visited())
        {
            node->set_visited(true);
            
            Node_type ntype = node->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                if (ntype == GRAPH_NODE)
                {
                    Node* entry = node->get_data<Node*>(_ENTRY_NODE);
                    if (node->get_data<Graph_type>(_GRAPH_TYPE) == LOOP)
                    {
                        compute_arrays_info_in_loop(entry);
                    }
                    else
                    {
                        compute_arrays_info(entry);
                    }
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    compute_arrays_info(*it);
                }
            }
            else
            {
                return;
            }
        }        
    }
    
    void LoopAnalysis::analyse_loops(Node* node)
    {
        // Compute induction_variables_info
        compute_induction_varaibles_info(node);
        ExtensibleGraph::clear_visits(node);
        
        // Analyse the possible arrays in the node
        compute_arrays_info(node);
        ExtensibleGraph::clear_visits(node);
    }
}