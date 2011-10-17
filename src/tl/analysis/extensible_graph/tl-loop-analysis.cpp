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

#include "tl-cfg-renaming-visitor.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-loop-analysis.hpp"

namespace TL
{
    // *** Induction Var Info *** //
    
    InductionVarInfo::InductionVarInfo(Symbol s, Nodecl::NodeclBase lb)
        : _s(s), _lb(lb), _ub(Nodecl::NodeclBase::null()), _step(Nodecl::NodeclBase::null()), 
          _step_is_one(false), _step_is_positive(2)
    {}
    
    Symbol InductionVarInfo::get_symbol() const
    {
        return _s;
    }

    Type InductionVarInfo::get_type() const
    {
        return _s.get_type();
    }

    Nodecl::NodeclBase InductionVarInfo::get_lb() const
    {
        return _lb;
    }

    void InductionVarInfo::set_lb(Nodecl::NodeclBase lb)
    {
        _lb = lb;
    }

    Nodecl::NodeclBase InductionVarInfo::get_ub() const
    {
        return _ub;
    }

    void InductionVarInfo::set_ub(Nodecl::NodeclBase ub)
    {
        _ub = ub;
    }

    Nodecl::NodeclBase InductionVarInfo::get_step() const
    {
        return _step;
    }

    void InductionVarInfo::set_step(Nodecl::NodeclBase step)
    {
        _step = step;
    }
    
    bool InductionVarInfo::step_is_one() const
    {
        return _step_is_one;
    }
    
    void InductionVarInfo::set_step_is_one(bool step_is_one)
    {
        _step_is_one = step_is_one;
    }

    int InductionVarInfo::step_is_positive() const
    {
        return _step_is_positive;
    }
    
    void InductionVarInfo::set_step_is_positive(int step_is_positive)
    {
        _step_is_positive = step_is_positive;
    }

    bool InductionVarInfo::operator==(const InductionVarInfo &v) const
    {
        return ( (_s == v._s) && (_lb ==  v._lb) && (_ub == v._ub) && (_step == v._step) );
    }

    bool InductionVarInfo::operator<(const InductionVarInfo &v) const
    {
        if ( (_s < v._s) 
             || ( (_s == v._s) && (_lb < v._lb) )
             || ( (_s == v._s) && (_lb ==  v._lb) && (_ub < v._ub) )
             || ( (_s == v._s) && (_lb ==  v._lb) && (_ub == v._ub) && (_step < v._step) ) )
        {
            return true;
        }
        
        return false;        
    }
    
    
    // *** Loop Analysis *** //
    
    LoopAnalysis::LoopAnalysis()
        : _induction_vars(), _loop_control_s()
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
            _induction_vars.insert(induc_vars_map::value_type(_loop_control_s.top(), ind));
        }
        else if (init.is<Nodecl::Assignment>())
        {
            Nodecl::Assignment init_ = init.as<Nodecl::Assignment>();
            Symbol def_var = init_.get_lhs().get_symbol();
            Nodecl::NodeclBase def_expr = init_.get_rhs();
            
            InductionVarInfo* ind = new InductionVarInfo(def_var, def_expr);
            _induction_vars.insert(induc_vars_map::value_type(_loop_control_s.top(), ind));
        }
        else
        {
            internal_error("Node kind '%s' while analysing the induction variables in loop init expression not yet implemented",
                ast_print_node_type(init.get_kind()));
        }
    }
    
    InductionVarInfo* LoopAnalysis::induction_vars_l_contains_symbol(Symbol s) const
    {
        for (induc_vars_map::const_iterator it = _induction_vars.begin(); it != _induction_vars.end(); ++it)
        {
            if (it->second->get_symbol() == s)
            {
                return it->second;
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
                    nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
                    loop_info_var->set_step(Nodecl::NodeclBase(one));
                    loop_info_var->set_step_is_one(true);
                    loop_info_var->set_step_is_positive(1);
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
                    nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
                    loop_info_var->set_step(Nodecl::NodeclBase(one));
                    loop_info_var->set_step_is_one(true);
                    loop_info_var->set_step_is_positive(1);
                    std::cerr << "Setting to 1 positive step in symbol " << loop_info_var->get_symbol().get_name() << std::endl;
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
        _loop_control_s.push(loop_control);
        // Compute loop control info
        traverse_loop_init(loop_control.get_init());
        traverse_loop_cond(loop_control.get_cond());
        traverse_loop_step(loop_control.get_next());
        
        // Check whether the statements within the loop modify the induction variables founded in the loop control
        induction_vars_are_defined_in_node(loop_node);
        
        // Print induction variables info
        std::cerr << "Info computed about the induction variables for Loop Control: '" 
                  << loop_control.prettyprint() << "'" << std::endl;
        std::pair<induc_vars_map::iterator, induc_vars_map::iterator> actual_ind_vars = _induction_vars.equal_range(loop_control);
        for(induc_vars_map::iterator it = actual_ind_vars.first; it != actual_ind_vars.second; ++it)
        {
            prettyprint_induction_var_info(it->second);
        }
        _loop_control_s.pop();
    }

    std::map<Symbol, Nodecl::NodeclBase> LoopAnalysis::get_induction_vars_mapping() const
    {
        std::map<Symbol, Nodecl::NodeclBase> result;
        
        for(induc_vars_map::const_iterator it = _induction_vars.begin(); it != _induction_vars.end(); ++it)
        {
            InductionVarInfo* ivar = it->second;
            Symbol s(ivar->get_symbol());
            result[s] = Nodecl::Range::make(ivar->get_lb(), ivar->get_ub(), ivar->get_step(), ivar->get_type(), 
                                            s.get_filename(), s.get_line());
        }
        
        return result;
    }

    void LoopAnalysis::compute_induction_variables_info(Node* node)
    {
        if (!node->is_visited())
        {
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
                    compute_induction_variables_info(node->get_data<Node*>(_ENTRY_NODE));
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    compute_induction_variables_info(*it);
                }
            }
            else
            {
                return;
            }
        }
    }
   
    void LoopAnalysis::set_access_range(Node* node, const char use_type, Nodecl::NodeclBase nodecl, 
                                        std::map<Symbol, Nodecl::NodeclBase> ind_var_map)
    {
        std::cerr << "Renaming nodecl " << nodecl.prettyprint() << std::endl;
        CfgRenamingVisitor renaming_v(ind_var_map, nodecl.get_filename().c_str(), nodecl.get_line());
        ObjectList<Nodecl::NodeclBase> renamed = renaming_v.walk(nodecl);
        if (!renamed.empty())
        {
            if (renamed.size() == 1)
            {
                if (use_type == '0')
                { 
                    node->unset_ue_var(ExtensibleSymbol(nodecl));
                    node->set_ue_var(ExtensibleSymbol(renamed[0]));
                }
                else if (use_type == '1')
                {
                    
                    node->unset_killed_var(ExtensibleSymbol(nodecl));
                    node->set_killed_var(ExtensibleSymbol(renamed[0]));
                }
                else if (use_type == '2')
                {
                    node->unset_reaching_definition(nodecl);
                    if (nodecl.is<Nodecl::ArraySubscript>() || nodecl.is<Nodecl::ArraySection>())
                    {   // The access of the array is protected by the loop boundaries, we have to reduce by one the limits
                        InductionVarInfo* ind_var = induction_vars_l_contains_symbol(renaming_v.get_matching_symbol());
                        char is_positive = ind_var->step_is_positive();
                        if (is_positive == 1)
                        {
                            nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
                            
                            if (nodecl.is<Nodecl::ArraySubscript>())
                            {
                                Nodecl::ArraySubscript rename = renamed[0].as<Nodecl::ArraySubscript>();
                                Nodecl::NodeclBase subscripts = rename.get_subscripts();
                                Nodecl::Range actual_range = rename.get_subscripts().as<Nodecl::Range>();
                                Nodecl::NodeclBase upper = actual_range.get_upper();
                                std::cerr << "actual range " << actual_range.prettyprint() << std::endl;
                                std::cerr << " upper node " << upper.prettyprint() << " type: " << ast_print_node_type(subscripts.get_kind()) 
                                        << std::endl;
                                Nodecl::NodeclBase new_range = Nodecl::Range::make(actual_range.get_lower(),
                                                                                Nodecl::Minus::make(actual_range.get_upper(),
                                                                                                    Nodecl::IntegerLiteral(one),
                                                                                                    nodecl.get_type(), 
                                                                                                    nodecl.get_filename(), nodecl.get_line()),
                                                                                actual_range.get_stride(),
                                                                                nodecl.get_type(), nodecl.get_filename(), nodecl.get_line());
                                node->set_reaching_definition(nodecl, new_range);
                            }
                            else
                            {
                                internal_error("Array Sections not yet implemented for induction variable range substitution", 0);
                            }
                        }
                        else if (is_positive == 0)
                        {
                            nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
                            Nodecl::Range actual_range = renamed[0].as<Nodecl::Range>();
                            Nodecl::NodeclBase new_range = Nodecl::Range::make(Nodecl::Add::make(actual_range.get_lower(),
                                                                                                 Nodecl::IntegerLiteral(one),
                                                                                                 nodecl.get_type(), 
                                                                                                 nodecl.get_filename(), nodecl.get_line()), 
                                                                               actual_range.get_upper(),
                                                                               actual_range.get_stride(),
                                                                               nodecl.get_type(), nodecl.get_filename(), nodecl.get_line());
                            node->set_reaching_definition(nodecl, new_range);
                        }
                        else
                        {   // We cannot define the boundaries of the loop
                            node->set_reaching_definition(nodecl, Nodecl::NodeclBase::null());
                        }
                    }
                    else
                    {   // if the variable is the induction variable, then the range overpasses the loop limits
                        node->set_reaching_definition(nodecl, renamed[0]);
                    }
                }                
                else
                {
                    internal_error("Unexpected type of variable use '%s' in node '%d'", use_type, node->get_id());
                }                    
                std::cerr << "Converting nodecl " << nodecl.prettyprint() << " to " << renamed[0].prettyprint() << std::endl;
            }
            else
            {
                internal_error("More than one nodecl returned while renaming constant values", 0);
            }
        }        
    }
   
    void LoopAnalysis::set_access_range_in_ext_sym_set(Node* node, ext_sym_set nodecl_l, const char use_type)
    {
        std::map<Symbol, Nodecl::NodeclBase> ind_var_map = get_induction_vars_mapping();
        for(ext_sym_set::iterator it = nodecl_l.begin(); it != nodecl_l.end(); ++it)
        {
            if (it->is_array())
            {    
                set_access_range(node, use_type, it->get_nodecl(), ind_var_map);
            }
        }
    }
    
    void LoopAnalysis::set_access_range_in_nodecl_map(Node* node, nodecl_map nodecl_m)
    {
        std::map<Symbol, Nodecl::NodeclBase> ind_var_map = get_induction_vars_mapping();
        for(nodecl_map::iterator it = nodecl_m.begin(); it != nodecl_m.end(); ++it)
        {
            set_access_range(node, '2', it->first, ind_var_map);
            set_access_range(node, '2', it->second, ind_var_map);
        }        
    }
    
    void LoopAnalysis::compute_ranges_for_variables_in_loop(Node* node)
    {
        if (!node->is_visited())
        {
            node->set_visited(true);
            
            Node_type ntype = node->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                if (ntype == GRAPH_NODE)
                {
                    compute_ranges_for_variables_in_loop(node->get_data<Node*>(_ENTRY_NODE));
                }
                else if (ntype == BASIC_NORMAL_NODE || ntype == BASIC_LABELED_NODE)
                {   // Check for arrays in that are used in some way within the BB statements
                    
                    set_access_range_in_ext_sym_set(node, node->get_ue_vars(), /* use type */ '0');
                    set_access_range_in_ext_sym_set(node, node->get_killed_vars(), /* use type */ '1');
                    set_access_range_in_nodecl_map(node, node->get_reaching_definitions());
                }
                else if (ntype == BASIC_FUNCTION_CALL_NODE)
                {   // Check for arrays in the arguments
                    // TODO
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    compute_ranges_for_variables_in_loop(*it);
                }
            }
            else
            {
                return;
            }
        }
    }
    
    void LoopAnalysis::compute_ranges_for_variables(Node* node)
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
                        compute_ranges_for_variables_in_loop(entry);
                    }
                    else
                    {
                        compute_ranges_for_variables(entry);
                    }
                    ExtensibleGraph::clear_visits(node);
                    node->set_graph_node_use_def();
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    compute_ranges_for_variables(*it);
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
        compute_induction_variables_info(node);
        ExtensibleGraph::clear_visits(node);
        
        // Analyse the possible arrays in the node
        compute_ranges_for_variables(node);
        ExtensibleGraph::clear_visits(node);
    }
}