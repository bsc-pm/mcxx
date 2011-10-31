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
#include "cxx-utils.h"

#include "tl-cfg-renaming-visitor.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-loop-analysis.hpp"
#include "tl-static-analysis.hpp"

namespace TL
{
    // *** Induction Var Info *** //
    
    InductionVarInfo::InductionVarInfo(Symbol s, Nodecl::NodeclBase lb)
        : _s(s), _lb(lb), _ub(Nodecl::NodeclBase::null()), _stride(Nodecl::NodeclBase::null()), _stride_is_positive(2)
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

    Nodecl::NodeclBase InductionVarInfo::get_stride() const
    {
        return _stride;
    }

    void InductionVarInfo::set_stride(Nodecl::NodeclBase stride)
    {
        _stride = stride;
    }
   
    int InductionVarInfo::stride_is_positive() const
    {
        return _stride_is_positive;
    }
    
    void InductionVarInfo::set_stride_is_positive(int stride_is_positive)
    {
        _stride_is_positive = stride_is_positive;
    }

    bool InductionVarInfo::operator==(const InductionVarInfo &v) const
    {
        return ( (_s == v._s) && (_lb ==  v._lb) && (_ub == v._ub) && (_stride == v._stride) );
    }

    bool InductionVarInfo::operator<(const InductionVarInfo &v) const
    {
        if ( (_s < v._s) 
             || ( (_s == v._s) && (_lb < v._lb) )
             || ( (_s == v._s) && (_lb ==  v._lb) && (_ub < v._ub) )
             || ( (_s == v._s) && (_lb ==  v._lb) && (_ub == v._ub) && (_stride < v._stride) ) )
        {
            return true;
        }
        
        return false;        
    }
   
    
    // *** Node hash ans comparator *** //
   
    size_t Node_hash::operator() (const int& n) const
    {
        return n;
    }
    
    bool Node_comp::operator() (const int& n1, const int& n2) const
    {
        return (n1 == n2);
    }
        
    
    // *** Loop Analysis *** //
    
    LoopAnalysis::LoopAnalysis()
        : _induction_vars()
    {}
    
    void LoopAnalysis::traverse_loop_init(Node* loop_node, Nodecl::NodeclBase init)
    {
        if (init.is<Nodecl::Comma>())
        {
            Nodecl::Comma init_ = init.as<Nodecl::Comma>();
            traverse_loop_init(loop_node, init_.get_rhs());
            traverse_loop_init(loop_node, init_.get_lhs());
        }
        else if (init.is<Nodecl::ObjectInit>())
        {
            Nodecl::ObjectInit init_ = init.as<Nodecl::ObjectInit>();
            Symbol def_var = init_.get_symbol();
            Nodecl::NodeclBase def_expr = def_var.get_initialization();
            
            InductionVarInfo* ind = new InductionVarInfo(def_var, def_expr);
            _induction_vars.insert(induc_vars_map::value_type(loop_node->get_id(), ind));
        }
        else if (init.is<Nodecl::Assignment>())
        {
            Nodecl::Assignment init_ = init.as<Nodecl::Assignment>();
            Symbol def_var = init_.get_lhs().get_symbol();
            Nodecl::NodeclBase def_expr = init_.get_rhs();
            
            InductionVarInfo* ind = new InductionVarInfo(def_var, def_expr);
            _induction_vars.insert(induc_vars_map::value_type(loop_node->get_id(), ind));
        }
        else
        {
            internal_error("Node kind '%s' while analysing the induction variables in loop init expression not yet implemented",
                ast_print_node_type(init.get_kind()));
        }
    }
    
    InductionVarInfo* LoopAnalysis::induction_vars_l_contains_symbol(Node* node, Symbol s) const
    {
        std::pair<induc_vars_map::const_iterator, induc_vars_map::const_iterator> actual_ind_vars = _induction_vars.equal_range(node->get_id());
        for (induc_vars_map::const_iterator it = actual_ind_vars.first; it != actual_ind_vars.second; ++it)
        {
            if (it->second->get_symbol() == s)
            {
                return it->second;
            }
        }
        return NULL;
    }
    
    void LoopAnalysis::traverse_loop_cond(Node* loop_node, Nodecl::NodeclBase cond)
    {
        // Logical Operators
        if (cond.is<Nodecl::LogicalAnd>())
        {
//             Nodecl::LogicalAnd cond_ = cond.as<Nodecl::LogicalAnd>();
//             
//             result = traverse_loop_cond(init_info_l, cond_.get_lhs());
//             
//             // To mix the values is not trivial, we have to get the bigger or the smaller, depending on the stride (positive or negative)
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
            ub = Nodecl::Utils::reduce_expression(ub);
            
            InductionVarInfo* loop_info_var;
            if ( (loop_info_var = induction_vars_l_contains_symbol(loop_node, def_var)) != NULL )
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
            if ( (loop_info_var = induction_vars_l_contains_symbol(loop_node, def_var)) != NULL )
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
            lb = Nodecl::Utils::reduce_expression(lb);
            
            InductionVarInfo* loop_info_var;
            if ( (loop_info_var = induction_vars_l_contains_symbol(loop_node, def_var)) != NULL )
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
            if ( (loop_info_var = induction_vars_l_contains_symbol(loop_node, def_var)) != NULL )
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
    
    void LoopAnalysis::traverse_loop_stride(Node* loop_node, Nodecl::NodeclBase stride)
    {
        if (stride.is<Nodecl::Preincrement>() || stride.is<Nodecl::Postincrement>())
        {
            Nodecl::NodeclBase rhs;
            if (stride.is<Nodecl::Preincrement>())
            {
                Nodecl::Preincrement stride_ = stride.as<Nodecl::Preincrement>();
                rhs = stride_.get_rhs();
            }
            else
            {   // Post-increment
                Nodecl::Postincrement stride_ = stride.as<Nodecl::Postincrement>();
                rhs = stride_.get_rhs();
            }
           
            InductionVarInfo* loop_info_var;
            if ( (loop_info_var = induction_vars_l_contains_symbol(loop_node, rhs.get_symbol())) != NULL )
            {
                nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
                loop_info_var->set_stride(Nodecl::NodeclBase(one));
                loop_info_var->set_stride_is_positive(1);            
            }
            else
            {
                internal_error("The stride of loop '%s' do not correspond to the variable used in the initial and codition expressions." \
                               " This is not yet supported", loop_node->get_graph_label().prettyprint().c_str());
            }
        }
        else if (stride.is<Nodecl::Predecrement>() || stride.is<Nodecl::Postdecrement>())
        {
            Nodecl::NodeclBase rhs;
            if (stride.is<Nodecl::Predecrement>())
            {
                Nodecl::Predecrement stride_ = stride.as<Nodecl::Predecrement>();
                rhs = stride_.get_rhs();
            }
            else
            {   // Post-decrement
                Nodecl::Postdecrement stride_ = stride.as<Nodecl::Postdecrement>();
                rhs = stride_.get_rhs();
            }
           
            InductionVarInfo* loop_info_var;
            if ( (loop_info_var = induction_vars_l_contains_symbol(loop_node, rhs.get_symbol())) != NULL )
            {
                nodecl_t minus_one = const_value_to_nodecl(const_value_get_minus_one(/* bytes */ 4, /* signed*/ 1));
                loop_info_var->set_stride(Nodecl::NodeclBase(minus_one));
                loop_info_var->set_stride_is_positive(0);            
            }
            else
            {
                internal_error("The stride of loop '%s' do not correspond to the variable used in the initial and codition expressions." \
                               " This is not yet supported", loop_node->get_graph_label().prettyprint().c_str());
            }
        }
        else if (stride.is<Nodecl::AddAssignment>())
        {
            Nodecl::AddAssignment stride_ = stride.as<Nodecl::AddAssignment>();
            
            InductionVarInfo* loop_info_var;
            if ( (loop_info_var = induction_vars_l_contains_symbol(loop_node, stride_.get_lhs().get_symbol())) != NULL )
            {
                loop_info_var->set_stride(stride_.get_rhs());
                // FIXME May be we can figure out the sign of the stride
                loop_info_var->set_stride_is_positive(2);
            }
            else
            {
                internal_error("The stride of loop '%s' do not correspond to the variable used in the initial and codition expressions." \
                               " This is not yet supported", loop_node->get_graph_label().prettyprint().c_str());
            }
        }
        else
        {
            internal_error("Node kind '%s' while analysing the induction variables in loop stride expression not yet implemented",
                ast_print_node_type(stride.get_kind()));            
        }
    }

    char LoopAnalysis::induction_vars_are_defined_in_node(Node* node, Node* loop_node)
    {
        if (!node->is_visited())
        {
            node->set_visited(true);
            
            Node_type ntype = node->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                if( (ntype == BASIC_NORMAL_NODE || ntype == BASIC_LABELED_NODE 
                    || ntype == BASIC_FUNCTION_CALL_NODE || ntype == GRAPH_NODE) 
                   && (node->get_id() != loop_node->get_stride_node()->get_id()) )
                {   // The node has Use-Def
                    ext_sym_set killed_vars = node->get_killed_vars();
                    for (ext_sym_set::iterator it = killed_vars.begin(); it != killed_vars.end(); ++it)
                    {
                        if (induction_vars_l_contains_symbol(loop_node, it->get_symbol()) != NULL)
                        {
                            return '1';
                        }
                    }
                    ext_sym_set undef_behaviour_vars = node->get_undefined_behaviour_vars();
                    for (ext_sym_set::iterator it = undef_behaviour_vars.begin(); it != undef_behaviour_vars.end(); ++it)
                    {
                        if (induction_vars_l_contains_symbol(loop_node, it->get_symbol()) != NULL)
                        {
                            return '2';
                        }
                    }                       
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    char result = induction_vars_are_defined_in_node(*it, loop_node);
                    if (result != '0')
                    {
                        return result;
                    }
                }
                return '0';
            }
            else
            {
                return '0';
            }
        }
        else
        {
            return '0';
        }
    }
    
    void LoopAnalysis::prettyprint_induction_var_info(InductionVarInfo* var_info)
    {
        std::cerr << "    Symbol: " << var_info->get_symbol().get_name()
                  << "    LB = '" << var_info->get_lb().prettyprint() << "'"
                  << "    UB = '" << var_info->get_ub().prettyprint() << "'"
                  << "    STEP = '" << var_info->get_stride().prettyprint() << "'" << std::endl;
    }    
    
    void LoopAnalysis::print_induction_vars_in_loop_info(Node* loop_node)
    {
        Nodecl::NodeclBase loop_control = loop_node->get_graph_label();
        std::cerr << " **** NODE '" << loop_node->get_id() << "', LOOP '" << loop_control.prettyprint() << "' ****" << std::endl;
        std::pair<induc_vars_map::iterator, induc_vars_map::iterator> actual_ind_vars = _induction_vars.equal_range(loop_node->get_id());
        for(induc_vars_map::iterator it = actual_ind_vars.first; it != actual_ind_vars.second; ++it)
        {
            prettyprint_induction_var_info(it->second);
        }
        std::cerr << " **** ************************************************** ****" << std::endl;
    }
    
    void LoopAnalysis::print_induction_vars_info()
    {
        for(induc_vars_map::iterator it = _induction_vars.begin(); it != _induction_vars.end(); ++it)
        {
            std::cerr << " LOOP NODE '" << it->first << std::endl;
            prettyprint_induction_var_info(it->second);
        }
    }
    
    void LoopAnalysis::compute_loop_induction_vars(Node* loop_node)
    {
        // Propagate induction variables of the outer loop to the actual loop
        Node* outer_node = loop_node->get_outer_node();
        while (outer_node != NULL)
        {
            if (outer_node->get_graph_type() == LOOP)
            {
                std::pair<induc_vars_map::const_iterator, induc_vars_map::const_iterator> outer_ind_vars =
                        _induction_vars.equal_range(outer_node->get_id());
                
                for (induc_vars_map::const_iterator it = outer_ind_vars.first; it != outer_ind_vars.second; ++it)
                {
                    _induction_vars.insert(induc_vars_map::value_type(loop_node->get_id(), it->second));
                }
            }
            outer_node = outer_node->get_outer_node();
        }
        
        // Compute actual loop control info
        Nodecl::LoopControl loop_control = loop_node->get_graph_label().as<Nodecl::LoopControl>();
        traverse_loop_init(loop_node, loop_control.get_init());
        traverse_loop_cond(loop_node, loop_control.get_cond());
        traverse_loop_stride(loop_node, loop_control.get_next());
        
        // Check whether the statements within the loop modify the induction variables founded in the loop control
        Node* entry = loop_node->get_data<Node*>(_ENTRY_NODE);
        char result = induction_vars_are_defined_in_node(entry, loop_node);
        if (result != '0')
        {   // FIXME The variable in the loop control is not an invariant
            internal_error("Analysis of variables in a loop control that are not an induction variable are not yet implemented", 0);
        }
        
        ExtensibleGraph::clear_visits(entry);
    }

    std::map<Symbol, Nodecl::NodeclBase> LoopAnalysis::get_induction_vars_mapping(Node* loop_node) const
    {
        std::map<Symbol, Nodecl::NodeclBase> result;
        std::pair<induc_vars_map::const_iterator, induc_vars_map::const_iterator> actual_ind_vars = _induction_vars.equal_range(loop_node->get_id());
        for(induc_vars_map::const_iterator it = actual_ind_vars.first; it != actual_ind_vars.second; ++it)
        {
            InductionVarInfo* ivar = it->second;
//             std::cerr << "Induction variable: " << ivar->get_symbol().get_name() 
//                       << "[" << ivar->get_lb().prettyprint() << ":" << ivar->get_ub().prettyprint() << ":" 
//                       << ivar->get_stride().prettyprint() << "]" << std::endl;
            Symbol s(ivar->get_symbol());
            result[s] = Nodecl::Range::make(ivar->get_lb(), ivar->get_ub(), ivar->get_stride(), ivar->get_type(), 
                                            s.get_filename(), s.get_line());
        }
        
        return result;
    }

    std::map<Symbol, int> LoopAnalysis::get_induction_vars_direction(Node* loop_node) const
    {
        std::map<Symbol, int> result;
        std::pair<induc_vars_map::const_iterator, induc_vars_map::const_iterator> actual_ind_vars = _induction_vars.equal_range(loop_node->get_id());
        for(induc_vars_map::const_iterator it = actual_ind_vars.first; it != actual_ind_vars.second; ++it)
        {
            InductionVarInfo* ivar = it->second;
            Symbol s(ivar->get_symbol());
            result[s] = ivar->stride_is_positive();
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
                        compute_loop_induction_vars(node);
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
   
    Nodecl::NodeclBase LoopAnalysis::set_access_range(Node* node, Node* loop_node,
                                                      const char use_type, Nodecl::NodeclBase nodecl, 
                                                      std::map<Symbol, Nodecl::NodeclBase> ind_var_map,
                                                      Nodecl::NodeclBase reach_def_var)
    {
        std::cerr << "Setting access range in '" << node->get_id() << " (loop " << loop_node->get_id()
                  << ") ' for : " << nodecl.prettyprint() << std::endl;
        Nodecl::NodeclBase renamed_nodecl;
        CfgRenamingVisitor renaming_v(ind_var_map, nodecl.get_filename().c_str(), nodecl.get_line());
        ObjectList<Nodecl::NodeclBase> renamed = renaming_v.walk(nodecl);
        
        if (!renamed.empty())
        {
            if (renamed.size() == 1)
            {
                std::cerr << "Renaming performed: " << nodecl.prettyprint() << " --> " << renamed[0].prettyprint() << std::endl;
                if (use_type == '0')
                { 
                    node->unset_ue_var(ExtensibleSymbol(nodecl));
                    node->set_ue_var(ExtensibleSymbol(renamed[0]));
                    renamed_nodecl = renamed[0];
                }
                else if (use_type == '1')
                {
                    
                    node->unset_killed_var(ExtensibleSymbol(nodecl));
                    node->set_killed_var(ExtensibleSymbol(renamed[0]));
                    renamed_nodecl = renamed[0];
                }
                else if (use_type == '2' || use_type == '3')
                {
                    InductionVarInfo* ind_var = induction_vars_l_contains_symbol(loop_node, renaming_v.get_matching_symbol());
                    int is_positive = ind_var->stride_is_positive();
                    
                    if (nodecl.is<Nodecl::ArraySubscript>())
                    {
                        if (use_type == '2')
                        {   // We are renaming the key, this is the defined variable
                            node->rename_reaching_defintion_var(nodecl, renamed[0]);
                            renamed_nodecl = renamed[0];
                        }
                        else
                        {   //  We are renaming the init expression of a reaching definition
                            node->set_reaching_definition(reach_def_var, renamed[0]);
                            renamed_nodecl = reach_def_var;
                        }
                    }
                    else
                    { 
                        if (use_type == '3')
                        {   // In a reaching definition, we cannot modify the defined variable, just the init expression
                            Nodecl::NodeclBase range = renamed[0];
                            node->set_reaching_definition(reach_def_var, range);
                            renamed_nodecl = reach_def_var;
                        }
                        else
                        {
                            renamed_nodecl = nodecl;
                        }
                    }
                }                
                else
                {
                    internal_error("Unexpected type of variable use '%s' in node '%d'", use_type, node->get_id());
                }
            }
            else
            {
                internal_error("More than one nodecl returned while renaming constant values", 0);
            }
        }
        else
        {
            renamed_nodecl = nodecl;
        }
        
        return renamed_nodecl;
    }
   
    void LoopAnalysis::set_access_range_in_ext_sym_set(Node* node, Node* loop_node, ext_sym_set nodecl_l, const char use_type)
    {
        std::map<Symbol, Nodecl::NodeclBase> ind_var_map = get_induction_vars_mapping(loop_node);
        for(ext_sym_set::iterator it = nodecl_l.begin(); it != nodecl_l.end(); ++it)
        {
            if (it->is_array())
            {  
                set_access_range(node, loop_node, use_type, it->get_nodecl(), ind_var_map);
            }
        }
    }
    
    void LoopAnalysis::set_access_range_in_nodecl_map(Node* node, Node* loop_node, nodecl_map nodecl_m)
    {
        std::map<Symbol, Nodecl::NodeclBase> ind_var_map = get_induction_vars_mapping(loop_node);
        for(nodecl_map::iterator it = nodecl_m.begin(); it != nodecl_m.end(); ++it)
        {
            Nodecl::NodeclBase first = it->first, second = it->second;
            Nodecl::NodeclBase renamed_reach_def = set_access_range(node, loop_node, '2', it->first, ind_var_map);
            set_access_range(node, loop_node, '3', it->second, ind_var_map, renamed_reach_def);
        }        
    }
    
    void LoopAnalysis::compute_ranges_for_variables_in_loop(Node* node, Node* loop_node)
    {
        if (!node->is_visited())
        {
            node->set_visited(true);
            
            Node_type ntype = node->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                if (ntype == GRAPH_NODE)
                {
                    compute_ranges_for_variables_in_loop(node->get_data<Node*>(_ENTRY_NODE), node);
                }
                else if (ntype == BASIC_NORMAL_NODE || ntype == BASIC_LABELED_NODE || ntype == BASIC_FUNCTION_CALL_NODE)
                {   // Check for arrays in that are used in some way within the BB statements
                    set_access_range_in_ext_sym_set(node, loop_node, node->get_ue_vars(), /* use type */ '0');
                    set_access_range_in_ext_sym_set(node, loop_node, node->get_killed_vars(), /* use type */ '1');
                    set_access_range_in_nodecl_map(node, loop_node, node->get_reaching_definitions());
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    compute_ranges_for_variables_in_loop(*it, loop_node);
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
                        compute_ranges_for_variables_in_loop(entry, node);
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
  
    void LoopAnalysis::propagate_reach_defs_in_for_loop_special_nodes(Node* loop_node)
    {   // Ranges for the Condition and Increment node are different that ranges for the code within the For Statement
        
        // Propagate reach defs to entry node
        Node* entry = loop_node->get_graph_entry_node();
        nodecl_map combined_parents_reach_defs = StaticAnalysis::compute_parents_reach_defs(loop_node);
        nodecl_map actual_reach_defs = entry->get_reaching_definitions();
        for(nodecl_map::iterator it = combined_parents_reach_defs.begin(); it != combined_parents_reach_defs.end(); ++it)
        {
            if (actual_reach_defs.find(it->first) == actual_reach_defs.end())
            {   // Only if the definition is not performed within the node, we store the parents values
                entry->set_reaching_definition(it->first, it->second);
            }
        }
        
        // Propagate reach defs to conditional node
        Node* cond = entry->get_children()[0];
        combined_parents_reach_defs = StaticAnalysis::compute_parents_reach_defs(cond);
        actual_reach_defs = cond->get_reaching_definitions();
        for(nodecl_map::iterator it = combined_parents_reach_defs.begin(); it != combined_parents_reach_defs.end(); ++it)
        {
            if (actual_reach_defs.find(it->first) == actual_reach_defs.end())
            {   // Only if the definition is not performed within the node, we store the parents values
                cond->set_reaching_definition(it->first, it->second);
            }
        }        
       
        ObjectList<Edge*> cond_exit_edges = cond->get_exit_edges();
        ObjectList<Edge*>::iterator ite = cond_exit_edges.begin();
        
        // Nodes through the True edge has a smaller range for the induction variable
        Node* true_node;
        if ((*ite)->get_type() == TRUE_EDGE)
        {
            true_node = (*ite)->get_target();
        }
        else
        {
            true_node = (*(ite+1))->get_target();
        }
        if (true_node->get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            true_node = true_node->get_data<Node*>(_ENTRY_NODE);
        }
        combined_parents_reach_defs = StaticAnalysis::compute_parents_reach_defs(true_node);
        actual_reach_defs = true_node->get_reaching_definitions();
        std::map<Symbol, Nodecl::NodeclBase> induction_vars_m = get_induction_vars_mapping(loop_node);
        for(nodecl_map::iterator it = combined_parents_reach_defs.begin(); it != combined_parents_reach_defs.end(); ++it)
        {
            if (actual_reach_defs.find(it->first) == actual_reach_defs.end())
            {   // Only if the definition is not performed within the node, we store the parents values
                // If the variable is an induction var, we get here the range of the var within the loop
                if (it->first.is<Nodecl::Symbol>()
                    && induction_vars_m.find(it->first.get_symbol()) != induction_vars_m.end())
                {
                    Nodecl::NodeclBase first = it->first, second = induction_vars_m[it->first.get_symbol()];
                    true_node->set_reaching_definition(it->first, induction_vars_m[it->first.get_symbol()]);
                }
                else
                {
                    Nodecl::NodeclBase first = it->first, second = it->second;
                    true_node->set_reaching_definition(it->first, it->second);
                }
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
