/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-loop-analysis.hpp"

namespace TL
{
    namespace Analysis
    {
        static bool definition_is_within_the_loop(Nodecl::NodeclBase family, Nodecl::NodeclBase iv_st, Node* iv_node, Node* loop);
        
        void LoopAnalysis::induction_variable_detection(Node* node)
        {
            if ( !node->is_visited() )
            {
                std::cerr << "induction_variable_detection  -->  " << node->get_id() << std::endl;
                node->set_visited(true);
                
                Node_type ntype = node->get_type();
                if (ntype == GRAPH_NODE)
                {
                    // IV is computed from inner to outer loops
                    Node* entry = node->get_graph_entry_node();
                    induction_variable_detection(entry);
                        
                    if (node->get_graph_type() == LOOP)
                    {   // Treat current loop
                        ExtensibleGraph::clear_visits_in_level(entry, node);
                        detect_basic_induction_variables(entry, node);
                        ExtensibleGraph::clear_visits_in_level(entry, node);
                        detect_derived_induction_variables(entry, node);
                        
                        // Propagate current iv's to inner loops
                        // TODO Think about it. Do we really need to propagate it??
                    }
                    
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {    
                    induction_variable_detection(*it);
                }
            }
        }
        
        void LoopAnalysis::detect_basic_induction_variables(Node* node, Node* loop)
        {
            if ( !node->is_visited() && 
                 node->get_id() != loop->get_graph_exit_node()->get_id() )
            {
                node->set_visited(true);
                
                // Look for IVs in the current node
                ObjectList<Nodecl::NodeclBase> stmts = node->get_statements();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                {
                    Nodecl::NodeclBase iv = is_basic_induction_variable(*it, loop);
                    if (!iv.is_null())
                        loop->set_induction_variable(iv, InductionVariableData('1', iv));
                }
                
                // Traverse the children
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    detect_basic_induction_variables(*it, loop);
                }
            }
        }
        
        // FIXME This method does not cover any possible induction variable.
        // F.i., 'st': iv = 1 + iv + z, where 'z' is loop invariant, will return false
        Nodecl::NodeclBase LoopAnalysis::is_basic_induction_variable(Nodecl::NodeclBase st, Node* loop)
        {
            Nodecl::NodeclBase res = Nodecl::NodeclBase::null();
            
            Node* loop_entry = loop->get_graph_entry_node();
            int id_end = loop->get_graph_exit_node()->get_id();
            if ( st.is<Nodecl::Assignment>() )
            {
                Nodecl::Assignment _st = st.as<Nodecl::Assignment>();
                Nodecl::NodeclBase lhs = _st.get_lhs();
                Nodecl::NodeclBase rhs = _st.get_rhs();
                    
                Nodecl::NodeclBase lhs_rhs, rhs_rhs;
                if ( rhs.is<Nodecl::Add>() )
                {   /*! Expressions accepted
                     * . iv = iv + x;
                     * . iv = x + iv; */
                    Nodecl::Add _rhs = rhs.as<Nodecl::Add>();
                    lhs_rhs = _rhs.get_lhs();
                    rhs_rhs = _rhs.get_rhs();
                    
                    if ( Nodecl::Utils::equal_nodecls(lhs, lhs_rhs) )
                    {
                        _constant = rhs_rhs;
                        if ( is_loop_invariant(loop_entry, id_end) )
                            res = lhs;
                    }
                    else if ( Nodecl::Utils::equal_nodecls(lhs, rhs_rhs) )
                    {
                        _constant = lhs_rhs;
                        if ( is_loop_invariant(loop_entry, id_end) )
                            res = lhs;
                    }
                }
                else if ( rhs.is<Nodecl::Minus>() )
                {   /*! Expressions accepted
                     * . iv = iv - x; */
                    Nodecl::Minus _rhs = rhs.as<Nodecl::Minus>();
                    lhs_rhs = _rhs.get_lhs();
                    rhs_rhs = _rhs.get_rhs();
                    
                    if ( Nodecl::Utils::equal_nodecls(lhs, lhs_rhs) )
                    {
                        _constant = rhs_rhs;
                        if ( is_loop_invariant(loop_entry, id_end) )
                            res = lhs;
                    }
                }
            }
            else if ( st.is<Nodecl::AddAssignment>() )
            {   /*! Expressions accepted
                     * . iv += x; */
                Nodecl::AddAssignment _st = st.as<Nodecl::AddAssignment>();
                Nodecl::NodeclBase lhs = _st.get_lhs();
                _constant = _st.get_rhs();
                if ( is_loop_invariant(loop_entry, id_end) )
                    res = lhs;
            }
            else if ( st.is<Nodecl::MinusAssignment>() )
            {   /*! Expressions accepted
                     * . iv -= x; */
                Nodecl::MinusAssignment _st = st.as<Nodecl::MinusAssignment>();
                Nodecl::NodeclBase lhs = _st.get_lhs();
                _constant = _st.get_rhs();
                if ( is_loop_invariant(loop_entry, id_end) )
                    res = lhs;
            }
            else if ( st.is<Nodecl::Preincrement>() )
            {
                Nodecl::Preincrement _st = st.as<Nodecl::Preincrement>();
                res = _st.get_rhs();
            }
            else if ( st.is<Nodecl::Postincrement>() )
            {
                Nodecl::Postincrement _st = st.as<Nodecl::Postincrement>();
                res = _st.get_rhs();
            }
            else if ( st.is<Nodecl::Predecrement>() )
            {
                Nodecl::Predecrement _st = st.as<Nodecl::Predecrement>();
                res = _st.get_rhs();
            }
            else if ( st.is<Nodecl::Postdecrement>() )
            {
                Nodecl::Postdecrement _st = st.as<Nodecl::Postdecrement>();
                res = _st.get_rhs();
            }
            
            if (!res.is_null())
            {   /*! \st has the form of an induction variable, but still 'iv' can be modified 
                 *  within the loop in some way it makes iv not to be an IV. */
                if ( is_false_induction_variable(res, st, loop_entry, id_end) )
                {
                    res = Nodecl::NodeclBase::null();
                }
                
            }
            
            return res;
        }
        
        void LoopAnalysis::detect_derived_induction_variables(Node* node, Node* loop)
        {
            if ( !node->is_visited() && 
                 node->get_id() != loop->get_graph_exit_node()->get_id() )
            {
                node->set_visited(true);
                
                // Look for IVs in the current node
                ObjectList<Nodecl::NodeclBase> stmts = node->get_statements();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                {
                    Nodecl::NodeclBase iv_family;
                    Nodecl::NodeclBase iv = is_derived_induction_variable(*it, node, loop, iv_family);
                    if (!iv.is_null())
                        loop->set_induction_variable(iv, InductionVariableData('2', iv));
                }
                
                // Traverse the children
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    detect_derived_induction_variables(*it, loop);
                }
            }
        }
        
        Nodecl::NodeclBase LoopAnalysis::is_derived_induction_variable(Nodecl::NodeclBase st, Node* node, Node* loop, Nodecl::NodeclBase& family)
        {
            Nodecl::NodeclBase res = Nodecl::NodeclBase::null();
            
            Node* loop_entry = loop->get_graph_entry_node();
            int id_end = loop->get_graph_exit_node()->get_id();
            if ( st.is<Nodecl::Assignment>() )
            {   /*! Expressions accepted
                 * . iv_1 = iv_2 + x;
                 * . iv_1 = x + iv_2; 
                 * . iv_1 = iv_2 * x;
                 * . iv_1 = x * iv_2; */
                Nodecl::Assignment _st = st.as<Nodecl::Assignment>();
                Nodecl::NodeclBase lhs = _st.get_lhs();
                Nodecl::NodeclBase rhs = _st.get_rhs();
                
                Nodecl::NodeclBase lhs_rhs, rhs_rhs;
                if ( rhs.is<Nodecl::Add>() )
                {
                    Nodecl::Add rhs_ = rhs.as<Nodecl::Add>();
                    lhs_rhs = rhs_.get_lhs();
                    rhs_rhs = rhs_.get_rhs();
                }
                else if ( rhs.is<Nodecl::Mul>() )
                {
                    Nodecl::Mul rhs_ = rhs.as<Nodecl::Mul>();
                    lhs_rhs = rhs_.get_lhs();
                    rhs_rhs = rhs_.get_rhs();
                }
                
                IV_map loop_ivs = loop->get_induction_variables();
                _constant = Nodecl::NodeclBase::null();
                if ( loop_ivs.find( lhs_rhs ) != loop_ivs.end( ) )
                {    
                    _constant = rhs_rhs;
                    family = lhs_rhs;
                }
                else if ( loop_ivs.find(rhs_rhs) != loop_ivs.end( ) )
                {    
                    _constant= lhs_rhs;
                    family = rhs_rhs;
                }
                if ( !_constant.is_null() )
                {
                    if ( is_loop_invariant(loop_entry, id_end) )
                    {   //! expression of type: "lhs = family (+,*) _constant"
                        if ( /*!loop_ivs.at(family).is_basic()*/ true )
                        {
                            // The only definition of \family that reaches \lhs is within the loop
                            _constant = family;
                            if (only_definition_is_in_loop(st, node, loop))
                            // The family of \family must not be defined between the definition of \family and \lhs
                            // TODO
                            if (true)
                            {
                                res = lhs;
                            }
                        }
                        else
                        {
                            res = lhs;
                        }
                    }
                }
            }
            
            if ( is_false_induction_variable(res, st, loop_entry, id_end) )
            {
                res = Nodecl::NodeclBase::null();
            }
            
            return res;
        }
        
        bool LoopAnalysis::only_definition_is_in_loop(Nodecl::NodeclBase iv_st, Node* iv_node, Node* loop)
        {
            // FIXME Check whether there is a definition of the induction variable outside the loop
            if (is_there_unique_definition_in_loop(iv_st, iv_node, loop) /*&& 
                !is_there_definition_out_loop(iv_st, is_node, node, loop)*/ )
                return true;
            else
                return false;
        }
        
        bool LoopAnalysis::is_there_unique_definition_in_loop(Nodecl::NodeclBase iv_st, Node* iv_node, Node* loop)
        {
            bool defined = false;
            
            //! Look for definitions of \family in \iv_node, before \iv_st
            ObjectList<Nodecl::NodeclBase> current_stmts = iv_node->get_statements();
            for (ObjectList<Nodecl::NodeclBase>::iterator it = current_stmts.begin(); 
                 it != current_stmts.end() && !Nodecl::Utils::equal_nodecls(*it, iv_st); ++it)
            {
                if ( walk(*it) )    // This visit returns true if the statement \*it contains a definition of the nodecl \_constant
                    defined = true;
            }
            
            //! Look for definitions of \family in \loop
            iv_node->set_visited_aux(true);
            ObjectList<Node*> parents = iv_node->get_parents();
            for (ObjectList<Node*>::iterator it = parents.begin(); it!= parents.end(); ++it)
            {
                if ( is_there_definition_in_loop_(iv_st, iv_node, *it, loop) )
                    if (defined)
                        return false;
                    else
                        defined = true;
            }
            
            ExtensibleGraph::clear_visits_aux_backwards_in_level(iv_node, loop);
            
            return defined;
            
        }
        
        bool LoopAnalysis::is_there_definition_in_loop_(Nodecl::NodeclBase iv_st, Node* iv_node, Node* node, Node* loop)
        {
            bool defined = false;
            
            if (iv_node->get_id() == node->get_id())
            {   //! Look for definitions of \family in \iv_node, after \iv_st
                ObjectList<Nodecl::NodeclBase> current_stmts = iv_node->get_statements();
                ObjectList<Nodecl::NodeclBase>::iterator it = current_stmts.begin();
                while ( !Nodecl::Utils::equal_nodecls(*it, iv_st) )
                    ++it;
                ++it;   // Get the following statement
                for (; it != current_stmts.end(); ++it)
                {
                    if ( walk(*it) )    // This visit returns true if the statement \*it contains a definition of the nodecl \_constant
                        defined = true;
                }
            }
            else if ( !iv_node->is_visited_aux() && 
                      node->get_id() != loop->get_graph_entry_node()->get_id() )
            {
                iv_node->set_visited_aux(true);

                //! Look for definitions of \family in \node
                ObjectList<Nodecl::NodeclBase> current_stmts = node->get_statements();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = current_stmts.begin(); it != current_stmts.end(); ++it)
                {
                    if ( walk(*it) )    // This visit returns true if the statement \*it contains a definition of the nodecl \_constant
                        if ( defined )
                            return false;
                        else
                            defined = true;
                }
                
                //! Look for definitions of \family in \node parents
                ObjectList<Node*> parents = node->get_parents();
                for (ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
                {
                    // FIXME
//                     if ( !ExtensibleGraph::node_is_in_outer_node( *it, loop ) )
//                     {
//                         //! Look for definitions out of the loop
//                         // TODO
//                     }
//                     else if ( !only_definition_is_in_loop(*it, loop) )
                        return false;
                }
            }
            
            return defined;
        }
        
        // TODO
        static bool definition_is_within_the_loop(Nodecl::NodeclBase family, Nodecl::NodeclBase iv_st, Node* iv_node, Node* loop)
        {
            
        }
        
        bool LoopAnalysis::is_loop_invariant(Node* node, int id_end)
        {
            bool res = is_loop_invariant_(node, id_end);
            ExtensibleGraph::clear_visits_aux(node);
            return res;
        }
        
        bool LoopAnalysis::is_loop_invariant_(Node* node, int id_end)
        {
            bool res = true;
            
            if (!node->is_visited_aux())
            {   
                node->set_visited_aux(true);
                
                if ( node->get_type() == GRAPH_NODE )
                {
                    res = is_loop_invariant_(node->get_graph_entry_node(), id_end);
                }
                else
                {
                    ObjectList<Nodecl::NodeclBase> stmts = node->get_statements();
                    for (ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                    {
                        if ( walk(*it) )
                        {    
                            res = false;    // The statement '*it' modifies some symbol contained in 'constant'
                            break;
                        }    
                    }
                }
                
                if ( res && node->get_id() != id_end )
                {    
                    ObjectList<Node*> children = node->get_children();
                    for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it )
                    {
                        is_loop_invariant_(*it, id_end);
                    }
                }
            }
            
            return res;
        }
        
        bool LoopAnalysis::is_false_induction_variable(Nodecl::NodeclBase iv, Nodecl::NodeclBase stmt, Node* node, int id_end)
        {
            bool res = is_false_induction_variable_(iv, stmt, node, id_end);
            ExtensibleGraph::clear_visits_aux(node);
            return res;
        }
        
        bool LoopAnalysis::is_false_induction_variable_(Nodecl::NodeclBase iv, Nodecl::NodeclBase stmt, Node* node, int id_end)
        {
            bool res = false;
            
            if ( !node->is_visited_aux() )
            {
                node->set_visited_aux(true);
                
                //! Look for false IVs in the current \node
                _constant = iv;
                ObjectList<Nodecl::NodeclBase> stmts = node->get_statements();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                {
                    if ( !Nodecl::Utils::equal_nodecls(stmt, *it) )
                        if ( walk(*it) )
                        {
                            res = true;
                            break;
                        }
                }
                
                if ( !res && node->get_id() == id_end)
                {   //! If \iv is still an  iv, then look for false positives in \node's children
                    ObjectList<Node*> children = node->get_children();
                    for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    {
                        if ( is_false_induction_variable_(iv, stmt, *it, id_end) )
                        {    
                            res = true;
                            break;
                        }
                    }
                }
            }
            
            return res;
        }
        
        void LoopAnalysis::print_induction_variables(Node* node)
        {
            if ( node->is_visited() )
            {
                node->set_visited(true);
                
                if ( node->get_type() == GRAPH_NODE )
                {
                    //! Look for IVs in the inner nodes
                    print_induction_variables(node->get_graph_entry_node());
                    
                    //! Check for IVs in the current loop
                    if ( node->get_graph_type() == LOOP )
                    {
                        IV_map current_ivs = node->get_induction_variables();
                    }
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    print_induction_variables(*it);
                }
            }
        }
        
        
        //////////////////////////////////////////////////////////////////////////
        /// Visitor for nodecl modification checking
        //////////////////////////////////////////////////////////////////////////
        
        bool LoopAnalysis::join_list(TL::ObjectList<bool>& list)
        {
            bool res = false;
            for (ObjectList<bool>::iterator it = list.begin(); it != list.end(); ++it)
            {
                res |= *it;
            }
            return res;
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::Symbol& n)
        {
            bool res = _defining && (Nodecl::Utils::equal_nodecls(n, _constant));
            _defining = false;
            return res;
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::Dereference& n)
        {
            // In case we where defining (left side of an assignment or a pre- post- incr- decrement)
            // a dereference causes that the symbol itself will not be changed, but the referenced object
            _defining = false;
            return walk(n.get_rhs());
        }

        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::ArraySubscript& n)
        {
            if (_defining)
            {
                // Return true when 'n' contains '_constant' or they are equal
                MatchingVisitor visitor(n);
                return visitor.walk(_constant);
            }
            
            _defining = false;
            return walk(n.get_subscripts());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::ClassMemberAccess& n)
        {
            if (_defining)
            {
                // Return true when 'n' contains '_constant' or they are equal
                MatchingVisitor visitor(n);
                return visitor.walk(_constant);
            }
            
            return false;
        }

        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::Assignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::AddAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::MinusAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::MulAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::DivAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::ModAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::BitwiseShlAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::ShrAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::BitwiseAndAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::BitwiseOrAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::BitwiseXorAssignment& n)
        {
            return visit_assignment(n.get_lhs(), n.get_rhs());
        }
        
        bool LoopAnalysis::visit_assignment(Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs)
        {
            _defining = true;
            bool res = walk(lhs);
            if (!res)
            {    
                _defining = false;
                res = walk(rhs);
            }
            
            return res;
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::FunctionCall& n)
        {
            Symbol func_sym = n.get_called().get_symbol();
            ObjectList<TL::Type> param_types = func_sym.get_type().parameters();
            Nodecl::List args = n.get_arguments().as<Nodecl::List>();
            return visit_function(func_sym, param_types, args);
        }
        
        LoopAnalysis::Ret LoopAnalysis::visit(const Nodecl::VirtualFunctionCall& n)
        {
            Symbol func_sym = n.get_called().get_symbol();
            ObjectList<TL::Type> param_types = func_sym.get_type().parameters();
            Nodecl::List args = n.get_arguments().as<Nodecl::List>();
            return visit_function(func_sym, param_types, args);
        }
        
        bool LoopAnalysis::visit_function(Symbol func_sym, ObjectList<Type> param_types, Nodecl::List arguments)
        {
            ObjectList<Type>::iterator itp = param_types.begin();
            Nodecl::List::iterator ita = arguments.begin();
            // if #args < #params => default parameters do not require symbol matching in any arguments
            // if #args > #params => ellipsed arguments can only be passed by value
            for(; itp != param_types.end() && ita != arguments.end(); ++itp, ++ita)
            {
                if ( Nodecl::Utils::equal_nodecls(_constant, *ita) && itp->is_any_reference())
                {   // The symbol is an argument passed by reference
                    for (ObjectList<ExtensibleGraph*>::iterator it = _cfgs.begin(); it != _cfgs.end(); ++it)
                    {
                        if ( (*it)->get_function_symbol() == func_sym)
                        {
                            if ( is_loop_invariant((*it)->get_graph()->get_graph_entry_node(), 
                                                   (*it)->get_graph()->get_graph_exit_node()->get_id()) )
                                return true;
                            return false;
                        }
                    }
                }
            }
            
            return false;
        }
        
        
        //////////////////////////////////////////////////////////////////////////
        /// Visitor for nodecl matching: looks for 'contained' in 'container'
        //////////////////////////////////////////////////////////////////////////
        
        MatchingVisitor::MatchingVisitor(Nodecl::NodeclBase nodecl)
            : _node_to_find(nodecl)
        {}
            
        bool MatchingVisitor::join_list(TL::ObjectList<bool>& list)
        {
            bool res = false;
            for (ObjectList<bool>::iterator it = list.begin(); it != list.end(); ++it)
            {
                res |= *it;
            }
            return res;
        }
            
        MatchingVisitor::Ret MatchingVisitor::visit(const Nodecl::Symbol& n)
        {
            return Nodecl::Utils::equal_nodecls(_node_to_find, n);
        }
        
        MatchingVisitor::Ret MatchingVisitor::visit(const Nodecl::ArraySubscript& n)
        {
            if ( Nodecl::Utils::equal_nodecls(_node_to_find, n) )
            {
                return true;
            }
            else if ( _node_to_find.is<Nodecl::ArraySubscript>() )
            {
                Nodecl::ArraySubscript array_to_find = _node_to_find.as<Nodecl::ArraySubscript>();
                if ( Nodecl::Utils::equal_nodecls(array_to_find.get_subscripted(), n.get_subscripted()) )
                {
                    Nodecl::NodeclBase subscripts_to_find = array_to_find.get_subscripts();
                    Nodecl::NodeclBase subscripts_base = n.get_subscripts();
                    
                    if ( Nodecl::Utils::equal_nodecls(subscripts_to_find, subscripts_base) )
                        return true;
                    else
                    {
                        if ( subscripts_to_find.is_constant() && subscripts_base.is_constant() )
                            return false;
                        else if ( subscripts_base.is<Nodecl::Range>() )
                        {
                            Nodecl::Range subscripts_range = subscripts_base.as<Nodecl::Range>();
                            Nodecl::NodeclBase lb = subscripts_range.get_lower();
                            Nodecl::NodeclBase ub = subscripts_range.get_lower();
                            
                            if ( Nodecl::Utils::equal_nodecls(lb, subscripts_to_find) || Nodecl::Utils::equal_nodecls(ub, subscripts_to_find) )
                                return true;
                            else
                            {
                                if ( subscripts_to_find.is_constant() && lb.is_constant() && ub.is_constant() )
                                {
                                    const_value_t* subs_const = subscripts_to_find.get_constant();
                                    if ( lb.get_constant() <= subs_const && subs_const <= ub.get_constant() )
                                    {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            return walk(n.get_subscripted());
        }
        
        MatchingVisitor::Ret MatchingVisitor::visit(const Nodecl::ClassMemberAccess& n)
        {
            if ( Nodecl::Utils::equal_nodecls(_node_to_find, n) )
            {
                return true;
            }
            else if ( Nodecl::Utils::equal_nodecls(_node_to_find, n.get_lhs()) )
            {
                return true;
            }
            
            return walk(n.get_lhs());
        }
    }
}