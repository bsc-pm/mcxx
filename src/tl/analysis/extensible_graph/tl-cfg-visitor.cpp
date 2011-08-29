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


#include "cxx-codegen.h"
#include "cxx-process.h"

#include "tl-cfg-visitor.hpp"

namespace TL
{
    CfgVisitor::CfgVisitor(ScopeLink sl)
        : _actual_cfg(NULL), _sl(sl), 
          _actual_loop_info(), _actual_switch_info(), _actual_try_info(),
          _cfgs(), _seq_nodecl()
    {}
    
    CfgVisitor::CfgVisitor(const CfgVisitor& visitor)
    {
        _actual_cfg = visitor._actual_cfg;
        _sl = visitor._sl;
        _actual_loop_info = visitor._actual_loop_info;
        _actual_switch_info = visitor._actual_switch_info;
        _cfgs = visitor._cfgs;
        _seq_nodecl = visitor._seq_nodecl;
    }
    
    CfgVisitor::Ret CfgVisitor::unhandled_node(const Nodecl::NodeclBase& n) 
    {
        std::cerr << "UNHANDLED -> " << ast_print_node_type(n.get_kind()) << std::endl;
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::TopLevel& n)
    {
        ObjectList<Node*> functions = walk(n.get_top_level());
        
        // TODO with the list of functions with its correspondent list of graphs in @cfgs
        // now we can perform some kind of inter-procedural analysis
        
        
        for (ObjectList<ExtensibleGraph*>::iterator it = _cfgs.begin();
            it != _cfgs.end(); 
            ++it)
        {
//             it->live_variable_analysis();
            (*it)->print_graph_to_dot();
        }
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FunctionCode& n)
    {
        TL::Symbol s = n.get_symbol();
        std::string func_decl = s.get_type().get_declaration(s.get_scope(), s.get_name());
        std::cerr << "ESTIC A LA FUNCIO -> " << func_decl << std::endl;

        // Create a new graph for the current function
        _actual_cfg = new ExtensibleGraph(_sl, s.get_name());
        std::cout << "Last nodes : " << _actual_cfg->_last_nodes.size() << std::endl;
        
        ObjectList<Node*> func_stmts = walk(n.get_statements());
        
        // The walk returns a list of nodes created from each statement
        // Now, we have to traverse this list and collapse the nodes when possible in an iterative way
        // until the result do not change
//         bool not_changed = false;
//         while (!not_changed)
//         {
            for (ObjectList<Node*>::iterator it = func_stmts.begin();
                it != func_stmts.end();
                it++)
            {
                std::cerr << "Node ->" << (*it)->get_id() << std::endl;
            }
//         }
       
        // Task subgrpahs must be appended, conservatively, at the end of the master graph
        // FIXME Before or after the Return ??
//         for (ObjectList<Node*>::iterator it = cfg._tasks_node_list.begin();
//             it != cfg._tasks_node_list.end();
//             ++it)
//         {
//             cfg->connect_nodes(cfg._last_nodes[0], *it);
//             cfg->_last_nodes[0] = *it;
//         }

        // Connect the exit nodes to the Exit node of the master graph
        Node* graph_exit = _actual_cfg->_graph->get_data<Node*>("exit");
        graph_exit->set_id(_actual_cfg->_nid);
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, graph_exit);
        _actual_cfg->connect_nodes(_actual_cfg->_unhand_try_excpt_list, graph_exit);
        _actual_cfg->connect_nodes(_actual_cfg->_throw_node_list, graph_exit);
        
        // Remove the unnecessary nodes and join these ones that are always executed consecutively
        _actual_cfg->clear_unnecessary_nodes();
        
        _cfgs.append(_actual_cfg);
        
        return ObjectList<Node*>(1,_actual_cfg->_graph);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::TryBlock& n)
    {
//         ObjectList<Node*> try_parents = _actual_cfg->_last_nodes;
//         walk(n.get_statement());
//         _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
//        
//         Node* first_try_node = try_parents[0]->get_exit_edges()[0]->get_target();
//         compute_catch_parents(first_try_node);
//         _actual_cfg->clear_visits(first_try_node);
//        
//         walk(n.get_catch_handlers());
//         
//         // Process the ellipsis
//         ObjectList<Node*> ellipsis_parents = _actual_cfg->_last_nodes;
//         _actual_cfg->_last_nodes = _actual_try_info.handler_parents;
//         walk(n.get_any());
//         if (ellipsis_parents != _actual_cfg->_last_nodes || !_seq_nodecl.empty())
//         {
//             std::cerr << "There exists handler" << std::endl;
//             _actual_try_info.nhandlers++;
//             _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
//             _actual_try_info.handler_exits.append(_actual_cfg->_last_nodes);
//             
//             // Set the type of the edge between each handler parent and the actual handler
//             for (ObjectList<Node*>::iterator it = _actual_try_info.handler_parents.begin();
//                 it != _actual_try_info.handler_parents.end();
//                 it++)
//             {  
//                 int last_edge_index = (*it)->get_exit_edges().size() - 1;
//                 Edge* catch_edge = (*it)->get_exit_edges()[last_edge_index];
//                 catch_edge->set_data("type", CATCH_EDGE);
//                 catch_edge->set_data("label", std::string("..."));
//             }           
//         }
//         
//         _actual_cfg->_last_nodes = _actual_try_info.handler_exits;
//         
//         _actual_try_info.clear();
        return Ret();
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::CatchHandler& n)
    {
        _actual_try_info.nhandlers++;
        
        // Build the handler nodes
        _actual_cfg->_last_nodes = _actual_try_info.handler_parents;
        Ret r = walk(n.get_statement());
        
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        
        _actual_try_info.handler_exits.append(_actual_cfg->_last_nodes);
        
        // Set the type of the edge between each handler parent and the actual handler
        for (ObjectList<Node*>::iterator it = _actual_try_info.handler_parents.begin();
            it != _actual_try_info.handler_parents.end();
            it++)
        {  
            int last_edge_index = (*it)->get_exit_edges().size() - 1;
            Edge* catch_edge = (*it)->get_exit_edges()[last_edge_index];
            catch_edge->set_data("type", CATCH_EDGE);
            std::string label = c_cxx_codegen_to_str(((Nodecl::NodeclBase)n.get_name()).get_internal_nodecl());
            catch_edge->set_data("label", label);
        }
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Throw& n)
    {
        walk(n.get_rhs());
        return Ret();
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::CompoundStatement& n)
    {
        walk(n.get_statements());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Symbol& n)
    {
        Node* basic_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        return ObjectList<Node*>(1, basic_node);
    }
    
    Node* CfgVisitor::get_first_node(Node* actual_node)
    {
        ObjectList<Edge*> actual_entries = actual_node->get_entry_edges();
        ObjectList<Node*> actual_parents;
        
        if (actual_entries.empty())
        {
            return actual_node;
        }
        else
        {
            for (ObjectList<Edge*>::iterator it = actual_entries.begin();
                it != actual_entries.end();
                ++it)
            {
                Node* parent = get_first_node((*it)->get_source());
                actual_parents.insert(parent);
            }
            
            if (actual_parents.size() != 1)
            {
                internal_error("'%d' nodes founded while looking for the first node of a subgraph\n", 
                            actual_parents.size());
            }
        }
        
        
        return actual_parents[0];
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ExpressionStatement& n)
    {
        std::cerr << "Expression '" << c_cxx_codegen_to_str(((Nodecl::NodeclBase)n).get_internal_nodecl()) << "'" << std::endl;
        
        ObjectList<Node*> expression_nodes = walk(n.get_nest());
        if (expression_nodes.size() != 1)
        {
            internal_error("Parsing an expression we have returned '%d' nodes, and only one must be returned\n", 
                           expression_nodes.size());
        }
        
        // Connect the partial node created recursively with the piece of Graph build until this moment
        Node* expr_first_node = get_first_node(expression_nodes[0]);
        _actual_cfg->clear_visits(expr_first_node);
        
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, expr_first_node);
        
        // Recompute actual last nodes for the actual graph
        _actual_cfg->_last_nodes.clear();
        _actual_cfg->_last_nodes.append(expr_first_node);
        
        return expression_nodes;
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ParenthesizedExpression& n)
    {
        walk(n.get_nest());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ErrExpr& n)
    {
        internal_error("Node '%s' should not arrive here. CFG construction failed.", 
                       ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ObjectInit& n)
    {
        std::cout << "Object init" << std::endl;
        walk(n.get_init_expr());
        _seq_nodecl.append(n);
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        walk(n.get_subscripted());
        walk(n.get_subscripts());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        walk(n.get_lhs());
        walk(n.get_member());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::New& n)
    {
        std::cerr << "warning: node '" << ast_print_node_type(n.get_kind()) << "' not properly implemented!! "
                  << "But the execution continues... " << std::endl;
        walk(n.get_init());
        walk(n.get_placement());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Delete& n)
    {
        walk(n.get_rhs());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::DeleteArray& n)
    {
        walk(n.get_rhs());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Sizeof& n)
    {
        walk(n.get_size_type());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Type& n)
    { // Nothing to be done
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Typeid& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Cast& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Offset& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        walk(n.get_arguments());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        walk(n.get_called());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FunctionCall& n)
    {
        // FIXME If this first condition necessary?? Or the arguments should be parsed with a different Visitor??
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        walk(n.get_arguments());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        walk(n.get_called());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
        ObjectList<Nodecl::NodeclBase> nodecls = _actual_cfg->_last_nodes[0]->get_data<ObjectList<Nodecl::NodeclBase> >("statements");
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Comma& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

// 
    // ************* Literals ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::StringLiteral& n)
    {
        Node* basic_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        return ObjectList<Node*>(1, basic_node);
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BooleanLiteral& n)
    {
        Node* basic_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        return ObjectList<Node*>(1, basic_node);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::IntegerLiteral& n)
    {
        Node* basic_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        return ObjectList<Node*>(1, basic_node);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ComplexLiteral& n)
    {
        Node* real = walk(n.get_real())[0];
        Node* imag = walk(n.get_imag())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, real, imag));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FloatingLiteral& n)
    {
        Node* basic_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        return ObjectList<Node*>(1, basic_node);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::StructuredLiteral& n)
    {
        ObjectList<Node*> items = walk(n.get_items());
        return Ret();
    }


    // ************* Special Statements ************* //       
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::EmptyStatement& n)
    {
        Node* basic_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        return ObjectList<Node*>(1, basic_node);
    }
            
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ReturnStatement& n)
    {
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl);
        _seq_nodecl.clear();
        walk(n.get_value());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
        return Ret();
    }


    // ************* Built-in ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BuiltinExpr& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BuiltinDecl& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }


    // ************* Pragmas ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomDirective& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomConstruct& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomClause& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomLine& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaClauseArg& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }
    
    // ************* Control Flow constructs ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ForStatement& n)
    {
        walk(n.get_loop_header());
        
        Node* empty_exit_node = new Node();
       
        _actual_cfg->_continue_stack.push(_actual_loop_info.next);
        _actual_cfg->_break_stack.push(empty_exit_node);
        walk(n.get_statement());
        if (!_seq_nodecl.empty())
        {   // Some statements remaining to be added from the loop body
            _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl);
            _seq_nodecl.clear();
        }
        // Compute the true edge from the loop condition
        Edge_type aux_etype = ALWAYS_EDGE;
        if (!_actual_loop_info.cond->get_exit_edges().empty())
        {
            _actual_loop_info.cond->get_exit_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
        }
        else
        { // It will be empty when the loop's body is empty.
            aux_etype = TRUE_EDGE;
        }
        _actual_cfg->_continue_stack.pop();
        _actual_cfg->_break_stack.pop();
        
        empty_exit_node->set_id(++_actual_cfg->_nid);
        empty_exit_node->set_data("outer_graph", _actual_cfg->_outer_node.top());
        _actual_cfg->connect_nodes(_actual_loop_info.cond, empty_exit_node, FALSE_EDGE);
        
        // Fill the empty fields of the Increment node
        if (!_actual_cfg->_break_stmt)
        {
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_loop_info.next, aux_etype);
            _actual_cfg->connect_nodes(_actual_loop_info.next, _actual_loop_info.cond);
        }
        else
        {
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, empty_exit_node, aux_etype);
            _actual_cfg->_break_stmt = false; // Reset this value
        }
        
        _actual_cfg->_continue_stmt = false; // Conservatively, reset this value
        
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(empty_exit_node);
        return Ret();
    }        

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LoopControl& n)
    {
//         _actual_loop_info.init = get_expression_node(n.get_init());
//         _actual_loop_info.cond = get_expression_node(n.get_cond());
//         _actual_loop_info.next = get_expression_node(n.get_next(), /* Connect node */false);
        return Ret();
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::WhileStatement& n)
    {
//         // Build condition node
//         Node* condition_node = get_expression_node(n.get_condition());
//      
//         Node* empty_exit_node = new Node();
//         
//         // Build the while body node/s
//         _actual_cfg->_continue_stack.push(condition_node);
//         _actual_cfg->_break_stack.push(empty_exit_node);
//         walk(n.get_statement());
//         _actual_cfg->_continue_stack.pop();
//         _actual_cfg->_break_stack.pop();
//         _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
//         condition_node->get_exit_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
//         
//         // Build the exit node
//         empty_exit_node->set_id(++_actual_cfg->_nid);
//         empty_exit_node->set_data("outer_graph", _actual_cfg->_outer_node.top());
//         _actual_cfg->connect_nodes(condition_node, empty_exit_node, FALSE_EDGE);
//         if (_actual_cfg->_continue_stmt)
//         {
//             _actual_cfg->connect_nodes(_actual_cfg->_continue_stack.top(), empty_exit_node);
//             _actual_cfg->_continue_stmt = false; // Reset this value
//         }
//         else if(_actual_cfg->_break_stmt)
//         {
//             _actual_cfg->connect_nodes(_actual_cfg->_break_stack.top(), empty_exit_node);
//             _actual_cfg->_break_stmt = false; // Reset this value
//         }
//         else
//         {
//             _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, condition_node);
//         }
// 
//         _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(empty_exit_node);
        return Ret();
    }     

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::IfElseStatement& n)
    {
//         Node* empty_exit_node = new Node();
//         
//         // Compose the condition node
//         Node* condition_node = get_expression_node(n.get_condition());
//         
//         // Compose the then node
//         walk(n.get_then());
//         _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
//         condition_node->get_exit_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
//         if (_actual_cfg->_continue_stmt)
//         {
//             _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_cfg->_continue_stack.top());
//             _actual_cfg->_continue_stmt = false; // Reset this value
//         }
//         else if (_actual_cfg->_break_stmt)
//         {
//             _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_cfg->_break_stack.top());
//             _actual_cfg->_break_stmt = false; // Reset this value
//         }
//         else if (_actual_cfg->_goto_stmt)
//         {    
//             _actual_cfg->_goto_stmt = false; // Reset this value
//         }
//         else
//         {    
//             _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, empty_exit_node);
//         }
//        
//         // Compose the else node, if it exists
//         ObjectList<Node*> then_last_nodes = _actual_cfg->_last_nodes;
//         _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(condition_node);
//         walk(n.get_else());
//         _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
//         empty_exit_node->set_id(++_actual_cfg->_nid);
//         empty_exit_node->set_data("outer_graph", _actual_cfg->_outer_node.top());        
//         if (then_last_nodes != _actual_cfg->_last_nodes)
//         {   // There exists an else statement
//             if (_actual_cfg->_continue_stmt)
//             {
//                 _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_cfg->_continue_stack.top());
//                 _actual_cfg->_continue_stmt = false; // Reset this value
//             }
//             else if (_actual_cfg->_break_stmt)
//             {
//                 _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_cfg->_break_stack.top());
//                 _actual_cfg->_break_stmt = false; // Reset this value                
//             }
//             else if (_actual_cfg->_goto_stmt)
//             {    
//                 _actual_cfg->_goto_stmt = false;
//             }
//             else
//             {   
//                 _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, empty_exit_node);
//             }
//         }
//         else
//         {
//             _actual_cfg->connect_nodes(condition_node, empty_exit_node);
//         }
// 
//         // Link the If condition with the FALSE statement (else or empty node)
//         condition_node->get_exit_edges()[1]->set_data("type", FALSE_EDGE);
//         
//         _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(empty_exit_node);
        return Ret();
    }



    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::SwitchStatement& n)
    {
//         // Compose the condition node
//         _actual_switch_info.cond = get_expression_node(n.get_switch());
//        
//         // Compose the statements nodes
//         walk(n.get_statement());
//         
//         // Link properly the exit node
//         Node* empty_exit_node = new Node();
//         empty_exit_node->set_id(++_actual_cfg->_nid);
//         empty_exit_node->set_data("outer_graph", _actual_cfg->_outer_node.top());
//         
//         // Finish computation of switch exit nodes
//         if (_actual_switch_info.ncases == -1)
//         {
//             _actual_cfg->connect_nodes(_actual_switch_info.cond, empty_exit_node);
//         }
//         else
//         {
//             if (_actual_cfg->_break_stmt)
//             {
//                 _actual_switch_info.cases_break.append(_actual_cfg->_last_nodes);
//                 _actual_cfg->_break_stmt = false;
//             }
//             else
//             {   // Avoid the case no statement appears within the switch statement
//                 _actual_cfg->connect_nodes(_actual_cfg->_last_nodes[0], empty_exit_node);
//             }
// 
//             _actual_cfg->connect_nodes(_actual_switch_info.cases_break, empty_exit_node);
//         }
// 
//         _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(empty_exit_node);
//         _actual_switch_info.clear();
        return Ret();
    }      

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::CaseStatement& n)
    {
//         // If previous cases were parsed, now we have to decide their exit edges
//         if (_actual_switch_info.ncases > -1)
//         {    
//             if (_actual_cfg->_break_stmt)
//             {
//                 _actual_switch_info.cases_break.append(_actual_cfg->_last_nodes);
//                 _actual_cfg->_break_stmt = false;
//             }
//             else
//             {
//                 _actual_switch_info.case_no_break = _actual_cfg->_last_nodes[0];
//             }
//         }
//         
//         _actual_switch_info.ncases++;
//         
//         // Prepare parent nodes list
//         _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(_actual_switch_info.cond);
//         if (_actual_switch_info.case_no_break != NULL)
//         {
//             _actual_cfg->_last_nodes.append(_actual_switch_info.case_no_break);
//             _actual_switch_info.case_no_break == NULL;
//         }
//         
//         // Build case nodes
//         walk(n.get_statement());
//         _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
//         
//         // Set the proper labels to the edge
//         int actual_case = _actual_switch_info.ncases;
//         Edge* case_edge = _actual_switch_info.cond->get_exit_edges()[actual_case];
//         case_edge->set_data("type", CASE_EDGE);
//         std::string label = c_cxx_codegen_to_str(((Nodecl::NodeclBase)n.get_case()).get_internal_nodecl());
//         case_edge->set_data("label", label);
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::DefaultStatement& n)
    {
//         // If previous cases were parsed, now we have to decide their exit edges
//         if (_actual_switch_info.ncases > -1)
//         {    
//             if (_actual_cfg->_break_stmt)
//             {
//                 _actual_switch_info.cases_break.append(_actual_cfg->_last_nodes);
//                 _actual_cfg->_break_stmt = false;
//             }
//             else
//             {
//                 _actual_switch_info.case_no_break = _actual_cfg->_last_nodes[0];
//             }
//         }
//         
//         _actual_switch_info.ncases++;
//         
//         // Prepare parent nodes list
//         _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(_actual_switch_info.cond);
//         if (_actual_switch_info.case_no_break != NULL)
//         {
//             _actual_cfg->_last_nodes.append(_actual_switch_info.case_no_break);
//             _actual_switch_info.case_no_break == NULL;
//         }        
// 
//         // Build default nodes
//         walk(n.get_statement());
//         _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
//         
//         // Set the proper labels to the edge
//         int actual_case = _actual_switch_info.ncases;
//         Edge* case_edge = _actual_switch_info.cond->get_exit_edges()[actual_case];
//         case_edge->set_data("type", CASE_EDGE);
//         case_edge->set_data("label", std::string("-1"));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BreakStatement& n)
    {
        _actual_cfg->_break_stmt = true;
        return Ret();
    }      

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ContinueStatement& n)
    {
        _actual_cfg->_continue_stmt = true;
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::GotoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }  
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ConditionalExpression& n)
    {
//         std::cerr << "Conditional expression " << c_cxx_codegen_to_str(((Nodecl::NodeclBase)n).get_internal_nodecl()) << std::endl;
// 
//         ObjectList<Node*> last_nodes = _actual_cfg->_last_nodes;
//         // Build condition node
//        
//         Node* condition_node = get_expression_node(n.get_condition());
//         
//         ObjectList<Node*> condition_exits = _actual_cfg->_last_nodes;
//         ObjectList<Node*> exit_parents;
//         
//         // Build true node
//         Node* true_node = get_expression_node(n.get_true());
//         exit_parents.append(true_node);
//         
//         // Build false node
//         _actual_cfg->_last_nodes = condition_exits;
//         Node* false_node = get_expression_node(n.get_false());
//         exit_parents.append(false_node);
//         
//         _actual_cfg->_last_nodes = exit_parents;
        return Ret();
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LabeledStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::DoStatement& n)
    {
//         ObjectList<Node*> do_parents = _actual_cfg->_last_nodes;
//         
//         walk(n.get_statement());
//         _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
//         
//         Node* condition_node = get_expression_node(n.get_condition());
//         
//         // The Statement walk will generate only one entry node for all parents
//         _actual_cfg->connect_nodes(condition_node, do_parents[0]->get_exit_edges()[0]->get_target());
        return Ret();
    }
    

    // ************* Assignment ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Assignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::AddAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::SubAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::DivAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::MulAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ModAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ShrAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ShlAssignment& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }


    // ************* Binary operations ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Add& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Minus& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Mul& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }    

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Div& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }        

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Mod& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Power& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LogicalAnd& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LogicalOr& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BitwiseAnd& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BitwiseOr& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BitwiseXor& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Concat& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Equal& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Different& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LowerThan& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::GreaterThan& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LowerOrEqualThan& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Shr& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1,merge_nodes(n, left, right));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Shl& n)
    {
        Node* left = walk(n.get_lhs())[0];
        Node* right = walk(n.get_rhs())[0];
       
        return ObjectList<Node*>(1, merge_nodes(n, left, right));
    }


    // ************* Unary operations ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Predecrement& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Postdecrement& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Preincrement& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Postincrement& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Plus& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Neg& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BitwiseNot& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LogicalNot& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }
    
    
    // ************* Addresses ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Derreference& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Reference& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }

    
    // ************* Fortran specifics ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Text& n)
    {   
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }
   
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranNamedPairSpec& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }    
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranWhere& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }   

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranWherePair& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }   

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::SubscriptTriplet& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }   

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranLabelAssignStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranComputedGotoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }   

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranAssignedGotoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }   

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranIoSpec& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }   

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FieldDesignator& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }    

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::IndexDesignator& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranEquivalence& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranData& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranImpliedDo& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranForall& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranArithmeticIfStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }      

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranNullifyStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }   
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranIoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranOpenStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranCloseStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    } 

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranReadStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    } 
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranWriteStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranPrintStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranStopStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranAllocateStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FortranDeallocateStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }
    

    /* *********************** Non visiting methods *********************** */
    
//     Node* CfgVisitor::get_expression_node(const Nodecl::NodeclBase& n, bool connect_node)
//     {
//         // Compose the node with the sequential statements store until this moment
//         _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
//         
//         // Build the new node
//         BreakingExpressionVisitor visitor(_sl);
//         visitor.walk(n);
//         Node* expression_node;
//         if (visitor._broken_expression)
//         {// The expression will be built within a graph node
//             // When the node must not be connected, we have to preserve @_last_nodes attribute
//             ObjectList<Node*> last_nodes = _actual_cfg->_last_nodes;
//             
//             // Compose the graph node
//             switch (visitor._breakage_type)
//             {
//                 case 1: // Function_call
//                     expression_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), AST_t(), "function_call");
//                     break;
//                 case 2: // Conditional expression
//                     expression_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), AST_t(), "conditional_expression");
//                     break;
//                 case 3: // Function call & Conditional Expression
//                     expression_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), AST_t(), "splitted_instruction");
//                     break;
//                 default:
//                     internal_error("Breaking type wrongly computed for the expression '%s'", 
//                                    c_cxx_codegen_to_str(((Nodecl::NodeclBase)n).get_internal_nodecl()));
//             }
//             if (connect_node) {                
//                 _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, expression_node);
//             }
//             
//             // Connect the entry node
//             _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(expression_node->get_data<Node*>("entry"));
//             
//             // Create and connect the statements nodes
//             walk(n);
//             if (!n.is<Nodecl::ConditionalExpression>())
//             {
//                 _seq_nodecl.append(n);
//                 _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl);
//                 _seq_nodecl.clear();                
//             }
//             
//             // Create and connect the exit nodes
//             Node* exit = expression_node->get_data<Node*>("exit");
//             exit->set_id(++_actual_cfg->_nid);
//             _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, exit);
//            
//             _actual_cfg->_outer_node.pop();
//             
//             // Recalculate @_last_nodes attribute
//             _actual_cfg->_last_nodes.clear();
//             if (connect_node) { 
//                 _actual_cfg->_last_nodes.append(expression_node);
//             } else {
//                 _actual_cfg->_last_nodes = last_nodes;
//             }             
//         }
//         else
//         {
//             if (connect_node)
//             {
//                 _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
//                 expression_node = _actual_cfg->_last_nodes[0];
//             }
//             else
//             {
//                 expression_node = _actual_cfg->create_unconnected_node(n);
//             }
//         }
//         
//         return expression_node;
//     }
//     
//     void  CfgVisitor::compute_catch_parents(Node* node)
//     {
//         while (!node->is_visited())
//         {
//             node->set_visited(true);
//             _actual_try_info.handler_parents.append(node);
//             ObjectList<Edge*> exit_edges = node->get_exit_edges();
//             for(ObjectList<Edge*>::iterator it = exit_edges.begin();
//                 it != exit_edges.end();
//                 it++)
//             {
//                 compute_catch_parents((*it)->get_target());
//             }
//         }
//     }
    
    Node* CfgVisitor::merge_nodes(Nodecl::NodeclBase n, Node* first, Node* second)
    {
        ObjectList<Node*> parents;
        if (first->get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            parents.append(first);
        }
        else
        {
            std::cerr << "Deleted node " << first->get_id() << std::endl;
            delete first;
        }
        
        // Only second node must be NULL and it will be the case of unary operations
        if (second != NULL)
        {
            if (second->get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
            {
                parents.append(second);
            }
            else
            {
                std::cerr << "Deleted node " << second->get_id() << std::endl;
                delete second;
            }
        }
        
        Node* result = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        if (!parents.empty())
        {    
            _actual_cfg->connect_nodes(parents, result);
        }
        
        return result;
    }
}
