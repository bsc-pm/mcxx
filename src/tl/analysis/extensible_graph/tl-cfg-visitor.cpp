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
          _actual_loop_info(), _actual_try_info(), 
          /* _omp_pragma_info_s(), */ _switch_cond_s(), 
          _cfgs()
    {}
    
    CfgVisitor::CfgVisitor(const CfgVisitor& visitor)
    {
        _actual_cfg = visitor._actual_cfg;
        _sl = visitor._sl;
        _actual_loop_info = visitor._actual_loop_info;
        _actual_try_info = visitor._actual_try_info;
        /* _omp_pragma_info_s = visitor._omp_pragma_info_s; */
        _switch_cond_s = visitor._switch_cond_s;
        _cfgs = visitor._cfgs;
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
       
        return functions;
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FunctionCode& n)
    {
        TL::Symbol s = n.get_symbol();
        std::string func_decl = s.get_type().get_declaration(s.get_scope(), s.get_name());
        std::cerr << "ESTIC A LA FUNCIO -> " << func_decl << std::endl;
        std::string nom = s.get_name();
        
//         if (nom == "main")
//         {
            // Create a new graph for the current function
            _actual_cfg = new ExtensibleGraph(_sl, s.get_name());
            
            ObjectList<Node*> func_stmts = walk(n.get_statements());
            
            // FIXME Do I really have to do that here???
            // The walk returns a list of nodes created from each statement
            // Now, we have to traverse this list and collapse the nodes when possible in an iterative way
            // until the result do not change
    //         bool not_changed = false;
    //         while (!not_changed)
    //         {
    //             for (ObjectList<Node*>::iterator it = func_stmts.begin();
    //                 it != func_stmts.end();
    //                 it++)
    //             {
    //                 std::cerr << "Node ->" << (*it)->get_id() << std::endl;
    //             }
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
            Node* graph_exit = _actual_cfg->_graph->get_data<Node*>(_EXIT_NODE);
            graph_exit->set_id(++_actual_cfg->_nid);
                 
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, graph_exit);
            
            // Remove the unnecessary nodes and join these ones that are always executed consecutively
            _actual_cfg->clear_unnecessary_nodes();
            
            _cfgs.append(_actual_cfg);
            
            return ObjectList<Node*>(1, _actual_cfg->_graph);
//         }
//         else
//         {
//             Node* empty = new Node();
//             return ObjectList<Node*>(1, empty);
//         }
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::TryBlock& n)
    {
        struct try_block_nodes new_try_block;
        _actual_try_info.append(new_try_block);
        ObjectList<Node*> try_parents = _actual_cfg->_last_nodes;
        ObjectList<Node*> try_stmts = walk(n.get_statement());
        
        Node* first_try_node = try_parents[0]->get_exit_edges()[0]->get_target();
        compute_catch_parents(first_try_node);
        _actual_cfg->clear_visits(first_try_node);
      
        ObjectList<Node*> handlers_l = walk(n.get_catch_handlers());
        
        // Process the ellipsis
        ObjectList<Node*> ellipsis_parents = _actual_cfg->_last_nodes;
        struct try_block_nodes* actual_try_info = &_actual_try_info.back();
        _actual_cfg->_last_nodes = actual_try_info->handler_parents;
        ObjectList<Node*> ellipsis_l = walk(n.get_any());
        if (!ellipsis_l.empty())
        {
            actual_try_info->nhandlers++;
            actual_try_info->handler_exits.append(_actual_cfg->_last_nodes);
            
            // Set the type of the edge between each handler parent and the actual handler
            for (ObjectList<Node*>::iterator it = actual_try_info->handler_parents.begin();
                it != actual_try_info->handler_parents.end();
                ++it)
            { 
                Edge* catch_edge = (*it)->get_exit_edges().back();
                catch_edge->set_data(_EDGE_TYPE, CATCH_EDGE);
                catch_edge->set_data(_EDGE_LABEL, ObjectList<Nodecl::NodeclBase>(1, Nodecl::NodeclBase::null()));
            }           
        }
        
        _actual_cfg->_last_nodes = actual_try_info->handler_exits;
        
        _actual_try_info.pop_back();
        if (!try_stmts.empty())
            return try_stmts;
        else if (!handlers_l.empty())
            return handlers_l;
        else if (!ellipsis_l.empty())
            return ellipsis_l;
        else return Ret();
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::CatchHandler& n)
    {
        struct try_block_nodes* actual_try_info = &_actual_try_info.back();
        actual_try_info->nhandlers++;
        
        // Build the handler nodes
        _actual_cfg->_last_nodes = actual_try_info->handler_parents;
        ObjectList<Node*> catch_l = walk(n.get_statement());
        
        actual_try_info->handler_exits.append(catch_l[0]);

        // Set the type of the edge between each handler parent and the actual handler
        Nodecl::NodeclBase label = n.get_name();
        for (ObjectList<Node*>::iterator it = actual_try_info->handler_parents.begin();
            it != actual_try_info->handler_parents.end();
            ++it)
        { 
            Edge* catch_edge = (*it)->get_exit_edge(catch_l[0]);
            if (catch_edge != NULL)
            {
                catch_edge->set_data(_EDGE_TYPE, CATCH_EDGE);
                catch_edge->set_data(_EDGE_LABEL, ObjectList<Nodecl::NodeclBase>(1, label));
            }
            
        }
        
        // FIXME If there is no Ellipsis, all statements within Try must be connected to the Exit of the graph
        // TODO We can reduce considerably the number of connections by analysing the kind of every exception
        
        return catch_l;
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Throw& n)
    {
        ObjectList<Node*> right = walk(n.get_rhs());
        Node* throw_node;
        if (right.empty())
        {   // Throw has no expression associated, we create now the throw node
            throw_node = _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
        }
        else
        {   // Some expression has been built. We merge here the node with the whole throw node
            throw_node = merge_nodes(n, right[0], NULL);
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, throw_node);
        }

        if (!_actual_try_info.empty())
        {   
            for (ObjectList<struct try_block_nodes>::reverse_iterator it = _actual_try_info.rbegin();
                it != _actual_try_info.rend();
                ++it)
            {
                (*it).handler_parents.append(throw_node);
            }
        }
        // Throw must be connected to the Graph exit as well
        _actual_cfg->connect_nodes(throw_node, _actual_cfg->_graph->get_data<Node*>(_EXIT_NODE));
        
        _actual_cfg->_last_nodes.clear();
        return ObjectList<Node*>(1, throw_node);
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::CompoundStatement& n)
    {
        return walk(n.get_statements());
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Symbol& n)
    {
        Node* basic_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        return ObjectList<Node*>(1, basic_node);
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ExpressionStatement& n)
    {
        std::cerr << "Expression '" << c_cxx_codegen_to_str(n.get_internal_nodecl()) << "'" << std::endl;
        
        ObjectList<Node*> expr_last_nodes = _actual_cfg->_last_nodes;
        ObjectList<Node*> expression_nodes = walk(n.get_nest());
        
        if (expression_nodes.size() >= 1)
        {
            // When the size of expression_nodes list is >1, the expression contained contains a comma operator.
            // Otherwise, the expression is any other kind of expression
            Node* last_node;
            if (expression_nodes.size() == 1)
            {
                last_node = expression_nodes[0];
            }
            else
            {   // expression_nodes.size() > 1
                last_node = merge_nodes(n, expression_nodes);
            }
            
            if (!last_node->is_empty_node())
            {
                // Connect the partial node created recursively with the piece of Graph build until this moment
                ObjectList<Node*> expr_first_nodes = get_first_nodes(last_node);
                for (ObjectList<Node*>::iterator it = expr_first_nodes.begin();
                    it != expr_first_nodes.end();
                    ++it)
                {
                    _actual_cfg->clear_visits(*it);
                }
                if (!expr_last_nodes.empty())
                {   // This will be empty when last statement visited was a Break Statement
                    int n_connects = (expr_first_nodes.size() > expr_last_nodes.size()) ? expr_first_nodes.size() : expr_last_nodes.size();
                    if (n_connects != 0)
                    {
                        _actual_cfg->connect_nodes(expr_last_nodes, expr_first_nodes, 
                                                ObjectList<Edge_type>(n_connects, ALWAYS_EDGE), ObjectList<std::string>(n_connects, ""));
                    }
                }
                
                // Recompute actual last nodes for the actual graph
                _actual_cfg->_last_nodes.clear();
                _actual_cfg->_last_nodes.append(last_node);
            }
            else
            {   // do nothing; this case appears when the expression is "new"
                // In this case we don't need adding this statement to the graph because it is meaningless
            }
        }
        else
        {
            internal_error("Parsing the expression '%s' 0 nodes has been returned, and they must be one or more\n", 
                           c_cxx_codegen_to_str(n.get_internal_nodecl()));
        }
        return expression_nodes;
    }

    // TODO for Fortran codes
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ParenthesizedExpression& n)
    {
        std::cerr << "TODO: Parenthesized expression: '" << c_cxx_codegen_to_str(n.get_internal_nodecl()) << "'" << std::endl;
        walk(n.get_nest());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ObjectInit& n)
    {
        if (_actual_cfg == NULL)
        {   // do nothing
            // We arrive here when a shared variable is declared. This point is not reached from the FunctionCode nodecl
            // but from the TopLevel nodecl.
            return Ret();
        }
        else
        {
            internal_error("The ObjectInit '%s' cannot be founded once the graph is already built.",
                           c_cxx_codegen_to_str(n.get_internal_nodecl()));            
        }
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        ObjectList<Node*> subscripted = walk(n.get_subscripted());

        ObjectList<Node*> subscripts_last_nodes = _actual_cfg->_last_nodes;
        _actual_cfg->_last_nodes.clear();
        ObjectList<Node*> subscripts = walk(n.get_subscripts());
        // Connect the partial node created recursively with the piece of Graph build until this moment
        ObjectList<Node*> subscripts_first_nodes = get_first_nodes(subscripts[0]);
        for (ObjectList<Node*>::iterator it = subscripts_first_nodes.begin();
            it != subscripts_first_nodes.end();
            ++it)
        {
            _actual_cfg->clear_visits(*it);
        }
        if (!subscripts_last_nodes.empty())
        {   // This will be empty when last statement visited was a Break Statement
            int n_connects = (subscripts_first_nodes.size() > subscripts_last_nodes.size()) ? 
                             subscripts_first_nodes.size() : subscripts_last_nodes.size();
            if (n_connects != 0)
            {
                _actual_cfg->connect_nodes(subscripts_last_nodes, subscripts_first_nodes, 
                                        ObjectList<Edge_type>(n_connects, ALWAYS_EDGE), ObjectList<std::string>(n_connects, ""));
            }
        }
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(subscripts);
        
        Node* merged = merge_nodes(n, subscripted[0], subscripts[0]);
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(merged);

        return ObjectList<Node*>(1, merged);
    }

    // TODO
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        walk(n.get_lhs());
        walk(n.get_member());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::New& n)
    {
        Node* empty_node = new Node();
        return ObjectList<Node*>(1, empty_node);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Delete& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::DeleteArray& n)
    {
        Node* right = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, right, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Sizeof& n)
    {
        Node* size_type = walk(n.get_size_type())[0];
        return (ObjectList<Node*>(1, merge_nodes(n, size_type, NULL)));
    }

    //TODO Test this kind of node
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Type& n)
    {
        Node* basic_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        return ObjectList<Node*>(1, basic_node);
    }

    // TODO
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Typeid& n)
    {
        ObjectList<Node*> arg = walk(n.get_arg());
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Cast& n)
    {
        Node* cast_expr = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, cast_expr, NULL));
    }

    // TODO
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Offset& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    // TODO Which kind of node is this?
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        // Create the new Function Call node and built it
        Node* func_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), Nodecl::NodeclBase::null(), "function_call");
//         _actual_cfg->_outer_node.push(func_node);
//         
//         ObjectList<Node*> arguments_l = walk(n.get_arguments());
//         // walk(n.get_called());    // This is not necessary, always return the called function
//         Node* func_node_node = merge_nodes(n, arguments_l);
//         
//         _actual_cfg->_outer_node.pop();
//         
//         _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, func_node);
//         _actual_cfg->_last_nodes.clear();
//         _actual_cfg->_last_nodes.append(func_node);
//         
        return ObjectList<Node*>(1, func_node);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FunctionCall& n)
    {
        // Create the new Function Call node and built it
        Node* func_graph_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), Nodecl::NodeclBase::null(), "function_call");
        if (!_actual_cfg->_last_nodes.empty())
        {   // If there is any node in 'last_nodes' list, then we have to connect the new graph node
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, func_graph_node);
            _actual_cfg->_last_nodes.clear();
        }
        _actual_cfg->_last_nodes.append(func_graph_node->get_data<Node*>(_ENTRY_NODE));
       
        ObjectList<Node*> arguments_l = walk(n.get_arguments());
        // walk(n.get_called());    // This is not necessary, always return the called function
        Node* func_node = merge_nodes(n, arguments_l);

        ObjectList<Node*> func_first_nodes = get_first_nodes(func_node);
        for (ObjectList<Node*>::iterator it = func_first_nodes.begin();
            it != func_first_nodes.end();
            ++it)
        {
            _actual_cfg->clear_visits(*it);
        }
        int n_connections = func_first_nodes.size();
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, func_first_nodes, 
                                   ObjectList<Edge_type>(n_connections, ALWAYS_EDGE), ObjectList<std::string>(n_connections, ""));

        Node* graph_exit = func_graph_node->get_data<Node*>(_EXIT_NODE);
        graph_exit->set_id(++_actual_cfg->_nid);
        _actual_cfg->connect_nodes(func_node, graph_exit);
       
        _actual_cfg->_outer_node.pop();
        _actual_cfg->_last_nodes.clear();
        _actual_cfg->_last_nodes.append(func_graph_node);
       
        return ObjectList<Node*>(1, func_graph_node);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Comma& n)
    {
        ObjectList<Node*> comma_nodes;
        comma_nodes.append(walk(n.get_rhs()));
        comma_nodes.append(walk(n.get_lhs()));
        
        return comma_nodes;
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
        ObjectList<Node*> returned_value = walk(n.get_value());
        Node* return_node = merge_nodes(n, returned_value);
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes[0], return_node);
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(return_node);
        return ObjectList<Node*>(1, return_node);
    }


    // ************* Built-in ************* //
    // TODO    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BuiltinExpr& n)
    {
        walk(n.get_components());
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    // TODO
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BuiltinDecl& n)
    {
        walk(n.get_components());
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }


    // ************* Pragmas ************* //
    // TODO
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomDirective& n)
    {
        walk(n.get_pragma_line());
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomConstruct& n)
    {
        // Built a new object in the pragma stack to store its relative info
        // struct omp_pragma actual_omp_pragma;
        // _omp_pragma_info_s.push(actual_omp_pragma);
        
        // // Node* pragma_graph_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), n.get_pragma_line(), "omp_pragma");
        // if (!_actual_cfg->_last_nodes.empty())
        // {   // If there is any node in 'last_nodes' list, then we have to connect the new graph node
        //     _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, pragma_graph_node);
        //     _actual_cfg->_last_nodes.clear();
        // }
        // 
        // pragma_graph_node->set_data(_NODE_LABEL, n.get_pragma_line());
        // 
        // _actual_cfg->_last_nodes.append(func_graph_node->get_data<Node*>(_ENTRY_NODE));
        // ObjectList<Node*> pragma_statement = walk(n.get_statement());
       
        // Node* graph_exit = pragma_graph_node->get_data<Node*>(_EXIT_NODE);
        // graph_exit->set_id(++_actual_cfg->_nid);
        // _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, graph_exit);
       
        // _actual_cfg->_outer_node.pop();
        // _actual_cfg->_last_nodes.clear();
        // _actual_cfg->_last_nodes.append(pragma_graph_node);
       
        // return ObjectList<Node*>(1, pragma_graph_node);
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomClause& n)
    {
        std::cerr << "Pragma Custom Clause: " << c_cxx_codegen_to_str(n.get_internal_nodecl()) << std::endl;
        // struct omp_clause actual_omp_clause = {n, n.get_arguments()};
        // _omp_pragma_info_s.top().clauses = actual_omp_clause;
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomLine& n)
    {
        // std::cerr << "Pragma Custom Line: " << c_cxx_codegen_to_str(n.get_internal_nodecl()) << std::endl;
        // _omp_pragma_info_s.top().params = n.get_parameters();
        // walk(n.get_clauses());  // This method do not return anything
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaClauseArg& n)
    {   
        // nothing to do
        std::cerr << "Pragma Clause Arg: " << c_cxx_codegen_to_str(n.get_internal_nodecl()) << std::endl;
        return Ret();
    }
    
    // ************* Control Flow constructs ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ForStatement& n)
    {
        // We don't need to catch the result of this visit because it is stored in the struct '_actual_loop_info'
        walk(n.get_loop_header());
        
        // We need to create a temporal node because of the possibility of having break statements inside the loop
        Node* exit_node = new Node();
       
        // Create the nodes from the list of inner statements of the loop
        _actual_cfg->_continue_stack.push(_actual_loop_info.next);
        _actual_cfg->_break_stack.push(exit_node);
        walk(n.get_statement());    // This list of nodes returned here will never be used
        _actual_cfg->_continue_stack.pop();
        _actual_cfg->_break_stack.pop();
        
        // Compute the true edge from the loop condition
        Edge_type aux_etype = ALWAYS_EDGE;
        if (!_actual_loop_info.cond->get_exit_edges().empty())
        {
            _actual_loop_info.cond->get_exit_edges()[0]->set_data<Edge_type>(_EDGE_TYPE, TRUE_EDGE);
        }
        else
        { // It will be empty when the loop's body is empty.
            aux_etype = TRUE_EDGE;
        }        
        
        exit_node->set_id(++_actual_cfg->_nid);
        exit_node->set_data(_OUTER_NODE, _actual_cfg->_outer_node.top());
        _actual_cfg->connect_nodes(_actual_loop_info.cond, exit_node, FALSE_EDGE);
        
        // Fill the empty fields of the Increment node
        if (_actual_loop_info.next != NULL)
        {
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_loop_info.next, aux_etype);
            _actual_cfg->connect_nodes(_actual_loop_info.next, _actual_loop_info.cond);
        }
        else
        {
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_loop_info.cond);
        }
       
        
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(exit_node);
        if (_actual_loop_info.init != NULL)
            return ObjectList<Node*>(1, _actual_loop_info.init);
        else
            return ObjectList<Node*>(1, _actual_loop_info.cond);
    }        

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LoopControl& n)
    {
        // Create initializing node
        
        ObjectList<Node*> init_last_nodes = _actual_cfg->_last_nodes;
        ObjectList<Node*> init_node_l = walk(n.get_init());
        if (init_node_l.empty())
        {   // The empty statement will return anything here. No node needs to be created
            _actual_loop_info.init = NULL;
        }
        else
        {
            _actual_loop_info.init = init_node_l[0];
            _actual_cfg->connect_nodes(init_last_nodes, _actual_loop_info.init);
            _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(_actual_loop_info.init);
        }
        
        // Create condition node
        ObjectList<Node*> cond_last_nodes = _actual_cfg->_last_nodes;
        ObjectList<Node*> cond_node_l = walk(n.get_cond());
        if (cond_node_l.empty())
        {   // The condition is an empty statement. 
            // In any case, we build here a node for easiness
            _actual_loop_info.cond = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n.get_cond());
        }
        else
        {
            _actual_loop_info.cond = cond_node_l[0];
        }
        _actual_cfg->connect_nodes(cond_last_nodes, _actual_loop_info.cond);
        
        // Create next node
        _actual_cfg->_last_nodes.clear();
        ObjectList<Node*> next_node_l = walk(n.get_next());
        if (next_node_l.empty())
        {
            _actual_loop_info.next = NULL;
        }
        else
        {
            _actual_loop_info.next = next_node_l[0];
        }
            
        // Recompute actual last nodes
        _actual_cfg->_last_nodes.clear();
        _actual_cfg->_last_nodes.append(_actual_loop_info.cond);
        
        return Ret();   // No return required here. The struct '_actual_loop_info' contains all information for loop nodes construction.
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::WhileStatement& n)
    {
        // Build condition node
        ObjectList<Node*> cond_last_nodes = _actual_cfg->_last_nodes;
        ObjectList<Node*> cond_node_l = walk(n.get_condition());
        Node* cond_node = cond_node_l[0];
        _actual_cfg->connect_nodes(cond_last_nodes, cond_node);
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(cond_node);
        
        Node* exit_node = new Node();
        
        // Build the while body node/s
        _actual_cfg->_continue_stack.push(cond_node);
        _actual_cfg->_break_stack.push(exit_node);
        walk(n.get_statement());    // This list of nodes returned here will never be used
        _actual_cfg->_continue_stack.pop();
        _actual_cfg->_break_stack.pop();
        cond_node->get_exit_edges()[0]->set_data<Edge_type>(_EDGE_TYPE, TRUE_EDGE);
        _actual_cfg->connect_nodes(cond_node, exit_node, FALSE_EDGE);
        
        // Build the exit node
        exit_node->set_id(++_actual_cfg->_nid);
        exit_node->set_data(_OUTER_NODE, _actual_cfg->_outer_node.top());
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, cond_node);
        
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(exit_node);
        return cond_node_l;
    }     

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::DoStatement& n)
    {
        ObjectList<Node*> do_parents = _actual_cfg->_last_nodes;
        
        Node* exit_node = new Node();
        Node* aux_condition_node = new Node();
        _actual_cfg->_continue_stack.push(aux_condition_node);
        _actual_cfg->_break_stack.push(exit_node);
        ObjectList<Node*> stmts = walk(n.get_statement());
        _actual_cfg->_continue_stack.pop();
        _actual_cfg->_break_stack.pop();
        if (!stmts.empty())
        {   // There is something within the Do Statement
            _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(stmts.back());
        }
       
        Node* condition_node = walk(n.get_condition())[0];
        if (aux_condition_node->is_connected())
        {
            int n_connects = aux_condition_node->get_parents().size();
            _actual_cfg->connect_nodes(aux_condition_node->get_parents(), condition_node, ALWAYS_EDGE, "");
            _actual_cfg->connect_nodes(condition_node, aux_condition_node->get_children(), 
                                       ObjectList<Edge_type>(n_connects, ALWAYS_EDGE), ObjectList<std::string>(n_connects, ""));
        }
       
        _actual_cfg->connect_nodes(stmts.back(), condition_node);
        if (!stmts.empty())
        {    
            _actual_cfg->connect_nodes(condition_node, stmts[0], TRUE_EDGE);
        }
        
        // Connect the False condition side to a provisional node
        exit_node->set_id(++_actual_cfg->_nid);
        exit_node->set_data(_OUTER_NODE, _actual_cfg->_outer_node.top());
        _actual_cfg->connect_nodes(condition_node, exit_node, FALSE_EDGE);
        
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(exit_node);
        
        if (!stmts.empty())
            return ObjectList<Node*>(1, stmts[0]);
        else
            return ObjectList<Node*>(1, condition_node);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::IfElseStatement& n)
    {
        Node* exit_node = new Node();
       
        // Compose the condition node
        ObjectList<Node*> cond_last_nodes = _actual_cfg->_last_nodes;
        ObjectList<Node*> cond_node_l = walk(n.get_condition());
        _actual_cfg->connect_nodes(cond_last_nodes, cond_node_l[0]);
        _actual_cfg->_last_nodes.clear();
        _actual_cfg->_last_nodes.append(cond_node_l[0]);
            
        // Compose the then node
        ObjectList<Node*> then_node_l = walk(n.get_then());
        cond_node_l[0]->get_exit_edges()[0]->set_data<Edge_type>(_EDGE_TYPE, TRUE_EDGE);
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, exit_node);
       
        // Compose the else node, if it exists
        _actual_cfg->_last_nodes.clear();
        _actual_cfg->_last_nodes.append(cond_node_l[0]);
        ObjectList<Node*> else_node_l = walk(n.get_else());
        
        exit_node->set_id(++_actual_cfg->_nid);
        exit_node->set_data(_OUTER_NODE, _actual_cfg->_outer_node.top());        
        if (!else_node_l.empty())
        {   // There exists an else statement
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, exit_node);
        }
        else
        {
            _actual_cfg->connect_nodes(cond_node_l[0], exit_node);
        }

        // Link the If condition with the FALSE statement (else or empty node)
        cond_node_l[0]->get_exit_edges()[1]->set_data(_EDGE_TYPE, FALSE_EDGE);
        
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(exit_node);
        return cond_node_l;
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::SwitchStatement& n)
    {
        // Build condition node
        ObjectList<Node*> cond_last_nodes = _actual_cfg->_last_nodes;
        ObjectList<Node*> cond_node_l = walk(n.get_switch());
        _actual_cfg->connect_nodes(cond_last_nodes, cond_node_l[0]);
       
        // Compose the statements nodes
        _actual_cfg->_last_nodes.clear();
        Node* switch_exit = new Node();
        _switch_cond_s.push(cond_node_l[0]);
        _actual_cfg->_break_stack.push(switch_exit);
        ObjectList<Node*> case_stmts = walk(n.get_statement());
        _actual_cfg->_break_stack.pop();
        
        // Link properly the exit node
        switch_exit->set_id(++_actual_cfg->_nid);
        switch_exit->set_data(_OUTER_NODE, _actual_cfg->_outer_node.top());
        
        // Finish computation of switch exit nodes
        if (cond_node_l[0]->get_exit_edges().empty())
        {   // There is no node node inside the statement
            _actual_cfg->connect_nodes(cond_node_l[0], switch_exit);
        }
        else
        {   // If there is some node in '_last_nodes' we connect it to the exit (Last Case stmt have not a Break stmt)
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, switch_exit);
        }

        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(switch_exit);
        return cond_node_l;
    }    

    template <typename T>
    CfgVisitor::Ret CfgVisitor::visit_Case_or_Default(const T& n)
    {
        // Build case nodes
        ObjectList<Node*> case_stmt_l = walk(n.get_statement());
     
        ObjectList<Nodecl::NodeclBase> label;
        // Set the edge between the Case and the Switch condition
        if (!case_stmt_l.empty())
        {
            Edge* e = _actual_cfg->connect_nodes(_switch_cond_s.top(), case_stmt_l[0], CASE_EDGE);
            if (e != NULL)
            {   // The edge between the nodes did not exist previously
                if (n.template is<Nodecl::CaseStatement>())
                {   // We are visiting a Case Statement
                    Nodecl::CaseStatement case_stmt = n.template as<Nodecl::CaseStatement>();
                    label.append(case_stmt.get_case());
                }
                else if (n.template is<Nodecl::DefaultStatement>())
                {   // We are visiting a Default Statement
                    label.append(Nodecl::NodeclBase::null());
                }
                else
                {
                    internal_error("Unexpected type of Nodecl '%s' found in Case Default visit.",
                                c_cxx_codegen_to_str(n.get_internal_nodecl()));
                }
                e->set_data(_EDGE_LABEL, label);
              
                if (case_stmt_l.back()->get_data<Node_type>(_NODE_TYPE) != BASIC_BREAK_NODE)
                {
                    _actual_cfg->_last_nodes = ObjectList<Node*>(1, case_stmt_l.back());
                }
            }
            else
            {   // if the nodes where already connected, the nodes where already connected, then the edge should hace two labels
                Edge* ee = NULL;
                ObjectList<Edge*> case_entry_edges = case_stmt_l[0]->get_entry_edges();
                for (ObjectList<Edge*>::iterator it = case_entry_edges.begin();
                    it != case_entry_edges.end();
                    ++it)
                {
                    if ((*it)->get_source()->get_id() == _switch_cond_s.top()->get_id())
                    {
                        ee = *it;
                        break;
                    }
                }
                if (ee == NULL)
                    internal_error("No edge found while searching existent edge between '%d' and '%d'", 
                                   _switch_cond_s.top()->get_id(), case_stmt_l[0]->get_id());
                    
                label.append(ee->get_data<nodecl_t>(_EDGE_LABEL));
                if (n.template is<Nodecl::CaseStatement>())
                {   // We are visiting a Case Statement
                    Nodecl::CaseStatement case_stmt = n.template as<Nodecl::CaseStatement>();
                    label.append(case_stmt.get_case());
                }
                else if (n.template is<Nodecl::DefaultStatement>())
                {   // We are visiting a Default Statement
                    label.append(Nodecl::NodeclBase::null());
                }
                else
                {
                    internal_error("Unexpected type of Nodecl '%s' found in Case Default visit.",
                                c_cxx_codegen_to_str(n.get_internal_nodecl()));
                }
                ee->set_data(_EDGE_LABEL, label);
            }
        }
        else
        {   // The case is empty. Nothing to do
        }

        return case_stmt_l;
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::CaseStatement& n)
    {
        return visit_Case_or_Default<Nodecl::CaseStatement>(n);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::DefaultStatement& n)
    {
        return visit_Case_or_Default<Nodecl::DefaultStatement>(n);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::BreakStatement& n)
    {
        Node* break_node = _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n, BASIC_BREAK_NODE);
        _actual_cfg->connect_nodes(break_node, _actual_cfg->_break_stack.top());
        _actual_cfg->_last_nodes.clear();
        return ObjectList<Node*>(1, break_node);
    }      

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ContinueStatement& n)
    {
        Node* continue_node = _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n, BASIC_CONTINUE_NODE);
        _actual_cfg->connect_nodes(continue_node, _actual_cfg->_continue_stack.top());
        _actual_cfg->_last_nodes.clear();
        return ObjectList<Node*>(1, continue_node);
    }

    // TODO
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::GotoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LabeledStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ConditionalExpression& n)
    {
        Node* cond_expr_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), Nodecl::NodeclBase::null(), "conditional_expression");
        Node* entry_node = cond_expr_node->get_data<Node*>(_ENTRY_NODE);
        
        // Build condition node
        Node* condition_node = walk(n.get_condition())[0];
        _actual_cfg->connect_nodes(entry_node, condition_node);
        ObjectList<Node*> exit_parents;
        
        // Build true node
        Node* true_node = walk(n.get_true())[0];
        _actual_cfg->connect_nodes(condition_node, true_node);
        exit_parents.append(true_node);
        
        // Build false node
        Node* false_node = walk(n.get_false())[0];
        _actual_cfg->connect_nodes(condition_node, false_node);        
        exit_parents.append(false_node);
        
        // Set exit graph node info
        Node* graph_exit = cond_expr_node->get_data<Node*>(_EXIT_NODE);
        graph_exit->set_id(++_actual_cfg->_nid);
        _actual_cfg->connect_nodes(exit_parents, graph_exit);
        _actual_cfg->_outer_node.pop();
        
        return ObjectList<Node*>(1, cond_expr_node);
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
    void CfgVisitor::compute_catch_parents(Node* node)
    {
        while (!node->is_visited())
        {
            node->set_visited(true);
            Node_type n_type = node->get_data<Node_type>(_NODE_TYPE);
            if (n_type == GRAPH_NODE)
            {
                compute_catch_parents(node->get_data<Node*>(_ENTRY_NODE));
            }
            else if (n_type == BASIC_EXIT_NODE)
            {
                return;
            }
            else if (n_type != BASIC_ENTRY_NODE && n_type != UNCLASSIFIED_NODE && n_type != BASIC_BREAK_NODE)
            {
                _actual_try_info.back().handler_parents.append(node);
            }
           
            ObjectList<Edge*> exit_edges = node->get_exit_edges();
            for(ObjectList<Edge*>::iterator it = exit_edges.begin();
                it != exit_edges.end();
                it++)
            {
                compute_catch_parents((*it)->get_target());
            }
        }
    }
       
    ObjectList<Node*> CfgVisitor::get_first_nodes(Node* actual_node)
    {
        ObjectList<Edge*> actual_entries = actual_node->get_entry_edges();
        ObjectList<Node*> actual_parents;
        
        if (actual_entries.empty())
        {
            if (actual_node->get_data<Node_type>(_NODE_TYPE) == BASIC_ENTRY_NODE)
            {   // 'actual_node' parent path is already connected with the graph Entry Node
                return ObjectList<Node*>();
            }
            else
            {
                return ObjectList<Node*>(1, actual_node);
            }
        }
        else
        {
            for (ObjectList<Edge*>::iterator it = actual_entries.begin();
                it != actual_entries.end();
                ++it)
            {
                ObjectList<Node*> parents = get_first_nodes((*it)->get_source());
                actual_parents.insert(parents);
            }
        }
        
        return actual_parents;
    }

    Node* CfgVisitor::merge_nodes(Nodecl::NodeclBase n, ObjectList<Node*> nodes_l)
    {
        Node* result;
        ObjectList<Node*>::iterator it, iit;
        
        if (nodes_l.size() > 1)
        {   // There is some node to merge. Otherwise, we only have to create the new node
           
            // Elements in the list 'nodes_l' may have relations between them
            // For example, the statement 'f(b) + g();' will generate:
            // - two graph nodes which will depend one on the other
            // - the 'result' node containing the whole expression
            // This last node must have as parent only the graph node containing 'g'
            // So, before iterate the list to get the parents of the new merging node
            // we are going to purge the list deleting those nodes depending on other nodes in the same list
            ObjectList<Node*> nodes_purged_l;
            bool found;
            bool there_is_graph = false;
            for (it = nodes_l.begin();
                it != nodes_l.end();
                ++it)
            {
                ObjectList<Node*> actual_children = (*it)->get_children();
                found = false;
                for (iit = nodes_l.begin();
                    iit != nodes_l.end();
                    ++iit)
                {
                    if (actual_children.contains(*iit))
                    {
                        found = true;
                        break;
                    }
                }
                if (!found)
                {
                    nodes_purged_l.append(*it);
                }
                if ((*it)->get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
                {
                    there_is_graph = true;
                }
            }
          
            if (there_is_graph)
            {   // If there exists any graph node in the list, 
                // then we must create a new graph node containing all nodes
                result = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), Nodecl::NodeclBase::null(), "split_stmt");
                Node* entry = result->get_data<Node*>(_ENTRY_NODE);
                
                // Get the children for the Entry node. They will be those nodes which do not have its parents
                // among the nodes in the list
                ObjectList<Node*> entry_children;
                for (it = nodes_l.begin();
                    it != nodes_l.end();
                    ++it)
                {
                    ObjectList<Node*> actual_parents = (*it)->get_parents();
                    found = false;
                    for (iit = nodes_l.begin();
                        iit != nodes_l.end();
                        ++iit)
                    {
                        if (actual_parents.contains(*iit))
                        {
                            found = true;
                            break;
                        }
                    }
                    if (!found)
                    {
                        entry_children.append(*it);
                    }
                    // Set to all nodes its new outer node
                    (*it)->set_data(_OUTER_NODE, result);
                }
                int n_connections = entry_children.size();
                _actual_cfg->connect_nodes(entry, entry_children, ObjectList<Edge_type>(n_connections, ALWAYS_EDGE),
                                           ObjectList<std::string>(n_connections, ""));
            }
            
            // Delete unnecessary nodes and create the merging node
            ObjectList<Node*> merged_parents;
            for (it = nodes_purged_l.begin();                
                it != nodes_purged_l.end();
                ++it)
            {
                
                if ((*it)->get_data<Node_type>(_NODE_TYPE) != GRAPH_NODE)
                {
                    ObjectList<Node*> aux = (*it)->get_parents();
                    for (ObjectList<Node*>::iterator iit = aux.begin();
                        iit != aux.end();
                        ++iit)
                    {
                        (*iit)->erase_exit_edge(*it);
                    }
                    delete (*it);
                }
                else
                {
                    merged_parents.append(*it);
                }
            }
            
            if (there_is_graph)
            {
                Node* merged_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
                if (!merged_parents.empty())
                {
                    _actual_cfg->connect_nodes(merged_parents, merged_node);
                }
                Node* graph_exit = result->get_data<Node*>(_EXIT_NODE);
                graph_exit->set_id(++_actual_cfg->_nid);
                _actual_cfg->connect_nodes(merged_node, graph_exit);
                _actual_cfg->_outer_node.pop();
            }
            else
            {
                 result = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
            }
        }
        else
        {
            result = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        }

        return result;        
    }

    Node* CfgVisitor::merge_nodes(Nodecl::NodeclBase n, Node* first, Node* second)
    {
        ObjectList<Node*> previous_nodes;
        
        previous_nodes.append(first);
        if (second != NULL)
        {   // Only second node must be NULL and it will be the case of unary operations
            previous_nodes.append(second);
        }
       
        return merge_nodes(n, previous_nodes);
    }
}
