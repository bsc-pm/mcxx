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

#include "cxx-prettyprint.h"
#include "cxx-process.h"

#include "tl-cfg-visitor.hpp"

namespace TL
{
    static bool pragma_is_worksharing(std::string pragma);
    
    CfgVisitor::CfgVisitor(int i)
        : _actual_cfg(NULL), _cfgs(), 
          _context_s(), _loop_info_s(), _actual_try_info(), 
          _pragma_info_s(), _omp_sections_info(), 
          _switch_cond_s()
    {}
    
    CfgVisitor::CfgVisitor(const CfgVisitor& visitor)
    {
        _actual_cfg = visitor._actual_cfg;
        _cfgs = visitor._cfgs;
        _context_s = visitor._context_s;
        _loop_info_s = visitor._loop_info_s;
        _actual_try_info = visitor._actual_try_info;
        _pragma_info_s = visitor._pragma_info_s;
        _omp_sections_info = visitor._omp_sections_info;
        _switch_cond_s = visitor._switch_cond_s;
    }
    
    ObjectList<ExtensibleGraph*> CfgVisitor::get_cfgs() const
    {
        return _cfgs;
    }
    
    void CfgVisitor::set_actual_cfg(ExtensibleGraph* graph)
    {
        _actual_cfg = graph;
    }
    
    void CfgVisitor::build_cfg(RefPtr<Nodecl::NodeclBase> nodecl, std::string graph_name)
    {
        if (nodecl->is<Nodecl::TopLevel>() || nodecl->is<Nodecl::FunctionCode>())
        {   // Each function will built its own CFG
            walk(*nodecl);
        }
        else
        {   // We must built now the CFG for 'nodecl'
            _actual_cfg = new ExtensibleGraph(graph_name);
            ObjectList<Node*> partial_cfg = walk(*nodecl);
            
            // Connect the exit nodes to the Exit node of the master graph
            Node* graph_exit = _actual_cfg->_graph->get_graph_exit_node();
            graph_exit->set_id(++_actual_cfg->_nid);
            
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, graph_exit);
            
            _actual_cfg->dress_up_graph();
        
            _cfgs.append(_actual_cfg);
        }
    }
    
    CfgVisitor::Ret CfgVisitor::unhandled_node(const Nodecl::NodeclBase& n) 
    {
        std::cerr << "Unhandled node while CFG construction '" << codegen_to_str(n.get_internal_nodecl())
                  << "' of type '" << ast_print_node_type(n.get_kind()) << "'" << std::endl;
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Context& n)
    {
        _context_s.push(n);
        ObjectList<Node*> in_context = walk(n.get_in_context());
        _context_s.pop();
        return in_context;
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::TopLevel& n)
    {
        return walk(n.get_top_level());
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FunctionCode& n)
    {
        Symbol s = n.get_symbol();
        std::string func_decl = s.get_type().get_declaration(s.get_scope(), s.get_name());
        DEBUG_CODE()
        {
            std::cerr << "=== CFG Function Visit [" << func_decl << "]===" << std::endl;
        }
        
        std::string nom = s.get_name();
       
        // Create a new graph for the current function
        ExtensibleGraph* actual_cfg = new ExtensibleGraph(s.get_name());
        _actual_cfg = actual_cfg;
        
        _actual_cfg->_function_sym = s;
        ObjectList<Node*> func_stmts = walk(n.get_statements());
        
        // Complete the exit node
        Node* graph_exit = _actual_cfg->_graph->get_graph_exit_node();
        graph_exit->set_id(++_actual_cfg->_nid);    
            
        // Connect the exit nodes to the Exit node of the master graph   
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, graph_exit);
        
//         _actual_cfg->dress_up_graph();
        
        _cfgs.append(_actual_cfg);
        _actual_cfg = NULL;
        
        return ObjectList<Node*>(1, actual_cfg->_graph);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::TryBlock& n)
    {
        struct try_block_nodes_t new_try_block;
        _actual_try_info.append(new_try_block);
        ObjectList<Node*> try_parents = _actual_cfg->_last_nodes;
        ObjectList<Node*> try_stmts = walk(n.get_statement());
        
        Node* first_try_node = try_parents[0]->get_exit_edges()[0]->get_target();
        compute_catch_parents(first_try_node);
        _actual_cfg->clear_visits(first_try_node);
      
        ObjectList<Node*> handlers_l = walk(n.get_catch_handlers());
        
        // Process the ellipsis
        ObjectList<Node*> ellipsis_parents = _actual_cfg->_last_nodes;
        struct try_block_nodes_t* actual_try_info = &_actual_try_info.back();
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
        struct try_block_nodes_t* actual_try_info = &_actual_try_info.back();
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
            for (ObjectList<struct try_block_nodes_t>::reverse_iterator it = _actual_try_info.rbegin();
                it != _actual_try_info.rend();
                ++it)
            {
                (*it).handler_parents.append(throw_node);
            }
        }
        // Throw must be connected to the Graph exit as well
        _actual_cfg->connect_nodes(throw_node, _actual_cfg->_graph->get_graph_exit_node());
        
        _actual_cfg->_last_nodes.clear();
        return ObjectList<Node*>(1, throw_node);
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::CompoundStatement& n)
    {
        ObjectList<Node*> stmts = walk(n.get_statements());

        return stmts;
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Conversion& n)
    {
        return walk(n.get_nest());
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Symbol& n)
    {
        Node* basic_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n);
        return ObjectList<Node*>(1, basic_node);
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ExpressionStatement& n)
    {
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
                    int n_connects = expr_first_nodes.size() * expr_last_nodes.size();
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
                           codegen_to_str(n.get_internal_nodecl()));
        }
        return expression_nodes;
    }

    // TODO for Fortran codes
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ParenthesizedExpression& n)
    {
        std::cerr << "TODO: Parenthesized expression: '" << codegen_to_str(n.get_internal_nodecl()) << "'" << std::endl;
        walk(n.get_nest());
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ObjectInit& n)
    {
        if (_actual_cfg == NULL)
        {   // do nothing: A shared variable is declared
            return Ret();
        }
        else
        {
            std::cerr << "ObjectInit in visit: " << codegen_to_str(n.get_internal_nodecl()) 
                      << " making new symbol" << std::endl;
            ObjectList<Node*> object_init_last_nodes = _actual_cfg->_last_nodes;
            nodecl_t n_sym = nodecl_make_symbol(n.get_symbol().get_internal_symbol(), n.get_filename().c_str(), n.get_line());
            Nodecl::Symbol nodecl_symbol(n_sym);
            ObjectList<Node*> init_sym = walk(nodecl_symbol);
            ObjectList<Node*> init_expr = walk(n.get_symbol().get_initialization());
        
            if (init_expr.empty())
            {   // do nothing: The Object Init is not initialized
                return Ret();
            }
            
            Node* merged_node = merge_nodes(n, init_sym[0], init_expr[0]);
            _actual_cfg->connect_nodes(object_init_last_nodes, merged_node);
            _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(merged_node);
            return ObjectList<Node*>(1, merged_node);
        }
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        ObjectList<Node*> subscripted_last_nodes = _actual_cfg->_last_nodes;
        _actual_cfg->_last_nodes.clear();
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
            int n_connects = subscripts_first_nodes.size() * subscripts_last_nodes.size();
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

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Range& n)
    {
        ObjectList<Node*> lower = walk(n.get_lower());
        ObjectList<Node*> upper = walk(n.get_upper());
        ObjectList<Node*> stride = walk(n.get_stride());

        std::cerr << "FIXME: Range node creation not correct." << std::endl;
        Node* merged_limits = merge_nodes(n, lower[0], upper[0]);
        Node* merged = merge_nodes(n, merged_limits, stride[0]);
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(merged);
       
        return ObjectList<Node*>(1, merged);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        ObjectList<Node*> lhs = walk(n.get_lhs());
        ObjectList<Node*> member = walk(n.get_member());
       
        return ObjectList<Node*>(1, merge_nodes(n, member));
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

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Offsetof& n)
    {
        Node* type = walk(n.get_offset_type())[0];
        Node* designator = walk(n.get_designator())[0];
        return (ObjectList<Node*>(1, merge_nodes(n, type, designator)));        
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
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Cast& n)
    {
        Node* cast_expr = walk(n.get_rhs())[0];
        return ObjectList<Node*>(1, merge_nodes(n, cast_expr, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Alignof& n)
    {
        Node* size_type = walk(n.get_align_type())[0];
        return ObjectList<Node*>(1, merge_nodes(n, size_type, NULL));
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::Offset& n)
    {
        Node* base = walk(n.get_base())[0];
        Node* offset = walk(n.get_offset())[0];
        return ObjectList<Node*>(1, merge_nodes(n, base, offset));
    }

    template <typename T>
    CfgVisitor::Ret CfgVisitor::function_call_visit(const T& n)
    {
        // Create the new Function Call node and built it
        Node* func_graph_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), Nodecl::NodeclBase::null(), FUNC_CALL);
        if (!_actual_cfg->_last_nodes.empty())
        {   // If there is any node in 'last_nodes' list, then we have to connect the new graph node
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, func_graph_node);
            _actual_cfg->_last_nodes.clear();
        }
        _actual_cfg->_last_nodes.append(func_graph_node->get_graph_entry_node());
        
        ObjectList<Node*> arguments_l = walk(n.get_arguments());

        // walk(n.get_called());    // This is not necessary, always return the called function
        Node* func_node;
        if (!arguments_l.empty())
        {
            // Method merge_nodes connects properly the nodes created
            func_node = merge_nodes(n, arguments_l);
        }
        else
        {
            func_node = new Node(_actual_cfg->_nid, BASIC_FUNCTION_CALL_NODE, func_graph_node, n);
        }
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, func_node);

        Node* graph_exit = func_graph_node->get_graph_exit_node();
        graph_exit->set_id(++_actual_cfg->_nid);
        _actual_cfg->connect_nodes(func_node, graph_exit);
       
        _actual_cfg->_outer_node.pop();
        _actual_cfg->_last_nodes.clear();
        _actual_cfg->_last_nodes.append(func_graph_node);
       
        // Add the function node to the list of called functions for later IPA
        _actual_cfg->_function_calls.append(func_node);
        
        return ObjectList<Node*>(1, func_graph_node);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        return function_call_visit(n);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::FunctionCall& n)
    {
        return function_call_visit(n);
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

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::StructuredValue& n)
    {
        ObjectList<Node*> items = walk(n.get_items());
        return ObjectList<Node*>(1, merge_nodes(n, items));
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


    // ************* Pragmas ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomDirective& n)
    {
        std::string pragma_line = n.get_pragma_line().get_text();
        if (pragma_line == "barrier")
        {
            _actual_cfg->create_barrier_node(_actual_cfg->_outer_node.top());
        }
        else
        {    
            internal_error("Unexpected directive '%s' found while building the CFG.", pragma_line.c_str());
        }
        return Ret();
    }

    static bool pragma_is_worksharing(std::string pragma)
    {
        return (pragma == "parallel" || pragma == "for" || pragma == "parallel|for"
                || pragma == "workshare" || pragma == "parallel|workshare"
                || pragma == "sections" || pragma == "parallel|sections"
                || pragma == "single");
    }

    template <typename T>
    CfgVisitor::Ret CfgVisitor::create_task_graph(const T& n)
    {
        ObjectList<Node*> previous_nodes;
        
        struct pragma_t actual_pragma;
        _pragma_info_s.push(actual_pragma);

        Node* task_graph_node;
        if (n.template is<Nodecl::PragmaCustomDeclaration>())
        {   // We must build here a new Extensible Graph
            std::cerr << "Declaration" << std::endl;
            _actual_cfg = new ExtensibleGraph("pragma_" + n.get_symbol().get_name());
            
            Symbol next_sym = n.get_symbol();
            if (next_sym.is_function())
            {
                scope_entry_t* next_sym_ = next_sym.get_internal_symbol();
                Nodecl::FunctionCode func(next_sym_->entity_specs.function_code);
                Nodecl::Context ctx = func.get_statements().as<Nodecl::Context>();
                
                task_graph_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), n.get_pragma_line(), TASK, ctx);
                int n_connects = _actual_cfg->_last_nodes.size();
                _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, task_graph_node, 
                                        ObjectList<Edge_type>(n_connects, TASK_EDGE), ObjectList<std::string>(n_connects, ""));
                _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(task_graph_node->get_graph_entry_node());
                
                walk(n.get_pragma_line());  // This visit computes information associated to the Task node, 
                                            // but do not create any additional node                        
               
                walk(func.get_statements());
            }
            else
            {   // Nothing to do. Variable declarations do not create any graph
                std::cerr << "Next symbol is not a function" << std::endl;
            }
        }
        else
        {   // PragmaCustomStatement
            previous_nodes = _actual_cfg->_last_nodes;
            task_graph_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), n.get_pragma_line(), TASK, 
                                                             _context_s.top());
            int n_connects = _actual_cfg->_last_nodes.size();
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, task_graph_node, 
                                    ObjectList<Edge_type>(n_connects, TASK_EDGE), ObjectList<std::string>(n_connects, ""));
            _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(task_graph_node->get_graph_entry_node());
            
            walk(n.get_pragma_line());  // This visit computes information associated to the Task node, 
                                        // but do not create any additional node        
        
            Nodecl::PragmaCustomStatement n_stmt = n.template as<Nodecl::PragmaCustomStatement>();
            walk(n_stmt.get_statement());
        }
        if (task_graph_node != NULL)
        {
            Node* graph_exit = task_graph_node->get_graph_exit_node();
            graph_exit->set_id(++_actual_cfg->_nid);
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, graph_exit);
            _actual_cfg->_outer_node.pop();
            _actual_cfg->_task_nodes_l.append(task_graph_node);
        }
        
        if (n.template is<Nodecl::PragmaCustomDeclaration>())
        {   // Complete the Extensible Graph created for this task
            Node* graph_entry = _actual_cfg->_graph->get_graph_entry_node();
            Node* graph_exit = _actual_cfg->_graph->get_graph_exit_node();
            graph_exit->set_id(++_actual_cfg->_nid);
            
            _actual_cfg->connect_nodes(graph_entry, graph_exit);
            
//             _actual_cfg->dress_up_graph();
        
            _cfgs.append(_actual_cfg);
        }
        else
        {
            _actual_cfg->_last_nodes = previous_nodes;
        }
        
        
        return ObjectList<Node*>(1, task_graph_node);        
    }
    
    template<typename T>
    CfgVisitor::Ret CfgVisitor::visit_pragma_construct(const T& n)
    {
        // Built a new object in the pragma stack to store its relative info
        struct pragma_t actual_pragma;
        _pragma_info_s.push(actual_pragma);
       
        std::string pragma = n.get_pragma_line().get_text();
        
        if (pragma == "task")
        {
            return create_task_graph(n);
        }
        else
        {
            if (pragma == "section")
            {
                _actual_cfg->_last_nodes = _omp_sections_info.top().section_parents;
            } 
            
            Node* pragma_graph_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), n.get_pragma_line(), OMP_PRAGMA);
            if (!_actual_cfg->_last_nodes.empty())
            {   // If there is any node in 'last_nodes' list, then we have to connect the new graph node
                _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, pragma_graph_node);
                _actual_cfg->_last_nodes.clear();
            }
            _actual_cfg->_last_nodes.append(pragma_graph_node->get_graph_entry_node());
            
            walk(n.get_pragma_line());  // This visit computes information associated to the Pragma node, 
                                        // but do not create any additional node
            
            if (pragma == "parallel" || pragma == "parallel|for" 
                || pragma == "parallel|workshare" || pragma == "parallel|sections"
                || pragma == "critical" || pragma == "atomic" || pragma == "ordered"
                || pragma == "task")
            {
                // We include here a Flush node before the pragma statements
                Node* flush_node = new Node(_actual_cfg->_nid, FLUSH_NODE, pragma_graph_node);
                _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, flush_node);
                _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(flush_node);
            }
            else if (pragma == "workshare" || pragma == "single" || pragma == "master" 
                    || pragma == "for" || pragma == "section")
            {   // Nothing to do before building the pragma inner statements
            }
            else
            {
                internal_error("Unexpected omp pragma construct '%s' while building the CFG", pragma.c_str());
            }
       
            if (pragma == "sections" || pragma == "parallel|sections")
            {   // push a new struct in the stack to store info about entries and exits
                struct omp_pragma_sections_t actual_sections_info(_actual_cfg->_last_nodes);
                _omp_sections_info.push(actual_sections_info);
                
                if (n.template is<Nodecl::PragmaCustomDeclaration>())
                {
                    Symbol next_sym = n.get_symbol();
                    if (next_sym.is_function())
                    {
                        scope_entry_t* next_sym_ = next_sym.get_internal_symbol();
                        // TODO
                        internal_error("Sections pragma '%s' at declaration level. Not implemented yet", 0);
                    }
                    else
                    {   // Nothing to do. Variable declarations do not create any graph
                    }
                }
                else
                {   // PragmaCustomStatement
                    // In a sections pragma custom construct, the first statement may not have a sections pragma
                    // In this case, we should wrap the statement within a Sections Pragma
                    Nodecl::PragmaCustomStatement n_stmt = n.template as<Nodecl::PragmaCustomStatement>();
                    Nodecl::List sections_stmt_list = n_stmt.get_statement().template as<Nodecl::List>(); // This list contains always one
                    Nodecl::CompoundStatement sections_compound_stmt = sections_stmt_list[0].as<Nodecl::CompoundStatement>();
                    Nodecl::List sections_stmts = sections_compound_stmt.get_statements().as<Nodecl::List>();
                    if (!sections_stmts.empty() && !sections_stmts[0].is<Nodecl::PragmaCustomStatement>())
                    {   // FIXME When core phase works, this will not be necessary. The phase will perform this transformation
                        const char* text = "section";
                        const char* filename =  n.get_filename().c_str();
                        int line = n.get_line();
                        Nodecl::List empty_nodecl_list(nodecl_null());              
                        nodecl_t pragma_line = nodecl_make_pragma_custom_line(/* clause args */ empty_nodecl_list.get_internal_nodecl(), 
                                                                                /* clauses */ empty_nodecl_list.get_internal_nodecl(),
                                                                            text, filename, line);
                        Nodecl::CompoundStatement first_section = sections_stmts[0].as<Nodecl::CompoundStatement>();
                        Nodecl::List stmt_seq(first_section.get_statements().get_internal_nodecl());
                        nodecl_t new_pragma_sections = nodecl_make_pragma_custom_statement(pragma_line,
                                                                                        stmt_seq.get_internal_nodecl(), 
                                                                                        text, filename, line);
                        // Walk the first wrapped node
                        walk(new_pragma_sections);
                        
                        // Walk the rest of nodes
                        for(std::vector<Nodecl::NodeclBase>::iterator it = sections_stmts.begin()+1;
                            it != sections_stmts.end();
                            ++it)
                        {
                            walk(*it);
                        }
                    }
                    else
                    {
                        Nodecl::PragmaCustomStatement n_stmt = n.template as<Nodecl::PragmaCustomStatement>();
                        walk(n_stmt.get_statement());    // We will not use the list result of this walk
                    }
                }                
            }
            else
            {
                if (n.template is<Nodecl::PragmaCustomDeclaration>())
                {
                    Symbol next_sym = n.get_symbol();
                    if (next_sym.is_function())
                    {
                        scope_entry_t* next_sym_ = next_sym.get_internal_symbol();
                        // TODO
                        internal_error("Non sections pragma '%s' at declaration level. Not implemented yet", 0);
                    }
                    else
                    {   // Nothing to do. Variable declarations do not create any graph
                    }
                }
                else
                {   // PragmaCustomStatement
                    Nodecl::PragmaCustomStatement n_stmt = n.template as<Nodecl::PragmaCustomStatement>();
                    walk(n_stmt.get_statement());
                }
            }
            
            if (pragma == "section")
            {
                _omp_sections_info.top().sections_exits.append(pragma_graph_node);
            }
            else if (pragma == "sections" || pragma == "parallel|sections")
            {
                _actual_cfg->_last_nodes = _omp_sections_info.top().sections_exits;
                _omp_sections_info.pop();
            }
            
            if (pragma_is_worksharing(pragma))
            {   // We include here a Barrier node after the pragma statement
                if (!_pragma_info_s.top().has_clause("nowait"))
                {
                    _actual_cfg->create_barrier_node(pragma_graph_node);
                }
            }
            
            if (pragma == "parallel" || pragma == "for" || pragma == "parallel|for"
                || pragma == "workshare" || pragma == "parallel|workshare"
                || pragma == "sections" || pragma == "parallel|sections"
                || pragma == "critical" || pragma == "atomic" || pragma == "ordered" || pragma == "single")
            {   // This constructs add a Flush at the end of the construct
                // FIXME Atomic construct implies a list of variables to be flushed
                Node* flush_node = new Node(_actual_cfg->_nid, FLUSH_NODE, pragma_graph_node);
                _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, flush_node);
                _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(flush_node);    
            }       
            
            Node* graph_exit = pragma_graph_node->get_graph_exit_node();
            graph_exit->set_id(++_actual_cfg->_nid);
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, graph_exit);
            _actual_cfg->_outer_node.pop();
            _actual_cfg->_last_nodes.clear();
            _actual_cfg->_last_nodes.append(pragma_graph_node);
        
            return ObjectList<Node*>(1, pragma_graph_node);
        }        
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomStatement& n)
    {
        return visit_pragma_construct(n);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomDeclaration& n)
    {
        return visit_pragma_construct(n);
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomLine& n)
    {
        // Get the empty clause as 'parameters'
        // We create conservatively a 'clause_t'. If no arguments has been found, then we remove it
        struct clause_t actual_clause(n.get_text());
        _pragma_info_s.top().clauses.append(actual_clause);
        walk(n.get_parameters());
        if (_pragma_info_s.top().clauses.back().args.empty())
        {
            _pragma_info_s.top().clauses.erase(_pragma_info_s.top().clauses.end()-1);
        }
        
        // Get the rest of clauses
        walk(n.get_clauses());  // This method fills _pragma_info_s with the clauses of the actual pragma
        
        
        
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaCustomClause& n)
    {
        struct clause_t actual_clause(n.get_text());
        _pragma_info_s.top().clauses.append(actual_clause);
        walk(n.get_arguments());    // This call fills _pragma_info_s with the arguments of the actual clause
        return Ret();
    }

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::PragmaClauseArg& n)
    {
        _pragma_info_s.top().clauses.back().args.append((Nodecl::NodeclBase)n);
        return Ret();
    }
    
    // ************* Control Flow constructs ************* //
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ForStatement& n)
    {
        // Compute the information about the loop control and keep the results in the struct '_actual_loop_info'
        ObjectList<Node*> actual_last_nodes = _actual_cfg->_last_nodes;
        walk(n.get_loop_header());
        _actual_cfg->_last_nodes = actual_last_nodes;
        
        // Connect the init
        if (_loop_info_s.top().init != NULL)
        {
            int n_connects = _actual_cfg->_last_nodes.size();
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _loop_info_s.top().init,
                          ObjectList<Edge_type>(n_connects, ALWAYS_EDGE), ObjectList<std::string>(n_connects, ""));
            _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(_loop_info_s.top().init);
        }
        
        // Create the natural loop graph node
        Node* for_graph_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), n.get_loop_header(), 
                                                              LOOP, Nodecl::NodeclBase::null());
        for_graph_node->set_loop_node_type(FOR);
        int n_connects = _actual_cfg->_last_nodes.size();
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, for_graph_node, 
                                   ObjectList<Edge_type>(n_connects, ALWAYS_EDGE), ObjectList<std::string>(n_connects, ""));
        
        // Connect the conditional node
        Node* entry_node = for_graph_node->get_graph_entry_node();
        _loop_info_s.top().cond->set_outer_node(for_graph_node);
        _actual_cfg->connect_nodes(entry_node, _loop_info_s.top().cond);
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(_loop_info_s.top().cond);
        
        Node* exit_node = for_graph_node->get_graph_exit_node();
       
        // Create the nodes from the list of inner statements of the loop
        _actual_cfg->_continue_stack.push(_loop_info_s.top().next);
        _actual_cfg->_break_stack.push(exit_node);
        walk(n.get_statement());    // This list of nodes returned here will never be used
        _actual_cfg->_continue_stack.pop();
        _actual_cfg->_break_stack.pop();
        
        // Compute the true edge from the loop condition
        Edge_type aux_etype = ALWAYS_EDGE;
        if (!_loop_info_s.top().cond->get_exit_edges().empty())
        {
            _loop_info_s.top().cond->get_exit_edges()[0]->set_data<Edge_type>(_EDGE_TYPE, TRUE_EDGE);
        }
        else
        { // It will be empty when the loop's body is empty.
            aux_etype = TRUE_EDGE;
        }        
        
        exit_node->set_id(++_actual_cfg->_nid);
        _actual_cfg->connect_nodes(_loop_info_s.top().cond, exit_node, FALSE_EDGE);
        
        // Fill the empty fields of the Increment node
        if (_loop_info_s.top().next != NULL)
        {
            _loop_info_s.top().next->set_outer_node(for_graph_node);
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _loop_info_s.top().next, aux_etype);
            _actual_cfg->connect_nodes(_loop_info_s.top().next, _loop_info_s.top().cond);
        }
        else
        {
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _loop_info_s.top().cond);
        }
      
        _actual_cfg->_outer_node.pop();
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(for_graph_node);
       
        if (_loop_info_s.top().init != NULL)
            return ObjectList<Node*>(1, _loop_info_s.top().init);
        else
            return ObjectList<Node*>(1, for_graph_node);
    }        

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LoopControl& n)
    {
        struct loop_control_nodes_t actual_loop_info;
       
        // Create initializing node
        _actual_cfg->_last_nodes.clear();
        ObjectList<Node*> init_node_l = walk(n.get_init());
        if (init_node_l.empty())
        {   // The empty statement will return anything here. No node needs to be created
            actual_loop_info.init = NULL;
        }
        else
        {
            actual_loop_info.init = init_node_l[0];
        }
        
        // Create condition node
        _actual_cfg->_last_nodes.clear();
        ObjectList<Node*> cond_node_l = walk(n.get_cond());
        if (cond_node_l.empty())
        {   // The condition is an empty statement. 
            // In any case, we build here a node for easiness
            actual_loop_info.cond = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, _actual_cfg->_outer_node.top(), n.get_cond());
        }
        else
        {
            actual_loop_info.cond = cond_node_l[0];
        }
        
        // Create next node
        _actual_cfg->_last_nodes.clear();
        ObjectList<Node*> next_node_l = walk(n.get_next());
        if (next_node_l.empty())
        {
            actual_loop_info.next = NULL;
        }
        else
        {
            actual_loop_info.next = next_node_l[0];
        }
        
        _loop_info_s.push(actual_loop_info);
        
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
        exit_node->set_outer_node(_actual_cfg->_outer_node.top());
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
        exit_node->set_outer_node(_actual_cfg->_outer_node.top());
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
        exit_node->set_outer_node(_actual_cfg->_outer_node.top());     
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
        switch_exit->set_outer_node(_actual_cfg->_outer_node.top());
        
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
                                codegen_to_str(n.get_internal_nodecl()));
                }
                e->set_data(_EDGE_LABEL, label);
              
                if (case_stmt_l.back()->get_type() != BASIC_BREAK_NODE)
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
                                codegen_to_str(n.get_internal_nodecl()));
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

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::GotoStatement& n)
    {
        Node* goto_node = _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n, BASIC_GOTO_NODE);
        goto_node->set_label(n.get_symbol());
        _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, goto_node);
        
        for (ObjectList<Node*>::iterator it = _actual_cfg->_labeled_node_l.begin();
            it != _actual_cfg->_labeled_node_l.end();
            ++it)
        {
            if ((*it)->get_label() == n.get_symbol())
            {   // Connect the nodes
                _actual_cfg->connect_nodes(goto_node, *it, GOTO_EDGE, n.get_symbol().get_name());
                break;
            }
        }
        
        _actual_cfg->_last_nodes.clear();
       
        return ObjectList<Node*>(1, goto_node);
    }  

    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::LabeledStatement& n)
    {
        Node* labeled_node = walk(n.get_statement())[0];
        labeled_node->set_data(_NODE_TYPE, BASIC_LABELED_NODE);
        labeled_node->set_label(n.get_symbol());
        
        for (ObjectList<Node*>::iterator it = _actual_cfg->_goto_node_l.begin();
            it != _actual_cfg->_goto_node_l.end();
            ++it)
        {
            if ((*it)->get_label() == n.get_symbol())
            {   // Connect the nodes
                _actual_cfg->connect_nodes(*it, labeled_node, GOTO_EDGE, n.get_symbol().get_name());
                break;
            }
        }
        
        _actual_cfg->_labeled_node_l.append(labeled_node);
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(labeled_node);
        
        return ObjectList<Node*>(1, labeled_node);
    }
    
    CfgVisitor::Ret CfgVisitor::visit(const Nodecl::ConditionalExpression& n)
    {
        Node* cond_expr_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), 
                                                              Nodecl::NodeclBase::null(), COND_EXPR);
        Node* entry_node = cond_expr_node->get_graph_entry_node();
        
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
        Node* graph_exit = cond_expr_node->get_graph_exit_node();
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

    // CfgVisitor::Ret CfgVisitor::visit(const Nodecl::SubscriptTriplet& n)
    // {
    //     internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    //     return Ret();
    // }   

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
            Node_type n_type = node->get_type();
            if (n_type == GRAPH_NODE)
            {
                compute_catch_parents(node->get_graph_entry_node());
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
            if (actual_node->get_type() == BASIC_ENTRY_NODE)
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

    /*! Elements in the list 'nodes_l' may have relations between them
     * For example, the statement 'f(b) + g();' will generate:
     * - two graph nodes which will depend one on the other
     * - the 'result' node containing the whole expression
     * This last node must have as parent only the graph node containing 'g'
     * So, before iterate the list to get the parents of the new merging node
     * we are going to purge the list deleting those nodes depending on other nodes in the same list
     */
    Node* CfgVisitor::merge_nodes(Nodecl::NodeclBase n, ObjectList<Node*> nodes_l)
    {
        Node* result;
     
        // Compute the type of node for the new merged node
        Node_type ntype;
        if (n.is<Nodecl::FunctionCall>() || n.is<Nodecl::VirtualFunctionCall>())
        {    
            ntype = BASIC_FUNCTION_CALL_NODE;
        }
        else if (n.is<Nodecl::LabeledStatement>())
        {
            ntype = BASIC_LABELED_NODE;
        }    
        else
        {    
            ntype = BASIC_NORMAL_NODE;
        }
        
        if (nodes_l.size() > 1)
        {   // There is some node to merge. Otherwise, we only have to create the new node
            
            // Check whether we need to build a graph node
            bool need_graph = false;
            for (ObjectList<Node*>::iterator it = nodes_l.begin(); it != nodes_l.end(); ++it)
            {
                if ((*it)->get_type() == GRAPH_NODE)
                {
                    need_graph = true;
                    break;
                }
            }
            
            if (need_graph)
            {
                bool found;
                
                // Build the new graph
                result = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), Nodecl::NodeclBase::null(), SPLIT_STMT);
                Node* entry = result->get_graph_entry_node();
                
                // Get parents of the new graph node and delete the old connections
                // Parents of the nodes in the list without parents within the list will be parents of the new graph
                // Nodes in the list without parents in the list are disconnected from its parents and connected to the Entry
                ObjectList<Node*> graph_parents;
                ObjectList<int> list_pos_to_erase;
                int i = 0;
                for (ObjectList<Node*>::iterator it = nodes_l.begin(); it != nodes_l.end(); ++it)
                {
                    found = false;
                    ObjectList<Node*> actual_parents = (*it)->get_parents();
                    ObjectList<Node*>::iterator iit;
                    for (iit = nodes_l.begin(); iit != nodes_l.end(); ++iit)
                    {
                        if (actual_parents.contains(*iit))
                        {
                            found = true;
                            break;
                        }
                    }
                    if (!found)
                    {
                        // add node to the list of graph parent
                        graph_parents.append((*it)->get_parents());
                        
                        // disconnect those nodes of its parents
                        ObjectList<Node*> aux = (*it)->get_parents();
                        for (ObjectList<Node*>::iterator iit = aux.begin();
                            iit != aux.end();
                            ++iit)
                        {
                            (*iit)->erase_exit_edge(*it);
                            (*it)->erase_entry_edge(*iit);
                        }
                        // delete the node if it is not of Graph type, otherwise, connect it to the Entry
                        if ((*it)->get_type() != GRAPH_NODE)
                        {
                            list_pos_to_erase.append(i);
                            delete (*it);
                        }
                        else
                        {
                            _actual_cfg->connect_nodes(entry, *it);
                        }
                    }
                    i++;
                }
                if (!graph_parents.empty())
                {
                    int n_connects = graph_parents.size();
                    _actual_cfg->connect_nodes(graph_parents, result, ObjectList<Edge_type>(n_connects, ALWAYS_EDGE), 
                                               ObjectList<std::string>(n_connects, ""));
                }
                
                // Erase those positions in the list that are non-Graph nodes
                for (ObjectList<int>::reverse_iterator it = list_pos_to_erase.rbegin();
                    it != list_pos_to_erase.rend(); ++it)
                {
                    nodes_l.erase(nodes_l.begin() + (*it));
                }
                
                // New merging node is created and connected with the nodes in the list without children within the list
                Node* merged_node = new Node(_actual_cfg->_nid, ntype, result, n);
                ObjectList<Node*> merged_parents;
                for (ObjectList<Node*>::iterator it = nodes_l.begin(); it != nodes_l.end(); ++it)
                {
                    found = false;
                    ObjectList<Node*> actual_children = (*it)->get_children();
                    for (ObjectList<Node*>::iterator iit = nodes_l.begin(); iit != nodes_l.end(); ++iit)
                    {
                        if (actual_children.contains(*iit))
                        {
                            found = true;
                            break;
                        }
                       
                    }
                    if (!found)
                    {
                        merged_parents.append(*it);
                    }
                    
                    // now, all nodes must have the new Graph node as outer node
                    (*it)->set_outer_node(result);
                }
                _actual_cfg->connect_nodes(merged_parents, merged_node);         
                
                // Connect merging node with the exit of the graph
                Node* graph_exit = result->get_graph_exit_node();
                graph_exit->set_id(++_actual_cfg->_nid);
                _actual_cfg->connect_nodes(merged_node, graph_exit);
                _actual_cfg->_outer_node.pop();       
            }
            else
            {
                // Delete the nodes and its connections
                for (ObjectList<Node*>::iterator it = nodes_l.begin(); it != nodes_l.end(); ++it)
                {
                    ObjectList<Node*> aux = (*it)->get_parents();
                    if (!aux.empty())
                    {
                        internal_error("Deleting a non-graph node that have '%d' parents. Those nodes shouldn't have any parent'", 
                                       aux.size());
                    }

                    delete (*it);
                }
            
                // Built the new node
                result = new Node(_actual_cfg->_nid, ntype, _actual_cfg->_outer_node.top(), n); 
            }
        }
        else
        {
            std::cerr << "warning: trying to merge an empty list of nodes. This shouldn't happen'" << std::endl;
            result = new Node(_actual_cfg->_nid, ntype, _actual_cfg->_outer_node.top(), n);
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
