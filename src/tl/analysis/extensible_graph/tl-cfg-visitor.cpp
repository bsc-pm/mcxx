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
        : _actual_cfg(new ExtensibleGraph(sl, "")), _sl(sl), 
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
    
    void CfgVisitor::unhandled_node(const Nodecl::NodeclBase& n) 
    {
        std::cerr << "UNHANDLED -> " << ast_print_node_type(n.get_kind()) << std::endl;
    }

    void CfgVisitor::visit(const Nodecl::TopLevel& n)
    {
        walk(n.get_top_level());
        
        for (ObjectList<ExtensibleGraph*>::iterator it = _cfgs.begin();
            it != _cfgs.end(); 
            ++it)
        {
//             it->live_variable_analysis();
            (*it)->print_graph_to_dot();
        }
    }

    void CfgVisitor::visit(const Nodecl::FunctionCode& n)
    {
        TL::Symbol s = n.get_symbol();
        std::string func_decl = s.get_type().get_declaration(s.get_scope(), s.get_name());
        std::cerr << "ESTIC A LA FUNCIO -> " << func_decl << std::endl;

        // Create a new graph for the current function
        ExtensibleGraph* cfg = new ExtensibleGraph(_sl, s.get_name());
        
        _actual_cfg = cfg;
        walk(n.get_statements());
        cfg = _actual_cfg;
        
        // Connect the sequential statements, if there are
        if (!_seq_nodecl.empty())
        {
            cfg->append_new_node_to_parent(cfg->_last_nodes, _seq_nodecl);
            _seq_nodecl.clear();
        }
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
        Node* graph_exit = cfg->_graph->get_data<Node*>("exit");
        graph_exit->set_id(++cfg->_nid);
        cfg->connect_nodes(cfg->_last_nodes, graph_exit);
        cfg->connect_nodes(cfg->_unhand_try_excpt_list, graph_exit);
        cfg->connect_nodes(cfg->_throw_node_list, graph_exit);
        
        // Remove the unnecessary nodes and join these ones that are always executed consecutively
        cfg->clear_unnecessary_nodes();
        
        _cfgs.append(cfg);
    }

    void CfgVisitor::visit(const Nodecl::TryBlock& n)
    {
        ObjectList<Node*> try_parents = _actual_cfg->_last_nodes;
        walk(n.get_statement());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        
        std::cerr << "Number of try parents: " << try_parents.size() << std::endl;
        std::cerr << "Number of exit edges in the first parent " << try_parents[0]->get_id()
                  << ": " << try_parents[0]->get_exit_edges().size() << std::endl;
        Node* first_try_node = try_parents[0]->get_exit_edges()[0]->get_target();
        compute_catch_parents(first_try_node);
        _actual_cfg->clear_visits(first_try_node);
       
        walk(n.get_catch_handlers());
        
        // This is the Ellipsis??
        walk(n.get_any());
        
        _actual_try_info.clear();
    }  

    void CfgVisitor::visit(const Nodecl::CatchHandler& n)
    {
        _actual_try_info.nhandlers++;
        
        // Build the handler nodes
        _actual_cfg->_last_nodes = _actual_try_info.catch_parents;
        walk(n.get_statement());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        
        // Set the type of the edge between each handler parent and the actual handler
        for (ObjectList<Node*>::iterator it = _actual_try_info.catch_parents.begin();
            it != _actual_try_info.catch_parents.end();
            it++)
        {   
            std::cerr << "Node " << (*it)->get_id() << " has " << (*it)->get_exit_edges().size() << " exit edges"<< std::endl;
            Edge* catch_edge = (*it)->get_exit_edges()[_actual_try_info.nhandlers];
            catch_edge->set_data("type", CATCH_EDGE);
            std::string label = c_cxx_codegen_to_str(((Nodecl::NodeclBase)n.get_name()).get_internal_nodecl());
            catch_edge->set_data("label", label);
        }
    }

    void CfgVisitor::visit(const Nodecl::Throw& n)
    {
        walk(n.get_rhs());
    }  

    void CfgVisitor::visit(const Nodecl::CompoundStatement& n)
    {
        walk(n.get_statements());
    }

    void CfgVisitor::visit(const Nodecl::AnyList& n)
    {
        walk(n.get_list());
    }

    void CfgVisitor::visit(const Nodecl::Symbol& n)
    {
        // Nothing to be done
//         TL::Symbol s = n.get_symbol();
//             std::cerr << "Symbol at " << n.get_locus() << " is " << s.get_qualified_name() 
//                       << " type: " << s.get_type().get_declaration(Scope(), "") << std::endl;
    }
    
    void CfgVisitor::visit(const Nodecl::ExpressionStatement& n)
    {
        std::cerr << "Expression '" << c_cxx_codegen_to_str(((Nodecl::NodeclBase)n).get_internal_nodecl()) << "'" << std::endl;
        // Figure out if the expression will be broken into different nodes
        BreakingExpressionVisitor visitor(_sl);
        visitor.walk(n.get_nest());
        if (visitor._broken_expression)
        {// The expression will be built within a graph node
            _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
            Node* expression_node;
            switch (visitor._breakage_type)
            {
                case 1: // Function_call
                    expression_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), AST_t(), "function_call");
                    break;
                case 2: // Conditional expression
                    expression_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), AST_t(), "conditional_expression");
                    break;
                case 3: // Function call & Conditional Expression
                    expression_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), AST_t(), "splitted_instruction");
                    break;
                default:
                    internal_error("Breaking type wrongly computed for the expression '%s'", 
                                   c_cxx_codegen_to_str(((Nodecl::NodeclBase)n).get_internal_nodecl()));
            }
            
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, expression_node); 
            _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(expression_node->get_data<Node*>("entry"));
            walk(n.get_nest());
            _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
            Node* exit = expression_node->get_data<Node*>("exit");
            exit->set_id(++_actual_cfg->_nid);
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, exit);
            _actual_cfg->_outer_node.pop();
            _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(expression_node);
        }
        else
        {
            walk(n.get_nest());
            _seq_nodecl.append(n);
        }
    }

    void CfgVisitor::visit(const Nodecl::ParenthesizedExpression& n)
    {
        walk(n.get_nest());
    }

    void CfgVisitor::visit(const Nodecl::ErrExpr& n)
    {
        internal_error("Node '%s' should not arrive here. CFG construction failed.", 
                       ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::ObjectInit& n)
    {
        std::cout << "Object init" << std::endl;
        walk(n.get_init_expr());
        _seq_nodecl.append(n);
    }

    void CfgVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        walk(n.get_subscripted());
        walk(n.get_subscripts());
    }

    void CfgVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        walk(n.get_lhs());
        walk(n.get_member());
    }

    void CfgVisitor::visit(const Nodecl::New& n)
    {
        std::cerr << "warning: node '" << ast_print_node_type(n.get_kind()) << "' not properly implemented!! "
                  << "But the execution continues... " << std::endl;
        walk(n.get_init());
        walk(n.get_placement());
    }

    void CfgVisitor::visit(const Nodecl::Delete& n)
    {
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::DeleteArray& n)
    {
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Sizeof& n)
    {
        walk(n.get_size_type());
    }

    void CfgVisitor::visit(const Nodecl::Type& n)
    { // Nothing to be done
    }

    void CfgVisitor::visit(const Nodecl::Typeid& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Cast& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Offset& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        walk(n.get_arguments());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        walk(n.get_called());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
    }

    void CfgVisitor::visit(const Nodecl::FunctionCall& n)
    {
        // FIXME If this first condition necessary?? Or the arguments should be parsed with a different Visitor??
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        walk(n.get_arguments());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        walk(n.get_called());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
        ObjectList<Nodecl::NodeclBase> nodecls = _actual_cfg->_last_nodes[0]->get_data<ObjectList<Nodecl::NodeclBase> >("statements");
    }

    void CfgVisitor::visit(const Nodecl::Comma& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }


    // ************* Literals ************* //
    void CfgVisitor::visit(const Nodecl::StringLiteral& n)
    { // Nothing to be done
    }
    
    void CfgVisitor::visit(const Nodecl::BooleanLiteral& n)
    { // Nothing to be done
    }

    void CfgVisitor::visit(const Nodecl::IntegerLiteral& n)
    { // Nothing to be done
    }

    void CfgVisitor::visit(const Nodecl::ComplexLiteral& n)
    { // Nothing to be done
    }

    void CfgVisitor::visit(const Nodecl::FloatingLiteral& n)
    { // Nothing to be done
    }

    void CfgVisitor::visit(const Nodecl::StructuredLiteral& n)
    { // Nothing to be done
    }


    // ************* Special Statements ************* //       
    void CfgVisitor::visit(const Nodecl::EmptyStatement& n)
    {
        std::cerr << "Empty Statement founded" << std::endl;
        _seq_nodecl.append(n);
    }
            
    void CfgVisitor::visit(const Nodecl::ReturnStatement& n)
    {
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl);
        _seq_nodecl.clear();
        walk(n.get_value());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
        
    }


    // ************* Built-in ************* //
    void CfgVisitor::visit(const Nodecl::BuiltinExpr& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::BuiltinDecl& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }


    // ************* Pragmas ************* //
    void CfgVisitor::visit(const Nodecl::PragmaCustomDirective& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::PragmaCustomConstruct& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::PragmaCustomClause& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::PragmaCustomLine& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::PragmaClauseArg& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }
    
    // ************* Control Flow constructs ************* //
    void CfgVisitor::visit(const Nodecl::ForStatement& n)
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
    }        

    void CfgVisitor::visit(const Nodecl::LoopControl& n)
    {
        _actual_loop_info.init = get_expression_node(n.get_init());
        _actual_loop_info.cond = get_expression_node(n.get_cond());
        _actual_loop_info.next = get_expression_node(n.get_next(), /* Connect node */false);
    }  

    void CfgVisitor::visit(const Nodecl::WhileStatement& n)
    {
        // Build condition node
        Node* condition_node = get_expression_node(n.get_condition());
     
        Node* empty_exit_node = new Node();
        
        // Build the while body node/s
        _actual_cfg->_continue_stack.push(condition_node);
        _actual_cfg->_break_stack.push(empty_exit_node);
        walk(n.get_statement());
        _actual_cfg->_continue_stack.pop();
        _actual_cfg->_break_stack.pop();
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        condition_node->get_exit_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
        
        // Build the exit node
        empty_exit_node->set_id(++_actual_cfg->_nid);
        empty_exit_node->set_data("outer_graph", _actual_cfg->_outer_node.top());
        _actual_cfg->connect_nodes(condition_node, empty_exit_node, FALSE_EDGE);
        if (_actual_cfg->_continue_stmt)
        {
            _actual_cfg->connect_nodes(_actual_cfg->_continue_stack.top(), empty_exit_node);
            _actual_cfg->_continue_stmt = false; // Reset this value
        }
        else if(_actual_cfg->_break_stmt)
        {
            _actual_cfg->connect_nodes(_actual_cfg->_break_stack.top(), empty_exit_node);
            _actual_cfg->_break_stmt = false; // Reset this value
        }
        else
        {
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, condition_node);
        }

        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(empty_exit_node);
    }     

    void CfgVisitor::visit(const Nodecl::IfElseStatement& n)
    {
        Node* empty_exit_node = new Node();
        
        // Compose the condition node
        Node* condition_node = get_expression_node(n.get_condition());
        
        // Compose the then node
        walk(n.get_then());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        condition_node->get_exit_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
        if (_actual_cfg->_continue_stmt)
        {
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_cfg->_continue_stack.top());
            _actual_cfg->_continue_stmt = false; // Reset this value
        }
        else if (_actual_cfg->_break_stmt)
        {
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_cfg->_break_stack.top());
            _actual_cfg->_break_stmt = false; // Reset this value
        }
        else if (_actual_cfg->_goto_stmt)
        {    
            _actual_cfg->_goto_stmt = false; // Reset this value
        }
        else
        {    
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, empty_exit_node);
        }
       
        // Compose the else node, if it exists
        ObjectList<Node*> then_last_nodes = _actual_cfg->_last_nodes;
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(condition_node);
        walk(n.get_else());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        empty_exit_node->set_id(++_actual_cfg->_nid);
        empty_exit_node->set_data("outer_graph", _actual_cfg->_outer_node.top());        
        if (then_last_nodes != _actual_cfg->_last_nodes)
        {   // There exists an else statement
            if (_actual_cfg->_continue_stmt)
            {
                _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_cfg->_continue_stack.top());
                _actual_cfg->_continue_stmt = false; // Reset this value
            }
            else if (_actual_cfg->_break_stmt)
            {
                _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, _actual_cfg->_break_stack.top());
                _actual_cfg->_break_stmt = false; // Reset this value                
            }
            else if (_actual_cfg->_goto_stmt)
            {    
                _actual_cfg->_goto_stmt = false;
            }
            else
            {   
                _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, empty_exit_node);
            }
        }
        else
        {
            _actual_cfg->connect_nodes(condition_node, empty_exit_node);
        }

        // Link the If condition with the FALSE statement (else or empty node)
        condition_node->get_exit_edges()[1]->set_data("type", FALSE_EDGE);
        
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(empty_exit_node);
    }



    void CfgVisitor::visit(const Nodecl::SwitchStatement& n)
    {
        // Compose the condition node
        _actual_switch_info.cond = get_expression_node(n.get_switch());
       
        // Compose the statements nodes
        walk(n.get_statement());
        
        // Link properly the exit node
        Node* empty_exit_node = new Node();
        empty_exit_node->set_id(++_actual_cfg->_nid);
        empty_exit_node->set_data("outer_graph", _actual_cfg->_outer_node.top());
        
        // Finish computation of switch exit nodes
        if (_actual_switch_info.ncases == -1)
        {
            _actual_cfg->connect_nodes(_actual_switch_info.cond, empty_exit_node);
        }
        else
        {
            if (_actual_cfg->_break_stmt)
            {
                _actual_switch_info.cases_break.append(_actual_cfg->_last_nodes);
                _actual_cfg->_break_stmt = false;
            }
            else
            {   // Avoid the case no statement appears within the switch statement
                _actual_cfg->connect_nodes(_actual_cfg->_last_nodes[0], empty_exit_node);
            }

            _actual_cfg->connect_nodes(_actual_switch_info.cases_break, empty_exit_node);
        }

        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(empty_exit_node);
        _actual_switch_info.clear();
    }      

    void CfgVisitor::visit(const Nodecl::CaseStatement& n)
    {
        // If previous cases were parsed, now we have to decide their exit edges
        if (_actual_switch_info.ncases > -1)
        {    
            if (_actual_cfg->_break_stmt)
            {
                _actual_switch_info.cases_break.append(_actual_cfg->_last_nodes);
                _actual_cfg->_break_stmt = false;
            }
            else
            {
                _actual_switch_info.case_no_break = _actual_cfg->_last_nodes[0];
            }
        }
        
        _actual_switch_info.ncases++;
        
        // Prepare parent nodes list
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(_actual_switch_info.cond);
        if (_actual_switch_info.case_no_break != NULL)
        {
            _actual_cfg->_last_nodes.append(_actual_switch_info.case_no_break);
            _actual_switch_info.case_no_break == NULL;
        }
        
        // Build case nodes
        walk(n.get_statement());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        
        // Set the proper labels to the edge
        int actual_case = _actual_switch_info.ncases;
        Edge* case_edge = _actual_switch_info.cond->get_exit_edges()[actual_case];
        case_edge->set_data("type", CASE_EDGE);
        std::string label = c_cxx_codegen_to_str(((Nodecl::NodeclBase)n.get_case()).get_internal_nodecl());
        case_edge->set_data("label", label);
    }

    void CfgVisitor::visit(const Nodecl::DefaultStatement& n)
    {
        // If previous cases were parsed, now we have to decide their exit edges
        if (_actual_switch_info.ncases > -1)
        {    
            if (_actual_cfg->_break_stmt)
            {
                _actual_switch_info.cases_break.append(_actual_cfg->_last_nodes);
                _actual_cfg->_break_stmt = false;
            }
            else
            {
                _actual_switch_info.case_no_break = _actual_cfg->_last_nodes[0];
            }
        }
        
        _actual_switch_info.ncases++;
        
        // Prepare parent nodes list
        _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(_actual_switch_info.cond);
        if (_actual_switch_info.case_no_break != NULL)
        {
            _actual_cfg->_last_nodes.append(_actual_switch_info.case_no_break);
            _actual_switch_info.case_no_break == NULL;
        }        

        // Build default nodes
        walk(n.get_statement());
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        
        // Set the proper labels to the edge
        int actual_case = _actual_switch_info.ncases;
        Edge* case_edge = _actual_switch_info.cond->get_exit_edges()[actual_case];
        case_edge->set_data("type", CASE_EDGE);
        case_edge->set_data("label", std::string("-1"));
    }

    void CfgVisitor::visit(const Nodecl::BreakStatement& n)
    {
        _actual_cfg->_break_stmt = true;
    }      

    void CfgVisitor::visit(const Nodecl::ContinueStatement& n)
    {
        _actual_cfg->_continue_stmt = true;
    }

    void CfgVisitor::visit(const Nodecl::GotoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }  
    
    void CfgVisitor::visit(const Nodecl::ConditionalExpression& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::LabeledStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::DoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }
    

    // ************* Assignment ************* //
    void CfgVisitor::visit(const Nodecl::Assignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::AddAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::SubAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::DivAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }
    
    void CfgVisitor::visit(const Nodecl::MulAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }
    
    void CfgVisitor::visit(const Nodecl::ModAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::ShrAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }
    
    void CfgVisitor::visit(const Nodecl::ShlAssignment& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }


    // ************* Binary operations ************* //
    void CfgVisitor::visit(const Nodecl::Add& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Minus& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());        
    }

    void CfgVisitor::visit(const Nodecl::Mul& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }    

    void CfgVisitor::visit(const Nodecl::Div& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }        

    void CfgVisitor::visit(const Nodecl::Mod& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }
    
    void CfgVisitor::visit(const Nodecl::Power& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }
    
    void CfgVisitor::visit(const Nodecl::LogicalAnd& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::LogicalOr& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::BitwiseAnd& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::BitwiseOr& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::BitwiseXor& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Concat& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Equal& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Different& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::LowerThan& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());        
    }

    void CfgVisitor::visit(const Nodecl::GreaterThan& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::LowerOrEqualThan& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Shr& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Shl& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }


    // ************* Unary operations ************* //
    void CfgVisitor::visit(const Nodecl::Predecrement& n)
    {
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Postdecrement& n)
    {
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Preincrement& n)
    {
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Postincrement& n)
    {
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::Plus& n)
    {
        walk(n.get_rhs());
    }
    
    void CfgVisitor::visit(const Nodecl::Neg& n)
    {
        walk(n.get_rhs());
    }

    void CfgVisitor::visit(const Nodecl::BitwiseNot& n)
    {
        walk(n.get_rhs());
    }
    
    void CfgVisitor::visit(const Nodecl::LogicalNot& n)
    {
        walk(n.get_rhs());
    }
    
    
    // ************* Addresses ************* //
    void CfgVisitor::visit(const Nodecl::Derreference& n)
    {
//             walk(n.get_rhs());
//             std::cerr << "DERREF -> " << ast_print_node_type(n.get_kind()).get_declaration(Scope(), "") << std::endl;
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Reference& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    
    // ************* Fortran specifics ************* //
    void CfgVisitor::visit(const Nodecl::Text& n)
    {   // Nothing to be done
    }
    
    // What's that??
    void CfgVisitor::visit(const Nodecl::FortranNamedPairSpec& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }    
    
    void CfgVisitor::visit(const Nodecl::FortranWhere& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::FortranWherePair& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::SubscriptTriplet& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::FortranLabelAssignStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::FortranComputedGotoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::FortranAssignedGotoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::FortranIoSpec& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::FieldDesignator& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }    

    void CfgVisitor::visit(const Nodecl::IndexDesignator& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::FortranEquivalence& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::FortranData& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::FortranImpliedDo& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::FortranForall& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }
    
    void CfgVisitor::visit(const Nodecl::FortranArithmeticIfStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }      

    void CfgVisitor::visit(const Nodecl::FortranNullifyStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }   
    
    void CfgVisitor::visit(const Nodecl::FortranIoStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }
    
    void CfgVisitor::visit(const Nodecl::FortranOpenStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::FortranCloseStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    } 

    void CfgVisitor::visit(const Nodecl::FortranReadStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    } 
    
    void CfgVisitor::visit(const Nodecl::FortranWriteStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::FortranPrintStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::FortranStopStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::FortranAllocateStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }
    
    void CfgVisitor::visit(const Nodecl::FortranDeallocateStatement& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", ast_print_node_type(n.get_kind()));
    }


    /* *********************** Special Expression Visitors *********************** */
    BreakingExpressionVisitor::BreakingExpressionVisitor(ScopeLink sl)
        : CfgVisitor(sl), _broken_expression(false), _breakage_type(-1)
    {}

    void BreakingExpressionVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        _broken_expression = true;
        _breakage_type = 1;
    }
    
    void BreakingExpressionVisitor::visit(const Nodecl::FunctionCall& n)
    {
        _broken_expression = true;
        _breakage_type = 1;
    }
    
    void BreakingExpressionVisitor::visit(const Nodecl::ConditionalExpression& n)
    {
        _broken_expression = true;
        _breakage_type = 2;
    }


    /* *********************** Non visiting methods *********************** */
    
    Node* CfgVisitor::get_expression_node(const Nodecl::NodeclBase& n, bool connect_node)
    {
        // Compose the node with the sequential statements store until this moment
        _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl); _seq_nodecl.clear();
        
        // Build the new node
        BreakingExpressionVisitor visitor(_sl);
        visitor.walk(n);
        Node* expression_node;
        if (visitor._broken_expression)
        {// The expression will be built within a graph node
            // When the node must not be connected, we have to preserve @_last_nodes attribute
            ObjectList<Node*> last_nodes = _actual_cfg->_last_nodes;
            
            // Compose the graph node
            switch (visitor._breakage_type)
            {
                case 1: // Function_call
                    expression_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), AST_t(), "function_call");
                    break;
                case 2: // Conditional expression
                    expression_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), AST_t(), "conditional_expression");
                    break;
                case 3: // Function call & Conditional Expression
                    expression_node = _actual_cfg->create_graph_node(_actual_cfg->_outer_node.top(), AST_t(), "splitted_instruction");
                    break;
                default:
                    internal_error("Breaking type wrongly computed for the expression '%s'", 
                                   c_cxx_codegen_to_str(((Nodecl::NodeclBase)n).get_internal_nodecl()));
            }
            if (connect_node) {                
                _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, expression_node);
            }
            
            // Connect the entry node
            _actual_cfg->_last_nodes.clear(); _actual_cfg->_last_nodes.append(expression_node->get_data<Node*>("entry"));
            
            // Create and connect the statements nodes
            walk(n);
            _seq_nodecl.append(n);
            _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, _seq_nodecl);
            _seq_nodecl.clear();
            
            // Create and connect the exit nodes
            Node* exit = expression_node->get_data<Node*>("exit");
            exit->set_id(++_actual_cfg->_nid);
            _actual_cfg->connect_nodes(_actual_cfg->_last_nodes, exit);
           
            _actual_cfg->_outer_node.pop();
            
            // Recalculate @_last_nodes attribute
            _actual_cfg->_last_nodes.clear();
            if (connect_node) { 
                _actual_cfg->_last_nodes.append(expression_node);
            } else {
                _actual_cfg->_last_nodes = last_nodes;
            }             
        }
        else
        {
            if (connect_node)
            {
                _actual_cfg->append_new_node_to_parent(_actual_cfg->_last_nodes, n);
                expression_node = _actual_cfg->_last_nodes[0];
            }
            else
            {
                expression_node = _actual_cfg->create_unconnected_node(n);
            }
        }
        
        return expression_node;
    }
    
    void CfgVisitor::compute_catch_parents(Node* node)
    {
        while (!node->is_visited())
        {
            node->set_visited(true);
            _actual_try_info.catch_parents.append(node);
            ObjectList<Edge*> exit_edges = node->get_exit_edges();
            for(ObjectList<Edge*>::iterator it = exit_edges.begin();
                it != exit_edges.end();
                it++)
            {
                compute_catch_parents((*it)->get_target());
            }
        }
    }
}
