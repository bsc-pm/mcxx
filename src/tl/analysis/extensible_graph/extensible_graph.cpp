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

#include "extensible_graph.hpp"

namespace TL
{   
    ExtensibleGraph::ExtensibleGraph(ScopeLink sl, std::string name)
            : _entry(NULL), _exit(NULL), _sl(sl), _name(name), _nid(0),
              _continue_stack(), _break_stack(),
              _unhand_try_excpt_list(), _labeled_node_list(), _goto_node_list(), _throw_node_list(),
              _continue_stmt(false), _break_stmt(false), _goto_stmt(false)
    {}

    ExtensibleGraph::ExtensibleGraph(const ExtensibleGraph& graph)
    {
        _entry = graph._entry;
        _exit = graph._exit;
        _sl = graph._sl;
        _name = graph._name;
        _nid = graph._nid;
        _continue_stack = graph._continue_stack;
        _break_stack = graph._break_stack;
        _continue_stmt = graph._continue_stmt;
        _unhand_try_excpt_list = graph._unhand_try_excpt_list;
        _labeled_node_list = graph._labeled_node_list; 
        _goto_node_list = graph._goto_node_list;
        _throw_node_list = graph._throw_node_list;
        _goto_stmt = graph._goto_stmt;
    }

    void ExtensibleGraph::build_CFG(Statement stmt)
    {
        _entry = new Node(_nid, BASIC_ENTRY_NODE, NULL);
        
        // Build the rest of the graph from @stmt
        ObjectList<Statement> code_statements;
        if (stmt.is_compound_statement()) {
            code_statements.append(stmt.get_inner_statements());
        } 
        else {
            code_statements.append(stmt);
        }
        Node *graph = build_graph_from_statements(_entry, code_statements);
        
        _exit = append_new_node_to_parent(graph, ObjectList<AST_t>(), NULL, BASIC_EXIT_NODE);
        connect_nodes(_unhand_try_excpt_list, _exit);
        connect_nodes(_throw_node_list, _exit);
        
        // remove the unnecessary nodes and join these ones that are always executed consecutively
        clear_unnecessary_nodes();
    }
    
    Node* ExtensibleGraph::build_graph_from_statement(Node *parent, Statement stmt, Node* outer_graph)
    {
        ObjectList<Statement> stmts;
        
        if (stmt.is_compound_statement())
            stmts.append(stmt.get_inner_statements());
        else
            stmts.append(stmt);
        
        return build_graph_from_statements(parent, stmts, outer_graph);
    }
    
    Node* ExtensibleGraph::build_graph_from_statements(Node *parent, ObjectList<Statement> stmts, Node* outer_graph)
    {
        Node *last_node = parent;

        for (ObjectList<Statement>::iterator it = stmts.begin(); it != stmts.end(); ++it)
        {
            // Treat the statements that, as Statements, they do not break the flow
            if (!it->breaks_flow())
            {
                ObjectList<Statement> sequential_stmts;
                while (it!=stmts.end() && !it->breaks_flow())
                {
                    sequential_stmts.append(*it);
                    ++it;
                }
                last_node = build_node_from_sequential_statements(last_node, sequential_stmts, outer_graph);
            }
            
            // Treat the other statements
            if (it != stmts.end())
            {   
                if (ForStatement::predicate(it->get_ast())) 
                {
                    last_node = build_for_node(last_node, *it, outer_graph);
                }
                else if (WhileStatement::predicate(it->get_ast())) 
                {
                    last_node = build_while_node(last_node, *it, outer_graph);
                }
                else if (DoWhileStatement::predicate(it->get_ast())) 
                {
                    last_node = build_dowhile_node(last_node, *it, outer_graph);
                }
                else if (IfStatement::predicate(it->get_ast())) 
                {
                    last_node = build_if_node(last_node, *it, outer_graph);
                }
                else if (SwitchStatement::predicate(it->get_ast())) 
                {
                    last_node = build_switch_node(last_node, *it, outer_graph);
                }
                else if (TryStatement::predicate(it->get_ast())) 
                {
                    last_node = build_try_node(last_node, *it, outer_graph);
                }
                else if (LabeledStatement::predicate(it->get_ast())) 
                {
                    last_node = build_labeled_statement(last_node, *it, outer_graph);
                }
                else if (GotoStatement::predicate(it->get_ast()))
                {
                    last_node = build_goto_statement(last_node, *it, outer_graph);
                }
                else if (ReturnStatement::predicate(it->get_ast()))
                {
                    last_node = build_return_node(last_node, *it, outer_graph);
                }
                else if (BreakStatement::predicate(it->get_ast()))
                {
                    // This Statement will never be within a Switch, 
                    // In that case we will be inside the function "build_switch_node"
                    _break_stmt = true;
                    
                    // Parse next Statements in a recursive call because the first of them is now orphan
                    // We need to do that because of the possible labels of the following nodes
                    ObjectList<Statement> stmts_after_break;
                    for(it = it+1; it != stmts.end(); it++) 
                    {
                        stmts_after_break.append(*it);
                    }
                    Node* empty_node = new Node(_nid, BASIC_NORMAL_NODE, outer_graph);
                    build_graph_from_statements(empty_node, stmts_after_break, outer_graph);
                    
                    break; // Now break, because all following nodes have been previously parsed
                }
                else if (ContinueStatement::predicate(it->get_ast()))
                {
                    _continue_stmt = true;
                    
                    // Parse next Statements in a recursive call because the first of them is now orphan
                    // We need to do that because of the possible labels of the following nodes
                    ObjectList<Statement> stmts_after_continue;
                    for(it = it+1; it != stmts.end(); it++) 
                    {
                        stmts_after_continue.append(*it);
                    }
                    Node* empty_node = new Node(_nid, BASIC_NORMAL_NODE, outer_graph);
                    build_graph_from_statements(empty_node, stmts_after_continue, outer_graph);
                    
                    break; // Now break, because all following nodes have been previously parsed
                }
                else if (it->is_pragma_construct())
                {
                    last_node = build_pragma_construct(last_node, *it);
                }
                else if (it->is_pragma_directive())
                {
                    last_node = append_new_node_to_parent(last_node, ObjectList<AST_t>(1, it->get_pragma_line().get_ast()), 
                                                          outer_graph, BASIC_PRAGMA_DIRECTIVE_NODE);
                }
                else
                {
                    internal_error("Unknown statement '%s' while building the CFG\n", it->get_ast().prettyprint().c_str());
                }
            }
            else 
                --it;
        }
        
        return last_node;
    }
    
    Node* ExtensibleGraph::build_node_from_sequential_statements(Node *parent, ObjectList<Statement> stmts, Node* outer_graph)
    {
        Node *last_node = parent;
        
        for(ObjectList<Statement>::iterator it = stmts.begin();
                it != stmts.end();
                ++it)
        {
            if (EmptyStatement::predicate(it->get_ast()))
            { // Do nothing 
            }
            else if (it->is_declaration())
            {
                // TODO When it is an array declaration, we must check the subscripting function. 
                // It is not already done because of the lack of information in the Declared Entities
                last_node = append_new_node_to_parent(last_node, ObjectList<AST_t>(1, it->get_simple_declaration().get_ast()), outer_graph);
            }
            else if (it->is_expression())
            {
                last_node = build_node_from_expression(last_node, it->get_expression(), /*always_create_node*/ true, outer_graph);
            }
            else if (it->is_compound_statement())
            {   // This will happens when a parallel code within brackets is read
                last_node = build_graph_from_statements(last_node, it->get_inner_statements(), outer_graph);
            }
            else
            {
                internal_error("Unexpected statement kind '%s' while building the CFG\n", it->prettyprint().c_str());
            }
        }
        
        return last_node;
    }
    
    Node* ExtensibleGraph::build_node_from_expression(Node* parent, Expression expr, bool always_create_node, Node* outer_graph)
    {
        //         std::cout << "Building expression: " << expr.prettyprint() << std::endl;
        Node* result = NULL;
        
        if (expr.is_literal() || expr.is_id_expression())
        {   // Do nothing
        }
        else if (expr.is_unary_operation())
        {
            Node* unary_operand = build_node_from_expression(parent, expr.get_unary_operand(), /*always_create_node*/ false, outer_graph);
            if (always_create_node)
            {
                Node* new_parent = parent;
                if (unary_operand != NULL)
                {    
                    new_parent = unary_operand;
                }
                result = append_new_node_to_parent(new_parent, ObjectList<AST_t>(1, expr.get_ast()), outer_graph);
            }
        }
        else if (expr.is_binary_operation())
        {
            Node* first_op_node = build_node_from_expression(parent, expr.get_first_operand(), /*always_create_node*/ false, outer_graph);
                                                     
            Node* next_parent = parent;
            if (first_op_node != NULL)
            {    
                next_parent = first_op_node;
            }
            
            Node* second_op_node = build_node_from_expression(next_parent, expr.get_second_operand(), /*always_create_node*/ false, outer_graph);
                                                        
            if (always_create_node)
            {
                if ((first_op_node==NULL) && (second_op_node==NULL))
                {    
                    result = append_new_node_to_parent(next_parent, ObjectList<AST_t>(1, expr.get_ast()), outer_graph);
                }
                else
                {
                    Node* binary_graph_node = create_graph_node(outer_graph);
                    next_parent = binary_graph_node->get_data<Node*>("entry");
                    if (first_op_node != NULL) 
                    {
                        disconnect_nodes(parent, first_op_node);
                        connect_nodes(next_parent, first_op_node);
                        first_op_node->set_data("outer_graph", binary_graph_node);
                        next_parent = first_op_node;
                    }
                    if (second_op_node != NULL) 
                    {
                        disconnect_nodes(second_op_node->get_entry_edges()[0]->get_source(), second_op_node);
                        connect_nodes(next_parent, second_op_node);
                        second_op_node->set_data("outer_graph", binary_graph_node);
                        next_parent = second_op_node;
                    }
                    Node *binary_node = append_new_node_to_parent(next_parent, ObjectList<AST_t>(1, expr.get_ast()), binary_graph_node);
                    
                    Node* exit_node = binary_graph_node->get_data<Node*>("exit");
                    exit_node->set_id(++_nid);
                    connect_nodes(binary_node, exit_node);
                    connect_nodes(parent, binary_graph_node);
                    result = binary_graph_node;
                }
            }
            else 
            {
                if ((first_op_node != NULL) && (second_op_node != NULL))
                {   // When both of the operands return a graph, we must build a graph wrapping them
                    Node *binary_graph_node = create_graph_node(outer_graph);
                    disconnect_nodes(parent, first_op_node);
                    connect_nodes(binary_graph_node->get_data<Node*>("entry"), first_op_node);
                    first_op_node->set_data("outer_graph", binary_graph_node);
                    second_op_node->set_data("outer_graph", binary_graph_node);
                    
                    Node* exit_node = binary_graph_node->get_data<Node*>("exit");
                    exit_node->set_id(++_nid);
                    connect_nodes(second_op_node, exit_node);
                    connect_nodes(parent, binary_graph_node);
                    result = binary_graph_node;
                }
                else if (first_op_node != NULL) 
                {
                    result = first_op_node;
                }
                else if (second_op_node != NULL) 
                {
                    result = second_op_node;
                }
            }
        }
        else if (expr.is_casting())
        {
            Node* casted_expr_node = build_node_from_expression(parent, expr.get_casted_expression(), /*always_create_node*/ false, outer_graph);
                                                        
            if (always_create_node)
            {
                if (casted_expr_node == NULL)
                {    
                    result = append_new_node_to_parent(parent, ObjectList<AST_t>(1, expr.get_ast()), outer_graph);
                }
                else
                {
                    Node *casting_graph_node = create_graph_node(outer_graph);
                    disconnect_nodes(parent, casted_expr_node);
                    connect_nodes(casting_graph_node->get_data<Node*>("entry"), casted_expr_node);
                    casted_expr_node->set_data("outer_graph", casting_graph_node);
                    
                    Node *casting_node = append_new_node_to_parent(casted_expr_node, ObjectList<AST_t>(1, expr.get_ast()), casting_graph_node);
                    Node* exit_node = casting_graph_node->get_data<Node*>("exit");
                    exit_node->set_id(++_nid);
                    connect_nodes(casting_node, exit_node);
                    
                    connect_nodes(parent, casting_graph_node);
                    result = casting_graph_node;
                }
            }
            else if (casted_expr_node != NULL)
            {
                result = casted_expr_node;
            }
        }
        else if (expr.is_array_subscript())
        {
            Node *subscript_node = build_node_from_expression(parent, expr.get_subscript_expression(), /*always_create_node*/ false);                            

            if (always_create_node)
            {
                if (subscript_node == NULL)
                {
                    result = append_new_node_to_parent(parent, ObjectList<AST_t>(1, expr.get_ast()), outer_graph);
                }
                else
                {
                    Node* subscript_graph_node = create_graph_node(outer_graph);
                    disconnect_nodes(parent, subscript_node);
                    connect_nodes(subscript_graph_node->get_data<Node*>("entry"), subscript_node);
                    subscript_node->set_data("outer_graph", subscript_graph_node);
                    
                    Node* array_subscript_node = append_new_node_to_parent(subscript_node, ObjectList<AST_t>(1, expr.get_ast()), subscript_graph_node);
                    Node* exit_node = subscript_graph_node->get_data<Node*>("exit");
                    exit_node->set_id(++_nid);
                    connect_nodes(array_subscript_node, exit_node);
                    
                    connect_nodes(parent, subscript_graph_node);
                    result = subscript_graph_node;
                }    
            }
            else
            {
                if (subscript_node != NULL)
                {
                    result = subscript_node;
                }
            }
        }
        else if (expr.is_array_section_range())
        {
            std::cerr << "TODO Array sections are not properly parsed in the CFG construction" << std::endl;
        }
        else if (expr.is_member_access())
        {
            if (always_create_node)
            {
                result = append_new_node_to_parent(parent, ObjectList<AST_t>(1, expr.get_ast()), outer_graph);
            }
        }
        else if (expr.is_pointer_member_access())
        {
            if (always_create_node)
            {
                result = append_new_node_to_parent(parent, ObjectList<AST_t>(1, expr.get_ast()), outer_graph);
            }
        }
        else if (expr.is_function_call())
        {
            // A function call always creates a CFG
            Node* casting_graph_node = create_graph_node(outer_graph);
            connect_nodes(parent, casting_graph_node);
            
            ObjectList<Expression> args = expr.get_argument_list();
            Node* last_node = casting_graph_node->get_data<Node*>("entry");
            for(ObjectList<Expression>::iterator it = args.begin();
                    it != args.end();
                    it++)
            {
                Node* argument_node = build_node_from_expression(last_node, *it, /*always_create_node*/ true, casting_graph_node);
                if (argument_node != NULL)
                {    
                    last_node = argument_node;
                }
            }
            
            Node* function_call_node = append_new_node_to_parent(last_node, ObjectList<AST_t>(1,expr.get_ast()), casting_graph_node, BASIC_FUNCTION_CALL_NODE);
            
            Node* exit_node = casting_graph_node->get_data<Node*>("exit");
            exit_node->set_id(++_nid);
            connect_nodes(function_call_node, exit_node);
            
            result = casting_graph_node;
        }
        else if (expr.is_conditional())
        {
            Node* conditional_graph_node = create_graph_node(outer_graph);
            Node* entry_node = conditional_graph_node->get_data<Node*>("entry");
            Node* exit_node = conditional_graph_node->get_data<Node*>("exit");            
            
            Node* condition_expression = build_node_from_expression(entry_node, expr.get_condition_expression(), /*always_create_node*/true, conditional_graph_node);
            Node* true_expression = build_node_from_expression(condition_expression, expr.get_true_expression(), /*always_create_node*/true, conditional_graph_node);
            Node* false_expression = build_node_from_expression(condition_expression, expr.get_false_expression(), /*always_create_node*/true, conditional_graph_node);
            // We need here to change the type of the first edge of the 'true/false' graphs to TRUE_EDGE/FALSE_EDGE
            true_expression->get_entry_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
            false_expression->get_entry_edges()[0]->set_data<Edge_type>("type", FALSE_EDGE);
            
            Node* cond_expr_node = append_new_node_to_parent(true_expression, ObjectList<AST_t>(1, expr.get_ast()), conditional_graph_node);
            connect_nodes(false_expression, cond_expr_node);

            exit_node->set_id(++_nid);
            connect_nodes(cond_expr_node, exit_node);
            
            result = conditional_graph_node;
        }
        else if (expr.is_throw_expression())
        {
            Node* throw_expr_node = build_node_from_expression(parent, expr.get_throw_expression(), /*always_create_node*/false, outer_graph);
                                                       
            if (always_create_node)
            {
                if (throw_expr_node == NULL)
                {
                    result = append_new_node_to_parent(parent, ObjectList<AST_t>(1, expr.get_ast()), outer_graph);
                }
                else
                {
                    Node* throw_graph_node = create_graph_node(outer_graph);
                    Node* entry_node = throw_graph_node->get_data<Node*>("entry");
                    Node* exit_node = throw_graph_node->get_data<Node*>("exit");
                    throw_expr_node->set_data("outer_graph", throw_graph_node);
                    
                    disconnect_nodes(parent, throw_expr_node);
                    connect_nodes(parent, throw_graph_node);
                    
                    Node* throw_node = append_new_node_to_parent(entry_node, ObjectList<AST_t>(1, expr.get_ast()), throw_graph_node);
                    
                    exit_node->set_id(++_nid);
                    connect_nodes(throw_node, exit_node);
                    result = throw_graph_node;
                }
            }
            _throw_node_list.append(result);
            
            result = NULL;
        }
        else 
        {
            internal_error("Unknown kind for expression '%s' while building the CFG\n", expr.prettyprint().c_str());
        }
        
        return result;
    }
    
    Node* ExtensibleGraph::build_for_node(Node* parent, Statement for_stmt, Node* outer_graph)
    {
        ForStatement inner_for_statement(for_stmt.get_ast(), _sl);
        
        Node* for_graph_node = append_new_node_to_parent(parent, ObjectList<AST_t>(), outer_graph, GRAPH_NODE);
        Node* last_node = for_graph_node->get_data<Node*>("entry");
        Node* exit_node = for_graph_node->get_data<Node*>("exit");
        
        // First child is an AST_EXPRESSION 'FOR_initial_expression'
        // This statement may be empty (';'); in that case, we mustn't create this node
        if ( !EmptyStatement::predicate(inner_for_statement.get_iterating_init()) )
        {
            Node* init_expr = append_new_node_to_parent(last_node, ObjectList<AST_t>(1, inner_for_statement.get_iterating_init()), for_graph_node);
            last_node = init_expr;
        }
        
        // Second child is an AST_CONDITION 'FOR_condition'
        Node* condition_expr = append_new_node_to_parent(last_node, 
                                                         ObjectList<AST_t>(1, inner_for_statement.get_iterating_condition().get_ast()), 
                                                         for_graph_node);
        
        // Third child is an AST_EXPRESSION 'FOR_increment_expression'
        Node* increment_expr = append_new_node_to_parent(NULL, ObjectList<AST_t>(1, inner_for_statement.get_iterating_expression().get_ast()),
                                                         for_graph_node);
        
        // Fourth child is an AST_COMPOUND_STATEMENT 'FOR_loop_body'
        _continue_stack.push(increment_expr);
        _break_stack.push(exit_node);
        Node* for_body = build_graph_from_statement(condition_expr, inner_for_statement.get_loop_body(), for_graph_node);
        condition_expr->get_exit_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
        _continue_stack.pop();
        _break_stack.pop();
        
        exit_node->set_id(++_nid);
        connect_nodes(condition_expr, exit_node, FALSE_EDGE);
        
        // Fill the empty fields of the Increment node
        if (!_break_stmt)
        {
            connect_nodes(for_body, increment_expr);
            connect_nodes(increment_expr, condition_expr);
        }
        else
        {
            connect_nodes(for_body, exit_node);
            _break_stmt = false; // Reset this value
        }
        
        _continue_stmt = false; // Conservatively, reset this value
        
        return for_graph_node;
    }
    
    Node* ExtensibleGraph::build_while_node(Node* parent, Statement while_stmt, Node* outer_graph)
    {
        WhileStatement inner_while_statement(while_stmt.get_ast(), _sl);
        
        Node* while_graph_node = append_new_node_to_parent(parent, ObjectList<AST_t>(), outer_graph, GRAPH_NODE);
        Node* entry_node = while_graph_node->get_data<Node*>("entry");
        Node* exit_node = while_graph_node->get_data<Node*>("exit");
        
        // First child is an AST_CONDITION 'WHILE_condition'
        AST_t cond = inner_while_statement.get_condition().get_expression().get_ast();
        Node* condition_expr = append_new_node_to_parent(entry_node, ObjectList<AST_t>(1, cond), while_graph_node);
        
        // Second child is an AST_COMPOUND_STATEMENT "WHILE_loop_body"
        _continue_stack.push(condition_expr);
        _break_stack.push(exit_node);
        Node* while_body = build_graph_from_statement(condition_expr, inner_while_statement.get_body(), while_graph_node);
        condition_expr->get_exit_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
        _continue_stack.pop();
        _break_stack.pop();
        
        exit_node->set_id(++_nid);
        connect_nodes(condition_expr, exit_node, FALSE_EDGE);
        // Fill the empty fields of the _exit node
        if (_continue_stmt)
        {
            connect_nodes(_continue_stack.top(), exit_node);
            _continue_stmt = false; // Reset this value
        }
        else if(_break_stmt)
        {
            connect_nodes(_break_stack.top(), exit_node);
            _break_stmt = false; // Reset this value
        }
        else
        {
            connect_nodes(while_body, condition_expr);
        }
        
        _continue_stmt = false; // Conservatively, reset this value
        
        return while_graph_node;
    }
    
    Node* ExtensibleGraph::build_dowhile_node(Node* parent, Statement dowhile_stmt, Node* outer_graph)
    {
        DoWhileStatement inner_dowhile_statement(dowhile_stmt.get_ast(), _sl);
            
        Node* dowhile_graph_node = append_new_node_to_parent(parent, ObjectList<AST_t>(), outer_graph, GRAPH_NODE);
        Node* entry_node = dowhile_graph_node->get_data<Node*>("entry");
        Node* exit_node = dowhile_graph_node->get_data<Node*>("exit");
        
        // Second child is a AST_EXPRESSION 'DO_WHILE_condition'
        AST_t cond = inner_dowhile_statement.get_expression().get_ast();
        Node* condition_expr = append_new_node_to_parent(NULL, ObjectList<AST_t>(1, cond), dowhile_graph_node);
        
        // First child is a COMPOUND_STATEMENT 'DO_WHILE_body'
        _continue_stack.push(condition_expr);
        _break_stack.push(exit_node);
        Node* dowhile_body = build_graph_from_statement(entry_node, inner_dowhile_statement.get_body(), dowhile_graph_node);
        connect_nodes(dowhile_body, condition_expr);
        _continue_stack.pop();
        _break_stack.pop();
        
        // Second child is a AST_EXPRESSION 'DO_WHILE_condition'
        connect_nodes(condition_expr, entry_node->get_exit_edges()[0]->get_target(), TRUE_EDGE);
        
        exit_node->set_id(++_nid);
        // Create the special exit node for this subtree
        if (!_break_stmt)
        {
            connect_nodes(condition_expr, exit_node, FALSE_EDGE);
        }
        else
        {
            connect_nodes(dowhile_body, exit_node);
            _break_stmt = false; // Reset this value            
        }
        
        _continue_stmt = false; // Conservatively, reset this value
        
        return dowhile_graph_node;
    }
                
    Node* ExtensibleGraph::build_if_node(Node* parent, Statement if_stmt, Node* outer_graph)
    {
        IfStatement inner_if_statement(if_stmt.get_ast(), _sl);
        
        Node* if_graph_node = append_new_node_to_parent(parent, ObjectList<AST_t>(), outer_graph, GRAPH_NODE);
        Node* entry_node = if_graph_node->get_data<Node*>("entry");
        Node* exit_node = if_graph_node->get_data<Node*>("exit");
        
        // First child is an AST_CONDITION 'IF_condition'
        AST_t if_ast = inner_if_statement.get_condition().get_expression().get_ast();
        Node* condition_expr = append_new_node_to_parent(entry_node, ObjectList<AST_t>(1, if_ast), if_graph_node);
        
        // Second node is a COMPOUND_STATEMENT "IF_then"
        Node* then_body = build_graph_from_statement(condition_expr, inner_if_statement.get_then_body(), if_graph_node);
        condition_expr->get_exit_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
        
        if (_continue_stmt)
        {
            connect_nodes(then_body, _continue_stack.top());
            _continue_stmt = false; // Reset this value
        }
        else if (_break_stmt)
        {
            connect_nodes(then_body, _break_stack.top());
            _break_stmt = false; // Reset this value
        }
        else if (_goto_stmt)
        {    
            _goto_stmt = false; // Reset this value
        }
        else
        {    
            connect_nodes(then_body, exit_node);
        }
        
        // Third child, if exists, is a COMPOUND_STATEMENT "IF_else"
        // it may contain other "ELSE IF" which will be called recursively
        if (inner_if_statement.has_else())
        {
            Node* else_body = build_graph_from_statement(condition_expr, inner_if_statement.get_else_body(), if_graph_node);
            exit_node->set_id(++_nid);
            if (_continue_stmt)
            {
                connect_nodes(else_body, _continue_stack.top());
                _continue_stmt = false; // Reset this value
            }
            else if (_break_stmt)
            {
                connect_nodes(else_body, _break_stack.top());
                _break_stmt = false; // Reset this value                
            }
            else if (_goto_stmt)
            {    
                _goto_stmt = false;
            }
            else
            {    
                connect_nodes(else_body, exit_node);
            }
        }
        else
        {
            exit_node->set_id(++_nid);
            connect_nodes(condition_expr, exit_node);
        }
        condition_expr->get_exit_edges()[1]->set_data<Edge_type>("type", FALSE_EDGE);
        
        return if_graph_node;
    }
    
    Node* ExtensibleGraph::build_switch_node(Node* parent, Statement switch_stmt, Node* outer_graph)
    {
        SwitchStatement inner_switch_statement(switch_stmt.get_ast(), _sl);
        
        Node* switch_graph_node = append_new_node_to_parent(parent, ObjectList<AST_t>(), outer_graph, GRAPH_NODE);
        Node* entry_node = switch_graph_node->get_data<Node*>("entry");
        Node* exit_node = switch_graph_node->get_data<Node*>("exit");
        
        // First children is an AST_CONDITION "SWITCH_condition"
        AST_t condition_ast = inner_switch_statement.get_condition().get_expression().get_ast();
        Node* condition_expr = append_new_node_to_parent(entry_node, ObjectList<AST_t>(1, condition_ast), switch_graph_node);
        
        // Second child is a COMPOUND_STATEMENT "SWITCH_body"
        Statement switch_body = inner_switch_statement.get_switch_body();
        ObjectList<Node*> switch_exit_parents;
        if (switch_body.is_compound_statement())
        {
            ObjectList<Node*> previous_parents;
            int actual_edge_index = 0;
            ObjectList<Statement> inner_switch_body_statements = switch_body.get_inner_statements();
            for(ObjectList<Statement>::iterator it = inner_switch_body_statements.begin();
                    it != inner_switch_body_statements.end(); 
                    ++it)
            {
                if (CaseStatement::predicate(it->get_ast()) || DefaultStatement::predicate(it->get_ast()))
                {
                    build_case_node(condition_expr, previous_parents, switch_exit_parents, 
                                    actual_edge_index, it, inner_switch_body_statements.end(), switch_graph_node);
                }
                else
                {   // Do nothing!
                }
            }
            
            exit_node->set_id(++_nid);
            connect_nodes(switch_exit_parents, exit_node);
        }
        
        return switch_graph_node;
    }
    
    Node* ExtensibleGraph::build_case_node(Node* condition_node, ObjectList<Node*>& previous_parents,
                                           ObjectList<Node*>& switch_exit_parents, int& actual_edge_index,
                                           ObjectList<Statement>::iterator& it, ObjectList<Statement>::iterator end,
                                           Node* outer_graph)
    {
        AST_t first_stmt;
        std::stringstream ss;
        ObjectList<Statement> inner_case_statements;
        if (CaseStatement::predicate(it->get_ast()))
        {
            CaseStatement cs(it->get_ast(), _sl);
            first_stmt = cs.get_statement().get_ast();
            ss << cs.get_case_expression().prettyprint();
            inner_case_statements.append(cs.get_statement());
        }
        else if (DefaultStatement::predicate(it->get_ast()))
        {
            DefaultStatement ds(it->get_ast(), _sl);
            first_stmt = ds.get_statement().get_ast();
            ss << -1;
            inner_case_statements.append(ds.get_statement());
        }
        if (BreakStatement::predicate(first_stmt))
        {
            Node* empty_case_node = append_new_node_to_parent(condition_node, ObjectList<AST_t>(), outer_graph);
            condition_node->get_exit_edges()[actual_edge_index]->set_data<Edge_type>("type", CASE_EDGE);
            condition_node->get_exit_edges()[actual_edge_index]->set_data<std::string>("label", ss.str());
            ++actual_edge_index;
            
            previous_parents.clear();
            switch_exit_parents.append(empty_case_node);
        }
        else if (CaseStatement::predicate(first_stmt) || DefaultStatement::predicate(first_stmt))
        {
            Node* empty_case_node = append_new_node_to_parent(condition_node, ObjectList<AST_t>(), outer_graph);
            condition_node->get_exit_edges()[actual_edge_index]->set_data<Edge_type>("type", CASE_EDGE);
            condition_node->get_exit_edges()[actual_edge_index]->set_data<std::string>("label", ss.str());
            ++actual_edge_index;
            
            connect_nodes(previous_parents, empty_case_node);
            previous_parents.append(empty_case_node);
            
            build_case_node(condition_node, previous_parents, switch_exit_parents, actual_edge_index, it, end, outer_graph);
        }
        else
        {
            ++it;
            while (it != end &&
                !BreakStatement::predicate(it->get_ast()) && 
                !CaseStatement::predicate(it->get_ast()) &&
                !DefaultStatement::predicate(it->get_ast()))
            {
                inner_case_statements.append(*it);
                ++it;
            }
            
            Node* inner_case_node = build_graph_from_statements(condition_node, inner_case_statements, outer_graph);
            condition_node->get_exit_edges()[actual_edge_index]->set_data<Edge_type>("type", CASE_EDGE);
            condition_node->get_exit_edges()[actual_edge_index]->set_data<std::string>("label", ss.str());
            ++actual_edge_index;
            connect_nodes(previous_parents, inner_case_node);
            
            if (BreakStatement::predicate(it->get_ast()))
            {
                previous_parents.clear();
                switch_exit_parents.append(inner_case_node);
            }
            else if (CaseStatement::predicate(it->get_ast()) || DefaultStatement::predicate(it->get_ast()))
            {
                previous_parents.append(inner_case_node);
                build_case_node(condition_node, previous_parents, switch_exit_parents, actual_edge_index, it, end, outer_graph);
            }
            if (it == end)
            {
                switch_exit_parents.append(inner_case_node);
                --it;
            }
        }
    }
    
    Node* ExtensibleGraph::build_try_node(Node* parent, Statement try_stmt, Node* outer_graph)
    {
        TryStatement inner_try_statement(try_stmt.get_ast(), _sl);
        
        Node* try_graph_node = append_new_node_to_parent(parent, ObjectList<AST_t>(), outer_graph, GRAPH_NODE);
        Node* entry_node = try_graph_node->get_data<Node*>("entry");
        Node* exit_node = try_graph_node->get_data<Node*>("exit");
        
        // In a Try Block, conservatively, all the inner statements may throw an exception
        // So, each statement is a Basic Block
        ObjectList<Statement> try_block_statements = inner_try_statement.get_try_protected_block().get_inner_statements();
        ObjectList<Node*> try_block_nodes;
        Node* last_node = entry_node;
        for(ObjectList<Statement>::iterator it = try_block_statements.begin();
                it != try_block_statements.end();
                it++)
        {
            last_node = build_graph_from_statement(last_node, *it, try_graph_node);
            try_block_nodes.append(last_node);
        }
        
        ObjectList<Node*> try_exit_parents;
        bool ellipsis_handler;
        if (try_block_statements.size() > 0)
        {
            ObjectList<Statement> try_handler_blocks = inner_try_statement.get_try_handler_blocks();
            int i = 0;
            ObjectList<Declaration> try_handler_declarations = inner_try_statement.get_try_handler_declarations();
            for(ObjectList<Statement>::iterator it = try_handler_blocks.begin();
                    it != try_handler_blocks.end();
                    it++, i++)
            {
                Node* try_handler_block = build_graph_from_statement(NULL, *it, try_graph_node);
                // TODO We should improve the Declaration object and here, only label the edge with the expression, not the whole declaration
                connect_nodes(try_block_nodes, try_handler_block, CATCH_EDGE, try_handler_declarations[i].prettyprint());
                try_exit_parents.append(try_handler_block);
                
                // We must know if there exists the ellipsis handler in order to connect 
                // the end of the try block with the exit of the try graph
                if (try_handler_declarations[i].get_ast().internal_ast_type() == "AST_ANY_EXCEPTION")
                {    
                    ellipsis_handler = true;
                }
            }

            if (!ellipsis_handler)
            {
                _unhand_try_excpt_list.append(try_block_nodes);
            }
        }
        else
        {
            try_exit_parents.append(entry_node);
        }

        exit_node->set_id(++_nid);
        connect_nodes(try_exit_parents, exit_node);
        
        return try_graph_node;
    }

    Node* ExtensibleGraph::build_labeled_statement(Node* parent, Statement labeled_stmt, Node* outer_graph)
    {
        LabeledStatement inner_labeled_statement(labeled_stmt.get_ast(), _sl);
        ObjectList<AST_t> labeled_ast(1, inner_labeled_statement.get_labeled_statement().get_ast());
        Node* labeled_node = append_new_node_to_parent(parent, labeled_ast, outer_graph, BASIC_LABELED_NODE);
        labeled_node->set_data<std::string>("label", inner_labeled_statement.get_label());
        
        _labeled_node_list.append(labeled_node);
        
        // Check if there is some GotoStatement that jumps to this node
        for(ObjectList<Node*>::iterator it = _goto_node_list.begin();
                it != _goto_node_list.end();
                it++)
        {
            if ((*it)->get_data<std::string>("label") == inner_labeled_statement.get_label())
            {    
                connect_nodes(*it, labeled_node);
            }
        }
        
        return labeled_node;
    }
    
    Node* ExtensibleGraph::build_goto_statement(Node* parent, Statement goto_stmt, Node* outer_graph)
    {
        GotoStatement inner_goto_statement(goto_stmt.get_ast(), _sl);
        
        Node* goto_node = append_new_node_to_parent(parent, ObjectList<AST_t>(1, goto_stmt.get_ast()), 
                                                    outer_graph, BASIC_GOTO_NODE);
        goto_node->set_data<std::string>("label", inner_goto_statement.get_label());
        
        _goto_node_list.append(goto_node);
        
        for(ObjectList<Node*>::iterator it = _labeled_node_list.begin();
                it != _labeled_node_list.end();
                it++)
        {
            if ((*it)->get_data<std::string>("label") == inner_goto_statement.get_label())
            {    
                connect_nodes(goto_node, *it);
            }
        }
        
        _goto_stmt = true;
        
        return NULL;
    }
    
    Node* ExtensibleGraph::build_pragma_construct(Node* parent, Statement pragma_stmt, Node* outer_graph)
    {
        Node* pragma_graph_node = append_new_node_to_parent(parent, ObjectList<AST_t>(), outer_graph, GRAPH_NODE);
        pragma_graph_node->set_data("pragma", pragma_stmt.get_pragma_line().get_ast());
        Node* entry_node = pragma_graph_node->get_data<Node*>("entry");
        Node* exit_node = pragma_graph_node->get_data<Node*>("exit");
        
        // Pragma construct node
        Node* pragma_construct_node = append_new_node_to_parent(entry_node, ObjectList<AST_t>(1, pragma_stmt.get_pragma_line().get_ast()), 
                                                                outer_graph);
        
        // Pragma Statement subgraph
        Node* pragma_stmt_cfg = build_graph_from_statement(pragma_construct_node, pragma_stmt.get_pragma_construct_statement());
        
        exit_node->set_id(_nid);
        connect_nodes(pragma_stmt_cfg, exit_node);
            
        return pragma_graph_node;
    }
    
    
    Node* ExtensibleGraph::build_return_node(Node* parent, Statement return_stmt, Node* outer_graph)
    {
        ReturnStatement inner_return_statement(return_stmt.get_ast(), _sl);
        
        Node* return_node = append_new_node_to_parent(parent, ObjectList<AST_t>(1, inner_return_statement.get_ast()), outer_graph);
        
        return return_node;
    }


    Node* ExtensibleGraph::create_graph_node(Node* outer_graph)
    {
        Node* result = new Node(_nid, GRAPH_NODE, outer_graph);
        
        Node* entry_node = result->get_data<Node*>("entry");
        entry_node->set_data<Node*>("outer_graph", result);
        Node* exit_node = result->get_data<Node*>("exit");
        exit_node->set_data<Node*>("outer_graph", result);
        
        return result;
    }

    Node* ExtensibleGraph::append_new_node_to_parent(Node *parent, ObjectList<Statement> stmts, 
                                                     Node* outer_graph, Node_type ntype, Edge_type etype)
    {
        ObjectList<AST_t> stmts_asts;
        for(ObjectList<Statement>::iterator it = stmts.begin();
                it != stmts.end();
                ++it)
        {
            stmts_asts.append(it->get_ast());
        }
        append_new_node_to_parent(parent, stmts_asts, outer_graph, ntype, etype);
    }

    Node* ExtensibleGraph::append_new_node_to_parent(Node *parent, ObjectList<AST_t> stmts, 
                                                     Node* outer_graph, Node_type ntype, Edge_type etype)
    {
        Node* new_node = new Node(_nid, ntype, outer_graph);
        if (ntype != BASIC_ENTRY_NODE && ntype != BASIC_EXIT_NODE && ntype != GRAPH_NODE)
        {
            new_node->set_data <ObjectList<AST_t> > ("Statements", stmts);
        }
        if (ntype == GRAPH_NODE)
        {
            Node* entry_node = new_node->get_data<Node*>("entry");
            Node* exit_node = new_node->get_data<Node*>("exit");
            entry_node->set_data("outer_graph", new_node);
            exit_node->set_data("outer_graph", new_node);
        }
        if (parent != NULL)
        {
            connect_nodes(parent, new_node, etype);
        }
        
        return new_node;
    }
    
    void ExtensibleGraph::connect_nodes(ObjectList<Node*> parents, Node* child, Edge_type etype, std::string label)
    {
        for(ObjectList<Node*>::iterator it = parents.begin();
                it != parents.end();
                ++it)
        {
            connect_nodes(*it, child, etype, label);
        }
    }    
    
    void ExtensibleGraph::connect_nodes(Node* parent, Node* child, Edge_type etype, std::string label)
    {
        if (parent != NULL && child != NULL)
        {
            Edge* new_edge = new Edge(parent, child, etype, label);
            parent->set_exit_edge(new_edge);
            child->set_entry_edge(new_edge);
        }
    }

    void ExtensibleGraph::disconnect_nodes(Node *parent, Node *child)
    {
        parent->erase_exit_edge(child);
        child->erase_entry_edge(parent);
    }

    bool ExtensibleGraph::belongs_to_the_same_graph(Edge* edge)
    {
        Node* source = edge->get_source();
        Node* target = edge->get_target();

        if (source->has_key("outer_graph") && target->has_key("outer_graph"))
        {
            Node* source_outer_graph = source->get_data<Node*>("outer_graph");
            Node* target_outer_graph = target->get_data<Node*>("outer_graph");

            if (source_outer_graph == target_outer_graph)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
        else
        {
            if (!source->has_key("outer_graph") && !target->has_key("outer_graph"))
            {
                return true;
            }
            else
            {
                return false;
            }
        }
    }
    
    void ExtensibleGraph::clear_unnecessary_nodes()
    {   
        // Clear all the Entry / Exit nodes except the first and the last ones
        clear_orphaned_nodes(_exit);
        clear_visits(_entry);
        
        //         ObjectList<CFGNode*> parents, children;
        //         ObjectList<CFGNode*>::iterator it;
        //         for(it = CFG::_nodeList.begin()+1;
        //                 it < CFG::_nodeList.end()-1;
        //                 it++)
        //         {
            //             if ((*it)->get_node_type()==ENTRY_NODE || (*it)->get_node_type()==EXIT_NODE)
            //             {
                //                 parents = (*it)->get_parents();
                //                 children = (*it)->get_children();
                //                 if (parents.size()>0)
                //                 {
                    // //                     std::cout << "__________________________________" << std::endl;
                    //                     for(ObjectList<CFGNode*>::iterator itp = parents.begin();
                    //                             itp != parents.end();
                    //                             itp++)
                    //                     {
                        //                         Edge_type edge_t = (*it)->get_edge_type(*itp);
                        //                         CFGEdge* edg = (*it)->get_edge(*itp);
                        //                         (*it)->disconnect_nodes(*itp);
                        //                         for(ObjectList<CFGNode*>::iterator itc = children.begin();
                        //                                 itc != children.end();
                        //                                 itc++)                    
                        //                         {
                            //                             int value;
                            //                             Edge_type e = edg->merge_edges_types((*itc)->get_edge(*it), value);
                            //                             (*itc)->connect_nodes(*itp, e, value);
                            //                         }
                            //                     }
                            //                     for(ObjectList<CFGNode*>::iterator itc = children.begin();
                            //                             itc != children.end();
                            //                             itc++)                    
                            //                     {
                                //                         (*itc)->disconnect_nodes(*it);
                                //                     }
                                //                 }
                                //                 else
                                //                 {
                                    // //                     std::cout << "*****************************" << std::endl;
                                    //                     for(ObjectList<CFGNode*>::iterator itc = children.begin();
                                    //                             itc != children.end();
                                    //                             itc++)                    
                                    //                     {
                                        //                         (*itc)->disconnect_nodes(*it);
                                        //                     }   
                                        //                 }
                                        //                 CFG::_nodeList.erase(it);
                                        //                 it--;   // We have reduced the length of @_nodeList
                                        //             }
                                        //         }
                                        //         
                                        //         // Concatenate the nodes that are always executed consecutively
                                        //         // Entry and Exit nodes are never concatenated
                                        //         // The second node will be always the only child of the Entry node, so we don't treat this node'
                                        //         // Nodes that belongs to a subgraph are never concatenated with nodes that do not belong to the same subgraph
                                        //         ObjectList<CFGEdge*>::iterator ite;
                                        //         for(it = CFG::_nodeList.begin()+2;
                                        //                 it < CFG::_nodeList.end()-1;
                                        //                 it++)
                                        //         {
                                            //             // Delete parents with identifier equal to -1 (nodes after a Throw expression)
                                            //             (*it)->remove_null_parents_from_list();
                                            //             
                                            //             if ((*it)->get_entry_edges().size() == 0 || 
                                            //                 ((*it)->get_entry_edges().size() == 1 && (*it)->get_entry_edges()[0]->get_source()->get_id() == -1 ))
                                            //             { // Disjointed node, we want to delete it
                                            //                 (*it)->delete_node();
                                            //                 delete(*it);
                                            //                 CFG::_nodeList.erase(it);
                                            //                 it--;   // We have reduced the length of @_nodeList
                                            //             }
                                            //             else if ((*it)->can_be_joined(CFG::_sl))
                                            //             { // The node can be joined with its parent
                                            //                 (*it)->get_parents()[0]->join_nodes(*it);
                                            //                 CFG::_nodeList.erase(it);
                                            //                 it--;   // We have reduced the length of @_nodeList
                                            //             }
                                            //         }
    }
    
    void ExtensibleGraph::clear_orphaned_nodes(Node* actual_node)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
            
            ObjectList<Edge*> entries = actual_node->get_entry_edges();
            
            Node_type ntype = actual_node->get_data<Node_type>("type");
            if (entries.empty() && actual_node != _entry)
            {
                clear_orphaned_cascade(actual_node);
            }
            else
            {
                if (ntype == GRAPH_NODE)
                {   // Traverse the inner nodes
                    clear_orphaned_nodes_in_subgraph(actual_node->get_data<Node*>("exit"));
                }
                // Continue with the outer traversal
                for (ObjectList<Edge*>::iterator it = entries.begin();
                        it != entries.end();
                        ++it)
                {
                    clear_orphaned_nodes((*it)->get_source());
                }
            }
        }
        return;
    }
    
    void ExtensibleGraph::clear_orphaned_nodes_in_subgraph(Node* actual_node)
    {
        if (!actual_node->is_visited())
        {    
            Node_type ntype = actual_node->get_data<Node_type>("type");
            
            if (ntype == BASIC_ENTRY_NODE)
            {
                actual_node->set_visited(true);
                return;
            }
            
            if (ntype == GRAPH_NODE)
            {
                actual_node->set_visited(true);
                clear_orphaned_nodes_in_subgraph(actual_node->get_data<Node*>("exit"));
                
                ObjectList<Edge*> entries = actual_node->get_entry_edges();
                for (ObjectList<Edge*>::iterator it = entries.begin();
                        it != entries.end();
                        ++it)
                {
                    clear_orphaned_nodes_in_subgraph((*it)->get_source());
                }
            }
            else
            {
                ObjectList<Edge*> entries = actual_node->get_entry_edges();
                
                if (entries.empty())
                {
                    clear_orphaned_cascade(actual_node);
                }
                else
                {
                    actual_node->set_visited(true);
                    for (ObjectList<Edge*>::iterator it = entries.begin();
                            it != entries.end();
                            ++it)
                    {
                        clear_orphaned_nodes_in_subgraph((*it)->get_source());
                    }
                }
            }
        }
        return;
    }
    
    void ExtensibleGraph::clear_orphaned_cascade(Node* actual_node)
    {
        ObjectList<Edge*> children = children = actual_node->get_exit_edges();
        for(ObjectList<Edge*>::iterator it = children.begin();
                it != children.end();
                ++it)
        {
            disconnect_nodes(actual_node, (*it)->get_target());
            if (!(*it)->get_target()->get_entry_edges().empty())
            {
                clear_orphaned_cascade((*it)->get_target());
            }
        }
    }
    
    void ExtensibleGraph::clear_visits(Node* actual)
    {
        if (actual->is_visited())
        {
            actual->set_visited(false);
            
            Node_type ntype = actual->get_data<Node_type>("type");
            if (ntype == GRAPH_NODE)
            {
                clear_visits(actual->get_data<Node*>("entry"));
            }
            else if (ntype == BASIC_EXIT_NODE)
            {
                return;
            }
            
            ObjectList<Edge*> children = actual->get_exit_edges();
            for(ObjectList<Edge*>::iterator it = children.begin();
                    it != children.end();
                    ++it)
            {
                if ((*it)->get_target()->is_visited())
                {
                    clear_visits((*it)->get_target());
                }
            }
        }
    }
    
}
