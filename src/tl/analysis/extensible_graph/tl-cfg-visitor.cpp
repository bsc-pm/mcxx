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


#include "cxx-process.h"

#include "tl-cfg-visitor.hpp"

namespace TL
{
    CfgVisitor::CfgVisitor(ScopeLink sl)
        : _actual_cfg(sl, ""), _sl(sl), _cfgs(), _seq_nodecl()
    {}
    
    void CfgVisitor::unhandled_node(const Nodecl::NodeclBase& n) 
    {
        std::cerr << "UNHANDLED -> " << ast_print_node_type(n.get_kind()) << std::endl;
    }

    void CfgVisitor::visit(const Nodecl::TopLevel& n)
    {
        walk(n.get_top_level());
        
        for (ObjectList<ExtensibleGraph>::iterator it = _cfgs.begin();
            it != _cfgs.end(); 
            ++it)
        {
//             it->live_variable_analysis();
            it->print_graph_to_dot();
        }
    }

    void CfgVisitor::visit(const Nodecl::FunctionCode& n)
    {
        TL::Symbol s = n.get_symbol();
        std::string func_decl = s.get_type().get_declaration(s.get_scope(), s.get_name());
        std::cerr << "ESTIC A LA FUNCIO -> " << func_decl << std::endl;

        // Create a new graph for the current function
        ExtensibleGraph cfg(_sl, s.get_name());
        cfg._entry = new Node(cfg._nid, BASIC_ENTRY_NODE, NULL);
        cfg._last_node = cfg._entry;
        
        _actual_cfg = cfg;
        walk(n.get_statements());
        cfg = _actual_cfg;
        
        // Connect the sequential statements, if there are
        if (!_seq_nodecl.empty())
        {
            cfg.append_new_node_to_parent(cfg._last_node, _seq_nodecl);
            _seq_nodecl.clear();
        }
        // Task subgrpahs must be appended, conservatively, at the end of the master graph
        // FIXME Before or after the Return ??
        for (ObjectList<Node*>::iterator it = cfg._tasks_node_list.begin();
            it != cfg._tasks_node_list.end();
            ++it)
        {
            cfg.connect_nodes(cfg._last_node, *it);
            cfg._last_node = *it;
        }
        // Connect the exit nodes to the Exit node of the master graph
        cfg.append_new_node_to_parent(cfg._last_node, ObjectList<Nodecl::NodeclBase>(), BASIC_EXIT_NODE);
        cfg._exit = cfg._last_node;
        cfg.connect_nodes(cfg._unhand_try_excpt_list, cfg._exit);
        cfg.connect_nodes(cfg._throw_node_list, cfg._exit);
        
        // Remove the unnecessary nodes and join these ones that are always executed consecutively
        cfg.clear_unnecessary_nodes();
        
        _cfgs.append(cfg);
    }

    void CfgVisitor::visit(const Nodecl::TryBlock& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::CatchHandler& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Throw& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::CompoundStatement& n)
    {
        walk(n.get_statements());
    }

    void CfgVisitor::visit(const Nodecl::AnyList& n)
    {
        std::cerr << "Visiting a List" << std::endl;
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
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
        walk(n.get_nest());
        _seq_nodecl.append(n);
    }

    void CfgVisitor::visit(const Nodecl::ParenthesizedExpression& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::ErrExpr& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::ObjectInit& n)
    {
        std::cerr << "This is an Object init" << std::endl;
        walk(n.get_init_expr());
        _seq_nodecl.append(n);
    }

    void CfgVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    // What's that??
    void CfgVisitor::visit(const Nodecl::NamedPairSpec& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    // What's that??
    void CfgVisitor::visit(const Nodecl::Concat& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::New& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Delete& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::DeleteArray& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Sizeof& n)
    {
        internal_error("Node '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str());            
//             internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
//                            n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Type& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Typeid& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Cast& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Offset& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        if (!_seq_nodecl.empty())
        {
            _actual_cfg.append_new_node_to_parent(_actual_cfg._last_node, _seq_nodecl);
        }
        walk(n.get_arguments());
        if (!_seq_nodecl.empty())
        {
            _actual_cfg.append_new_node_to_parent(_actual_cfg._last_node, _seq_nodecl);
        }        
        walk(n.get_called());
        _actual_cfg.append_new_node_to_parent(_actual_cfg._last_node, ObjectList<Nodecl::NodeclBase>(1,n));
    }

    void CfgVisitor::visit(const Nodecl::FunctionCall& n)
    {
        // FIXME If this first condition necessary?? Or the arguments should be parsed with a different Visitor??
        if (!_seq_nodecl.empty())
        {
            _actual_cfg.append_new_node_to_parent(_actual_cfg._last_node, _seq_nodecl);
        }
        walk(n.get_arguments());
        if (!_seq_nodecl.empty())
        {
            _actual_cfg.append_new_node_to_parent(_actual_cfg._last_node, _seq_nodecl);
        }        
        walk(n.get_called());
        _actual_cfg.append_new_node_to_parent(_actual_cfg._last_node, ObjectList<Nodecl::NodeclBase>(1,n));
    }


    // ************* Literals ************* //
    void CfgVisitor::visit(const Nodecl::StringLiteral& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }
    
    void CfgVisitor::visit(const Nodecl::BooleanLiteral& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::IntegerLiteral& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::ComplexLiteral& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::FloatingLiteral& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::StructuredLiteral& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }


    // ************* Special Statements ************* //       
    void CfgVisitor::visit(const Nodecl::EmptyStatement& n)
    {
        _seq_nodecl.append(n);
    }
            
    void CfgVisitor::visit(const Nodecl::ReturnStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }


    // ************* Built-in ************* //
    void CfgVisitor::visit(const Nodecl::BuiltinExpr& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::BuiltinDecl& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }


    // ************* Pragmas ************* //
    void CfgVisitor::visit(const Nodecl::PragmaCustomDirective& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::PragmaCustomConstruct& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::PragmaCustomClause& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::PragmaCustomLine& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::PragmaClauseArg& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }
    
    
    // ************* Control Flow constructs ************* //
    void CfgVisitor::visit(const Nodecl::ForStatement& n)
    {
        walk(n.get_loop_header());
        
        Node* empty_exit_node = new Node();
        walk(n.get_statement());
        
        
        
//                 ForStatement inner_for_statement(for_stmt.get_ast(), _sl);
//         
//         
//         Node* last_node = parent;
//         
//         // Fourth child is an AST_COMPOUND_STATEMENT 'FOR_loop_body'
//         _continue_stack.push(increment_expr);
//         _break_stack.push(empty_exit_node);
//         Node* for_body = build_graph_from_statement(condition_expr,
//                                                     inner_for_statement.get_loop_body(),
//                                                     outer_graph);
//         Edge_type aux_etype = ALWAYS_EDGE;
//         if (!condition_expr->get_exit_edges().empty())
//         {   // It will be empty when the loop's body is empty
//             condition_expr->get_exit_edges()[0]->set_data<Edge_type>("type", TRUE_EDGE);
//         }
//         else
//         {
//             aux_etype = TRUE_EDGE;
//         }
//         _continue_stack.pop();
//         _break_stack.pop();
//         
//         empty_exit_node->set_id(++_nid);
//         connect_nodes(condition_expr, empty_exit_node, FALSE_EDGE);
//         
//         // Fill the empty fields of the Increment node
//         if (!_break_stmt)
//         {
//             connect_nodes(for_body, increment_expr, aux_etype);
//             connect_nodes(increment_expr, condition_expr);
//         }
//         else
//         {
//             connect_nodes(for_body, empty_exit_node, aux_etype);
// 
//             _break_stmt = false; // Reset this value
//         }
//         
//         _continue_stmt = false; // Conservatively, reset this value
//         
//         return empty_exit_node;
        
        
    }        

    void CfgVisitor::visit(const Nodecl::LoopControl& n)
    {
        // Build the init node
        walk(n.get_init());
        _actual_cfg.append_new_node_to_parent(_actual_cfg._last_node, _seq_nodecl);
        _seq_nodecl.clear();
        // Build the conditional node
        walk(n.get_cond());
        _actual_cfg.append_new_node_to_parent(_actual_cfg._last_node, _seq_nodecl);
        _seq_nodecl.clear();
        // Build and keep the next node
        LoopNextVisitor n_visitor(_actual_cfg, _sl);
        n_visitor.walk(n.get_next());
    }  

    void CfgVisitor::visit(const Nodecl::WhileStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }     

    void CfgVisitor::visit(const Nodecl::IfElseStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }        

    void CfgVisitor::visit(const Nodecl::SwitchStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }      

    void CfgVisitor::visit(const Nodecl::CaseStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }    

    void CfgVisitor::visit(const Nodecl::DefaultStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }    

    void CfgVisitor::visit(const Nodecl::ConditionalExpression& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }    

    void CfgVisitor::visit(const Nodecl::ComputedGotoStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::AssignedGotoStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::GotoStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::LabeledStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::ContinueStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::BreakStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::DoStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
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

    void CfgVisitor::visit(const Nodecl::Equal& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Different& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::LowerThan& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::GreaterThan& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::LowerOrEqualThan& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Shr& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Shl& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
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
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Reference& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    
    // ************* Fortran specifics ************* //
    void CfgVisitor::visit(const Nodecl::Text& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }
    
    void CfgVisitor::visit(const Nodecl::Where& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::WherePair& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::SubscriptTriplet& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::LabelAssignStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::FortranIoSpec& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }   

    void CfgVisitor::visit(const Nodecl::FieldDesignator& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }    

    void CfgVisitor::visit(const Nodecl::IndexDesignator& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::FortranEquivalence& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::FortranData& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::ImpliedDo& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::Forall& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }
    
    void CfgVisitor::visit(const Nodecl::ArithmeticIfStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }      

    void CfgVisitor::visit(const Nodecl::NullifyStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }   
    
    void CfgVisitor::visit(const Nodecl::IoStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }
    
    void CfgVisitor::visit(const Nodecl::OpenStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::CloseStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    } 

    void CfgVisitor::visit(const Nodecl::ReadStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    } 
    
    void CfgVisitor::visit(const Nodecl::WriteStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }  

    void CfgVisitor::visit(const Nodecl::PrintStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::StopStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }

    void CfgVisitor::visit(const Nodecl::AllocateStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }
    
    void CfgVisitor::visit(const Nodecl::DeallocateStatement& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }
            
    
    
    // ************* Cxx specifics ************* //
    void CfgVisitor::visit(const Nodecl::CxxRaw& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }  
    
    void CfgVisitor::visit(const Nodecl::Comma& n)
    {
        internal_error("Node '%s' with type '%s' not implemented yet. CFG construction failed.", 
                        n.get_text().c_str(), ast_print_node_type(n.get_kind()));
    }
    
    
    // ************* Special visitors for Next node of a Loop Control ************* //
    
    LoopNextVisitor::LoopNextVisitor(ExtensibleGraph egraph, ScopeLink sl)
        : CfgVisitor(sl), _next_nodecls()
    {
        _actual_cfg = egraph;
    }
    
    void LoopNextVisitor::visit(const Nodecl::ExpressionStatement& n)
    {
        walk(n.get_nest());
        _next_nodecls.append(n);
    }
    
    void LoopNextVisitor::visit(const Nodecl::FunctionCall& n)
    {
        
        _next_nodecls.append(n);
    }

    void LoopNextVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        walk(n.get_arguments());
        walk(n.get_called());
        _next_nodecls.append(n);
    }
}