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


#include "tl-extensible-graph.hpp"
#include "tl-extensible-symbol.hpp"

namespace TL
{
    //! This function returns the set which is the union of the two input sets
    static std::set<ExtensibleSymbol, 
                    ExtensibleSymbol_comp> sets_union(std::set<ExtensibleSymbol,
                                                               ExtensibleSymbol_comp> set1,
                                                      std::set<ExtensibleSymbol,
                                                               ExtensibleSymbol_comp> set2);
    
    //! This function returns true if the two sets contain the same elements
    static bool sets_equals(std::set<ExtensibleSymbol, ExtensibleSymbol_comp> set1, 
                            std::set<ExtensibleSymbol, ExtensibleSymbol_comp> set2);
    
    //! This function substract from @set1 the elements in @set2
    static std::set<ExtensibleSymbol, 
                    ExtensibleSymbol_comp> sets_difference(std::set<ExtensibleSymbol,
                                                                    ExtensibleSymbol_comp> set1,
                                                           std::set<ExtensibleSymbol,
                                                                    ExtensibleSymbol_comp> set2);
    
    void ExtensibleGraph::live_variable_analysis()
    {
//         gather_live_initial_information(_entry);
//         clear_visits(_entry);
//         solve_live_equations();
    }
    
    void ExtensibleGraph::gather_live_initial_information(Node* actual)
    {
        Node_type ntype = actual->get_data<Node_type>(_NODE_TYPE);
        while (!actual->is_visited())
        {
            actual->set_visited(true);
            if (ntype != BASIC_EXIT_NODE)
            {
                if (ntype == GRAPH_NODE)
                {
                    gather_live_initial_information(actual->get_data<Node*>(_ENTRY_NODE));
                }
                else if (ntype != BASIC_ENTRY_NODE)
                {
                    actual->set_live_initial_information(_sl);
                }
                    
                ObjectList<Edge*> exit_edges = actual->get_exit_edges();
                for (ObjectList<Edge*>::iterator it = exit_edges.begin();
                        it != exit_edges.end();
                        ++it)
                {
                    gather_live_initial_information((*it)->get_target());
                }
                continue;
            }
            return;
        }
    }
    
    void ExtensibleGraph::solve_live_equations()
    {
        bool changed = true;
        while (changed)
        {
            changed = false;
//             solve_live_equations_recursive(_entry, changed);
//             clear_visits(_entry);
        }
    }
    
    void ExtensibleGraph::solve_live_equations_recursive(Node* actual, bool& changed)
    {
        while (!actual->is_visited())
        {
            actual->set_visited(true);
            
            Node_type ntype = actual->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                ObjectList<Node*> children = actual->get_children();
                
                if (ntype == GRAPH_NODE)
                {
                    if (!actual->has_key(_NODE_LABEL))
                    {
                        internal_error("Graph node '%d' with no specified label."
                                       "Expecting a Pragma, Function_call, Conditional Espression "
                                       "or Splitted instruction as a Graph node here",
                                       actual->get_id());
                    }
                    Node* entry_node = actual->get_data<Node*>(_ENTRY_NODE);
                    solve_live_equations_recursive(entry_node, changed);
                    
                    // spread the liveness inside the node to the Graph node
                    actual->set_graph_node_liveness();
                }
                else
                {
                    
                    if (ntype != BASIC_ENTRY_NODE)
                    {
                        std::set<ExtensibleSymbol, ExtensibleSymbol_comp> old_live_in = 
                            actual->get_live_in_vars();
                        std::set<ExtensibleSymbol, ExtensibleSymbol_comp> old_live_out = 
                            actual->get_live_out_vars();
                        std::set<ExtensibleSymbol, ExtensibleSymbol_comp> live_out, live_in, 
                            aux_live_in, aux;
                        
                        for(ObjectList<Node*>::iterator it = children.begin();
                            it != children.end();
                            ++it)
                        {
                            Node_type nt = (*it)->get_data<Node_type>(_NODE_TYPE);
                            if (nt == GRAPH_NODE)
                            {
                                ObjectList<Node*> inner_children = 
                                    (*it)->get_data<Node*>(_ENTRY_NODE)->get_children();
                                for(ObjectList<Node*>::iterator itic = inner_children.begin();
                                    itic != inner_children.end();
                                    ++itic)
                                {
                                    std::set<ExtensibleSymbol, ExtensibleSymbol_comp> aux_set = 
                                        (*itic)->get_live_in_vars();
                                    aux_live_in.insert(aux_set.begin(), aux_set.end());
                                }
                            }
                            else if (nt == BASIC_EXIT_NODE /*&& *it != _exit*/)     // Exit ja no existeix així!!
                            {
                                ObjectList<Node*> outer_children = 
                                    (*it)->get_data<Node*>(_OUTER_NODE)->get_children();
                                for(ObjectList<Node*>::iterator itoc = outer_children.begin();
                                    itoc != outer_children.end();
                                    ++itoc)
                                {
                                    std::set<ExtensibleSymbol, ExtensibleSymbol_comp> aux_set = 
                                        (*itoc)->get_live_in_vars();
                                    aux_live_in.insert(aux_set.begin(), aux_set.end());
                                }
                            }
                            else
                            {
                                aux_live_in = (*it)->get_live_in_vars();
                            }
                            live_out.insert(aux_live_in.begin(), aux_live_in.end());
                        }
                        
                        aux = sets_difference(live_out, actual->get_killed_vars());
                        live_in = sets_union(actual->get_ue_vars(), aux);
                        
                        if (!sets_equals(old_live_in, live_in) || 
                            !sets_equals(old_live_out, live_out))
                        {
                            actual->set_live_in(live_in);
                            actual->set_live_out(live_out);
                            changed = true;
                        }
                    }
                }
                
                for(ObjectList<Node*>::iterator it = children.begin();
                    it != children.end();
                    ++it)
                {
                    solve_live_equations_recursive(*it, changed);
                }
                continue;
            }
            return;
        }
    }
    
    void Node::set_live_initial_information(ScopeLink sl)
    {
        if (has_key(_NODE_STMTS)) 
        {
            ObjectList<AST_t> basic_block = get_data<ObjectList<AST_t> >(_NODE_STMTS);
            for (ObjectList<AST_t>::iterator it = basic_block.begin();
                    it != basic_block.end();
                    ++it)
            {
                if (Declaration::predicate(*it))
                {
                    // Get the defined variables in the declaration
                    Declaration decl(*it, sl);
                    ObjectList<DeclaredEntity> decl_ents = decl.get_declared_entities();
                    for(ObjectList<DeclaredEntity>::iterator itdecl = decl_ents.begin();
                        itdecl != decl_ents.end();
                        itdecl++)
                    {
                        set_killed_var(ExtensibleSymbol(itdecl->get_declared_symbol()));
                    }
                }
                else if (Expression::predicate(*it))
                {
                    Expression expr(*it, sl);
                    set_live_initial_expression_information(expr, /* Left-hand expr */ false);
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    if (Fortran::StopStatement::predicate(*it))
                    {  // Do nothing
                    }
                    else if (Fortran::SpecificationStatement::predicate(*it))
                    {
                        std::cout << "warning: '" << it->prettyprint() 
                                  << "' is a special Fortran Statement. It is not already supported"
                                    << std::endl;
                    }
                }
                else if (Statement::predicate(*it))
                {
                    if (ReturnStatement::predicate(*it))
                    {
                        ReturnStatement stmt(*it, sl);
                        if (stmt.has_return_expression())
                            set_live_initial_expression_information(stmt.get_return_expression(), 
                                                                    /* Left-hand expr */ false);
                    }
                    else
                    {
                        internal_error("Unexpected Statement '%s' while computing initial node "
                                       "information in Liveness analysis", 
                                        it->prettyprint().c_str());
                    }
                }
                else
                {
                    internal_error("Unexpected tree type '%s' while computing initial node "
                                    "information in Liveness analysis", 
                                    it->internal_ast_type().c_str());
                }
            }
        }
    }
    
    void Node::set_live_initial_expression_information(Expression e, bool defined)
    {
        //         std::cout << "Expression: " << e.prettyprint() << std::endl;
        bool has_ellipsis = false;
        if (e.is_literal())
            return;
        if (e.is_id_expression())
            fill_use_def_sets(e.get_id_expression().get_symbol(), defined);
        else if (e.is_unary_operation() && e.get_operation_kind()==Expression::REFERENCE)
            fill_use_def_sets(e.get_unary_operand().get_symbol(), defined);
        else if (e.is_member_access() || e.is_pointer_member_access())
            fill_use_def_sets(e.get_accessed_entity().get_symbol(), defined);
        else if (e.is_assignment())
        {
            set_live_initial_expression_information(e.get_second_operand(), /* Defined */ false);
            set_live_initial_expression_information(e.get_first_operand(), /* Defined */ true);
        }
        else if (e.is_binary_operation())
        {
            if (e.get_operation_kind() == Expression::SHIFT_LEFT)
            {
                std::cerr <<" ** static_analysis.cpp :: set_live_initial_expression_information ** "
                          << "warning: When shift operation is an overloaded funtion " 
                          << " then the left hand operator must be treated as a definition "
                          << "but it is done as a use. We must fix that" << std::endl;
            }
            // FIXME When '<<' is an overloaded function, then the left variable must be defined
            set_live_initial_expression_information(e.get_first_operand(), /* Defined */ false);
            set_live_initial_expression_information(e.get_second_operand(), /* Defined */ false);
        }
        else if (e.is_unary_operation())
        {
            // The symbol will always be read before than written, so it will be Used before Defined
            set_live_initial_expression_information(e.get_unary_operand(), /* Defined */ false);
            // If the unary operation is an Incr/Decrement, then the symbol will be also Defined
            Expression::OperationKind op = e.get_operation_kind();
            if ((op == Expression::PREINCREMENT) || (op == Expression::POSTINCREMENT) 
                || (op == Expression::PREDECREMENT) || (op == Expression::POSTDECREMENT))
                set_live_initial_expression_information(e.get_unary_operand(), /* Defined */ true);
        }
        else if (e.is_casting())
        {
            set_live_initial_expression_information(e.get_casted_expression(), /* Defined */ false);
        }
        else if (e.is_array_subscript())
        {
            set_live_initial_expression_information(e.get_subscript_expression(), 
                                                    /* Defined */ false);
            set_live_initial_expression_information(e.get_subscripted_expression(), defined);
        }
        else if (e.is_function_call())
        {
            Expression called_expression = e.get_called_expression();
            Type type = called_expression.get_type();
            ObjectList<Type> parameter_types = type.parameters(has_ellipsis);
            
            ObjectList<Expression> args = e.get_argument_list();
            ObjectList<Type>::iterator itp = parameter_types.begin();
            ObjectList<Expression>::iterator ita = args.begin();
            for(; itp != parameter_types.end(); ita++, itp++)
            {
                // Regarding the parameter, if possible (arguments that are not in ellipsis)
                if ((itp->is_pointer() && !itp->points_to().is_const()) || 
                    (itp->is_reference() && !itp->references_to().is_const()))
                {
//                     std::cout << "Parameter " << ita->prettyprint() 
//                               << " is pointer and not const" << std::endl;
                    // Assuming that the argument is used and defined, in this order
                    set_live_initial_expression_information(*ita, /* Defined */ false);
                    set_live_initial_expression_information(*ita, /* Defined */ true);
                }
                else
                {
//                     std::cout << "Parameter " << ita->prettyprint() 
//                               << " is not pointer or const" << std::endl;
                    set_live_initial_expression_information(*ita, /* Defined */ false);
                }
            }
            if (has_ellipsis)
            {
                // There are more arguments than parameters, so we must regard the argument
                for(; ita != args.end(); ita++)
                {
                    if ((ita->get_type().is_pointer() && !ita->get_type().points_to().is_const()) ||
                        (ita->get_type().is_reference() && 
                             !ita->get_type().references_to().is_const()))
                    {
//                         std::cout << "Argument " << ita->prettyprint() 
//                                   << " is pointer and not const" << std::endl;
                        // Assuming that the argument is used and defined, in this order
                        set_live_initial_expression_information(*ita, /* Defined */ false);
                        set_live_initial_expression_information(*ita, /* Defined */ true);
                    }
                    else
                    {    
//                         std::cout << "Argument " << ita->prettyprint() 
//                                   << " is not pointer or const" << std::endl;
                        set_live_initial_expression_information(*ita, /* Defined */ false);
                    }
                }
            }
        }
        else if (e.is_conditional())
        {
            set_live_initial_expression_information(e.get_condition_expression(), 
                                                    /* Defined */ false);
            set_live_initial_expression_information(e.get_true_expression(), /* Defined */ false);
            set_live_initial_expression_information(e.get_false_expression(), /* Defined */ false);
        }
        else if (e.is_throw_expression())
        {
            set_live_initial_expression_information(e.get_throw_expression(), /* Defined */ false);
        }
        else
        {
            internal_error("Unexpected expression '%s' when computing the live variable analysis\n",
                           e.prettyprint().c_str());
        }
    }
    
    void Node::fill_use_def_sets(Symbol s, bool defined)
    {
        if (defined)
        {
            set_killed_var(ExtensibleSymbol(s));
        }
        else
        {
            std::set<ExtensibleSymbol, ExtensibleSymbol_comp> killed_vars = get_killed_vars();
            if (killed_vars.find(ExtensibleSymbol(s)) == killed_vars.end())
            {
                set_ue_var(ExtensibleSymbol(s));
            }
        }
    }
   
    static std::set<ExtensibleSymbol, 
                    ExtensibleSymbol_comp> sets_union(std::set<ExtensibleSymbol,
                                                               ExtensibleSymbol_comp> set1, 
                                                      std::set<ExtensibleSymbol,
                                                               ExtensibleSymbol_comp> set2)
    {
        std::vector<ExtensibleSymbol> v_result(set1.size() + set2.size());
        std::vector<ExtensibleSymbol>::iterator it;
        std::set<ExtensibleSymbol, ExtensibleSymbol_comp> result;
        
        it = set_union(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
        
        for(int i=0; i<int(it-v_result.begin()); i++)
        {    
            result.insert(v_result.at(i));
        }
        
        return result;
    }
   
    static bool sets_equals(std::set<ExtensibleSymbol, ExtensibleSymbol_comp> set1, 
                            std::set<ExtensibleSymbol, ExtensibleSymbol_comp> set2)
    {
        if (set1.size() == set2.size())
        {
            std::vector<ExtensibleSymbol>::iterator it;
            std::vector<ExtensibleSymbol> v_result(set1.size());
            
            it = set_intersection(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
            
            return (int(it-v_result.begin()) == set1.size());
        }
        else
        {    
            return false;
        }
    }
   
    static std::set<ExtensibleSymbol, 
                    ExtensibleSymbol_comp> sets_difference(std::set<ExtensibleSymbol,
                                                                    ExtensibleSymbol_comp> set1, 
                                                           std::set<ExtensibleSymbol,
                                                                    ExtensibleSymbol_comp> set2)
    {
        std::vector<ExtensibleSymbol> v_result(set1.size());
        std::vector<ExtensibleSymbol>::iterator it;
        std::set<ExtensibleSymbol, ExtensibleSymbol_comp> result;
        
        it = set_difference(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
        
        for(int i=0; i<int(it-v_result.begin()); i++)
        {    
            result.insert(v_result.at(i));
        }
        
        return result;
    }
}