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


#include <typeinfo>

#include "tl-cfg-analysis-visitor.hpp"
#include "tl-cfg-visitor.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-extensible-symbol.hpp"
#include "tl-loop-analysis.hpp"
#include "tl-static-analysis.hpp"


namespace TL
{
    namespace Analysis
    {
        // *** NODE *** //
        
        void Node::fill_use_def_sets(Nodecl::NodeclBase n, bool defined)
        {
            ExtensibleSymbol s(n);
            if (defined)
            {
                set_killed_var(ExtensibleSymbol(n));
            }
            else
            {
                ext_sym_set killed_vars = get_killed_vars();
                if (!killed_vars.contains(s))
                {
                    set_ue_var(s);
                }
            }
        }   
    
        void Node::fill_use_def_sets(Nodecl::List n_l, bool defined)
        {
            for(Nodecl::List::iterator it = n_l.begin(); it != n_l.end(); ++it)
            {
                fill_use_def_sets(*it, defined);
            }
        }
        
        StaticAnalysis::StaticAnalysis(LoopAnalysis* loop_analysis)
            :_loop_analysis(loop_analysis)
        {}
        
        void StaticAnalysis::live_variable_analysis(Node* node)
        {
            solve_live_equations(node);
            ExtensibleGraph::clear_visits(node);
        }
        
        void StaticAnalysis::solve_live_equations(Node* node)
        {
            bool changed = true;
            while (changed)
            {
                changed = false;
                solve_live_equations_recursive(node, changed);
                ExtensibleGraph::clear_visits(node);
            }
        }

        static std::string prettyprint_ext_sym_set(ext_sym_set s)
        {
            std::string result;
            
            for(ext_sym_set::iterator it = s.begin(); it != s.end(); ++it)
            {
                if (it->get_nodecl().is_null())
                {
                    result += it->get_name() + ", ";
                }
                else
                {
                    std::string nodecl_string(codegen_to_str(it->get_nodecl().get_internal_nodecl()));
                    result += nodecl_string + ", ";                
                }
            }
            
            result = result.substr(0, result.size()-2);
            
            return result;
        }

        void StaticAnalysis::solve_live_equations_recursive(Node* actual, bool& changed)
        {
            while (!actual->is_visited())
            {
                actual->set_visited(true);
                
                Node_type ntype = actual->get_type();
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
                        Node* entry_node = actual->get_graph_entry_node();
                        solve_live_equations_recursive(entry_node, changed);
                        
                        // spread the liveness inside the node to the Graph node
                        actual->set_graph_node_liveness();
                    }
                    else
                    {
                        
                        if (ntype != BASIC_ENTRY_NODE)
                        {
                            ext_sym_set old_live_in = actual->get_live_in_vars();
                            ext_sym_set old_live_out = actual->get_live_out_vars();
                            ext_sym_set live_out, live_in, aux_live_in, aux;
                            
                            // Computing Live out
                            for(ObjectList<Node*>::iterator it = children.begin();it != children.end();++it)
                            {
                                Node_type nt = (*it)->get_type();
                                /*if (nt == GRAPH_NODE)
                                {
                                    ObjectList<Node*> inner_children = (*it)->get_graph_entry_node()->get_children();
                                    for(ObjectList<Node*>::iterator itic = inner_children.begin();
                                        itic != inner_children.end();
                                        ++itic)
                                    {
                                        ext_sym_set current_live_in = (*itic)->get_live_in_vars();
                                        for (ext_sym_set::iterator itli = current_live_in.begin(); itli != current_live_in.end(); ++itli)
                                        {
                                            if ((*it)->get_scope().is_valid())
                                                if (!itli->get_symbol().get_scope().scope_is_enclosed_by((*it)->get_scope()))
                                                    aux_live_in.insert(*itli);
                                            else
                                                aux_live_in.insert(*itli);
                                        }
                                    }
                                }
                                else */if (nt == BASIC_EXIT_NODE)
                                {
                                    ObjectList<Node*> outer_children = (*it)->get_outer_node()->get_children();
                                    for(ObjectList<Node*>::iterator itoc = outer_children.begin();
                                        itoc != outer_children.end();
                                        ++itoc)
                                    {
                                        ext_sym_set outer_live_in = (*itoc)->get_live_in_vars();
                                        for (ext_sym_set::iterator itli = outer_live_in.begin(); itli != outer_live_in.end(); ++itli)
                                        {
                                            aux_live_in.insert(*itli);
                                        }
                                    }
                                }
                                else
                                {
                                    aux_live_in = (*it)->get_live_in_vars();
                                }
                                live_out.insert(aux_live_in);
                            }
                           
                            // Computing Live In
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
                    
                    for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    {
                        solve_live_equations_recursive(*it, changed);
                    }
                    continue;
                }
                return;
            }
        }
        
        // FIXME For the moment we assume the user has used the 'auto-deps' clause
        /*!
         * Input values al those which are Live in and are not Live out
         * Output values al those which are Live out and are not Live in
         * Inout values al those which are Live in and Live out
         */
        void StaticAnalysis::analyse_task(Node* task_node)
        {            
            // Scope variables as Private, Firstprivate, Shared or UndefinedScope
            compute_auto_scoping(task_node);
            
            // Specify Shared Variables into Input, Output and Inout
            ext_sym_set in_vars = task_node->get_live_in_vars();
            ext_sym_set out_vars = task_node->get_live_out_vars();
            ext_sym_set input_deps, output_deps, inout_deps;
            ext_sym_set shared_vars = task_node->get_shared_vars();
            for (ext_sym_set::iterator it = shared_vars.begin(); it != shared_vars.end(); ++it)
            {
                if (ext_sym_set_contains_englobing_nodecl(*it, in_vars))
                {
                    if (ext_sym_set_contains_englobing_nodecl(*it, out_vars))
                    {
                        inout_deps.insert(*it);
                    }
                    else
                    {
                        input_deps.insert(*it);
                    }
                }
                else if (ext_sym_set_contains_englobing_nodecl(*it, out_vars))
                {
                    output_deps.insert(*it);
                }
                else
                {
                    std::cerr << "warning: variable " << it->get_nodecl().prettyprint() 
                              << " computed as shared but it is not LiveIn nor LiveOut" << std::endl;
                }
            }
            
            task_node->set_input_deps(input_deps);
            task_node->set_output_deps(output_deps);
            task_node->set_inout_deps(inout_deps);
            task_node->set_undef_deps(task_node->get_undef_sc_vars());
            
            task_node->set_deps_computed();
        }
        
        void StaticAnalysis::analyse_tasks_rec(Node* current)
        {
            if (!current->is_visited())
            {
                current->set_visited(true);
                if (current->get_type() == GRAPH_NODE)
                {
                    if (current->get_graph_type() == TASK)
                    {
                        if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                            CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                            std::cerr << std::endl << "   ==> Task '" << current->get_graph_label().prettyprint() << "'" << std::endl;
                                
                        StaticAnalysis::analyse_task(current);

                        if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                            CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                        {
                            current->print_use_def_chains();
                            current->print_liveness();
                            current->print_task_dependencies();
                        }
                    }
                    else
                    {
                        analyse_tasks_rec(current->get_graph_entry_node());
                    }
                }
                    
                ObjectList<Node*> children = current->get_children();
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    analyse_tasks_rec(*it);
            }
        }
        
        void StaticAnalysis::analyse_tasks(Node* graph_node)
        {
            analyse_tasks_rec(graph_node);
            ExtensibleGraph::clear_visits(graph_node);
        }
        
        /*!
         * Algorithm:
         * 1.- Determine task scheduling point (1) and next synchronization points (2, 3, ..., n)
         *    - '1' is the node/s parent/s from the task  ->  init_point
         *    - '2' is first taskwait or barrier founded after '1'  ->  end_point
         * 2.- For each scalar variable 'v' appearing within the task:
         *    - if 'v' is dead after '1':
         *        - if the first action performed in 'v' is a write  =>  PRIVATE
         *        - if the first action performed in 'v' is a read  =>  FIRSTPRIVATE
         *    - if 'v' is live between '1' and 'n':
         *        - if 'v' is only read between '1' and 'n' and within the task  =>  FIRSTPRIVATE
         *        - if 'v' is written in some point between '1' and 'n' and/or within the task:
         *              - if there exist race condition  =>  UNDEFINED SCOPING
         *              - if there is no race condition  =>  SHARED
         * 3.- For each array variable appearing within the task defined the scoping for the whole array as follows:
         *    - Apply the algorithm above for every use of the array within the task
         *          - if the whole array or the used regions of the array have the same scope  =>  propagate scope
         *          - if there are different scoping for different regions of the array:
         *              - if some part has an UNDEFINED SCOPING  =>  UNDEFINED SCOPING
         *              - if at least one part is FIRSTPRIVATE and the rest is PRIVATE  =>  FIRSTPRIVATE
         *              - if at least one part is SHARED and the rest is PRIVATE/FIRSTPRIVATE  =>  SHARED???  (sequential consistency rules)
         * NOTE: There exist race condition when more than one thread can access to the same variable at the same time 
         * and at least one of the accesses is a write. 
         * NOTE: 'atomic' and 'critical' constructs affect in race condition determining. 
         * NOTE: 'ordered' construct affects in determining variables as 'private' or 'firstprivate'
         */
        void StaticAnalysis::compute_auto_scoping(Node* task)
        {
            ObjectList<Node*> end_point = task->get_children();
            if (end_point.size() != 1)
            {    
                internal_error("The end point of a task should be one unique node representing a 'taskwait', a 'barrier' or the 'exit' of a graph. "\
                               "But task '%s' has more than one exit", task->get_graph_label().prettyprint().c_str());
            }    
            if (end_point[0]->get_type() == BASIC_EXIT_NODE)
            {   // If it is an 'exit' we don't know anything (FIXME: we can know things)
                
            }
            else
            {
                task->set_undef_sc_var(task->get_undefined_behaviour_vars());
                ext_sym_set scoped_vars;
                compute_auto_scoping_rec(task, task->get_graph_entry_node(), scoped_vars);
            }
        }
    
    
        void StaticAnalysis::compute_auto_scoping_rec(Node* task, Node* current, ext_sym_set& scoped_vars)
        {
            Node_type ntype = current->get_type();
            if (ntype == GRAPH_NODE)
            {
                compute_auto_scoping_rec(task, current->get_graph_entry_node(), scoped_vars);
            }
            else if (current->has_key(_NODE_STMTS))
            {
                ext_sym_set undef = task->get_undefined_behaviour_vars();
                
                ext_sym_set ue = current->get_ue_vars();
                for (ext_sym_set::iterator it = ue.begin(); it != ue.end(); ++it)
                    if (!ext_sym_set_contains_englobing_nodecl(*it, undef))
                        scope_variable(task, *it, scoped_vars);
                
                ext_sym_set killed = current->get_killed_vars();
                for (ext_sym_set::iterator it = killed.begin(); it != killed.end(); ++it)
                    if (!ext_sym_set_contains_englobing_nodecl(*it, undef))
                        scope_variable(task, *it, scoped_vars);
            }
            
            ObjectList<Node*> children = current->get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                compute_auto_scoping_rec(task, *it, scoped_vars);
            }

        }

        void StaticAnalysis::scope_variable(Node* task, ExtensibleSymbol ei, ext_sym_set& scoped_vars)
        {
            if (!ext_sym_set_contains_englobing_nodecl(ei, scoped_vars) 
                && !ei.get_symbol().get_scope().scope_is_enclosed_by(task->get_scope()))
            {
                scoped_vars.insert(ei);
                
                char usage;
                if ((usage = var_is_used_out_task(task, ei)) != '0')
                {   // The variable is used out of the task
                    if (usage == '1')
                    {   // The variable is only read
                        task->set_firstprivate_var(ei);
                    }
                    else
                    {   // The variable is written (it can also be read)
                        if (race_condition(task, ei))
                        {
                            task->set_undef_sc_var(ei);
                        }
                        else
                        {
                            task->set_shared_var(ei);
                        }
                    }
                }
                else
                {   // The variable is not used out of the task
                    if (ext_sym_set_contains_englobing_nodecl(ei, task->get_children()[0]->get_live_in_vars()))
                    {   // The variable is used out after the task
                        task->set_shared_var(ei);
                    }
                    else
                    {   // The variable is not used after the task
                        if (ext_sym_set_contains_englobing_nodecl(ei, task->get_ue_vars()))
                        {   // It is first read
                            task->set_firstprivate_var(ei);
                        }
                        else
                        {   // It is first written 
                            task->set_private_var(ei);
                        }
                    }
                }
            }
        }

        char StaticAnalysis::var_is_used_out_task(Node* task, ExtensibleSymbol ei)
        {
            char res = '0';
            
            // TODO
            
            return res;
        }

        bool StaticAnalysis::race_condition(Node* task, ExtensibleSymbol ei)
        {
            bool res = false;
            
            // TODO
            
            return res;
        }

        // *** CFG_VISITOR *** //
        
        static bool nodecl_is_base_of_symbol(Symbol s, Nodecl::NodeclBase n)
        {
            if (n.is<Nodecl::Symbol>())
            {
                if (n.get_symbol() == s)
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
                if (n.is<Nodecl::Derreference>())
                {
                    Nodecl::Derreference n_ = n.as<Nodecl::Derreference>();
                    return nodecl_is_base_of_symbol(s, n_.get_rhs());
                }
                else if (n.is<Nodecl::ClassMemberAccess>())
                {
                    Nodecl::ClassMemberAccess n_ = n.as<Nodecl::ClassMemberAccess>();
                    return nodecl_is_base_of_symbol(s, n_.get_lhs());
                }
                else if (n.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript n_ = n.as<Nodecl::ArraySubscript>();
                    return nodecl_is_base_of_symbol(s, n_.get_subscripted());
                }
                else
                {
                    internal_error("Unexpected type of node '%s' while looking for base symbol in an extensible symbol", 
                                ast_print_node_type(n.get_kind()));
                }
            }
        }
        
        static bool nodecl_is_base_of_symbol(ObjectList<Symbol> s_l, Nodecl::NodeclBase n)
        {
            for (ObjectList<Symbol>::iterator it = s_l.begin(); it != s_l.end(); ++it)
            {
                if (nodecl_is_base_of_symbol(*it, n))
                {
                    return true;
                }
            }
            return false;
        }
        
        static Symbol get_symbol_of_lvalue(Nodecl::NodeclBase n)
        {
            while (!n.is<Nodecl::Symbol>())
            {
                if (n.is<Nodecl::ClassMemberAccess>())
                {
                    Nodecl::ClassMemberAccess n_ = n.as<Nodecl::ClassMemberAccess>();
                    get_symbol_of_lvalue(n_.get_member());
                }
                else if (n.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript n_ = n.as<Nodecl::ArraySubscript>();
                    get_symbol_of_lvalue(n_.get_subscripted());
                }
                else if (n.is<Nodecl::FunctionCall>())
                {
                    return Symbol();
                }
                else
                {
                    internal_error("Unexpected type of node '%s' while looking for the symbol in an lvalue Nodecl",
                                    ast_print_node_type(n.get_kind()));
                }
            }
            
        }

        static bool nodecl_is_constant_value(Nodecl::NodeclBase n)
        {
            if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() 
                || n.is<Nodecl::ComplexLiteral>() || n.is<Nodecl::BooleanLiteral>()
                || n.is<Nodecl::StringLiteral>())
            {
                return true;
            }
            return false;
        }

        /*!
         * This method looks for the argument corresponding to a parameter which matches with a given nodecl
         * \param n   Nodecl being used/defined which has been stored in an ExtensibleSymbol                  -> a.b.c
         * \param s   Symbol containing a parameter of the function where de nodecl has being used/defined    -> A& a
         * \param arg Nodecl containing the argument corresponding to de parameter                        -> a' of type A&
         * \return copy of the argument 'arg' corresponding to the parameter 'n' when this parameter is the symbol 's'
         */
        static Nodecl::NodeclBase match_nodecl_with_symbol(Nodecl::NodeclBase n, Symbol s, Nodecl::NodeclBase arg)
        {
            if (!nodecl_is_constant_value(arg))
            {
                Type arg_type = arg.get_type();
                const char * arg_file = arg.get_filename().c_str();
                int arg_line = arg.get_line();
                
                if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() || n.is<Nodecl::ComplexLiteral>()
                    || n.is<Nodecl::StringLiteral>() || n.is<Nodecl::BooleanLiteral>())
                {}   // When the nodecl is a literal, no symbol is involved
                else if (n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>())
                {
                    if (n.get_symbol() == s)
                    {
                        return arg.copy();
                    }
                }
                else if (n.is<Nodecl::ClassMemberAccess>())
                {
                    Nodecl::ClassMemberAccess aux = n.as<Nodecl::ClassMemberAccess>();
                    Nodecl::NodeclBase lhs = match_nodecl_with_symbol(aux.get_lhs(), s, arg);
                    if (!lhs.is_null())
                    {
                        return Nodecl::ClassMemberAccess::make(lhs, aux.get_member(), arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript aux = n.as<Nodecl::ArraySubscript>();
                    Nodecl::NodeclBase subscripted = match_nodecl_with_symbol(aux.get_subscripted(), s, arg);
                    if (!subscripted.is_null())
                    {
                        return Nodecl::ArraySubscript::make(subscripted, aux.get_subscripts(), arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Reference>())
                {
                    Nodecl::Reference aux = n.as<Nodecl::Reference>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Reference::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Derreference>())
                {
                    Nodecl::Derreference aux = n.as<Nodecl::Derreference>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Derreference::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Conversion>())
                {
                    Nodecl::Conversion aux = n.as<Nodecl::Conversion>();
                    Nodecl::NodeclBase nest = match_nodecl_with_symbol(aux.get_nest(), s, arg);
                    if (!nest.is_null())
                    {
                        return Nodecl::Conversion::make(nest, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Cast>())
                {
                    Nodecl::Cast aux = n.as<Nodecl::Cast>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Conversion::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                // While checking parameters, we can have many types of "Extensible symbols"
                else if (n.is<Nodecl::Minus>())
                {
                    Nodecl::Minus aux = n.as<Nodecl::Minus>();
                    Nodecl::NodeclBase lhs = match_nodecl_with_symbol(aux.get_lhs(), s, arg);
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!lhs.is_null() || !rhs.is_null())
                    {
                        return Nodecl::Minus::make(lhs, rhs, arg_type, arg_file, arg_line);
                    }                
                }
                else if (n.is<Nodecl::Preincrement>())
                {
                    Nodecl::Preincrement aux = n.as<Nodecl::Preincrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Preincrement::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Postincrement>())
                {
                    Nodecl::Postincrement aux = n.as<Nodecl::Postincrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Postincrement::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Predecrement>())
                {
                    Nodecl::Predecrement aux = n.as<Nodecl::Predecrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Predecrement::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Postdecrement>())
                {
                    Nodecl::Postdecrement aux = n.as<Nodecl::Postdecrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Postdecrement::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else
                {
                    internal_error("Node type '%s' in node '%s' not yet implemented while parsing an Extensible symbol matching",
                                   ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
                }
            }

            return Nodecl::NodeclBase::null();
        }
    
        /*!
        * This method searches a nodecl corresponding to a parameter when the corresponding argument is not a constant value
        * \param n                Nodecl we are looking for
        * \param params           List of parameters
        * \param args             List of arguments
        * \param matching_symbol  Symbol in the parameter list which matches 
        * \return Copy of the symbol, if matched, otherwise, null nodecl
        */
        static Nodecl::NodeclBase match_nodecl_in_symbol_l(Nodecl::NodeclBase n, ObjectList<Symbol> params, 
                                                           ObjectList<Nodecl::NodeclBase> args, Symbol& matching_symbol)
        {
            ObjectList<Symbol>::iterator itp = params.begin();
            ObjectList<Nodecl::NodeclBase>::iterator ita = args.begin();
            Nodecl::NodeclBase actual_nodecl;
            for (; (itp != params.end()) && (ita != args.end()); ++itp, ++ita)
            {
                actual_nodecl = match_nodecl_with_symbol(n, *itp, *ita);
                if (!actual_nodecl.is_null())
                {
                    matching_symbol = *itp;
                    break;
                }
            }
            
            return actual_nodecl;
        }
        
        //! Set the variable in #n to UE or KILLED list
        static void set_usage_to_symbol_in_node(Node* node, struct var_usage_t* n, ExtensibleGraph* called_func_graph)
        {
            char usage = n->get_usage();
            if (usage == '0')
            {
                ExtensibleSymbol ue_var(n->get_nodecl());
                node->set_ue_var(ue_var);
            }
            else if (usage == '1')
            {
                ExtensibleSymbol killed_var(n->get_nodecl());
                node->set_killed_var(killed_var);
            }
            else if (usage == '2')
            {
                ExtensibleSymbol ue_killed_var(n->get_nodecl());
                node->set_ue_var(ue_killed_var);
                node->set_killed_var(ue_killed_var);
            }
            else
            {
                internal_error("Unexpected usage value '%c' while computing global variable usage in function ", 
                            usage, called_func_graph->get_function_symbol().get_name().c_str());
            }
        }
        
        bool CfgVisitor::func_has_cyclic_calls_rec(Symbol reach_func, Symbol stop_func, ExtensibleGraph * graph)
        {
            ObjectList<Symbol> called_syms = graph->get_function_calls();
           
            for (ObjectList<Symbol>::iterator it = called_syms.begin(); it != called_syms.end(); ++it)
            {
                ExtensibleGraph* called_func_graph = find_function_for_ipa(*it, _cfgs);
                if (called_func_graph != NULL)
                {
                    if (called_func_graph->get_function_symbol() == reach_func)
                    {
                        return true;
                    }
                    else
                    {
                        if (stop_func != *it)
                            if (func_has_cyclic_calls(reach_func, called_func_graph))
                                return true;
                    }
                }
            }
            
            return false;
        }
        
        bool CfgVisitor::func_has_cyclic_calls(Symbol reach_func, ExtensibleGraph * graph)
        {
            return func_has_cyclic_calls_rec(reach_func, graph->get_function_symbol(), graph);
        }

        void CfgVisitor::set_live_initial_information(Node* node)
        {
            if (node->get_type() == BASIC_FUNCTION_CALL_NODE)
            {
                ExtensibleGraph* called_func_graph = find_function_for_ipa(node->get_function_node_symbol(), _cfgs);
                Nodecl::NodeclBase func_call = /*Function call*/ node->get_statements()[0];
                if (called_func_graph != NULL)
                {
                    // Create a map between the parameters of the called function and the current arguments of the call
                    std::map<Symbol, Nodecl::NodeclBase> ref_params_to_args = map_reference_params_to_args(func_call, called_func_graph);
                        
                    Symbol function_sym = called_func_graph->get_function_symbol();
                    if (func_has_cyclic_calls(function_sym, called_func_graph))
                    {   // Recursive analysis
                        ext_sym_set ue_vars, killed_vars, undef_vars;
                        
                        // Analyse reference parameters and global variables
                        ObjectList<var_usage_t*> glob_vars = called_func_graph->get_global_variables();
                        if (!glob_vars.empty() || !ref_params_to_args.empty())
                        {
                            // Compute liveness for global variables and parameters
                            ObjectList<Symbol> ref_params = get_reference_params(called_func_graph);
                            CfgIPAVisitor ipa_visitor(called_func_graph, _cfgs, glob_vars, ref_params, ref_params_to_args);
                            ipa_visitor.compute_usage();
                            
                            // Propagate this information to the current graph analysis
                            ObjectList<struct var_usage_t*> ipa_usage = ipa_visitor.get_usage();
                            for (ObjectList<struct var_usage_t*>::iterator it = ipa_usage.begin(); it != ipa_usage.end(); ++it)
                            {
                                char usage = (*it)->get_usage();
                                Nodecl::NodeclBase s = (*it)->get_nodecl();
                                if (usage == '0')
                                {
                                    killed_vars.insert(ExtensibleSymbol(s));
                                }
                                else if (usage == '1')
                                {
                                    ue_vars.insert(ExtensibleSymbol(s));
                                }
                                else if (usage == '2')
                                {
                                    killed_vars.insert(ExtensibleSymbol(s));
                                    ue_vars.insert(ExtensibleSymbol(s));
                                }
                                else if (usage == '3')
                                {
                                    undef_vars.insert(ExtensibleSymbol(s));
                                }
                                else
                                {
                                    internal_error("Undefined usage %s for symbol %s\n", usage, s.prettyprint().c_str())
                                }
                            }
                        }
                        
                        // Non-reference parameters are always used
                        ObjectList<Nodecl::NodeclBase> non_ref_args = get_non_reference_args(func_call, called_func_graph);
                        for (ObjectList<Nodecl::NodeclBase>::iterator it = non_ref_args.begin(); it != non_ref_args.end(); ++it)
                        {
                            SymbolVisitor sv;
                            sv.walk(*it);
                            ObjectList<Nodecl::Symbol> arg_syms = sv.get_symbols();
                            for (ObjectList<Nodecl::Symbol>::iterator its = arg_syms.begin(); its != arg_syms.end(); ++its)
                                ue_vars.insert(ExtensibleSymbol(*its));
                        }
                        
                        node->set_ue_var(ue_vars);
                        node->set_killed_var(killed_vars);
                        node->set_undefined_behaviour_var(undef_vars);
                    }
                    else
                    {
                        if (!_visited_functions.contains(called_func_graph->get_function_symbol()))
                        {
                            _visited_functions.insert(called_func_graph->get_function_symbol());
                            
                            // Compute the Use-Def information of the called function, if necessary
                            if (called_func_graph->has_use_def_computed() == '0')
                            {
                                ExtensibleGraph* actual_cfg = _actual_cfg;
                                _actual_cfg = called_func_graph;
                               
                                compute_use_def_chains(called_func_graph->get_graph());
                                if (called_func_graph->has_use_def_computed() == '2')
                                    _actual_cfg->set_use_def_computed('2');
                                else
                                    called_func_graph->set_use_def_computed('1');
                                
                                _actual_cfg = actual_cfg;
                            }
                            
                            // Filter map keeping those arguments that are constants for "constant propagation" in USE-DEF info
                            std::map<Symbol, Nodecl::NodeclBase> const_args;
                            for(std::map<Symbol, Nodecl::NodeclBase>::iterator it = ref_params_to_args.begin(); it != ref_params_to_args.end(); ++it)
                            {
                                if (it->second.is_constant())
                                {
                                    const_args[it->first] = it->second;
                                }
                            }
                            
                            // Propagate use-def chains of the function call to the current graph
                            ObjectList<Symbol> ref_params;
                            ObjectList<Nodecl::NodeclBase> ref_args = get_reference_params_and_args(func_call, called_func_graph, ref_params);

                            ext_sym_set called_func_ue_vars = called_func_graph->get_graph()->get_ue_vars();
                            ext_sym_set node_ue_vars;
                            for(ext_sym_set::iterator it = called_func_ue_vars.begin(); it != called_func_ue_vars.end(); ++it)
                            {
                                Symbol s(NULL);
                                Nodecl::NodeclBase ue_arg = match_nodecl_in_symbol_l(it->get_nodecl(), ref_params, ref_args, s);
                                if ( !ue_arg.is_null() )
                                {   // UE variable is a parameter or a part of a parameter
                                    if (ue_arg.is<Nodecl::Symbol>() || ue_arg.is<Nodecl::Cast>() || ue_arg.is<Nodecl::ClassMemberAccess>()
                                        || ue_arg.is<Nodecl::Reference>() || ue_arg.is<Nodecl::Derreference>() 
                                        || ue_arg.is<Nodecl::ArraySubscript>()) 
                                    {
                                        ExtensibleSymbol ei(ue_arg);
                                        ei.propagate_constant_values(const_args);
                                        node_ue_vars.insert(ei);
                                    }
                                    else if (ue_arg.is<Nodecl::FunctionCall>() || ue_arg.is<Nodecl::VirtualFunctionCall>())
                                    {}  // Nothing to do, we don't need to propagate the usage of a temporal value
                                    else
                                    {
                                        SymbolVisitor sv;
                                        sv.walk(ue_arg);
                                        ObjectList<Nodecl::Symbol> nodecl_symbols = sv.get_symbols();
                                        for (ObjectList<Nodecl::Symbol>::iterator its = nodecl_symbols.begin(); 
                                                its != nodecl_symbols.end(); ++its)
                                        {
                                            ExtensibleSymbol ei(*its);
                                            ei.propagate_constant_values(const_args);
                                            node_ue_vars.insert(ei);
                                        }
                                    }
                                }
                                else
                                {
                                    /*Nodecl::NodeclBase ue = it->get_nodecl();
                                    if (ue.is<Nodecl::BooleanLiteral>() || ue.is<Nodecl::StringLiteral>() 
                                        || ue.is<Nodecl::IntegerLiteral>() || ue.is<Nodecl::FloatingLiteral>() || ue.is<Nodecl::ComplexLiteral>() )
                                    { }     // Nothing to do with a constant argument
                                    
                                    else */if ( !it->get_symbol().get_scope().scope_is_enclosed_by(function_sym.get_scope()) 
                                             && it->get_symbol().get_scope() != function_sym.get_scope() )
                                    {   // UE variable is global
                                    node_ue_vars.insert(*it);
                                    }
                                }
                            }
                            // For all parameters passed by value, the corresponding arguments are used in the current function call node
                            ObjectList<Nodecl::NodeclBase> non_ref_args = get_non_reference_args(func_call, called_func_graph);
                            for (ObjectList<Nodecl::NodeclBase>::iterator it = non_ref_args.begin(); it != non_ref_args.end(); ++it)
                            {
                                if (!it->is<Nodecl::BooleanLiteral>() && !it->is<Nodecl::StringLiteral>() 
                                    && !it->is<Nodecl::IntegerLiteral>() && !it->is<Nodecl::FloatingLiteral>() && !it->is<Nodecl::ComplexLiteral>())
                                    node_ue_vars.insert(ExtensibleSymbol(*it));
                            }
                        
                            ext_sym_set called_func_killed_vars = called_func_graph->get_graph()->get_killed_vars();
                            ext_sym_set node_killed_vars;
                            for(ext_sym_set::iterator it = called_func_killed_vars.begin(); it != called_func_killed_vars.end(); ++it)
                            {
                                Symbol s(NULL);
                                Nodecl::NodeclBase killed_arg = match_nodecl_in_symbol_l(it->get_nodecl(), ref_params, ref_args, s);
                                if ( !killed_arg.is_null() )
                                {   // KILLED variable is a parameter or a part of a parameter
                                    ExtensibleSymbol ei(killed_arg);
                                    ei.propagate_constant_values(const_args);
                                    // Only reference parameters can be killed, the rest are only used to make a temporary copy
                                    if (ei.get_symbol().get_type().is_pointer())
                                    {   // FIXME We must go in this case if the Parameter is passed by reference and 
                                        // if the argument is not a temporary value
                                        // decl_context_t param_context =
                                        //        function_sym.get_internal_symbol()->entity_specs.related_symbols[0]->decl_context;
                                        // if (s.get_scope() != Scope(param_context))
                                        node_killed_vars.insert(ei);
                                    }
                                    else
                                    {
                                        node_ue_vars.insert(ei);
                                    }
                                }
                                else if ( !it->get_symbol().get_scope().scope_is_enclosed_by(function_sym.get_scope()) 
                                        && it->get_symbol().get_scope() != function_sym.get_scope() )
                                {   // KILLED variable is global
                                    node_killed_vars.insert(it->get_nodecl());
                                }
                            }

                            ext_sym_set called_func_undef_vars = called_func_graph->get_graph()->get_undefined_behaviour_vars();
                            // FIXME We should delete from this list those parameters passed by value and add them to the UE_list
                            
                            node->set_ue_var(node_ue_vars);
                            node->set_killed_var(node_killed_vars);
                            node->set_undefined_behaviour_var(called_func_undef_vars);
                        }
                        else
                        {}  // We already have analysed this function and propagated its Use-Def info to the current graph
                    }
                }
                else
                {
                    _actual_cfg->set_use_def_computed('2');
                   
                    // Set undefined_behaviour to all parameters in the function call
                    ext_sym_set undef_behaviour_vars, ue_vars;
                    Nodecl::List args = get_func_call_args(node->get_statements()[0]);
                   
                    for(Nodecl::List::iterator it = args.begin(); it != args.end(); ++it)
                    {
                        if (it->is<Nodecl::Symbol>() || it->is<Nodecl::ClassMemberAccess>() || it->is<Nodecl::ArraySubscript>()
                            || it->is<Nodecl::Reference>() || it->is<Nodecl::Derreference>()
                            || it->is<Nodecl::Conversion>() || it->is<Nodecl::Cast>())
                        {
                            ExtensibleSymbol ei(*it);
                            undef_behaviour_vars.insert(ei);
                        }
                        else if (it->is<Nodecl::FunctionCall>() || it->is<Nodecl::VirtualFunctionCall>())
                        {}  // Nothing to do, we don't need to propagate the usage of a temporal value
                        else
                        {   // FIXME We can define a variable here passing as argument "(n = 3)"
                            SymbolVisitor sv;
                            sv.walk(*it);
                            ObjectList<Nodecl::Symbol> syms_in_arg = sv.get_symbols();
                            for (ObjectList<Nodecl::Symbol>::iterator it = syms_in_arg.begin(); it != syms_in_arg.end(); ++it)
                                ue_vars.insert(ExtensibleSymbol(*it));
                        }
                    }
                    
                    node->set_ue_var(ue_vars);
                    node->set_undefined_behaviour_var(undef_behaviour_vars);
                    
                    // Se undefined behaviour to the global variables appearing in the current cfg, only if their usage was not 'killed'
                    // FIXME Here we should take into account any global variable, 
                    // but from now we only have access to the global variables appearing in the current cfg
                    ObjectList<struct var_usage_t*> current_global_vars = _actual_cfg->get_global_variables();
                    for (ObjectList<var_usage_t*>::iterator it = current_global_vars.begin(); it != current_global_vars.end(); ++it)
                    {
                        char usage = (*it)->get_usage();
                        if (usage != '0' && usage != '2')
                          (*it)->set_usage('3');
                    }
                }
            }
            else
            {
                ObjectList<Nodecl::NodeclBase> basic_block = node->get_statements();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = basic_block.begin();
                        it != basic_block.end();
                        ++it)
                {
                    CfgAnalysisVisitor cfg_analysis_visitor(node);
                    cfg_analysis_visitor.walk(*it);
                }
            }
        }
        
        void CfgVisitor::gather_live_initial_information(Node* node)
        {
            if (!node->is_visited())
            {
                node->set_visited(true);

                Node_type ntype = node->get_type();
                if (ntype != BASIC_EXIT_NODE)
                {
                    if (ntype == GRAPH_NODE)
                    {
                        Node* entry = node->get_graph_entry_node();
                        gather_live_initial_information(entry);
                        ExtensibleGraph::clear_visits_in_level(entry, node);
                        node->set_visited(false);
                        node->set_graph_node_use_def();
                        node->set_visited(true);
                    }
                    else if (ntype != BASIC_ENTRY_NODE)
                    {
                        set_live_initial_information(node);
                    }
                        
                    ObjectList<Edge*> exit_edges = node->get_exit_edges();
                    for (ObjectList<Edge*>::iterator it = exit_edges.begin();
                            it != exit_edges.end();
                            ++it)
                    {
                        gather_live_initial_information((*it)->get_target());
                    }
                }
                return;
            }
        }    
        
        void CfgVisitor::compute_use_def_chains(Node* node)
        {
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose)
                std::cerr << "  ==> Graph '" << _actual_cfg->get_name() << "'" << std::endl;
            gather_live_initial_information(node);
            ExtensibleGraph::clear_visits(node);
            _visited_functions.clear();
        }
        
        void CfgVisitor::analyse_loops(Node* node)
        {
            LoopAnalysis loop_analysis;
            loop_analysis.analyse_loops(node);
            
//             DEBUG_CODE()
            {
                std::cerr << "=== INDUCTION VARIABLES INFO AFTER LOOP ANALYSIS ===" << std::endl;
                loop_analysis.print_induction_vars_info();
            }
            
            StaticAnalysis static_analysis(&loop_analysis);
            static_analysis.extend_reaching_definitions_info(node);
            ExtensibleGraph::clear_visits(node);
        }
    }
}
