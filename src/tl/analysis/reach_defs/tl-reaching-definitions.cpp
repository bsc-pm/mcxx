/*--------------------------------------------------------------------
(C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include "tl-analysis-utils.hpp"
#include "tl-reaching-definitions.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ************************** Class implementing reaching definition analysis ************************* //

    ReachingDefinitions::ReachingDefinitions(ExtensibleGraph* graph)
        : _graph(graph), _first_stmt_node(NULL)
    {}

    void ReachingDefinitions::compute_reaching_definitions()
    {
        Node* graph = _graph->get_graph();

        // Set a fictitious reaching definition for each parameter 
        generate_unknown_reaching_definitions();
        
        // Compute initial info (liveness only regarding the current node)
        gather_reaching_definitions_initial_information(graph);
        ExtensibleGraph::clear_visits(graph);

        // Common Reaching Definitions analysis
        solve_reaching_definition_equations(graph);
        ExtensibleGraph::clear_visits(graph);
    }

    // Each parameter generates an unknow definition
    void ReachingDefinitions::generate_unknown_reaching_definitions()
    {
        Symbol func_sym = _graph->get_function_symbol();
        if (!func_sym.is_valid())
            return;

        NodeclMap unknown_reach_defs;
        ObjectList<Symbol> params = func_sym.get_function_parameters();
        for (ObjectList<Symbol>::iterator it = params.begin(); it != params.end(); ++it)
        {
            Nodecl::Symbol s = Nodecl::Symbol::make(*it);
            s.set_type(it->get_type());
            unknown_reach_defs.insert(
                    std::pair<NBase, NodeclPair>(
                            s, NodeclPair(Nodecl::Unknown::make(), Nodecl::Unknown::make())));
        }

        // Get first node in code with statements
        Node* stmt_node = _graph->get_graph()->get_graph_entry_node();
        Node* graph_exit = _graph->get_graph()->get_graph_exit_node();
        while (!stmt_node->has_statements() && (stmt_node != graph_exit))
        {
            if (stmt_node->is_graph_node())
                stmt_node = stmt_node->get_graph_entry_node();
            else if (stmt_node->is_exit_node() && (stmt_node->get_outer_node()->get_id() != 0))
                stmt_node = stmt_node->get_outer_node()->get_children()[0];
            else
            {
                const ObjectList<Node*>& children = stmt_node->get_children();
                if (!children.empty())
                    stmt_node = children[0];
                else
                    break;
            }
        }

        // Set the unknown RD to the first node with statements
        if (stmt_node->has_statements())
        {
            _first_stmt_node = stmt_node;
            stmt_node->set_reaching_definitions_in(unknown_reach_defs);
        }
    }
    
    void ReachingDefinitions::gather_reaching_definitions_initial_information(Node* current)
    {
        if (current->is_visited())
            return;

        current->set_visited(true);

        if (current->is_exit_node())
            return;

        if (current->is_graph_node())
        {
            Node* entry = current->get_graph_entry_node();
            gather_reaching_definitions_initial_information(entry);
            set_graph_node_reaching_definitions(current);
        }
        else if (!current->is_entry_node())
        {
            GeneratedStatementsVisitor rdv;
            NodeclList stmts = current->get_statements();
            for(NodeclList::iterator it = stmts.begin(); it != stmts.end(); ++it)
            {
                rdv.walk(*it);
            }
            current->set_generated_stmts(rdv.get_gen());
        }

        ObjectList<Node*> exits = current->get_children();
        for (ObjectList<Node*>::iterator it = exits.begin(); it != exits.end(); ++it)
        {
            gather_reaching_definitions_initial_information(*it);
        }
    }

    void ReachingDefinitions::solve_reaching_definition_equations(Node* current)
    {
        bool changed = true;
        while (changed)
        {
            changed = false;
            solve_reaching_definition_equations_rec(current, changed);
            ExtensibleGraph::clear_visits(current);
        }
    }

    void ReachingDefinitions::solve_reaching_definition_equations_rec(Node* current, bool& changed)
    {
        if (current->is_visited())
            return;

        current->set_visited(true);

        if (current->is_exit_node())
            return;

        if (current->is_graph_node())
        {
            solve_reaching_definition_equations_rec(current->get_graph_entry_node(), changed);
            set_graph_node_reaching_definitions(current);
        }
        else if (!current->is_entry_node())
        {
            const NodeclMap& old_rd_in = current->get_reaching_definitions_in();
            const NodeclMap& old_rd_out = current->get_reaching_definitions_out();
            NodeclMap rd_out, rd_in, pred_rd_out;

            // Computing Reach Defs In
            // First node with statements may have RDI comming from the parameters
            if (current == _first_stmt_node)
                rd_in = current->get_reaching_definitions_in();
            const ObjectList<Node*>& parents = current->get_parents();
            for (ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
            {
                if ((*it)->is_entry_node())
                {
                    // Iterate over outer parents while we found an ENTRY node
                    // Gather all parents which are not entry nodes
                    ObjectList<Node*> non_entry_outer_parents;
                    std::stack<Node*> entries;
                    entries.push(*it);
                    while (!entries.empty())
                    {
                        Node* current_entry = entries.top();
                        entries.pop();
                        bool parent_is_entry = current_entry->is_entry_node();
                        Node* entry_outer_node = current_entry->get_outer_node();
                        ObjectList<Node*> outer_parents;
                        while (parent_is_entry)
                        {
                            outer_parents = entry_outer_node->get_parents();
                            if (outer_parents.empty())
                                break;
                            // Operate with the first parent of the list
                            parent_is_entry = outer_parents[0]->is_entry_node();
                            // Push the other parents to the stack, so they will be traversed later
                            if (outer_parents.size() > 1)
                            {
                                for (unsigned int i = 1; i < outer_parents.size(); ++i)
                                    entries.push(outer_parents[i]);
                            }
                            entry_outer_node = (parent_is_entry ? outer_parents[0]->get_outer_node() : NULL);
                        }
                        if (!outer_parents.empty())
                            non_entry_outer_parents.append(outer_parents[0]);
                    }
                    // Get the Reach Def Out of the current predecessors
                    for (ObjectList<Node*>::iterator itop = non_entry_outer_parents.begin();
                         itop != non_entry_outer_parents.end(); ++itop)
                    {
                        const NodeclMap& outer_rd_out = (*itop)->get_reaching_definitions_out();
                        pred_rd_out.insert(outer_rd_out.begin(), outer_rd_out.end());
                    }
                }
                else
                {
                    pred_rd_out = (*it)->get_reaching_definitions_out();
                }
                rd_in = Utils::nodecl_map_union(rd_in, pred_rd_out);
            }

            // Computing Reach Defs Out
            NodeclSet killed;
            if (current->is_omp_task_creation_node())
            {   // Variables from non-task children nodes do not count here
                Node* created_task = ExtensibleGraph::get_task_from_task_creation(current);
                ERROR_CONDITION(created_task==NULL,
                                "Task created by task creation node %d not found.\n",
                                current->get_id());
                const NodeclSet& task_killed = created_task->get_killed_vars();
                const NodeclSet& shared_vars = created_task->get_all_shared_accesses();
                for (NodeclSet::const_iterator it = task_killed.begin(); it != task_killed.end(); ++it)
                {
                    if (shared_vars.find(*it) != shared_vars.end())
                        killed.insert(*it);
                }
            }
            else
            {
                killed = current->get_killed_vars();
            }
            NodeclMap diff = Utils::nodecl_map_minus_nodecl_set(rd_in, killed);

            const NodeclMap& gen = current->get_generated_stmts();
            rd_out = Utils::nodecl_map_union(gen, diff);

            if (!Utils::nodecl_map_equivalence(old_rd_in, rd_in) ||
                !Utils::nodecl_map_equivalence(old_rd_out, rd_out))
            {
                current->set_reaching_definitions_in(rd_in);
                current->set_reaching_definitions_out(rd_out);
                changed = true;
            }
        }

        ObjectList<Node*> children = current->get_children();
        for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
        {
            solve_reaching_definition_equations_rec(*it, changed);
        }
    }

    void ReachingDefinitions::set_graph_node_reaching_definitions(Node* current)
    {
        if(current->is_graph_node())
        {
            // RDI(graph) = U RDI(inner entries)
            NodeclMap graph_rdi;
            ObjectList<Node*> entries = current->get_graph_entry_node()->get_children();
            for(ObjectList<Node*>::iterator it = entries.begin(); it != entries.end(); ++it)
            {
                if(!(*it)->is_labeled_node())
                {
                    graph_rdi = Utils::nodecl_map_union(graph_rdi, (*it)->get_reaching_definitions_in());
                }
                else
                {   // Remove those definitions coming from any goto to this labeled node
                    ObjectList<Node*> parents = (*it)->get_parents();
                    for(ObjectList<Node*>::iterator itt = entries.begin(); itt != entries.end(); ++itt)
                    {
                        if(!(*itt)->is_goto_node())
                        {
                            graph_rdi = Utils::nodecl_map_union(graph_rdi, (*it)->get_reaching_definitions_in());
                        }
                    }
                }
            }
            
            current->set_reaching_definitions_in(graph_rdi);

            // RDO(graph) = U RDO(inner exits)
            NodeclMap graph_rdo;
            ObjectList<Node*> exits = current->get_graph_exit_node()->get_parents();
            for(ObjectList<Node*>::iterator it = exits.begin(); it != exits.end(); ++it)
            {
                graph_rdo = Utils::nodecl_map_union(graph_rdo, (*it)->get_reaching_definitions_out());
            }
            if(graph_rdo.empty())
            {   // This may happen when no Reaching Defintion has been computed inside the graph or
                // when there is no statement inside the task and the information has not been propagated 
                // (Entry and Exit nodes do not contain any analysis information)
                // In this case, we propagate the Reaching Definition Out from the parents
                graph_rdo = graph_rdi;
            }
            current->set_reaching_definitions_out(graph_rdo);
        }
    }

    // *********************** End class implementing reaching definitions analysis *********************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ************************ Class implementing a visitor of reaching definition *********************** //

    GeneratedStatementsVisitor::GeneratedStatementsVisitor()
            : _gen()
    {}

    NodeclMap GeneratedStatementsVisitor::get_gen()
    {
        return _gen;
    }

    void GeneratedStatementsVisitor::visit_assignment(const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs, 
                                                      const Nodecl::NodeclBase& stmt)
    {
        if(_gen.find(lhs) != _gen.end())
        {   // Generated set only contains downwards exposed definitions
            // So, if there is a previous statement that generated a definition for the same variable
            // we remove it from the list
            _gen.erase(lhs);
        }
        _gen.insert(std::pair<NBase, NodeclPair>(lhs, NodeclPair(rhs, stmt)));
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::AddAssignment& n)
    {
        Nodecl::Add rhs = Nodecl::Add::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                             n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::ArithmeticShrAssignment& n)
    {
        Nodecl::ArithmeticShr rhs = Nodecl::ArithmeticShr::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                                                 n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::Assignment& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs(), Nodecl::NodeclBase::null());
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        Nodecl::BitwiseAnd rhs = Nodecl::BitwiseAnd::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                                           n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        Nodecl::BitwiseOr rhs = Nodecl::BitwiseOr::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                                         n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::BitwiseShlAssignment& n)
    {
        Nodecl::BitwiseShl rhs = Nodecl::BitwiseShl::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                                           n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::BitwiseShrAssignment& n)
    {
        Nodecl::BitwiseShr rhs = Nodecl::BitwiseShr::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                                           n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        Nodecl::BitwiseXor rhs = Nodecl::BitwiseXor::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                                           n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::DivAssignment& n)
    {
        Nodecl::Div rhs = Nodecl::Div::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                             n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::MinusAssignment& n)
    {
        Nodecl::Minus rhs = Nodecl::Minus::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                                 n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::ModAssignment& n)
    {
        Nodecl::Mod rhs = Nodecl::Mod::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                             n.get_type(),  n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::MulAssignment& n)
    {
        Nodecl::Mul rhs = Nodecl::Mul::make(n.get_lhs().shallow_copy(), n.get_rhs().shallow_copy(),
                                             n.get_type(), n.get_locus());
        visit_assignment(n.get_lhs(), rhs, n);
    }
    
    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::ObjectInit& n)
    {
        TL::Symbol lhs_sym = n.get_symbol();
        Nodecl::Symbol lhs = Nodecl::Symbol::make(lhs_sym, n.get_locus());
        NBase rhs = lhs_sym.get_value();
        
        // check for nested assignments
        walk(rhs);
        
        _gen.insert(std::pair<NBase, NodeclPair>(lhs, NodeclPair(rhs, NBase::null())));
    }
    
    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::Postdecrement& n)
    {
        Nodecl::IntegerLiteral one = Nodecl::IntegerLiteral::make(n.get_type(), const_value_get_one(/* bytes */ 4, /* signed */ 1),
                                                                   n.get_locus());
        Nodecl::Minus rhs = Nodecl::Minus::make(n.get_rhs().shallow_copy(), one , n.get_type(),
                                                 n.get_locus());
        visit_assignment(n.get_rhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::Postincrement& n)
    {
        Nodecl::IntegerLiteral one = Nodecl::IntegerLiteral::make(n.get_type(), const_value_get_one(/* bytes */ 4, /* signed */ 1),
                                                                   n.get_locus());
        Nodecl::Add rhs = Nodecl::Add::make(n.get_rhs().shallow_copy(), one, n.get_type(),
                                             n.get_locus());
        visit_assignment(n.get_rhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::Predecrement& n)
    {
        Nodecl::IntegerLiteral one = Nodecl::IntegerLiteral::make(n.get_type(), const_value_get_one(/* bytes */ 4, /* signed */ 1),
                                                                   n.get_locus());
        Nodecl::Minus rhs = Nodecl::Minus::make(n.get_rhs().shallow_copy(), one, n.get_type(),
                                                 n.get_locus());
        visit_assignment(n.get_rhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::Preincrement& n)
    {
        Nodecl::IntegerLiteral one = Nodecl::IntegerLiteral::make(n.get_type(), const_value_get_one(/* bytes */ 4, /* signed */ 1),
                                                                   n.get_locus());
        Nodecl::Add rhs = Nodecl::Add::make(n.get_rhs().shallow_copy(), one, n.get_type(),
                                             n.get_locus());
        visit_assignment(n.get_rhs(), rhs, n);
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::VectorAssignment& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs(), Nodecl::NodeclBase::null());
    }
    
    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit(const Nodecl::VectorMaskAssignment& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs(), Nodecl::NodeclBase::null());
    }
    
    // ********************** END class implementing a visitor of reaching definition ********************* //
    // **************************************************************************************************** //
}
}
