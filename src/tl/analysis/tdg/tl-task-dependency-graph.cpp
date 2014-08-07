/*--------------------------------------------------------------------
 (C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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

#include <algorithm>
#include <cassert>
#include <fstream>
#include <queue>
#include <unistd.h>
#include <sys/stat.h>

#include "cxx-cexpr.h"
#include "tl-pcfg-utils.hpp"
#include "tl-task-dependency-graph.hpp"

namespace TL { 
namespace Analysis {
    
    typedef std::map<unsigned int, TDG_Node*> TDG_Node_map;
    typedef ObjectList<TDG_Edge*> TDG_Edge_list;
    typedef ObjectList<Node*> Node_list;
    typedef ObjectList<Edge*> Edge_list;
    
    static int id = 0;
    static int control_id = 0;
    
namespace{
    
    std::string get_list_as_string(const ObjectList<std::string>& list)
    {
        std::stringstream ids;
        for(ObjectList<std::string>::const_iterator it = list.begin(); it != list.end(); )
        {
            ids << *it;
            ++it;
            if(it != list.end())
                ids << ", ";
        }
        return ids.str();
    }

    // FIXME We should get this information from the PCFGPragmaInfo instead of getting it from the related AST
    //! TDG_Edge :: This method returns the clauses associated to a Nodecl::OpenMP::Task node
    NodeclList get_task_dependency_clauses(const Nodecl::OpenMP::Task& task)
    {
        NodeclList result;
        
        Nodecl::List task_environ = task.get_environment().as<Nodecl::List>();
        for(Nodecl::List::iterator it = task_environ.begin(); it != task_environ.end(); ++it)
        {
            if(it->is<Nodecl::OpenMP::DepIn>() || it->is<Nodecl::OpenMP::DepInout>() || it->is<Nodecl::OpenMP::DepOut>())
            {
                result.insert(*it);
            }
            else
            {
                if(it->is<Nodecl::OpenMP::Target>() || it->is<Nodecl::OpenMP::If>() || 
                    it->is<Nodecl::OpenMP::Final>() || it->is<Nodecl::OpenMP::Untied>() ||
                    it->is<Nodecl::OpenMP::Firstprivate>() || it->is<Nodecl::OpenMP::Private>() || 
                    it->is<Nodecl::OpenMP::Shared>() || 
                    it->is<Nodecl::OpenMP::FlushAtEntry>() || it->is<Nodecl::OpenMP::FlushAtExit>())
                {}  // Ignore them, we expect them here
                else
                {
                    WARNING_MESSAGE("Ignoring clause %s for task %s. Maybe we should do something with it...\n", 
                                     it->prettyprint().c_str());
                }
            }
        }
        
        return result;
    }
    
    TDGEdgeType get_tdg_edge_type_from_pcfg_edge_type(NBase pcfg_edge_type)
    {
        TDGEdgeType result;
        ERROR_CONDITION(!pcfg_edge_type.is<Nodecl::StringLiteral>(), 
                        "Expected StringLiteral as attribute of a task edge, but %s found.\n", 
                        ast_print_node_type(pcfg_edge_type.get_kind()));
        char is_null_ended = 0;
        std::string type_str = std::string(const_value_string_unpack_to_string(
                    pcfg_edge_type.get_constant(),
                    &is_null_ended));
        if(type_str == "strict")
            result = Strict;
        else if(type_str == "static") 
            result = Static;
        else if(type_str == "maybe")
            result = Maybe;
        else if(type_str == "post")
            result = Post;
        else
        {
            internal_error("Unexpected type of synchronization edge '%s' from PCFG. "
                           "Expected strict|static|maybe|post.", pcfg_edge_type.prettyprint().c_str());
        }
        return result;
    }
    
    //! Returns true when there is a path between 'current' and 'task'
    bool task_is_in_path(Node* control_structure, Node* current, Node* task)
    {
        if(current->is_visited())
            return false;
        
        current->set_visited(true);
        
        // Only traverse the nodes that are inside control_structure
        if(current == control_structure->get_graph_exit_node())
            return false;
        
        // Return true only when we find the task traversing the current path
        if(current->is_omp_task_node())
        {
            if(current == task)
                return true;
            else
                return false;   // Return false because we do not want to traverse tasks depending on other tasks
                                // but only reach a tasks when traversing its corresponding task creation node
        }
        
        bool result = false;
        
        // Traverse the inner nodes if 'current' is a graph node
        if(current->is_graph_node())
            result = task_is_in_path(control_structure, current->get_graph_entry_node(), task);
        
        // Traverse the children
        ObjectList<Node*> children = current->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end() && !result; ++it)
            result = task_is_in_path(control_structure, *it, task);
        
        return result;
    }
    
    NBase get_condition_stmts(Node* cond_node)
    {
        NBase cond_stmt;
        
        if(cond_node->is_graph_node())
            cond_stmt = cond_node->get_graph_related_ast();
        else
        {
            NodeclList stmts = cond_node->get_statements();
            ERROR_CONDITION(stmts.size()!=1, "%s statements found in node %d condition. Only one statement expected.\n", 
                            stmts.size(), cond_node->get_id());
            cond_stmt = stmts[0];
        }
        
        return cond_stmt;
    }
    
    //! TaskDependencyGraph :: Returns a nodecl containing the condition that must fulfill 
    //! to follow the branch of an ifelse that takes to 'task'
    NBase get_ifelse_condition_and_path(Node* control_structure, Node* task, ObjectList<std::string>& taken_branches)
    {
        NBase condition;
        
        // Get the statements that form the condition
        Node* cond_node = control_structure->get_condition_node();
        NBase cond_stmt = get_condition_stmts(cond_node);
        
        // Find which path (TRUE|FALSE) takes to the task and compute the condition accordingly
        ObjectList<Edge*> exit_edges = cond_node->get_exit_edges();
        for(ObjectList<Edge*>::iterator it = exit_edges.begin(); it != exit_edges.end(); ++it)
        {
            if(task_is_in_path(control_structure, (*it)->get_target(), task))
            {
                condition = cond_stmt;
                taken_branches.append((*it)->is_true_edge() ? "1" : "2");
                
                break;  // Stop iterating, for we already found the task
            }
        }
        
        // Clean up the graph from the visits
        ObjectList<Node*> children = cond_node->get_children();
        for(ObjectList<Node*>::iterator itt = children.begin(); itt != children.end(); ++itt)
            ExtensibleGraph::clear_visits_in_level(*itt, control_structure);
        
        return condition;
    }
    
    void get_cases_leading_to_task(Node* switch_node, Node* control_structure, Node* current, NodeclList& cases)
    {
        if(current->is_switch_case_node())
        {
            ObjectList<Edge*> entry_edges = current->get_entry_edges();
            for(ObjectList<Edge*>::iterator it = entry_edges.begin(); it != entry_edges.end(); ++it )
                if((*it)->is_case_edge())
                    cases.insert((*it)->get_label());
            
            // If there is only one entry, no other case can lead to 'task'
            if(entry_edges.size()==1)
                return;
        }
        else if(current == switch_node)
        {
            return;
        }
        
        ObjectList<Node*> parents = (current->is_entry_node() ? ObjectList<Node*>(1, current->get_outer_node())
                                                              : current->get_parents());
        for(ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            get_cases_leading_to_task(switch_node, control_structure, *it, cases);
    }
    
    NBase get_switch_condition_from_path(Node* switch_node, Node* control_structure, Node* task, ObjectList<std::string>& taken_branches)
    {
        // Get the statements that form the condition
        Node* cond = NULL;
        ObjectList<Node*> control_parents = control_structure->get_parents();
        for(ObjectList<Node*>::iterator it = control_parents.begin(); it != control_parents.end(); ++it)
            if((*it)->is_entry_node())
            {
                Node* p = (*it);
                while(!p->is_switch_statement() && (p != NULL))
                    p = p->get_outer_node();
                ERROR_CONDITION(p==NULL, "No switch node found for case node %d.", control_structure->get_id());
                
                cond = p->get_condition_node();
                break;
            }
        NBase condition = get_condition_stmts(cond);
        
        NodeclList cases;
        get_cases_leading_to_task(switch_node, control_structure, task->get_parents()[0], cases);
        ERROR_CONDITION(cases.empty(), "No case leading to task %d has been found in control structure %d.\n", 
                        task->get_id(), control_structure->get_id());
        
        // Create the condition for each case leading to the task
        for(NodeclList::iterator it = cases.begin(); it != cases.end(); ++it)
            taken_branches.append("\"" + it->prettyprint() + "\"");
        
        return condition;
    }
    
    static std::string transform_expression_to_json_expression(
        ControlStructure* cs_node,
        const NBase& expression,
        VarToValueMap& var_to_value_map,
        VarToIdMap& var_to_id_map,
        ObjectList<NBase>& ordered_vars,
        unsigned int& last_var_id);
    
    static void get_variable_values(
            ControlStructure* cs_node,
            const NBase& var,
            VarToValueMap& var_to_value_map,
            VarToIdMap& var_to_id_map,
            ObjectList<NBase>& ordered_vars,
            unsigned int& last_var_id)
    {
        std::string range_str = "";
        ControlStructure* current_cs_node = cs_node;
check_ivs:
        // 1.- Check whether the variable is an Induction Variable
        if(current_cs_node->get_type() == Loop)
        {
            Utils::InductionVarList ivs = current_cs_node->get_pcfg_node()->get_induction_variables();
            if(Utils::induction_variable_list_contains_variable(ivs, var))
            {   // The variable is an IV: 
                // If we are parsing the original Control Structure, get the values of the Induction Variable
                if(cs_node == current_cs_node)
                {
                    Utils::InductionVar* iv = get_induction_variable_from_list(ivs, var);
                    //             range_str = iv->print_iv_as_range();
                    range_str = "[" + transform_expression_to_json_expression(
                                            current_cs_node, iv->get_lb(), var_to_value_map, var_to_id_map, ordered_vars, last_var_id)
                              + ":" + transform_expression_to_json_expression(
                                            current_cs_node, iv->get_ub(), var_to_value_map, var_to_id_map, ordered_vars, last_var_id)
                              + ":" + transform_expression_to_json_expression(
                                            current_cs_node, iv->get_increment(), var_to_value_map, var_to_id_map, ordered_vars, last_var_id)
                              + "]";
                }
                else
                {   // TODO We should compute the offset here
                    WARNING_MESSAGE("Retrieving values for variable %s in node %d which is the IV of loop %d." 
                                    "This is not yet implemented. We set the offset '0' by default.\n", 
                                    var.prettyprint().c_str(), cs_node->get_pcfg_node()->get_id(), current_cs_node->get_pcfg_node()->get_id());
                    range_str = "0";
                }
                goto end_get_vars;
            }
        }
        
        // 2.- The variable is not an IV: 
        // 2.1.- it may be an IV from an outer loop => get the proper offset
        current_cs_node = current_cs_node->get_enclosing_cs();
        while(current_cs_node!=NULL && current_cs_node->get_type()!=Loop)
            current_cs_node = current_cs_node->get_enclosing_cs();
        if(current_cs_node != NULL)
        {
            goto check_ivs;
        }
        // 2.2.- it is not an IV from an outer loop => get its values from Range Analysis
        {
            NBase range = cs_node->get_pcfg_node()->get_range(var);
            ERROR_CONDITION(range.is_null(), 
                            "No range found for non-induction_variable %s involved in loop condition.\n", 
                            var.prettyprint().c_str());
            range_str = transform_expression_to_json_expression(
                                cs_node, range, var_to_value_map, var_to_id_map, ordered_vars, last_var_id);
        }
        
end_get_vars:
        ;
        
        var_to_value_map[var] = range_str;
    }
    
namespace {
    struct StructuralCompareBind1 : Predicate<NBase>
    {
        NBase n1;
        
        StructuralCompareBind1(const NBase n1_) : n1(n1_) { }
        
        virtual bool do_(const NBase& n2) const
        {
            return Nodecl::Utils::structurally_equal_nodecls(n1, n2);
        }
    };   
}
    
    static std::string transform_expression_to_json_expression(
            ControlStructure* cs_node, 
            const NBase& expression, 
            VarToValueMap& var_to_value_map,
            VarToIdMap& var_to_id_map,
            ObjectList<NBase>& ordered_vars,
            unsigned int& last_var_id)
    {
        // This may happen when calling recursively (i.e. boundaries of an IV)
        if(expression.is_null())
            return "";
        
        std::string json_expr = expression.prettyprint();
        
        NodeclList new_vars;
        NodeclList vars_accesses = Nodecl::Utils::get_all_memory_accesses(expression);
        for (NodeclList::iterator it = vars_accesses.begin(); it != vars_accesses.end(); ++it)
        {
            // Check whether the variable has not been replaced yet
            unsigned int current_var_id;
            if(ordered_vars.filter(StructuralCompareBind1(*it)).empty())
            {
                new_vars.append(*it);
                
                // Get the identifier corresponding to this variable
                current_var_id = ++last_var_id;
                ordered_vars.append(*it);
                var_to_id_map[*it] = current_var_id;
            }
            else
            {
                current_var_id = var_to_id_map[*it];
            }
            
            std::stringstream id_ss; id_ss << "$" << current_var_id;
            
            // Replace all occurrences of that variable with the corresponding identifier
            size_t pos = 0;
            std::string var_name = it->prettyprint();   // We don't access the symbol here 
            // because we may have accesses to arrays or structs
            while((pos=json_expr.find(var_name, pos)) != std::string::npos)
            {
                std::string id_str = id_ss.str();
                json_expr.replace(pos, var_name.length(), id_str);
                pos += id_str.length();
            }
        }
        
        // Get the values of the involved variables
        for (NodeclList::iterator it = new_vars.begin(); it != new_vars.end(); ++it)
        {
            get_variable_values(cs_node, *it, var_to_value_map, var_to_id_map, ordered_vars, last_var_id);
        }
        
        return json_expr;
    }
    
    // FIXME This replacement does not take into account that input values of the variables
    // may be different on the left and right hand of the condition (for example, variables within a loop)
    static std::string transform_node_condition_into_json_expr(
            ControlStructure* cs_node, 
            const NBase& condition, 
            VarToValueMap& var_to_value_map,
            ObjectList<NBase>& ordered_vars)
    {
        unsigned int last_var_id = 0;
        VarToIdMap var_to_id_map;
        return transform_expression_to_json_expression(
                    cs_node, condition, 
                    var_to_value_map, var_to_id_map, 
                    ordered_vars, last_var_id);
    }
    
    struct ConditionVisitor : public Nodecl::NodeclVisitor<std::string> 
    {
        // *** Class members *** //
        TDG_Edge* _edge;
        int _id;
        VarToValueMap _var_to_value_map;
        NBase _dependecy_size;
        
        // *** Constructor *** //
        ConditionVisitor(TDG_Edge* edge)
            : _edge(edge), _id(0), _var_to_value_map(), _dependecy_size(NBase::null())
        {}
        
        VarToValueMap get_var_to_value_map() const
        {
            return _var_to_value_map;
        }
        
        NBase get_dependency_size() const
        {
            return _dependecy_size;
        }
        
        void collect_condition_info(int pcfg_node_id, NodeclMap reach_defs, const NBase& condition)
        {
            // Get all the variables involved in the condition
            NodeclList tmp = Nodecl::Utils::get_all_memory_accesses(condition);
            std::queue<NBase, std::deque<NBase> > vars(std::deque<NBase>(tmp.begin(), tmp.end()));
            NBase rd;
            
            NodeclSet already_treated;
            while(!vars.empty())
            {
                NBase n = vars.front();         
                vars.pop();
                ERROR_CONDITION(reach_defs.find(n)==reach_defs.end(),
                                "No reaching definition arrives from node %d for variable '%s' in condition part '%s'.\n", 
                                pcfg_node_id, n.prettyprint().c_str(), condition.prettyprint().c_str());
                if(already_treated.find(n) == already_treated.end())
                    already_treated.insert(n);
                
                NBase values;
                NodeclSet to_treat;
                if(reach_defs.find(n)!=reach_defs.end())
                {
                    // Get all the reaching definitions associated with the current variable
                    std::pair<NodeclMap::iterator, NodeclMap::iterator> reach_defs_map = reach_defs.equal_range(n);
                    NodeclMap::iterator it = reach_defs_map.first;
                    
                    // Add the first reaching definition as a value for the current variable
                    rd = it->second.first;
                    values = rd;
                    
                    // Add all the symbols involved in the reaching definition that has not yet been treated
                    // Note: this will not work when a RD uses a variable that is used in the condition (or in a recursively previous RD)
                    // and the definitions arriving for the variable are different in each point
                    tmp = Nodecl::Utils::get_all_memory_accesses(rd);
                    for(NodeclList::iterator itt = tmp.begin(); itt != tmp.end(); ++itt)
                    {
                        NBase var = *itt;
                        if ((already_treated.find(var) == already_treated.end()) && 
                            (to_treat.find(var) == to_treat.end()))
                        {
                            vars.push(var);
                            to_treat.insert(var);
                        }
                    }
                    
                    // Keep iterating the rest of reaching definitions, if there are
                    ++it;
                    for(; it != reach_defs_map.second; ++it)
                    {
                        // Add the first reaching definition as a value for the current variable
                        rd = it->second.first;
                        values = Nodecl::LogicalOr::make(values.shallow_copy(), rd.shallow_copy(), rd.get_type());
                        
                        // Add all the symbols involved in the reaching definition that has not yet been treated
                        tmp = Nodecl::Utils::get_all_memory_accesses(rd);
                        for(NodeclList::iterator itt = tmp.begin(); itt != tmp.end(); ++itt)
                        {
                            NBase var = *itt;
                            if ((already_treated.find(var) == already_treated.end()) && 
                                (to_treat.find(var) == to_treat.end()))
                            {
                                vars.push(var);
                                to_treat.insert(var);
                            }
                        }
                    }
                }
                else
                    values = n.shallow_copy();
                
                // Store the reaching definition related with the identifier of the variable defined
                _var_to_value_map[n] = values.prettyprint();
            }
        }
        
        std::string unhandled_node(const NBase& n)
        {
            internal_error( "Unhandled node of type '%s' while visiting TDG condition.\n '%s' ",
            ast_print_node_type( n.get_kind( ) ), n.prettyprint( ).c_str( ) );
            return "";
        }
        
        std::string join_list( ObjectList<std::string>& list )
        {
            WARNING_MESSAGE("Called method join_list in ConditionVisitor. This is not yet implemented", 0);
            return "";
        }
        
        // The only supported expression is Range1 ∩ Range2 != ∅
        std::string visit(const Nodecl::Different& n)
        {
            NBase lhs = n.get_lhs();
            NBase rhs = n.get_rhs();
            ERROR_CONDITION(!lhs.is<Nodecl::Analysis::RangeIntersection>(), 
                            "RangeIntersection expected but %s '%s' found.\n", 
                            ast_print_node_type(lhs.get_kind()), lhs.prettyprint().c_str());
            ERROR_CONDITION(!rhs.is<Nodecl::Analysis::EmptyRange>(), 
                            "EmptyRange expected but %s '%s' found.\n", 
                            ast_print_node_type(rhs.get_kind()), rhs.prettyprint().c_str());
            
            // Get the size of the data flow
            Type t = lhs.get_type();
            Nodecl::Analysis::RangeIntersection intersec = lhs.as<Nodecl::Analysis::RangeIntersection>();
            Nodecl::Range r1 = intersec.get_lhs().as<Nodecl::Range>();
            Nodecl::Range r2 = intersec.get_rhs().as<Nodecl::Range>();
            NBase current_size = 
                Nodecl::Mul::make(
                    Nodecl::Minus::make(
                        Nodecl::Analysis::Maximum::make(Nodecl::List::make(r1.get_lower(), r2.get_lower()), t),
                        Nodecl::Analysis::Minimum::make(Nodecl::List::make(r1.get_upper(), r2.get_upper()), t),
                        t
                    ),
                    Nodecl::Sizeof::make(Nodecl::Type::make(lhs.no_conv().get_type().no_ref()), 
                                         NBase::null(), t),
                    t
                );
            if(_dependecy_size.is_null())
                _dependecy_size = current_size;
            else
                _dependecy_size = Nodecl::Add::make(_dependecy_size.shallow_copy(), current_size, t);
            
            // Transform the variables of the inequality into its corresponding identifiers
            Node* source = _edge->get_source()->get_pcfg_node();
            collect_condition_info(source->get_id(), source->get_reaching_definitions_out(), lhs);
            return (lhs.prettyprint() + " != ∅");
        }
        
        // The variables on the LHS correspond to the source of the 'edge'
        // and the variables on the RHS correspond to the target of the 'edge'
        std::string visit(const Nodecl::Equal& n)
        {
            NBase lhs = n.get_lhs();
            NBase rhs = n.get_rhs();
            
            // Get the size of the data flow
            NBase current_size = Nodecl::Sizeof::make(Nodecl::Type::make(lhs.no_conv().get_type().no_ref()), 
                                                      NBase::null(), lhs.get_type());
            if(_dependecy_size.is_null())
                _dependecy_size = current_size;
            else
                _dependecy_size = Nodecl::Add::make(_dependecy_size.shallow_copy(), current_size, lhs.get_type());
            
            // Transform the variables of the equality into its corresponding identifiers
            Node* source = _edge->get_source()->get_pcfg_node();
            collect_condition_info(source->get_id(), source->get_reaching_definitions_out(), lhs);
            Node* target = _edge->get_target()->get_pcfg_node();
            collect_condition_info(target->get_id(), target->get_reaching_definitions_in(), rhs);
            return (lhs.prettyprint() + " == " + rhs.prettyprint());
        }
        
        std::string visit(const Nodecl::LogicalAnd& n)
        {
            std::string lhs_result = walk(n.get_lhs());
            std::string rhs_result = walk(n.get_rhs());
            return (lhs_result + " && " + rhs_result);
        }
        
        std::string visit(const Nodecl::Analysis::RangeIntersection& n)
        {
            std::string lhs_result = walk(n.get_lhs());
            std::string rhs_result = walk(n.get_rhs());
            return ("[" + lhs_result + "] ∩ [" + rhs_result + "]");
        }
    };
    
    // This method returns a string corresponding to the prettyprinted version of a nodecl
    // where each symbol occurrence is replaced by a $id
    // Example:
    //     The expression :         'i == j'
    //     Will return the string:  '$1 == $2'
    // FIXME This replacement does not take into account that input values of the variables
    // may be different in the source and the target of the edge
    std::string transform_edge_condition_into_json_expr(TDG_Edge* edge, const NBase& condition, 
                                                        VarToValueMap& var_to_values_map,
                                                        NBase& dependency_size)
    {
        ConditionVisitor cv(edge);
        // Traverse the condition to store information necessary for the transformation
        std::string result = cv.walk(condition);
        
        // Set the output parameters
        var_to_values_map = cv.get_var_to_value_map();
        dependency_size = cv.get_dependency_size();
        
        // Replace the variables' occurrences with variables ids
        std::map<std::string, int> symbol_position_map;
        unsigned int id = 0;
        for(VarToValueMap::iterator it = var_to_values_map.begin(); it != var_to_values_map.end(); ++it)
        {
            std::string var = it->first.prettyprint();
            // Find the position to be replaced initializing to 0 if it is the first occurrence
            if(symbol_position_map.find(var) == symbol_position_map.end())
                symbol_position_map[var] = 0;
            size_t pos = result.find(var, symbol_position_map[var]);
            while(pos != std::string::npos)
            {
                // Replace it    
                std::stringstream ss; ss << "$" << id;
                result.replace(pos, var.size(), ss.str());
                // Modify the base position
                symbol_position_map[var] = pos + ss.str().size() - 1;
                // Prepare next iteration
                pos = result.find(var, symbol_position_map[var]);
            }
            ++id;
        }
        
        return result;
    }
}

    // ******************************************************************* //
    // ************ Task Dependency Graph Control Structures ************* //
    
    ControlStructure::ControlStructure(int cs_id, ControlStructureType type, 
                                       const NBase& condition, Node* pcfg_node)
        : _id(cs_id), _type(type), _condition(condition), _pcfg_node(pcfg_node), _enclosing(NULL)
    {}
    
    int ControlStructure::get_id() const
    {
        return _id;
    }
    
    ControlStructureType ControlStructure::get_type() const
    {
        return _type;
    }
    
    std::string ControlStructure::get_type_as_string() const
    {
        std::string result;
        switch(_type)
        {
            case Implicit:  result = "Implicit"; break;
            case Loop:      result = "Loop";     break;
            case IfElse:    result = "IfElse";   break;
            case Switch:    result = "Switch";   break;
            default:        result = "Blank";
        };
        return result;
    }
    
    NBase ControlStructure::get_condition() const
    {
        return _condition;
    }
    
    Node* ControlStructure::get_pcfg_node() const
    {
        return _pcfg_node;
    }
    
    ControlStructure* ControlStructure::get_enclosing_cs() const
    {
        return _enclosing;
    }
    
    void ControlStructure::set_enclosing_cs(ControlStructure* cs)
    {
        _enclosing = cs;
    }
    
    // ************ Task Dependency Graph Control Structures ************* //
    // ******************************************************************* //
    
 
    // ******************************************************************* //
    // ************** Task Dependency Graph Edges and Nodes ************** //
    
    TDG_Node::TDG_Node(Node* n, TDGNodeType type)
        : _id(++id), _pcfg_node(n), _type(type), _entries(), _exits(), _control_structures()
    {}
    
    unsigned int TDG_Node::get_id() const
    {
        return _id;
    }
    
    Node* TDG_Node::get_pcfg_node() const
    {
        return _pcfg_node;
    }
    
    void TDG_Node::add_control_structure(ControlStructure* cs, const ObjectList<std::string>& taken_branches)
    {
        _control_structures.push_back(std::pair<ControlStructure*, ObjectList<std::string> >(cs, taken_branches));
    }
    
    ControlStList TDG_Node::get_control_structures() const
    {
        return _control_structures;
    }
    
    TDG_Edge::TDG_Edge(TDG_Node* source, TDG_Node* target, TDGEdgeType type, const NBase& condition)
        : _source(source), _target(target), _type(type), 
          _source_clauses(), _target_clauses(), _condition(condition)
    {
        // Fill source and target lists with the corresponding clauses
        if(source->_pcfg_node->is_omp_task_node())
        {
            Nodecl::OpenMP::Task task = source->_pcfg_node->get_graph_related_ast().as<Nodecl::OpenMP::Task>();
            _source_clauses = get_task_dependency_clauses(task);
        }
        if(target->_pcfg_node->is_omp_task_node())
        {
            Nodecl::OpenMP::Task task = target->_pcfg_node->get_graph_related_ast().as<Nodecl::OpenMP::Task>();
            _target_clauses = get_task_dependency_clauses(task);
        }
    }
    
    TDG_Node* TDG_Edge::get_source() const
    {
        return _source;
    }
    
    TDG_Node* TDG_Edge::get_target() const
    {
        return _target;
    }
    
    // ************** Task Dependency Graph Edges and Nodes ************** //
    // ******************************************************************* //

    
    // ******************************************************************* //
    // ********************** Task Dependency Graph ********************** //
    
    TaskDependencyGraph::TaskDependencyGraph(ExtensibleGraph* pcfg)
            : _pcfg(pcfg), _tdg_nodes(), _syms(), _pcfg_to_cs_map()
    {
        Node* pcfg_node = _pcfg->get_graph();
        
        // The whole code must be taskified, otherwise some dependences may not be shown
        taskify_graph(pcfg_node);
        
        // Compute the Task Dependency Graph from the PCFG
        create_tdg(pcfg_node);
    }
    
    std::string TaskDependencyGraph::get_name() const
    {
        std::string name;
        if(_pcfg != NULL)
            name = _pcfg->get_name();
        return name;
    }
    
    bool TaskDependencyGraph::contains_nodes() const
    {
        return !_tdg_nodes.empty();
    }
    
    void TaskDependencyGraph::connect_tdg_nodes(TDG_Node* parent, TDG_Node* child, 
                                                NBase type, const NBase& condition)
    {    
        TDG_Edge* edge = new TDG_Edge(parent, child, get_tdg_edge_type_from_pcfg_edge_type(type), condition);
        parent->_exits.insert(edge);
        child->_entries.insert(edge);
    }
    
    TDG_Node* TaskDependencyGraph::find_task_from_tdg_nodes_list(Node* task)
    {
        TDG_Node* result = NULL;
        for(TDG_Node_map::iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
            if(it->second->_pcfg_node == task)
            {
                result = it->second;
                break;
            }
        return result;
    }
    
    // TODO
    void TaskDependencyGraph::taskify_graph(Node* current)
    {
    }
    
    void TaskDependencyGraph::create_tdg(Node* current)
    {
        create_tdg_nodes_from_pcfg(current);
        ExtensibleGraph::clear_visits(current);
        set_tdg_nodes_control_structures();
        connect_tdg_nodes_from_pcfg(current);
        ExtensibleGraph::clear_visits(current);
    }
    
    void TaskDependencyGraph::create_tdg_nodes_from_pcfg(Node* current)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            
            // Call recursively with inner nodes if applies
            if(current->is_graph_node())
                create_tdg_nodes_from_pcfg(current->get_graph_entry_node());
            
            // Create the TDG task|taskwait|barrier node if applies
            TDG_Node* tdg_current = NULL;
            unsigned int tdg_node_id;
            if(current->is_omp_task_node())
            {
                tdg_node_id = current->get_graph_related_ast().get_line();
                tdg_current = new TDG_Node(current, Task);
            }
            else if(current->is_omp_taskwait_node())
            {
                tdg_node_id = current->get_statements()[0].get_line();
                tdg_current = new TDG_Node(current, Taskwait);
            }
            else if(current->is_omp_barrier_graph_node())   // We do not need to traverse the Graph Barrier Node
            {
                tdg_node_id = current->get_statements()[0].get_line();
                tdg_current = new TDG_Node(current, Barrier);
            }
            if(tdg_current != NULL)
                _tdg_nodes.insert(std::pair<unsigned int, TDG_Node*>(tdg_node_id, tdg_current));
            
            // Iterate over the children
            Node_list children = current->get_children();
            for(Node_list::iterator it = children.begin(); it != children.end(); ++it)
                create_tdg_nodes_from_pcfg(*it);
        }
    }
    
    void TaskDependencyGraph::set_tdg_nodes_control_structures()
    {
        for(TDG_Node_map::iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
        {
            TDG_Node* tdg_node = it->second;
            Node* node = tdg_node->_pcfg_node;
            ControlStructure* last_cs = NULL;
            
            // 1.- Add the implicit control structure: 
            //     this is necessary to set the values of the variables reaching a task
            {
                ObjectList<std::string> taken_branches;
                ControlStructure* cs = new ControlStructure(++control_id, Implicit, NBase::null(), NULL);
                _pcfg_to_cs_map[node] = cs;
                tdg_node->add_control_structure(cs, taken_branches);
                last_cs = cs;
            }
            
            // 2.- Add the real control structures
            Node* control_structure = ExtensibleGraph::get_enclosing_control_structure(node);
            while(control_structure != NULL)
            {
                // 1.- Get control structure type and condition
                ControlStructureType cs_t;
                NBase condition;
                ObjectList<std::string> taken_branches;
                if(control_structure->is_loop_node())
                {
                    // get the type of the Control Structure
                    cs_t = Loop;
                    
                    // Get the condition of the loop
                    Node* cond = control_structure->get_condition_node();
                    assert(cond != NULL);
                    NodeclList stmts = cond->get_statements();
                    assert(stmts.size() == 1);
                    condition = stmts[0];
                }
                else if(control_structure->is_ifelse_statement())
                {
                    // get the type of the Control Structure
                    cs_t = IfElse;
                    
                    // Check whether the statement is in the TRUE or the FALSE branch of the condition
                    condition = get_ifelse_condition_and_path(control_structure, node, taken_branches);
                }
                else if(control_structure->is_switch_case_node())
                {
                    // Get the switch containing the current case
                    Node* outer_switch = control_structure->get_outer_node();
                    while((outer_switch!=NULL) && !outer_switch->is_switch_statement())
                        outer_switch = outer_switch->get_outer_node();

                    ERROR_CONDITION(outer_switch==NULL, 
                                    "No switch previously found for switch case %d\n", 
                                    control_structure->get_id());
                    
                    // get the type of the Control Structure
                    cs_t = Switch;
                    
                    // Build the condition depending on the branch where the task is created
                    condition = get_switch_condition_from_path(outer_switch, control_structure, node, taken_branches);
                }
                else
                {
                    internal_error("Unexpected node type %s when printing condition to TDG.\nExpected Loop, IfElse or Switch.\n", 
                                   control_structure->get_type_as_string().c_str())
                }
                
                // 2.- Store the symbols involved in the condition in the list of used symbols in the graph
                ERROR_CONDITION(condition.is_null(), "No condition has been computed for task %d in control structure %d.\n", 
                                node->get_id(), control_structure->get_id());
                store_condition_list_of_symbols(condition, node->get_reaching_definitions_in());
                
                // 3.- Crete the ControlStructure object and set it to the node
                // For switch cases, the control structure is the 'case' because 
                // we need the path to the 'case' currently being treated to build the condition
                // However, the control structure to which we associate an identifier is the switch, not the particular case
                Node* real_control_structure = control_structure;
                if(control_structure->is_switch_case_node())
                {
                    while(real_control_structure!=NULL && !real_control_structure->is_switch_statement())
                        real_control_structure = real_control_structure->get_outer_node();
                    ERROR_CONDITION(real_control_structure==NULL, 
                                    "No Switch node found wrapping Case node %d.\n", control_structure->get_id());
                }
                
                ControlStructure* cs;
                if(_pcfg_to_cs_map.find(real_control_structure) != _pcfg_to_cs_map.end())
                    cs = _pcfg_to_cs_map[real_control_structure];
                else
                {   // The control structure did not exist yet
                    cs = new ControlStructure(++control_id, cs_t, condition, real_control_structure);
                    _pcfg_to_cs_map[real_control_structure] = cs;
                }
                last_cs->set_enclosing_cs(cs);
                tdg_node->add_control_structure(cs, taken_branches);
                
                // Prepare next iteration
                last_cs = cs;
                control_structure = ExtensibleGraph::get_enclosing_control_structure(control_structure);
            }
        }
    }
    
    void TaskDependencyGraph::store_condition_list_of_symbols(const NBase& condition, const NodeclMap& reach_defs)
    {
        NodeclSet already_treated;
        NodeclList tmp = Nodecl::Utils::get_all_memory_accesses(condition);
        std::queue<NBase, std::deque<NBase> > vars(std::deque<NBase>(tmp.begin(), tmp.end()));
        while(!vars.empty())
        {
            NBase n = vars.front();         
            vars.pop();
            already_treated.insert(n);
            _syms.insert(std::pair<NBase, unsigned int>(n, 0));
            
            // Add all the variables found in the reaching definitions of the current variable
            ERROR_CONDITION(reach_defs.find(n) == reach_defs.end(), 
                            "No reaching definition found for variable '%s' while gathering all necessary symbols.\n", 
                            n.prettyprint().c_str());
            
            std::pair<NodeclMap::const_iterator, NodeclMap::const_iterator> reach_defs_map = reach_defs.equal_range(n);
            NodeclSet to_treat;
            for(NodeclMap::const_iterator it = reach_defs_map.first; it != reach_defs_map.second; ++it)
            {
                tmp = Nodecl::Utils::get_all_memory_accesses(it->second.first);
                for(NodeclList::iterator itt = tmp.begin(); itt != tmp.end(); ++itt)
                {
                    NBase var = *itt;
                    if ((already_treated.find(var) == already_treated.end()) && 
                        (to_treat.find(var) == to_treat.end()))
                    {
                        to_treat.insert(var);
                        vars.push(var);
                    }
                }
            }
        }
    }
    
    void TaskDependencyGraph::connect_tdg_nodes_from_pcfg(Node* current)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            
            if(current->is_omp_task_node())
            {
                // Connect all tasks synchronized here with the new Taskwait/Barrier TDG_Node
                TDG_Node* tdg_sync = find_task_from_tdg_nodes_list(current);
                Edge_list sync_exits = current->get_exit_edges();
                for(Edge_list::iterator it = sync_exits.begin(); it != sync_exits.end(); ++it)
                {
                    Node* child = (*it)->get_target();
                    if(child->is_omp_task_node() || child->is_omp_taskwait_node() || child->is_omp_barrier_graph_node())
                    {
                        TDG_Node* tdg_child_task = find_task_from_tdg_nodes_list(child);
                        connect_tdg_nodes(tdg_sync, tdg_child_task, (*it)->get_label(), (*it)->get_condition());
                        store_condition_list_of_symbols((*it)->get_condition(), current->get_reaching_definitions_out());
                    }
                }
            }
            else if(current->is_omp_taskwait_node() || current->is_omp_barrier_graph_node())
            {
                TDG_Node* tdg_sync = find_task_from_tdg_nodes_list(current);
                // Look for the real node to whom the current synchronization is connected
                Edge_list sync_exits = current->get_exit_edges();
                while(sync_exits.size()==1 && 
                      (sync_exits[0]->get_target()->is_omp_flush_node() || sync_exits[0]->get_target()->is_exit_node()))
                {
                    Node* child = sync_exits[0]->get_target();
                    if(child->is_exit_node())
                        sync_exits = child->get_outer_node()->get_exit_edges();
                    else
                        sync_exits = child->get_exit_edges();
                }
                // Connect the synchronization to the exit node if it is a task or another synchronization
                for(Edge_list::iterator it = sync_exits.begin(); it != sync_exits.end(); ++it)
                {
                    Node* child = (*it)->get_target();
                    if(child->is_omp_task_node() || child->is_omp_taskwait_node() || child->is_omp_barrier_graph_node())
                    {
                        TDG_Node* tdg_child_task = find_task_from_tdg_nodes_list(child);
                        connect_tdg_nodes(tdg_sync, tdg_child_task, (*it)->get_label(), (*it)->get_condition());
                        store_condition_list_of_symbols((*it)->get_condition(), current->get_reaching_definitions_out());
                    }
                }
            }
            
            if(current->is_graph_node())
                connect_tdg_nodes_from_pcfg(current->get_graph_entry_node());
            
            // Iterate over the children
            Node_list children = current->get_children();
            for(Node_list::iterator it = children.begin(); it != children.end(); ++it)
                connect_tdg_nodes_from_pcfg(*it);
        }
    }
    
//     static std::string prettyprint_clauses(Nodecl_list clauses)
//     {
//         std::string result;
//         
//         for(NodeclList::iterator it = clauses.begin(); it != clauses.end();)
//         {
//             // Note: there is no codegen for OpenMP nodecls, 
//             // that is why we print it manually instead of calling prettyprint
//             std::string clause_name;
//             std::string clause_args;
//             Nodecl::List args;
//             if(it->is<Nodecl::OpenMP::DepIn>())
//             {
//                 clause_name = "in";
//                 args =  it->as<Nodecl::OpenMP::DepIn>().get_in_deps().as<Nodecl::List>();
//             }
//             else if(it->is<Nodecl::OpenMP::DepOut>())
//             {
//                 clause_name = "out";
//                 args =  it->as<Nodecl::OpenMP::DepOut>().get_out_deps().as<Nodecl::List>();
//             }
//             else if(it->is<Nodecl::OpenMP::DepInout>())
//             {
//                 clause_name = "inout";
//                 args =  it->as<Nodecl::OpenMP::DepInout>().get_inout_deps().as<Nodecl::List>();
//             }
//             
//             for(Nodecl::List::iterator it_a = args.begin(); it_a != args.end();)
//             {
//                 clause_args += it_a->prettyprint();
//                 ++it_a;
//                 if(it_a != args.end())
//                     clause_args += ", ";
//             }
//             result += clause_name + "(" + clause_args + ")";
//             
//             ++it;
//             if(it != clauses.end())
//                 result += ", ";
//         }
//         
//         return result;
//     }
    
    void TaskDependencyGraph::print_tdg_node_to_dot(TDG_Node* current, std::ofstream& dot_tdg) const
    {
        std::stringstream ss; ss << current->_id;
        std::string current_id = ss.str();
        Node* n = current->_pcfg_node;
        
        // Print the control structures (subgraphs) where the node is enclosed in
        ControlStList control_structures = current->get_control_structures();
        std::string indent = "\t";
        unsigned int n_cs = 0;
        for(ControlStList::const_reverse_iterator it = control_structures.rbegin(); it != control_structures.rend(); ++it)
        {
            ControlStructure* cs = it->first;
            if(cs->get_type() != Implicit)
            {
                dot_tdg << indent << "subgraph cluster_" << ++id << "{\n";
                indent += "\t";
                dot_tdg << indent << "label=\"" << cs->get_condition().prettyprint() << "\";\n";
                dot_tdg << indent << "color=\"" << (cs->get_type()==Loop ? "deeppink" : "deepskyblue1") << "\";\n";
                dot_tdg << indent << "style=\"dashed\";\n";
                ++n_cs;
            }
        }
        
        // Create the node
        std::string task_label = "";
        TDGNodeType ntype = current->_type;
        if(ntype == Task)
        {
            // Get the name of the task
            Nodecl::OpenMP::Task task = n->get_graph_related_ast().as<Nodecl::OpenMP::Task>();
            task_label = "Task :: " + task.get_locus_str();
            Nodecl::List environ = task.get_environment().as<Nodecl::List>();
            for(Nodecl::List::iterator it = environ.begin(); it != environ.end(); ++it)
                if(it->is<Nodecl::OpenMP::TaskLabel>())
                {
                    task_label = "_" + it->prettyprint();
                    break;
                }
        }
        else if(ntype == Taskwait)
        {
            NBase tw_stmt = n->get_statements()[0];
            task_label = "Taskwait :: " + tw_stmt.get_locus_str();
        }
        else if(ntype == Barrier)
        {
            NBase barrier_stmt = n->get_graph_related_ast();
            task_label = "Barrier :: " + barrier_stmt.get_locus_str();
        }
        
        // print the node
        dot_tdg << indent << current_id << " [label=\"" << task_label << "\"];\n";
        
        
        // Close the subgraphs of the control structures
        for(unsigned int i = 0; i < n_cs; ++i)
        {
            indent = indent.substr(0, indent.size()-1);
            dot_tdg << indent << "}\n";
        }
        
        // Create the connections from the current node to its children
        std::string headlabel, taillabel, style, condition;
        for(TDG_Edge_list::iterator it = current->_exits.begin(); it != current->_exits.end(); ++it)
        {
            // Get the edge info in a string
//             headlabel = "headlabel=\"" + prettyprint_clauses((*it)->_target_clauses) + "\"";
//             taillabel = "taillabel=\"" + prettyprint_clauses((*it)->_source_clauses) + "\"";
            TDGEdgeType etype = (*it)->_type;
            style = "style=\"" + std::string((etype == Strict || etype == Static) ? "solid" : "dashed") + "\"";
            if(!(*it)->_condition.is_null())
                condition = ", label=\"" + (*it)->_condition.prettyprint() + "\"";
            else
                condition = ", label=\"true\"";
            // Create the dot edge
            std::stringstream child_id; child_id << (*it)->_target->_id;
            dot_tdg << "\t" << current_id << " -> " << child_id.str() 
                    << "[" << style << condition /*<< headlabel << ", " << taillabel*/ << "];\n";
        }
    }
    
    void TaskDependencyGraph::print_tdg_to_dot() const
    {
        // Create the directory of dot files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/dot/";
        if(stat(directory_name.c_str(), &st) != 0)
        {
            int dot_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if(dot_directory != 0)
                internal_error ("An error occurred while creating the dot directory in '%s'", 
                                 directory_name.c_str());
        }
        
        // Create the file where we will store the DOT TDG
        std::string dot_file_name = directory_name + _pcfg->get_name() + "_tdg.dot";
        std::ofstream dot_tdg;
        dot_tdg.open(dot_file_name.c_str());
        if(!dot_tdg.good())
            internal_error ("Unable to open the file '%s' to store the TDG.", dot_file_name.c_str());
        
        // Create the DOT graphs
        if(VERBOSE)
            std::cerr << "- TDG DOT file '" << dot_file_name << "'" << std::endl;
        dot_tdg << "digraph TDG {\n";
            dot_tdg << "\tcompound=true;\n";
            for(TDG_Node_map::const_iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
                print_tdg_node_to_dot(it->second, dot_tdg);
        dot_tdg << "}\n";
        dot_tdg.close();
        if(!dot_tdg.good())
            internal_error ("Unable to close the file '%s' where TDG has been stored.", dot_file_name.c_str());
        ExtensibleGraph::clear_visits(_pcfg->get_graph());
    }
    
    void TaskDependencyGraph::print_tdg_control_structs_to_json(std::ofstream& json_tdg) const
    {
        json_tdg << "\t\t\"control_structures\" : [\n";
        
        // Print the Controls Structures involved in the tasks instantiation
        if(!_pcfg_to_cs_map.empty())
        {
            NBase dependency_size;

            for(PCFG_to_CS::const_iterator it = _pcfg_to_cs_map.begin(); it != _pcfg_to_cs_map.end(); )
            {
                ControlStructure* cs = it->second;
                json_tdg << "\t\t\t{\n";
                
                    json_tdg << "\t\t\t\t\"id\" : " << cs->get_id() << ",\n";
                    json_tdg << "\t\t\t\t\"type\" : \"" << cs->get_type_as_string();
                    if(cs->get_pcfg_node() != NULL)
                    {
                        json_tdg << "\",\n";
                        json_tdg << "\t\t\t\t\"locus\" : \"" << cs->get_pcfg_node()->get_graph_related_ast().get_locus_str() << "\",\n";
                        json_tdg << "\t\t\t\t\"when\" : {\n";
                        print_condition(NULL, cs, json_tdg, "\t\t\t\t\t", 
                                        /*unnecessary param for a control structure's condition*/dependency_size);
                        json_tdg << "\t\t\t\t}";
                        if((cs->get_type() == IfElse) || (cs->get_type() == Switch))
                        {
                            json_tdg << ",\n";
                            unsigned int nbranches = 0;
                            ObjectList<Node*> branches = cs->get_pcfg_node()->get_condition_node()->get_children();
                            if(cs->get_type() == IfElse)
                            {   // branches list contains 2 elements
                                if(!branches[0]->is_exit_node())
                                    nbranches++;
                                if(!branches[1]->is_exit_node())
                                    nbranches++;
                            }
                            else    // Switch
                            {
                                // The children of the condition in a Switch node is the Context
                                branches = branches[0]->get_graph_entry_node()->get_children();
                                nbranches = branches.size();
                            }
                            json_tdg << "\t\t\t\t\"nbranches\" : " << nbranches << "\n";
                        }
                        else
                        {
                            json_tdg << "\n";
                        }
                    }
                    else
                    {
                        json_tdg << "\"\n";
                    }
                    
                ++it;
                if(it != _pcfg_to_cs_map.end())
                    json_tdg << "\t\t\t},\n";
                else
                    json_tdg << "\t\t\t}\n";
            }
        }
        
        json_tdg << "\t\t],\n" ;
    }
    
    void TaskDependencyGraph::print_tdg_syms_to_json(std::ofstream& json_tdg)
    {
        if(!_syms.empty())
        {
            json_tdg << "\t\t\"variables\" : [\n";
            unsigned int i = 1;
            for(std::map<NBase, unsigned int>::iterator it = _syms.begin(); it != _syms.end(); ++i)
            {
                NBase n = it->first;
                json_tdg << "\t\t\t{\n";
                    json_tdg << "\t\t\t\t\"id\" : " << i << ",\n";
                    json_tdg << "\t\t\t\t\"name\" : \"" << n.prettyprint() << "\",\n";
                    json_tdg << "\t\t\t\t\"locus\" : \"" << n.get_locus_str() << "\",\n";
                    json_tdg << "\t\t\t\t\"type\" : \"" << n.get_type().no_ref().get_declaration(n.retrieve_context(), /*no symbol name*/"") << "\"\n";
                _syms[n] = i;
                ++it;
                if(it != _syms.end())
                    json_tdg << "\t\t\t},\n";
                else
                    json_tdg << "\t\t\t}\n";
            }
            json_tdg << "\t\t],\n" ;
        }
    }
    
    // FIXME Try to homogenize printing condition from nodes and edges
    void TaskDependencyGraph::print_condition(TDG_Edge* edge, ControlStructure* node_cs, 
                                              std::ofstream& json_tdg, std::string indent, 
                                              NBase& dependency_size) const
    {
        json_tdg << indent << "\"expression\" : ";
        assert(edge!=NULL || node_cs!=NULL);
        if((edge != NULL && !edge->_condition.is_null()) || (node_cs != NULL))
        {   
            NBase condition;
            VarToValueMap var_to_value_map;
            ObjectList<NBase> ordered_vars;
            if(node_cs != NULL)
            {
                // Get the condition
                condition = node_cs->get_condition();
                json_tdg << "\"" << transform_node_condition_into_json_expr(
                                            node_cs, condition, 
                                            var_to_value_map, ordered_vars) << "\",\n";
                // Generate the list of involved variables
                json_tdg << indent << "\"vars\" : [\n";
                for(ObjectList<NBase>::iterator itt = ordered_vars.begin(); itt != ordered_vars.end(); )
                {
                    VarToValueMap::iterator it = var_to_value_map.find(*itt);
                    std::map<NBase, unsigned int>::const_iterator its = _syms.find(it->first);
                    if(its == _syms.end())
                    {
                        internal_error ("Variable %s, found in condition %s, "
                                        "has not been found during the phase of gathering the variables", 
                                        it->first.prettyprint().c_str(), condition.prettyprint().c_str());
                    }
                    json_tdg << indent << "\t{\n";
                        json_tdg << indent << "\t\t\"id\" : " << its->second << ",\n";
                        json_tdg << indent << "\t\t\"values\" : \""<< it->second << "\"\n";
                    ++itt;
                    if(itt != ordered_vars.end())
                        json_tdg << indent << "\t},\n";
                    else
                        json_tdg << indent << "\t}\n";
                }
            }
            else
            {
                // Get the condition
                condition = edge->_condition;
                json_tdg << "\"" << transform_edge_condition_into_json_expr(
                                            edge, condition, 
                                            var_to_value_map, dependency_size) << "\",\n";
                // Generate the list of involved variables
                json_tdg << indent << "\"vars\" : [\n";
                for(VarToValueMap::iterator it = var_to_value_map.begin(); it != var_to_value_map.end(); )
                {
                    std::map<NBase, unsigned int>::const_iterator its = _syms.find(it->first);
                    if(its == _syms.end())
                    {
                        internal_error ("Variable %s, found in condition %s, "
                                        "has not been found during the phase of gathering the variables", 
                                        it->first.prettyprint().c_str(), condition.prettyprint().c_str());
                    }
                    json_tdg << indent << "\t{\n";
                        json_tdg << indent << "\t\t\"id\" : " << its->second << ",\n";
                        json_tdg << indent << "\t\t\"values\" : \""<< it->second << "\"\n";
                    ++it;
                    if(it != var_to_value_map.end())
                        json_tdg << indent << "\t},\n";
                    else
                        json_tdg << indent << "\t}\n";
                }
            }
            
            json_tdg << indent << "]\n";
        }
        else    // There is no condition => TRUE
            json_tdg << "true\n";
    }
    
    void TaskDependencyGraph::print_tdg_nodes_to_json(std::ofstream& json_tdg) const
    {
        json_tdg << "\t\t\"nodes\" : [\n";
        for(TDG_Node_map::const_iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); )
        {
            TDG_Node* n = it->second;
            json_tdg << "\t\t\t{\n";
                
            // node identifier
                json_tdg << "\t\t\t\t\"id\" : " << n->_id << ",\n";
                
            // node locus and type
            if(n->_type == Task) 
            {
                json_tdg << "\t\t\t\t\"locus\" : \"" << n->_pcfg_node->get_graph_related_ast().get_locus_str() << "\",\n";
                json_tdg << "\t\t\t\t\"type\" : \"Task\"";
            }
            else
            {
                json_tdg << "\t\t\t\t\"locus\" : \"" << n->_pcfg_node->get_statements()[0].get_locus_str() << "\",\n";
                if(n->_type == Taskwait)
                    json_tdg << "\t\t\t\t\"type\" : \"Taskwait\"";
                else
                    json_tdg << "\t\t\t\t\"type\" : \"Barrier\"";
            }
            
            // node control structures
            ControlStList control_structures = n->get_control_structures();
            json_tdg << ",\n";
            json_tdg << "\t\t\t\t\"control\" : [\n";
            {
                for(ControlStList::iterator itt = control_structures.begin(); itt != control_structures.end(); )
                {
                    json_tdg << "\t\t\t\t\t{\n";
                    json_tdg << "\t\t\t\t\t\t\"control_id\" : " << itt->first->get_id();
                    if(itt->first->get_type() == IfElse)
                    {
                        json_tdg << ",\n";
                        json_tdg << "\t\t\t\t\t\t\"branch_id\" : [" << get_list_as_string(itt->second) << "]\n";
                    }
                    else if(itt->first->get_type() == Switch)
                    {
                        json_tdg << ",\n";
                        json_tdg << "\t\t\t\t\t\t\"branch_cond\" : [" << get_list_as_string(itt->second) << "]\n";
                    }
                    else
                    {
                        json_tdg << "\n";
                    }
                    ++itt;
                    if(itt != control_structures.end())
                        json_tdg << "\t\t\t\t\t},\n";
                    else
                        json_tdg << "\t\t\t\t\t}\n";
                }
            }
            json_tdg << "\t\t\t\t]\n";
            
            ++it;
            if(it != _tdg_nodes.end())
                json_tdg << "\t\t\t},\n";
            else
                json_tdg << "\t\t\t}\n";
        }
        json_tdg << "\t\t]";
    }
    
    void TaskDependencyGraph::print_tdg_edges_to_json(std::ofstream& json_tdg) const
    {
        // Get all edges in the graph
        TDG_Edge_list edges;
        for(TDG_Node_map::const_iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
            edges.append(it->second->_exits);
        
        // Print the edges into the dot file
        if(!edges.empty())
        {
            json_tdg << ",\n";
            json_tdg << "\t\t\"dependencies\" : [\n";
            for(TDG_Edge_list::iterator it = edges.begin(); it != edges.end(); )
            {
                json_tdg << "\t\t\t{\n";
                    json_tdg << "\t\t\t\t\"source\" : " << (*it)->_source->_id << ",\n";
                    json_tdg << "\t\t\t\t\"target\" : " << (*it)->_target->_id << ",\n";
                    json_tdg << "\t\t\t\t\"when\" : {\n";
                        NBase dependency_size;
                        print_condition(*it, NULL, json_tdg, "\t\t\t\t\t", dependency_size);
                    json_tdg << "\t\t\t\t}";
                    if(!dependency_size.is_null())
                    {
                        json_tdg << ",\n";
                        json_tdg << "\t\t\t\t\"size\" : \"" << dependency_size.prettyprint() << "\"\n";
                    }
                    else
                        json_tdg << "\n";
                ++it;
                if(it != edges.end())
                    json_tdg << "\t\t\t},\n";
                else
                    json_tdg << "\t\t\t}\n";
            }
            json_tdg << "\t\t]\n";
        }
        else
            json_tdg << "\n";
    }
    
    void TaskDependencyGraph::print_tdg_to_json()
    {
        // Create the directory of json files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/json/";
        if(stat(directory_name.c_str(), &st) != 0)
        {
            int json_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if(json_directory != 0)
                internal_error ("An error occurred while creating the json directory in '%s'", 
                                 directory_name.c_str());
        }
        
        // Create the file where we will store the JSON TDG
        std::string json_file_name = directory_name + _pcfg->get_name() + "_tdg.json";
        std::ofstream json_tdg;
        json_tdg.open(json_file_name.c_str());
        if(!json_tdg.good())
            internal_error ("Unable to open the file '%s' to store the TDG.", json_file_name.c_str());
        
        // Create the JSON graphs
        if(VERBOSE)
            std::cerr << "- TDG JSON file '" << json_file_name << "'" << std::endl;
        json_tdg << "{\n";
            json_tdg << "\t\"tdg\" : {\n";
                TL::Symbol sym = _pcfg->get_function_symbol();
                json_tdg << "\t\t\"function\" : \"" << (sym.is_valid() ? sym.get_name() : "") << "\",\n";
                json_tdg << "\t\t\"locus\" : \"" << _pcfg->get_nodecl().get_locus_str() << "\",\n";
                print_tdg_syms_to_json(json_tdg);
                print_tdg_control_structs_to_json(json_tdg);
                print_tdg_nodes_to_json(json_tdg);
                print_tdg_edges_to_json(json_tdg);
            json_tdg << "\t}\n";
        json_tdg << "}\n";
        json_tdg.close();
        if(!json_tdg.good())
            internal_error ("Unable to close the file '%s' where PCFG has been stored.", json_file_name.c_str());
        ExtensibleGraph::clear_visits(_pcfg->get_graph());
    }
}
}
