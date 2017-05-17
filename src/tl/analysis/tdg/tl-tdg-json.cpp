/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supercomputing Center             * *
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

#include <cassert>
#include <queue>
#include <fstream>
#include <sys/stat.h>
#include <unistd.h>

#include "cxx-cexpr.h"
#include "tl-compilerpipeline.hpp"
#include "tl-counters.hpp"
#include "tl-task-dependency-graph.hpp"


namespace TL { 
namespace Analysis {

namespace {

    typedef std::multimap<Node*, NBase> Node_to_NBase_map;

    TL::Counter &tdg_var_id = TL::CounterManager::get_counter("tdg-var-id");
    std::string full_report_name;
    FILE* report_file;

    Node_to_NBase_map _reported_offset_vars;

    void report_default_offset(Node* n, Node* loop, const NBase& var)
    {
        if (TDG_DEBUG)
        {
            // Check whether the variable has already been reported
            if (_reported_offset_vars.find(n) != _reported_offset_vars.end())
            {
                std::pair<Node_to_NBase_map::iterator, Node_to_NBase_map::iterator> n_reported_vars = _reported_offset_vars.equal_range(n);
                for (Node_to_NBase_map::iterator it = n_reported_vars.first; it != n_reported_vars.second; ++it)
                {
                    if (Nodecl::Utils::structurally_equal_nodecls(it->second, var, /*skip_conversion_nodes*/true))
                        return;
                }
            }

            // Report the variable and store to avoid reporting it again
            WARNING_MESSAGE("Retrieving values for variable %s in node %d which is the IV of loop %d."
                            "This is not yet implemented. We set the offset '0' by default.\n",
                            var.prettyprint().c_str(), n->get_id(), loop->get_id());
            _reported_offset_vars.insert(std::pair<Node*, NBase>(n, var));
        }
    }

    struct StructuralCompareBind1
    {
        NBase n1;

        StructuralCompareBind1(const NBase n1_) : n1(n1_) { }

        virtual bool operator()(const NBase& n2) const
        {
            return Nodecl::Utils::structurally_equal_nodecls(n1, n2);
        }
    };

    void transform_expression_to_json_expression(
            ControlStructure* cs_node,
            NBase& expression,
            VarToNodeclMap& var_to_value_map,
            VarToNodeclMap& var_to_id_map,
            ObjectList<NBase>& ordered_vars,
            unsigned int& last_var_id);

    void get_variable_values(
            ControlStructure* cs_node,
            const NBase& var,
            VarToNodeclMap& var_to_value_map,
            VarToNodeclMap& var_to_id_map,
            ObjectList<NBase>& ordered_vars,
            unsigned int& last_var_id)
    {
        NBase range;
        ControlStructure* current_cs_node = cs_node;
check_ivs:
        // 1.- Check whether the variable is an Induction Variable
        if(current_cs_node->get_type() == Loop)
        {
            const Utils::InductionVarList& ivs = current_cs_node->get_pcfg_node()->get_induction_variables();
            if(Utils::induction_variable_list_contains_variable(ivs, var))
            {   // The variable is an IV: 
                // If we are parsing the original Control Structure, get the values of the Induction Variable
                if(cs_node == current_cs_node)
                {
                    Utils::InductionVar* iv = get_induction_variable_from_list(ivs, var);
                    const NodeclSet& iv_lb = iv->get_lb();
                    ERROR_CONDITION(iv_lb.size()>1,
                                    "More than one LB found for IV %s. This is not yet implemented.\n",
                                    iv->get_variable().prettyprint().c_str());
                    NBase lb = iv_lb.begin()->shallow_copy();
                    transform_expression_to_json_expression(
                            current_cs_node, lb, var_to_value_map, var_to_id_map, ordered_vars, last_var_id);
                    const NodeclSet& iv_ub = iv->get_ub();
                    ERROR_CONDITION(iv_lb.size()>1,
                                    "More than one LB found for IV %s. This is not yet implemented.\n",
                                    iv->get_variable().prettyprint().c_str());
                    NBase ub = iv_ub.begin()->shallow_copy();
                    transform_expression_to_json_expression(
                            current_cs_node, ub, var_to_value_map, var_to_id_map, ordered_vars, last_var_id);
                    NBase incr = iv->get_increment().shallow_copy();
                    transform_expression_to_json_expression(
                            current_cs_node, incr, var_to_value_map, var_to_id_map, ordered_vars, last_var_id);
                    range = Nodecl::Range::make(lb, ub, incr, Type::get_int_type());
                }
                else
                {   // TODO We should compute the offset here
                    report_default_offset(cs_node->get_pcfg_node(), current_cs_node->get_pcfg_node(), var);
                    range = Nodecl::IntegerLiteral::make(Type::get_int_type(), const_value_get_zero(/*bytes*/ 4, /*signed*/ 1));
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
            range = cs_node->get_pcfg_node()->get_range(var).shallow_copy();
            ERROR_CONDITION(range.is_null(), 
                            "No range found for non-induction_variable %s involved in loop condition.\n", 
                            var.prettyprint().c_str());
            transform_expression_to_json_expression(
                    cs_node, range, var_to_value_map, var_to_id_map, ordered_vars, last_var_id);
        }

end_get_vars:
        ;

        var_to_value_map[var] = range;
    }

    void transform_expression_to_json_expression(
            ControlStructure* cs_node,
            NBase& expression,
            VarToNodeclMap& var_to_value_map,
            VarToNodeclMap& var_to_id_map,
            ObjectList<NBase>& ordered_vars,
            unsigned int& last_var_id)
    {
        // This may happen when calling recursively (i.e. boundaries of an IV)
        if (expression.is_null())
            return;

        // Gather all variables and compute their corresponding identifier
        NodeclList new_vars;
        NodeclList vars_accesses = Nodecl::Utils::get_all_memory_accesses(expression);
        for (NodeclList::iterator it = vars_accesses.begin(); it != vars_accesses.end(); ++it)
        {
            // Check whether the variable has not been replaced yet
            if (ordered_vars.filter(StructuralCompareBind1(*it)).empty())
            {
                const NBase& var = it->shallow_copy();
                new_vars.append(var);
                ordered_vars.append(var);

                // Get the identifier corresponding to this variable
                std::stringstream id_ss; id_ss << "$" << ++last_var_id;
                var_to_id_map[var] = Nodecl::Text::make(id_ss.str());
            }
        }

        // Replace the original variables with their corresponding $id
        NodeclReplacer nr(var_to_id_map);
        nr.walk(expression);

        // Get the values of the involved variables
        for (NodeclList::iterator it = new_vars.begin(); it != new_vars.end(); ++it)
        {
            get_variable_values(cs_node, *it, var_to_value_map, var_to_id_map, ordered_vars, last_var_id);
        }
    }

    // FIXME This replacement does not take into account that input values of the variables
    // may be different on the left and right hand of the condition (for example, variables within a loop)
    void transform_node_condition_into_json_expr(
            ControlStructure* cs_node, 
            NBase& condition,
            VarToNodeclMap& var_to_value_map,
            ObjectList<NBase>& ordered_vars)
    {
        unsigned int last_var_id = 0;
        VarToNodeclMap var_to_id_map;
        return transform_expression_to_json_expression(
                    cs_node, condition, 
                    var_to_value_map, var_to_id_map, 
                    ordered_vars, last_var_id);
    }

    /*! This class gathers information of the variables in a dependency condition
     * in order to translate it into its JSON form */
    struct ConditionVisitor : public Nodecl::NodeclVisitor<void>
    {
        // *** Class members *** //
        TDG_Edge* _edge;
        int _id;
        VarToNodeclMap _source_var_to_value_map;
        VarToNodeclMap _target_var_to_value_map;
        
        // *** Constructor *** //
        ConditionVisitor(TDG_Edge* edge)
            : _edge(edge), _id(0),
              _source_var_to_value_map(), _target_var_to_value_map()
        {}
        
        VarToNodeclMap get_source_var_to_value_map() const
        {
            return _source_var_to_value_map;
        }

        VarToNodeclMap get_target_var_to_value_map() const
        {
            return _target_var_to_value_map;
        }

        void collect_condition_info(TDG_Node* n, const NBase& cond_expr, bool is_source)
        {
            // 1.- Get all the variables involved in cond_expr
            NodeclList tmp = Nodecl::Utils::get_all_memory_accesses(cond_expr);
            std::queue<NBase, std::deque<NBase> > vars(std::deque<NBase>(tmp.begin(), tmp.end()));

            // 2.- For each variable involved in the condition, gather its values
            Node* pcfg_n = n->get_pcfg_node();
            NodeclSet already_treated;
            while (!vars.empty())
            {
                // 2.1.- Get the current variable
                NBase v = vars.front();
                vars.pop();

                // 2.2.- Compute the value(s) of the variable
                NBase values;

                // 2.2.1.- First check whether this is an induction variable of any loop enclosing the task
                const ControlStList& cs_list = n->get_control_structures();
                for (ControlStList::const_iterator it = cs_list.begin(); it != cs_list.end(); ++it)
                {
                    ControlStructure* cs = it->first;
                    if(cs->get_type() == Loop)
                    {
                        bool is_loop_iv = false;
                        Utils::InductionVarList ivs = cs->get_pcfg_node()->get_induction_variables();
                        for (Utils::InductionVarList::iterator itt = ivs.begin(); itt != ivs.end(); ++itt)
                        {
                            if (Nodecl::Utils::structurally_equal_nodecls((*itt)->get_variable(), v, /*skip_conversions*/true))
                            {
                                is_loop_iv = true;
                                break;
                            }
                        }

                        if (is_loop_iv)
                        {   // The variable is an IV
                            report_default_offset(pcfg_n, cs->get_pcfg_node(), v);
                            values = Nodecl::IntegerLiteral::make(Type::get_int_type(), const_value_get_zero(/*bytes*/ 4, /*signed*/ 1));
                            goto insert_values;
                        }
                    }
                }

                // 2.2.2.- The variable is not an induction variable. Retrieve values from range analysis
                {
                    // 2.2.2.1.-  Make sure we have some value for the variable
                    values = pcfg_n->get_range(v);
                    ERROR_CONDITION(values.is_null(),
                                    "No range computed in node %d for variable '%s', in condition's %s '%s'.\n",
                                    pcfg_n->get_id(), v.prettyprint().c_str(), (is_source ? "LHS" : "RHS"), cond_expr.prettyprint().c_str());

                    // 2.2.2.2.- Store the variable so we do not treat it again (to avoid recursive definitions)
                    if (already_treated.find(v) == already_treated.end())
                        already_treated.insert(v);

                    // 2.2.2.3.- Add to 'vars' all the symbols involved in 'values' that has not yet been treated
                    NodeclSet to_treat;
                    tmp = Nodecl::Utils::get_all_memory_accesses(values);
                    for (NodeclList::iterator itt = tmp.begin(); itt != tmp.end(); ++itt)
                    {
                        NBase var = *itt;
                        if ((already_treated.find(var) == already_treated.end())
                            && (to_treat.find(var) == to_treat.end()))
                        {
                            vars.push(var);
                            to_treat.insert(var);
                        }
                    }
                }

insert_values:
                // 2.5.- Store the reaching definition related with the identifier of the variable defined
                if (is_source)
                    _source_var_to_value_map[v] = values;
                else
                    _target_var_to_value_map[v] = values;
            }
        }

        void unhandled_node(const NBase& n)
        {
            internal_error( "Unhandled node of type '%s' while visiting TDG condition.\n '%s' ",
            ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
        }
        
        void join_list(ObjectList<void>& list)
        {
            if (TDG_DEBUG)
                WARNING_MESSAGE("Called method join_list in ConditionVisitor. This is not yet implemented", 0);
        }

        // The variables on the LHS correspond to the source of the 'edge'
        // and the variables on the RHS correspond to the target of the 'edge'
        void visit(const Nodecl::Equal& n)
        {
            // Recursively call with the LHS and RHS of the condition
            collect_condition_info(_edge->get_source(), n.get_lhs(), /*is_source*/true);
            collect_condition_info(_edge->get_target(), n.get_rhs(), /*is_source*/false);
        }
        
        void visit(const Nodecl::LogicalAnd& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());
        }

        void visit(const Nodecl::LogicalOr& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());
        }

        void visit(const Nodecl::LowerOrEqualThan& n)
        {
            // Recursively call with the LHS and RHS of the condition
            collect_condition_info(_edge->get_source(), n.get_lhs(), /*is_source*/true);
            collect_condition_info(_edge->get_target(), n.get_rhs(), /*is_source*/false);
        }

        void visit(const Nodecl::GreaterOrEqualThan& n)
        {
            // Recursively call with the LHS and RHS of the condition
            collect_condition_info(_edge->get_source(), n.get_lhs(), /*is_source*/true);
            collect_condition_info(_edge->get_target(), n.get_rhs(), /*is_source*/false);
        }
    };

    void get_operands_position(
            size_t init,
            std::string condition_str,
            size_t& min_logical_op,
            size_t& min_comp_op)
    {
        size_t tmp_or = condition_str.find("||", init);
        size_t tmp_and = condition_str.find("&&", init);
        min_logical_op = (tmp_or < tmp_and ? tmp_or : tmp_and);
        size_t tmp_eq = condition_str.find("==", init);
        size_t tmp_l_eq = condition_str.find("<=", init);
        size_t tmp_g_eq = condition_str.find(">=", init);
        min_comp_op = (tmp_eq < tmp_l_eq
                            ? (tmp_eq < tmp_g_eq
                                    ? tmp_eq
                                    : tmp_g_eq)
                            : (tmp_l_eq < tmp_g_eq
                                    ? tmp_l_eq
                                    : tmp_g_eq)
                        );
    }

    void replace_vars_with_ids(
            const VarToNodeclMap& var_to_values_map,
            VarToNodeclMap& var_to_id_map,
            NBase& condition,
            unsigned int& id,
            bool is_source)
    {
        // NOTE: We cannot use NodeclReplacer because the LHS of the comparisons must be replaced with an identifier and
        // the RHS with another (example: x == x  -> $1 == $2)
        std::string condition_str = condition.prettyprint();
        for (VarToNodeclMap::const_iterator it = var_to_values_map.begin(); it != var_to_values_map.end(); ++it)
        {
            std::string var = it->first.prettyprint();
            std::stringstream ss; ss << "$" << id;

            size_t init = condition_str.find(var, 0);
            // Replace the variables with their id
            while (init != std::string::npos)
            {
                // check whether this occurrence belongs to the LHS or the RHS of the condition

                size_t min_logical_op, min_comp_op;
                get_operands_position(init, condition_str, min_logical_op, min_comp_op);

                if ((is_source && (min_comp_op < min_logical_op))
                        || (!is_source && ((min_logical_op < min_comp_op) || (min_logical_op == std::string::npos))))
                {
                    // The variable may appear more than once in the same side of a comparison
                    while (init != std::string::npos
                        && ((is_source && init < min_comp_op)
                            || (!is_source && init < min_logical_op)))
                    {
                        // Only replace if the substring found is not a part of another variable
                        // (example: var_name and var_name_longer)
                        std::string c_after_var_name = condition_str.substr(init+var.size(), 1);
                        if (!isalpha(c_after_var_name.c_str()[0])
                                || condition_str.size() == init+var.size())                 // end of string
                        {
                            condition_str.replace(init, var.size(), ss.str());
                            get_operands_position(init, condition_str, min_logical_op, min_comp_op);
                        }
                        init = condition_str.find(var, init+1);
                    }
                }

                // prepare the next iteration
                init = condition_str.find(var, min_logical_op);
            }

            // Identifier must always be incremented because
            // it defined the order in which the variables will be printed in the JSON
            ++id;
        }

        condition = Nodecl::Text::make(condition_str);
    }

    // This method returns a string corresponding to the prettyprinted version of a nodecl
    // where each symbol occurrence is replaced by a $id
    // Example:
    //     The expression :         'i == j'
    //     Will return the string:  '$1 == $2'
    void transform_edge_condition_into_json_expr(
            TDG_Edge* edge,
            NBase& condition,
            VarToNodeclMap& source_var_to_values_map,
            VarToNodeclMap& target_var_to_values_map,
            VarToNodeclMap& source_var_to_id_map,
            VarToNodeclMap& target_var_to_id_map)
    {
        ConditionVisitor cv(edge);
        // Traverse the condition to store information necessary for the transformation
        cv.walk(condition);

        // Set the output parameters
        source_var_to_values_map = cv.get_source_var_to_value_map();
        target_var_to_values_map = cv.get_target_var_to_value_map();

        // LHS and RHS variables must be treated separately
        unsigned int id = 1;
        replace_vars_with_ids(source_var_to_values_map, source_var_to_id_map, condition, id, /*is_source*/true);
        replace_vars_with_ids(target_var_to_values_map, target_var_to_id_map, condition, id, /*is_source*/false);
    }

}

    void OldTaskDependencyGraph::print_tdg_control_structs_to_json(std::ofstream& json_tdg) const
    {
        json_tdg << "\t\t\"control_structures\" : [\n";
        
        // Print the Controls Structures involved in the tasks instantiation
        if(!_pcfg_to_cs_map.empty())
        {
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
                        print_condition(NULL, cs, json_tdg, "\t\t\t\t\t");
                        json_tdg << "\t\t\t\t}";
                        if(cs->get_type() == IfElse)
                        {
                            json_tdg << ",\n";
                            unsigned int nbranches = 0;
                            ObjectList<Node*> branches = cs->get_pcfg_node()->get_condition_node()->get_children();
                            // branches list contains 2 elements
                            if(!branches[0]->is_exit_node())
                                nbranches++;
                            if(!branches[1]->is_exit_node())
                                nbranches++;
                            json_tdg << "\t\t\t\t\"nbranches\" : " << nbranches << "\n";
                        }
                        else
                        {
                            json_tdg << "\n";
                        }
                    }
                    else
                    {
                        // If the task has some dependency with variables involved,
                        // add here the values of those variables
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

    void OldTaskDependencyGraph::print_tdg_syms_to_json(std::ofstream& json_tdg)
    {
        if (!_syms.empty())
        {
            json_tdg << "\t\t\"variables\" : [\n";
            for (std::map<NBase, unsigned int>::iterator it = _syms.begin(); it != _syms.end(); )
            {
                int id = ++tdg_var_id;
                NBase n = it->first;
                json_tdg << "\t\t\t{\n";
                    json_tdg << "\t\t\t\t\"id\" : " << id << ",\n";
                    json_tdg << "\t\t\t\t\"name\" : \"" << n.prettyprint() << "\",\n";
                    json_tdg << "\t\t\t\t\"locus\" : \"" << n.get_locus_str() << "\",\n";
                    Type t = n.get_type().no_ref().advance_over_typedefs().advance_over_typedefs();
                    json_tdg << "\t\t\t\t\"type\" : \"" << t.get_declaration(n.retrieve_context(), /*no symbol name*/"") << "\"\n";
                _syms[n] = id;
                ++it;
                if (it != _syms.end())
                    json_tdg << "\t\t\t},\n";
                else
                    json_tdg << "\t\t\t}\n";
            }
            json_tdg << "\t\t],\n" ;
        }
    }

    void replace_vars_values_with_ids(const VarToNodeclMap& var_to_id_map, NBase& value)
    {
        NodeclReplacer nr(var_to_id_map);
        nr.walk(value);
    }

    // Returns as string a nodecl representing the value of a variable
    // When this value is a range with only one element,
    // then we return a sequence with the unique value
    std::string OldTaskDependencyGraph::print_value(const NBase& var, const NBase& value) const
    {
        std::string value_str;
        if (value.is<Nodecl::Range>())
        {
            const Nodecl::Range& value_range = value.as<Nodecl::Range>();
            const Nodecl::NodeclBase& lb = value_range.get_lower();
            const Nodecl::NodeclBase& ub = value_range.get_upper();
            if (report_file != NULL)
            {
                if (lb.is<Nodecl::Analysis::MinusInfinity>()
                    || ub.is<Nodecl::Analysis::PlusInfinity>())
                {
                    fprintf(report_file,
                            "    Infinit loop boundary found for variable '%s' in line '%d'. "
                            "Boxer will not be able to expand the TDG properly.\n",
                            var.prettyprint().c_str(),
                            var.get_line());
                }
            }
            if (Nodecl::Utils::structurally_equal_nodecls(lb, ub, /*skip_conversions*/true)
                    || (lb.is_constant() && ub.is_constant()
                            && const_value_is_zero(const_value_sub(lb.get_constant(), ub.get_constant()))))
            {   // Return a sequence with the unique value
                value_str = "{" + lb.prettyprint() + "}";
            }
            else
            {   // Return the range, since it has more than one value
                value_str = value.prettyprint();
            }
        }
        else
        {   // Normally, return the string representing the value
            value_str = value.prettyprint();
        }
        return value_str;
    }

    void OldTaskDependencyGraph::print_dependency_variables_to_json(
            std::ofstream& json_tdg,
            const VarToNodeclMap& var_to_value_map,
            const VarToNodeclMap& var_to_id_map,
            const NBase& condition,
            std::string indent,
            bool is_source,
            bool add_final_comma) const
    {
        for (VarToNodeclMap::const_iterator it = var_to_value_map.begin(); it != var_to_value_map.end(); )
        {
            std::map<NBase, unsigned int>::const_iterator its = _syms.find(it->first);
            ERROR_CONDITION(its == _syms.end(),
                            "Variable %s, found in condition %s, "
                            "has not been found during the phase of gathering the variables",
                            it->first.prettyprint().c_str(), condition.prettyprint().c_str());
            json_tdg << indent << "\t{\n";
            json_tdg << indent << "\t\t\"id\" : " << its->second << ",\n";
            NBase value = it->second.shallow_copy();
            replace_vars_values_with_ids(var_to_id_map, value);
            json_tdg << indent << "\t\t\"values\" : \""<< print_value(its->first, value) << "\",\n";
            json_tdg << indent << "\t\t\"side\" : \"" << (is_source ? "source" : "target") << "\"\n";
            ++it;
            if (it != var_to_value_map.end() || add_final_comma)
                json_tdg << indent << "\t},\n";
            else
                json_tdg << indent << "\t}\n";
        }
    }

    void OldTaskDependencyGraph::print_condition(
            TDG_Edge* edge,
            ControlStructure* n,
            std::ofstream& json_tdg,
            std::string indent) const
    {
        json_tdg << indent << "\"expression\" : ";
        assert(edge!=NULL || n!=NULL);
        if ((edge != NULL && !edge->_condition.is_null()) || (n != NULL))
        {
            NBase condition;
            // 1.- This condition belongs to a control structure
            if (n != NULL)
            {
                // Get the condition
                condition = n->get_condition().shallow_copy();
                VarToNodeclMap var_to_value_map;
                ObjectList<NBase> ordered_vars;
                transform_node_condition_into_json_expr(n, condition,
                        var_to_value_map, ordered_vars);
                json_tdg << "\"" << condition.prettyprint() << "\",\n";
                // Get the IVs of the related PCFG if the node is a Loop control structure
                Utils::InductionVarList ivs;
                if (n->get_type() == Loop)
                    ivs = n->get_pcfg_node()->get_induction_variables();
                // Generate the list of involved variables
                json_tdg << indent << "\"vars\" : [\n";
                for (ObjectList<NBase>::iterator itt = ordered_vars.begin(); itt != ordered_vars.end(); )
                {
                    VarToNodeclMap::iterator it = var_to_value_map.find(*itt);
                    std::map<NBase, unsigned int>::const_iterator its = _syms.find(it->first);
                    ERROR_CONDITION(its == _syms.end(),
                                    "Variable %s, found in condition %s, "
                                    "has not been found during the phase of gathering the variables",
                                    it->first.prettyprint().c_str(), condition.prettyprint().c_str());
                    json_tdg << indent << "\t{\n";
                        json_tdg << indent << "\t\t\"id\" : " << its->second << ",\n";
                        if (!ivs.empty())
                        {   // Only induction variables are always printed as ranges.
                            // Other kinds of variables are printed as sequences if they can take only one value
                            if (Utils::induction_variable_list_contains_variable(ivs, it->first))
                                json_tdg << indent << "\t\t\"values\" : \""<< it->second.prettyprint() << "\"\n";
                            else
                                json_tdg << indent << "\t\t\"values\" : \""<< print_value(it->first, it->second) << "\"\n";
                        }
                        else
                            json_tdg << indent << "\t\t\"values\" : \""<< print_value(it->first, it->second) << "\"\n";
                    ++itt;
                    if (itt != ordered_vars.end())
                        json_tdg << indent << "\t},\n";
                    else
                        json_tdg << indent << "\t}\n";
                }
            }
            // 2.- This condition belongs to a dependency expression
            else
            {
                // Get the condition
                condition = edge->_condition.shallow_copy();
                VarToNodeclMap source_var_to_value_map, target_var_to_value_map;
                VarToNodeclMap source_var_to_id_map, target_var_to_id_map;
                transform_edge_condition_into_json_expr(edge, condition,
                        source_var_to_value_map, target_var_to_value_map,
                        source_var_to_id_map, target_var_to_id_map);
                json_tdg << "\"" << condition.prettyprint() << "\",\n";
                // Generate the list of involved variables
                json_tdg << indent << "\"vars\" : [\n";
                print_dependency_variables_to_json(json_tdg, source_var_to_value_map, source_var_to_id_map,
                        condition, indent, /*is_source*/true, /*add_final_comma*/true);
                print_dependency_variables_to_json(json_tdg, target_var_to_value_map, target_var_to_id_map,
                        condition, indent, /*is_source*/false, /*add_final_comma*/false);
            }
            
            json_tdg << indent << "]\n";
        }
        else
        {   // There is no condition => TRUE
            json_tdg << "true\n";
        }
    }

namespace {
    void compute_size(Type t, std::stringstream& size)
    {
        // Base case: the compiler cannot compute the size of the type
        if (t.is_incomplete())
        {
           size << "sizeof(" << t.print_declarator() << ")";
        }
        // The compiler computes the size
        else
        {
            size << t.get_size();
        }
    }

    void get_size_str(const Nodecl::List& map_vars, std::stringstream& size)
    {
        for (Nodecl::List::const_iterator itv = map_vars.begin();
             itv != map_vars.end(); )
        {
            compute_size(itv->get_type().no_ref(), size);
            ++itv;
            if (itv != map_vars.end())
                size << " + ";
        }
    }
}

    void OldTaskDependencyGraph::print_tdg_nodes_to_json(std::ofstream& json_tdg)
    {
        json_tdg << "\t\t\"nodes\" : [\n";
        for (TDG_Node_map::const_iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); )
        {
            TDG_Node* n = it->second;
            json_tdg << "\t\t\t{\n";
                
            // node identifier
            json_tdg << "\t\t\t\t\"id\" : " << n->_id << ",\n";
                
            // node locus and type
            if (n->_type == Task || n->_type == Target || n->_type == Barrier)
            {   // These are graph nodes
                // We split locus into filename and line because
                // for implicit barrier nodes the line is computed when filling the _tdg_nodes map
                json_tdg << "\t\t\t\t\"locus\" : \"" << n->_pcfg_node->get_graph_related_ast().get_filename()
                         << ":" << it->first << "\",\n";
                std::string type_str =
                        ((n->_type == Task) ? "Task"
                                            : ((n->_type == Target) ? "Target"
                                                                    : "Barrier"));
                json_tdg << "\t\t\t\t\"type\" : \"" << type_str << "\"";

                if (n->_type == Target)
                {   // Add the size of the data moved into the device and from it
                    PCFGPragmaInfo clauses = n->_pcfg_node->get_pragma_node_info();
                    std::stringstream size_in, size_out;
                    if (clauses.has_clause(NODECL_OPEN_M_P_MAP_TO))
                    {
                        Nodecl::OpenMP::MapTo map_to = clauses.get_clause(NODECL_OPEN_M_P_MAP_TO).as<Nodecl::OpenMP::MapTo>();
                        get_size_str(map_to.get_map_to().as<Nodecl::List>(), size_in);
                    }
                    if (clauses.has_clause(NODECL_OPEN_M_P_MAP_FROM))
                    {
                        Nodecl::OpenMP::MapFrom map_from = clauses.get_clause(NODECL_OPEN_M_P_MAP_FROM).as<Nodecl::OpenMP::MapFrom>();
                        get_size_str(map_from.get_map_from().as<Nodecl::List>(), size_out);
                    }
                    if (clauses.has_clause(NODECL_OPEN_M_P_MAP_TO_FROM))
                    {
                        if (!size_in.str().empty())
                        {
                            size_in << " + ";
                        }
                        if (!size_out.str().empty())
                        {
                            size_out << " + ";
                        }
                        Nodecl::OpenMP::MapToFrom map_tofrom = clauses.get_clause(NODECL_OPEN_M_P_MAP_TO_FROM).as<Nodecl::OpenMP::MapToFrom>();
                        get_size_str(map_tofrom.get_map_tofrom().as<Nodecl::List>(), size_in);
                        get_size_str(map_tofrom.get_map_tofrom().as<Nodecl::List>(), size_out);
                    }
                    json_tdg << ",\n";
                    json_tdg << "\t\t\t\t\"size_in\" : \"" << size_in.str() << "\",\n";
                    json_tdg << "\t\t\t\t\"size_out\" : \"" << size_out.str() << "\"";
                }
            }
            else
            {   // This is a normal node
                json_tdg << "\t\t\t\t\"locus\" : \"" << n->_pcfg_node->get_statements()[0].get_filename()
                         << ":" << it->first << "\",\n";
                if (n->_type == Taskpart)
                {
                    // FIXME Since boxer has not been modified to accept Taskparts,
                    //       we represent them as Tasks
//                     json_tdg << "\t\t\t\t\"type\" : \"Taskpart\"";
                    json_tdg << "\t\t\t\t\"type\" : \"Task\"";

                    // FIXME Since boxer has not been modified to accept the new attribute 'parent_id'
                    //       we do not insert it yet
                    // Add the identifier of the creating task (0 by default) and
                    // the order of the taskpart
//                     int parent_id = (n->_parent->get_id() == 1
//                             ? 0 /* parent task is the implicit task */
//                             : find_tdg_node_from_pcfg_node(n->_parent)->_id);
//                     json_tdg << ",\n";
//                     json_tdg << "\t\t\t\t\"parent_id\" : \"" << parent_id << "\"";
                }
                else if (n->_type == Taskwait)
                {
                    json_tdg << "\t\t\t\t\"type\" : \"Taskwait\"";
                }
                else
                {
                    internal_error("Unexpected TDG node type %d.\n", n->_type);
                }
            }

            // node control structures
            ControlStList control_structures = n->get_control_structures();
            json_tdg << ",\n";
            json_tdg << "\t\t\t\t\"control\" : [\n";
            for (ControlStList::iterator itt = control_structures.begin(); itt != control_structures.end(); )
            {
                json_tdg << "\t\t\t\t\t{\n";
                json_tdg << "\t\t\t\t\t\t\"control_id\" : " << itt->first->get_id();
                if (itt->first->get_type() == IfElse)
                {
                    json_tdg << ",\n";
                    json_tdg << "\t\t\t\t\t\t\"branch_id\" : [" << itt->second << "]\n";
                }
                else
                {
                    json_tdg << "\n";
                }
                ++itt;
                if (itt != control_structures.end())
                    json_tdg << "\t\t\t\t\t},\n";
                else
                    json_tdg << "\t\t\t\t\t}\n";
            }
            json_tdg << "\t\t\t\t]\n";
            
            ++it;
            if (it != _tdg_nodes.end())
                json_tdg << "\t\t\t},\n";
            else
                json_tdg << "\t\t\t}\n";
        }
        json_tdg << "\t\t]";
    }
    
    void OldTaskDependencyGraph::print_tdg_edges_to_json(std::ofstream& json_tdg) const
    {
        // Get all edges in the graph
        TDG_Edge_list edges;
        for (TDG_Node_map::const_iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
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
                        print_condition(*it, NULL, json_tdg, "\t\t\t\t\t");
                    json_tdg << "\t\t\t\t}\n";
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

    void OldTaskDependencyGraph::print_tdg_to_json(std::ofstream& json_tdg)
    {
        json_tdg << "\t{\n";
            TL::Symbol sym = _pcfg->get_function_symbol();
            json_tdg << "\t\t\"tdg_id\" : " << _id << ",\n";
            json_tdg << "\t\t\"function\" : \"" << (sym.is_valid() ? sym.get_name() : "") << "\",\n";
            json_tdg << "\t\t\"locus\" : \"" << _pcfg->get_nodecl().get_locus_str() << "\",\n";
            print_tdg_syms_to_json(json_tdg);
            print_tdg_control_structs_to_json(json_tdg);
            print_tdg_nodes_to_json(json_tdg);
            print_tdg_edges_to_json(json_tdg);
        json_tdg << "\t}";

        ExtensibleGraph::clear_visits(_pcfg->get_graph());
    }

    void OldTaskDependencyGraph::print_tdgs_to_json(const ObjectList<OldTaskDependencyGraph*>& tdgs)
    {
        if (tdgs.empty())
            return;

        // 1.- Create the directory of json files if it has not been previously created
        char directory_name[1024];
        char* err = getcwd(directory_name, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string full_directory_name = std::string(directory_name) + "/json/";
        if(stat(full_directory_name.c_str(), &st) != 0)
        {
            int json_directory = mkdir(full_directory_name.c_str(), S_IRWXU);
            if(json_directory != 0)
                internal_error ("An error occurred while creating the json directory in '%s'",
                                 full_directory_name.c_str());
        }

        // 2.- Create the file where we will store the JSON TDG
        std::string json_file_name = full_directory_name + "tdgs.json";
        std::ofstream json_tdg;
        json_tdg.open(json_file_name.c_str());
        if(!json_tdg.good())
            internal_error ("Unable to open the file '%s' to store the TDG.", json_file_name.c_str());

        // 3.- Create (or open if already exists) the file where the PSocrates reports will be stored
        if (full_report_name == "")
        {
            ExtensibleGraph* pcfg = (*tdgs.begin())->get_pcfg();
            std::string report_name = pcfg->get_nodecl().get_filename();
            report_name = report_name.substr(0, report_name.find(".", 0));
            full_report_name = std::string(directory_name) + "/" + report_name + "_psocrates.report";
            report_file = fopen(full_report_name.c_str(), "wt");
            TL::CompiledFile current = TL::CompilationProcess::get_current_file();
            std::string current_file = current.get_filename();
            fputs("/* ---------------------------------------------------------\n", report_file);
            fputs("   This file is a report of the Task Dependency Graphs\n", report_file);
            fputs("   generated in the context of the P-Socrates project.\n", report_file);
            fputs("   Information about non-supported features is provided.\n\n", report_file);
            fprintf(report_file, "   ===== Compiled file '%s' =====\n\n", current_file.c_str());
            fputs("--------------------------------------------------------- */\n\n\n\n", report_file);
        }
        else
        {
            report_file = fopen(full_report_name.c_str(), "a");
        }
        if (report_file == NULL)
        {
            internal_error("Unable to open the file '%s' to store Psocrates report.",
                           full_report_name.c_str());
        }

        // 4.- Create the JSON graphs ordered by identifier (filling the report on the way)
        if(VERBOSE)
            std::cerr << "- TDG JSON file '" << json_file_name << "'" << std::endl;
        json_tdg << "{\n";
        json_tdg << "\"num_tdgs\" : " << tdgs.size() << ",\n";
        json_tdg << "\"tdgs\" : [\n";
        unsigned int current_id = 0;
        std::set<OldTaskDependencyGraph*> tdgs_set(tdgs.begin(), tdgs.end());
        std::set<OldTaskDependencyGraph*>::iterator it = tdgs_set.begin();
        while (!tdgs_set.empty() && it != tdgs_set.end())
        {
            if ((*it)->get_id() != current_id)
            {
                ++it;
                continue;
            }

            //  Print in report's file
            ExtensibleGraph* pcfg = (*it)->get_pcfg();
            std::string func_name =
                    (pcfg->get_function_symbol().is_valid()
                            ? pcfg->get_function_symbol().get_name()
                            : (*it)->get_name());
            fprintf(report_file, "Funtion '%s' \n", func_name.c_str());

            (*it)->print_tdg_to_json(json_tdg);

            // Prepare next iteration
            current_id++;
            tdgs_set.erase(it);
            it = tdgs_set.begin();

            //  Print in report's file
            fprintf(report_file, "END funtion '%s'\n\n", func_name.c_str());

            // Print in JSON's file
            if (!tdgs_set.empty())
                json_tdg << ",\n";
            else
                json_tdg << "\n";

        }
        ERROR_CONDITION(!tdgs_set.empty(),
                        "Not all TDGs have been printed!\n", 0);

        json_tdg << "]\n";
        json_tdg << "}\n";

        // 5.- Close the JSON file
        json_tdg.close();
        if (!json_tdg.good())
            internal_error ("Unable to close the file '%s' where PCFG has been stored.", json_file_name.c_str());

        // 6.- Close the report file
        int res = fclose(report_file);
        if (res == EOF)
            internal_error("Unable to close the file '%s' where Psocrates report has been stored.", full_report_name.c_str());
    }

}
}
    