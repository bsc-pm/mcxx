/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "tl-analysis-check-phase.hpp"
#include "tl-analysis-base.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-pcfg-visitor.hpp"
#include "tl-omp-lint.hpp"

#include <algorithm>

namespace TL {
namespace Analysis {

namespace {

    const std::string analysis_none_sym_name = "__ANALYSIS_NONE__";
    
    NBase get_nodecl_from_string(std::string str, ReferenceScope sc)
    {
        Source src; src << str;
        return src.parse_expression(sc);
    }

    Nodecl::List get_nodecl_list_from_string(std::string str, ReferenceScope sc)
    {
        Nodecl::List result;

        size_t ini = 0;
        size_t fin = str.find(',', ini);
        std::string v;
        while (fin != std::string::npos)
        {
            v = str.substr(ini, fin-ini);
            const NBase& n = get_nodecl_from_string(v, sc);
            result.append(n);
            ini = fin+1;
            fin = str.find(',', ini);
        }
        v = str.substr(ini, str.size());
        const NBase& n = get_nodecl_from_string(v, sc);
        result.append(n);

        return result;
    }

    Nodecl::List extract_reaching_definitions_map_from_clause( const PragmaCustomClause& c, ReferenceScope sc )
    {
        Nodecl::List result;

        ObjectList<std::string> args = c.get_tokenized_arguments( ExpressionTokenizerTrim( ';' ) );
        for( ObjectList<std::string>::iterator it = args.begin( ); it != args.end( ); ++it )
        {   // Each token will have the form: "var : value-list"
            // Split the token using the ':'
            std::string token = *it;
            std::string::iterator end_pos = std::remove( token.begin( ), token.end( ), ' ' );
            token.erase( end_pos, token.end( ) );
            int colon_pos = token.find( ':' );
            std::string reach_def = token.substr( 0, colon_pos );
            std::string value = token.substr( colon_pos+1, token.size( )-colon_pos );

            // Parse the variable which is a reaching definition
            NBase reach_def_nodecl = get_nodecl_from_string(reach_def, sc);

            // Parse the value/s set for this variable
            // Create the ReachDefExpr corresponding to a pair of <reach_def, value>
            std::string current_value;
            int pos = 0;
            while( value.find( ',', pos ) != std::string::npos )
            {
                current_value = value.substr( pos, value.find( ',', pos ) - pos );
                NBase value_nodecl = get_nodecl_from_string(current_value, sc);
                result.append( Nodecl::Analysis::ReachDefExpr::make( reach_def_nodecl.shallow_copy( ), value_nodecl ) );

                pos = value.find( ',', pos ) + 1;
            }
            current_value = value.substr( pos, value.size( ) - pos );
            NBase value_nodecl = get_nodecl_from_string(current_value, sc);
            result.append( Nodecl::Analysis::ReachDefExpr::make( reach_def_nodecl.shallow_copy( ), value_nodecl ) );
        }

        return result;
    }

    Nodecl::List extract_induction_variable_info_from_clause( const PragmaCustomClause& c, ReferenceScope sc )
    {   // Each token will have the form: "iv : lb : ub : stride "
        Nodecl::List result;

        ObjectList<std::string> args = c.get_tokenized_arguments( ExpressionTokenizerTrim( ';' ) );
        for( ObjectList<std::string>::iterator it = args.begin( ); it != args.end( ); ++it )
        {
            // Split the token using the ':'
            std::string token = *it;
            std::string::iterator end_pos = std::remove( token.begin( ), token.end( ), ' ' );
            token.erase( end_pos, token.end( ) );

            int colon_pos = token.find( ':' );
            std::string iv = token.substr( 0, colon_pos );
            colon_pos++;
            std::string lb = token.substr( colon_pos, token.find( ':', colon_pos )-colon_pos );
            colon_pos = token.find( ':', colon_pos ) + 1;
            std::string ub = token.substr( colon_pos, token.find( ':', colon_pos )-colon_pos );
            colon_pos = token.find( ':', colon_pos ) + 1;
            std::string stride = token.substr( colon_pos, token.find( ':', colon_pos )-colon_pos );

            NBase iv_nodecl = get_nodecl_from_string(iv, sc);
            Nodecl::List lb_nodecl = get_nodecl_list_from_string(lb, sc);
            Nodecl::List ub_nodecl = get_nodecl_list_from_string(ub, sc);
            NBase stride_nodecl = get_nodecl_from_string(stride, sc);

            result.append( Nodecl::Analysis::InductionVarExpr::make( iv_nodecl, lb_nodecl, ub_nodecl, stride_nodecl ) );
        }

        return result;
    }

    void check_task_synchronizations( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( current->is_omp_task_creation_node( ) )
            {   // This nodes have two children, the task they create and the child that is sequentially executed
                ObjectList<Node*> children = current->get_children( );
                ERROR_CONDITION( children.size( ) != 2,
                                 "A task creation node must have 2 children, the created task and "\
                                 "the node that is sequentially executed. "\
                                 "Task creation node %d has %d children", current->get_id( ), children.size( ) );
            }
            else if( current->is_omp_task_node( ) )
            {
                ObjectList<Node*> children = current->get_children( );
                ERROR_CONDITION( children.empty( ),
                                 "Task %d is never synchronized", current->get_id( ) );
            }
        }
    }

    void compare_assert_set_with_analysis_set(const NodeclSet& assert_set, const NodeclSet& analysis_set,
                                               std::string locus_str, int node_id,
                                               std::string clause_name, std::string analysis_name )
    {
        if( !assert_set.empty( ) )
        {
            if((assert_set.size() == 1) && 
               (assert_set.begin()->is<Nodecl::Symbol>()) && 
               (assert_set.begin()->get_symbol().get_name()==analysis_none_sym_name))
            {
                if(!analysis_set.empty())
                {
                    internal_error("%s: Assertion '%s(%s)' does not fulfill.\n"\
                                   "There should not be %s variables associated to node %d\n",
                                   locus_str.c_str( ),
                                   clause_name.c_str(), Utils::prettyprint_nodecl_set(assert_set, /*dot*/ false).c_str(),
                                   analysis_name.c_str( ), node_id );
                }
            }
            else if( analysis_set.empty( ) )
            {
                internal_error( "%s: Assertion '%s(%s)' does not fulfill.\n"\
                                "There are no %s variables associated to node %d\n",
                                locus_str.c_str( ),
                                clause_name.c_str(), Utils::prettyprint_nodecl_set(assert_set, /*dot*/ false).c_str(),
                                analysis_name.c_str( ), node_id );
            }
            else
            {
                NodeclSet diff = Utils::nodecl_set_difference(assert_set, analysis_set);
                if( !diff.empty( ) )
                {
                    internal_error( "%s: Assertion '%s(%s)' does not fulfill.\n"\
                                    "Expressions '%s' are no %s variables associated to node %d\n",
                                    locus_str.c_str( ),
                                    clause_name.c_str(), Utils::prettyprint_nodecl_set(assert_set, /*dot*/ false).c_str(),
                                    Utils::prettyprint_nodecl_set(diff, /*dot*/ false).c_str(),
                                    analysis_name.c_str( ), node_id );
                }
            }
        }
    }

    void compare_assert_map_with_analysis_map(NodeclMap assert_map, NodeclMap analysis_map,
                                               std::string locus_str, int node_id,
                                               std::string clause_name, std::string analysis_name )
    {
        if (!assert_map.empty())
        {
            Nodecl::List rd_visited;
            for (NodeclMap::const_iterator it = assert_map.begin(); it != assert_map.end(); ++it)
            {
                NBase expr = it->first;
                if (!Nodecl::Utils::nodecl_is_in_nodecl_list(expr, rd_visited))
                {
                    rd_visited.append(expr);

                    // Get all values possible for the current expression
                    std::pair<NodeclMap::iterator, NodeclMap::iterator> assert_rd;
                    assert_rd = assert_map.equal_range(expr);

                    std::pair<NodeclMap::iterator, NodeclMap::iterator> rd;
                    rd = analysis_map.equal_range(expr);

                    // Check whether the values are the same (or contain the RD UNKNOWN) or
                    // check for UNDEFINED reaching definitions
                    for (NodeclMap::iterator it_r = assert_rd.first; it_r != assert_rd.second; ++it_r)
                    {
                        NBase value = it_r->second.first;
                        bool found = false;
                        for (NodeclMap::iterator it_s = rd.first; it_s != rd.second && !found; ++it_s)
                        {
                            if (Nodecl::Utils::structurally_equal_nodecls(value, it_s->second.first, /*skip_conversions*/true)
                                    || ((value.prettyprint()=="UNKNOWN" || value.prettyprint()=="::UNKNOWN")
                                            && it_s->second.first.prettyprint()=="UNKNOWN"))
                                found = true;
                        }
                        if(!found)
                        {   // Check whether the assert value for this expression is "UNDEFINED"
                            ERROR_CONDITION(value.get_symbol().get_name() != "UNDEFINED", 
                                            "%s: Assertion 'reaching_definition_in(%s)' does not fulfill.\n"\
                                            "Variable '%s' do not have the value '%s' in the set of reaching definitions "\
                                            "computed during the analysis for node %d\n",
                                            locus_str.c_str(), Utils::prettyprint_nodecl_map(assert_map, /*dot*/ false).c_str(),
                                            expr.prettyprint().c_str(), value.prettyprint().c_str(), node_id)
                        }
                    }
                }
            }
        }
    }

    static Nodecl::List nodecl_list_difference(const Nodecl::List& assert_list, const Nodecl::List& analysis_list)
    {
        Nodecl::List res;
        for(Nodecl::List::const_iterator it = assert_list.begin(); it != assert_list.end(); ++it)
        {
            if(!Nodecl::Utils::nodecl_is_in_nodecl_list(*it, analysis_list))
                res.append(*it);
        }
        return res;
    }
    
    void compare_assert_list_with_analysis_list(
            const Nodecl::List& assert_list, const Nodecl::List& analysis_list,
            std::string locus_str, int node_id,
            std::string clause_name, std::string analysis_name)
    {
        if(assert_list.size() != analysis_list.size())
        {
            std::string assert_list_str = (assert_list.empty() ? "null" : assert_list.prettyprint());
            internal_error( "%s: Assertion '%s(%s)' does not fulfill.\n"\
                            "The number of %s variables associated to node %d is not the same as in the assert list\n",
                            locus_str.c_str( ),
                            clause_name.c_str( ), assert_list_str.c_str(),
                            analysis_name.c_str( ), node_id );
        }
        else
        {
            if(!assert_list.empty())
            {
                if((assert_list.size() == 1) &&
                (assert_list.begin()->is<Nodecl::Symbol>()) &&
                (assert_list.begin()->get_symbol().get_name()==analysis_none_sym_name))
                {
                    if(!analysis_list.empty())
                    {
                        internal_error("%s: Assertion '%s(%s)' does not fulfill.\n"\
                                    "There are %s variables associated to node %d\n",
                                    locus_str.c_str( ),
                                    clause_name.c_str( ), assert_list.prettyprint().c_str(),
                                    analysis_name.c_str( ), node_id );
                    }
                }
                else if(analysis_list.empty())
                {
                    internal_error( "%s: Assertion '%s(%s)' does not fulfill.\n"\
                                    "There are no %s variables associated to node %d\n",
                                    locus_str.c_str( ),
                                    clause_name.c_str( ), assert_list.prettyprint().c_str(),
                                    analysis_name.c_str( ), node_id );
                }
                else
                {
                    Nodecl::List diff = nodecl_list_difference( assert_list, analysis_list );
                    if( !diff.empty( ) )
                    {
                        internal_error( "%s: Assertion '%s(%s)' does not fulfill.\n"\
                                        "Expressions '%s' are no %s variables associated to node %d\n",
                                        locus_str.c_str( ),
                                        clause_name.c_str( ), assert_list.prettyprint().c_str(),
                                        diff.prettyprint().c_str(),
                                        analysis_name.c_str( ), node_id );
                    }
                }
            }
        }
    }
    
    void check_assertions_rec(Node* current)
    {
        if (!current->is_visited())
        {
            current->set_visited(true);

            // Treat current node
            std::string locus_str = "";
            if (current->is_graph_node())
                locus_str = current->get_graph_related_ast( ).get_locus_str( );
            else
            {
                ObjectList<NBase> stmts = current->get_statements();
                if( !stmts.empty( ) )
                    locus_str = stmts[0].get_locus_str( );
            }

            // Check UseDef analysis
            if (current->has_usage_assertion())
            {
                NodeclSet assert_ue = current->get_assert_ue_vars();
                NodeclSet assert_killed = current->get_assert_killed_vars();
                NodeclSet assert_undef = current->get_assert_undefined_behaviour_vars();
                if (current->is_context_node())
                {
                    // Consider the case:
                    //      #pragma analysis_check assert
                    //      #pragma omp task
                    // -> Context
                    //       |_____ Entry
                    //       |______Task Creation
                    //       |______Exit
                    // Although it could also contain any other nodes inside the context
                    Node* first_inner_node = current->get_graph_entry_node()->get_children()[0];
                    if (first_inner_node->is_omp_task_creation_node())
                    {
                        current = ExtensibleGraph::get_task_from_task_creation(first_inner_node);
                    }
                }
                NodeclSet ue = current->get_ue_vars();
                NodeclSet killed = current->get_killed_vars();
                NodeclSet undef = current->get_undefined_behaviour_vars();

                compare_assert_set_with_analysis_set( assert_ue, ue, locus_str, current->get_id( ), "upper_exposed", "Upper Exposed" );
                compare_assert_set_with_analysis_set( assert_killed, killed, locus_str, current->get_id( ), "defined", "Killed" );
                compare_assert_set_with_analysis_set( assert_undef, undef, locus_str, current->get_id( ), "undefined", "Undefined Behavior" );
            }

            // Check Liveness analysis
            if (current->has_liveness_assertion())
            {
                NodeclSet assert_live_in = current->get_assert_live_in_vars();
                NodeclSet assert_live_out = current->get_assert_live_out_vars();
                NodeclSet assert_dead = current->get_assert_dead_vars();
                if (current->is_context_node())
                {
                    // Consider the case:
                    //      #pragma analysis_check assert
                    //      #pragma omp task
                    // -> Context
                    //       |_____ Entry
                    //       |______Task Creation
                    //       |______Exit
                    // Although it could also contain any other nodes inside the context
                    Node* first_inner_node = current->get_graph_entry_node()->get_children()[0];
                    if (first_inner_node->is_omp_task_creation_node())
                    {
                        current = ExtensibleGraph::get_task_from_task_creation(first_inner_node);
                    }
                }
                NodeclSet live_in = current->get_live_in_vars();
                NodeclSet live_out = current->get_live_out_vars();

                compare_assert_set_with_analysis_set( assert_live_in, live_in, locus_str, current->get_id( ), "live_in", "Live In" );
                compare_assert_set_with_analysis_set( assert_live_out, live_out, locus_str, current->get_id( ), "live_out", "Live Out" );
                // Dead variables checking behaves a bit different, since we don't have a 'dead' set associated to each node
                if (!assert_dead.empty())
                {
                    for (NodeclSet::iterator it = assert_dead.begin(); it != assert_dead.end(); ++it)
                    {
                        if (Utils::nodecl_set_contains_nodecl(*it, live_in))
                        {
                            internal_error( "%s: Assertion 'dead(%s)' does not fulfill.\n"\
                                            "Expression '%s' is not Dead at the Entry point of node %d\n",
                                            locus_str.c_str(), Utils::prettyprint_nodecl_set(assert_dead, /*dot*/ false).c_str(),
                                            it->prettyprint().c_str(), current->get_id());
                        }
                    }
                }
            }

            // Check Reaching Definitions analysis
            if (current->has_reach_defs_assertion())
            {
                NodeclMap assert_reach_defs_in = current->get_assert_reaching_definitions_in();
                NodeclMap assert_reach_defs_out = current->get_assert_reaching_definitions_out();
                if (current->is_context_node())
                {
                    // Consider the case:
                    //      #pragma analysis_check assert
                    //      #pragma omp task
                    // -> Context
                    //       |_____ Entry
                    //       |______Task Creation
                    //       |______Exit
                    // Although it could also contain any other nodes inside the context
                    Node* first_inner_node = current->get_graph_entry_node()->get_children()[0];
                    if (first_inner_node->is_omp_task_creation_node())
                    {
                        current = ExtensibleGraph::get_task_from_task_creation(first_inner_node);
                    }
                }
                NodeclMap reach_defs_in = current->get_reaching_definitions_in();
                NodeclMap reach_defs_out = current->get_reaching_definitions_out();

                compare_assert_map_with_analysis_map( assert_reach_defs_in, reach_defs_in, locus_str, current->get_id( ),
                                                        "reaching_definition_in", "Input Reaching Definitions" );
                compare_assert_map_with_analysis_map( assert_reach_defs_out, reach_defs_out, locus_str, current->get_id( ),
                                                        "reaching_definition_out", "Output Reaching Definitions" );
            }

            // Induction Variables
            if (current->has_induction_vars_assertion())
            {
                Utils::InductionVarList assert_induction_vars = current->get_assert_induction_vars();
                // 'current' is the context created by the checking pragma -> get the inner loop node
                Node* inner_loop = current->get_graph_entry_node()->get_children()[0];
                if (!inner_loop->is_loop_node())
                {   // We may be in the "init" of a ForStatement
                    inner_loop = inner_loop->get_children()[0];
                }
                if (inner_loop->is_loop_node())
                {
                    if (!assert_induction_vars.empty())
                    {
                        Utils::InductionVarList induction_vars = inner_loop->get_induction_variables();
                        for (Utils::InductionVarList::iterator it = assert_induction_vars.begin();
                            it != assert_induction_vars.end(); ++it)
                        {
                            Utils::InductionVar* iv = *it;
                            NBase iv_nodecl = iv->get_variable();
                            bool found = false;
                            for (Utils::InductionVarList::iterator it2 = induction_vars.begin();
                                 it2 != induction_vars.end() && !found; ++it2)
                            {
                                if(Nodecl::Utils::structurally_equal_nodecls((*it2)->get_variable(), iv_nodecl, /*skip_conversions*/true))
                                {
                                    found = true;

                                    // Compare LBs
                                    const NodeclSet& lb_a = iv->get_lb();
                                    const NodeclSet& lb = (*it2)->get_lb();
                                    ERROR_CONDITION(lb_a.size() != lb.size(),
                                                    "%s: Assertion 'induction_var(%s)' does not fulfill.\n"
                                                    "Lower Bounds computed for induction variable '%s' "
                                                    "in node %d are '%s', but lower bounds indicated in the assertion are '%s'.\n",
                                                    locus_str.c_str(),
                                                    Utils::prettyprint_induction_vars(assert_induction_vars, /*to_dot*/ false).c_str(),
                                                    iv_nodecl.prettyprint().c_str(),
                                                    inner_loop->get_id(),
                                                    Utils::prettyprint_iv_boundary_list((*it2)->get_lb()).c_str(),
                                                    Utils::prettyprint_iv_boundary_list(iv->get_lb()).c_str());
                                    NodeclSet::const_iterator it_lb_a = lb_a.begin();
                                    NodeclSet::const_iterator it_lb = lb.begin();
                                    for ( ; it_lb_a != lb_a.end(); ++it_lb_a, ++it_lb)
                                    {
                                        ERROR_CONDITION(!Nodecl::Utils::structurally_equal_nodecls(*it_lb_a, *it_lb, /*skip_conversions*/true),
                                                        "%s: Assertion 'induction_var(%s)' does not fulfill.\n"
                                                        "Lower Bounds computed for induction variable '%s' "
                                                        "in node %d are '%s', but lower bounds indicated in the assertion are '%s'.\n",
                                                        locus_str.c_str(),
                                                        Utils::prettyprint_induction_vars(assert_induction_vars, /*to_dot*/ false).c_str(),
                                                        iv_nodecl.prettyprint().c_str(),
                                                        inner_loop->get_id(),
                                                        Utils::prettyprint_iv_boundary_list((*it2)->get_lb()).c_str(),
                                                        Utils::prettyprint_iv_boundary_list(iv->get_lb()).c_str());
                                    }

                                    // Compare UBs
                                    const NodeclSet& ub_a = iv->get_ub();
                                    const NodeclSet& ub = (*it2)->get_ub();
                                    ERROR_CONDITION(ub_a.size() != ub.size(),
                                                    "%s: Assertion 'induction_var(%s)' does not fulfill.\n"
                                                    "Upper Bounds computed for induction variable '%s' "
                                                    "in node %d are '%s', but the upper bounds indicated in the assertion are '%s'.\n",
                                                    locus_str.c_str(),
                                                    Utils::prettyprint_induction_vars(assert_induction_vars, /*to_dot*/ false).c_str(),
                                                    iv_nodecl.prettyprint().c_str(), inner_loop->get_id(),
                                                    Utils::prettyprint_iv_boundary_list((*it2)->get_ub()).c_str(),
                                                    Utils::prettyprint_iv_boundary_list(iv->get_ub()).c_str());
                                    NodeclSet::const_iterator it_ub_a = ub_a.begin();
                                    NodeclSet::const_iterator it_ub = ub.begin();
                                    for ( ; it_ub_a != ub_a.end(); ++it_ub_a, ++it_ub)
                                    {

                                        ERROR_CONDITION(!Nodecl::Utils::structurally_equal_nodecls(*it_ub_a, *it_ub, /*skip_conversions*/true),
                                                        "%s: Assertion 'induction_var(%s)' does not fulfill.\n"
                                                        "Upper Bounds computed for induction variable '%s' "
                                                        "in node %d are '%s', but the upper bounds indicated in the assertion are '%s'.\n",
                                                        locus_str.c_str(),
                                                        Utils::prettyprint_induction_vars(assert_induction_vars, /*to_dot*/ false).c_str(),
                                                        iv_nodecl.prettyprint().c_str(), inner_loop->get_id(),
                                                        Utils::prettyprint_iv_boundary_list((*it2)->get_ub()).c_str(),
                                                        Utils::prettyprint_iv_boundary_list(iv->get_ub()).c_str());
                                    }

                                    // Compare Strides
                                    ERROR_CONDITION(!Nodecl::Utils::structurally_equal_nodecls((*it2)->get_increment(), iv->get_increment(), /*skip_conversions*/true),
                                                    "%s: Assertion 'induction_var(%s)' does not fulfill.\n"
                                                    "Stride computed for induction variable '%s' "
                                                    "in node %d is '%s', but the stride indicated in the assertion is '%s'.\n",
                                                    locus_str.c_str(),
                                                    Utils::prettyprint_induction_vars(assert_induction_vars, /*to_dot*/ false).c_str(),
                                                    iv_nodecl.prettyprint().c_str(), inner_loop->get_id(),
                                                    (*it2)->get_increment().prettyprint().c_str(),
                                                    iv->get_increment().prettyprint().c_str());
                                }
                            }
                            ERROR_CONDITION(!found,
                                            "%s: Assertion 'induction_var(%s)' does not fulfill.\n"
                                            "Induction variable '%s' not found in the induction variables list of node %d\n",
                                            locus_str.c_str(), Utils::prettyprint_induction_vars(assert_induction_vars, /*to_dot*/ false).c_str(),
                                            iv_nodecl.prettyprint().c_str(), inner_loop->get_id());
                        }
                    }
                }
                else
                {
                    if (!assert_induction_vars.empty())
                    {
                        WARNING_MESSAGE("%s: warning: #pragma analysis_check assert induction_variables is only used "
                                        "when associated with a loop structure. Ignoring it when associated with any other statement.",
                                        locus_str.c_str());
                    }
                }
            }

            // Auto-scoping
            if (current->has_autoscope_assertion())
            {
                // Context
                //    |_____ Entry
                //    |______Task Creation
                //    |______Exit
                ERROR_CONDITION(!current->is_context_node(),
                                "Correctness assertion pragmas are expected to be associated with a Context node. '%s' found instead.\n",
                                (current->is_graph_node() ? current->get_graph_type_as_string() : current->get_type_as_string()).c_str());
                Node* task_creation = current->get_graph_entry_node()->get_children()[0];
                ERROR_CONDITION(!task_creation->is_omp_task_creation_node(),
                                "Correctness assertion pragmas' Context is expected to contain just a TaskCreation node. '%s' found instead.\n",
                                (task_creation->is_graph_node() ? task_creation->get_graph_type_as_string() : task_creation->get_type_as_string()).c_str());
                Node* task = ExtensibleGraph::get_task_from_task_creation(task_creation);

                NodeclSet assert_autosc_firstprivate = current->get_assert_auto_sc_firstprivate_vars();
                NodeclSet assert_autosc_private = current->get_assert_auto_sc_private_vars();
                NodeclSet assert_autosc_shared = current->get_assert_auto_sc_shared_vars();
                NodeclSet autosc_firstprivate = task->get_sc_firstprivate_vars();
                NodeclSet autosc_private = task->get_sc_private_vars();
                NodeclSet autosc_shared = task->get_sc_shared_vars();

                compare_assert_set_with_analysis_set( assert_autosc_firstprivate, autosc_firstprivate,
                                                      locus_str, task->get_id( ), "auto_sc_firstprivate", "AutoScope Firstprivate" );
                compare_assert_set_with_analysis_set( assert_autosc_private, autosc_private,
                                                      locus_str, task->get_id( ), "auto_sc_private", "AutoScope Private" );
                compare_assert_set_with_analysis_set( assert_autosc_shared, autosc_shared,
                                                      locus_str, task->get_id( ), "auto_sc_shared", "AutoScope Shared" );

            }

            // Correctness
            if (current->has_correctness_assertion())
            {
                // Context
                //    |_____ Entry
                //    |______Task Creation
                //    |______Exit
                ERROR_CONDITION(!current->is_context_node(),
                                "Correctness assertion pragmas are expected to be associated with a Context node. '%s' found instead.\n",
                                (current->is_graph_node() ? current->get_graph_type_as_string() : current->get_type_as_string()).c_str());
                Node* task_creation = current->get_graph_entry_node()->get_children()[0];
                ERROR_CONDITION(!task_creation->is_omp_task_creation_node(),
                                "Correctness assertion pragmas' Context is expected to contain just a TaskCreation node. '%s' found instead.\n",
                                (task_creation->is_graph_node() ? task_creation->get_graph_type_as_string() : task_creation->get_type_as_string()).c_str());
                Node* task = ExtensibleGraph::get_task_from_task_creation(task_creation);

                const Nodecl::List& assert_correctness_auto_storage = current->get_assert_correctness_auto_storage_vars();
                const Nodecl::List& assert_correctness_dead = current->get_assert_correctness_dead_vars();
                const Nodecl::List& assert_correctness_incoherent_fp = current->get_assert_correctness_incoherent_fp_vars();
                const Nodecl::List& assert_correctness_incoherent_p = current->get_assert_correctness_incoherent_p_vars();
                const Nodecl::List& assert_correctness_incoherent_in = current->get_assert_correctness_incoherent_in_vars();
                const Nodecl::List& assert_correctness_incoherent_in_pointed = current->get_assert_correctness_incoherent_in_pointed_vars();
                const Nodecl::List& assert_correctness_incoherent_out = current->get_assert_correctness_incoherent_out_vars();
                const Nodecl::List& assert_correctness_incoherent_out_pointed = current->get_assert_correctness_incoherent_out_pointed_vars();
                const Nodecl::List& assert_correctness_race = current->get_assert_correctness_race_vars();
                const Nodecl::List& correctness_auto_storage = task->get_correctness_auto_storage_vars();
                const Nodecl::List& correctness_dead = task->get_correctness_dead_vars();
                const Nodecl::List& correctness_incoherent_fp = task->get_correctness_incoherent_fp_vars();
                const Nodecl::List& correctness_incoherent_p = task->get_correctness_incoherent_p_vars();
                const Nodecl::List& correctness_incoherent_in = task->get_correctness_incoherent_in_vars();
                const Nodecl::List& correctness_incoherent_in_pointed = task->get_correctness_incoherent_in_pointed_vars();
                const Nodecl::List& correctness_incoherent_out = task->get_correctness_incoherent_out_vars();
                const Nodecl::List& correctness_incoherent_out_pointed = task->get_correctness_incoherent_out_pointed_vars();
                const Nodecl::List& correctness_race = task->get_correctness_race_vars();

                compare_assert_list_with_analysis_list(assert_correctness_auto_storage, correctness_auto_storage,
                                                       locus_str, task->get_id(), "correctness_auto_storage", "Correctness Automatic Storage");
                compare_assert_list_with_analysis_list(assert_correctness_dead, correctness_dead,
                                                       locus_str, task->get_id(), "correctness_dead", "Correctness Dead");
                compare_assert_list_with_analysis_list(assert_correctness_incoherent_fp, correctness_incoherent_fp,
                                                       locus_str, task->get_id(), "correctness_incoherent_firstprivate", "Correctness Incoherent Firstprivate Data-Sharing");
                compare_assert_list_with_analysis_list(assert_correctness_incoherent_p, correctness_incoherent_p,
                                                       locus_str, task->get_id(), "correctness_incoherent_private", "Correctness Incoherent Private Data-Sharing");
                compare_assert_list_with_analysis_list(assert_correctness_incoherent_in, correctness_incoherent_in,
                                                       locus_str, task->get_id(), "correctness_incoherent_in", "Correctness Incoherent In Dependency");
                compare_assert_list_with_analysis_list(assert_correctness_incoherent_in_pointed, correctness_incoherent_in_pointed,
                                                       locus_str, task->get_id(), "correctness_incoherent_in_pointed", "Correctness Incoherent In Pointed Dependency");
                compare_assert_list_with_analysis_list(assert_correctness_incoherent_out, correctness_incoherent_out,
                                                       locus_str, task->get_id(), "correctness_incoherent_out", "Correctness Incoherent Out Dependency");
                compare_assert_list_with_analysis_list(assert_correctness_incoherent_out_pointed, correctness_incoherent_out_pointed,
                                                       locus_str, task->get_id(), "correctness_incoherent_out_pointed", "Correctness Incoherent Out Pointed Dependency");
                compare_assert_list_with_analysis_list(assert_correctness_race, correctness_race,
                                                       locus_str, task->get_id(), "correctness_race", "Correctness Race Condition");
            }
            
            // Recursively visit inner nodes
            if (current->is_graph_node())
            {
                check_assertions_rec(current->get_graph_entry_node());
            }

            // Recursively visit current children
            const ObjectList<Node*>& children = current->get_children();
            for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
            {
                check_assertions_rec(*it);
            }
        }
    }

    void check_pragma_clauses( PragmaCustomLine pragma_line, const locus_t* loc, Nodecl::List& environment )
    {
        // Use-Def analysis clauses
        // #pragma analysis_check assert upper_exposed(expr-list)
        if( pragma_line.get_clause( "upper_exposed" ).is_defined( ) )
        {
            PragmaCustomClause upper_exposed_clause = pragma_line.get_clause( "upper_exposed" );

            environment.append(
                Nodecl::Analysis::UpperExposed::make(
                    Nodecl::List::make( upper_exposed_clause.get_arguments_as_expressions( ) ), loc ) );
        }
        if( pragma_line.get_clause( "defined" ).is_defined( ) )
        {
            PragmaCustomClause defined_clause = pragma_line.get_clause( "defined" );

            environment.append(
                Nodecl::Analysis::Defined::make(
                    Nodecl::List::make( defined_clause.get_arguments_as_expressions( ) ), loc ) );
        }
        if( pragma_line.get_clause( "undefined" ).is_defined( ) )
        {
            PragmaCustomClause undefined_clause = pragma_line.get_clause( "undefined" );

            environment.append(
                Nodecl::Analysis::Undefined::make(
                    Nodecl::List::make( undefined_clause.get_arguments_as_expressions( ) ), loc ) );
        }

        // Liveness analysis clauses
        // #pragma analysis_check assert live_in(expr-list)
        if( pragma_line.get_clause( "live_in" ).is_defined( ) )
        {
            PragmaCustomClause live_in_clause = pragma_line.get_clause( "live_in" );

            environment.append(
                Nodecl::Analysis::LiveIn::make(
                    Nodecl::List::make( live_in_clause.get_arguments_as_expressions( ) ), loc ) );
        }
        if( pragma_line.get_clause( "live_out" ).is_defined( ) )
        {
            PragmaCustomClause live_out_clause = pragma_line.get_clause( "live_out" );

            environment.append(
                Nodecl::Analysis::LiveOut::make(
                    Nodecl::List::make( live_out_clause.get_arguments_as_expressions( ) ), loc ) );
        }
        if( pragma_line.get_clause( "dead" ).is_defined( ) )
        {
            PragmaCustomClause dead_clause = pragma_line.get_clause( "dead" );

            environment.append(
                Nodecl::Analysis::Dead::make(
                    Nodecl::List::make( dead_clause.get_arguments_as_expressions( ) ), loc ) );
        }

        // Reaching definition analysis clauses
        // #pragma analysis_check assert reaching_definition_in(<expr:expr-list>-list)
        if( pragma_line.get_clause( "reaching_definition_in" ).is_defined( ) )
        {
            PragmaCustomClause reach_defs_in_clause = pragma_line.get_clause( "reaching_definition_in" );
            Nodecl::List reach_defs_in =
                extract_reaching_definitions_map_from_clause( reach_defs_in_clause,
                                                              pragma_line.retrieve_context( ) );

            environment.append( Nodecl::Analysis::ReachingDefinitionIn::make( reach_defs_in, loc ) );
        }
        if( pragma_line.get_clause( "reaching_definition_out" ).is_defined( ) )
        {
            PragmaCustomClause reach_defs_out_clause = pragma_line.get_clause( "reaching_definition_out" );

            Nodecl::List reach_defs_out =
                extract_reaching_definitions_map_from_clause( reach_defs_out_clause,
                                                              pragma_line.retrieve_context( ) );

            environment.append( Nodecl::Analysis::ReachingDefinitionOut::make( reach_defs_out, loc ) );
        }

        // Induction variables analysis clauses
        if( pragma_line.get_clause( "induction_var" ).is_defined( ) )
        {
            PragmaCustomClause induction_vars_clause = pragma_line.get_clause( "induction_var" );

            Nodecl::List induction_vars =
                extract_induction_variable_info_from_clause( induction_vars_clause,
                                                             pragma_line.retrieve_context( ) );

            environment.append( Nodecl::Analysis::InductionVariable::make( induction_vars, loc ) );
        }

        // Auto-scope analysis clauses
        if( pragma_line.get_clause( "auto_sc_firstprivate" ).is_defined( ) )
        {
            PragmaCustomClause auto_sc_fp_vars_clause = pragma_line.get_clause( "auto_sc_firstprivate" );

            environment.append(
                Nodecl::Analysis::AutoScope::Firstprivate::make(
                    Nodecl::List::make( auto_sc_fp_vars_clause.get_arguments_as_expressions( ) ), loc ) );
        }
        if( pragma_line.get_clause( "auto_sc_private" ).is_defined( ) )
        {
            PragmaCustomClause auto_sc_p_vars_clause = pragma_line.get_clause( "auto_sc_private" );

            environment.append(
                Nodecl::Analysis::AutoScope::Private::make(
                    Nodecl::List::make( auto_sc_p_vars_clause.get_arguments_as_expressions( ) ), loc ) );
        }
        if( pragma_line.get_clause( "auto_sc_shared" ).is_defined( ) )
        {
            PragmaCustomClause auto_sc_s_vars_clause = pragma_line.get_clause( "auto_sc_shared" );

            environment.append(
                Nodecl::Analysis::AutoScope::Shared::make(
                    Nodecl::List::make( auto_sc_s_vars_clause.get_arguments_as_expressions( ) ), loc ) );
        }
        
        // Correctness clauses
        if(pragma_line.get_clause("correctness_auto_storage").is_defined())
        {
            PragmaCustomClause correctness_auto_storage_clause = pragma_line.get_clause("correctness_auto_storage");
            environment.append(
                Nodecl::Analysis::Correctness::AutoStorage::make(
                    Nodecl::List::make(correctness_auto_storage_clause.get_arguments_as_expressions( ) ), loc ));
        }
        if(pragma_line.get_clause("correctness_dead").is_defined())
        {
            PragmaCustomClause correctness_dead_clause = pragma_line.get_clause("correctness_dead");
            environment.append(
                Nodecl::Analysis::Correctness::Dead::make(
                    Nodecl::List::make(correctness_dead_clause.get_arguments_as_expressions( ) ), loc ));
        }
        if(pragma_line.get_clause("correctness_auto_storage").is_defined())
        {
            PragmaCustomClause correctness_auto_storage_clause = pragma_line.get_clause("correctness_auto_storage");
            environment.append(
                Nodecl::Analysis::Correctness::AutoStorage::make(
                    Nodecl::List::make(correctness_auto_storage_clause.get_arguments_as_expressions( ) ), loc ));
        }
        if(pragma_line.get_clause("correctness_incoherent_fp").is_defined())
        {
            PragmaCustomClause correctness_incoherent_fp_clause = pragma_line.get_clause("correctness_incoherent_fp");
            environment.append(
                Nodecl::Analysis::Correctness::IncoherentFp::make(
                    Nodecl::List::make(correctness_incoherent_fp_clause.get_arguments_as_expressions( ) ), loc ));
        }
        if(pragma_line.get_clause("correctness_incoherent_p").is_defined())
        {
            PragmaCustomClause correctness_incoherent_p_clause = pragma_line.get_clause("correctness_incoherent_p");
            environment.append(
                Nodecl::Analysis::Correctness::IncoherentP::make(
                    Nodecl::List::make(correctness_incoherent_p_clause.get_arguments_as_expressions( ) ), loc ));
        }
        if(pragma_line.get_clause("correctness_incoherent_in").is_defined())
        {
            PragmaCustomClause correctness_incoherent_in_clause = pragma_line.get_clause("correctness_incoherent_in");
            environment.append(
                Nodecl::Analysis::Correctness::IncoherentIn::make(
                    Nodecl::List::make(correctness_incoherent_in_clause.get_arguments_as_expressions( ) ), loc ));
        }
        if(pragma_line.get_clause("correctness_incoherent_in_pointed").is_defined())
        {
            PragmaCustomClause correctness_incoherent_in_pointed_clause = pragma_line.get_clause("correctness_incoherent_in_pointed");
            environment.append(
                Nodecl::Analysis::Correctness::IncoherentInPointed::make(
                    Nodecl::List::make(correctness_incoherent_in_pointed_clause.get_arguments_as_expressions( ) ), loc ));
        }
        if(pragma_line.get_clause("correctness_incoherent_out").is_defined())
        {
            PragmaCustomClause correctness_incoherent_out_clause = pragma_line.get_clause("correctness_incoherent_out");
            environment.append(
                Nodecl::Analysis::Correctness::IncoherentOut::make(
                    Nodecl::List::make(correctness_incoherent_out_clause.get_arguments_as_expressions( ) ), loc ));
        }
        if(pragma_line.get_clause("correctness_incoherent_out_pointed").is_defined())
        {
            PragmaCustomClause correctness_incoherent_out_pointed_clause = pragma_line.get_clause("correctness_incoherent_out_pointed");
            environment.append(
                Nodecl::Analysis::Correctness::IncoherentOutPointed::make(
                    Nodecl::List::make(correctness_incoherent_out_pointed_clause.get_arguments_as_expressions( ) ), loc ));
        }
        if(pragma_line.get_clause("correctness_race").is_defined())
        {
            PragmaCustomClause correctness_race_clause = pragma_line.get_clause("correctness_race");
            environment.append(
                Nodecl::Analysis::Correctness::Race::make(
                    Nodecl::List::make(correctness_race_clause.get_arguments_as_expressions( ) ), loc ));
        }
    }
}

    AnalysisCheckPhase::AnalysisCheckPhase( )
        : PragmaCustomCompilerPhase("analysis_check"), _correctness_log_path("")
    {
        set_phase_name( "Phase checking the correctness of different analysis" );
        set_phase_description( "This phase checks first the robustness of a PCFG and then "\
                                " the correctness of different analysis based on user defined pragmas." );

        // Register constructs
        register_construct("assert");
        register_construct("assert_decl");

        dispatcher( ).statement.pre["assert"].connect( functor( &AnalysisCheckPhase::assert_handler_pre, *this ) );
        dispatcher( ).statement.post["assert"].connect( functor( &AnalysisCheckPhase::assert_handler_post, *this ) );
        dispatcher( ).declaration.pre["assert_decl"].connect( functor( &AnalysisCheckPhase::assert_decl_handler_pre, *this ) );
        dispatcher( ).declaration.post["assert_decl"].connect( functor( &AnalysisCheckPhase::assert_decl_handler_post, *this ) );
        
        // Register parameters
        register_parameter("correctness_log_dir",
                           "Sets the path where correctness logs will be stored, in addition to showing them in the standard output",
                           _correctness_log_path,
                           "");

        register_parameter("ompss_mode",
                           "Enables OmpSs semantics instead of OpenMP semantics",
                           _ompss_mode_str,
                           "0").connect(functor(&AnalysisCheckPhase::set_ompss_mode, *this));
    }

    void AnalysisCheckPhase::assert_handler_pre( TL::PragmaCustomStatement directive )
    {   // Nothing to be done
    }

    void AnalysisCheckPhase::assert_handler_post( TL::PragmaCustomStatement directive )
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line( );
        const locus_t* loc = directive.get_locus( );
        Nodecl::List environment;
        check_pragma_clauses( pragma_line, loc, environment );
        Nodecl::Analysis::Assert assert_nodecl = Nodecl::Analysis::Assert::make(
                directive.get_statements( ), environment, directive.get_locus( ) );

        pragma_line.diagnostic_unused_clauses( );
        directive.replace( assert_nodecl );
    }

    void AnalysisCheckPhase::assert_decl_handler_pre( TL::PragmaCustomDeclaration directive )
    {   // Nothing to be done
    }

    void AnalysisCheckPhase::assert_decl_handler_post( TL::PragmaCustomDeclaration directive )
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line( );
        const locus_t* loc = directive.get_locus( );

        Nodecl::List environment;
        check_pragma_clauses( pragma_line, loc, environment );
        Nodecl::Analysis::AssertDecl assert_nodecl = Nodecl::Analysis::AssertDecl::make(
                environment, directive.get_symbol( ), directive.get_locus( ) );

        pragma_line.diagnostic_unused_clauses( );
        directive.replace( assert_nodecl );
    }

    void AnalysisCheckPhase::pre_run( TL::DTO& dto )
    {
        // Add a new symbol to the empty translation unit that can be used in this phase
        // to specify that an analysis set must be empty
        TL::Scope sc = CURRENT_COMPILED_FILE->global_decl_context;
        TL::Symbol none_symbol = sc.new_symbol(analysis_none_sym_name);
        none_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        none_symbol.get_internal_symbol()->type_information = ::get_void_type();
    }
    
    void AnalysisCheckPhase::run(TL::DTO& dto)
    {
        PragmaCustomCompilerPhase::run(dto);

        NBase ast = dto["nodecl"];

        // 1.- Execute analyses
        // 1.1.- Compute all data-flow analysis
        // FIXME We should launch the analyses depending on the clauses in the assert directives
        AnalysisBase analysis(_ompss_mode_enabled);
        analysis.all_analyses(ast);
        // 1.2.- Execute correctness phase, which can also be checked
        // FIXME We should only execute this is there are assert clauses checking this information
        TL::OpenMP::launch_correctness(analysis, _correctness_log_path);
        
        // 2.- Perform checks
        const ObjectList<ExtensibleGraph*> pcfgs = analysis.get_pcfgs();
        // 2.1.- Check PCFG consistency
        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if (VERBOSE)
                printf("Check PCFG '%s' consistency\n", (*it)->get_name().c_str());
            check_pcfg_consistency(*it);
        }
        // 2.2.- Check user assertions
        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if (VERBOSE)
            {
                analysis.print_pcfg((*it)->get_name());
                printf("Check analysis assertions of PCFG '%s'\n", (*it)->get_name().c_str());
            }
            check_analysis_assertions(*it);
        }

        // 3.- Remove the nodes added in this phase
        AnalysisCheckVisitor v;
        v.walk(ast);
    }

    void AnalysisCheckPhase::check_pcfg_consistency( ExtensibleGraph* graph )
    {
        Node* graph_node = graph->get_graph( );
        check_task_synchronizations( graph_node );
        ExtensibleGraph::clear_visits( graph_node );

    }

    void AnalysisCheckPhase::check_analysis_assertions( ExtensibleGraph* graph )
    {
        Node* graph_node = graph->get_graph( );
        check_assertions_rec( graph_node );
        ExtensibleGraph::clear_visits( graph_node );
    }

    void AnalysisCheckPhase::set_ompss_mode(const std::string& ompss_mode_str)
    {
        if(ompss_mode_str == "1")
            _ompss_mode_enabled = true;
    }
    
    void AnalysisCheckVisitor::visit( const Nodecl::Analysis::Assert& n )
    {
        Nodecl::Utils::remove_from_enclosing_list( n );
    }

    void AnalysisCheckVisitor::visit( const Nodecl::Analysis::AssertDecl& n )
    {
        Nodecl::Utils::remove_from_enclosing_list( n );
    }
}
}

EXPORT_PHASE(TL::Analysis::AnalysisCheckPhase);
