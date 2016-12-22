/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include <climits>

#include "cxx-codegen.h"
#include "cxx-process.h"

#include "tl-analysis-utils.hpp"
#include "tl-edge.hpp"
#include "tl-iv-analysis.hpp"
#include "tl-node.hpp"

namespace TL {
namespace Analysis {

    // ****************************************************************************** //
    // ******************************** Constructors ******************************** //

    Node::Node(unsigned int& id, NodeType type, Node* outer_node)
            : _id(++id), _num(), _type(type), _outer_node(outer_node),
              _entry_edges(), _exit_edges(), _has_assertion(false),
              _visited(false), _visited_aux(false), _visited_extgraph(false), _visited_extgraph_aux(false)
    {
        if (type == __Graph)
        {
            set_data(_ENTRY_NODE, new Node(id, __Entry, NULL));
            unsigned int exit_id = INT_MAX - 1;
            set_data(_EXIT_NODE, new Node(exit_id, __Exit, NULL));
        }
    }

    Node::Node(unsigned int& id, NodeType type, Node* outer_node, const NodeclList& nodecls)
            : _id(++id), _num(),_type(type), _outer_node(outer_node),
              _entry_edges(), _exit_edges(), _has_assertion(false),
              _visited(false), _visited_aux(false), _visited_extgraph(false), _visited_extgraph_aux(false)
    {
        set_data(_NODE_STMTS, nodecls);
    }

    Node::Node(unsigned int& id, NodeType type, Node* outer_node, const NBase& nodecl)
            : _id(++id), _num(),_type(type), _outer_node(outer_node),
              _entry_edges(), _exit_edges(), _has_assertion(false),
              _visited(false), _visited_aux(false), _visited_extgraph(false), _visited_extgraph_aux(false)
    {
        set_data(_NODE_STMTS, NodeclList(1, nodecl));
    }

    // ****************************** END Constructors ****************************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ************************ Data-members getters/setters ************************ //

    bool Node::operator==(const Node& node) const
    {
        return (_id == node._id);
    }

    unsigned int Node::get_id() const
    {
        return _id;
    }

    void Node::set_id(unsigned int id)
    {
        _id = id;
    }

    NodeType Node::get_type() const
    {
        return _type;
    }

    void Node::set_type(NodeType type)
    {
        _type = type;
    }

    Node* Node::get_outer_node() const
    {
        return _outer_node;
    }

    void Node::set_outer_node(Node* node)
    {
        _outer_node = node;
    }

    const EdgeList& Node::get_entry_edges() const
    {
        return _entry_edges;
    }

    void Node::set_entry_edge(Edge *entry_edge)
    {
        _entry_edges.append(entry_edge);
    }

    EdgeTypeList Node::get_entry_edge_types()
    {
        EdgeTypeList result;
        for (EdgeList::iterator it = _entry_edges.begin(); it != _entry_edges.end(); ++it)
            result.append((*it)->get_type());
        return result;
    }

    NodeclList Node::get_entry_edge_labels()
    {
        NodeclList result;
        for (EdgeList::iterator it = _entry_edges.begin(); it != _entry_edges.end(); ++it)
            result.append((*it)->get_label());
        return result;
    }

    NodeList Node::get_parents()
    {
        NodeList result;
        for (EdgeList::iterator it = _entry_edges.begin(); it != _entry_edges.end(); ++it)
            result.append((*it)->get_source());
        return result;
    }

    const EdgeList& Node::get_exit_edges() const
    {
        return _exit_edges;
    }

    void Node::set_exit_edge(Edge *exit_edge)
    {
        _exit_edges.append(exit_edge);
    }

    EdgeTypeList Node::get_exit_edge_types()
    {
        EdgeTypeList result;
        for (EdgeList::iterator it = _exit_edges.begin(); it != _exit_edges.end(); ++it)
            result.append((*it)->get_type());
        return result;
    }

    NodeclList Node::get_exit_edge_labels()
    {
        NodeclList result;
        for (EdgeList::iterator it = _exit_edges.begin(); it != _exit_edges.end(); ++it)
            result.append((*it)->get_label());
        return result;
    }

    Edge* Node::get_exit_edge(Node* target)
    {
        Edge* result = NULL;
        for (EdgeList::iterator it = _exit_edges.begin(); it != _exit_edges.end(); ++it)
        {
            if ((*it)->get_target() == target)
            {
                result = *it;
                break;
            }
        }
        return result;
    }

    NodeList Node::get_children()
    {
        NodeList result;
        for (EdgeList::iterator it = _exit_edges.begin(); it != _exit_edges.end(); ++it)
            result.append((*it)->get_target());
        return result;
    }

    bool Node::is_visited() const
    {
        return _visited;
    }

    bool Node::is_visited_aux() const
    {
        return _visited_aux;
    }

    bool Node::is_visited_extgraph() const
    {
        return _visited_extgraph;
    }

    bool Node::is_visited_extgraph_aux() const
    {
        return _visited_extgraph_aux;
    }

    void Node::set_visited(bool visited)
    {
        _visited = visited;
    }

    void Node::set_visited_aux(bool visited)
    {
        _visited_aux = visited;
    }

    void Node::set_visited_extgraph(bool visited)
    {
        _visited_extgraph = visited;
    }

    void Node::set_visited_extgraph_aux(bool visited)
    {
        _visited_extgraph_aux = visited;
    }

    int Node::get_num() const
    {
        return _num;
    }

    void Node::set_num(int num)
    {
        _num = num;
    }

    // ********************** END Data-members getters/setters ********************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // *********************** Linked-members getters/setters *********************** //

    //! Returns a string with the graph type of the node.
    inline std::string graph_node_type_to_str(GraphType gt)
    {
        switch(gt)
        {
            #undef GRAPH_TYPE
            #define GRAPH_TYPE(X) case __##X : return #X;
            GRAPH_NODE_TYPE_LIST
            #undef GRAPH_TYPE
            default: WARNING_MESSAGE("Unexpected type of graph node '%d'", gt);
        }
        return "";
    }

    std::string Node::get_graph_type_as_string()
    {
        std::string graph_type = "";
        if (has_key(_GRAPH_TYPE))
        {
            GraphType type = get_data<GraphType>(_GRAPH_TYPE);
            graph_type = graph_node_type_to_str(type);
        }
        else
        {
            internal_error("The node '%s' is not a graph node, this operation is not allowed", 0);
        }

        return graph_type;
    }

    Node* Node::get_graph_entry_node()
    {
        Node* entry_node;
        if (is_graph_node())
            entry_node = get_data<Node*>(_ENTRY_NODE);
        else
            internal_error("Asking for the Entry Node of a non GRAPH node. Nodes of type '%s' do not have Entry node.",
                             get_type_as_string().c_str());
        return entry_node;
    }

    void Node::set_graph_entry_node(Node* node)
    {
        if (!node->is_entry_node())
        {
            internal_error("Unexpected node type '%s' while setting the entry node to node '%d'. ENTRY expected.",
                            get_type_as_string().c_str(), _id);
        }
        else if (is_graph_node())
            set_data(_ENTRY_NODE, node);
        else
            internal_error("Unexpected node type '%s' while setting the entry node to node '%d'. GRAPH expected.",
                            get_type_as_string().c_str(), _id);
    }

    Node* Node::get_graph_exit_node()
    {
        Node* exit_node;
        if (is_graph_node())
            exit_node = get_data<Node*>(_EXIT_NODE);
        else
            internal_error("Asking for the Entry Node of a non GRAPH node. Nodes of type '%s' do not have Exit node.",
                             get_type_as_string().c_str());
        return exit_node;
    }

    void Node::set_graph_exit_node(Node* node)
    {
        if (!node->is_exit_node())
        {
            internal_error("Unexpected node type '%s' while setting the exit node to node '%d'. EXIT expected.",
                            get_type_as_string().c_str(), _id);
        }
        else if (is_graph_node())
            set_data(_EXIT_NODE, node);
        else
            internal_error("Unexpected node type '%s' while setting the exit node to node '%d'. GRAPH expected.",
                            get_type_as_string().c_str(), _id);
    }

    NBase Node::get_graph_related_ast()
    {
        if (_type == __Graph)
        {
            NBase res = NBase::null();
            if (has_key(_NODE_LABEL))
                res = get_data<NBase>(_NODE_LABEL, NBase::null());
            return res;
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the label to node '%d'",
                            get_type_as_string().c_str(), _id);
        }
    }

    void Node::set_graph_label(NBase n)
    {
        if (_type == __Graph)
            set_data(_NODE_LABEL, n);
        else
            internal_error("Unexpected node type '%s' while setting the label to node '%d'. GRAPH expected.",
                            get_type_as_string().c_str(), _id);
    }

    GraphType Node::get_graph_type()
    {
        if (_type == __Graph)
            return get_data<GraphType>(_GRAPH_TYPE);
        else
            internal_error("Unexpected node type '%s' while getting graph type to node '%d'. GRAPH expected.",
                           get_type_as_string().c_str(), _id);
    }

    void Node::set_graph_type(GraphType graph_type)
    {
        if (_type == __Graph)
            set_data(_GRAPH_TYPE, graph_type);
        else
            internal_error("Unexpected node type '%s' while setting graph type to node '%d'. GRAPH expected.",
                            get_type_as_string().c_str(), _id);
    }
    static bool node_is_omp_graph_with_clause(GraphType type)
    {
        return (type == __OmpAtomic || __OmpCritical
                 || type == __OmpLoop || type == __OmpParallel  || type == __OmpSections
                 || type == __OmpSingle || type == __OmpTask);
    }

    PCFGPragmaInfo Node::get_pragma_node_info()
    {
        if ((is_graph_node() && node_is_omp_graph_with_clause(get_graph_type()))
            || _type == __OmpFlush)
        {
            if (has_key(_OMP_INFO))
                return get_data<PCFGPragmaInfo>(_OMP_INFO);
            else
            {
                PCFGPragmaInfo p;
                return p;
            }
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the OmpSs. OMP node expected.",
                            get_type_as_string().c_str());
        }
    }

    void Node::set_pragma_node_info(const PCFGPragmaInfo& pragma)
    {
        if ((is_graph_node() && node_is_omp_graph_with_clause(get_graph_type()))
            || _type == __OmpFlush)
        {
            set_data<PCFGPragmaInfo>(_OMP_INFO, pragma);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the OmpSs node info. OMP node expected",
                            get_type_as_string().c_str());
        }
    }

    bool Node::has_statements()
    {
        return has_key(_NODE_STMTS);
    }

    NodeclList Node::get_statements()
    {
        NodeclList stmts;
        if (has_key(_NODE_STMTS))
            stmts = get_data<NodeclList >(_NODE_STMTS);
        return stmts;
    }

    void Node::set_statements(NodeclList stmts)
    {
        if ((is_normal_node() || is_function_call_node()
            || is_labeled_node() || is_goto_node()
            || is_break_node() || is_continue_node())
           && !stmts.empty())
        {
            set_data(_NODE_STMTS, stmts);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the statements to node '%d'",
                            get_type_as_string().c_str(), _id);
        }
    }

    Symbol Node::get_label()
    {
        if (_type == __Goto || _type == __Labeled)
            return get_data<Symbol>(_NODE_LABEL);
        else
            internal_error("Unexpected node type '%s' while getting the label to node '%d'. GOTO or LABELED NODES expected.",
                            get_type_as_string().c_str(), _id);
    }

    void Node::set_label(Symbol s)
    {
        if (_type == __Goto || _type == __Labeled)
            set_data(_NODE_LABEL, s);
        else
            internal_error("Unexpected node type '%s' while setting the label to node '%d'. GOTO or LABELED NODES expected.",
                            get_type_as_string().c_str(), _id);
    }

    ASM_node_info Node::get_asm_info()
    {
        Node* outer_node = get_outer_node();
        if (get_type() == __AsmOp
            || (outer_node != NULL && outer_node->get_graph_type() == __AsmDef))
        {
            return get_data<ASM_node_info>(_ASM_INFO);
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the ASM info from node '%d'. ASM node expected.",
                            get_type_as_string().c_str(), _id);
        }
    }

    void Node::set_asm_info(ASM_node_info inf)
    {
        Node* outer_node = get_outer_node();
        if (get_type() == __AsmOp
            || (outer_node != NULL && outer_node->get_graph_type() == __AsmDef))
        {
            set_data(_ASM_INFO, inf);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the ASM info to node '%d'. ASM node expected.",
                            get_type_as_string().c_str(), _id);
        }
    }

    bool Node::has_usage_assertion() const
    {
        return (has_key(_ASSERT_UPPER_EXPOSED) || has_key(_ASSERT_KILLED) || has_key(_ASSERT_UNDEFINED));
    }

    bool Node::has_upper_exposed_assertion() const
    {
        return has_key(_ASSERT_UPPER_EXPOSED);
    }

    bool Node::has_defined_assertion() const
    {
        return has_key(_ASSERT_KILLED);
    }

    bool Node::has_undefined_assertion() const
    {
        return has_key(_ASSERT_UNDEFINED);
    }

    bool Node::has_liveness_assertion() const
    {
        return (has_key(_ASSERT_LIVE_IN) || has_key(_ASSERT_LIVE_OUT) || has_key(_ASSERT_DEAD));
    }

    bool Node::has_live_in_assertion() const
    {
        return has_key(_ASSERT_LIVE_IN);
    }

    bool Node::has_live_out_assertion() const
    {
        return has_key(_ASSERT_LIVE_OUT);
    }

    bool Node::has_dead_assertion() const
    {
        return has_key(_ASSERT_DEAD);
    }

    bool Node::has_reach_defs_assertion() const
    {
        return (has_key(_ASSERT_REACH_DEFS_IN) || has_key(_ASSERT_REACH_DEFS_OUT));
    }

    bool Node::has_reach_defs_in_assertion() const
    {
        return has_key(_ASSERT_REACH_DEFS_IN);
    }

    bool Node::has_reach_defs_out_assertion() const
    {
        return has_key(_ASSERT_REACH_DEFS_OUT);
    }

    bool Node::has_induction_vars_assertion() const
    {
        return has_key(_ASSERT_INDUCTION_VARS);
    }

    bool Node::has_autoscope_assertion() const
    {
        return (has_key(_ASSERT_AUTOSC_FIRSTPRIVATE)
                    || has_key(_ASSERT_AUTOSC_PRIVATE)
                    || has_key(_ASSERT_AUTOSC_SHARED));
    }

    bool Node::has_autoscope_fp_assertion() const
    {
        return has_key(_ASSERT_AUTOSC_FIRSTPRIVATE);
    }

    bool Node::has_autoscope_p_assertion() const
    {
        return has_key(_ASSERT_AUTOSC_PRIVATE);
    }

    bool Node::has_autoscope_s_assertion() const
    {
        return has_key(_ASSERT_AUTOSC_SHARED);
    }

    bool Node::has_range_assertion() const
    {
        return has_key(_ASSERT_RANGE);
    }

    bool Node::has_correctness_assertion() const
    {
        return (has_key(_ASSERT_CORRECTNESS_AUTO_STORAGE_VARS) ||
                has_key(_ASSERT_CORRECTNESS_INCOHERENT_FP_VARS) ||
                has_key(_ASSERT_CORRECTNESS_INCOHERENT_IN_VARS) ||
                has_key(_ASSERT_CORRECTNESS_INCOHERENT_IN_POINTED_VARS) ||
                has_key(_ASSERT_CORRECTNESS_INCOHERENT_OUT_VARS) ||
                has_key(_ASSERT_CORRECTNESS_INCOHERENT_OUT_POINTED_VARS) ||
                has_key(_ASSERT_CORRECTNESS_INCOHERENT_P_VARS) ||
                has_key(_ASSERT_CORRECTNESS_RACE_VARS) ||
                has_key(_ASSERT_CORRECTNESS_DEAD_VARS));
    }

    bool Node::has_correctness_auto_storage_assertion() const
    {
        return has_key(_ASSERT_CORRECTNESS_AUTO_STORAGE_VARS);
    }

    bool Node::has_correctness_incoherent_fp_assertion() const
    {
        return has_key(_ASSERT_CORRECTNESS_INCOHERENT_FP_VARS);
    }

    bool Node::has_correctness_incoherent_p_assertion() const
    {
        return has_key(_ASSERT_CORRECTNESS_INCOHERENT_P_VARS);
    }

    bool Node::has_correctness_incoherent_in_assertion() const
    {
        return has_key(_ASSERT_CORRECTNESS_INCOHERENT_IN_VARS);
    }

    bool Node::has_correctness_incoherent_in_pointed_assertion() const
    {
        return has_key(_ASSERT_CORRECTNESS_INCOHERENT_IN_POINTED_VARS);
    }

    bool Node::has_correctness_incoherent_out_assertion() const
    {
        return has_key(_ASSERT_CORRECTNESS_INCOHERENT_OUT_VARS);
    }

    bool Node::has_correctness_incoherent_out_pointed_assertion() const
    {
        return has_key(_ASSERT_CORRECTNESS_INCOHERENT_OUT_POINTED_VARS);
    }

    bool Node::has_correctness_race_assertion() const
    {
        return has_key(_ASSERT_CORRECTNESS_RACE_VARS);
    }

    bool Node::has_correctness_dead_assertion() const
    {
        return has_key(_ASSERT_CORRECTNESS_DEAD_VARS);
    }

    // *********************** Linked-members getters/setters *********************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ********************************* Modifiers ********************************** //

    void Node::erase_entry_edge(Node* source)
    {
        EdgeList::iterator it;
        for (it = _entry_edges.begin(); it != _entry_edges.end(); ++it)
        {
            if ((*it)->get_source() == source)
            {
                _entry_edges.erase(it);
                --it;   // Decrement to allow the correctness of the comparison outside the loop
                break;
            }
        }
        if (it == _entry_edges.end())
        {
            internal_error("Trying to delete an non-existent edge between nodes '%d' and '%d'", source->_id, _id);
        }
    }

    void Node::erase_exit_edge(Node* target)
    {
        EdgeList::iterator it;
        for (it = _exit_edges.begin(); it != _exit_edges.end(); ++it)
        {
            if ((*it)->get_target() == target)
            {
                _exit_edges.erase(it);
                --it;   // Decrement to allow the correctness of the comparison outside the loop
                break;
            }
        }
        if (it == _exit_edges.end())
        {
            internal_error("Trying to delete an non-existent edge between nodes '%d' and '%d'", _id, target->_id);
        }
    }

    // ******************************* END Modifiers ******************************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ******************************** Consultants ********************************* //

    bool Node::node_is_enclosed_by(Node* potential_encloser)
    {
        Node* outer_node = get_outer_node();
        while((outer_node != NULL) && (outer_node != potential_encloser))
            outer_node = outer_node->get_outer_node();
        return (outer_node != NULL);
    }

    bool Node::is_builtin_node()
    {
        return (get_type() == __Builtin);
    }

    bool Node::is_graph_node()
    {
        return (get_type() == __Graph);
    }

    bool Node::is_extended_graph_node()
    {
        return ((get_type() == __Graph) && (get_graph_type() == __ExtensibleGraph));
    }

    bool Node::is_entry_node()
    {
        return (get_type() == __Entry);
    }

    bool Node::is_exit_node()
    {
        return (get_type() == __Exit);
    }

    bool Node::is_function_code_node()
    {
        return is_graph_node() && get_graph_related_ast().is<Nodecl::FunctionCode>();
    }

    bool Node::is_break_node()
    {
        return (get_type() == __Break);
    }

    bool Node::is_conditional_expression()
    {
        return ((get_type() == __Graph) && (get_graph_type() == __CondExpr));
    }
    
    bool Node::is_continue_node()
    {
        return (get_type() == __Continue);
    }
    
    bool Node::is_context_node()
    {
        return ((get_type() == __Graph) && (get_graph_type() == __Context));
    }

    bool Node::is_ifelse_statement()
    {
        return ((get_type() == __Graph) && get_graph_type() == __IfElse);
    }

    bool Node::is_switch_statement()
    {
        return ((get_type() == __Graph) && get_graph_type() == __Switch);
    }

    bool Node::is_switch_case_node()
    {
        return ((get_type() == __Graph) && get_graph_type() == __SwitchCase);
    }
    
    bool Node::is_goto_node()
    {
        return (get_type() == __Goto);
    }

    bool Node::is_split_statement()
    {
        return (is_graph_node() && get_graph_type() == __SplitStmt);
    }

    bool Node::is_return_node()
    {
        return (get_type() == __Return);
    }

    bool Node::is_unclassified_node()
    {
        return (get_type() == __UnclassifiedNode);
    }

    bool Node::is_graph_entry_node(Node* graph)
    {
        return (_id == graph->get_graph_entry_node()->get_id());
    }

    bool Node::is_graph_exit_node(Node* graph)
    {
        return (_id == graph->get_graph_exit_node()->get_id());
    }

    bool Node::is_loop_node()
    {
        return ((get_type() == __Graph)
                 && ((get_graph_type() == __LoopDoWhile)
                        || (get_graph_type() == __LoopFor)
                        || (get_graph_type() == __LoopWhile)));
    }

    bool Node::is_for_loop()
    {
        return ((get_type() == __Graph) && (get_graph_type() == __LoopFor));
    }

    bool Node::is_while_loop()
    {
        return ((get_type() == __Graph) && (get_graph_type() == __LoopWhile));
    }

    bool Node::is_do_loop()
    {
        return ((get_type() == __Graph) && (get_graph_type() == __LoopDoWhile));
    }

    bool Node::is_normal_node()
    {
        return (get_type() == __Normal);
    }

    bool Node::is_labeled_node()
    {
        return (get_type() == __Labeled);
    }

    bool Node::is_function_call_graph_node()
    {
        return (is_graph_node() && (get_graph_type() == __FunctionCallGraph));
    }
    
    bool Node::is_function_call_node()
    {
        return (get_type() == __FunctionCall);
    }
    
    bool Node::is_asm_def_node()
    {
        return (is_graph_node() && (get_graph_type() == __AsmDef));
    }

    bool Node::is_asm_op_node()
    {
        return (get_type() == __AsmOp);
    }

    bool Node::is_omp_node()
    {
        return (is_omp_atomic_node() || is_omp_barrier_node() || is_omp_barrier_graph_node() || is_omp_critical_node()
                || is_omp_flush_node() || is_omp_loop_node() || is_omp_master_node() || is_omp_parallel_node()
                || is_omp_section_node() || is_omp_sections_node() || is_omp_simd_node() || is_omp_single_node()
                || is_omp_task_creation_node() || is_omp_task_node() || is_omp_taskwait_node() || is_omp_taskyield_node()
                || is_omp_async_target_node() || is_omp_sync_target_node() || is_ompss_taskwait_on_node());
    }
    
    bool Node::is_omp_atomic_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpAtomic));
    }

    bool Node::is_omp_barrier_graph_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpBarrierGraph));
    }
    
    bool Node::is_omp_barrier_node()
    {
        return (get_type() == __OmpBarrier);
    }

    bool Node::is_omp_critical_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpCritical));
    }

    bool Node::is_omp_flush_node()
    {
        return (get_type() == __OmpFlush);
    }

    bool Node::is_omp_loop_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpLoop));
    }

    bool Node::is_omp_master_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpMaster));
    }

    bool Node::is_omp_parallel_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpParallel));
    }

    bool Node::is_omp_section_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpSection));
    }

    bool Node::is_omp_sections_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpSections));
    }

    bool Node::is_omp_simd_node()
    {
        GraphType gt = get_graph_type();
        return (is_graph_node() 
                 && ((gt == __OmpSimd) || (gt == __OmpSimdFor) 
                 || (gt == __OmpSimdFunction) || (gt == __OmpSimdParallelFor)));
    }
    
    bool Node::is_omp_simd_function_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpSimdFunction));
    }
    
    bool Node::is_omp_single_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpSingle));
    }

    // Fortran only
    bool Node::is_omp_workshare_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpWorkshare));
    }

    bool Node::is_omp_task_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpTask));
    }

    bool Node::is_omp_task_creation_node()
    {
        return (get_type () == __OmpTaskCreation);
    }

    bool Node::is_omp_taskwait_node()
    {
        return (get_type() == __OmpTaskwait);
    }

    bool Node::is_omp_async_target_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpAsyncTarget));
    }

    bool Node::is_omp_sync_target_node()
    {
        return (is_graph_node() && (get_graph_type() == __OmpSyncTarget));
    }

    bool Node::is_ompss_taskwait_on_node()
    {
        return (get_type() == __OmpWaitonDeps);
    }

    bool Node::is_omp_taskyield_node()
    {
        return (get_type() == __OmpTaskyield);
    }

    bool Node::is_omp_virtual_tasksync()
    {
        return (get_type() == __OmpVirtualTaskSync);
    }

    bool Node::is_vector_node()
    {
        if (is_graph_node())
        {
            GraphType gt = get_graph_type();
            if ((gt == __VectorCondExpr) || (gt == __VectorFunctionCallGraph))
                return true;
        }
        else
        {
            NodeType nt = get_type();
            if ((nt == __VectorFunctionCall) || (nt == __VectorGather) || (nt == __VectorLoad)
                    || (nt == __VectorNormal) || (nt == __VectorReduction) || (nt == __VectorScatter)
                    || (nt == __VectorStore))
            {
                return true;
            }
        }
        return false;
    }

    bool Node::has_child(Node* n)
    {
        for (EdgeList::iterator it = _exit_edges.begin(); it != _exit_edges.end(); ++it)
        {
            if ((*it)->get_target() == n)
                return true;
        }

        return false;
    }

    bool Node::has_parent(Node* n)
    {
        for (EdgeList::iterator it = _entry_edges.begin(); it != _entry_edges.end(); ++it)
        {
            if ((*it)->get_source() == n)
                return true;
        }

        return false;
    }

    Symbol Node::get_function_node_symbol()
    {
        if (!is_function_call_node())
            return Symbol();

        NBase stmt = get_statements()[0];
        Symbol s;
        if (stmt.is<Nodecl::FunctionCall>())
        {
            Nodecl::FunctionCall f = stmt.as<Nodecl::FunctionCall>();
            s = f.get_called().get_symbol();
        }
        else if (stmt.is<Nodecl::VirtualFunctionCall>())
        {
            Nodecl::FunctionCall f = stmt.as<Nodecl::FunctionCall>();
            s = f.get_called().get_symbol();
        }

        return s;
    }

    // ****************************** END Consultants ******************************* //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ******************* Getters for OpenMP/OmpSs clauses info ******************** //

    NodeclSet Node::get_firstprivate_vars()
    {
        NodeclSet firstprivate_vars;
        const TL::Analysis::PCFGPragmaInfo& task_pragma_info = get_pragma_node_info();
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_FIRSTPRIVATE))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OPEN_M_P_FIRSTPRIVATE).as<Nodecl::OpenMP::Private>().get_symbols().shallow_copy().as<Nodecl::List>();
            firstprivate_vars.insert(tmp.begin(), tmp.end());
        }
        return firstprivate_vars;
    }

    NodeclSet Node::get_private_vars()
    {
        NodeclSet private_vars;
        const TL::Analysis::PCFGPragmaInfo& task_pragma_info = get_pragma_node_info();
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_PRIVATE))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OPEN_M_P_PRIVATE).as<Nodecl::OpenMP::Private>().get_symbols().shallow_copy().as<Nodecl::List>();
            private_vars.insert(tmp.begin(), tmp.end());
        }
        return private_vars;
    }

    NodeclSet Node::get_all_private_vars()
    {
        NodeclSet private_vars;
        const TL::Analysis::PCFGPragmaInfo& task_pragma_info = get_pragma_node_info();
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_FIRSTPRIVATE))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OPEN_M_P_FIRSTPRIVATE).as<Nodecl::OpenMP::Firstprivate>().get_symbols().shallow_copy().as<Nodecl::List>();
            private_vars.insert(tmp.begin(), tmp.end());
        }
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_PRIVATE))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OPEN_M_P_PRIVATE).as<Nodecl::OpenMP::Private>().get_symbols().shallow_copy().as<Nodecl::List>();
            private_vars.insert(tmp.begin(), tmp.end());
        }
        return private_vars;
    }

    NodeclSet Node::get_all_shared_accesses()
    {
        // 1.- Collect the shared variables appearing in the data-sharing or dependency clauses
        const TL::Analysis::PCFGPragmaInfo& task_pragma_info = get_pragma_node_info();
        NodeclSet shared_vars;
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_SHARED))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OPEN_M_P_SHARED).as<Nodecl::OpenMP::Shared>().get_symbols().shallow_copy().as<Nodecl::List>();
            shared_vars.insert(tmp.begin(), tmp.end());
        }
        if (task_pragma_info.has_clause(NODECL_OMP_SS_SHARED_AND_ALLOCA))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OMP_SS_SHARED_AND_ALLOCA).as<Nodecl::OmpSs::SharedAndAlloca>().get_exprs().shallow_copy().as<Nodecl::List>();
            shared_vars.insert(tmp.begin(), tmp.end());
        }
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_DEP_IN))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OPEN_M_P_DEP_IN).as<Nodecl::OpenMP::DepIn>().get_exprs().shallow_copy().as<Nodecl::List>();
            shared_vars.insert(tmp.begin(), tmp.end());
        }
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_DEP_OUT))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OPEN_M_P_DEP_OUT).as<Nodecl::OpenMP::DepOut>().get_exprs().shallow_copy().as<Nodecl::List>();
            shared_vars.insert(tmp.begin(), tmp.end());
        }
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_DEP_INOUT))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OPEN_M_P_DEP_INOUT).as<Nodecl::OpenMP::DepInout>().get_exprs().shallow_copy().as<Nodecl::List>();
            shared_vars.insert(tmp.begin(), tmp.end());
        }
        if (task_pragma_info.has_clause(NODECL_OMP_SS_CONCURRENT))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OMP_SS_CONCURRENT).as<Nodecl::OmpSs::Concurrent>().get_exprs().shallow_copy().as<Nodecl::List>();
            shared_vars.insert(tmp.begin(), tmp.end());
        }
        if (task_pragma_info.has_clause(NODECL_OMP_SS_COMMUTATIVE))
        {
            Nodecl::List tmp = task_pragma_info.get_clause(NODECL_OMP_SS_COMMUTATIVE).as<Nodecl::OmpSs::Commutative>().get_exprs().shallow_copy().as<Nodecl::List>();
            shared_vars.insert(tmp.begin(), tmp.end());
        }

        // 2.- Collect those objects which, although are not in the data-sharing or dependency, have dynamic storage duration
        const NodeclSet& ue_vars = get_ue_vars();
        const NodeclSet& killed_vars = get_killed_vars();
        const NodeclSet& undef_vars = get_undefined_behaviour_vars();
        NodeclSet inner_vars(ue_vars.begin(), ue_vars.end());
        inner_vars.insert(killed_vars.begin(), killed_vars.end());
        inner_vars.insert(undef_vars.begin(), undef_vars.end());
        const Nodecl::NodeclBase& ast = get_graph_related_ast();
        ERROR_CONDITION(ast.is_null(),
                        "Cannot retrieve the shared accesses of node %d, which does not relate to an AST",
                        _id);
        Scope task_sc(ast.retrieve_context());
        for (NodeclSet::const_iterator it = inner_vars.begin(); it != inner_vars.end(); ++it)
        {
            // Discard all vars local to the task
            Scope var_sc(Analysis::Utils::get_nodecl_base(*it).get_symbol().get_scope());
            if (var_sc.scope_is_enclosed_by(task_sc))
                continue;

            // If the variables is an array ArraySubscript, check whether it is allocated dynamically
            if (it->no_conv().is<Nodecl::ArraySubscript>())
            {
                const NBase& base = Utils::get_nodecl_base(it->no_conv());
                if (base.get_type().no_ref().is_pointer())
                {   // An array has been dynamically allocated => the object pointed by #base is shared
                    shared_vars.insert(it->shallow_copy());
                }
            }
        }

        return shared_vars;
    }

    // ******************* Getters for OpenMP/OmpSs clauses info ******************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ****************** Getters and setters for PCFG analysis ********************* //
    
    AliveTaskSet& Node::get_live_in_tasks()
    {
        return get_data<AliveTaskSet>(_LIVE_IN_TASKS);
    }
    
    AliveTaskSet& Node::get_live_out_tasks()
    {
        return get_data<AliveTaskSet>(_LIVE_OUT_TASKS);
    }

    // **************** END getters and setters for PCFG analysis ******************* //
    // ****************************************************************************** //
    

    
    // ****************************************************************************** //
    // **************** Getters and setters for constants analysis ****************** //

//     ObjectList<LatticeCellValue> Node::get_lattice_val()
//     {
//         if (_type != GRAPH)
//         {
//             ObjectList<LatticeCellValue> result;
//             if (has_key(_LATTICE_VALS))
//             {
//                 result = get_data<ObjectList<LatticeCellValue> >(_LATTICE_VALS);
//             }
//             return result;
//         }
//         else
//         {
//             internal_error("Requesting Lattice Cell Values list in a GRAPH. Simple node expected.",
//                             get_type_as_string().c_str(), _id);
//         }
//     }
//
//     void Node::set_lattice_val(LatticeCellValue lcv)
//     {
//         if (_type != GRAPH)
//         {
//             ObjectList<LatticeCellValue> result;
//             if (has_key(_LATTICE_VALS))
//             {
//                 result = get_data<ObjectList<LatticeCellValue> >(_LATTICE_VALS);
//             }
//
//             result.append(lcv);
//             set_data(_LATTICE_VALS, result);
//         }
//         else
//         {
//             internal_error("Requesting Lattice Cell Values list in a GRAPH. Simple node expected.",
//                             get_type_as_string().c_str(), _id);
//         }
//     }

    // ************** END getters and setters for constants analysis **************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ****************************** Private methods ******************************* //

    template <typename T>
    T Node::get_vars(PCFGAttribute attr)
    {
        if (has_key(attr))
            return get_data<T>(attr);

        return T();
    }

    template <typename T>
    void Node::add_var_to_container(const NBase& var, PCFGAttribute attr)
    {
        T& c = get_data<T>(attr);
        if (Utils::nodecl_set_contains_enclosing_nodecl(var, c).is_null())
        {
            const Nodecl::List& subparts = Utils::nodecl_set_contains_enclosed_nodecl(var, c);
            if (!subparts.is_null())
            {
                for (Nodecl::List::const_iterator it = subparts.begin(); it != subparts.end(); ++it)
                    c.erase(*it);
            }

            c.insert(var);
        }
    }

    template <typename T>
    void Node::add_vars_to_container(const T& vars, PCFGAttribute attr)
    {
        if (vars.empty())
        {   // ensure that, in case no attribute called #attr was attached to the node, now it will be attached
            /*T& c = */get_data<T>(attr);
        }
        else
        {
            for (typename T::const_iterator it = vars.begin(); it != vars.end(); ++it)
                add_var_to_container<T>(*it, attr);
        }
    }

    void Node::add_var_to_list(const NBase& var, PCFGAttribute attr)
    {
        Nodecl::List& list = get_data<Nodecl::List>(attr);
        for (Nodecl::List::iterator it = list.begin(); it != list.end(); ++it)
        {
            if (Nodecl::Utils::structurally_equal_nodecls(var, *it))
                return;
        }

        list.append(var);
    }

    void Node::add_vars_to_list(const Nodecl::List& vars, PCFGAttribute attr)
    {
        if (vars.empty())
        {   // ensure that, in case no attribute called #attr was attached to the node, now it will be attached
            /*Nodecl::List& list =*/ get_data<Nodecl::List>(attr);
        }
        else
        {
            for (Nodecl::List::const_iterator it = vars.begin(); it != vars.end(); ++it)
                add_var_to_list(*it, attr);
        }
    }

    void Node::remove_var_from_set(const NBase& var, PCFGAttribute attr)
    {
        get_data<NodeclSet>(attr);
    }

    // **************************** END private methods ***************************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // *************** Getters and setters for use-definition analysis ************** //

    // **** Upwards exposed *** //
    NodeclSet& Node::get_ue_vars()
    {
        return get_data<NodeclSet>(_UPPER_EXPOSED);
    }

    void Node::add_ue_var(const NBase& new_ue_var)
    {
        add_var_to_container<NodeclSet>(new_ue_var, _UPPER_EXPOSED);
    }

    void Node::add_ue_var(const NodeclSet& new_ue_vars)
    {
        add_vars_to_container<NodeclSet>(new_ue_vars, _UPPER_EXPOSED);
    }

    void Node::set_ue_var(const NodeclSet& new_ue_vars)
    {
        set_data(_UPPER_EXPOSED, new_ue_vars);
    }
    
    void Node::remove_ue_var(const NBase& old_ue_var)
    {
        remove_var_from_set(old_ue_var, _UPPER_EXPOSED);
    }

    NodeclSet& Node::get_private_ue_vars()
    {
        return get_data<NodeclSet>(_PRIVATE_UPPER_EXPOSED);
    }

    void Node::add_private_ue_var(const NodeclSet& new_private_ue_vars)
    {
        add_vars_to_container<NodeclSet>(new_private_ue_vars, _PRIVATE_UPPER_EXPOSED);
    }

    void Node::set_private_ue_var(const NodeclSet& new_private_ue_vars)
    {
        set_data(_PRIVATE_UPPER_EXPOSED, new_private_ue_vars);
    }

    // **** Killed *** //
    NodeclSet& Node::get_killed_vars()
    {
        return get_data<NodeclSet>(_KILLED);
    }

    void Node::add_killed_var(const NBase& new_killed_var)
    {
        add_var_to_container<NodeclSet>(new_killed_var, _KILLED);
    }

    void Node::add_killed_var(const NodeclSet& new_killed_vars)
    {
        add_vars_to_container<NodeclSet>(new_killed_vars, _KILLED);
    }

    void Node::set_killed_var(const NodeclSet& new_killed_vars)
    {
        set_data(_KILLED, new_killed_vars);
    }
    
    void Node::remove_killed_var(const NBase& old_killed_var)
    {
        remove_var_from_set(old_killed_var, _KILLED);
    }

    NodeclSet& Node::get_private_killed_vars()
    {
        return get_data<NodeclSet>(_PRIVATE_KILLED);
    }
    
    void Node::add_private_killed_var(const NodeclSet& new_private_killed_vars)
    {
        add_vars_to_container<NodeclSet>(new_private_killed_vars, _PRIVATE_KILLED);
    }
        
    void Node::set_private_killed_var(const NodeclSet& new_private_killed_vars)
    {
        set_data(_PRIVATE_KILLED, new_private_killed_vars);
    }

    // **** Undefined behavior *** //
    NodeclSet& Node::get_undefined_behaviour_vars()
    {
        return get_data<NodeclSet>(_UNDEF);
    }

    void Node::add_undefined_behaviour_var(const NBase& new_undef_var)
    {
        add_var_to_container<NodeclSet>(new_undef_var, _UNDEF);
    }

    void Node::add_undefined_behaviour_var_and_recompute_use_and_killed_sets(
        const NBase& new_undef_var)
    {
        // Conservatively, delete the reference argument of UE and KILL sets
        if (has_key(_UPPER_EXPOSED))
            remove_ue_var(new_undef_var);
        if (has_key(_KILLED))
            remove_killed_var(new_undef_var);

        // Add the global variable to the UNDEF list
        add_undefined_behaviour_var(new_undef_var);
    }

    void Node::set_undefined_behaviour_var(const NodeclSet& new_undef_vars)
    {
        set_data(_UNDEF, new_undef_vars);
    }

    void Node::remove_undefined_behaviour_var(const NBase& old_undef_var)
    {
        remove_var_from_set(old_undef_var, _UNDEF);
    }

    NodeclSet& Node::get_private_undefined_behaviour_vars()
    {
        return get_data<NodeclSet>(_PRIVATE_UNDEF);
    }

    void Node::add_private_undefined_behaviour_var(const NodeclSet& new_private_undef_vars)
    {
        add_vars_to_container<NodeclSet>(new_private_undef_vars, _PRIVATE_UNDEF);
    }

    void Node::set_private_undefined_behaviour_var(const NodeclSet& new_private_undef_vars)
    {
        set_data(_PRIVATE_UNDEF, new_private_undef_vars);
    }

    // *** Used addresses *** //
    NodeclSet& Node::get_used_addresses()
    {
        return get_data<NodeclSet>(_USED_ADDRESSES);
    }

    void Node::add_used_address(const NBase& es)
    {
        get_data<NodeclSet>(_USED_ADDRESSES).insert(es);
    }

    void Node::set_used_addresses(const NodeclSet& used_addresses)
    {
        set_data(_USED_ADDRESSES, used_addresses);
    }

    // ************* END getters and setters for use-definition analysis ************ //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ****************** Getters and setters for liveness analysis ***************** //

    NodeclSet& Node::get_live_in_vars()
    {
        return get_data<NodeclSet>(_LIVE_IN);
    }

    void Node::set_live_in(const NBase& new_live_in_var)
    {
        get_data<NodeclSet>(_LIVE_IN).insert(new_live_in_var);
    }

    void Node::set_live_in(const NodeclSet& new_live_in_set)
    {
        set_data(_LIVE_IN, new_live_in_set);
    }

    NodeclSet& Node::get_live_out_vars()
    {
        return get_data<NodeclSet>(_LIVE_OUT);
    }

    void Node::add_live_out(const NodeclSet& new_live_out_set)
    {
        add_vars_to_container<NodeclSet>(new_live_out_set, _LIVE_OUT);
    }
    
    void Node::set_live_out(const NBase& new_live_out_var)
    {
        get_data<NodeclSet>(_LIVE_OUT).insert(new_live_out_var);
    }

    void Node::set_live_out(const NodeclSet& new_live_out_set)
    {
        set_data(_LIVE_OUT, new_live_out_set);
    }

    // **************** END getters and setters for liveness analysis *************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ************ Getters and setters for reaching definitions analysis *********** //

    NodeclMap& Node::get_generated_stmts()
    {
        return get_data<NodeclMap>(_GEN);
    }

    void Node::set_generated_stmts(const NodeclMap& gen)
    {
        NodeclMap& gen_stmts = get_data<NodeclMap>(_GEN);
        for (NodeclMap::const_iterator it = gen.begin(); it != gen.end(); ++it)
        {
            if (gen_stmts.find(it->first) != gen_stmts.end())
                gen_stmts.erase(it->first);
        }
        gen_stmts.insert(gen.begin(), gen.end());
    }

    NodeclMap& Node::get_reaching_definitions_in()
    {
        return get_data<NodeclMap>(_REACH_DEFS_IN);
    }

    NodeclMap& Node::get_reaching_definitions_out()
    {
        return get_data<NodeclMap>(_REACH_DEFS_OUT);
    }

    void Node::set_reaching_definition_in(const NBase& var, const NBase& init, const NBase& stmt)
    {
        get_data<NodeclMap>(_REACH_DEFS_IN).insert(std::pair<NBase, NodeclPair>(var, NodeclPair(init, stmt)));
    }

    void Node::set_reaching_definitions_in(const NodeclMap& reach_defs_in)
    {
        set_data(_REACH_DEFS_IN, reach_defs_in);
    }

    void Node::set_reaching_definition_out(const NBase& var, const NBase& init, const NBase& stmt)
    {
        get_data<NodeclMap>(_REACH_DEFS_OUT).insert(std::pair<NBase, NodeclPair>(var, NodeclPair(init, stmt)));
    }

    void Node::set_reaching_definitions_out(const NodeclMap& reach_defs_out)
    {
        set_data(_REACH_DEFS_OUT, reach_defs_out);
    }

    // ********** END getters and setters for reaching definitions analysis ********* //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ******************* Getters and setters for loops analysis ******************* //

    Utils::InductionVarList& Node::get_induction_variables()
    {
        ERROR_CONDITION(!is_loop_node() && !is_omp_loop_node()
                            && (!is_graph_node() || !get_graph_related_ast().is<Nodecl::FunctionCode>()),
                        "Asking for induction_variables in a node '%d' of type '%s'. Loop expected",
                        _id, get_type_as_string().c_str());

        return get_data<Utils::InductionVarList>(_INDUCTION_VARS);
    }

    void Node::set_induction_variable(Utils::InductionVar* iv)
    {
        ERROR_CONDITION(!is_loop_node() && !is_omp_loop_node(),
                        "Unexpected node type '%s' while adding induction variable to node '%d'. LOOP expected.",
                        get_type_as_string().c_str(), _id);

        get_data<Utils::InductionVarList>(_INDUCTION_VARS).insert(iv);
    }

    bool Node::is_loop_induction_variable(const NBase& iv)
    {
        ERROR_CONDITION(!is_loop_node() && !is_omp_loop_node(),
                        "Unexpected node type '%s' while reporting an induction variable. LOOP expected.",
                        get_type_as_string().c_str(), _id);

        const Utils::InductionVarList& ivs = get_induction_variables();
        for (Utils::InductionVarList::const_iterator it = ivs.begin(); it != ivs.end(); ++it)
            if (Nodecl::Utils::structurally_equal_nodecls((*it)->get_variable(), iv, /*skip_conversions*/true))
                return true;

        return false;
    }

    Node* Node::get_condition_node()
    {
        if (is_graph_node())
        {
            if (is_loop_node() || is_switch_statement() || is_ifelse_statement())
                return get_data<Node*>(_CONDITION_NODE);
            
            internal_error("Unexpected graph type '%s' while getting the condition node of loop node '%d'. LOOP|SWITCH|IFELSE expected",
                            get_graph_type_as_string().c_str(), _id);
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting condition node of loop graph node '%d'. GRAPH NODE expected.",
                            get_type_as_string().c_str(), _id);
        }
    }
    
    void Node::set_condition_node(Node* cond)
    {
        if (is_graph_node())
        {
            if (is_loop_node() || is_switch_statement() || is_ifelse_statement())
            {
                set_data(_CONDITION_NODE, cond);
            }
            else
            {
                internal_error("Unexpected graph type '%s' while setting the condition node to loop node '%d'. LOOP expected",
                                get_graph_type_as_string().c_str(), _id);
            }
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting condition node to loop graph node '%d'. GRAPH NODE expected.",
                            get_type_as_string().c_str(), _id);
        }
    }

    // ***************** END getters and setters for loops analysis ***************** //
    // ****************************************************************************** //
    
    
    
    // ****************************************************************************** //
    // ******************* Getters and setters for range analysis ******************* //
    
    RangeValuesMap Node::get_ranges()
    {
        RangeValuesMap ranges;
        if (has_key(_RANGES))
            ranges = get_data<RangeValuesMap>(_RANGES);
        return ranges;
    }
    
    NBase Node::get_range(const NBase& var)
    {
        NBase res;
        RangeValuesMap ranges = get_ranges();
        RangeValuesMap::iterator it = ranges.find(var);
        if (it != ranges.end())
            res = it->second;
        return res;
    }
    
    void Node::set_range(const NBase& var, const NBase& value)
    {
        RangeValuesMap ranges = get_ranges();
        RangeValuesMap::iterator it = ranges.find(var);
        if (it != ranges.end())
        {   // Check whether the value already in the set is the same we are trying to insert here
            // - If it is different, something wrong happened. In this case, abort here
            // - If it is the same, one may com from the list of constraints belonging to the node
            //   and the other propagated from previous nodes. In this case we want the value only once
            if (Nodecl::Utils::structurally_equal_nodecls(it->second, value, /*skip_conversion_nodes*/true))
                return;
            WARNING_MESSAGE("Two different ranges (%s and %s) for the same variable '%s' in the same node %d.\n", 
                            it->second.prettyprint().c_str(), value.prettyprint().c_str(), 
                            var.prettyprint().c_str(), _id);
        }
        ranges.insert(std::pair<NBase, NBase>(var, value));
        set_data(_RANGES, ranges);
    }
    
    // ***************** END getters and setters for range analysis ***************** //
    // ****************************************************************************** //
    
    
    
    // ****************************************************************************** //
    // ******************* Getters and setters for OmpSs analysis ******************* //

    const NBase& Node::get_task_context()
    {
        if (_type == __Graph)
        {
            GraphType graph_type = get_data<GraphType>(_GRAPH_TYPE);
            if (graph_type == __OmpTask)
                return get_data<Nodecl::Context>(_TASK_CONTEXT);
            else
                internal_error("Unexpected graph type '%s' while getting the context of the task node '%d'. " \
                                "\"task\" type expected", get_graph_type_as_string().c_str(), _id);
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the context of the task node '%d'. \"task\" type expected.",
                            get_type_as_string().c_str(), _id);
        }
    }

    void Node::set_task_context(NBase c)
    {
        if (_type == __Graph)
        {
            GraphType graph_type = get_data<GraphType>(_GRAPH_TYPE);
            if (graph_type == __OmpTask)
                return set_data(_TASK_CONTEXT, c);
            else
                internal_error("Unexpected graph type '%s' while setting the context of the task node '%d'. " \
                                "\"task\" type expected", get_graph_type_as_string().c_str(), _id);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the label to node '%d'. GRAPH NODE expected.",
                            get_type_as_string().c_str(), _id);
        }
    }

    Symbol Node::get_task_function()
    {
        if (_type == __Graph)
        {
            GraphType graph_type = get_data<GraphType>(_GRAPH_TYPE);
            if (graph_type == __OmpTask)
                return get_data<Symbol>(_TASK_FUNCTION);
            else
                internal_error("Unexpected graph type '%s' while getting the symbol of the function embedded in the task '%s'. " \
                                "\"task\" type expected", get_graph_type_as_string().c_str(),
                               get_data<NBase>(_NODE_LABEL).prettyprint().c_str());
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the symbol of the function embedded in a task'. GRAPH NODE expected.",
                        get_type_as_string().c_str());
        }
    }

    void Node::set_task_function(Symbol func_sym)
    {
        if (_type == __Graph)
        {
            GraphType graph_type = get_data<GraphType>(_GRAPH_TYPE);
            if (graph_type == __OmpTask)
                return set_data(_TASK_FUNCTION, func_sym);
            else
                internal_error("Unexpected graph type '%s' while setting the symbol of the function embedded in the task '%s'. " \
                                "\"task\" type expected", get_graph_type_as_string().c_str(),
                                get_data<NBase>(_NODE_LABEL).prettyprint().c_str());
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the symbol of the function embedded in a task. GRAPH NODE expected.",
                            get_type_as_string().c_str());
        }
    }

    // ***************** END Getters and setters for OmpSs analysis ***************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // *************** Getters and setters for auto-scoping analysis **************** //

    bool Node::is_auto_scoping_enabled()
    {
        return has_key(_SC_AUTO);
    }

    void Node::set_auto_scoping_enabled()
    {
        set_data(_SC_AUTO, true);
    }

    NodeclSet Node::get_sc_shared_vars()
    {
        NodeclSet sc_shared_vars;
        if (has_key(_SC_SHARED))
            sc_shared_vars = get_data<NodeclSet>(_SC_SHARED);
        return sc_shared_vars;
    }

    void Node::set_sc_shared_var(NBase es)
    {
        NodeclSet sc_shared_vars = get_sc_shared_vars();
        sc_shared_vars.insert(es);
        set_data(_SC_SHARED, sc_shared_vars);
    }
    void Node::set_sc_shared_var(NodeclSet es_list)
    {
        NodeclSet sc_shared_vars = get_sc_shared_vars();
        sc_shared_vars.insert(es_list.begin(), es_list.end());
        set_data(_SC_SHARED, sc_shared_vars);
    }

    NodeclSet Node::get_sc_private_vars()
    {
        NodeclSet sc_private_vars;
        if (has_key(_SC_PRIVATE))
            sc_private_vars = get_data<NodeclSet>(_SC_PRIVATE);
        return sc_private_vars;
    }

    void Node::set_sc_private_var(NBase es)
    {
        NodeclSet sc_private_vars = get_sc_private_vars();
        sc_private_vars.insert(es);
        set_data(_SC_PRIVATE, sc_private_vars);
    }
    void Node::set_sc_private_var(NodeclSet es_list)
    {
        NodeclSet sc_private_vars = get_sc_private_vars();
        sc_private_vars.insert(es_list.begin(), es_list.end());
        set_data(_SC_PRIVATE, sc_private_vars);
    }

    NodeclSet Node::get_sc_firstprivate_vars()
    {
        NodeclSet sc_firstprivate_vars;
        if (has_key(_SC_FIRSTPRIVATE))
            sc_firstprivate_vars = get_data<NodeclSet>(_SC_FIRSTPRIVATE);
        return sc_firstprivate_vars;
    }

    void Node::set_sc_firstprivate_var(NBase es)
    {
        NodeclSet sc_firstprivate_vars = get_sc_firstprivate_vars();
        sc_firstprivate_vars.insert(es);
        set_data(_SC_FIRSTPRIVATE, sc_firstprivate_vars);
    }
    void Node::set_sc_firstprivate_var(NodeclSet es_list)
    {
        NodeclSet sc_firstprivate_vars = get_sc_firstprivate_vars();
        sc_firstprivate_vars.insert(es_list.begin(), es_list.end());
        set_data(_SC_FIRSTPRIVATE, sc_firstprivate_vars);
    }

    NodeclSet Node::get_sc_undef_vars()
    {
        NodeclSet undef_sc_vars;
        if (has_key(_SC_UNDEF))
            undef_sc_vars = get_data<NodeclSet>(_SC_UNDEF);
        return undef_sc_vars;
    }

    void Node::set_sc_undef_var(NBase es)
    {
        NodeclSet sc_undef_vars = get_sc_undef_vars();
        sc_undef_vars.insert(es);
        set_data(_SC_UNDEF, sc_undef_vars);
    }
    void Node::set_sc_undef_var(NodeclSet es_list)
    {
        NodeclSet sc_undef_vars = get_sc_undef_vars();
        sc_undef_vars.insert(es_list.begin(), es_list.end());
        set_data(_SC_UNDEF, sc_undef_vars);
    }

    NodeclSet Node::get_sc_race_vars()
    {
        NodeclSet race_vars;
        if (has_key(_SC_RACE))
            race_vars = get_data<NodeclSet>(_SC_RACE);
        return race_vars;
    }

    void Node::set_sc_race_var(NBase es)
    {
        NodeclSet sc_race_vars = get_sc_race_vars();
        sc_race_vars.insert(es);
        set_data(_SC_RACE, sc_race_vars);
    }

    Utils::AutoScopedVariables Node::get_auto_scoped_variables()
    {
        NodeclSet private_vars = get_sc_private_vars();
        NodeclSet firstprivate_vars = get_sc_firstprivate_vars();
        NodeclSet race_vars = get_sc_race_vars();
        NodeclSet shared_vars = get_sc_shared_vars();
        NodeclSet undef_vars = get_sc_undef_vars();

        Utils::AutoScopedVariables res(private_vars, firstprivate_vars, race_vars,
                                        shared_vars, undef_vars);
        return res;
    }

    // ************* END getters and setters for auto-scoping analysis ************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ************** Getters and setters for task dependence analysis ************** //

    NodeclSet Node::get_deps_private_vars()
    {
        NodeclSet deps_private_vars;
        if (has_key(_DEPS_PRIVATE))
            deps_private_vars = get_data<NodeclSet>(_DEPS_PRIVATE);
        return deps_private_vars;
    }

    void Node::set_deps_private_vars(NodeclSet new_deps_private_var)
    {
        NodeclSet deps_private_vars = get_deps_private_vars();
        deps_private_vars.insert(new_deps_private_var.begin(), new_deps_private_var.end());
        set_data(_DEPS_PRIVATE, deps_private_vars);
    }


    NodeclSet Node::get_deps_firstprivate_vars()
    {
        NodeclSet deps_firstprivate_vars;
        if (has_key(_DEPS_FIRSTPRIVATE))
            deps_firstprivate_vars = get_data<NodeclSet>(_DEPS_FIRSTPRIVATE);
        return deps_firstprivate_vars;
    }

    void Node::set_deps_firstprivate_vars(NodeclSet new_deps_firstprivate_var)
    {
        NodeclSet deps_firstprivate_vars = get_deps_firstprivate_vars();
        deps_firstprivate_vars.insert(new_deps_firstprivate_var.begin(), new_deps_firstprivate_var.end());
        set_data(_DEPS_FIRSTPRIVATE, deps_firstprivate_vars);
    }

    NodeclSet Node::get_deps_shared_vars()
    {
        NodeclSet deps_shared_vars;
        if (has_key(_DEPS_SHARED))
            deps_shared_vars = get_data<NodeclSet>(_DEPS_SHARED);
        return deps_shared_vars;
    }

    void Node::set_deps_shared_vars(NodeclSet new_deps_shared_var)
    {
        NodeclSet deps_shared_vars = get_deps_shared_vars();
        deps_shared_vars.insert(new_deps_shared_var.begin(), new_deps_shared_var.end());
        set_data(_DEPS_SHARED, deps_shared_vars);
    }

    NodeclSet Node::get_deps_in_exprs()
    {
        NodeclSet in_deps;
        if (has_key(_DEPS_IN))
            in_deps = get_data<NodeclSet>(_DEPS_IN);
        return in_deps;
    }

    void Node::set_deps_in_exprs(NodeclSet new_in_deps)
    {
        NodeclSet in_deps = get_deps_in_exprs();
        in_deps.insert(new_in_deps.begin(), new_in_deps.end());
        set_data(_DEPS_IN, in_deps);
    }

    NodeclSet Node::get_deps_out_exprs()
    {
        NodeclSet out_deps;
        if (has_key(_DEPS_OUT))
            out_deps = get_data<NodeclSet>(_DEPS_OUT);
        return out_deps;
    }

    void Node::set_deps_out_exprs(NodeclSet new_out_deps)
    {
        NodeclSet out_deps = get_deps_out_exprs();
        out_deps.insert(new_out_deps.begin(), new_out_deps.end());
        set_data(_DEPS_OUT, out_deps);
    }

    NodeclSet Node::get_deps_inout_exprs()
    {
        NodeclSet inout_deps;
        if (has_key(_DEPS_INOUT))
            inout_deps = get_data<NodeclSet>(_DEPS_INOUT);
        return inout_deps;
    }

    void Node::set_deps_inout_exprs(NodeclSet new_inout_deps)
    {
        NodeclSet inout_deps = get_deps_inout_exprs();
        inout_deps.insert(new_inout_deps.begin(), new_inout_deps.end());
        set_data(_DEPS_INOUT, inout_deps);
    }

    NodeclSet Node::get_deps_undef_vars()
    {
        NodeclSet undef_deps;
        if (has_key(_DEPS_UNDEF))
            undef_deps = get_data<NodeclSet>(_DEPS_UNDEF);
        return undef_deps;
    }

    void Node::set_deps_undef_vars(NodeclSet new_undef_deps)
    {
        NodeclSet undef_deps = get_deps_undef_vars();
        undef_deps.insert(new_undef_deps.begin(), new_undef_deps.end());
        set_data(_DEPS_UNDEF, undef_deps);
    }

    // ************ END getters and setters for task dependence analysis ************ //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // **************** Getters and setters for correctness analysis **************** //

    Nodecl::List Node::get_correctness_auto_storage_vars()
    {
        return get_vars<Nodecl::List>(_CORRECTNESS_AUTO_STORAGE_VARS);
    }
    
    void Node::add_correctness_auto_storage_var(const Nodecl::NodeclBase& n)
    {
        add_var_to_list(n, _CORRECTNESS_AUTO_STORAGE_VARS);
    }
    
    Nodecl::List Node::get_correctness_incoherent_fp_vars()
    {
        return get_vars<Nodecl::List>(_CORRECTNESS_INCOHERENT_FP_VARS);
    }
    
    void Node::add_correctness_incoherent_fp_var(const Nodecl::NodeclBase& n)
    {
        add_var_to_list(n, _CORRECTNESS_INCOHERENT_FP_VARS);
    }
    
    Nodecl::List Node::get_correctness_incoherent_in_vars()
    {
        return get_vars<Nodecl::List>(_CORRECTNESS_INCOHERENT_IN_VARS);
    }
    
    void Node::add_correctness_incoherent_in_var(const Nodecl::NodeclBase& n)
    {
        add_var_to_list(n, _CORRECTNESS_INCOHERENT_IN_VARS);
    }
    
    Nodecl::List Node::get_correctness_incoherent_in_pointed_vars()
    {
        return get_vars<Nodecl::List>(_CORRECTNESS_INCOHERENT_IN_POINTED_VARS);
    }

    void Node::add_correctness_incoherent_in_pointed_var(const Nodecl::NodeclBase& n)
    {
        add_var_to_list(n, _CORRECTNESS_INCOHERENT_IN_POINTED_VARS);
    }

    Nodecl::List Node::get_correctness_incoherent_out_vars()
    {
        return get_vars<Nodecl::List>(_CORRECTNESS_INCOHERENT_OUT_VARS);
    }
    
    void Node::add_correctness_incoherent_out_var(const Nodecl::NodeclBase& n)
    {
        add_var_to_list(n, _CORRECTNESS_INCOHERENT_OUT_VARS);
    }
    
    Nodecl::List Node::get_correctness_incoherent_out_pointed_vars()
    {
        return get_vars<Nodecl::List>(_CORRECTNESS_INCOHERENT_OUT_POINTED_VARS);
    }
    
    void Node::add_correctness_incoherent_out_pointed_var(const Nodecl::NodeclBase& n)
    {
        add_var_to_list(n, _CORRECTNESS_INCOHERENT_OUT_POINTED_VARS);
    }
    
    Nodecl::List Node::get_correctness_incoherent_p_vars()
    {
        return get_vars<Nodecl::List>(_CORRECTNESS_INCOHERENT_P_VARS);
    }
    
    void Node::add_correctness_incoherent_p_var(const Nodecl::NodeclBase& n)
    {
        add_var_to_list(n, _CORRECTNESS_INCOHERENT_P_VARS);
    }
    
    NodeclTriboolMap Node::get_correctness_race_vars()
    {
        return get_vars<NodeclTriboolMap>(_CORRECTNESS_RACE_VARS);
    }
    
    Nodecl::List Node::get_true_correctness_race_vars()
    {
        const NodeclTriboolMap& all_race_vars = get_vars<NodeclTriboolMap>(_CORRECTNESS_RACE_VARS);
        Nodecl::List true_race_vars;
        for (NodeclTriboolMap::const_iterator it = all_race_vars.begin();
             it != all_race_vars.end(); ++it)
        {
            if (it->second.is_true())
                true_race_vars.append(it->first.shallow_copy());
        }
        return true_race_vars;
    }

    void Node::add_correctness_race_var(const Nodecl::NodeclBase& n, tribool certainty)
    {
        NodeclTriboolMap& race_vars = get_data<NodeclTriboolMap>(_CORRECTNESS_RACE_VARS);
        race_vars[n] = certainty;
    }
    
    Nodecl::List Node::get_correctness_dead_vars()
    {
        return get_vars<Nodecl::List>(_CORRECTNESS_DEAD_VARS);
    }
    
    void Node::add_correctness_dead_var(const Nodecl::NodeclBase& n)
    {
        add_var_to_list(n, _CORRECTNESS_DEAD_VARS);
    }
    
    Nodecl::List Node::get_correctness_unnecessarily_scoped_vars()
    {
        return get_vars<Nodecl::List>(_CORRECTNESS_UNNECESSARILY_SCOPED_VARS);
    }
    
    void Node::add_correctness_unnecessarily_scoped_var(const Nodecl::NodeclBase& n)
    {
        add_var_to_list(n, _CORRECTNESS_UNNECESSARILY_SCOPED_VARS);
    }
    
    // **************** Getters and setters for correctness analysis **************** //
    // ****************************************************************************** //


    
    // ****************************************************************************** //
    // **************** Getters and setters for vectorization analysis ************** //
    
    ObjectList<Symbol> Node::get_reductions()
    {
        ObjectList<Symbol> result;
        const ObjectList<NBase> clauses = this->get_pragma_node_info().get_clauses();
        for (ObjectList<NBase>::const_iterator it = clauses.begin(); it != clauses.end(); ++it)
        {
            if (it->get_kind() == NODECL_OPEN_M_P_REDUCTION_ITEM)
            {
                Symbol reduced_sym(it->as<Nodecl::OpenMP::ReductionItem>().get_reduced_symbol().get_symbol());
                result.insert(reduced_sym);
            }
        }
        return result;
    }
    
    static void check_for_simd_node(Node*& n)
    {
        if (n->is_loop_node())
        {
            n = n->get_outer_node();
            if (!n->is_omp_simd_node())
            {
                if (VERBOSE)
                {
                    WARNING_MESSAGE("Asking for linear symbols in loop node %d, "\
                                    "which is not contained in an OpenMP loop. Returning empty list.\n", n->get_id());
                }
                n = NULL;
            }
        }
        else if (n->is_function_code_node())
        {
            n = n->get_outer_node();
            if (n != NULL && !n->is_omp_simd_function_node())
            {
                if (VERBOSE)
                {
                    WARNING_MESSAGE("Asking for linear symbols in function code node %d, "\
                                    "which is not contained in an simd function code. Returning empty list.\n", n->get_id());
                }
                n = NULL;
            }
        }
        else if (!n->is_omp_simd_node())
        {
            if (VERBOSE)
            {
                WARNING_MESSAGE("Asking for linear symbols in node %d, which is not an OpenMP loop, "\
                                "neither a simd function code. Returning empty list.\n", n->get_id());
            }
            n = NULL;
        }
    }
    
    ObjectList<Utils::LinearVars> Node::get_linear_symbols()
    {
        ObjectList<Utils::LinearVars> result;
        Node* n = this;
        check_for_simd_node(n);
        if (n != NULL)
        {
            const ObjectList<NBase>& clauses = n->get_pragma_node_info().get_clauses();
            for (ObjectList<NBase>::const_iterator it = clauses.begin(); it != clauses.end(); ++it)
            {
                if (it->get_kind() == NODECL_OPEN_M_P_LINEAR)
                {
                    Nodecl::OpenMP::Linear omp_linear = it->as<Nodecl::OpenMP::Linear>();
                    const Nodecl::List& linear_exprs = 
                        omp_linear.get_linear_expressions().as<Nodecl::List>();
                    const NBase step = omp_linear.get_step();
                    ObjectList<Symbol> syms;
                    for (Nodecl::List::const_iterator itl = linear_exprs.begin(); itl != linear_exprs.end(); ++itl)
                    {
                        if (itl->is<Nodecl::Symbol>())
                        {
                            syms.insert(itl->get_symbol());
                        }
                        else
                        {
                            internal_error("Linear expression is not a symbol", 0);
                        }
                    }
                   
                    result.append(Utils::LinearVars(syms, step));
                }
            }
        }
        return result;
    }
    
    ObjectList<Symbol> Node::get_uniform_symbols()
    {
        ObjectList<Symbol> result;
        Node* n = this;
        check_for_simd_node(n);
        if (n != NULL)
        {
            const ObjectList<NBase> clauses = n->get_pragma_node_info().get_clauses();
            for (ObjectList<NBase>::const_iterator it = clauses.begin(); it != clauses.end(); ++it)
            {
                if (it->get_kind() == NODECL_OPEN_M_P_UNIFORM)
                {
                    const Nodecl::List& uniform_exprs = it->as<Nodecl::OpenMP::Uniform>().get_uniform_expressions().as<Nodecl::List>();
                    for (Nodecl::List::const_iterator itl = uniform_exprs.begin(); itl != uniform_exprs.end(); ++itl)
                    {
                        Symbol lin(itl->get_symbol());
                        ERROR_CONDITION(!lin.is_valid(), "Invalid symbol stored for Uniform argument '%s'", 
                                        itl->prettyprint().c_str());
                        result.insert(lin);
                    }
                    break;
                }
            }
        }
        return result;
    }
    
    // ************** END getters and setters for vectorization analysis ************ //
    // ****************************************************************************** //
    
    
    
    // ****************************************************************************** //
    // ****************** Getters and setters for analysis checking ***************** //
    
    NodeclSet Node::get_assert_ue_vars()
    {
        return get_vars<NodeclSet>(_ASSERT_UPPER_EXPOSED);
    }
    
    void Node::add_assert_ue_var(const Nodecl::List& new_assert_ue_vars)
    {
        add_vars_to_container<NodeclSet>(NodeclSet(new_assert_ue_vars.begin(), new_assert_ue_vars.end()), 
                                         _ASSERT_UPPER_EXPOSED);
    }

    NodeclSet Node::get_assert_killed_vars()
    {
        return get_vars<NodeclSet>(_ASSERT_KILLED);
    }
    
    void Node::add_assert_killed_var(const Nodecl::List& new_assert_killed_vars)
            {
        add_vars_to_container<NodeclSet>(NodeclSet(new_assert_killed_vars.begin(), new_assert_killed_vars.end()), 
                                         _ASSERT_KILLED);
    }
    
    NodeclSet Node::get_assert_undefined_behaviour_vars()
    {
        return get_vars<NodeclSet>(_ASSERT_UNDEFINED);
    }
    
    void Node::add_assert_undefined_behaviour_var(const Nodecl::List& new_assert_undefined_vars)
    {
        add_vars_to_container<NodeclSet>(NodeclSet(new_assert_undefined_vars.begin(), new_assert_undefined_vars.end()), 
                                         _ASSERT_UNDEFINED);
    }
    
    NodeclSet Node::get_assert_live_in_vars()
    {
        return get_vars<NodeclSet>(_ASSERT_LIVE_IN);
    }
    
    void Node::add_assert_live_in_var(const Nodecl::List& new_assert_live_in_vars)
    {
        add_vars_to_container<NodeclSet>(NodeclSet(new_assert_live_in_vars.begin(), new_assert_live_in_vars.end()), 
                                         _ASSERT_LIVE_IN);
    }
    
    NodeclSet Node::get_assert_live_out_vars()
    {
        return get_vars<NodeclSet>(_ASSERT_LIVE_OUT);
    }
    
    void Node::add_assert_live_out_var(const Nodecl::List& new_assert_live_out_vars)
    {
        add_vars_to_container<NodeclSet>(NodeclSet(new_assert_live_out_vars.begin(), new_assert_live_out_vars.end()), 
                                         _ASSERT_LIVE_OUT);
    }
    
    NodeclSet Node::get_assert_dead_vars()
    {
        return get_vars<NodeclSet>(_ASSERT_DEAD);
    }
    
    void Node::add_assert_dead_var(const Nodecl::List& new_assert_dead_vars)
    {
        add_vars_to_container<NodeclSet>(NodeclSet(new_assert_dead_vars.begin(), new_assert_dead_vars.end()), 
                                         _ASSERT_DEAD);
    }    
    
    NodeclMap Node::get_assert_reaching_definitions_in()
    {
        return get_vars<NodeclMap>(_ASSERT_REACH_DEFS_IN);
    }

    void Node::add_assert_reaching_definitions_in(const Nodecl::List& new_assert_reach_defs_in)
    {   
        NodeclMap assert_reach_defs_in = get_assert_reaching_definitions_in();
        for (Nodecl::List::const_iterator it = new_assert_reach_defs_in.begin(); 
             it != new_assert_reach_defs_in.end(); ++it)
        {
            Nodecl::Analysis::ReachDefExpr rd = it->as<Nodecl::Analysis::ReachDefExpr>();
            NBase rd_var(rd.get_expression());
            assert_reach_defs_in.insert(
                std::pair<NBase, NodeclPair>(rd_var, NodeclPair(rd.get_value(), NBase::null())));
        }
        set_data(_ASSERT_REACH_DEFS_IN, assert_reach_defs_in);
    }
    
    NodeclMap Node::get_assert_reaching_definitions_out()
    {
        return get_vars<NodeclMap>(_ASSERT_REACH_DEFS_OUT);
    }
    
    void Node::add_assert_reaching_definitions_out(const Nodecl::List& new_assert_reach_defs_out)
    {   
        NodeclMap assert_reach_defs_out = get_assert_reaching_definitions_out();
        for (Nodecl::List::const_iterator it = new_assert_reach_defs_out.begin(); 
             it != new_assert_reach_defs_out.end(); ++it)
        {
            Nodecl::Analysis::ReachDefExpr rd = it->as<Nodecl::Analysis::ReachDefExpr>();
            NBase rd_var(rd.get_expression());
            assert_reach_defs_out.insert(
                std::pair<NBase, NodeclPair>(rd_var, NodeclPair(rd.get_value(), NBase::null())));
        }
        set_data(_ASSERT_REACH_DEFS_OUT, assert_reach_defs_out);
    }
    
    Utils::InductionVarList Node::get_assert_induction_vars()
    {
        return get_vars<Utils::InductionVarList>(_ASSERT_INDUCTION_VARS);
    }
    
    void Node::add_assert_induction_variables(const Nodecl::List& new_assert_induction_vars)
    {
        Utils::InductionVarList assert_induction_vars = get_assert_induction_vars();
        for (Nodecl::List::const_iterator it = new_assert_induction_vars.begin(); 
            it != new_assert_induction_vars.end(); ++it)
        {
            Nodecl::Analysis::InductionVarExpr iv = it->as<Nodecl::Analysis::InductionVarExpr>();
            Utils::InductionVar* iv_data = new Utils::InductionVar(NBase(iv.get_variable()));
            Nodecl::List lower = iv.get_lower().as<Nodecl::List>();
            Nodecl::List upper = iv.get_upper().as<Nodecl::List>();
            iv_data->set_lb(NodeclSet(lower.begin(), lower.end()));
            iv_data->set_ub(NodeclSet(upper.begin(), upper.end()));
            iv_data->set_increment(iv.get_stride());
            assert_induction_vars.insert(iv_data);
        }
        set_data(_ASSERT_INDUCTION_VARS, assert_induction_vars);
    }
    
    NodeclSet Node::get_assert_auto_sc_firstprivate_vars()
    {
        return get_vars<NodeclSet>(_ASSERT_AUTOSC_FIRSTPRIVATE);
    }
    
    void Node::add_assert_auto_sc_firstprivate_var(const Nodecl::List& new_assert_auto_sc_fp)
    {
        add_vars_to_container(NodeclSet(new_assert_auto_sc_fp.begin(), new_assert_auto_sc_fp.end()), 
                              _ASSERT_AUTOSC_FIRSTPRIVATE);
    }
    
    NodeclSet Node::get_assert_auto_sc_private_vars()
    {
        return get_vars<NodeclSet>(_ASSERT_AUTOSC_PRIVATE);
    }
    
    void Node::add_assert_auto_sc_private_var(const Nodecl::List& new_assert_auto_sc_p)
    {
        add_vars_to_container(NodeclSet(new_assert_auto_sc_p.begin(), new_assert_auto_sc_p.end()), 
                              _ASSERT_AUTOSC_PRIVATE);
    }
    
    NodeclSet Node::get_assert_auto_sc_shared_vars()
    {
        return get_vars<NodeclSet>(_ASSERT_AUTOSC_SHARED);
    }
    
    void Node::add_assert_auto_sc_shared_var(const Nodecl::List& new_assert_auto_sc_s)
    {
        add_vars_to_container(NodeclSet(new_assert_auto_sc_s.begin(), new_assert_auto_sc_s.end()), 
                              _ASSERT_AUTOSC_SHARED);
    }

    Utils::InductionVarList Node::get_assert_ranges()
    {
        return get_vars<Utils::InductionVarList>(_ASSERT_RANGE);
    }

    void Node::add_assert_ranges(const Nodecl::List& new_assert_ranges)
    {
        Utils::InductionVarList assert_ranges = get_assert_ranges();
        for (Nodecl::List::const_iterator it = new_assert_ranges.begin();
             it != new_assert_ranges.end(); ++it)
        {
            Nodecl::Analysis::InductionVarExpr v = it->as<Nodecl::Analysis::InductionVarExpr>();
            Utils::InductionVar* v_data = new Utils::InductionVar(NBase(v.get_variable()));
            Nodecl::List lower = v.get_lower().as<Nodecl::List>();
            Nodecl::List upper = v.get_upper().as<Nodecl::List>();
            v_data->set_lb(NodeclSet(lower.begin(), lower.end()));
            v_data->set_ub(NodeclSet(upper.begin(), upper.end()));
            v_data->set_increment(v.get_stride());
            assert_ranges.insert(v_data);
        }
        set_data(_ASSERT_RANGE, assert_ranges);
    }

    Nodecl::List Node::get_assert_correctness_auto_storage_vars()
    {
        return get_vars<Nodecl::List>(_ASSERT_CORRECTNESS_AUTO_STORAGE_VARS);
    }
    
    void Node::add_assert_correctness_auto_storage_var(const Nodecl::List& vars)
    {
        add_vars_to_list(vars, _ASSERT_CORRECTNESS_AUTO_STORAGE_VARS);
    }
    
    Nodecl::List Node::get_assert_correctness_dead_vars()
    {
        return get_vars<Nodecl::List>(_ASSERT_CORRECTNESS_DEAD_VARS);
    }
    
    void Node::add_assert_correctness_dead_var(const Nodecl::List& vars)
    {
        add_vars_to_list(vars, _ASSERT_CORRECTNESS_DEAD_VARS);
    }
    
    Nodecl::List Node::get_assert_correctness_incoherent_fp_vars()
    {
        return get_vars<Nodecl::List>(_ASSERT_CORRECTNESS_INCOHERENT_FP_VARS);
    }
    
    void Node::add_assert_correctness_incoherent_fp_var(const Nodecl::List& vars)
    {
        add_vars_to_list(vars, _ASSERT_CORRECTNESS_INCOHERENT_FP_VARS);
    }
    
    Nodecl::List Node::get_assert_correctness_incoherent_in_vars()
    {
        return get_vars<Nodecl::List>(_ASSERT_CORRECTNESS_INCOHERENT_IN_VARS);
    }
    
    void Node::add_assert_correctness_incoherent_in_var(const Nodecl::List& vars)
    {
        add_vars_to_list(vars, _ASSERT_CORRECTNESS_INCOHERENT_IN_VARS);
    }
    
    Nodecl::List Node::get_assert_correctness_incoherent_in_pointed_vars()
    {
        return get_vars<Nodecl::List>(_ASSERT_CORRECTNESS_INCOHERENT_IN_POINTED_VARS);
    }

    void Node::add_assert_correctness_incoherent_in_pointed_var(const Nodecl::List& vars)
    {
        add_vars_to_list(vars, _ASSERT_CORRECTNESS_INCOHERENT_IN_POINTED_VARS);
    }

    Nodecl::List Node::get_assert_correctness_incoherent_out_vars()
    {
        return get_vars<Nodecl::List>(_ASSERT_CORRECTNESS_INCOHERENT_OUT_VARS);
    }
    
    void Node::add_assert_correctness_incoherent_out_var(const Nodecl::List& vars)
    {
        add_vars_to_list(vars, _ASSERT_CORRECTNESS_INCOHERENT_OUT_VARS);
    }
    
    Nodecl::List Node::get_assert_correctness_incoherent_out_pointed_vars()
    {
        return get_vars<Nodecl::List>(_ASSERT_CORRECTNESS_INCOHERENT_OUT_POINTED_VARS);
    }
    
    void Node::add_assert_correctness_incoherent_out_pointed_var(const Nodecl::List& vars)
    {
        add_vars_to_list(vars, _ASSERT_CORRECTNESS_INCOHERENT_OUT_POINTED_VARS);
    }
    
    Nodecl::List Node::get_assert_correctness_incoherent_p_vars()
    {
        return get_vars<Nodecl::List>(_ASSERT_CORRECTNESS_INCOHERENT_P_VARS);
    }
    
    void Node::add_assert_correctness_incoherent_p_var(const Nodecl::List& vars)
    {
        add_vars_to_list(vars, _ASSERT_CORRECTNESS_INCOHERENT_P_VARS);
    }
    
    Nodecl::List Node::get_assert_correctness_race_vars()
    {
        return get_vars<Nodecl::List>(_ASSERT_CORRECTNESS_RACE_VARS);
    }
    
    void Node::add_assert_correctness_race_var(const Nodecl::List& vars)
    {
        add_vars_to_list(vars, _ASSERT_CORRECTNESS_RACE_VARS);
    }
    
    // **************** END getters and setters for analysis checking *************** //
    // ****************************************************************************** //
    
    

    // ****************************************************************************** //
    // *********************************** Utils ************************************ //

    static std::string print_set(NodeclSet es_set)
    {
        std::string result;
        for (NodeclSet::iterator it = es_set.begin(); it != es_set.end(); ++it)
            result += it->prettyprint() + ", ";
        if (!es_set.empty())
        result.erase(result.end() - 2, result.end());
        return result;
    }

    void Node::print_use_def_chains()
    {
        if (VERBOSE)
        {
            NodeclSet ue_vars = get_data<NodeclSet>(_UPPER_EXPOSED);
            std::cerr << " - Upper Exposed: " << print_set(ue_vars) << std::endl;

            NodeclSet killed_vars = get_data<NodeclSet>(_KILLED);
            std::cerr << " - Killed: " << print_set(killed_vars);

            NodeclSet undef_vars = get_data<NodeclSet>(_UNDEF);
            std::cerr << " - Undefined usage: " << print_set(undef_vars);
        }
    }

    void Node::print_liveness()
    {
        if (VERBOSE)
        {
            NodeclSet live_in_vars = get_data<NodeclSet>(_LIVE_IN);
            std::cerr << " - Live in: " << print_set(live_in_vars) << std::endl;

            NodeclSet live_out_vars = get_data<NodeclSet>(_LIVE_OUT);
            std::cerr << " - Live out: " << print_set(live_out_vars) << std::endl;
        }
    }

    void Node::print_auto_scoping()
    {
        if (VERBOSE)
        {
            NodeclSet private_vars = get_sc_private_vars();
            NodeclSet firstprivate_vars = get_sc_firstprivate_vars();
            NodeclSet race_vars = get_sc_race_vars();
            NodeclSet shared_vars = get_sc_shared_vars();
            NodeclSet undef_vars = get_sc_undef_vars();

            if (!private_vars.empty())
                std::cerr << "   Variables autoscoped as private: "             << print_set(private_vars)      << std::endl;

            if (!firstprivate_vars.empty())
                std::cerr << "   Variables autoscoped as firstprivate: "        << print_set(firstprivate_vars) << std::endl;

            if (!race_vars.empty())
                std::cerr << "   Variables autoscoped as race: "                << print_set(race_vars)         << std::endl;

            if (!shared_vars.empty())
                std::cerr << "   Variables autoscoped as shared: "              << print_set(shared_vars)            << std::endl;

            if (!undef_vars.empty())
                std::cerr << " Variables that cannot be automatically scoped: " << print_set(undef_vars)        << std::endl;
        }
    }

    void Node::print_task_dependencies()
    {
        if (VERBOSE)
        {
            std::string private_s      = " - Private: "      + print_set(get_deps_private_vars())      + "\n";
            std::string firstprivate_s = " - Firstprivate: " + print_set(get_deps_firstprivate_vars()) + "\n";
            std::string shared_s       = " - Shared: "       + print_set(get_deps_shared_vars())       + "\n";
            std::string in_s           = " - In deps: "      + print_set(get_deps_in_exprs())          + "\n";
            std::string out_s          = " - Out deps: "     + print_set(get_deps_out_exprs())         + "\n";
            std::string inout_s        = " - Inout deps: "   + print_set(get_deps_inout_exprs())       + "\n";
            std::string undef_s        = " - Undef deps: "   + print_set(get_deps_undef_vars())        + "\n";

            std::cerr << private_s << firstprivate_s << shared_s
                      << in_s << out_s << inout_s << undef_s << std::endl;
        }
    }

    // ********************************* END utils ********************************** //
    // ****************************************************************************** //
}
}
