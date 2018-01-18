/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

#ifndef TL_TASK_DEPENDENCY_GRAPH_HPP
#define TL_TASK_DEPENDENCY_GRAPH_HPP

#include "tl-extensible-graph.hpp"
#include "tl-nodecl-replacer.hpp"

#include <deque>

#define TDG_DEBUG debug_options.tdg_verbose

namespace TL { 
namespace Analysis {

    extern int tdg_node_id;

    class FTDGNode;
    extern std::map<Node*, FTDGNode*> pcfg_to_ftdg;

    // ******************************************************************* //
    // ************ Task Dependency Graph Control Structures ************* //
    
    enum ControlStructureType {
        Implicit,
        Loop,
        IfElse
    };
    
    struct ControlStructure {
        int _id;
        ControlStructureType _type;
        NBase _condition;
        Node* _pcfg_node;
        ControlStructure* _enclosing;
        
        // *** Constructor *** //
        ControlStructure(int id, ControlStructureType type, 
                         const NBase& condition, Node* pcfg_node);
        
        // *** Getters and setters *** //
        int get_id() const;
        ControlStructureType get_type() const;
        std::string get_type_as_string() const;
        Nodecl::NodeclBase get_condition() const;
        Node* get_pcfg_node() const;
        ControlStructure* get_enclosing_cs() const;
        void set_enclosing_cs(ControlStructure* cs);
    };
    
    typedef std::vector<std::pair<ControlStructure*, std::string> > ControlStList;
    
    // ************ Task Dependency Graph Control Structures ************* //
    // ******************************************************************* //
    
    
    // ******************************************************************* //
    // ************** Task Dependency Graph Edges and Nodes ************** //
    
    struct TDG_Edge;
    
    enum TDGNodeType {
        Task,
        Taskpart,
        Target,
        Taskwait,
        Barrier,
        Unknown
    };
    
    struct TDG_Node {
        unsigned int _id;
        Node* _pcfg_node;
        TDGNodeType _type;
        Node* _parent;
        ObjectList<TDG_Edge*> _entries;
        ObjectList<TDG_Edge*> _exits;
        ControlStList _control_structures;

        Node* _init_taskpart;

        // *** Constructor *** //
        TDG_Node(Node* n, TDGNodeType type, Node* parent, Node* init_tp=NULL);
        
        // *** Getters and setters *** //
        unsigned int get_id() const;
        Node* get_pcfg_node() const;
        void add_control_structure(ControlStructure* cs, std::string taken_branch);
        ControlStList get_control_structures() const;

        friend struct TDG_Edge;
        friend class OldTaskDependencyGraph;
    };

    struct TDG_Edge {
        TDG_Node* _source;
        TDG_Node* _target;
        SyncKind _kind;
        NBase _condition;

        // *** Constructor *** //
        TDG_Edge(TDG_Node* source, TDG_Node* target, SyncKind kind, const NBase& condition);

        // *** Getters and setters *** //
        TDG_Node* get_source() const;
        TDG_Node* get_target() const;

        friend struct TDG_Node;
        friend class OldTaskDependencyGraph;
    };
    
    // ************** Task Dependency Graph Edges and Nodes ************** //
    // ******************************************************************* //


    typedef std::map<unsigned int, TDG_Node*> TDG_Node_map;
    typedef ObjectList<TDG_Edge*> TDG_Edge_list;


    // ******************************************************************* //
    // ******************** Old Task Dependency Graph ******************* //
    
    typedef std::multimap<std::pair<Node*, Node*>, ControlStructure*> PCFG_to_CS;
    
    class LIBTL_CLASS OldTaskDependencyGraph
    {
    private:
        // *** Class members *** //
        unsigned int _id;                               /*!< Identifier to be used at runtime */
        ExtensibleGraph* _pcfg;                         /*!< PCFG corresponding to the graph */
        std::string _json_name;                         /*!< Name of the TDG and the JSON file */
        std::map<unsigned int, TDG_Node*> _tdg_nodes;   /*!< Map of nodes in the TDG */
                                                        // We have a map where the key is the locus of the original statement
                                                        // this way we can print the nodes in order
        
        std::map<NBase, unsigned int, Nodecl::Utils::Nodecl_structural_less> _syms; /*!< Map of symbols appearing in the TDG associated to their identifier */
        PCFG_to_CS _pcfg_to_cs_map;                     /*!< Map of PCFG control structure nodes to their TDG control structure object */

        bool _taskparts_enabled;                        /*!< Enable support for taskparts analysis */


        // *** Not allowed construction methods *** //
        OldTaskDependencyGraph(const OldTaskDependencyGraph& n);
        OldTaskDependencyGraph& operator=(const OldTaskDependencyGraph&);

        // *** Private methods *** //
        void create_tdg_node(
                Node* current,
                int id,
                TDGNodeType type,
                Node* init_tp = NULL);

        void connect_tdg_nodes(
                TDG_Node* parent, TDG_Node* child,
                SyncKind sync_type, const NBase& condition);

        TDG_Node* find_tdg_node_from_pcfg_node(Node* task);

        void create_tdg_task_nodes_from_pcfg(Node* current);
        void create_tdg_nodes_from_pcfg(Node* current);
        void create_tdg_nodes_from_taskparts(Node* current);

        enum CS_case {
            same_cs_init_before_end,    /*this is valid also for non-taskpart nodes*/
            same_cs_end_before_init,
            init_encloses_end_init_before_end,
            init_encloses_end_end_before_init,
            end_encloses_init_init_before_end,
            end_encloses_init_end_before_init
        };
        void create_control_structure_rec(
                CS_case cs_case,
                Node* init,     /*NULL for non-taskpart nodes*/
                Node* n,
                Node* control_structure,
                TDG_Node* tdg_node,
                ControlStructure* last_cs);
        void create_control_structures(
                TDG_Node* tdg_node,
                ControlStructure* last_cs);
        void set_tdg_nodes_control_structures();

        /*!This method forces tasks to be connected to
         * their immediate previous static synchronization (taskwait or barrier)
         * The method traverses the graph from node \p sync until it exits or
         * finds a taskwait or barrier in the same context.
         * All tasks found during the traversal are connected to \p sync
         * with a Static synchronization edge
         *
         * \param sync a taskwait or barrier nodes
         */
        void connect_tasks_to_previous_synchronization(Node* sync);
        void connect_dependent_nodes(Node* n);
        void connect_taskparts();
        void connect_tdg_nodes_from_pcfg(Node* current);

        void store_condition_list_of_symbols(const NBase& condition, const NodeclMap& reach_defs);

        void create_tdg(Node* current);

        std::string print_value(const NBase& var, const NBase& value) const;
        void print_tdg_node_to_dot(TDG_Node* current, std::ofstream& dot_tdg) const;
        void print_condition(
                TDG_Edge* edge,
                ControlStructure* node_cs,
                std::ofstream& json_tdg,
                std::string indent) const;
        void print_tdg_control_structs_to_json(std::ofstream& json_tdg) const;

        void print_tdg_syms_to_json(std::ofstream& json_tdg);
        void print_dependency_variables_to_json(
            std::ofstream& json_tdg,
            const VarToNodeclMap& var_to_value_map,
            const VarToNodeclMap& var_to_id_map,
            const NBase& condition,
            std::string indent,
            bool is_source,
            bool add_final_comma) const;
        void print_tdg_nodes_to_json(std::ofstream& json_tdg);
        void print_tdg_edges_to_json(std::ofstream& json_tdg) const;
        
    public:
        // *** Constructor *** //
        OldTaskDependencyGraph(
                ExtensibleGraph* pcfg,
                std::string json_name,
                bool taskparts_enabled);
        
        // *** Getters and Setters *** //
        std::string get_name() const;
        ExtensibleGraph* get_pcfg() const;
        unsigned int get_id() const;
        
        // *** Printing methods *** //
        void print_tdg_to_dot() const;
        void print_tdg_to_json(std::ofstream& json_tdg);
        static void print_tdgs_to_json(const ObjectList<OldTaskDependencyGraph*>& tdgs);
    };

    // ******************** Old Task Dependency Graph ******************* //
    // ******************************************************************* //



    // ******************************************************************* //
    // ******************** Flow Task Dependency Graph ******************* //

    enum FTDGNodeType {
        FTDGLoop,
        FTDGCondition,
        FTDGTarget,
        FTDGTask,
        FTDGTaskwait,
        FTDGBarrier
    };

    class FTDGNode {
    private:
        unsigned _id;
        Node* _n;

        FTDGNode* _parent;                      // Nested tasks, parent
        ObjectList<FTDGNode*> _predecessors;    // Control and data-flow predecessors (siblings)

        enum FTDGNodeType _type;

        ObjectList<FTDGNode*> _inner_true;  // For true edges in an IfElse conditional and
                                            // for all inner nodes in Loops
        ObjectList<FTDGNode*> _inner_false; // For false edges in an IfElse conditional
        ObjectList<FTDGNode*> _outer;

    public:
        FTDGNode(unsigned id, Node* n, FTDGNodeType type);

        unsigned get_id() const;
        Node* get_pcfg_node() const;

        FTDGNode* get_parent() const;
        void set_parent(FTDGNode* n);
        const ObjectList<FTDGNode*>& get_predecessors() const;
        void add_predecessor(FTDGNode* predecessor);

        FTDGNodeType get_type() const;

        ObjectList<FTDGNode*> get_inner() const;
        const ObjectList<FTDGNode*>& get_inner_true() const;
        const ObjectList<FTDGNode*>& get_inner_false() const;
        const ObjectList<FTDGNode*>& get_outer() const;

        void add_inner(FTDGNode* n);
        void add_inner_true(FTDGNode* n);
        void add_inner_false(FTDGNode* n);
        void add_outer(FTDGNode* n);
    };

    class LIBTL_CLASS FlowTaskDependencyGraph
    {
    private:
        ExtensibleGraph* _pcfg;
        std::vector<FTDGNode*> _parents;                        // Parent FTDGNode for each nesting level of parallelism
        std::vector<std::vector<FTDGNode*> > _outermost_nodes;  // Set of outermost nodes for each nesting level of parallelism

        void build_siblings_flow_tdg(
            Node* n,
            std::stack<Node*> parent);
        void build_flow_tdg_rec(
            Node* n,
            std::stack<Node*> parent,
            std::vector<FTDGNode*>& control,
            bool conditional,
            bool true_edge);

        void print_tdg_node_to_dot(
                FTDGNode* n,
                std::string indent, std::string color,
                std::ofstream& dot_tdg,
                /*out*/ FTDGNode*& parent,
                /*out*/ FTDGNode*& head);

    public:
        FlowTaskDependencyGraph(ExtensibleGraph* pcfg);

        ExtensibleGraph* get_pcfg() const;
        const std::vector<FTDGNode*>& get_parents() const;
        const std::vector<std::vector<FTDGNode*> >& get_outermost_nodes() const;

        void print_tdg_to_dot();
    };

    // ******************** Flow Task Dependency Graph ******************* //
    // ******************************************************************* //



    // ******************************************************************* //
    // ****************** Expanded Task Dependency Graph ***************** //

    class SubETDG;
    class ETDGNode {
    private:
        unsigned _id;
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> _var_to_value;

        std::set<ETDGNode*> _inputs;
        std::set<ETDGNode*> _outputs;

        SubETDG* _child;   // Child TDG enclosed in an ETDGNode

        Node* _pcfg_node;

        bool _visited;

    public:
        ETDGNode(int id, Node* n);

        int get_id() const;

        std::set<ETDGNode*> get_inputs() const;
        void add_input(ETDGNode* n);
        void remove_input(ETDGNode* n);
        std::set<ETDGNode*> get_outputs() const;
        void add_output(ETDGNode* n);
        void remove_output(ETDGNode* n);

        SubETDG* get_child() const;
        void set_child(SubETDG* child);

        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> get_vars_map() const;
        void set_vars_map(std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> var_to_value);

        Node* get_pcfg_node() const;
        Nodecl::NodeclBase get_source_task() const;

        bool is_visited() const;
        void set_visited(bool visited);
    };

    class ReplaceAndEvalVisitor : public Nodecl::NodeclVisitor<bool>
    {
    private:
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> _lhs_vars;
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> _rhs_vars;
        bool _lhs;

        bool visit_comparison(const Nodecl::NodeclBase& n);

    public:
        ReplaceAndEvalVisitor(std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> lhs_vars,
                              std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> rhs_vars);

        bool unhandled_node(const NBase& n);
        bool join_list(ObjectList<bool>& list);

        bool visit(const Nodecl::Add& n);
        bool visit(const Nodecl::Conversion& n);
        bool visit(const Nodecl::Different& n);
        bool visit(const Nodecl::Equal& n);
        bool visit(const Nodecl::GreaterOrEqualThan& n);
        bool visit(const Nodecl::GreaterThan& n);
        bool visit(const Nodecl::IntegerLiteral& n);
        bool visit(const Nodecl::LogicalAnd& n);
        bool visit(const Nodecl::LogicalOr& n);
        bool visit(const Nodecl::LowerThan& n);
        bool visit(const Nodecl::LowerOrEqualThan& n);
        bool visit(const Nodecl::Minus& n);
        bool visit(const Nodecl::Symbol& n);
    };

    class LIBTL_CLASS SubETDG
    {
    private:
        std::vector<FTDGNode*> _ftdg_outermost_nodes;
        unsigned _tdg_id;
        unsigned _parent_tdg_id;

        unsigned _maxI;
        unsigned _maxT;

        ObjectList<ETDGNode*> _roots;
        std::set<ETDGNode*> _leafs;

        ObjectList<ETDGNode*> _tasks;

        std::map<Nodecl::NodeclBase, ObjectList<ETDGNode*> > _source_to_etdg_nodes;

        void expand_loop(
                FTDGNode* n,
                std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars,
                std::deque<unsigned>& loops_ids,
                std::string indent);
        void expand_condition(
                FTDGNode* n,
                std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars,
                std::deque<unsigned>& loops_ids,
                std::string indent);

        unsigned get_etdg_node_id(unsigned task_id, std::deque<unsigned> loops_ids);

        ETDGNode* create_task_node(FTDGNode* n, std::deque<unsigned> loops_ids, std::string indent);
        void connect_task_node(ETDGNode* etdg_n, FTDGNode* pcfg_n, std::string indent);
        bool compute_task_connections(
                ETDGNode* possible_source,
                ETDGNode* target,
                std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> target_vars_map,
                std::set<ETDGNode*>& all_possible_ancestors,
                std::string indent);
        void task_create_and_connect(
                FTDGNode* ftdg_n,
                std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars,
                std::deque<unsigned> loops_ids,
                std::string indent);
        ETDGNode* create_sync_node(FTDGNode* n, std::string indent);
        void connect_sync_node(ETDGNode* etdg_n, std::string indent);
        void sync_create_and_connect(FTDGNode* n, std::string indent);

        void connect_nodes(ETDGNode* source, ETDGNode* target, std::string indent);
        void disconnect_nodes(ETDGNode* source, ETDGNode* target, std::string indent);

        bool is_ancestor(ETDGNode* source, ETDGNode* target);

        void remove_task_transitive_inputs(ETDGNode* n);
        void remove_synchronizations_rec(ETDGNode* n);
        void remove_synchronizations();

        void clear_visits_rec(ETDGNode* n);

    public:
        SubETDG(
            unsigned maxI, unsigned maxT,
            unsigned parent_tdg_id, const std::vector<FTDGNode*>& outermost_nodes);

        void purge_subtdg();
        void expand_subtdg();

        unsigned get_tdg_id() const;
        unsigned get_parent_tdg_id() const;
        void set_parent_tdg_id(unsigned parent_tdg_id);

        unsigned get_maxI() const;
        unsigned get_maxT() const;

        const ObjectList<ETDGNode*>& get_roots() const;
        const std::set<ETDGNode*>& get_leafs() const;

        const ObjectList<ETDGNode*>& get_tasks() const;
        unsigned get_nTasks() const;

        const std::map<Nodecl::NodeclBase, ObjectList<ETDGNode*> >& get_source_to_etdg_nodes() const;

        void clear_visits();
    };

    struct LoopInfo {
        Utils::InductionVar* _iv;
        NBase _lb;
        NBase _ub;
        NBase _incr;
        unsigned _niter;

        LoopInfo(Utils::InductionVar* iv, NBase lb, NBase ub, NBase incr, unsigned niter)
            : _iv(iv), _lb(lb), _ub(ub), _incr(incr), _niter(niter)
        {}
    };

    class LIBTL_CLASS ExpandedTaskDependencyGraph
    {
    private:
        FlowTaskDependencyGraph* _ftdg;
        std::vector<SubETDG*> _etdgs;

        unsigned _maxI;
        unsigned _maxT;

        void compute_constants_rec(FTDGNode* n);
        void compute_constants();
        void expand_tdg();

        void print_tdg_to_dot_rec(ETDGNode* n, std::ofstream& dot_tdg);

    public:
        ExpandedTaskDependencyGraph(ExtensibleGraph* pcfg);

        FlowTaskDependencyGraph* get_ftdg() const;
        const std::vector<SubETDG*>& get_etdgs() const;
        unsigned get_maxI() const;
        unsigned get_maxT() const;

        void print_tdg_to_dot();
    };

    // ****************** Expanded Task Dependency Graph ***************** //
    // ******************************************************************* //



    // ******************************************************************* //
    // ****************** Runtime Task Dependency Graph ****************** //

    class LIBTL_CLASS TaskDependencyGraphMapper
    {
    private:
        ObjectList<ExpandedTaskDependencyGraph*> _etdgs;

    public:
        TaskDependencyGraphMapper(ObjectList<ExpandedTaskDependencyGraph*> etdgs);

        void generate_runtime_tdg();
    };

    // ****************** Runtime Task Dependency Graph ****************** //
    // ******************************************************************* //



    // ******************************************************************* //
    // ****************** Wrapper Task Dependency Graph ****************** //
    
    class LIBTL_CLASS TaskDependencyGraph
    {
    private:
        union {
            OldTaskDependencyGraph* otdg;
            ExpandedTaskDependencyGraph* etdg;
        } _tdg;

        bool _use_expanded;

    public:
        // *** Constructors *** //
        TaskDependencyGraph(ExtensibleGraph* pcfg);

        TaskDependencyGraph(
                ExtensibleGraph* pcfg,
                std::string json_name,
                bool taskparts_enabled);
    
        // *** Getters and Setters *** //
        std::string get_name() const;
        ExpandedTaskDependencyGraph* get_etdg() const;

        // *** Printing methods *** //
        void print_tdg_to_dot() const;
        static void print_tdgs_to_json(const ObjectList<TaskDependencyGraph*>& tdgs);
    };

    // ****************** Wrapper Task Dependency Graph ****************** //
    // ******************************************************************* //
}
}

#endif  // TL_TASK_DEPENDENCY_GRAPH_HPP
