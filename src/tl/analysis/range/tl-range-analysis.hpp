/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona* Supercomputing Center
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

#ifndef TL_RANGE_ANALYSIS_HPP
#define TL_RANGE_ANALYSIS_HPP

#include "tl-extensible-graph.hpp"
#include "tl-range-analysis-utils.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ****************************** Classes implementing constraint graph ******************************* //
    
    class CGEdge;
    class LIBTL_CLASS CGNode
    {
    private:
        // *** Members *** //
        unsigned int _id;
        NBase _value;
        ObjectList<CGEdge*> _entries;
        ObjectList<CGEdge*> _exits;
        
        int _scc_index;
        int _scc_lowlink_index;
        
    public:    
        // *** Constructor *** //
        CGNode(const NBase& value);
        
        // *** Getters and setters *** //
        unsigned int get_id() const;
        NBase get_value() const;
        
        ObjectList<CGEdge*> get_entries() const;
        ObjectList<CGNode*> get_parents();
        void add_entry(CGEdge* e);
        CGEdge* add_parent(CGNode* parent, NBase predicate = NBase::null());
        
        ObjectList<CGEdge*> get_exits() const;
        ObjectList<CGNode*> get_children();
        void add_exit(CGEdge* e);
        CGEdge* add_child(CGNode* child, NBase predicate = NBase::null());
        
        int get_scc_index() const;
        void set_scc_index(int scc_index);
        int get_scc_lowlink_index() const;
        void set_scc_lowlink_index(int scc_lowlink_index);
    };
    
    class LIBTL_CLASS CGEdge
    {
    private:
        // *** Members *** //
        CGNode* _source;
        CGNode* _target;
        NBase _predicate;
        
    public:
        // *** Constructor *** //
        CGEdge(CGNode* source, CGNode* target, const NBase& predicate);
        
        // *** Getters and setters *** //
        CGNode* get_source() const;
        CGNode* get_target() const;
        NBase get_predicate() const;
    };
    
    typedef std::map<NBase, CGNode*, Nodecl::Utils::Nodecl_structural_less> CGNode_map;
    
    class LIBTL_CLASS SCC
    {
    private:
        // *** Members *** //
        std::vector<CGNode*> _nodes;
        
    public:
        // *** Getters and setters *** //
        bool empty() const;
        std::vector<CGNode*> get_nodes() const;
        void add_node(CGNode* n);
    };
    
    class LIBTL_CLASS ConstraintGraph
    {
    private:
        // *** Members *** //
        std::string _name;
        CGNode_map _nodes;
        
        void print_graph_rec(std::ofstream& dot_file);
        
    public:
        // *** Constructor *** //
        ConstraintGraph(std::string name);
        
        // *** Modifiers *** //
        //! Insert, if it is not yet there, a new node in the CG with the value #value
        CGNode* insert_node(const NBase& value);
        
        //! Returns the node in the CG corresponding to the value #value
        CGNode* get_node(const NBase& value);
        
        //! Connects nodes #source and #target with a directed edge extended with #predicate
        void connect_nodes(CGNode* source, CGNode* target, NBase predicate = NBase::null());
        
        // Decompose the Constraint Graph in a set of Strongly Connected Components
        std::vector<SCC> extract_strongly_connected_components();
        
        //! Generates a dot file with the structure of the graph
        void print_graph();
    };
    
    // **************************** END classes implementing constraint graph ***************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // **************************** Visitor implementing constraint building ****************************** //
    
    class LIBTL_CLASS ConstraintReplacement : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        Utils::ConstraintMap _constraints_map;
        
    public:
        // *** Constructor *** //
        ConstraintReplacement(Utils::ConstraintMap constraints_map);
        
        // *** Visiting methods *** //
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
        Ret visit(const Nodecl::Symbol& n);
    };
    
    class LIBTL_CLASS ConstraintBuilderVisitor : public Nodecl::NodeclVisitor<Utils::Constraint>
    {
    private:
        // map containing the constraints arriving at the nodecl being visited
        Utils::ConstraintMap _input_constraints_map;        // Constraints coming from the parents or from previous statements in the current node
        Utils::ConstraintMap _output_constraints_map;       // Constraints computed so far for the current node
        Utils::ConstraintMap _output_true_constraints_map;  // Constraints for the child of the current node that reaches when the condition of the current node evaluates to true
        Utils::ConstraintMap _output_false_constraints_map; // Constraints for the child of the current node that reaches when the condition of the current node evaluates to false
        
        Ret visit_assignment(const NBase& lhs, const NBase& rhs);
        
    public:
        
        // *** Constructor *** //
        ConstraintBuilderVisitor(Utils::ConstraintMap input_constraints, 
                                 Utils::ConstraintMap current_constraints );
        
        // *** Modifiers *** //
        void compute_constraints(const NBase& n);
        
        // *** Getters and setters *** //
        Utils::ConstraintMap get_output_constraints_map();
        Utils::ConstraintMap get_output_true_constraints_map();
        Utils::ConstraintMap get_output_false_constraints_map();
        
        // *** Consultants *** //
        bool new_constraint_is_repeated(const Utils::Constraint& c);
        
        // *** Visiting methods *** //
        Ret join_list(TL::ObjectList<Utils::Constraint>& list);
        Ret visit(const Nodecl::AddAssignment& n);
        Ret visit(const Nodecl::Assignment& n);
        Ret visit(const Nodecl::LowerThan& n);
        Ret visit(const Nodecl::Mod& n);
        Ret visit(const Nodecl::ObjectInit& n);
        Ret visit(const Nodecl::Preincrement& n);
    };
    
    // ************************** END Visitor implementing constraint building **************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ******************************** Class implementing range analysis ********************************* //
    
    class LIBTL_CLASS RangeAnalysis
    {
    private:
        ExtensibleGraph* _pcfg;
        ConstraintGraph* _cg;
        
        void set_parameters_constraints();
        void compute_initial_constraints(Node* entry);
        void propagate_constraints_from_backwards_edges(Node* n);
        void create_constraint_graph(Node* n);
        
    public:
        //! Constructor
        RangeAnalysis(ExtensibleGraph* pcfg);
        
        //! Method computing the Ranges information on the member #pcfg
        void compute_range_analysis();
    };

    // ****************************** End class implementing range analysis ******************************* //
    // **************************************************************************************************** //
    
}
}

#endif      // TL_LIVENESS_HPP