/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona* Supercomputing Center
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
#include "tl-range-utils.hpp"

namespace TL {
namespace Analysis {

    typedef std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> SSAVarToValue_map;
    typedef std::map<NBase, CGNode*, Nodecl::Utils::Nodecl_structural_less> CGValueToCGNode_map;
    
    
    // **************************************************************************************************** //
    // **************************** Visitor implementing constraint building ****************************** //
    
    class LIBTL_CLASS ConstraintReplacement : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        Utils::VarToConstraintMap _constraints_map;
        
    public:
        // *** Constructor *** //
        ConstraintReplacement(Utils::VarToConstraintMap constraints_map);
        
        // *** Visiting methods *** //
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
        Ret visit(const Nodecl::Symbol& n);
    };
    
    class LIBTL_CLASS ConstraintBuilderVisitor : public Nodecl::NodeclVisitor<void>
    {
    private:
        //! PCFG node related to the constraints that are to be built
        Node* _n;
        
        // map containing the constraints arriving at the nodecl being visited
        Utils::VarToConstraintMap _input_constraints_map;        // Constraints coming from the parents or from previous statements in the current node
        Utils::VarToConstraintMap _output_constraints_map;       // Constraints computed so far for the current node
        Utils::VarToConstraintMap _output_true_constraints_map;  // Constraints for the child of the current node that reaches when the condition of the current node evaluates to true
        Utils::VarToConstraintMap _output_false_constraints_map; // Constraints for the child of the current node that reaches when the condition of the current node evaluates to false
        
        SSAVarToValue_map *_constraints;
        NodeclList *_ordered_constraints;
        
        Symbol get_condition_node_constraints(const NBase& lhs, const Type& t, 
                                              std::string s_str, std::string nodecl_str);
        Ret visit_assignment(const NBase& lhs, const NBase& rhs);
        Ret visit_increment(const NBase& rhs, bool positive);
        
    public:
        
        // *** Constructor *** //
        ConstraintBuilderVisitor(Node* n,
                Utils::VarToConstraintMap input_constraints, 
                Utils::VarToConstraintMap current_constraints, 
                SSAVarToValue_map *constraints,
                NodeclList *ordered_constraints);
        
        ConstraintBuilderVisitor(Node* n,
                SSAVarToValue_map *constraints,
                NodeclList *ordered_constraints);
        
        // *** Modifiers *** //
        Utils::Constraint build_constraint(const Symbol& s, const NBase& val, const Type& t, std::string c_name);
        void compute_stmt_constraints(const NBase& n);
        void compute_parameters_constraints(const ObjectList<Symbol>& params);
        
        // *** Getters and setters *** //
        Utils::VarToConstraintMap get_output_constraints_map();
        Utils::VarToConstraintMap get_output_true_constraints_map();
        Utils::VarToConstraintMap get_output_false_constraints_map();
        
        // *** Consultants *** //
        bool new_constraint_is_repeated(const Utils::Constraint& c);
        
        // *** Visiting methods *** //
        Ret join_list(TL::ObjectList<Utils::Constraint>& list);
        Ret visit(const Nodecl::AddAssignment& n);
        Ret visit(const Nodecl::Assignment& n);
        Ret visit(const Nodecl::Equal& n);
        Ret visit(const Nodecl::GreaterThan& n);
        Ret visit(const Nodecl::GreaterOrEqualThan& n);
        Ret visit(const Nodecl::LogicalAnd& n);
        Ret visit(const Nodecl::LowerOrEqualThan& n);
        Ret visit(const Nodecl::LowerThan& n);
        Ret visit(const Nodecl::Mod& n);
        Ret visit(const Nodecl::ObjectInit& n);
        Ret visit(const Nodecl::Postdecrement& n);
        Ret visit(const Nodecl::Postincrement& n);
        Ret visit(const Nodecl::Predecrement& n);
        Ret visit(const Nodecl::Preincrement& n);
    };
    
    // ************************** END Visitor implementing constraint building **************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ****************************** Classes implementing constraint graph ******************************* //
    
    class LIBTL_CLASS ConstraintGraph
    {
    private:
        // *** Members *** //
        std::string _name;
        CGValueToCGNode_map _nodes;
        std::map<CGNode*, SCC*> _node_to_scc_map;
        
        //! Method building the SCCs from the Constraint Graph. It follows the Tarjan's method to do so
        void strong_connect(CGNode* n, unsigned int& scc_current_index, 
                            std::stack<CGNode*>& s, std::vector<SCC*>& scc_list, 
                            std::map<CGNode*, int>& scc_lowlink_index,
                            std::map<CGNode*, int>& scc_index);
        
        //! Insert, if it is not yet there, a new node in the CG with the value #value
        CGNode* insert_node(const NBase& value);
        CGNode* insert_node(CGNodeType type);
        
        //! Connects nodes #source and #target with a directed edge extended with #predicate
        void connect_nodes(CGNode* source, CGNode* target, NBase predicate = NBase::null());
        
        //! Method to solve constraints within a cycle
        void resolve_cycle(SCC* scc);
        
        //! Method to evaluate the ranges in a sinle Constraint Graph node
        void evaluate_cgnode(CGNode* const node);
        
    public:
        // *** Constructor *** //
        ConstraintGraph(std::string name);
        
        // *** Getters and setters *** //
        //! Retrieves the Constraint Graph node given a SSA variable
        CGNode* get_node_from_ssa_var(const NBase& n);
        
        // *** Modifiers *** //
        void fill_constraint_graph(
                const SSAVarToValue_map& constraints,
                const NodeclList& ordered_constraints);
        
        //! Decompose the Constraint Graph in a set of Strongly Connected Components
        std::vector<SCC*> topologically_compose_strongly_connected_components();
        
        //! Use the different rules to topologically solve and propagate the constraints over the CG
        void solve_constraints(const std::vector<SCC*>& roots);
        
        // *** Utils *** //
        //! Generates a dot file with the structure of the graph
        void print_graph();
    };
    
    // **************************** END classes implementing constraint graph ***************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ******************************** Class implementing range analysis ********************************* //
    
    class LIBTL_CLASS RangeAnalysis
    {
    private:
        ExtensibleGraph* _pcfg;
        ConstraintGraph* _cg;
        
        SSAVarToValue_map _constraints;
        NodeclList _ordered_constraints;
        
        //! Method computing constraints for the parameters of a function
        void compute_parameters_constraints(
                std::map<Node*, Utils::VarToConstraintMap>& constr_map);
        
        //! Method computing the constraints of the whole #pcfg
        //! It perform deep first search over the #pcfg without back edges
        void compute_constraints_rec(
                Node* n, 
                std::map<Node*, Utils::VarToConstraintMap>& constr_map, 
                std::map<Node*, Utils::VarToConstraintMap>& propagated_constr_map);
        
        //! Method propagating constraints from the back edges of the #pcfg
        void propagate_constraints_from_back_edges(
                Node* n, 
                std::map<Node*, Utils::VarToConstraintMap>& constr_map, 
                std::map<Node*, Utils::VarToConstraintMap>& propagated_constr_map);
        
        //! Method generating all constraints of the #pcfg
        void compute_constraints(
            std::map<Node*, Utils::VarToConstraintMap>& constr_map,
            std::map<Node*, Utils::VarToConstraintMap>& propagated_constr_map);
        
        //! Method building a Constraint Graph from a set of constraints
        void build_constraint_graph();
        
        //! Method propagating ranges information from the #cg to the #pcfg
        void set_ranges_to_pcfg(
            const std::map<Node*, Utils::VarToConstraintMap>& constr_map);

    public:
        //! Constructor
        RangeAnalysis(ExtensibleGraph* pcfg);
        
        //! Method computing the Ranges information on the #pcfg
        void compute_range_analysis();
        
        //! Method printing all constraint found so far in the #pcfg
        void print_constraints();
    };

    // ****************************** End class implementing range analysis ******************************* //
    // **************************************************************************************************** //
    
}
}

#endif      // TL_LIVENESS_HPP
