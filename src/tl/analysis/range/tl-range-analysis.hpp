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

#include <queue>

#include "tl-extensible-graph.hpp"
#include "tl-range-utils.hpp"
#include "tl-ssa.hpp"

namespace TL {
namespace Analysis {

    typedef std::map<Symbol, NBase> Constraints;
    /* This must be a multimap, so constant values may be repeated */
    typedef std::multimap<NBase, CGNode*, Nodecl::Utils::Nodecl_structural_less> CGValueToCGNode_map;

    // **************************************************************************************************** //
    // **************************** Visitor implementing constraint building ****************************** //

    //! ConstraintReplacement replaces the variables in a constraint value with the corresponding ssa symbols
    /*!
     * During the replacement it may happen that an expression does not have an incoming ssa symbol
     * This may occur for function calls, array subscripts and class member accesses
     * In this case, we build a constraint in place
     */
    class LIBTL_CLASS ConstraintReplacement : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        VarToConstraintMap* _input_constraints;

        // Necessary members for storing those constraints built in place
        Constraints *_constraints;
        std::vector<Symbol> *_ordered_constraints;

    public:
        // *** Constructor *** //
        ConstraintReplacement(
                VarToConstraintMap* constraints_map,
                Constraints *constraints,
                std::vector<Symbol> *ordered_constraints);

        // *** Visiting methods *** //
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
        Ret visit(const Nodecl::Conversion& n);
        Ret visit(const Nodecl::FunctionCall& n);
        Ret visit(const Nodecl::Symbol& n);
    };

    class LIBTL_CLASS ConstraintBuilder : public Nodecl::NodeclVisitor<void>
    {
    private:
        // map containing the constraints arriving at the nodecl being visited
        VarToConstraintMap _input_constraints;        // Constraints coming from the parents or from previous statements in the current node
        VarToConstraintMap _output_constraints;       // Constraints computed so far for the current node
        VarToConstraintMap _output_true_constraints;  // Constraints for the child of the current node that reaches when the condition of the current node evaluates to true
        VarToConstraintMap _output_false_constraints; // Constraints for the child of the current node that reaches when the condition of the current node evaluates to false

        Constraints *_constraints;
        std::vector<Symbol> *_ordered_constraints;

        // ************ Private visiting methods ************ //
        Ret visit_assignment(const NBase& lhs, const NBase& rhs);
        Ret visit_increment(const NBase& rhs, bool positive);
        void create_array_fake_constraint(const NBase& n);
        void create_struct_fake_constraint(const NBase& n);
        void compute_comparison_constraint(
            /*in*/  const NBase& n,
            /*in*/  const NBase& m,
            /*in*/  node_t comparison_kind,
            /*out*/ NBase& val_true,
            /*out*/ NBase& val_false);
        void build_equal_constraint(
            /*in*/  const Symbol& ssa_sym,
            /*in*/  const NBase& n,
            /*in*/  Type t,
            /*out*/ NBase& val_true,
            /*out*/ NBase& val_false);
        void build_lower_constraint(
            /*in*/  const Symbol& ssa_sym,
            /*in*/  const NBase& n,
            /*in*/  Type t,
            /*out*/ NBase& val_true,
            /*out*/ NBase& val_false);
        void build_lower_or_equal_constraint(
            /*in*/  const Symbol& ssa_sym,
            /*in*/  const NBase& n,
            /*in*/  Type t,
            /*out*/ NBase& val_true,
            /*out*/ NBase& val_false);
        void build_greater_constraint(
            /*in*/  const Symbol& ssa_sym,
            /*in*/  const NBase& n,
            /*in*/  Type t,
            /*out*/ NBase& val_true,
            /*out*/ NBase& val_false);
        void build_greater_or_equal_constraint(
            /*in*/  const Symbol& ssa_sym,
            /*in*/  const NBase& n,
            /*in*/  Type t,
            /*out*/ NBase& val_true,
            /*out*/ NBase& val_false);
        void visit_comparison(const NBase& lhs, const NBase& rhs, node_t comparison_kind);

    public:
        // *** Constructors *** //
        ConstraintBuilder(
                const VarToConstraintMap& input_constraints,
                Constraints *constraints,
                std::vector<Symbol> *ordered_constraints);

        ConstraintBuilder(
                const VarToConstraintMap& input_constraints,
                const VarToConstraintMap& current_constraints,
                Constraints *constraints,
                std::vector<Symbol> *ordered_constraints);

        // *** Modifiers *** //
        Utils::Constraint build_constraint(const Symbol& s,
                const NBase& val, Utils::ConstraintKind c_kind);
        void compute_parameters_constraints(const ObjectList<Symbol>& params);
        void compute_non_local_symbols_constraints(
            const ObjectList<Nodecl::Symbol>& non_local_syms);
        void set_false_constraint_to_inf(const NBase& n);

        // *** Getters and setters *** //
        VarToConstraintMap get_output_constraints() const;
        VarToConstraintMap get_output_true_constraints() const;
        VarToConstraintMap get_output_false_constraints() const;

        // *** Visiting methods *** //
        Ret join_list(TL::ObjectList<Utils::Constraint>& list);
        Ret visit(const Nodecl::AddAssignment& n);
        Ret visit(const Nodecl::Assignment& n);
        Ret visit(const Nodecl::Different& n);
        Ret visit(const Nodecl::Equal& n);
        Ret visit(const Nodecl::GreaterThan& n);
        Ret visit(const Nodecl::GreaterOrEqualThan& n);
        Ret visit(const Nodecl::LogicalAnd& n);
        Ret visit(const Nodecl::LogicalNot& n);
        Ret visit(const Nodecl::LowerOrEqualThan& n);
        Ret visit(const Nodecl::LowerThan& n);
        Ret visit(const Nodecl::Mod& n);
        Ret visit(const Nodecl::ObjectInit& n);
        Ret visit(const Nodecl::Postdecrement& n);
        Ret visit(const Nodecl::Postincrement& n);
        Ret visit(const Nodecl::Predecrement& n);
        Ret visit(const Nodecl::Preincrement& n);
        Ret visit(const Nodecl::ReturnStatement& n);
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
        CGNode* insert_node(const NBase& value, CGNodeType type=__Sym);
        CGNode* insert_node(CGNodeType type);

        //! Connects nodes #source and #target with a directed edge extended with #predicate
        void connect_nodes(
                CGNode* source, CGNode* target,
                bool is_back_edge = false, bool is_future_edge = false);
        void disconnect_nodes(
                CGNode* source, CGEdge* exit);

        void widen(SCC* scc);
        void futures(SCC* scc);
        void narrow(SCC* scc);

        //! Method to evaluate the ranges in a single Constraint Graph node
        // When narrowing, during the first iteration, we may not want to consider back edges,
        // otherwise Phi operations that went to +-inf will never change
        void evaluate_cgnode(
                CGNode* const node,
                bool narrowing = 0);

        CGNode* fill_cg_with_binary_op_rec(
                const NBase& val);

        //! Method collecting all constant values in the SCC
        std::set<const_value_t*> gather_scc_constants(SCC* scc);

    public:
        // *** Constructor *** //
        ConstraintGraph(std::string name);

        // *** Getters and setters *** //
        //! Retrieves the Constraint Graph node given a SSA variable
        CGNode* get_node_from_ssa_var(const NBase& n);

        // *** Modifiers *** //
        void fill_cg_with_binary_op(
                const NBase& s,
                const NBase& val);

        /*!Create a Constraint Graph from a list of constraints
         * \param[in] constraints map relating the SSA symbols and their value
         * \param[in] ordered_constraints vector with the SSA symbols created during constraints generation
         */
        void fill_constraint_graph(
                const Constraints& constraints,
                const std::vector<Symbol>& ordered_constraints);

        //! Decompose the Constraint Graph in a set of Strongly Connected Components
        std::vector<SCC*> topologically_compose_strongly_connected_components();
        
        //! Use the different rules to topologically solve and propagate the constraints over the CG
        void solve_constraints(const std::vector<SCC*>& roots);

        // *** Utils *** //
        //! Generates a dot file with the structure of the graph
        void print_graph() const;
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

        Constraints _constraints;
        std::vector<Symbol> _ordered_constraints;

        //! Method computing constraints for the parameters of a function
        //! \param[out] constr_map Map where constraints are stored for each PCFG node
        void compute_parameters_constraints(
                /*inout*/ std::map<Node*, VarToConstraintMap>& constr_map);

        //! Method computing constraints for all non local symbols of a function
        //! \param[out] constr_map Map where constraints are stored for each PCFG node
        void compute_non_local_symbols_constraints(
            /*out*/ std::map<Node*, VarToConstraintMap>& pcfg_constraints);

        //! Method computing the constraints of the whole #pcfg
        //! It performs breadth first search over the #pcfg without back edges
        void compute_constraints_rec(
                std::queue<Node*>& worklist,
                std::set<Node*>& treated,
                std::map<Node*, VarToConstraintMap>& constr_map);

        //! Method generating all constraints of the #pcfg
        void compute_constraints(
                std::map<Node*, VarToConstraintMap>& constr_map);

        //! Method that removes those constraints that are not used
        /*! During the construction of the constraints, some constraints
         *  may be built but generate unused SSA symbols
         *  (conditions in either loops of selection statements).
         *  These are not necessary nor useful, so it is better to remove them
         *  here and reduce the complexity of the Constraint Graph
         */
        void remove_unnecessary_constraints(std::map<Node*, VarToConstraintMap>& constr_map);

        //! Method building a Constraint Graph from a set of constraints
        void build_constraint_graph();

        //! Method propagating ranges information from the #cg to the #pcfg
        void set_ranges_to_pcfg(
            const std::map<Node*, VarToConstraintMap>& constr_map);

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

#endif      // TL_RANGE_ANALYSIS_HPP
