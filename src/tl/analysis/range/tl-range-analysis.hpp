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

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // Comparator for the values introduced in the Constraint Graph
    // We cannot use the Nodecl_structural_less becuase it compares constant values,
    // and, for convenience, we introduce constant values to constant constraints
    // For example:
    //      int a = 5;          -> a_0 = [5:5]; a_0 has const value 5
    //      b = a;              -> b_0 = a_0;   this a_0 does not have any const value
    int cmp_trees(nodecl_t n1, nodecl_t n2)
    {
        const bool n1_is_null = nodecl_is_null(n1);
        const bool n2_is_null = nodecl_is_null(n2);

        if ((n1_is_null && n2_is_null)
            || (nodecl_get_ast(n1) == nodecl_get_ast(n2)))
            return 0;

        if (n1_is_null == n2_is_null)
        {
            if(nodecl_get_kind(n1) == NODECL_CONVERSION)
                n1 = nodecl_get_child(n1, 0);
            if(nodecl_get_kind(n2) == NODECL_CONVERSION)
                n2 = nodecl_get_child(n2, 0);

            const node_t n1_kind = nodecl_get_kind(n1);
            const node_t n2_kind = nodecl_get_kind(n2);

            if (n1_kind == n2_kind) // kind
            {
                const scope_entry_t * const n1_symbol = nodecl_get_symbol(n1);
                const scope_entry_t * const n2_symbol = nodecl_get_symbol(n2);

                if (n1_symbol == n2_symbol) // symbol
                {
                    // Everything looks equal in this single node, let's check our children
                    int equal = 0;
                    for (int i=0; (equal == 0) && (i < MCXX_MAX_AST_CHILDREN); i++)
                    {
                        const nodecl_t n1_child = nodecl_get_child(n1, i);
                        const nodecl_t n2_child = nodecl_get_child(n2, i);

                        if(nodecl_is_null(n1_child) &&
                            nodecl_is_null(n2_child)) // Optimization: Skip recursive call.
                            continue;

                        equal = cmp_trees(n1_child, n2_child);
                    }

                    return equal;
                }
                else if (n1_symbol < n2_symbol) // symbol
                {
                    return -1;
                }
                else // symbol
                {
                    return 1;
                }
            }
            else if (n1_kind < n2_kind) // kind
            {
                return -1;
            }
            else // kind
            {
                return 1;
            }
        }
        else if (!n1_is_null)
        {
            return -1;
        }
        else
        {
            return 1;
        }
    }

    bool structurally_less(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2)
    {
        nodecl_t n1_ = n1.get_internal_nodecl();
        nodecl_t n2_ = n2.get_internal_nodecl();

        return cmp_trees(n1_, n2_) < 0;
    }

    struct RangeA_structural_less {
        bool operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const
        {
            return structurally_less(n1, n2);
        }
    };

    typedef std::map<Symbol, NBase> Constraints;
    /* This must be a multimap, so constant values may be repeated */
    typedef std::multimap<NBase, CGNode*, RangeA_structural_less> CGValueToCGNode_map;
    typedef std::map<NBase, Utils::Constraint, Nodecl::Utils::Nodecl_structural_less> VarToConstraintMap;

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
        Ret visit(const Nodecl::Cast& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
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
        void visit_comparison_side(
            const NBase& n,
            const NBase& val,
            char side /*l:left, r:right*/,
            node_t comparison_kind);
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
                bool narrowing = 0,
                bool consider_back_edges = 1);

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
