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
    // ******************************** Class implementing range analysis ********************************* //
    
    class LIBTL_CLASS ConstraintReplacement : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        Utils::ConstraintMap _constraints;
        
    public:
        // *** Constructor *** //
        ConstraintReplacement(Utils::ConstraintMap constraints);
        
        // *** Visiting methods *** //
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
        Ret visit(const Nodecl::Symbol& n);
    };
    
    class LIBTL_CLASS ConstraintBuilderVisitor : public Nodecl::NodeclVisitor<Utils::Constraint>
    {
    private:
        // map containing the constraints arriving at the nodecl being visited
        Utils::ConstraintMap _input_constraints;        // Constraints coming from the parents or from previous statements in the current node
        Utils::ConstraintMap _output_constraints;       // Constraints computed so far for the current node
        Utils::ConstraintMap _output_true_constraints;  // Constraints for the child of the current node that reaches when the condition of the current node evaluates to true
        Utils::ConstraintMap _output_false_constraints; // Constraints for the child of the current node that reaches when the condition of the current node evaluates to false
        
        Ret visit_assignment(const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs);
        
    public:
        
        // *** Constructor *** //
        ConstraintBuilderVisitor(Utils::ConstraintMap input_constraints, 
                                 Utils::ConstraintMap current_constraints );
        
        // *** Modifiers *** //
        void compute_constraints(const Nodecl::NodeclBase& n);
        
        // *** Getters and setters *** //
        Utils::ConstraintMap get_output_constraints();
        Utils::ConstraintMap get_output_true_constraints();
        Utils::ConstraintMap get_output_false_constraints();
        
        // *** Consultants *** //
        bool new_constraint_is_repeated(const Utils::Constraint& c);
        
        // *** Visiting methods *** //
        Ret join_list(TL::ObjectList<Utils::Constraint>& list);
        Ret visit(const Nodecl::AddAssignment& n);
        Ret visit(const Nodecl::Assignment& n);
        Ret visit(const Nodecl::LowerThan& n);
        Ret visit(const Nodecl::ObjectInit& n);
        Ret visit(const Nodecl::Preincrement& n);
    };
    
    class LIBTL_CLASS RangeAnalysis
    {
    private:
        ExtensibleGraph* _graph;
        
        void compute_initial_constraints(Node* entry);
        void set_parameters_constraints();
        
    public:
        //! Constructor
        RangeAnalysis(ExtensibleGraph* graph);
        
        //! Method computing the Ranges information on the member #graph
        void compute_range_analysis();
        
        void propagate_constraints_from_backwards_edges(Node* n);
        void recompute_node_constraints(Node* n, Utils::ConstraintMap new_constraint_map);
        
    };

    // ****************************** End class implementing range analysis ******************************* //
    // **************************************************************************************************** //
    
}
}

#endif      // TL_LIVENESS_HPP