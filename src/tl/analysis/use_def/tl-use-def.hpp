/*--------------------------------------------------------------------
 ( C) Copyright 2006-2013 Barcelona* Supercomputing Center
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

#ifndef TL_USE_DEF_HPP
#define TL_USE_DEF_HPP


#include "tl-extensible-graph.hpp"
#include "tl-nodecl-calc.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // **************************** Class implementing use-definition analysis **************************** //

    //! Class implementing Use-Def Analysis
    class LIBTL_CLASS UseDef
    {
    private:
        //!Graph we are analyzing the usage which
        ExtensibleGraph* _graph;

        //!Graphs containing those functions which code is reachable
        ObjectList<ExtensibleGraph*>* _pcfgs;
        
        //!Usage of Global Variables and Reference parameters, necessary to propage usage information in recursive calls
        std::map<Symbol, Utils::UsageKind> _global_vars;
        std::map<Symbol, Utils::UsageKind> _reference_params;
        
        /*!Method that computes recursively the Use-Definition information from a node
         * \param current Node from which the method begins the computation
         * \param ipa Boolean indicating the Use-Def is only for global variables and referenced parameters
         * \param ipa_arguments List of Nodecl which are reference arguments in an IPA call
         */
        void compute_usage_rec( Node* current );
        
        /*!Recursive method that returns a list with three elements:
         * - The first is the list of upper exposed variables of the graph node;
         * - The second is the list of killed variables of the graph node
         * - The third is the list of undefined variables of the graph
         */
        ObjectList<Utils::ext_sym_set> get_use_def_over_nodes( Node* current );

        //!Propagate the Use-Def information from inner nodes to outer nodes
        void set_graph_node_use_def( Node* graph_node );

        void propagate_usage_over_task_creation( Node* task_creation );
        
        void merge_children_usage( Utils::ext_sym_set& ue_vars, Utils::ext_sym_set& killed_vars, 
                                   Utils::ext_sym_set& undef_vars, int node_id );
        
        //!Propagate the Use-Def information from tasks to the graph
        void propagate_tasks_usage_to_graph( Node* graph_node, Node* current, Node* last_sync );

    public:
        //! Constructor
        UseDef( ExtensibleGraph* graph, ObjectList<ExtensibleGraph*>* pcfgs );

        /*! Method computing the Use-Definition information on the member #graph
         * \param ipa Boolean indicating the Use-Def is only for global variables and referenced parameters
         * \param ipa_arguments List of Nodecl which are reference arguments in an IPA call
         *                      Only necessary when \ipa is true
         */
        void compute_usage( );
    };

    // ************************** End class implementing use-definition analysis ************************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ***************************** Class implementing use-definition visitor **************************** //

    //! This Class implements a Visitor that computes the Use-Definition information of a concrete statement
    //! and attaches this information to the Node in a PCFG which the statements belongs to
    class LIBTL_CLASS UsageVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:

        // *********************** Private members *********************** //

        //! Pointer to the Node in a PCFG where the Nodecl is contained
        //! The results of the analysis performed during the visit will be attached to the node
        Node* _node;

        //! State of the traversal
        /*!
         * This value will be true when the actual expression is a defined value
         * Otherwise, when the value is just used, the value will be false
         * By default this value will be false. Each time we change the value to true for any definition,
         * at the end of the recursion, we turn back the value to false
         */
        bool _define;

        //! Nodecl we are traversing actually
        /*!
         * This attribute stores the actual nodecl when we are traversing class member access.
         * It can be of type reference, dereference or array subscript
         */
        Nodecl::NodeclBase _current_nodecl;

        //!List of functions visited
        std::set<Symbol> _visited_functions;

        //! List of global variables and reference parameters appeared until a given point of the analysis
        std::map<Symbol, Utils::UsageKind>* _global_vars;
        std::map<Symbol, Utils::UsageKind>* _reference_params;

        /*! Boolean useful for split statements: we want to calculate the usage of a function call only once
         *  When a function call appears in a split statement we calculate the first time (the func_call node)
         *  but for the other nodes, we just propagate the information
         */
        bool _avoid_func_calls;
        
        // Current pcfg being analyzed
        // We want this here because when running IPa analysis, we need to propagate the Global Variables
        // used in called functions to the current graph, otherwise,
        // we can lose global variables usage over the nested function calls
        ExtensibleGraph* _pcfg;
        
        //! Graphs containing those functions which code is reachable
        ObjectList<ExtensibleGraph*>* _pcfgs;
        
        // *********************** Private methods *********************** //
        
        template<typename T>
        void visit_assignment( const T& n );
        
        //! This method implements the visitor for any Binary Assignment operation
        template<typename T>
        void visit_binary_assignment( const T& n );

        void function_visit( const Nodecl::NodeclBase& called_sym, const Nodecl::List& arguments );

        template<typename T>
        Ret visit_increment( const T& n );

        //!Prevents copy construction.
        UsageVisitor( const UsageVisitor& v );

        /*!Method that computs the usage of a node taking into account the previous usage in the same node
         * For nodes that has a part of an object that is upwards exposed and a part of an object that is killed,
         * this method computes the different parts
         */
        void set_var_usage_to_node( const Utils::ExtendedSymbol& var, Utils::UsageKind usage_kind );
        
        //! Overloaded method to compute a nodes usage for a set of extended symbols instead of a single symbol
        void set_var_usage_to_node( const Utils::ext_sym_set& var_set, Utils::UsageKind usage_kind );
        
        // Methods to parse the file containing C function definitions useful for Use-Def analysis
        void parse_parameter( std::string current_param, const Nodecl::NodeclBase& arg );
        bool parse_c_functions_file( Symbol func_sym, const Nodecl::List& args );
        
        Utils::ext_sym_set get_ipa_usage( Utils::UsageKind usage_kind, const Utils::ext_sym_set& list,
                                          const Nodecl::List& arguments, const TL::Symbol& func_sym );

    public:
        // *** Constructors *** //
        UsageVisitor( Node* fake_node );
        
        UsageVisitor( Node* n, ExtensibleGraph* pcfg, ObjectList<ExtensibleGraph*>* pcfgs,
                      std::map<Symbol, Utils::UsageKind>* global_vars,
                      std::map<Symbol, Utils::UsageKind>* reference_params );
        
        // *** Modifiers *** //
        void compute_statement_usage( Nodecl::NodeclBase st );

        // *** Visitors *** //
        Ret unhandled_node( const Nodecl::NodeclBase& n );
        Ret visit( const Nodecl::AddAssignment& n );
        Ret visit( const Nodecl::ArithmeticShrAssignment& n );
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::Assignment& n );
        Ret visit( const Nodecl::BitwiseAndAssignment& n );
        Ret visit( const Nodecl::BitwiseOrAssignment& n );
        Ret visit( const Nodecl::BitwiseShlAssignment& n );
        Ret visit( const Nodecl::BitwiseShrAssignment& n );
        Ret visit( const Nodecl::BitwiseXorAssignment& n );
        Ret visit( const Nodecl::ClassMemberAccess& n );
        Ret visit( const Nodecl::Dereference& n );
        Ret visit( const Nodecl::DivAssignment& n );
        Ret visit( const Nodecl::FunctionCall& n );
        Ret visit( const Nodecl::MinusAssignment& n );
        Ret visit( const Nodecl::ModAssignment& n );
        Ret visit( const Nodecl::MulAssignment& n );
        Ret visit( const Nodecl::ObjectInit& n );
        Ret visit( const Nodecl::Postdecrement& n );
        Ret visit( const Nodecl::Postincrement& n );
        Ret visit( const Nodecl::Predecrement& n );
        Ret visit( const Nodecl::Preincrement& n );
        Ret visit( const Nodecl::Range& n );
        Ret visit( const Nodecl::Reference& n );
        Ret visit( const Nodecl::Symbol& n );
        Ret visit( const Nodecl::UnalignedVectorStore& n );
        Ret visit( const Nodecl::VectorAssignment& n );
        Ret visit( const Nodecl::VectorGather& n );
        Ret visit( const Nodecl::VectorMaskAssignment& n );
        Ret visit( const Nodecl::VectorScatter& n );
        Ret visit( const Nodecl::VectorStore& n );
        Ret visit( const Nodecl::VirtualFunctionCall& n );
    };

    
    class LIBTL_CLASS ReferenceUsageVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        Nodecl::NodeclBase _current_nodecl;
        bool _store_symbol;
        Utils::ext_sym_set _used_ext_syms;
        
    public:
        // Constructor
        ReferenceUsageVisitor( );
        
        // Consultants
        Utils::ext_sym_set get_ue_vars( );
        
        // Visitors
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::ClassMemberAccess& n );
        Ret visit( const Nodecl::Reference& n );
        Ret visit( const Nodecl::Symbol& n );
    };
    
    // *************************** End class implementing use-definition visitor ************************** //
    // **************************************************************************************************** //
}
}

#endif      // TL_USE_DEF_HPP
