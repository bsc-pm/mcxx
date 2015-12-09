/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona* Supercomputing Center
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
#include "tl-nodecl-replacer.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // **************************** Class implementing use-definition analysis **************************** //
    
    typedef std::map<NBase, Utils::UsageKind, Nodecl::Utils::Nodecl_structural_less> IpUsageMap;
    
    extern SizeMap _pointer_to_size_map;
    
    //! Class implementing Use-Def Analysis
    class LIBTL_CLASS UseDef
    {
    private:
        //!Graph we are analyzing the usage which
        ExtensibleGraph* _graph;

        //! The analysis must propagate information to outer nodes
        //! Necessary when checking analysis results
        bool _propagate_graph_nodes;

        //!Usage of IPA modifiable variable (reference variables, pointed values of pointer parameters and global variables)
        IpUsageMap _ipa_modif_vars;
        
        //! Path to the file with the c lib functions
        std::string _c_lib_file;

        //! Scope where the c_lib functions are registered
        Scope _c_lib_sc;

        //! Load the functions from the file with the C lib functions usage
        void load_c_lib_functions();

        //!Initialize all IPA modifiable variables' usage to NONE
        void initialize_ipa_var_usage();

        /*!Method that computes recursively the Use-Definition information from a node
         * \param current Node from which the method begins the computation
         */
        void compute_usage_rec(Node* current);

        //! Propagate the use-def of the children of a task creation to the task_creation node
        void propagate_task_usage_to_task_creation_node(Node* task_creation);

    public:
        /*! Constructor
         * \param graph Pointer to the graph where to calculate the analysis
         * \param pcfgs List of pcfgs in the current translation unit (necessary for IPA)
         */
        UseDef(ExtensibleGraph* graph,
               bool propagate_graph_nodes,
               const ObjectList<ExtensibleGraph*>& pcfgs);

        //! Method computing the Use-Definition information on the member #graph
        void compute_usage();
    };

    // ************************** End class implementing use-definition analysis ************************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ********************** Class implementing nodecl visitor for use-def analysis ********************** //

    //! This Class implements a Visitor that computes the Use-Definition information of a concrete statement
    //! and attaches this information to the Node in a PCFG which the statements belongs to
    class LIBTL_CLASS UsageVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:

        // *********************** Private members *********************** //

        //! Pointer to the Node in a PCFG where the Nodecl is contained
        //! The results of the analysis performed during the visit will be attached to the node
        Node* _node;

        //! The analysis must propagate information to outer nodes
        //! Necessary when checking analysis results
        bool _propagate_graph_nodes;

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
        NBase _current_nodecl;

        //! List of IPA modifiable variables appeared until a given point of the analysis
        IpUsageMap* _ipa_modif_vars;

        //! Path to the file with the c lib functions
        std::string _c_lib_file;

        //! Scope where the c lib functions are registered
        Scope _c_lib_sc;

        /*! Boolean useful for split statements: we want to calculate the usage of a function call only once
         *  When a function call appears in a split statement we calculate the first time (the func_call node)
         *  but for the other nodes, we just propagate the information
         */
        bool _avoid_func_calls;
        
        //! Current pcfg being analyzed
        // We want this here because when running IPa analysis, we need to propagate the Global Variables
        // used in called functions to the current graph, otherwise,
        // we can lose global variables usage over the nested function calls
        ExtensibleGraph* _pcfg;
        
        
        // ****************** Private visiting methods ****************** //
        
        void visit_assignment(const NBase& lhs, const NBase& rhs);
        void visit_binary_assignment(const NBase& lhs, const NBase& rhs);
        void visit_function(const NBase& called_sym, const Nodecl::List& arguments);
        void visit_increment(const NBase& n);
        void visit_vector_load(const NBase& rhs, const NBase& mask);
        void visit_vector_store(const NBase& lhs, const NBase& rhs, const NBase& mask);
        
        // ********************** Private modifiers ********************** //
        
        //!Prevents copy construction.
        UsageVisitor(const UsageVisitor& v);
        
        /*!Method that computs the usage of a node taking into account the previous usage in the same node
         * For nodes that has a part of an object that is upwards exposed and a part of an object that is killed,
         * this method computes the different parts
         */
        void set_var_usage_to_node(const NBase& var, Utils::UsageKind usage_kind);
        
        //! Overloaded method to compute a nodes usage for a set of extended symbols instead of a single symbol
        void set_var_usage_to_node(const NodeclSet& var_set, Utils::UsageKind usage_kind);
        
        
        // ************************ IPA methods ************************ //
        
        // *** Known called function code use-def analysis *** //
        void propagate_called_func_pointed_values_usage_to_func_call(
                const NodeclSet& called_func_usage, 
                const SymToNodeclMap& ptr_param_to_arg_map,
                Utils::UsageKind usage_kind);
        
        void propagate_called_func_params_usage_to_func_call(
                const NodeclSet& called_func_usage,
                const SymToNodeclMap& ref_param_to_arg_map,
                Utils::UsageKind usage_kind);
        
        void propagate_global_variables_usage(
                const NodeclSet& called_func_usage,
                const NodeclSet& called_global_vars,
                const SymToNodeclMap& param_to_arg_map,
                Utils::UsageKind usage_kind);
        
        void ipa_propagate_known_function_usage(
                ExtensibleGraph* called_pcfg, 
                const Nodecl::List& args);
        
        
        // *** Unknown called function code use-def analysis *** //
        bool check_c_lib_functions(Symbol func_sym, const Nodecl::List& args);

        bool check_function_gcc_attributes(Symbol func_sym, const Nodecl::List& args);
        
        void ipa_propagate_unreachable_function_usage(Symbol func_sym, 
                                                      const ObjectList<Symbol>& params, 
                                                      const Nodecl::List& args, 
                                                      const SizeMap& ptr_to_size_map);
        
        
        // *** Recursive calls use-def analysis *** //
        void set_ipa_variable_as_defined(const NBase& var);
        void set_ipa_variable_as_upwards_exposed(const NBase& var);
        void store_ipa_information(const NBase& n);        
        void ipa_propagate_recursive_call_usage(const ObjectList<Symbol>& params, const Nodecl::List& args);
        
        
        // *** Call to a pointer to function *** //
        void ipa_propagate_pointer_to_function_usage(const Nodecl::List& args);

    public:
        // *** Constructor *** //
        UsageVisitor(Node* n,
                bool propagate_graph_nodes,
                ExtensibleGraph* pcfg,
                IpUsageMap* ipa_modifiable_vars,
                std::string c_lib_file,
                Scope c_lib_sc);
        
        // *** Modifiers *** //
        void compute_statement_usage(NBase st);

        // *** Visitors *** //
        Ret unhandled_node(const NBase& n);
        Ret visit(const Nodecl::AddAssignment& n);
        Ret visit(const Nodecl::ArithmeticShrAssignment& n);
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::Assignment& n);
        Ret visit(const Nodecl::BitwiseAndAssignment& n);
        Ret visit(const Nodecl::BitwiseOrAssignment& n);
        Ret visit(const Nodecl::BitwiseShlAssignment& n);
        Ret visit(const Nodecl::BitwiseShrAssignment& n);
        Ret visit(const Nodecl::BitwiseXorAssignment& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
        Ret visit(const Nodecl::Dereference& n);
        Ret visit(const Nodecl::DivAssignment& n);
        Ret visit(const Nodecl::FunctionCall& n);
        Ret visit(const Nodecl::IntelAssume& n);
        Ret visit(const Nodecl::IntelAssumeAligned& n);
        Ret visit(const Nodecl::MinusAssignment& n);
        Ret visit(const Nodecl::ModAssignment& n);
        Ret visit(const Nodecl::MulAssignment& n);
        Ret visit(const Nodecl::ObjectInit& n);
        Ret visit(const Nodecl::Postdecrement& n);
        Ret visit(const Nodecl::Postincrement& n);
        Ret visit(const Nodecl::Predecrement& n);
        Ret visit(const Nodecl::Preincrement& n);
        Ret visit(const Nodecl::Range& n);
        Ret visit(const Nodecl::Reference& n);
        Ret visit(const Nodecl::Symbol& n);
        Ret visit(const Nodecl::VectorAssignment& n);
        Ret visit(const Nodecl::VectorGather& n);
        Ret visit(const Nodecl::VectorLoad& n);
        Ret visit(const Nodecl::VectorMaskAssignment& n);
        Ret visit(const Nodecl::VectorScatter& n);
        Ret visit(const Nodecl::VectorSincos& n);
        Ret visit(const Nodecl::VectorStore& n);
        Ret visit(const Nodecl::VirtualFunctionCall& n);
    };
    
    // ******************** END Class implementing nodecl visitor for use-def analysis ******************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ******************************** Utils methods for use-def analysis ******************************** //

    NBase simplify_pointer(const NBase& original_variables);
    Nodecl::List simplify_pointers(const Nodecl::List& original_variables);
    Nodecl::List simplify_arguments(const Nodecl::List& original_args);

    NBase split_var_depending_on_usage(NBase container, NBase contained);

    bool any_parameter_is_pointer(const ObjectList<Symbol>& params);
    bool any_parameter_is_reference(const ObjectList<Symbol>& params);

    SymToNodeclMap get_parameters_to_arguments_map(
        const ObjectList<Symbol>& params, 
        const Nodecl::List& args);

    void propagate_usage_to_ancestor(
        Node* ancestor,
        NodeclSet& ue_vars, NodeclSet& killed_vars,
        NodeclSet& undef_vars, NodeclSet& used_addresses,
        const NodeclSet& ue_children, const NodeclSet& killed_children,
        const NodeclSet& undef_children, const NodeclSet& used_addresses_children);

    //!Propagate the Use-Def information from inner nodes to outer nodes
    // This method may be used from UseDef and from UsageVisitor classes
    // (depending on whether graph information propagation is activated or not)
    void set_graph_node_use_def(Node* graph_node);

    // ****************************** END Utils methods for use-def analysis ****************************** //    
    // **************************************************************************************************** //
    
}
}

#endif      // TL_USE_DEF_HPP
