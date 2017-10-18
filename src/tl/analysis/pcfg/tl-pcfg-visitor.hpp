/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion

  This file is part of Mercurium C/C++ source-to-source compiler.

  See AUTHORS file in the top level directory for information
  regarding developers and contributors.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option ) any later version.

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

#ifndef TL_PCFG_VISITOR_HPP
#define TL_PCFG_VISITOR_HPP

#include <stack>

#include "tl-extensible-graph.hpp"
#include "tl-node.hpp"
#include "tl-pcfg-utils.hpp"


namespace TL {
namespace Analysis {

    class LIBTL_CLASS PCFGVisitor : public Nodecl::NodeclVisitor<TL::ObjectList<Node*> >
    {
    private:
        PCFGVisitUtils* _utils;     /*!< Class storing temporary values for the construction of the graph */

        ExtensibleGraph* _pcfg;     /*!< Actual PCFG being built during the visit */

        std::map<Symbol, Nodecl::NodeclBase> _asserted_funcs;  /*!< Map relating function symbols with 
                                                                    its related pragma analysis_check assert directive, if exists */

        //! This method creates a list with the nodes in an specific subgraph
        /*!
        * \param node First node to be traversed. The method will visit all nodes from here.
        */
        void compute_catch_parents(Node* node);

        //! This method finds the parent nodes in a sequence of connected nodes
        /*!
         * The method fails when the sub-graph has more than one entry node.
         * Since this method is used specifically to collapse the nodes created while building the node of an expression
         * we expect to find only one entry node.
         * (We don't refer a node of BASIC_ENTRY_NODE type, but the first node in the sub-graph)
         * \param actual_node Node we are computing in this moment
         * \return The entry node of a sub-graph
            */
        ObjectList<Node*> get_first_nodes(Node* actual_node);

        //! This method merges a list of nodes containing an Expression into one
        /*!
        * The way the method merges the nodes depends on the kind of the nodes to be merged:
        * The nodes that are not a GRAPH NODE are deleted. The rest remain there to be the parents of the new node.
        * \param n Nodecl containing a Expression which will be wrapped in the new node
        * \param nodes_l List of nodes containing the different parts of an expression
        * \return The new node created
        */
        Node* merge_nodes(Nodecl::NodeclBase n, ObjectList<Node*> nodes_l);

        //! This is a wrapper method of #merge_nodes for the case having only one or two nodes to be merged
        /*!
        * \param n Nodecl containing a Expression which will be wrapped in the new node
        * \param first Pointer to the node containing one part of the new node
        * \param second Pointer to the node containing other part of the new node
        */
        Node* merge_nodes(Nodecl::NodeclBase n, Node* first, Node* second);

        bool same_parent_task(Node* task_1, Node* task_2);

        // ************************************************************************************** //
        // ********************************** Visiting methods ********************************** //

        //! This method implements teh visitor for any kind of barrier: BarrierAtEnd, BarrierFull
        Ret visit_barrier(const Nodecl::NodeclBase& n);

        //! This method implements the visitor for binary nodecls
        /*!
         * The nodes wrapped in this visitor method are:
         *   Add, AddAssignment, ArithmeticShr, ArithmeticShrAssignment, Assignment,
         *   BitwiseAnd, BitwiseAndAssignment, BitwiseOr, BitwiseOrAssignment,
         *   BitwiseShl, BitwiseShlAssignment, BitwiseShr, BitwiseShrAssignment,
         *   BitwiseXor, BitwiseXorAssignment, ClassMemberAccess, Concat, Different,
         *   Div, DivAssignment, Equal, GreaterOrEqualThan, GreaterThan, LogicalAnd,
         *   LogicalOr, LowerOrEqualThan, LowerThan, MinusAssignment, Mod, ModAssignment,
         *   Mul, MulAssignment, Offset, Offsetof, Power
         * \param lhs Nodecl to be visited
         * \param lhs Left-hand side of the nodecl
         * \param rhs Right-hand side of the nodecl
         */
        Ret visit_binary_node(const Nodecl::NodeclBase& n,
                               const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs);

        //! This method implements the visitor for a CaseStatement and for DefaultStatement
        /*!
         * \param n Nodecl containing the Case or the Default Statement
         * \return The graph node created while the Statement has been parsed
         */
        Ret visit_case_or_default(const Nodecl::NodeclBase& case_stmt, const Nodecl::List& case_val);

        //! This method implements the visitor for a ConditionalExpression and a VectorConditionalExpression
        /*!
         * \param n Nodecl containing the VirtualFunctionCall or the FunctionCall
         * \return The graph node created while the expression has been parsed
         */
        template <typename T>
        ObjectList<Node*> visit_conditional_expression(const T& n);
        
        //! This method implements the visitor for a VirtualFunctionCall and a FunctionCall
        /*!
         * \param n Nodecl containing the VirtualFunctionCall or the FunctionCall
         * \return The graph node created while the function call has been parsed
         */
        template <typename T>
        Ret visit_function_call(const T& n);

        //! This method implements the visitor for nodecls generating a unique node containing itself
        /*!
         * The nodes wrapped in this visitor method are:
         *   BooleanLiteral, ComplexLiteral, EmptyStatement, FloatingLiteral,
         *   IntegerLiteral, StringLiteral, MaskLiteral, Symbol, Type
         * \param n The nodecl
         */
        Ret visit_literal_node(const Nodecl::NodeclBase& n);

        //! This method implements the visitor for a taskwait without dependences
        Ret visit_taskwait(const Nodecl::NodeclBase& n);

        //! This method implements the visitor for taskwait on dependences
        Ret visit_taskwait_on(const Nodecl::NodeclBase& n);

        //! This method implements the visitor for unary nodecls
        /*!
         * The nodes wrapped in this visitor method are:
         *   BitwiseNot, Cast, Delete, DeleteArray, Dereference, LogicalNot, Neg, New, Plus,
         *   Postdecrement, Postincrement, Predecrement, Preincrement, Reference, Sizeof,
         *   Typeid
         * \param rhs Right-hand side
         */
        Ret visit_unary_node(const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& rhs);

        Ret visit_loop_control(
                const Nodecl::NodeclBase& init,
                const Nodecl::NodeclBase& cond,
                const Nodecl::NodeclBase& next);
        
        //! This method implements the visitor for VectorFunctionCall 
        template <typename T>
        ObjectList<Node*> visit_vector_function_call(const T& n);
        
        //! Wrapper of visit_binary_node for vector nodecls
        Ret visit_vector_binary_node(const Nodecl::NodeclBase& n,
                                      const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs);
        
        //! This method implements the visitor for vector memory accesses 
        /*!
         * The nodes wrapped in this visitor method are: VectorGather
         * \param n nodecl
         * \param mem_access_type Char indicating the type of access: 
         *                        '1' => load, '2' => gather, 
         *                        '3' => store, '4' => scatter
         */
        Ret visit_vector_memory_func(const Nodecl::NodeclBase& n, char mem_access_type);
        
        //! Wrapper of visit_unary_node for vector nodecls
        Ret visit_vector_unary_node(const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& rhs);
        
        
        // ******************************** END visiting methods ******************************** //
        // ************************************************************************************** //


        bool func_has_cyclic_calls_rec(Symbol reach_func, Symbol stop_func, ExtensibleGraph * graph);


    public:

        // ************************************************************************************** //
        // ************************************ Constructors ************************************ //

        //! Constructor building an empty PCFG
        PCFGVisitor(std::string name, Nodecl::NodeclBase nodecl);

        // ********************************** END constructors ********************************** //
        // ************************************************************************************** //



        // ************************************************************************************** //
        // ******************************** Non-visiting methods ******************************** //

        /*!Generates one PCFG per each function of an AST
         * \param n AST containing the code used to generate the PCFG
         * \param asserted_funcs Map containing the relation between functions and assert pragmas
         *                       This parameter is used when calling this function from the Singleton interface
         */
        ExtensibleGraph* parallel_control_flow_graph(const Nodecl::NodeclBase& n,
                const std::map<Symbol, Nodecl::NodeclBase>& asserted_funcs = (std::map<Symbol, Nodecl::NodeclBase>()));

        void set_actual_pcfg(ExtensibleGraph* graph);

        // ****************************** END non-visiting methods ****************************** //
        // ************************************************************************************** //



        // ************************* IPA ************************* //
        //! Computes the define-use chain of a node
        void compute_use_def_chains(Node* node);

        //! Analyse loops and ranged access to variables. Recomputes use-def and reaching definitions
        //! with the info of iterated accesses
        void analyse_loops(Node* node);

        //! Once the use-def chains are calculated for every graph, we are able to recalculate the use-def of every function call
        bool propagate_use_def_ipa(Node* node);

        bool func_has_cyclic_calls(Symbol actual_func, ExtensibleGraph* graph);


        // ************************************************************************************** //
        // ********************************** Visiting methods ********************************** //

        Ret unhandled_node(const Nodecl::NodeclBase& n);
        Ret visit(const Nodecl::Add& n);
        Ret visit(const Nodecl::AddAssignment& n);
        Ret visit(const Nodecl::Alignof& n);
        Ret visit(const Nodecl::Analysis::Assert& n);
        Ret visit(const Nodecl::Analysis::AssertDecl& n);
        Ret visit(const Nodecl::Analysis::AutoScope::Firstprivate& n);
        Ret visit(const Nodecl::Analysis::AutoScope::Private& n);
        Ret visit(const Nodecl::Analysis::AutoScope::Shared& n);
        Ret visit(const Nodecl::Analysis::Correctness::AutoStorage& n);
        Ret visit(const Nodecl::Analysis::Correctness::Dead& n);
        Ret visit(const Nodecl::Analysis::Correctness::IncoherentFp& n);
        Ret visit(const Nodecl::Analysis::Correctness::IncoherentIn& n);
        Ret visit(const Nodecl::Analysis::Correctness::IncoherentInPointed& n);
        Ret visit(const Nodecl::Analysis::Correctness::IncoherentOut& n);
        Ret visit(const Nodecl::Analysis::Correctness::IncoherentOutPointed& n);
        Ret visit(const Nodecl::Analysis::Correctness::IncoherentP& n);
        Ret visit(const Nodecl::Analysis::Correctness::Race& n);
        Ret visit(const Nodecl::Analysis::Dead& n);
        Ret visit(const Nodecl::Analysis::Defined& n);
        Ret visit(const Nodecl::Analysis::InductionVariable& n);
        Ret visit(const Nodecl::Analysis::LiveIn& n);
        Ret visit(const Nodecl::Analysis::LiveOut& n);
        Ret visit(const Nodecl::Analysis::Range& n);
        Ret visit(const Nodecl::Analysis::ReachingDefinitionIn& n);
        Ret visit(const Nodecl::Analysis::ReachingDefinitionOut& n);
        Ret visit(const Nodecl::Analysis::UpperExposed& n);
        Ret visit(const Nodecl::Analysis::Undefined& n);
        Ret visit(const Nodecl::ArithmeticShr& n);
        Ret visit(const Nodecl::ArithmeticShrAssignment& n);
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::Assignment& n);
        Ret visit(const Nodecl::BitwiseAnd& n);
        Ret visit(const Nodecl::BitwiseAndAssignment& n);
        Ret visit(const Nodecl::BitwiseNot& n);
        Ret visit(const Nodecl::BitwiseOr& n);
        Ret visit(const Nodecl::BitwiseOrAssignment& n);
        Ret visit(const Nodecl::BitwiseShl& n);
        Ret visit(const Nodecl::BitwiseShlAssignment& n);
        Ret visit(const Nodecl::BitwiseShr& n);
        Ret visit(const Nodecl::BitwiseShrAssignment& n);
        Ret visit(const Nodecl::BitwiseXor& n);
        Ret visit(const Nodecl::BitwiseXorAssignment& n);
        Ret visit(const Nodecl::BooleanLiteral& n);
        Ret visit(const Nodecl::BreakStatement& n);
        Ret visit(const Nodecl::CaseStatement& n);
        Ret visit(const Nodecl::CatchHandler& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
        Ret visit(const Nodecl::Comma& n);
        Ret visit(const Nodecl::ComplexLiteral& n);
        Ret visit(const Nodecl::CompoundExpression& n);
        Ret visit(const Nodecl::CompoundStatement& n);
        Ret visit(const Nodecl::Concat& n);
        Ret visit(const Nodecl::ConditionalExpression& n);
        Ret visit(const Nodecl::Context& n);
        Ret visit(const Nodecl::ContinueStatement& n);
        Ret visit(const Nodecl::Conversion& n);
        Ret visit(const Nodecl::CxxDef& n);
        Ret visit(const Nodecl::CxxDecl& n);
        Ret visit(const Nodecl::CxxUsingNamespace& n);
        Ret visit(const Nodecl::DefaultStatement& n);
        Ret visit(const Nodecl::Delete& n);
        Ret visit(const Nodecl::DeleteArray& n);
        Ret visit(const Nodecl::Dereference& n);
        Ret visit(const Nodecl::Different& n);
        Ret visit(const Nodecl::Div& n);
        Ret visit(const Nodecl::DivAssignment& n);
        Ret visit(const Nodecl::DoStatement& n);
        Ret visit(const Nodecl::EmptyStatement& n);
        Ret visit(const Nodecl::Equal& n);
        Ret visit(const Nodecl::ExpressionStatement& n);
        Ret visit(const Nodecl::FieldDesignator& n);
        Ret visit(const Nodecl::FloatingLiteral& n);
        Ret visit(const Nodecl::ForStatement& n);
        Ret visit(const Nodecl::FunctionCall& n);
        Ret visit(const Nodecl::FunctionCode& n);
        Ret visit(const Nodecl::GccAsmDefinition& n);
        Ret visit(const Nodecl::GccAsmOperand& n);
        Ret visit(const Nodecl::GccBuiltinVaArg& n);
        Ret visit(const Nodecl::GotoStatement& n);
        Ret visit(const Nodecl::GreaterOrEqualThan& n);
        Ret visit(const Nodecl::GreaterThan& n);
        Ret visit(const Nodecl::IfElseStatement& n);
        Ret visit(const Nodecl::IndexDesignator& n);
        Ret visit(const Nodecl::IntegerLiteral& n);
        Ret visit(const Nodecl::IntelAssume& n);
        Ret visit(const Nodecl::IntelAssumeAligned& n);
        Ret visit(const Nodecl::LabeledStatement& n);
        Ret visit(const Nodecl::LogicalAnd& n);
        Ret visit(const Nodecl::LogicalNot& n);
        Ret visit(const Nodecl::LogicalOr& n);
        Ret visit(const Nodecl::LoopControl& n);
        Ret visit(const Nodecl::RangeLoopControl& n);
        Ret visit(const Nodecl::LowerOrEqualThan& n);
        Ret visit(const Nodecl::LowerThan& n);
        Ret visit(const Nodecl::MaskLiteral& n);
        Ret visit(const Nodecl::Minus& n);
        Ret visit(const Nodecl::MinusAssignment& n);
        Ret visit(const Nodecl::Mod& n);
        Ret visit(const Nodecl::ModAssignment& n);
        Ret visit(const Nodecl::Mul& n);
        Ret visit(const Nodecl::MulAssignment& n);
        Ret visit(const Nodecl::Neg& n);
        Ret visit(const Nodecl::New& n);
        Ret visit(const Nodecl::ObjectInit& n);
        Ret visit(const Nodecl::Offset& n);
        Ret visit(const Nodecl::Offsetof& n);
        Ret visit(const Nodecl::OmpSs::DepCommutative& n);
        Ret visit(const Nodecl::OmpSs::DepConcurrent& n);
        Ret visit(const Nodecl::OmpSs::CopyIn& n);
        Ret visit(const Nodecl::OmpSs::CopyInout& n);
        Ret visit(const Nodecl::OmpSs::CopyOut& n);
        Ret visit(const Nodecl::OmpSs::SharedAndAlloca& n);
        Ret visit(const Nodecl::OmpSs::Target& n);
        Ret visit(const Nodecl::OmpSs::TaskCall& n);
        Ret visit(const Nodecl::OmpSs::TaskExpression& n);
        Ret visit(const Nodecl::OmpSs::TaskLabel& n);
        Ret visit(const Nodecl::OpenMP::Aligned& n);
        Ret visit(const Nodecl::OpenMP::Atomic& n);
        Ret visit(const Nodecl::OpenMP::Auto& n);
        Ret visit(const Nodecl::OpenMP::BarrierAtEnd& n);
        Ret visit(const Nodecl::OpenMP::BarrierFull& n);
        Ret visit(const Nodecl::OpenMP::BarrierSignal& n);
        Ret visit(const Nodecl::OpenMP::BarrierWait& n);
        Ret visit(const Nodecl::OpenMP::Device& n);
        Ret visit(const Nodecl::OpenMP::Overlap& n);
        Ret visit(const Nodecl::OpenMP::CombinedWithParallel& n);
        Ret visit(const Nodecl::OpenMP::Critical& n);
        Ret visit(const Nodecl::OpenMP::CriticalName& n);
        Ret visit(const Nodecl::OpenMP::DepIn& n);
        Ret visit(const Nodecl::OpenMP::DepInout& n);
        Ret visit(const Nodecl::OpenMP::DepOut& n);
        Ret visit(const Nodecl::OpenMP::Final& n);
        Ret visit(const Nodecl::OpenMP::Firstprivate& n);
        Ret visit(const Nodecl::OpenMP::FirstLastprivate& n);
        Ret visit(const Nodecl::OpenMP::FlushAtEntry& n);
        Ret visit(const Nodecl::OpenMP::FlushAtExit& n);
        Ret visit(const Nodecl::OpenMP::FlushMemory& n);
        Ret visit(const Nodecl::OpenMP::For& n);
        Ret visit(const Nodecl::OpenMP::ForAppendix& n);
        Ret visit(const Nodecl::OpenMP::FunctionTaskParsingContext& n);
        Ret visit(const Nodecl::OpenMP::If& n);
        Ret visit(const Nodecl::OpenMP::Lastprivate& n);
        Ret visit(const Nodecl::OpenMP::Linear& n);
        Ret visit(const Nodecl::OpenMP::MapFrom& n);
        Ret visit(const Nodecl::OpenMP::MapTo& n);
        Ret visit(const Nodecl::OpenMP::MapToFrom& n);
        Ret visit(const Nodecl::OpenMP::Mask& n);
        Ret visit(const Nodecl::OpenMP::Master& n);
        Ret visit(const Nodecl::OpenMP::NoMask& n);
        Ret visit(const Nodecl::OpenMP::Nontemporal& n);
        Ret visit(const Nodecl::OpenMP::Parallel& n);
        Ret visit(const Nodecl::OpenMP::ParallelSimdFor& n);
        Ret visit(const Nodecl::OpenMP::Prefetch& n);
        Ret visit(const Nodecl::OpenMP::Priority& n);
        Ret visit(const Nodecl::OpenMP::Private& n);
        Ret visit(const Nodecl::OpenMP::PrivateInit& n);
        Ret visit(const Nodecl::OpenMP::Reduction& n);
        Ret visit(const Nodecl::OpenMP::ReductionItem& n);
        Ret visit(const Nodecl::OpenMP::Schedule& n);
        Ret visit(const Nodecl::OpenMP::Section& n);
        Ret visit(const Nodecl::OpenMP::Sections& n);
        Ret visit(const Nodecl::OpenMP::Shared& n);
        Ret visit(const Nodecl::OpenMP::Simd& n);
        Ret visit(const Nodecl::OpenMP::SimdFor& n);
        Ret visit(const Nodecl::OpenMP::SimdFunction& n);
        Ret visit(const Nodecl::OpenMP::SimdReduction& n);
        Ret visit(const Nodecl::OpenMP::Single& n);
        Ret visit(const Nodecl::OpenMP::Suitable& n);
        Ret visit(const Nodecl::OpenMP::Target& n);
        Ret visit(const Nodecl::OpenMP::TargetTaskUndeferred& n);
        Ret visit(const Nodecl::OpenMP::Task& n);
        Ret visit(const Nodecl::OpenMP::Taskwait& n);
        Ret visit(const Nodecl::OpenMP::Uniform& n);
        Ret visit(const Nodecl::OpenMP::Unroll& n);
        Ret visit(const Nodecl::OpenMP::Untied& n);
        Ret visit(const Nodecl::OpenMP::VectorLengthFor& n);
        Ret visit(const Nodecl::OpenMP::Workshare& n);
        Ret visit(const Nodecl::ParenthesizedExpression& n);
        Ret visit(const Nodecl::Plus& n);
        Ret visit(const Nodecl::PointerToMember& n);
        Ret visit(const Nodecl::Postdecrement& n);
        Ret visit(const Nodecl::Postincrement& n);
        Ret visit(const Nodecl::Power& n);
        Ret visit(const Nodecl::PragmaContext& n);
//         Ret visit(const Nodecl::PragmaCustomClause& n);
        Ret visit(const Nodecl::PragmaCustomDirective& n);
        Ret visit(const Nodecl::PragmaCustomStatement& n);
        Ret visit(const Nodecl::Predecrement& n);
        Ret visit(const Nodecl::Preincrement& n);
        Ret visit(const Nodecl::Range& n);
        Ret visit(const Nodecl::Reference& n);
        Ret visit(const Nodecl::ReturnStatement& n);
        Ret visit(const Nodecl::Sizeof& n);
        Ret visit(const Nodecl::StringLiteral& n);
        Ret visit(const Nodecl::StructuredValue& n);
        Ret visit(const Nodecl::SwitchStatement& n);
        Ret visit(const Nodecl::Symbol& n);
        Ret visit(const Nodecl::Text& n);
        Ret visit(const Nodecl::Throw& n);
//      Ret visit(const Nodecl::TopLevel& n); // This method is not implemented because PCFGVisitor must visit
                                              // sections of code creating a unique PCFG.
                                              // Use AnalysisSingleton methods instead.

        Ret visit(const Nodecl::TryBlock& n);
        Ret visit(const Nodecl::Type& n);
        Ret visit(const Nodecl::Typeid& n);
        Ret visit(const Nodecl::UnknownPragma& n);
        Ret visit(const Nodecl::VectorAdd& n);
        Ret visit(const Nodecl::VectorAlignRight& n);
        Ret visit(const Nodecl::VectorArithmeticShr& n);
        Ret visit(const Nodecl::VectorAssignment& n);
        Ret visit(const Nodecl::VectorBitwiseAnd& n);
        Ret visit(const Nodecl::VectorBitwiseNot& n);
        Ret visit(const Nodecl::VectorBitwiseOr& n);
        Ret visit(const Nodecl::VectorBitwiseShl& n);
        Ret visit(const Nodecl::VectorBitwiseShr& n);
        Ret visit(const Nodecl::VectorBitwiseXor& n);
        Ret visit(const Nodecl::VectorConditionalExpression& n);
        Ret visit(const Nodecl::VectorConversion& n);
        Ret visit(const Nodecl::VectorDifferent& n);
        Ret visit(const Nodecl::VectorDiv& n);
        Ret visit(const Nodecl::VectorEqual& n);
        Ret visit(const Nodecl::VectorFabs& n);
        Ret visit(const Nodecl::VectorFmadd& n);
        Ret visit(const Nodecl::VectorFmminus& n);
        Ret visit(const Nodecl::VectorFunctionCall& n);
        Ret visit(const Nodecl::VectorGather& n);
        Ret visit(const Nodecl::VectorGreaterOrEqualThan& n);
        Ret visit(const Nodecl::VectorGreaterThan& n);
        Ret visit(const Nodecl::VectorLiteral& n);
        Ret visit(const Nodecl::VectorLoad& n);
        Ret visit(const Nodecl::VectorLogicalAnd& n);
        Ret visit(const Nodecl::VectorLogicalNot& n);
        Ret visit(const Nodecl::VectorLogicalOr& n);
        Ret visit(const Nodecl::VectorLowerThan& n);
        Ret visit(const Nodecl::VectorLowerOrEqualThan& n);
        Ret visit(const Nodecl::VectorMaskAnd& n);
        Ret visit(const Nodecl::VectorMaskAnd1Not& n);
        Ret visit(const Nodecl::VectorMaskAnd2Not& n);
        Ret visit(const Nodecl::VectorMaskAssignment& n);
        Ret visit(const Nodecl::VectorMaskConversion& n);
        Ret visit(const Nodecl::VectorMaskNot& n);
        Ret visit(const Nodecl::VectorMaskOr& n);
        Ret visit(const Nodecl::VectorMaskXor& n);
        Ret visit(const Nodecl::VectorMinus& n);
        Ret visit(const Nodecl::VectorMod& n);
        Ret visit(const Nodecl::VectorMul& n);
        Ret visit(const Nodecl::VectorNeg& n);
        Ret visit(const Nodecl::VectorPrefetch& n);
        Ret visit(const Nodecl::VectorPromotion& n);
        Ret visit(const Nodecl::VectorRcp& n);
        Ret visit(const Nodecl::VectorReductionAdd& n);
        Ret visit(const Nodecl::VectorReductionMinus& n);
        Ret visit(const Nodecl::VectorReductionMul& n);
        Ret visit(const Nodecl::VectorRsqrt& n);
        Ret visit(const Nodecl::VectorScatter& n);
        Ret visit(const Nodecl::VectorSincos& n);
        Ret visit(const Nodecl::VectorSqrt& n);
        Ret visit(const Nodecl::VectorStore& n);
        Ret visit(const Nodecl::VirtualFunctionCall& n);
        Ret visit(const Nodecl::DefaultArgument& n);
        Ret visit(const Nodecl::FortranActualArgument& n);
        Ret visit(const Nodecl::WhileStatement& n);

        Ret visit(const Nodecl::FortranAllocateStatement& n);
        Ret visit(const Nodecl::FortranBozLiteral& n);
        Ret visit(const Nodecl::FortranDeallocateStatement& n);
        Ret visit(const Nodecl::FortranOpenStatement& n);
        Ret visit(const Nodecl::FortranCloseStatement& n);
        Ret visit(const Nodecl::FortranPrintStatement& n);
        Ret visit(const Nodecl::FortranStopStatement& n);
        Ret visit(const Nodecl::FortranIoStatement& n);
        Ret visit(const Nodecl::FortranWhere& n);
        Ret visit(const Nodecl::FortranReadStatement& n);
        Ret visit(const Nodecl::FortranWriteStatement& n);

        Ret visit(const Nodecl::CudaKernelCall& n);

        // ******************************** END visiting methods ******************************** //
        // ************************************************************************************** //
    };
}
}

#endif  // TL_PCFG_VISITOR_HPP
