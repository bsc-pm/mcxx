/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#ifndef CODEGEN_CXX_HPP
#define CODEGEN_CXX_HPP

#include "codegen-phase.hpp"
#include "tl-scope.hpp"
#include "tl-symbol.hpp"
#include <sstream>
#include <map>
#include <set>

namespace Codegen
{
    class CxxBase : public CodegenPhase
    {
        protected:
            virtual void codegen(const Nodecl::NodeclBase&, std::ostream* out);

        public:
            CxxBase();

            virtual void push_scope(TL::Scope sc);
            virtual void pop_scope();

            void codegen_cleanup();

            void handle_parameter(int n, void* data);

            using CodegenPhase::visit;
            Ret visit(const Nodecl::Add &);
            Ret visit(const Nodecl::AddAssignment &);
            Ret visit(const Nodecl::Alignof &);
            Ret visit(const Nodecl::Analysis::PlusInfinity &);
            Ret visit(const Nodecl::Analysis::Maximum &);
            Ret visit(const Nodecl::Analysis::Minimum &);
            Ret visit(const Nodecl::Analysis::MinusInfinity &);
            Ret visit(const Nodecl::Analysis::EmptyRange& node);
            Ret visit(const Nodecl::Analysis::Phi &);
            Ret visit(const Nodecl::Analysis::RangeIntersection &);
            Ret visit(const Nodecl::Analysis::RangeSub &);
            Ret visit(const Nodecl::Analysis::RangeUnion &);
            Ret visit(const Nodecl::ArraySubscript &);
            Ret visit(const Nodecl::Assignment &);
            Ret visit(const Nodecl::BitwiseAnd &);
            Ret visit(const Nodecl::BitwiseAndAssignment &);
            Ret visit(const Nodecl::BitwiseNot &);
            Ret visit(const Nodecl::BitwiseOr &);
            Ret visit(const Nodecl::BitwiseOrAssignment &);
            Ret visit(const Nodecl::BitwiseShl &);
            Ret visit(const Nodecl::BitwiseShlAssignment &);
            Ret visit(const Nodecl::BitwiseXor &);
            Ret visit(const Nodecl::BitwiseXorAssignment &);
            Ret visit(const Nodecl::BooleanLiteral &);
            Ret visit(const Nodecl::BreakStatement &);
            Ret visit(const Nodecl::C99DesignatedInitializer &);
            Ret visit(const Nodecl::C99FieldDesignator &);
            Ret visit(const Nodecl::C99IndexDesignator &);
            Ret visit(const Nodecl::CaseStatement &);
            Ret visit(const Nodecl::CatchHandler &);
            Ret visit(const Nodecl::ClassMemberAccess &);
            Ret visit(const Nodecl::Comma &);
            Ret visit(const Nodecl::ComplexLiteral &);
            Ret visit(const Nodecl::CompoundExpression &);
            Ret visit(const Nodecl::CompoundStatement &);
            Ret visit(const Nodecl::ConditionalExpression &);
            Ret visit(const Nodecl::Context &);
            Ret visit(const Nodecl::ContinueStatement &);
            Ret visit(const Nodecl::Conversion &);
            Ret visit(const Nodecl::CxxCast &);
            Ret visit(const Nodecl::CxxClassMemberAccess &);
            Ret visit(const Nodecl::CxxArrow &);
            Ret visit(const Nodecl::CxxArrowPtrMember& node);
            Ret visit(const Nodecl::CxxBracedInitializer &);
            Ret visit(const Nodecl::CxxDepDecltype &);
            Ret visit(const Nodecl::CxxDepGlobalNameNested &);
            Ret visit(const Nodecl::CxxDepNameConversion &);
            Ret visit(const Nodecl::CxxDepNameNested &);
            Ret visit(const Nodecl::CxxDepNameSimple &);
            Ret visit(const Nodecl::CxxDepNew &);
            Ret visit(const Nodecl::CxxDepTemplateId &);
            Ret visit(const Nodecl::CxxDotPtrMember& node);
            Ret visit(const Nodecl::CxxNoexcept &);
            Ret visit(const Nodecl::CxxInitializer &);
            Ret visit(const Nodecl::CxxEqualInitializer &);
            Ret visit(const Nodecl::CxxMemberInit &);
            Ret visit(const Nodecl::CxxExplicitTypeCast &);
            Ret visit(const Nodecl::CxxParenthesizedInitializer &);
            Ret visit(const Nodecl::CxxSizeof &);
            Ret visit(const Nodecl::CxxSizeofPack &);
            Ret visit(const Nodecl::CxxAlignof &);
            Ret visit(const Nodecl::DefaultArgument &);
            Ret visit(const Nodecl::DefaultStatement &);
            Ret visit(const Nodecl::Delete &);
            Ret visit(const Nodecl::DeleteArray &);
            Ret visit(const Nodecl::Dereference &);
            Ret visit(const Nodecl::Different &);
            Ret visit(const Nodecl::Div &);
            Ret visit(const Nodecl::DivAssignment &);
            Ret visit(const Nodecl::DoStatement &);
            Ret visit(const Nodecl::EmptyStatement &);
            Ret visit(const Nodecl::Equal &);
            Ret visit(const Nodecl::ErrExpr &);
            Ret visit(const Nodecl::ErrStatement &);
            Ret visit(const Nodecl::ExpressionStatement &);
            Ret visit(const Nodecl::FieldDesignator &);
            Ret visit(const Nodecl::FloatingLiteral &);
            Ret visit(const Nodecl::ForStatement &);
            Ret visit(const Nodecl::FunctionCall &);
            Ret visit(const Nodecl::FunctionCode &);
            Ret visit(const Nodecl::GotoStatement &);
            Ret visit(const Nodecl::GreaterOrEqualThan &);
            Ret visit(const Nodecl::GreaterThan &);
            Ret visit(const Nodecl::IfElseStatement &);
            Ret visit(const Nodecl::ImagPart &);
            Ret visit(const Nodecl::IndexDesignator &);
            Ret visit(const Nodecl::CxxLambda &);
            Ret visit(const Nodecl::IntegerLiteral &);
            Ret visit(const Nodecl::LabeledStatement &);
            Ret visit(const Nodecl::LogicalAnd &);
            Ret visit(const Nodecl::LogicalNot &);
            Ret visit(const Nodecl::LogicalOr &);
            Ret visit(const Nodecl::LoopControl &);
            Ret visit(const Nodecl::IteratorLoopControl &);
            Ret visit(const Nodecl::LowerOrEqualThan &);
            Ret visit(const Nodecl::LowerThan &);
            Ret visit(const Nodecl::MemberInit &);
            Ret visit(const Nodecl::Minus &);
            Ret visit(const Nodecl::MinusAssignment &);
            Ret visit(const Nodecl::Mod &);
            Ret visit(const Nodecl::ModAssignment &);
            Ret visit(const Nodecl::Mul &);
            Ret visit(const Nodecl::MulAssignment &);
            Ret visit(const Nodecl::Neg &);
            Ret visit(const Nodecl::New &);
            Ret visit(const Nodecl::ObjectInit &);
            Ret visit(const Nodecl::Offset &);
            Ret visit(const Nodecl::Offsetof &);
            Ret visit(const Nodecl::ParenthesizedExpression &);
            Ret visit(const Nodecl::Plus &);
            Ret visit(const Nodecl::PointerToMember &);
            Ret visit(const Nodecl::Postdecrement &);
            Ret visit(const Nodecl::Postincrement &);
            Ret visit(const Nodecl::PragmaClauseArg &);
            Ret visit(const Nodecl::PragmaCustomClause &);
            Ret visit(const Nodecl::PragmaCustomDeclaration &);
            Ret visit(const Nodecl::PragmaCustomDirective &);
            Ret visit(const Nodecl::PragmaCustomLine &);
            Ret visit(const Nodecl::PragmaCustomStatement &);
            Ret visit(const Nodecl::Predecrement &);
            Ret visit(const Nodecl::Preincrement &);
            Ret visit(const Nodecl::PseudoDestructorName &);
            Ret visit(const Nodecl::Range &);
            Ret visit(const Nodecl::RealPart &);
            Ret visit(const Nodecl::Reference &);
            Ret visit(const Nodecl::ReturnStatement &);
            Ret visit(const Nodecl::Shaping &);
            Ret visit(const Nodecl::BitwiseShr &);
            Ret visit(const Nodecl::ArithmeticShr &);
            Ret visit(const Nodecl::BitwiseShrAssignment &);
            Ret visit(const Nodecl::ArithmeticShrAssignment &);
            Ret visit(const Nodecl::Sizeof &);
            Ret visit(const Nodecl::StringLiteral &);
            Ret visit(const Nodecl::StructuredValue &);
            Ret visit(const Nodecl::SwitchStatement &);
            Ret visit(const Nodecl::Symbol &);
            Ret visit(const Nodecl::TemplateFunctionCode &);
            Ret visit(const Nodecl::Text &);
            Ret visit(const Nodecl::Throw &);
            Ret visit(const Nodecl::TopLevel &);
            Ret visit(const Nodecl::TryBlock &);
            Ret visit(const Nodecl::Type &);
            Ret visit(const Nodecl::Typeid &);
            Ret visit(const Nodecl::Unknown &);
            Ret visit(const Nodecl::VirtualFunctionCall &);

#if 0
            Ret visit(const Nodecl::VectorAdd &);
            Ret visit(const Nodecl::VectorAlignRight &);
            Ret visit(const Nodecl::VectorArithmeticShr &);
            Ret visit(const Nodecl::VectorAssignment &);
            Ret visit(const Nodecl::VectorBitwiseAnd &);
            Ret visit(const Nodecl::VectorBitwiseOr &);
            Ret visit(const Nodecl::VectorBitwiseShl &);
            Ret visit(const Nodecl::VectorBitwiseShr &);
            Ret visit(const Nodecl::VectorBitwiseXor &);
            Ret visit(const Nodecl::VectorConversion &);
            Ret visit(const Nodecl::VectorDifferent &);
            Ret visit(const Nodecl::VectorDiv &);
            Ret visit(const Nodecl::VectorEqual &);
            Ret visit(const Nodecl::VectorGreaterOrEqualThan &);
            Ret visit(const Nodecl::VectorGreaterThan &);
            Ret visit(const Nodecl::VectorLaneId &);
            Ret visit(const Nodecl::VectorLiteral &);
            Ret visit(const Nodecl::VectorLoad &);
            Ret visit(const Nodecl::VectorLogicalAnd &);
            Ret visit(const Nodecl::VectorLogicalOr &);
            Ret visit(const Nodecl::VectorLowerOrEqualThan &);
            Ret visit(const Nodecl::VectorLowerThan &);
            Ret visit(const Nodecl::VectorMaskAssignment &);
            Ret visit(const Nodecl::VectorMinus &);
            Ret visit(const Nodecl::VectorMod &);
            Ret visit(const Nodecl::VectorMul &);
            Ret visit(const Nodecl::VectorPromotion &);
#endif
            Ret visit(const Nodecl::VectorSubscript &);

            Ret visit(const Nodecl::WhileStatement &);

            Ret visit(const Nodecl::AsmDefinition& node);

            Ret visit(const Nodecl::CxxDecl& node);
            Ret visit(const Nodecl::CxxDef& node);
            Ret visit(const Nodecl::CxxExplicitInstantiationDef& node);
            Ret visit(const Nodecl::CxxExplicitInstantiationDecl& node);
            Ret visit(const Nodecl::CxxImplicitInstantiation& node);
            Ret visit(const Nodecl::CxxUsingNamespace& node);
            Ret visit(const Nodecl::CxxUsingDecl& node);
            Ret visit(const Nodecl::CxxDepFunctionCall &);

            Ret visit(const Nodecl::CxxValuePack &);

            Ret visit(const Nodecl::CxxForRanged& node);

            Ret visit(const Nodecl::CxxStaticAssert& node);

            Ret visit(const Nodecl::Verbatim& node);
            Ret visit(const Nodecl::VlaWildcard &);

            Ret visit(const Nodecl::UnknownPragma& node);
            Ret visit(const Nodecl::GxxTrait& node);
            Ret visit(const Nodecl::GccAsmDefinition& node);
            Ret visit(const Nodecl::GccAsmOperand& node);
            Ret visit(const Nodecl::GccAsmSpec& node);
            Ret visit(const Nodecl::GccBuiltinVaArg& node);
            Ret visit(const Nodecl::UpcSyncStatement& node);
            Ret visit(const Nodecl::SourceComment& node);
            Ret visit(const Nodecl::PreprocessorLine& node);

            Ret visit(const Nodecl::IntelAssume& node);
            Ret visit(const Nodecl::IntelAssumeAligned& node);

            template <typename Node>
                CxxBase::Ret visit_function_call(const Node&, bool is_virtual_call);

            void set_emit_always_extern_linkage(bool emit);

        private:

            // State
            struct State
            {
                TL::Symbol current_symbol;
                TL::Symbol global_namespace;
                TL::Symbol opened_namespace;

                enum EmitDeclarations
                {
                    EMIT_ALL_DECLARATIONS,
                    EMIT_CURRENT_SCOPE_DECLARATIONS,
                    EMIT_NO_DECLARATIONS,
                };

                EmitDeclarations emit_declarations;

                bool in_condition;
                Nodecl::NodeclBase condition_top;

                bool in_for_stmt_decl;

                bool in_member_declaration;

                bool in_forwarded_member_declaration;

                bool in_dependent_template_function_code;

                bool nontype_template_argument_needs_parentheses;

                bool inside_structured_value;

                // States whether we are visiting the called entity of a function call
                bool visiting_called_entity_of_function_call;

                TL::ObjectList<TL::Symbol> classes_being_defined;

                // This one is to be used only in define_required_before_class
                std::set<TL::Symbol> being_checked_for_required;

                // Used in define_required_before_class and define_symbol_if_nonnested
                std::set<TL::Symbol> pending_nested_types_to_define;

                // Used in define_generic_entities
                std::set<TL::Symbol> walked_symbols;

                // Object init
                std::set<TL::Symbol> must_be_object_init;

                // friends that have been declared but not yet defined
                std::set<TL::Symbol> friend_function_declared_but_not_defined;

                // This means that we are doing &X and X is a rebindable reference
                bool do_not_derref_rebindable_reference;

                // Not to be used directly. Use start_inline_comment and end_inline_comment
                int _inline_comment_nest;

                // Not meant to be used directly, use functions
                // get_indent_level, set_indent_level
                // inc_indent, dec_indent
                int _indent_level;

                // Emit '(*this).x.foo' -> as 'x.foo'
                // This is needed to workaround some issues with g++ and
                // its C++11 support
                bool _do_not_emit_this;

                // Saved locus when emitting location
                const locus_t* _saved_locus;

                State() :
                    global_namespace(),
                    opened_namespace(),
                    emit_declarations(EMIT_NO_DECLARATIONS),
                    in_condition(false),
                    condition_top(Nodecl::NodeclBase::null()),
                    in_for_stmt_decl(false),
                    in_member_declaration(false),
                    in_forwarded_member_declaration(false),
                    in_dependent_template_function_code(false),
                    nontype_template_argument_needs_parentheses(false),
                    inside_structured_value(false),
                    visiting_called_entity_of_function_call(false),
                    classes_being_defined(),
                    being_checked_for_required(),
                    pending_nested_types_to_define(),
                    walked_symbols(),
                    must_be_object_init(),
                    friend_function_declared_but_not_defined(),
                    do_not_derref_rebindable_reference(false),
                    _inline_comment_nest(0),
                    _indent_level(0),
                   _do_not_emit_this(false),
                   _saved_locus(NULL) { }
            } state;
            // End of State

            void codegen(const Nodecl::NodeclBase &n, const State &new_state, std::ostream* out);

            std::vector<TL::Scope> _scope_stack;

            bool symbol_is_same_or_nested_in(TL::Symbol symbol, TL::Symbol class_sym);
            bool symbol_is_nested_in_defined_classes(TL::Symbol symbol);
            bool symbol_or_its_bases_are_nested_in_defined_classes(TL::Symbol symbol);

            TL::ObjectList<TL::Symbol> define_required_before_class(TL::Symbol symbol,
                    void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
                    void (CxxBase::*def_sym_fun)(TL::Symbol symbol));

            void define_class_symbol_aux(TL::Symbol symbol,
                    TL::ObjectList<TL::Symbol> symbols_defined_inside_class,
                    int level,
                    TL::Scope* scope = NULL);

            void define_class_symbol_using_member_declarations_aux(TL::Symbol symbol,
                    TL::ObjectList<TL::Symbol> symbols_defined_inside_class,
                    int level,
                    TL::Scope* scope);

            void define_class_symbol(TL::Symbol symbol,
                    void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
                    void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
                    TL::Scope* scope = NULL);

            void declare_nondependent_friend_class(TL::Symbol friend_symbol,
                    TL::Symbol class_symbol);
            void declare_dependent_friend_class(TL::Symbol friend_symbol,
                    TL::Symbol class_symbol);

            void declare_nondependent_friend_function(TL::Symbol friend_symbol, TL::Symbol class_symbol);
            void declare_dependent_friend_function(TL::Symbol friend_symbol, TL::Symbol class_symbol);

            void declare_friend_symbol(TL::Symbol friend_symbol,
                    TL::Symbol class_symbol);

            void define_or_declare_variable(TL::Symbol,
                    bool is_definition);
            std::string define_or_declare_variable_get_name_variable(TL::Symbol& symbol);
            void define_or_declare_variable_emit_initializer(TL::Symbol& symbol, bool is_definition);

            void define_or_declare_variables(TL::ObjectList<TL::Symbol>& symbols, bool is_definition);

            void define_generic_entities(Nodecl::NodeclBase node,
                    void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
                    void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
                    void (CxxBase::*define_entities_fun)(const Nodecl::NodeclBase& node),
                    void (CxxBase::*define_entry_fun)(
                        const Nodecl::NodeclBase& node, TL::Symbol entry,
                        void (CxxBase::*def_sym_fun_2)(TL::Symbol symbol)),
                    std::set<TL::Symbol>& visited_symbols
                    );

            bool is_local_symbol(TL::Symbol entry);
            // Note: is_nonlocal_symbol is NOT EQUIVALENT to !is_local_symbol
            bool is_nonlocal_symbol(TL::Symbol entry);
            bool is_prototype_symbol(TL::Symbol entry);
            bool all_enclosing_classes_are_user_declared(TL::Symbol entry);

            void define_all_entities_in_trees(const Nodecl::NodeclBase&);
            void define_nonlocal_entities_in_trees(const Nodecl::NodeclBase&);
            void define_nonlocal_nonprototype_entities_in_trees(const Nodecl::NodeclBase& node);
            void define_nonprototype_entities_in_trees(const Nodecl::NodeclBase& node);
            void define_nonnested_entities_in_trees(const Nodecl::NodeclBase&);
            void define_local_entities_in_trees(const Nodecl::NodeclBase&);

            void declare_symbol_always(TL::Symbol);
            void define_symbol_always(TL::Symbol);

            void declare_symbol_if_nonlocal(TL::Symbol);
            void define_symbol_if_nonlocal(TL::Symbol);

            void declare_symbol_if_nonlocal_nonprototype(TL::Symbol);
            void define_symbol_if_nonlocal_nonprototype(TL::Symbol);

            void declare_symbol_if_nonprototype(TL::Symbol);
            void define_symbol_if_nonprototype(TL::Symbol);

            void declare_symbol_if_local(TL::Symbol);
            void define_symbol_if_local(TL::Symbol);

            void declare_symbol_if_nonnested(TL::Symbol);
            void define_symbol_if_nonnested(TL::Symbol);

            void define_or_declare_if_complete(TL::Symbol sym,
                    void (CxxBase::* symbol_to_declare)(TL::Symbol),
                    void (CxxBase::* symbol_to_define)(TL::Symbol));

            void walk_type_for_symbols(TL::Type,
                    void (CxxBase::* declare_fun)(TL::Symbol),
                    void (CxxBase::* define_fun)(TL::Symbol),
                    void (CxxBase::* define_entities)(const Nodecl::NodeclBase&),
                    bool needs_definition = true);

            void walk_vector_type(TL::Type t,
                    void (CxxBase::* symbol_to_declare)(TL::Symbol),
                    void (CxxBase::* symbol_to_define)(TL::Symbol),
                    void (CxxBase::* define_entities_in_tree)(const Nodecl::NodeclBase&),
                    bool needs_definition);

            void walk_mask_type(TL::Type t,
                    void (CxxBase::* symbol_to_declare)(TL::Symbol),
                    void (CxxBase::* symbol_to_define)(TL::Symbol),
                    void (CxxBase::* define_entities_in_tree)(const Nodecl::NodeclBase&),
                    bool needs_definition);

            void entry_just_define(
                    const Nodecl::NodeclBase&,
                    TL::Symbol symbol,
                    void (CxxBase::*def_sym_fun)(TL::Symbol));

            void entry_local_definition(
                    const Nodecl::NodeclBase&,
                    TL::Symbol entry,
                    void (CxxBase::*def_sym_fun)(TL::Symbol));

            std::map<TL::Symbol, codegen_status_t> _codegen_status;

            void codegen_fill_namespace_list_rec(
                    scope_entry_t* namespace_sym,
                    scope_entry_t** list,
                    int* position);
            void codegen_move_namespace_from_to(TL::Symbol from, TL::Symbol to);

            void move_to_namespace_of_symbol(TL::Symbol symbol);
            void move_to_namespace(TL::Symbol namespace_sym);

            void indent();
            void inc_indent(int n = 1);
            void dec_indent(int n = 1);

            int get_indent_level();
            void set_indent_level(int);

            void walk_expression_list(const Nodecl::List&);

            template <typename Iterator>
                void walk_expression_unpacked_list(Iterator begin, Iterator end);

            template <typename Iterator>
                void codegen_function_call_arguments(Iterator begin, Iterator end, TL::Type function_type, int ignore_n_first);

            template <typename Node>
                void visit_function_call_form_template_id(const Node&);

            template <typename Node>
                static bool is_implicit_function_call(const Node& node);

            static Nodecl::NodeclBase advance_implicit_function_calls(Nodecl::NodeclBase node);

            template <typename Node>
                static bool is_binary_infix_operator_function_call(const Node& node);

            template <typename Node>
                static bool is_unary_prefix_operator_function_call(const Node& node);

            template <typename Node>
                static bool is_unary_postfix_operator_function_call(const Node& node);

            template <typename Node>
                static bool is_operator_function_call(const Node& node);

//            template <typename Node>
//                CxxBase::Ret visit_function_call(const Node&, bool is_virtual_call);

            template < typename Node>
                static node_t get_kind_of_operator_function_call(const Node & node);

            static int get_rank_kind(node_t n, const std::string& t);
            static int get_rank(Nodecl::NodeclBase n);
            bool same_operation(Nodecl::NodeclBase current_operator, Nodecl::NodeclBase operand);
            static bool operand_has_lower_priority(Nodecl::NodeclBase operation, Nodecl::NodeclBase operand);
            static std::string quote_c_string(int* c, int length, const std::string& prefix);
            static bool nodecl_calls_to_constructor(Nodecl::NodeclBase);
            static bool nodecl_calls_to_constructor_indirectly(Nodecl::NodeclBase);
            static bool nodecl_is_parenthesized_explicit_type_conversion(Nodecl::NodeclBase);
            static Nodecl::List nodecl_calls_to_constructor_get_arguments(Nodecl::NodeclBase initializer);
            static bool nodecl_calls_to_constructor_default_init(Nodecl::NodeclBase node);
            static bool nodecl_calls_to_constructor_default_init_braced(Nodecl::NodeclBase node);

            static std::string unmangle_symbol_name(TL::Symbol);

            void codegen_template_headers_all_levels(
                    TL::TemplateParameters template_parameters,
                    bool show_default_values);

            void codegen_template_headers_bounded(
                    TL::TemplateParameters template_parameters,
                    TL::TemplateParameters lim,
                    bool show_default_values);

            void codegen_template_header(
                    TL::TemplateParameters template_parameters,
                    bool show_default_values,
                    bool endline = true);

            std::string template_arguments_to_str(TL::Symbol);

            std::string exception_specifier_to_str(TL::Symbol);

            std::string gcc_attributes_to_str(TL::Symbol);
            std::string gcc_asm_specifier_to_str(TL::Symbol);

            std::string alignas_attributes_to_str(TL::Symbol);

            std::string ms_attributes_to_str(TL::Symbol);

            virtual Ret unhandled_node(const Nodecl::NodeclBase & n);

            void fill_parameter_names_and_parameter_attributes(TL::Symbol symbol,
                    TL::ObjectList<std::string>& parameter_names,
                    TL::ObjectList<std::string>& parameter_attributes,
                    bool emit_default_arguments);

            TL::Type coerce_parameter_types_of_function_type(TL::Symbol sym);

            static const char* print_name_str(scope_entry_t* s, const decl_context_t* decl_context, void *data);
            static const char* print_type_str(type_t* t, const decl_context_t* decl_context, void *data);

            std::string get_declaration(TL::Type t, TL::Scope scope, const std::string& name);
            std::string get_declaration_only_declarator(TL::Type t, TL::Scope scope, const std::string& name);
            std::string get_declaration_with_parameters(TL::Type, TL::Scope, const std::string& symbol_name,
                    TL::ObjectList<std::string>& parameter_names, TL::ObjectList<std::string> & parameter_attributes);
            TL::Type fix_references(TL::Type t);

            std::string get_qualified_name(TL::Symbol sym, bool without_template_id = false) const;
            std::string get_qualified_name(TL::Symbol sym, TL::Scope sc, bool without_template_id = false) const;

            TL::Scope get_current_scope() const;

            bool is_non_language_reference_variable(TL::Symbol sym);
            bool is_non_language_reference_variable(const Nodecl::NodeclBase& n);
            bool is_non_language_reference_type(TL::Type type);


            void codegen_explicit_instantiation(TL::Symbol sym,
                    const Nodecl::NodeclBase & declarator_name,
                    const Nodecl::NodeclBase & context,
                    bool is_extern = false);

            void emit_declarations_of_initializer(TL::Symbol symbol);

            void emit_integer_constant(const_value_t* cval, TL::Type t);
            void emit_floating_constant(const_value_t* cval, TL::Type t);

            bool is_pointer_arithmetic_add_helper(TL::Type op1, TL::Type op2);
            bool is_pointer_arithmetic_add(const Nodecl::Add &node, TL::Type &pointer_type);

            bool is_friend_of_class(TL::Symbol sym, TL::Symbol class_sym);

            std::string start_inline_comment();
            std::string end_inline_comment();

            bool is_assignment_operator(const std::string& operator_name);

            void emit_line_marker(Nodecl::NodeclBase n);
            void emit_line_marker(const locus_t* locus);

            void emit_saved_locus();

            bool looks_like_braced_list(Nodecl::NodeclBase n);

            void emit_explicit_cast(Nodecl::NodeclBase node, Nodecl::NodeclBase nest);
        protected:

            void walk_list(const Nodecl::List&,
                    const std::string& separator);
            void walk_initializer_list(const Nodecl::List&,
                    const std::string& separator);

            virtual void do_define_symbol(TL::Symbol symbol,
                    void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
                    void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
                    TL::Scope* scope = NULL);

            virtual void do_declare_symbol(TL::Symbol symbol,
                    void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
                    void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
                    TL::Scope* scope = NULL);

            void set_codegen_status(TL::Symbol sym, codegen_status_t status);

            codegen_status_t get_codegen_status(TL::Symbol sym);

            virtual bool cuda_print_special_attributes();

            virtual bool cuda_emit_always_extern_linkage();

            std::string _emit_saved_variables_as_unused_str;
            bool _emit_saved_variables_as_unused;
            void set_emit_saved_variables_as_unused(const std::string& str);

            std::string _prune_saved_variables_str;
            bool _prune_saved_variables;
            void set_prune_saved_variables(const std::string& str);
    };
}

#endif // CODEGEN_CXX_HPP
