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

#ifndef CODEGEN_FORTRAN_HPP
#define CODEGEN_FORTRAN_HPP

#include "codegen-phase.hpp"

#include <set>
#include <stack>

namespace Codegen
{
    class FortranBase : public CodegenPhase
    {
        protected:

            virtual void codegen(const Nodecl::NodeclBase&, std::ostream* out);
            virtual void codegen_cleanup();

        public:
            FortranBase();

            void visit(const Nodecl::TopLevel& node);
            void visit(const Nodecl::FunctionCode& node);
            void visit(const Nodecl::Context& node);
            void visit(const Nodecl::CompoundStatement& node);
            void visit(const Nodecl::ExpressionStatement& node);
            void visit(const Nodecl::ObjectInit& node);
            void visit(const Nodecl::Plus& node);
            void visit(const Nodecl::Neg& node);
            void visit(const Nodecl::LogicalNot& node);
            void visit(const Nodecl::Mul& node);
            void visit(const Nodecl::Div& node);
            void visit(const Nodecl::Mod& node);
            void visit(const Nodecl::Add& node);
            void visit(const Nodecl::Minus& node);
            void visit(const Nodecl::LowerThan& node);
            void visit(const Nodecl::LowerOrEqualThan& node);
            void visit(const Nodecl::GreaterThan& node);
            void visit(const Nodecl::GreaterOrEqualThan& node);
            void visit(const Nodecl::LogicalAnd& node);
            void visit(const Nodecl::LogicalOr& node);
            void visit(const Nodecl::Power& node);
            void visit(const Nodecl::Concat& node);
            void visit(const Nodecl::ClassMemberAccess& node);
            void visit(const Nodecl::Range& node);
            void visit(const Nodecl::StringLiteral& node);
            void visit(const Nodecl::Text& node);
            void visit(const Nodecl::StructuredValue& node);
            void visit(const Nodecl::BooleanLiteral& node);
            void visit(const Nodecl::IntegerLiteral& node);
            void visit(const Nodecl::ComplexLiteral& node);
            void visit(const Nodecl::FloatingLiteral& node);
            void visit(const Nodecl::Symbol& node);
            void visit(const Nodecl::Assignment& node);
            void visit(const Nodecl::Equal& node);
            void visit(const Nodecl::Different& node);
            void visit(const Nodecl::Dereference& node);
            void visit(const Nodecl::Reference& node);
            void visit(const Nodecl::ParenthesizedExpression& node);
            void visit(const Nodecl::ArraySubscript& node);
            void visit(const Nodecl::FunctionCall& node);
            void visit(const Nodecl::FortranActualArgument& node);
            void visit(const Nodecl::EmptyStatement& node);
            void visit(const Nodecl::IfElseStatement& node);
            void visit(const Nodecl::ReturnStatement& node);
            void visit(const Nodecl::LabeledStatement& node);
            void visit(const Nodecl::GotoStatement& node);
            void visit(const Nodecl::ForStatement& node);
            void visit(const Nodecl::WhileStatement& node);
            void visit(const Nodecl::RangeLoopControl& node);
            void visit(const Nodecl::SwitchStatement& node);
            void visit(const Nodecl::CaseStatement& node);
            void visit(const Nodecl::DefaultStatement& node);
            void visit(const Nodecl::BreakStatement& node);
            void visit(const Nodecl::ContinueStatement& node);
            void visit(const Nodecl::FortranIoSpec& node);
            void visit(const Nodecl::FortranPrintStatement& node);
            void visit(const Nodecl::FortranWriteStatement& node);
            void visit(const Nodecl::FortranReadStatement& node);
            void visit(const Nodecl::FortranStopStatement& node);
            void visit(const Nodecl::FortranPauseStatement& node);
            void visit(const Nodecl::FortranComputedGotoStatement& node);
            void visit(const Nodecl::FortranIoStatement& node);
            void visit(const Nodecl::FortranOpenStatement& node);
            void visit(const Nodecl::FortranCloseStatement& node);
            void visit(const Nodecl::FortranAllocateStatement& node);
            void visit(const Nodecl::FortranDeallocateStatement& node);
            void visit(const Nodecl::FortranNullifyStatement& node);
            void visit(const Nodecl::FortranArithmeticIfStatement& node);
            void visit(const Nodecl::FortranLabelAssignStatement& node);
            void visit(const Nodecl::FortranAssignedGotoStatement& node);
            void visit(const Nodecl::FortranEntryStatement& node);
            void visit(const Nodecl::FortranImpliedDo& node);
            void visit(const Nodecl::FortranData& node);
            void visit(const Nodecl::FortranEquivalence& node);
            void visit(const Nodecl::FortranAlternateReturnArgument& node);
            void visit(const Nodecl::FortranAlternateReturnStatement& node);
            void visit(const Nodecl::FortranForall& node);
            void visit(const Nodecl::FortranWhere& node);
            void visit(const Nodecl::FortranBozLiteral& node);
            void visit(const Nodecl::FortranHollerith& node);
            void visit(const Nodecl::FortranUse& node);
            void visit(const Nodecl::FortranUseOnly& node);
            void visit(const Nodecl::FieldDesignator& node);
            void visit(const Nodecl::IndexDesignator& node);
            void visit(const Nodecl::Conversion& node);
            void visit(const Nodecl::UnknownPragma& node);
            void visit(const Nodecl::PragmaCustomDeclaration& node);
            void visit(const Nodecl::PragmaCustomClause& node);
            void visit(const Nodecl::PragmaCustomLine& node);
            void visit(const Nodecl::PragmaCustomStatement& node);
            void visit(const Nodecl::PragmaCustomDirective& node);
            void visit(const Nodecl::PragmaClauseArg& node);
            void visit(const Nodecl::SourceComment& node);
            void visit(const Nodecl::Sizeof& node);
            void visit(const Nodecl::Alignof& node);

            void visit(const Nodecl::MulAssignment & node);
            void visit(const Nodecl::DivAssignment & node);
            void visit(const Nodecl::AddAssignment & node);
            void visit(const Nodecl::MinusAssignment & node);

            void visit(const Nodecl::Preincrement& node);
            void visit(const Nodecl::Postincrement& node);
            void visit(const Nodecl::Predecrement& node);
            void visit(const Nodecl::Postdecrement& node);

            void visit(const Nodecl::ErrExpr& node);
            void visit(const Nodecl::ErrStatement& node);

            void emit_explicit_use_statement(TL::Symbol &module,
                    Nodecl::List items,
                    bool is_only);

            void visit(const Nodecl::CxxDepNameSimple& node);

            void codegen_type(TL::Type t, 
                    std::string& type_specifier, 
                    std::string& array_specifier);
            void codegen_type_extended(TL::Type t, 
                    std::string& type_specifier, 
                    std::string& array_specifier,
                    bool force_deferred_shape,
                    bool without_type_qualifier);

            virtual void push_scope(TL::Scope sc) { }
            virtual void pop_scope() { }

            std::string emit_declaration_for_symbol(TL::Symbol symbol, TL::Scope sc);
            std::string emit_declaration_for_symbols(const TL::ObjectList<TL::Symbol>& sym_set, TL::Scope sc);

            void set_emit_types_as_literals(bool b)
            {
                state.emit_types_as_literals = b;
            }
        private:
            // State
            struct State
            {
                // Level of indentation
                int _indent_level;

                // Inside a FORALL construct
                bool in_forall;

                // An INTERFACE block (without generic-specifier) is open
                bool in_interface;

                // We are in the top level of a data-value
                bool in_data_value;

                // We emit an array construct but we want it flattened
                bool flatten_array_construct;

                bool emit_types_as_literals;

                // Instead of using a KIND integer value, emit the appropiate
                // kind-name from ISO_C_BINDING
                bool emit_interoperable_types;

                // Fun locs
                bool emit_fun_loc;

                // Used when emitting C for-statements
                Nodecl::NodeclBase loop_next_iter;

                State()
                    : _indent_level(0),
                    in_forall(false),
                    in_interface(false),
                    in_data_value(false),
                    flatten_array_construct(false),
                    emit_types_as_literals(false),
                    emit_interoperable_types(false),
                    emit_fun_loc(false)
                {
                }
            } state;

            struct UseStmtItem
            {
                TL::Symbol symbol;
                bool operator==(const UseStmtItem& info) const
                {
                    return (this->symbol == info.symbol);
                }
                bool operator!=(const UseStmtItem& info) const
                {
                    return !this->operator==(info);
                }
            };

            struct UseStmtInfo : std::map<TL::Symbol, TL::ObjectList<UseStmtItem> >
            {
                bool emit_iso_c_binding;

                UseStmtInfo()
                    : emit_iso_c_binding(false) { }

                void add_item(TL::Symbol module, TL::Symbol sym)
                {
                    UseStmtInfo::iterator it = this->find(module);

                    UseStmtItem item;
                    item.symbol = sym;

                    if (it == this->end())
                    {
                        TL::ObjectList<UseStmtItem> new_list;
                        new_list.append(item);

                        this->insert(std::make_pair(module, new_list));
                    }
                    else
                    {
                        it->second.insert(item);
                    }
                }
            };

            struct DoDeclareSymFromModuleInfo
            {
                TL::Scope sc;
                UseStmtInfo& use_stmt_info;
                DoDeclareSymFromModuleInfo(TL::Scope _sc, UseStmtInfo& _use_stmt_info)
                    : sc(_sc), use_stmt_info(_use_stmt_info) { }
            };

            // Status of the declaration of a given symbol inside a program unit
            // in Fortran only CODEGEN_STATUS_DEFINED or CODEGEN_STATUS_NONE
            //
            // If a symbol is in CODEGEN_STATUS_DEFINED we do not define it again
            typedef std::map<TL::Symbol, codegen_status_t> codegen_status_map_t;
            codegen_status_map_t _codegen_status;
            std::vector<codegen_status_map_t> _codegen_status_stack;

            // Set of names actively used in the current scoping unit
            // This is used for renames (see later)
            typedef std::set<std::string> name_set_t;
            std::vector<name_set_t> _name_set_stack;

            // We have to remember explicit uses because of a bug in gfortran
            typedef std::set<std::pair<std::string, std::pair<std::string, std::string> > > explicit_use_t;
            std::vector<explicit_use_t> _explicit_use_stack;

            // Given a symbol, its rename, if any. When _name_set detects
            // that a name has already been used in this scoping unit
            // a rename for it is computed, and then kep here
            typedef std::map<TL::Symbol, std::string> rename_map_t;
            std::vector<rename_map_t> _rename_map_stack;

            std::vector<TL::Symbol> _being_declared_stack;

            // Map of types for PTR_LOC
            // When (due to code coming from C-parsed code) we need
            // the address of a pointer variable (not the address of what
            // is pointing which can actually be obtained using LOC)
            // we emit a PTR_LOC_xxx call which must be later emitted in C
            typedef std::map<TL::Type, std::string> ptr_loc_map_t;
            ptr_loc_map_t _ptr_loc_map;
            ptr_loc_map_t _fun_loc_map;

            // This is a set stating if a given PTR_LOC_xxx names has been
            // emitted already or not in the current scoping unit
            typedef std::set<std::string> external_symbol_set_t;
            external_symbol_set_t _external_symbols;

            std::string define_ptr_loc(TL::Type t, const std::string& function_name);
            std::string define_fun_loc(TL::Type t, const std::string& function_name);

            void set_codegen_status(TL::Symbol sym, codegen_status_t status);
            codegen_status_t get_codegen_status(TL::Symbol sym);

            void indent();
            void inc_indent(int n = 1);
            void dec_indent(int n = 1);

            int get_indent_level();
            void set_indent_level(int);

            void codegen_procedure(TL::Symbol entry, Nodecl::List statement_seq,
                   TL::ObjectList<Nodecl::NodeclBase>& internal_subprograms,
                   bool lacks_result);

            void codegen_procedure_declaration_header(TL::Symbol entry, bool& lacks_result);
            void codegen_procedure_declaration_footer(TL::Symbol entry);

            void codegen_module_header(TL::Symbol, 
                    TL::ObjectList<Nodecl::NodeclBase>& before_contains,
                    TL::ObjectList<Nodecl::NodeclBase>& after_contains);

            void codegen_module_footer(TL::Symbol);

            void codegen_blockdata_header(TL::Symbol);
            void codegen_blockdata_footer(TL::Symbol);

            void codegen_comma_separated_list(Nodecl::NodeclBase);

            void codegen_array_subscripts(TL::Symbol array_symbol, Nodecl::NodeclBase node);
            void codegen_single_array_subscript(
                    Nodecl::NodeclBase node,
                    TL::Symbol array_symbol,
                    int dim);
            bool subscript_expresses_whole_dimension(Nodecl::NodeclBase node,
                    TL::Symbol array_symbol,
                    int dim);
            bool calls_to_xbound_for_array_symbol_dim(
                    Nodecl::NodeclBase range_item,
                    TL::Symbol array_symbol,
                    const std::string &function_name,
                    int dim);

            void codegen_function_call_arguments(const Nodecl::NodeclBase arguments, 
                    TL::Symbol called_symbol,
                    TL::Type function_type);

            void do_declare_symbol(TL::Symbol entry, Nodecl::NodeclBase, void*);
            void do_declare_symbol_in_scope(TL::Symbol entry, Nodecl::NodeclBase, void*);
            void declare_symbol(TL::Symbol, TL::Scope sc);

            void declare_everything_needed(Nodecl::NodeclBase statement_seq);
            void declare_everything_needed(Nodecl::NodeclBase statement_seq, TL::Scope sc);

            void declare_everything_needed_by_the_type(TL::Type t, TL::Scope sc);

            void traverse_looking_for_symbols(Nodecl::NodeclBase node,
                    void (FortranBase::*do_declare)(TL::Symbol entry, Nodecl::NodeclBase node, void *data),
                    void *data);

            void do_declare_symbol_from_module(TL::Symbol entry, Nodecl::NodeclBase node, void *data);
            void declare_use_statements(Nodecl::NodeclBase statement_seq, UseStmtInfo&);
            void declare_use_statements(Nodecl::NodeclBase node, TL::Scope sc, UseStmtInfo&);
            void declare_use_statements(TL::ObjectList<Nodecl::NodeclBase> node,
                    TL::Scope sc, UseStmtInfo& use_stmt_info);
            void declare_use_statements_of_procedure(
                    TL::Symbol entry,
                    Nodecl::List statement_seq,
                    TL::ObjectList<Nodecl::NodeclBase> &internal_subprograms);
            void emit_use_statement_if_symbol_comes_from_module(TL::Symbol entry, const TL::Scope &sc, UseStmtInfo&);

            void declare_module_level_entities(Nodecl::NodeclBase node);
            void do_declare_module_level_entities(TL::Symbol entry, Nodecl::NodeclBase, void *data);

            void codegen_write_or_read_statement(
                    const std::string& keyword,
                    Nodecl::NodeclBase io_spec_list,
                    Nodecl::NodeclBase io_item_list);

            void codegen_open_close_statement(const std::string& keyword, 
                    Nodecl::NodeclBase io_spec);

            void codegen_allocation_statement(const std::string& keyword,
                    Nodecl::NodeclBase allocation_items,
                    Nodecl::NodeclBase io_spec);

            void codegen_comparison(
                    Nodecl::NodeclBase lhs, 
                    Nodecl::NodeclBase rhs, 
                    const std::string& operator_arith, 
                    const std::string& operator_bool);

            bool is_fortran_representable_pointer(TL::Type t);

            bool requires_explicit_cast(const Nodecl::Conversion& node);
            void codegen_casting(
                    TL::Type dest_type, 
                    TL::Type source_type, 
                    Nodecl::NodeclBase nest);

            void emit_collected_use_statements(UseStmtInfo& use_stmt_info);
            void codegen_use_statement(TL::Symbol entry, const TL::Scope &sc, UseStmtInfo& use_stmt_info);

            void declare_symbols_from_modules_rec(Nodecl::NodeclBase node, const TL::Scope &sc, UseStmtInfo&);

            void declare_symbols_rec(Nodecl::NodeclBase node);
            void declare_symbols_rec(Nodecl::NodeclBase node, TL::Scope sc);
            void address_of_pointer(Nodecl::NodeclBase node);
            void address_of_pointer(Nodecl::NodeclBase node, TL::Scope sc);

            virtual Ret unhandled_node(const Nodecl::NodeclBase & n);

            void clear_codegen_status();

            bool is_bitfield_access(const Nodecl::NodeclBase &node);
            void emit_bitfield_store(const Nodecl::Assignment &node);
            void emit_bitfield_load(const Nodecl::ClassMemberAccess &node);

            void common_increment(const Nodecl::NodeclBase& item);
            void common_decrement(const Nodecl::NodeclBase& item);

            std::string compute_new_rename(TL::Symbol sym);

            bool name_has_already_been_used(const std::string &str);
            bool name_has_already_been_used(TL::Symbol sym);

            void set_symbol_name_as_already_used(TL::Symbol sym);

            std::string rename(TL::Symbol sym);
            void remove_rename(TL::Symbol sym);
            void clear_renames();

            bool explicit_use_has_already_been_emitted(
                    const std::string& module_name,
                    const std::string& name,
                    const std::string& rename_name);
            void set_explicit_use_has_already_been_emitted(
                    const std::string& module_name,
                    const std::string& name,
                    const std::string& rename_name);

            void emit_ptr_loc_C();

            void push_declaration_status();
            void pop_declaration_status();

            void push_declaring_entity(TL::Symbol sym);
            void pop_declaring_entity();

            TL::Symbol get_current_declaring_symbol();
            TL::Symbol get_current_declaring_module();

            bool inside_an_interface();

            void emit_interface_for_symbol(TL::Symbol entry);

            bool entry_is_in_scope(TL::Symbol entry, TL::Scope sc);

            bool module_can_be_reached(TL::Symbol current_module, TL::Symbol module_target);
            bool symbol_is_public_in_module(TL::Symbol current_module, TL::Symbol entry);

            static Nodecl::NodeclBase advance_parenthesized_expression(Nodecl::NodeclBase n);

            void if_else_body(Nodecl::NodeclBase then, Nodecl::NodeclBase else_);

            void emit_floating_constant(const_value_t* value);
            void emit_integer_constant(const_value_t* value, TL::Type t);

            void emit_only_list(const std::string& module_name, Nodecl::List only_items);

            std::string _emit_fun_loc_str;
            bool _emit_fun_loc;
            void set_emit_fun_loc(const std::string& str);

            std::string _deduce_use_statements_str;
            bool _deduce_use_statements;
            void set_deduce_use_statements(const std::string& str);

            std::string _emit_full_array_subscripts_str;
            bool _emit_full_array_subscripts;
            void set_emit_full_array_subscripts(const std::string& str);
    };
}

#endif // CODEGEN_FORTRAN_HPP
