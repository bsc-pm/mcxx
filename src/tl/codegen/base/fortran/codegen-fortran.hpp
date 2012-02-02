#ifndef CODEGEN_FORTRAN_HPP
#define CODEGEN_FORTRAN_HPP

#include "codegen-phase.hpp"

#include <set>

namespace Codegen
{
    class FortranBase : public CodegenPhase
    {
        protected:

            virtual std::string codegen(const Nodecl::NodeclBase&);

        public:

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
            void visit(const Nodecl::Derreference& node);
            void visit(const Nodecl::Reference& node);
            void visit(const Nodecl::ParenthesizedExpression& node);
            void visit(const Nodecl::ArraySubscript& node);
            void visit(const Nodecl::FunctionCall& node);
            void visit(const Nodecl::FortranNamedPairSpec& node);
            void visit(const Nodecl::EmptyStatement& node);
            void visit(const Nodecl::IfElseStatement& node);
            void visit(const Nodecl::ReturnStatement& node);
            void visit(const Nodecl::LabeledStatement& node);
            void visit(const Nodecl::GotoStatement& node);
            void visit(const Nodecl::ForStatement& node);
            void visit(const Nodecl::WhileStatement& node);
            void visit(const Nodecl::LoopControl& node);
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
            void visit(const Nodecl::FieldDesignator& node);
            void visit(const Nodecl::Conversion& node);
            void visit(const Nodecl::UnknownPragma& node);
            void visit(const Nodecl::PragmaCustomClause& node);
            void visit(const Nodecl::PragmaCustomLine& node);
            void visit(const Nodecl::PragmaCustomStatement& node);
            void visit(const Nodecl::PragmaCustomDirective& node);
            void visit(const Nodecl::PragmaClauseArg& node);
            void visit(const Nodecl::SourceComment& node);
            void visit(const Nodecl::Cast& node);
            void visit(const Nodecl::Sizeof& node);
            void visit(const Nodecl::Alignof& node);

            void visit(const Nodecl::SavedExpr& node);

            void visit(const Nodecl::CxxDepNameSimple& node);

            void codegen_type(TL::Type t, 
                    std::string& type_specifier, 
                    std::string& array_specifier,
                    bool is_dummy);
        private:
            // State
            struct State
            {
                int _indent_level;

                TL::Symbol current_symbol;
                TL::Symbol current_module;

                bool in_forall;
                bool in_interface;

                State()
                    : _indent_level(0),
                    current_symbol(NULL),
                    current_module(NULL),
                    in_forall(false),
                    in_interface(false)
                {
                }
            } state;

            typedef std::map<TL::Symbol, codegen_status_t> codegen_status_map_t;
            codegen_status_map_t _codegen_status;

            typedef std::set<std::string> name_set_t;
            name_set_t _name_set;

            typedef std::map<TL::Symbol, std::string> rename_map_t;
            rename_map_t _rename_map;

            void set_codegen_status(TL::Symbol sym, codegen_status_t status);
            codegen_status_t get_codegen_status(TL::Symbol sym);

            void indent();
            void inc_indent(int n = 1);
            void dec_indent(int n = 1);

            int get_indent_level();
            void set_indent_level(int);

            void codegen_procedure(TL::Symbol entry, Nodecl::List statement_seq, Nodecl::List internal_subprograms, bool lacks_result);

            void codegen_procedure_declaration_header(TL::Symbol entry, bool& lacks_result);
            void codegen_procedure_declaration_footer(TL::Symbol entry);

            void codegen_module_header(TL::Symbol, TL::ObjectList<Nodecl::NodeclBase>);

            void codegen_module_footer(TL::Symbol);

            void codegen_blockdata_header(TL::Symbol);
            void codegen_blockdata_footer(TL::Symbol);

            void codegen_comma_separated_list(Nodecl::NodeclBase);
            void codegen_reverse_comma_separated_list(Nodecl::NodeclBase);

            void declare_symbol(TL::Symbol);
            void declare_everything_needed(Nodecl::NodeclBase statement_seq);

            void declare_use_statements(Nodecl::NodeclBase statement_seq);
            void emit_use_statement_if_symbol_comes_from_module(TL::Symbol entry, const TL::Scope &sc);

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

            void codegen_casting(
                    TL::Type dest_type, 
                    TL::Type source_type, 
                    Nodecl::NodeclBase nest);

            void codegen_use_statement(TL::Symbol entry, const TL::Scope &sc);

            void declare_symbols_from_modules_rec(Nodecl::NodeclBase node, const TL::Scope &sc);

            void declare_symbols_rec(Nodecl::NodeclBase node);

            virtual Ret unhandled_node(const Nodecl::NodeclBase & n);

            void clear_codegen_status();

            bool is_bitfield_access(const Nodecl::NodeclBase &node);
            void emit_bitfield_store(const Nodecl::Assignment &node);

            bool name_has_already_been_used(std::string str);
            bool name_has_already_been_used(TL::Symbol sym);
            std::string rename(TL::Symbol sym);
            void clear_renames();
    };
}

#endif // CODEGEN_FORTRAN_HPP
