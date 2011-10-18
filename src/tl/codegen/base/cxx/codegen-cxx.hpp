#ifndef CODEGEN_CXX_HPP
#define CODEGEN_CXX_HPP

#include "codegen-common.hpp"
#include "tl-scope.hpp"
#include "tl-symbol.hpp"
#include <sstream>
#include <map>

namespace Codegen
{
    class CXXBase : public CodegenVisitor
    {
        public:
            virtual std::string codegen(const Nodecl::NodeclBase&);

            Ret visit(const Nodecl::Add &);
            Ret visit(const Nodecl::AddAssignment &);
            Ret visit(const Nodecl::AnyList &);
            Ret visit(const Nodecl::ArraySubscript &);
            Ret visit(const Nodecl::Assignment &);
            Ret visit(const Nodecl::BitwiseAnd &);
            Ret visit(const Nodecl::BitwiseAndAssignment &);
            Ret visit(const Nodecl::BitwiseNot &);
            Ret visit(const Nodecl::BitwiseOr &);
            Ret visit(const Nodecl::BitwiseOrAssignment &);
            Ret visit(const Nodecl::BitwiseXor &);
            Ret visit(const Nodecl::BitwiseXorAssignment &);
            Ret visit(const Nodecl::BooleanLiteral &);
            Ret visit(const Nodecl::BreakStatement &);
            Ret visit(const Nodecl::BuiltinDecl &);
            Ret visit(const Nodecl::BuiltinExpr &);
            Ret visit(const Nodecl::C99DesignatedInitializer &);
            Ret visit(const Nodecl::C99FieldDesignator &);
            Ret visit(const Nodecl::C99IndexDesignator &);
            Ret visit(const Nodecl::CaseStatement &);
            Ret visit(const Nodecl::Cast &);
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
            Ret visit(const Nodecl::CxxBracedInitializer &);
            Ret visit(const Nodecl::CxxDepGlobalNameNested &);
            Ret visit(const Nodecl::CxxDepNameConversion &);
            Ret visit(const Nodecl::CxxDepNameNested &);
            Ret visit(const Nodecl::CxxDepNameSimple &);
            Ret visit(const Nodecl::CxxDepTemplateId &);
            Ret visit(const Nodecl::CxxEqualInitializer &);
            Ret visit(const Nodecl::CxxExplicitTypeCast &);
            Ret visit(const Nodecl::CxxParenthesizedInitializer &);
            Ret visit(const Nodecl::DefaultStatement &);
            Ret visit(const Nodecl::Delete &);
            Ret visit(const Nodecl::DeleteArray &);
            Ret visit(const Nodecl::Derreference &);
            Ret visit(const Nodecl::Different &);
            Ret visit(const Nodecl::Div &);
            Ret visit(const Nodecl::DivAssignment &);
            Ret visit(const Nodecl::DoStatement &);
            Ret visit(const Nodecl::EmptyStatement &);
            Ret visit(const Nodecl::Equal &);
            Ret visit(const Nodecl::ErrExpr &);
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
            Ret visit(const Nodecl::IntegerLiteral &);
            Ret visit(const Nodecl::LabeledStatement &);
            Ret visit(const Nodecl::LogicalAnd &);
            Ret visit(const Nodecl::LogicalNot &);
            Ret visit(const Nodecl::LogicalOr &);
            Ret visit(const Nodecl::LoopControl &);
            Ret visit(const Nodecl::LowerOrEqualThan &);
            Ret visit(const Nodecl::LowerThan &);
            Ret visit(const Nodecl::MemberInit &);
            Ret visit(const Nodecl::Minus &);
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
            Ret visit(const Nodecl::Shl &);
            Ret visit(const Nodecl::ShlAssignment &);
            Ret visit(const Nodecl::Shr &);
            Ret visit(const Nodecl::ShrAssignment &);
            Ret visit(const Nodecl::Sizeof &);
            Ret visit(const Nodecl::StringLiteral &);
            Ret visit(const Nodecl::StructuredValue &);
            Ret visit(const Nodecl::SubAssignment &);
            Ret visit(const Nodecl::SwitchStatement &);
            Ret visit(const Nodecl::Symbol &);
            Ret visit(const Nodecl::Text &);
            Ret visit(const Nodecl::Throw &);
            Ret visit(const Nodecl::TopLevel &);
            Ret visit(const Nodecl::TryBlock &);
            Ret visit(const Nodecl::Type &);
            Ret visit(const Nodecl::Typeid &);
            Ret visit(const Nodecl::VirtualFunctionCall &);
            Ret visit(const Nodecl::WhileStatement &);

        private:

            // State
            TL::Scope current_scope;
            std::stringstream file;
            TL::Symbol current_symbol;
            // End of State

            void define_symbol(TL::Symbol symbol);
            void declare_symbol(TL::Symbol symbol);

            void define_all_entities_in_trees(const Nodecl::NodeclBase&);
            void define_nonlocal_entities_in_trees(const Nodecl::NodeclBase&);

            void walk_type_for_symbols(TL::Type, 
                    bool needs_def, 
                    void (CXXBase::* declare_fun)(TL::Symbol),
                    void (CXXBase::* define_fun)(TL::Symbol),
                    void (CXXBase::* define_entities)(const Nodecl::NodeclBase&));

            std::map<TL::Symbol, codegen_status_t> _codegen_status;
            void set_codegen_status(TL::Symbol sym, codegen_status_t status);
            codegen_status_t get_codegen_status(TL::Symbol sym);
    };
}

#endif // CODEGEN_CXX_HPP
