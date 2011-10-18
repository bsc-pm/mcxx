#include "codegen-cxx.hpp"
#include "tl-objectlist.hpp"
#include "tl-type.hpp"

namespace Codegen {

std::string CXXBase::codegen(const Nodecl::NodeclBase &n) 
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Add& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::AddAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::AnyList& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ArraySubscript& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Assignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BitwiseAnd& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BitwiseAndAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BitwiseNot& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BitwiseOr& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BitwiseOrAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BitwiseXor& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BitwiseXorAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BooleanLiteral& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BreakStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BuiltinDecl& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BuiltinExpr& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::C99DesignatedInitializer& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::C99FieldDesignator& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::C99IndexDesignator& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CaseStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Cast& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CatchHandler& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ClassMemberAccess& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Comma& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ComplexLiteral& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CompoundExpression& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CompoundStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ConditionalExpression& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Context& node)
{
    TL::Scope old_scope = this->current_scope;
    this->current_scope = node.retrieve_context();

    walk(node.get_in_context());

    this->current_scope = old_scope;
}

CXXBase::Ret CXXBase::visit(const Nodecl::ContinueStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Conversion& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CxxBracedInitializer& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CxxDepGlobalNameNested& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CxxDepNameConversion& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CxxDepNameNested& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CxxDepNameSimple& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CxxDepTemplateId& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CxxEqualInitializer& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CxxExplicitTypeCast& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CxxParenthesizedInitializer& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::DefaultStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Delete& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::DeleteArray& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Derreference& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Different& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Div& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::DivAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::DoStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::EmptyStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Equal& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ErrExpr& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ExpressionStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::FieldDesignator& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::FloatingLiteral& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ForStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::FunctionCall& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::FunctionCode& node)
{
    Nodecl::Context context = node.get_statements().as<Nodecl::Context>();
    Nodecl::List statement_seq = context.get_in_context().as<Nodecl::List>();
    Nodecl::NodeclBase initializers = node.get_initializers();
    Nodecl::NodeclBase internal_functions = node.get_internal_functions();

    if (!internal_functions.is_null())
    {
        internal_error("C/C++ does not have internal functions", 0);
    }

    if (statement_seq.size() != 1)
    {
        internal_error("C/C++ functions only have one statement", 0);
    }

    Nodecl::NodeclBase statement = statement_seq[0];

    TL::Symbol symbol = node.get_symbol();
    TL::Type symbol_type = node.get_type();

    ERROR_CONDITION(!symbol.is_function(), "Invalid symbol", 0);

    if (symbol.is_member())
    {
        TL::Symbol class_symbol = symbol.get_class_type().get_symbol();
        this->define_symbol(class_symbol);
    }
    else
    {
        if (symbol_type.is_template_specialized_type()
                && symbol_type.template_specialized_type_get_template_arguments().get_num_parameters() != 0)
        {
            TL::Type template_type = symbol_type.get_related_template_type();
            TL::Type primary_type = template_type.get_primary_template();
            TL::Symbol primary_symbol = primary_type.get_symbol();
            declare_symbol(primary_symbol);
        }
    }

    walk_type_for_symbols(symbol_type.returns(), 
            /* needs_def */ true,
            &CXXBase::declare_symbol,
            &CXXBase::define_symbol,
            &CXXBase::define_all_entities_in_trees);

    this->current_symbol = symbol;

    bool has_ellipsis = false;
    TL::ObjectList<TL::Type> parameter_list = symbol_type.parameters(has_ellipsis);
    int num_parameters = parameter_list.size();

    for (TL::ObjectList<TL::Type>::iterator it = parameter_list.begin();
            it != parameter_list.end();
            it++)
    {
        walk_type_for_symbols(*it, /* needs_def */ 1, 
                &CXXBase::declare_symbol,
                &CXXBase::define_symbol,
                &CXXBase::define_all_entities_in_trees);
    }

    define_nonlocal_entities_in_trees(statement);

    TL::ObjectList<TL::Symbol> related_symbols = symbol.get_related_symbols();
    TL::ObjectList<std::string> parameter_names(related_symbols.size());
    int i = 0;
    for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
            it != related_symbols.end();
            it++, i++)
    {
        if (it->is_valid())
        {
            parameter_names[i] = it->get_name();
            set_codegen_status(*it, CODEGEN_STATUS_DEFINED);
        }
    }

    std::string decl_spec_seq;

    if (symbol.is_static()
            && !symbol.is_member())
    {
        decl_spec_seq += "static ";
    }
    if (symbol.is_extern())
    {
        decl_spec_seq += "extern ";
    }
    if (symbol.is_inline())
    {
        C_LANGUAGE()
        {
            decl_spec_seq += "__inline ";
        }
        CXX_LANGUAGE()
        {
            decl_spec_seq += "inline ";
        }
    }

    std::string gcc_attributes;

    TL::ObjectList<TL::GCCAttribute> gcc_attr_list = symbol.get_gcc_attributes();

    for (TL::ObjectList<TL::GCCAttribute>::iterator it = gcc_attr_list.begin();
            it != gcc_attr_list.end();
            it++)
    {
    }
}

CXXBase::Ret CXXBase::visit(const Nodecl::GotoStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::GreaterOrEqualThan& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::GreaterThan& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::IfElseStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ImagPart& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::IndexDesignator& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::IntegerLiteral& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::LabeledStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::LogicalAnd& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::LogicalNot& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::LogicalOr& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::LoopControl& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::LowerOrEqualThan& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::LowerThan& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::MemberInit& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Minus& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Mod& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ModAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Mul& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::MulAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Neg& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::New& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ObjectInit& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Offset& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Offsetof& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ParenthesizedExpression& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Plus& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::PointerToMember& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Postdecrement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Postincrement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaClauseArg& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomClause& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomDeclaration& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomDirective& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomLine& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Predecrement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Preincrement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::PseudoDestructorName& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Range& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::RealPart& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Reference& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ReturnStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Shaping& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Shl& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ShlAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Shr& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ShrAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Sizeof& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::StringLiteral& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::StructuredValue& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::SubAssignment& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::SwitchStatement& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Symbol& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Text& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Throw& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::TopLevel& node)
{
    walk(node.get_top_level());
}

CXXBase::Ret CXXBase::visit(const Nodecl::TryBlock& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Type& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Typeid& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::VirtualFunctionCall& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::WhileStatement& node)
{
}

void CXXBase::set_codegen_status(TL::Symbol sym, codegen_status_t status)
{
    _codegen_status[sym] = status;
}

codegen_status_t CXXBase::get_codegen_status(TL::Symbol sym)
{
    std::map<TL::Symbol, codegen_status_t>::iterator it = _codegen_status.find(sym);

    if (it == _codegen_status.end())
    {
        return CODEGEN_STATUS_NONE;
    }
    else
    {
        return it->second;
    }
}

} // Codegen
