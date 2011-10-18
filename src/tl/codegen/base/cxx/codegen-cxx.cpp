#include "codegen-cxx.hpp"
#include "tl-objectlist.hpp"
#include "tl-type.hpp"
#include "cxx-cexpr.h"

#ifdef HAVE_QUADMATH_H
#include <quadmath.h>
#endif

namespace Codegen {

std::string CXXBase::codegen(const Nodecl::NodeclBase &n) 
{
}

#define OPERATOR_TABLE \
    PREFIX_UNARY_EXPRESSION(Plus, "+") \
    PREFIX_UNARY_EXPRESSION(Neg, "-") \
    PREFIX_UNARY_EXPRESSION(LogicalNot, "!") \
    PREFIX_UNARY_EXPRESSION(BitwiseNot, "~") \
    PREFIX_UNARY_EXPRESSION(Derreference, "*") \
    PREFIX_UNARY_EXPRESSION(Reference, "&") \
    PREFIX_UNARY_EXPRESSION(Preincrement, "++") \
    PREFIX_UNARY_EXPRESSION(Predecrement, "--") \
    PREFIX_UNARY_EXPRESSION(Delete, "delete ") \
    PREFIX_UNARY_EXPRESSION(DeleteArray, "delete[] ") \
    PREFIX_UNARY_EXPRESSION(RealPart, "_Real__ ") \
    PREFIX_UNARY_EXPRESSION(ImagPart, "_Imag__ ") \
    POSTFIX_UNARY_EXPRESSION(Postincrement, "++") \
    POSTFIX_UNARY_EXPRESSION(Postdecrement, "--") \
    BINARY_EXPRESSION(Add, " + ") \
    BINARY_EXPRESSION(Mul, " * ") \
    BINARY_EXPRESSION(Div, " / ") \
    BINARY_EXPRESSION(Mod, " % ") \
    BINARY_EXPRESSION(Minus, " - ") \
    BINARY_EXPRESSION(Equal, " == ") \
    BINARY_EXPRESSION(Different, " != ") \
    BINARY_EXPRESSION(LowerThan, " < ") \
    BINARY_EXPRESSION(LowerOrEqualThan, " <= ") \
    BINARY_EXPRESSION(GreaterThan, " > ") \
    BINARY_EXPRESSION(GreaterOrEqualThan, " >= ") \
    BINARY_EXPRESSION(LogicalAnd, " && ") \
    BINARY_EXPRESSION(LogicalOr, " || ") \
    BINARY_EXPRESSION(BitwiseAnd, " & ") \
    BINARY_EXPRESSION(BitwiseOr, " | ") \
    BINARY_EXPRESSION(BitwiseXor, " ^ ") \
    BINARY_EXPRESSION(Shl, " << ") \
    BINARY_EXPRESSION(Shr, " >> ") \
    BINARY_EXPRESSION_ASSIG(Assignment, " = ") \
    BINARY_EXPRESSION_ASSIG(MulAssignment, " *= ") \
    BINARY_EXPRESSION_ASSIG(DivAssignment, " /= ") \
    BINARY_EXPRESSION_ASSIG(AddAssignment, " += ") \
    BINARY_EXPRESSION_ASSIG(SubAssignment, " -= ") \
    BINARY_EXPRESSION_ASSIG(ShlAssignment, " <<= ") \
    BINARY_EXPRESSION_ASSIG(ShrAssignment, " >>= ") \
    BINARY_EXPRESSION_ASSIG(BitwiseAndAssignment, " &= ") \
    BINARY_EXPRESSION_ASSIG(BitwiseOrAssignment, " |= ") \
    BINARY_EXPRESSION_ASSIG(BitwiseXorAssignment, " ^= ") \
    BINARY_EXPRESSION_ASSIG(ModAssignment, " %= ") \
    BINARY_EXPRESSION(Offset, ".*") \
    BINARY_EXPRESSION(Comma, ", ") \

#define PREFIX_UNARY_EXPRESSION(_name, _operand) \
    void CXXBase::visit(const Nodecl::_name &node) \
    { \
        Nodecl::NodeclBase rhs = node.children()[0]; \
        char needs_parentheses = operand_has_lower_priority(node, rhs); \
        file << "%s" << _operand; \
        if (needs_parentheses) \
        { \
            file << "("; \
        } \
        walk(rhs); \
        if (needs_parentheses) \
        { \
            file << ")"; \
        } \
    }

#define POSTFIX_UNARY_EXPRESSION(_name, _operand) \
    void CXXBase::visit(const Nodecl::_name& node) \
    { \
        Nodecl::NodeclBase rhs = node.children()[0]; \
        char needs_parentheses = operand_has_lower_priority(node, rhs); \
        if (needs_parentheses) \
        { \
            file << "("; \
        } \
        walk(rhs); \
        if (needs_parentheses) \
        { \
            file << ")"; \
        } \
        file << "%s" << _operand; \
    }

#define BINARY_EXPRESSION(_name, _operand) \
    void CXXBase::visit(const Nodecl::_name& node) \
    { \
        Nodecl::NodeclBase lhs = node.children()[0]; \
        Nodecl::NodeclBase rhs = node.children()[1]; \
        char needs_parentheses = operand_has_lower_priority(node, lhs); \
        if (needs_parentheses) \
        { \
            file << "("; \
        } \
        walk(lhs); \
        if (needs_parentheses) \
        { \
            file << ")"; \
        } \
        file << _operand; \
        needs_parentheses = operand_has_lower_priority(node, rhs); \
        if (needs_parentheses) \
        { \
            file << "("; \
        } \
        walk(rhs); \
        if (needs_parentheses) \
        { \
            file << ")"; \
        } \
    }
#define BINARY_EXPRESSION_ASSIG(_name, _operand) \
    void CXXBase::visit(const Nodecl::_name& node) \
    { \
        Nodecl::NodeclBase lhs = node.children()[0]; \
        Nodecl::NodeclBase rhs = node.children()[1]; \
        if (state.in_condition && state.condition_top == node) \
        { \
            file << "("; \
        } \
        char needs_parentheses = operand_has_lower_priority(node, lhs); \
        if (needs_parentheses) \
        { \
            file << "("; \
        } \
        walk(lhs); \
        if (needs_parentheses) \
        { \
            file << ")"; \
        } \
        file << _operand; \
        needs_parentheses = operand_has_lower_priority(node, rhs); \
        if (needs_parentheses) \
        { \
            file << "("; \
        } \
        walk(rhs); \
        if (needs_parentheses) \
        { \
            file << ")"; \
        } \
        if (state.in_condition && state.condition_top == node) \
        { \
            file << "("; \
        } \
    }
OPERATOR_TABLE
#undef POSTFIX_UNARY_EXPRESSION
#undef PREFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION
#undef BINARY_EXPRESSION_ASSIG


CXXBase::Ret CXXBase::visit(const Nodecl::AnyList& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ArraySubscript& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::BooleanLiteral& node)
{
    const_value_t* val = nodecl_get_constant(node.get_internal_nodecl());

    if (const_value_is_zero(val))
    {
        file << "false";
    }
    else
    {
        file << "true";
    }
}

CXXBase::Ret CXXBase::visit(const Nodecl::BreakStatement& node)
{
    indent();
    file << "break;\n";
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
    Nodecl::NodeclBase expression = node.get_case();
    Nodecl::NodeclBase statement = node.get_statement();

    indent();
    file << "case ";
    walk(expression);
    file << " :\n";

    walk(statement);
}

CXXBase::Ret CXXBase::visit(const Nodecl::Cast& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CatchHandler& node)
{
    Nodecl::NodeclBase name = node.get_name();
    Nodecl::NodeclBase statement = node.get_statement();
    TL::Type type = node.get_type();

    indent();
    file << "catch (";

    if (name.is_null())
    {
        // FIXME: Is this always safe?
        file << type.get_declaration(state.current_scope, "");
    }
    else
    {
        int old_condition = state.in_condition;
        Nodecl::NodeclBase old_condition_top = state.condition_top;
        int old_indent = get_indent_level();
        set_indent_level(0);

        state.in_condition = 1;
        state.condition_top = name;

        walk(name);

        set_indent_level(old_indent);
        state.condition_top = old_condition_top;
        state.in_condition = old_condition;
    }

    file << ")\n";

    walk(statement);
}

CXXBase::Ret CXXBase::visit(const Nodecl::ClassMemberAccess& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ComplexLiteral& node)
{
    // This is a GCC extension
    //
    // In C this complex literal is created using "2j" so it will always be a
    // literal integer/float and the real part will be zero
    
    // nodecl_t real_part = nodecl_get_child(node, 0); // Zero
    Nodecl::NodeclBase imag_part = node.get_imag();

    walk(imag_part);
    file << "i";
}

CXXBase::Ret CXXBase::visit(const Nodecl::CompoundExpression& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::CompoundStatement& node)
{
    indent();
    file << "{\n";
    inc_indent();

    Nodecl::NodeclBase statement_seq = node.get_statements();

    define_local_entities_in_trees(statement_seq);

    walk(statement_seq);

    dec_indent();
    indent();
    file << "}\n";
}

CXXBase::Ret CXXBase::visit(const Nodecl::ConditionalExpression& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Context& node)
{
    TL::Scope old_scope = state.current_scope;
    state.current_scope = node.retrieve_context();

    walk(node.get_in_context());

    state.current_scope = old_scope;
}

CXXBase::Ret CXXBase::visit(const Nodecl::ContinueStatement& node)
{
    indent();
    file << "continue;\n";
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
    Nodecl::NodeclBase statement = node.get_statement();

    indent();
    file << "default :\n";

    walk(statement);
}

CXXBase::Ret CXXBase::visit(const Nodecl::DoStatement& node)
{
    Nodecl::NodeclBase statement = node.get_statement();
    Nodecl::NodeclBase condition = node.get_condition();

    indent();
    file << "do\n";

    inc_indent();
    walk(statement);
    dec_indent();

    indent();
    file << "while (";
    walk(condition);
    file << ");\n";
}

CXXBase::Ret CXXBase::visit(const Nodecl::EmptyStatement& node)
{
    indent();
    file << ";\n";
}

CXXBase::Ret CXXBase::visit(const Nodecl::ErrExpr& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ExpressionStatement& node)
{
    Nodecl::NodeclBase expression = node.get_nest();
    indent();
    walk(expression);
    file << ";\n";
}

CXXBase::Ret CXXBase::visit(const Nodecl::FieldDesignator& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::FloatingLiteral& node)
{
    const_value_t* value = nodecl_get_constant(node.get_internal_nodecl());
    ERROR_CONDITION(value == NULL, "Invalid value", 0);

    type_t* t = nodecl_get_type(node.get_internal_nodecl());
    int precision = floating_type_get_info(t)->p + 1;


    // FIXME - We are relying on a (really handy) GNU extension
    if (const_value_is_float(value))
    {
        char* floating = NULL;
        asprintf(&floating, "%.*ef", precision, const_value_cast_to_float(value));
        file << floating;
        free(floating);
    }
    else if (const_value_is_double(value))
    {
        char* floating = NULL;
        asprintf(&floating, "%.*e", precision, const_value_cast_to_double(value));
        file << floating;
        free(floating);
    }
    else if (const_value_is_long_double(value))
    {
        char* floating = NULL;
        asprintf(&floating, "%.*LeL", precision, const_value_cast_to_long_double(value));
        file << floating;
        free(floating);
    }
#ifdef HAVE_QUADMATH_H
    else if (const_value_is_float128(value))
    {
        __float128 f128 = const_value_cast_to_float128(value);
        int n = quadmath_snprintf (NULL, 0, "%.*Qe", precision, f128);
        char *c = new char[n + 1];
        quadmath_snprintf (c, n, "%.*Qe", precision, f128);
        c[n] = '\0';
        file << c << "Q";
        delete[] c;
    }
#endif
}

CXXBase::Ret CXXBase::visit(const Nodecl::ForStatement& node)
{
    Nodecl::NodeclBase loop_control = node.get_loop_header();
    Nodecl::NodeclBase statement = node.get_statement();

    indent();
    file << "for (";
    walk(loop_control);
    file << ")";

    inc_indent();
    walk(statement);
    dec_indent();
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

    state.current_symbol = symbol;

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
        if (it->get_expression_list().is_null())
        {
            std::stringstream ss;
            ss << "__attribute__((" << it->get_attribute_name() << ")) ";
        }
        else
        {
            internal_error("Not yet implemented", 0);
        }
    }

    std::string asm_specification;

    if (!symbol.get_asm_specification().is_null())
    {
        internal_error("Not yet implemented", 0);
    }

    std::string qualified_name = symbol.get_class_qualification(symbol.get_scope(), /* without_template */ true);

    if (symbol_type.is_template_specialized_type()
            && !symbol.is_conversion_function())
    {
        // FIXME - This function is not wrapped
        qualified_name += get_template_arguments_str(symbol.get_internal_symbol(), symbol.get_scope().get_decl_context());
    }

    TL::Type real_type = symbol_type.advance_over_typedefs();

    if (symbol.is_conversion_function()
            || symbol.is_destructor())
    {
        // FIXME - Use TL::Type to build this type
        real_type = ::get_new_function_type(NULL, NULL, 0);
    }

    std::string declarator;
    declarator = real_type.get_declaration_with_parameters(symbol.get_scope(), qualified_name, parameter_names);

    std::string exception_spec;
    CXX_LANGUAGE()
    {
        if (!symbol.function_throws_any_exception())
        {
            exception_spec += " throw(";

            TL::ObjectList<TL::Type> exceptions = symbol.get_thrown_exceptions();
            for (TL::ObjectList<TL::Type>::iterator it = exceptions.begin();
                    it != exceptions.end();
                    it++)
            {
                if (it != exceptions.begin())
                {
                    exception_spec += ", ";
                }
                exception_spec += it->get_declaration(symbol.get_scope(), "");
            }

            exception_spec += ")";
        }
    }

    move_to_namespace_of_symbol(symbol);

    if (symbol_type.is_template_specialized_type()
            && symbol_type.template_specialized_type_get_template_arguments().get_num_parameters() != 0)
    {
        indent();
        file << "template<>\n";
    }

    indent();
    file << decl_spec_seq << gcc_attributes << declarator << exception_spec << asm_specification;

    set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);

    if (!initializers.is_null())
    {
        inc_indent();

        indent();
        file << ": ";

        walk_list(initializers, ", ");

        dec_indent();

        file << "\n";
    }

    this->walk(context);
}

CXXBase::Ret CXXBase::visit(const Nodecl::GotoStatement& node)
{
    TL::Symbol label_sym = node.get_symbol();

    indent();
    file << "goto " << label_sym.get_name() << ";\n";
}

CXXBase::Ret CXXBase::visit(const Nodecl::IfElseStatement& node)
{
    Nodecl::NodeclBase condition = node.get_condition();
    Nodecl::NodeclBase then = node.get_then();
    Nodecl::NodeclBase _else = node.get_else();

    indent();

    file << "if (";
    int old_condition = state.in_condition;
    int old_indent = get_indent_level();
    Nodecl::NodeclBase old_condition_top = state.condition_top;

    set_indent_level(0);
    state.in_condition = 1;
    state.condition_top = condition;

    walk(condition);

    set_indent_level(old_indent);
    state.in_condition = old_condition;
    state.condition_top = old_condition_top;

    file << ")\n";

    inc_indent();
    walk(then);
    dec_indent();

    if (!_else.is_null())
    {
        indent();
        file << "else\n";
        inc_indent();
        walk(_else);
        dec_indent();
    }
}

CXXBase::Ret CXXBase::visit(const Nodecl::IndexDesignator& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::IntegerLiteral& node)
{
    const_value_t* value = nodecl_get_constant(node.get_internal_nodecl());
    ERROR_CONDITION(value == NULL, "Invalid value", 0);

    TL::Type t = node.get_type();

    if (t.is_char())
    {
        unsigned char b = const_value_cast_to_1(value);

        switch (b)
        {
            case '\'' : { file << "'\\''"; break; }
            case '\\': { file <<  "'\\\\'"; break; }
            case '\a' : { file << "'\\a'"; break; }
            case '\b' : { file << "'\\b'"; break; }
            case '\e' : { file << "'\\e'"; break; } // GCC extension
            case '\f' : { file << "'\\f'"; break; }
            case '\n' : { file << "'\\n'"; break; }
            case '\r' : { file << "'\\r'"; break; }
            case '\t' : { file << "'\\t'"; break; }
            case '\v' : { file << "'\\v'"; break; }
            case '\"' : { file << "'\\\"'"; break; }
            default: {
                         if (isprint(b))
                         {
                             if (t.is_signed_char())
                             {
                                 file << (signed char) b;
                             }
                             else
                             {
                                 file << (unsigned char) b;
                             }
                         }
                         else
                         {
                             file << "\\x" << std::hex << b << std::dec << "'";
                         }
                     }
        }
    }
    else if (IS_CXX_LANGUAGE && t.is_wchar_t())
    {
        unsigned int mb = const_value_cast_to_4(value);
        file << "L'\\x" << std::hex << mb << std::dec << "'";
    }
    else 
    {
        unsigned long long int v = const_value_cast_to_8(value);

        if (t.is_signed_int())
        {
            file << (signed long long)v;
        }
        else if (t.is_unsigned_int())
        {
            file << (unsigned long long)v << "U";
        }
        else if (t.is_signed_long_int())
        {
            file << (signed long long)v << "L";
        }
        else if (t.is_unsigned_long_int()) 
        {
            file << (unsigned long long)v << "LU";
        }
        else if (t.is_signed_long_long_int())
        {
            file << (signed long long)v << "LL";
        }
        else if (t.is_unsigned_long_long_int())
        {
            file << (unsigned long long)v << "LLU";
        }
        else
        {
            // Remaining integers like 'short'
            if (const_value_is_signed(value))
            {
                file << (signed long long)v;
            }
            else
            {
                file << (unsigned long long)v;
            }
        }
    }

}

CXXBase::Ret CXXBase::visit(const Nodecl::LabeledStatement& node)
{
    TL::Symbol label_sym = node.get_symbol();
    Nodecl::NodeclBase statement = node.get_statement();

    indent();
    file << label_sym.get_name() << " : ";

    int old = get_indent_level();
    set_indent_level(0);
    walk(statement);
    set_indent_level(old);
}

CXXBase::Ret CXXBase::visit(const Nodecl::LoopControl& node)
{
    Nodecl::NodeclBase init = node.get_init();
    Nodecl::NodeclBase cond = node.get_cond();
    Nodecl::NodeclBase next = node.get_next();

    // No condition top as "for((i=0); ...)" looks unnecessary ugly
    int old = state.in_condition;
    state.in_condition = 1;

    walk(init);
    file << ";";

    Nodecl::NodeclBase old_condition_top = state.condition_top;
    state.condition_top = cond;

    // But it is desirable for the condition in "for( ... ; (i = x) ; ...)"
    walk(cond);
    file << ";";

    state.condition_top = old_condition_top;

    // Here we do not care about parentheses "for ( ... ; ... ; i = i + 1)"
    walk(next);
    state.in_condition = old;
}

CXXBase::Ret CXXBase::visit(const Nodecl::MemberInit& node)
{
    TL::Symbol entry = node.get_symbol();
    Nodecl::NodeclBase init_expr = node.get_init_expr();

    if (state.do_not_emit_declarations)
    {
        file << entry.get_type().get_declaration(entry.get_scope(), entry.get_qualified_name());

        if (!init_expr.is_null())
        {
            file << " = ";
            walk(init_expr);
        }
    }
    else // !do_not_emit_declarations
    {
        file << "%s(" << entry.get_name();

        if (nodecl_calls_to_constructor(init_expr, entry.get_type()))
        {
            // Ignore top level constructor
            walk_expression_list(init_expr.children()[1]);
        }
        else
        {
            walk(init_expr);
        }

        file << ")";
    }
}

CXXBase::Ret CXXBase::visit(const Nodecl::New& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ObjectInit& node)
{
    TL::Symbol sym = node.get_symbol();

    if (state.do_not_emit_declarations)
    {
        file << sym.get_type().get_declaration(sym.get_scope(), sym.get_qualified_name());
    }
    else 
    {
        walk_type_for_symbols(sym.get_type(),
                /* needs def */ 1,
                &CXXBase::declare_symbol,
                &CXXBase::define_symbol,
                &CXXBase::define_all_entities_in_trees);

        set_codegen_status(sym, CODEGEN_STATUS_NONE);
        define_symbol(sym);
    }
}

CXXBase::Ret CXXBase::visit(const Nodecl::Offsetof& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ParenthesizedExpression& node)
{
    Nodecl::NodeclBase nest = node.get_nest();
    file << "(";
    walk(nest);
    file << ")";
}

CXXBase::Ret CXXBase::visit(const Nodecl::PointerToMember& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaClauseArg& node)
{
    file << node.get_text();
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomClause& node)
{
    Nodecl::NodeclBase arguments = node.get_arguments();

    file << node.get_text();

    if (!arguments.is_null())
    {
        file << "(";
        walk_list(arguments, ", ");
        file << ")";
    }
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomDeclaration& node)
{
    Nodecl::NodeclBase pragma_line = node.get_pragma_line();
    TL::Symbol symbol = node.get_symbol();

    indent();

    // FIXME  parallel|for must be printed as parallel for
    file << "/* decl: #pragma " << node.get_text();
    walk(pragma_line);
    file << "'" << symbol.get_qualified_name() << "' */\n";
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomDirective& node)
{
    Nodecl::NodeclBase pragma_line = node.get_pragma_line();

    indent();
    file << "#pragma " << node.get_text() << " ";
    walk(pragma_line);
    file << "\n";
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomLine& node)
{
    Nodecl::NodeclBase parameters = node.get_parameters();
    Nodecl::NodeclBase clauses = node.get_clauses();

    file << node.get_text();

    if (!parameters.is_null())
    {
        file << "(";
        walk_list(parameters, ", ");
        file << ")";
    }
    else
    {
        file << " ";
    }

    walk_list(clauses, " ");
}

CXXBase::Ret CXXBase::visit(const Nodecl::PragmaCustomStatement& node)
{
    Nodecl::NodeclBase pragma_line = node.get_pragma_line();
    Nodecl::NodeclBase statement = node.get_statement();

    indent();

    // FIXME  parallel|for must be printed as parallel for
    file << "#pragma %s " << node.get_text();
    walk(pragma_line);
    file << "\n";
    walk(statement);
}

CXXBase::Ret CXXBase::visit(const Nodecl::PseudoDestructorName& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Range& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::ReturnStatement& node)
{
    Nodecl::NodeclBase expression = node.get_value();

    indent();
    file << "return ";

    walk(expression);

    file << ";\n";
}

CXXBase::Ret CXXBase::visit(const Nodecl::Shaping& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::Sizeof& node)
{
}

std::string quote_c_string(int* c, int length, char is_wchar)
{
    std::string result;
    if (is_wchar)
    {
        result += "L";
    }

    result += "\"";

    int i;
    for (i = 0; i < length; i++)
    {
        int current = c[i];

        if (current == '\n')
        {
            result += "\\n";
        }
        else if (current ==  '\'')
        {
            result += "\\\'";
        }
        else if (current ==  '"')
        {
            result += "\\\"";
        }
        else if (current ==  '?')
        {
            result += "\\\?";
        }
        else if (current ==  '\\')
        {
            result += "\\\\";
        }
        else if (current ==  '\a')
        {
            result += "\\\a";
        }
        else if (current ==  '\b')
        {
            result += "\\\b";
        }
        else if (current ==  '\f')
        {
            result += "\\\f";
        }
        else if (current ==  '\n')
        {
            result += "\\\n";
        }
        else if (current ==  '\r')
        {
            result += "\\\r";
        }
        else if (current ==  '\t')
        {
            result += "\\\t";
        }
        else if (current ==  '\v')
        {
            result += "\\\v";
        }
        else if (isprint(current))
        {
            std::string str(1, (char)current);
            result += str;
        }
        // Best effort
        else
        {
            std::stringstream ss;
            if (!is_wchar
                    || (current < 255))
            {
                ss << "\\x" << std::hex << current << std::dec;
                result += ss.str();
            }
            else
            {
                ss << "\\U" << std::hex << current << std::dec;
                result += ss.str();
            }
        }
    }

    result += "\"";

    return result;
}

CXXBase::Ret CXXBase::visit(const Nodecl::StringLiteral& node)
{
    const_value_t* v = nodecl_get_constant(node.get_internal_nodecl());

    int *bytes = NULL;
    int length = 0;
    const_value_string_unpack(v, &bytes, &length);

    type_t* element_type = array_type_get_element_type(no_ref(nodecl_get_type(node.get_internal_nodecl())));
    char is_wchar = !is_unsigned_char_type(element_type)
        && !is_signed_char_type(element_type);

    file << quote_c_string(bytes, length, is_wchar);

    ::free(bytes);
}

CXXBase::Ret CXXBase::visit(const Nodecl::StructuredValue& node)
{
}

CXXBase::Ret CXXBase::visit(const Nodecl::SwitchStatement& node)
{
    Nodecl::NodeclBase expression = node.get_switch();
    Nodecl::NodeclBase statement = node.get_statement();

    indent();
    file << "switch (";
    Nodecl::NodeclBase old_condition_top = state.condition_top;
    int old_condition = state.in_condition;
    int old_indent = get_indent_level();

    set_indent_level(0);
    state.in_condition = 1;
    state.condition_top = expression;

    walk(expression);

    set_indent_level(old_indent);
    state.in_condition = old_condition;
    state.condition_top = old_condition_top;

    file << ")\n";

    inc_indent(2);
    walk(statement);
    dec_indent(2);
}

CXXBase::Ret CXXBase::visit(const Nodecl::Symbol& node)
{
    TL::Symbol entry = node.get_symbol();

    if (entry.is_member())
    {
        define_symbol(entry.get_class_type().get_symbol());
    }

    CXX_LANGUAGE()
    {
        if (!entry.is_template_parameter()
                && !entry.is_builtin())
        {
            file << entry.get_qualified_name(entry.get_scope());
        }
        else
        {
            file << entry.get_name();
        }
    }

    C_LANGUAGE()
    {
        file << entry.get_name();
    }
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
    Nodecl::NodeclBase statement = node.get_statement();
    Nodecl::NodeclBase catch_handlers = node.get_catch_handlers();
    Nodecl::NodeclBase any_catch_handler = node.get_any();
    indent();

    file << "try\n";

    walk(statement);
    walk(catch_handlers);

    if (!any_catch_handler.is_null())
    {
        indent();
        file << "catch (...)\n";
        walk(any_catch_handler);
    }
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
    Nodecl::NodeclBase condition = node.get_condition();
    Nodecl::NodeclBase statement = node.get_statement();

    indent();
    file << "while (";

    int old = state.in_condition;
    Nodecl::NodeclBase old_condition_top = state.condition_top;
    int old_indent = get_indent_level();
    set_indent_level(0);
    state.in_condition = 1;
    state.condition_top = condition;

    walk(condition);

    set_indent_level(old_indent);
    state.in_condition = old;
    state.condition_top = old_condition_top;
    file << ")\n";

    inc_indent();
    walk(statement);
    dec_indent();
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
