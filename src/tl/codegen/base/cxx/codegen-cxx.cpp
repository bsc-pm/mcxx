/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "codegen-cxx.hpp"
#include "tl-objectlist.hpp"
#include "tl-type.hpp"
#include "cxx-cexpr.h"
#include "string_utils.h"
#include <iomanip>
#ifdef HAVE_QUADMATH_H
MCXX_BEGIN_DECLS
#include <quadmath.h>
MCXX_END_DECLS
#endif

#include "cxx-printscope.h"

namespace Codegen {

std::string CxxBase::codegen(const Nodecl::NodeclBase &n) 
{
    if (n.is_null())
        return "";

    // Keep the state and reset it
    State old_state = state;
    state = State();

    decl_context_t decl_context = this->get_current_scope().get_decl_context();

    state.global_namespace = decl_context.global_scope->related_entry;
    state.opened_namespace = decl_context.namespace_scope->related_entry;

    state.emit_declarations = this->is_file_output() ? State::EMIT_ALL_DECLARATIONS : State::EMIT_NO_DECLARATIONS;

    std::string old_file = file.str();

    file.clear();
    file.str("");

    walk(n);

    // Make sure the starting namespace is closed
    codegen_move_namespace_from_to(state.opened_namespace, decl_context.namespace_scope->related_entry);
    
    std::string result = file.str();

    // Restore previous state
    state = old_state;
    file.str(old_file);
    file.seekp(0, std::ios_base::end);

    return result;
}

void CxxBase::push_scope(TL::Scope sc)
{
    _scope_stack.push_back(sc);
}

void CxxBase::pop_scope()
{
    _scope_stack.pop_back();
}

TL::Scope CxxBase::get_current_scope() const
{
    return _scope_stack.back();
}

#define OPERATOR_TABLE \
    PREFIX_UNARY_EXPRESSION(Plus, " +") \
    PREFIX_UNARY_EXPRESSION(Neg, " -") \
    PREFIX_UNARY_EXPRESSION(LogicalNot, "!") \
    PREFIX_UNARY_EXPRESSION(BitwiseNot, "~") \
    PREFIX_UNARY_EXPRESSION(Derreference, "*") \
    PREFIX_UNARY_EXPRESSION(Preincrement, "++") \
    PREFIX_UNARY_EXPRESSION(Predecrement, "--") \
    PREFIX_UNARY_EXPRESSION(Delete, "delete ") \
    PREFIX_UNARY_EXPRESSION(DeleteArray, "delete[] ") \
    PREFIX_UNARY_EXPRESSION(RealPart, "__real__ ") \
    PREFIX_UNARY_EXPRESSION(ImagPart, "__imag__ ") \
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
    void CxxBase::visit(const Nodecl::_name &node) \
    { \
        Nodecl::NodeclBase rhs = node.children()[0]; \
        char needs_parentheses = operand_has_lower_priority(node, rhs); \
        file << _operand; \
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

bool CxxBase::is_non_language_reference_type(TL::Type type)
{
    return ((IS_C_LANGUAGE && type.is_any_reference())
            || (IS_CXX_LANGUAGE && type.is_rebindable_reference()));
}

bool CxxBase::is_non_language_reference_variable(TL::Symbol sym)
{
    return is_non_language_reference_type(sym.get_type());
}

bool CxxBase::is_non_language_reference_variable(const Nodecl::NodeclBase &n)
{
    if (n.get_symbol().is_valid())
    {
        return is_non_language_reference_variable(n.get_symbol());
    }
    return false;
}

void CxxBase::visit(const Nodecl::Reference &node) 
{ 
    Nodecl::NodeclBase rhs = node.children()[0]; 
    char needs_parentheses = operand_has_lower_priority(node, rhs); 

    bool old_do_not_derref_rebindable_ref = state.do_not_derref_rebindable_reference;

    if (is_non_language_reference_variable(rhs))
    {
        state.do_not_derref_rebindable_reference = true;
    }
    else
    {
        file << "&"; 
    }

    if (needs_parentheses) 
    { 
        file << "("; 
    } 
    walk(rhs); 
    if (needs_parentheses) 
    { 
        file << ")"; 
    } 

    state.do_not_derref_rebindable_reference = old_do_not_derref_rebindable_ref;
}

#define POSTFIX_UNARY_EXPRESSION(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name& node) \
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
        file << _operand; \
    }

#define BINARY_EXPRESSION(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name& node) \
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
        needs_parentheses = operand_has_lower_priority(node, rhs) || same_operation(node, rhs); \
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
    void CxxBase::visit(const Nodecl::_name& node) \
    { \
        Nodecl::NodeclBase lhs = node.children()[0]; \
        Nodecl::NodeclBase rhs = node.children()[1]; \
        if (state.in_condition && state.condition_top == node) \
        { \
            file << "("; \
        } \
        char needs_parentheses = operand_has_lower_priority(node, lhs) || same_operation(node, lhs); \
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
            file << ")"; \
        } \
    }
OPERATOR_TABLE
#undef POSTFIX_UNARY_EXPRESSION
#undef PREFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION
#undef BINARY_EXPRESSION_ASSIG

CxxBase::Ret CxxBase::visit(const Nodecl::ArraySubscript& node)
{
    Nodecl::NodeclBase subscripted = node.get_subscripted();
    Nodecl::List subscript = node.get_subscripts().as<Nodecl::List>();

    if (operand_has_lower_priority(node, subscripted))
    {
        file << "(";
    }
    walk(subscripted);
    if (operand_has_lower_priority(node, subscripted))
    {
        file << ")";
    }

    // We keep a list instead of a single dimension for multidimensional arrays
    // alla Fortran
    for(Nodecl::List::iterator it = subscript.begin(); 
           it != subscript.end();
           it++)
    {
        file << "[";
        walk(*it);
        file << "]";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::BooleanLiteral& node)
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

CxxBase::Ret CxxBase::visit(const Nodecl::BreakStatement& node)
{
    indent();
    file << "break;\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::C99DesignatedInitializer& node)
{
    walk(node.get_designation());
    file << " = ";
    walk(node.get_init());
}

CxxBase::Ret CxxBase::visit(const Nodecl::C99FieldDesignator& node)
{
    file << ".";
    walk(node.get_name());
}

CxxBase::Ret CxxBase::visit(const Nodecl::C99IndexDesignator& node)
{
    file << "[";
    walk(node.get_expr());
    file << "]";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CaseStatement& node)
{
    Nodecl::NodeclBase expression = node.get_case();
    Nodecl::NodeclBase statement = node.get_statement();

    indent();
    file << "case ";
    walk(expression);
    file << " :\n";

    walk(statement);
}

CxxBase::Ret CxxBase::visit(const Nodecl::Cast& node)
{
    std::string cast_kind = node.get_text();
    TL::Type t = node.get_type();
    Nodecl::NodeclBase nest = node.get_rhs();

    if (IS_C_LANGUAGE
            || cast_kind == "C")
    {
        file << "(" << this->get_declaration(t, this->get_current_scope(),  "") << ")";
        char needs_parentheses = operand_has_lower_priority(node, nest);
        if (needs_parentheses)
        {
            file << "(";
        }
        walk(nest);
        if (needs_parentheses)
        {
            file << ")";
        }
    }
    else
    {
        std::string decl = this->get_declaration(t, this->get_current_scope(),  "");
        if (decl[0] == ':')
        {
            decl = " " + decl;
        }

        file << cast_kind << "<" << decl << ">(";
        walk(nest);
        file << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CatchHandler& node)
{
    Nodecl::NodeclBase name = node.get_name();
    Nodecl::NodeclBase statement = node.get_statement();
    TL::Type type = node.get_type();

    indent();
    file << "catch (";

    if (name.is_null())
    {
        // FIXME: Is this always safe?
        file << this->get_declaration(type, this->get_current_scope(),  "");
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

CxxBase::Ret CxxBase::visit(const Nodecl::ClassMemberAccess& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_member();

    TL::Symbol sym = rhs.get_symbol();

    bool is_anonymous = sym.is_valid()
        && sym.get_type().is_named_class()
        && sym.get_type().get_symbol().is_anonymous_union();

    bool must_derref_all = (sym.is_valid()
            && is_non_language_reference_variable(sym)
            && !sym.get_type().references_to().is_array()
            && !state.do_not_derref_rebindable_reference);

    bool old_do_not_derref_rebindable_ref = state.do_not_derref_rebindable_reference;

    if (must_derref_all)
    {
        file << "(*";
    }

    char needs_parentheses = operand_has_lower_priority(node, lhs);
    if (needs_parentheses)
    { 
        file << "(";
    } 
    // Left hand side does not care about the top level reference status
    state.do_not_derref_rebindable_reference = false;
    walk(lhs);
    if (needs_parentheses)
    { 
        file << ")";
    } 
    // Do not print anonymous symbols
    if (!is_anonymous)
    {
        // Right part can be a reference but we do not want to derref it
        state.do_not_derref_rebindable_reference = true;

        file << ".";
        needs_parentheses = operand_has_lower_priority(node, rhs); 
        if (needs_parentheses) 
        { 
            file << "(";
        } 

        walk(rhs);

        if (needs_parentheses) 
        { 
            file << ")";
        } 
    }

    if (must_derref_all)
    {
        file << ")";
    }
    state.do_not_derref_rebindable_reference = old_do_not_derref_rebindable_ref;
}

CxxBase::Ret CxxBase::visit(const Nodecl::ComplexLiteral& node)
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

CxxBase::Ret CxxBase::visit(const Nodecl::CompoundExpression& node)
{
    file << " (";

    Nodecl::Context context = node.get_nest().as<Nodecl::Context>();
    Nodecl::List statements = context.get_in_context().as<Nodecl::List>();

    this->push_scope(context.retrieve_context());

    file << "{\n";
    inc_indent();

    State::EmitDeclarations emit_declarations = state.emit_declarations;
    if (state.emit_declarations == State::EMIT_NO_DECLARATIONS)
        state.emit_declarations = State::EMIT_CURRENT_SCOPE_DECLARATIONS;

    bool in_condition = state.in_condition;
    state.in_condition = 0;

    ERROR_CONDITION(statements.size() != 1, "In C/C++ blocks only have one statement", 0);
    ERROR_CONDITION(!statements[0].is<Nodecl::CompoundStatement>(), "Invalid statement", 0);

    Nodecl::NodeclBase statement_seq = statements[0].as<Nodecl::CompoundStatement>().get_statements();

    define_local_entities_in_trees(statement_seq);
    walk(statement_seq);

    state.in_condition = in_condition;
    state.emit_declarations = emit_declarations;
    dec_indent();

    indent();
    file << "}";

    this->pop_scope();

    file << ") ";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CompoundStatement& node)
{
    indent();
    file << "{\n";
    inc_indent();

    State::EmitDeclarations emit_declarations = state.emit_declarations;
    if (state.emit_declarations == State::EMIT_NO_DECLARATIONS)
        state.emit_declarations = State::EMIT_CURRENT_SCOPE_DECLARATIONS;

    Nodecl::NodeclBase statement_seq = node.get_statements();

    define_local_entities_in_trees(statement_seq);

    walk(statement_seq);

    state.emit_declarations = emit_declarations;

    dec_indent();
    indent();
    file << "}\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::ConditionalExpression& node)
{
    Nodecl::NodeclBase cond = node.get_condition();
    Nodecl::NodeclBase then = node.get_true();
    Nodecl::NodeclBase _else = node.get_false();

    if (operand_has_lower_priority(node, cond))
    {
        file << "(";
    }
    walk(cond);
    if (operand_has_lower_priority(node, cond))
    {
        file << ")";
    }

    file << " ? ";

    if (operand_has_lower_priority(node, then))
    {
        file << "(";
    }
    walk(then);
    if (operand_has_lower_priority(node, then))
    {
        file << ")";
    }

    file << " : ";

    if (operand_has_lower_priority(node, _else))
    {
        file << "(";
    }
    walk(_else);
    if (operand_has_lower_priority(node, _else))
    {
        file << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::Context& node)
{
    this->push_scope(node.retrieve_context());

    walk(node.get_in_context());

    this->pop_scope();
}

CxxBase::Ret CxxBase::visit(const Nodecl::ContinueStatement& node)
{
    indent();
    file << "continue;\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Conversion& node)
{
    // Do nothing
    walk(node.get_nest());
}

CxxBase::Ret CxxBase::visit(const Nodecl::SavedExpr& node)
{
    // Do nothing
    walk(node.get_expression());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxArrow& node)
{
    walk(node.get_lhs());
    file << "->";
    walk(node.get_member());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxBracedInitializer& node)
{
    file << "{";
    if (!node.get_init().is_null())
    {
        walk(node.get_init());
    }
    file << "}";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepGlobalNameNested& node)
{
    file << "::";
    visit(node.as<Nodecl::CxxDepNameNested>());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepNameConversion& node)
{
    file << "operator " << this->get_declaration(node.get_type(), this->get_current_scope(), "");
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepNameNested& node)
{
    walk_list(node.get_items().as<Nodecl::List>(), "::");
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepNameSimple& node)
{
    file << node.get_text();
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepTemplateId& node)
{
    file << node.get_text();

    walk(node.get_name());
    TL::TemplateParameters tpl(nodecl_get_template_parameters(node.get_internal_nodecl()));

    file << ::template_arguments_to_str(
            tpl.get_internal_template_parameter_list(),
            this->get_current_scope().get_decl_context());

    // The function 'template_arguments_to_str' does not print anything when
    // template arguments are empty. For this reason, we add the empty list '<>'
    if (tpl.get_num_parameters() == 0)
    {
        file << "<>";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxEqualInitializer& node)
{
    file << " = ";
    walk(node.get_init());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxMemberInit& node)
{
    TL::Symbol function = this->get_current_scope().get_decl_context().current_scope->related_entry;

    walk(node.get_name());
    walk(node.get_initializer());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxExplicitTypeCast& node)
{
    TL::Type t = node.get_type();

    file << this->get_declaration(t, this->get_current_scope(),  "");

    walk(node.get_init_list());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxParenthesizedInitializer& node)
{
    file << "(";
    if (!node.get_init().is_null())
    {
        walk_expression_list(node.get_init().as<Nodecl::List>());
    }
    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxSizeof& node)
{
    file << "sizeof(";
    walk(node.get_expr());
    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::DefaultStatement& node)
{
    Nodecl::NodeclBase statement = node.get_statement();

    indent();
    file << "default :\n";

    walk(statement);
}

CxxBase::Ret CxxBase::visit(const Nodecl::DoStatement& node)
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

CxxBase::Ret CxxBase::visit(const Nodecl::EmptyStatement& node)
{
    indent();
    file << ";\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::ErrExpr& node)
{
    if (!this->is_file_output())
    {
        file << "<<error expression>>";
    }
    else
    {
        internal_error("%s: error: <<error expression>> found when the output is a file", 
                node.get_locus().c_str());
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::ExpressionStatement& node)
{
    Nodecl::NodeclBase expression = node.get_nest();
    indent();
    walk(expression);
    file << ";\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::FieldDesignator& node)
{
    Nodecl::NodeclBase field = node.get_field();
    Nodecl::NodeclBase next = node.get_next();

    if (IS_CXX_LANGUAGE)
    {
        file << " /* ";
    }

    file << ".";
    walk(field);

    if (!next.is<Nodecl::FieldDesignator>()
            && !next.is<Nodecl::IndexDesignator>())
    {
        file << " = ";
    }

    if (IS_CXX_LANGUAGE)
    {
        file << " */ ";
    }

    walk(next);
}

CxxBase::Ret CxxBase::visit(const Nodecl::FloatingLiteral& node)
{
    const_value_t* value = nodecl_get_constant(node.get_internal_nodecl());
    ERROR_CONDITION(value == NULL, "Invalid value", 0);

    type_t* t = nodecl_get_type(node.get_internal_nodecl());
    int precision = floating_type_get_info(t)->p + 1;


    // FIXME - We are relying on a (really handy) GNU extension
    if (const_value_is_float(value))
    {
        const char* floating = NULL;
        uniquestr_sprintf(&floating, "%.*ef", precision, const_value_cast_to_float(value));
        file << floating;
    }
    else if (const_value_is_double(value))
    {
        const char* floating = NULL;
        uniquestr_sprintf(&floating, "%.*e", precision, const_value_cast_to_double(value));
        file << floating;
    }
    else if (const_value_is_long_double(value))
    {
        const char* floating = NULL;
        uniquestr_sprintf(&floating, "%.*LeL", precision, const_value_cast_to_long_double(value));
        file << floating;
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

void CxxBase::emit_range_loop_header(
        Nodecl::RangeLoopControl lc,
        Nodecl::NodeclBase statement,
        const std::string& rel_op)
{

    TL::Symbol ind_var = lc.get_symbol();

    indent();
    file << "for (";

    // I = L
    file << ind_var.get_qualified_name();
    file << " = ";
    walk(lc.get_lower());
    file << "; ";

    // I <= L     (or   I >= L)
    file << ind_var.get_qualified_name() << rel_op;
    walk(lc.get_upper());
    file << "; ";

    // I += S
    file << ind_var.get_qualified_name() << " += ";
    walk(lc.get_step());

    file << ")\n";

    inc_indent();
    walk(statement);
    dec_indent();
}

CxxBase::Ret CxxBase::visit(const Nodecl::ForStatement& node)
{
    Nodecl::NodeclBase loop_control = node.get_loop_header();
    Nodecl::NodeclBase statement = node.get_statement();

    if (loop_control.is<Nodecl::LoopControl>())
    {
        indent();
        file << "for (";
        walk(loop_control);
        file << ")\n";

        inc_indent();
        walk(statement);
        dec_indent();
    }
    else if (loop_control.is<Nodecl::UnboundedLoopControl>())
    {
        // This only happens for DO without loop-control
        indent();
        file << "for (;;)\n";
        inc_indent();
        walk(statement);
        dec_indent();
    }
    else if (loop_control.is<Nodecl::RangeLoopControl>())
    {
        Nodecl::RangeLoopControl lc = loop_control.as<Nodecl::RangeLoopControl>();

        Nodecl::NodeclBase lower = lc.get_lower();
        Nodecl::NodeclBase upper = lc.get_upper();
        Nodecl::NodeclBase step = lc.get_step();

        if (!step.is_null()
                && step.is_constant())
        {
            std::string rel_op = " <= ";
            bool is_positive = 0;
            const_value_t* v = step.get_constant();
            if (const_value_is_negative(v))
            {
                rel_op = " >= ";
            }

            emit_range_loop_header(lc, statement, rel_op);
        }
        else
        {
            indent();
            file << "if (";
            walk(step);
            file << "> 0)\n;";

            inc_indent();
            indent();
            file << "{\n";

            inc_indent();
            emit_range_loop_header(lc, statement, " <= ");
            dec_indent();

            indent();
            file << "}\n";
            dec_indent();

            indent();
            file << "else\n";

            inc_indent();
            indent();
            file << "{\n";

            inc_indent();
            emit_range_loop_header(lc, statement, " >= ");
            dec_indent();

            indent();
            file << "}\n";
            dec_indent();
        }

    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

template <typename Iterator>
CxxBase::Ret CxxBase::codegen_function_call_arguments(Iterator begin, Iterator end, TL::Type function_type, int ignore_n_first)
{
    bool has_ellipsis = 0;
    TL::ObjectList<TL::Type> parameters_type = function_type.parameters(has_ellipsis);
    TL::ObjectList<TL::Type>::iterator type_it = parameters_type.begin();
    TL::ObjectList<TL::Type>::iterator type_end = parameters_type.end();


    Iterator arg_it = begin;
    while (arg_it != end 
            && ignore_n_first > 0)
    {
        arg_it++;
        if (type_it != type_end)
            type_it++;

        ignore_n_first--;
    }

    // Update begin if we have ignored any argument
    begin = arg_it;
    while (arg_it != end)
    {
        if (arg_it != begin)
        {
            file << ", ";
        }

        Nodecl::NodeclBase actual_arg = *arg_it;

        bool old_do_not_derref_rebindable_ref = state.do_not_derref_rebindable_reference;
        state.do_not_derref_rebindable_reference = false;

        bool do_reference = false;

        if (type_it != type_end
                && type_it->is_valid())
        {
            bool param_is_ref = is_non_language_reference_type(*type_it);

            bool arg_is_ref = is_non_language_reference_variable(*arg_it);

            if (param_is_ref && !arg_is_ref)
            {
                if (arg_it->template is<Nodecl::Derreference>())
                {
                    actual_arg = arg_it->template as<Nodecl::Derreference>().get_rhs();
                }
                else
                {
                    do_reference = true;
                }
            }
            else if (!param_is_ref && arg_is_ref)
            {
                // Do nothing
            }
            else if (param_is_ref && arg_is_ref)
            {
                state.do_not_derref_rebindable_reference = true;
            }
            else if (!param_is_ref && !arg_is_ref)
            {
                // Do nothing
            }
        }

        if (do_reference)
        {
            file << "&(";
        }
        walk(actual_arg);
        if (do_reference)
        {
            file << ")";
        }

        if (type_it != type_end)
            type_it++;

        arg_it++;

        state.do_not_derref_rebindable_reference = old_do_not_derref_rebindable_ref;
    }
}

template <typename Node>
CxxBase::Ret CxxBase::visit_function_call(const Node& node, bool is_virtual_call)
{
    Nodecl::NodeclBase called_entity = node.get_called();
    Nodecl::List arguments = node.get_arguments().template as<Nodecl::List>();

    enum call_kind
    {
        INVALID_CALL = 0,
        ORDINARY_CALL,
        NONSTATIC_MEMBER_CALL,
        STATIC_MEMBER_CALL,
        CONSTRUCTOR_INITIALIZATION
    } kind = ORDINARY_CALL;

    TL::Symbol called_symbol = called_entity.get_symbol();
    if (called_symbol.is_valid())
    {
        if (called_symbol.is_function()
                && called_symbol.is_member())
        {
            if (called_symbol.is_constructor())
            {
                kind = CONSTRUCTOR_INITIALIZATION;
            }
            else if (!called_symbol.is_static())
            {
                kind = NONSTATIC_MEMBER_CALL;
            }
            else // if (called_symbol.is_static())
            {
                kind = STATIC_MEMBER_CALL;
            }
        }
    }

    TL::Type function_type(NULL);

    if (called_entity.is<Nodecl::Symbol>())
    {
        function_type = called_entity.as<Nodecl::Symbol>().get_symbol().get_type();
    }
    else
    {
        function_type = called_entity.get_type();
    }

    if (function_type.is_any_reference())
        function_type = function_type.references_to();

    if (function_type.is_pointer())
        function_type = function_type.points_to();
    
    // There are a lot of cases!
    if (!function_type.is_function())
    {
        walk(called_entity);
        file << "(";
        walk_expression_list(arguments);
        file << ")";
        return;
    }
    
    // Currently, this never happens
    ERROR_CONDITION(!function_type.is_function(), "Expecting a function type", 0);

    bool is_non_language_ref = is_non_language_reference_type(function_type.returns());
    if (is_non_language_ref)
    {
        file << "(*(";
    }

    switch (kind)
    {
        case ORDINARY_CALL:
        case STATIC_MEMBER_CALL:
            {
                char needs_parentheses = operand_has_lower_priority(node, called_entity);
                if (needs_parentheses) 
                {
                    file << "(";
                }
                walk(called_entity);
                if (needs_parentheses)
                {
                    file << ")";
                }
                file << "(";

                codegen_function_call_arguments(arguments.begin(), arguments.end(), function_type, /* ignore_n_first */ 0);

                file << ")";
                break;
            }
        case NONSTATIC_MEMBER_CALL:
            {
                ERROR_CONDITION(!(arguments.size() >= 1), "A nonstatic member call lacks the implicit argument", 0);

                char needs_parentheses = (get_rank(arguments[0])
                        < get_rank_kind(NODECL_CLASS_MEMBER_ACCESS, ""));

                if (needs_parentheses)
                {
                    file << "(";
                }
                walk(arguments[0]);
                if (needs_parentheses)
                {
                    file << ")";
                }
                file << ".";

                if (is_virtual_call)
                {
                    ERROR_CONDITION(!called_symbol.is_valid(), "Virtual call lacks called symbol", 0);
                    file << unmangle_symbol_name(called_symbol);
                }
                else
                {
                    walk(called_entity);
                }

                file << "(";

                codegen_function_call_arguments(arguments.begin(), arguments.end(), function_type, /* ignore_n_first */ 1);

                file << ")";
                break;
            }
        case CONSTRUCTOR_INITIALIZATION:
            {
                TL::Symbol class_symbol = called_symbol.get_class_type().get_symbol();
                file << class_symbol.get_qualified_name();
                file << "(";

                codegen_function_call_arguments(arguments.begin(), arguments.end(), function_type, /* ignore_n_first */ 0);

                file << ")";
                break;
            }
        default:
            {
                internal_error("Unhandled function call kind", 0);
            }
    }

    if (is_non_language_ref)
    {
        file << "))";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::FunctionCall& node)
{
    visit_function_call(node, /* is_virtual_call */ false);
}

CxxBase::Ret CxxBase::visit(const Nodecl::TemplateFunctionCode& node)
{
    // We are going to generate dependent code
    state.in_dependent_template_function_code = true;

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
    TL::Type symbol_type = symbol.get_type();
    TL::Scope function_scope = context.retrieve_context();

    ERROR_CONDITION(!symbol.is_function(), "Invalid symbol", 0);

    if (symbol.is_member())
    {
        TL::Symbol class_symbol = symbol.get_class_type().get_symbol();
        this->define_symbol_always(class_symbol);
    }
    else
    {
        if (symbol_type.is_template_specialized_type()
                && symbol_type.template_specialized_type_get_template_arguments().get_num_parameters() != 0)
        {
            TL::Type template_type = symbol_type.get_related_template_type();
            TL::Type primary_type = template_type.get_primary_template();
            TL::Symbol primary_symbol = primary_type.get_symbol();

            declare_symbol_always(primary_symbol);
        }
    }

    walk_type_for_symbols(symbol_type.returns(),
            /* needs_def */ true,
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always,
            &CxxBase::define_nonlocal_entities_in_trees);

    state.current_symbol = symbol;

    bool has_ellipsis = false;
    TL::ObjectList<TL::Type> parameter_list = symbol_type.parameters(has_ellipsis);
    int num_parameters = parameter_list.size();

    for (TL::ObjectList<TL::Type>::iterator it = parameter_list.begin();
            it != parameter_list.end();
            it++)
    {
        walk_type_for_symbols(*it, /* needs_def */ 1,
                &CxxBase::declare_symbol_always,
                &CxxBase::define_symbol_always,
                &CxxBase::define_nonlocal_entities_in_trees);
    }

    define_nonlocal_entities_in_trees(statement);

    TL::ObjectList<TL::Symbol> related_symbols = symbol.get_related_symbols();
    TL::ObjectList<std::string> parameter_names(related_symbols.size());
    TL::ObjectList<std::string> parameter_attributes(related_symbols.size());
    int i = 0;
    for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
            it != related_symbols.end();
            it++, i++)
    {
        TL::Symbol current_param = *it;
        if (current_param.is_valid())
        {
            parameter_names[i] = current_param.get_name();
            set_codegen_status(current_param, CODEGEN_STATUS_DEFINED);
            if (current_param.has_gcc_attributes())
            {
                parameter_attributes[i] = gcc_attributes_to_str(current_param); 
            }
        }
    }

    std::string decl_spec_seq;

    if (symbol.is_static()
            && !symbol.is_member())
    {
        decl_spec_seq += "static ";
    }
    if (symbol.is_extern() && symbol.get_initialization().is_null())
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

    std::string gcc_attributes = "";
    
    if (symbol.has_gcc_attributes())
    {
        gcc_attributes = gcc_attributes_to_str(symbol) + " ";
    }

    std::string asm_specification = gcc_asm_specifier_to_str(symbol);

    std::string qualified_name = symbol.get_class_qualification(function_scope, /* without_template */ true);

    //if (symbol_type.is_template_specialized_type()
    //        && !symbol.is_conversion_function())
    //{
    //    qualified_name += template_arguments_to_str(symbol);
    //}

    TL::Type real_type = symbol_type.advance_over_typedefs();

    if (symbol.is_conversion_function()
            || symbol.is_destructor())
    {
        // FIXME - Use TL::Type to build this type
        real_type = ::get_new_function_type(NULL, NULL, 0);
    }

    std::string declarator;
    declarator = this->get_declaration_with_parameters(real_type, function_scope, qualified_name, parameter_names, parameter_attributes);

    std::string exception_spec = exception_specifier_to_str(symbol);

    move_to_namespace_of_symbol(symbol);

    TL::TemplateParameters template_parameters = function_scope.get_template_parameters();
    codegen_template_headers_all_levels(template_parameters);
    
    bool requires_extern_linkage = false;
    CXX_LANGUAGE()
    {
        requires_extern_linkage = (!symbol.is_member() 
                && symbol.has_nondefault_linkage());

        if (requires_extern_linkage)
        {
            file << "extern " + symbol.get_linkage() + "\n";
            indent();
            file << "{\n";

            inc_indent();
        }
    }

    if (!symbol.is_member()
            && asm_specification != "")
    {
        // gcc does not like asm specifications appear in the
        // function-definition so emit a declaration before the definition
        indent();
        file << decl_spec_seq << gcc_attributes << declarator << exception_spec << asm_specification << ";\n";
    }

    indent();
    file << decl_spec_seq << gcc_attributes << declarator << exception_spec << "\n";

    set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);

    if (!initializers.is_null())
    {
        push_scope(symbol.get_scope());
        inc_indent();

        indent();
        file << ": ";

        walk_list(initializers.as<Nodecl::List>(), ", ");

        dec_indent();

        file << "\n";
        pop_scope();
    }

    this->walk(context);

    CXX_LANGUAGE()
    {
        if (requires_extern_linkage)
        {
            dec_indent();
            indent();
            file << "}\n";
        }
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::FunctionCode& node)
{
    //Only independent code
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
    TL::Type symbol_type = symbol.get_type();

    TL::Scope symbol_scope = symbol.get_scope();
    
    ERROR_CONDITION(!symbol.is_function(), "Invalid symbol", 0);
    
    bool is_template_specialized = symbol_type.is_template_specialized_type();
    if (symbol.is_member())
    {
        TL::Symbol class_symbol = symbol.get_class_type().get_symbol();
        this->define_symbol_always(class_symbol);
    }
    else
    {
        if (is_template_specialized
                && symbol_type.template_specialized_type_get_template_arguments().get_num_parameters() != 0)
        {
            TL::Type template_type = symbol_type.get_related_template_type();
            TL::Type primary_type = template_type.get_primary_template();
            TL::Symbol primary_symbol = primary_type.get_symbol();
            declare_symbol_always(primary_symbol);
        }
    }

    walk_type_for_symbols(symbol_type.returns(), 
            /* needs_def */ true,
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always,
            &CxxBase::define_nonlocal_entities_in_trees);

    state.current_symbol = symbol;

    bool has_ellipsis = false;
    TL::ObjectList<TL::Type> parameter_list = symbol_type.parameters(has_ellipsis);
    int num_parameters = parameter_list.size();

    for (TL::ObjectList<TL::Type>::iterator it = parameter_list.begin();
            it != parameter_list.end();
            it++)
    {
        walk_type_for_symbols(*it, /* needs_def */ 1, 
                &CxxBase::declare_symbol_always,
                &CxxBase::define_symbol_always,
                &CxxBase::define_nonlocal_entities_in_trees);
    }

    define_nonlocal_entities_in_trees(statement);

    TL::ObjectList<TL::Symbol> related_symbols = symbol.get_related_symbols();
    TL::ObjectList<std::string> parameter_names(related_symbols.size());
    TL::ObjectList<std::string> parameter_attributes(related_symbols.size());
    int i = 0;
    for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
            it != related_symbols.end();
            it++, i++)
    {
        TL::Symbol current_param = *it;
        if (current_param.is_valid())
        {
            parameter_names[i] = current_param.get_name();
            set_codegen_status(current_param, CODEGEN_STATUS_DEFINED);
            if (current_param.has_gcc_attributes())
            {
                parameter_attributes[i] = gcc_attributes_to_str(current_param); 
            }
        }
    }

    std::string decl_spec_seq;

    if (symbol.is_static()
            && !symbol.is_member())
    {
        decl_spec_seq += "static ";
    }
    if (symbol.is_extern() && symbol.get_initialization().is_null())
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

    std::string gcc_attributes = "";
    
    if (symbol.has_gcc_attributes())
    {
        gcc_attributes = gcc_attributes_to_str(symbol) + " ";
    }

    std::string asm_specification = gcc_asm_specifier_to_str(symbol);

    std::string qualified_name = symbol.get_class_qualification(symbol_scope, /* without_template */ true);

    if (is_template_specialized
            && !symbol.is_conversion_function())
    {
        qualified_name += template_arguments_to_str(symbol);
    }

    TL::Type real_type = symbol_type.advance_over_typedefs();

    if (symbol.is_conversion_function()
            || symbol.is_destructor())
    {
        // FIXME - Use TL::Type to build this type
        real_type = ::get_new_function_type(NULL, NULL, 0);
    }

    std::string declarator = this->get_declaration_with_parameters(
            real_type, symbol_scope, qualified_name, parameter_names, parameter_attributes);

    std::string exception_spec = exception_specifier_to_str(symbol);

    move_to_namespace_of_symbol(symbol);

    // We may need zero or more empty template headers
    TL::TemplateParameters tpl = symbol_scope.get_template_parameters();
    while (tpl.is_valid())
    {
        // We should ignore some 'fake' empty template headers
        if (tpl.get_num_parameters() > 0 || tpl.get_is_explicit_specialization())
        {
             indent();
             file << "template <>\n";
        }
        tpl = tpl.get_enclosing_parameters();
    }

    bool requires_extern_linkage = false;
    CXX_LANGUAGE()
    {
        requires_extern_linkage = (!symbol.is_member()
                && symbol.has_nondefault_linkage());

        if (requires_extern_linkage)
        {
            file << "extern " + symbol.get_linkage() + "\n";
            indent();
            file << "{\n";

            inc_indent();
        }
    }

    if (!symbol.is_member()
            && asm_specification != "")
    {
        // gcc does not like asm specifications appear in the
        // function-definition so emit a declaration before the definition
        indent();
        file << decl_spec_seq << gcc_attributes << declarator << exception_spec << asm_specification << ";\n";
    }

    indent();
    file << decl_spec_seq << gcc_attributes << declarator << exception_spec << "\n";

    set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);

    if (!initializers.is_null())
    {
        push_scope(symbol.get_scope());
        inc_indent();

        indent();
        file << ": ";

        walk_list(initializers.as<Nodecl::List>(), ", ");

        dec_indent();

        file << "\n";
        pop_scope();
    }

    this->walk(context);

    CXX_LANGUAGE()
    {
        if (requires_extern_linkage)
        {
            dec_indent();
            indent();
            file << "}\n";
        }
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::GotoStatement& node)
{
    TL::Symbol label_sym = node.get_symbol();

    indent();
    file << "goto " << label_sym.get_name() << ";\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::IfElseStatement& node)
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

CxxBase::Ret CxxBase::visit(const Nodecl::IndexDesignator& node)
{
    Nodecl::NodeclBase _index = node.get_index();
    Nodecl::NodeclBase next = node.get_next();

    if (IS_CXX_LANGUAGE)
    {
        file << " /* ";
    }

    file << "[";
    walk(_index);
    file << "]";


    if (!next.is<Nodecl::FieldDesignator>()
            && !next.is<Nodecl::IndexDesignator>())
    {
        file << " = ";
    }
    if (IS_CXX_LANGUAGE)
    {
        file << " */ ";
    }

    walk(next);
}

CxxBase::Ret CxxBase::visit(const Nodecl::IntegerLiteral& node)
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
                                 file << "'" << (signed char) b << "'";
                             }
                             else
                             {
                                 file << "'" << (unsigned char) b << "'";
                             }
                         }
                         else
                         {
                             file << "'\\" 
                                 << std::oct << std::setfill('0') << std::setw(3) 
                                 << (unsigned int)b 
                                 << std::dec << std::setw(0) << "'";
                         }
                     }
        }
    }
    else if (IS_CXX_LANGUAGE && t.is_wchar_t())
    {
        unsigned int mb = const_value_cast_to_4(value);
        file << "L'\\x" 
            << std::hex << std::setfill('0') << std::setw(4) 
            << mb 
            << std::dec << std::setw(0) << "'";
    }
    else 
    {
        std::string complex_prefix;
        if (const_value_is_complex(value))
        {
            // In C complex constants only have imag part
            value = const_value_complex_get_imag_part(value);
        }

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

CxxBase::Ret CxxBase::visit(const Nodecl::LabeledStatement& node)
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

CxxBase::Ret CxxBase::visit(const Nodecl::LoopControl& node)
{
    Nodecl::NodeclBase init = node.get_init();
    Nodecl::NodeclBase cond = node.get_cond();
    Nodecl::NodeclBase next = node.get_next();

    // No condition top as "for((i=0); ...)" looks unnecessary ugly
    int old = state.in_condition;
    state.in_condition = 1;

    walk(init);
    file << "; ";

    Nodecl::NodeclBase old_condition_top = state.condition_top;
    state.condition_top = cond;

    // But it is desirable for the condition in "for( ... ; (i = x) ; ...)"
    walk(cond);
    file << "; ";

    state.condition_top = old_condition_top;

    // Here we do not care about parentheses "for ( ... ; ... ; i = i + 1)"
    walk(next);
    state.in_condition = old;
}

CxxBase::Ret CxxBase::visit(const Nodecl::MemberInit& node)
{
    TL::Symbol entry = node.get_symbol();
    Nodecl::NodeclBase init_expr = node.get_init_expr();

    file << entry.get_name() << "(";

    if (nodecl_calls_to_constructor(init_expr, entry.get_type()))
    {
        // Ignore top level constructor
        walk_expression_list(init_expr.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>());
    }
    else if (init_expr.is<Nodecl::StructuredValue>())
    {
        walk_expression_list(init_expr.as<Nodecl::StructuredValue>().get_items().as<Nodecl::List>());
    }
    else
    {
        walk(init_expr);
    }

    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::New& node)
{
    Nodecl::NodeclBase structured_value = node.get_init();
    ERROR_CONDITION(structured_value.is_null(), "New lacks structured value", 0);

    Nodecl::NodeclBase placement = node.get_placement();
    // Nodecl::NodeclBase operator_new = nodecl_get_child(node, 2);
    
    if (node.get_text() == "global")
        file << "::";

    file << "new ";

    if (!placement.is_null())
    {
        file << "(";
        walk_expression_list(placement.as<Nodecl::List>());
        file << ")";
    }

    TL::Type t = structured_value.get_type();

    if (!t.is_array())
    {
        walk(structured_value);
    }
    else
    {
        // new[] cannot have an initializer, so just print the type
        file << this->get_declaration(t, this->get_current_scope(),  "");
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::ObjectInit& node)
{
    TL::Symbol sym = node.get_symbol();

    walk_type_for_symbols(sym.get_type(),
            /* needs def */ 1,
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always,
            &CxxBase::define_all_entities_in_trees);

    state.must_be_object_init.erase(sym);
    define_symbol_always(sym);
}

CxxBase::Ret CxxBase::visit(const Nodecl::Offsetof& node)
{
    file << "__builtin_offsetof(";

    walk(node.get_offset_type());

    file << ", ";

    // Except for the first, the remaining must be printed as usual
    Nodecl::List designator = node.get_designator().as<Nodecl::List>();

    for (Nodecl::List::iterator it = designator.begin();
            it != designator.end();
            it++)
    {
        if (it == designator.begin())
        {
            ERROR_CONDITION(!it->is<Nodecl::C99FieldDesignator>(), "Invalid node", 0);

            walk(it->as<Nodecl::C99FieldDesignator>().get_name());
        }
        else
        {
            walk(*it);
        }
    }

    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::ParenthesizedExpression& node)
{
    Nodecl::NodeclBase nest = node.get_nest();
    file << "(";
    walk(nest);
    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::PointerToMember& node)
{
    TL::Symbol symbol = node.get_symbol();

    file << "&" << symbol.get_qualified_name();
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaClauseArg& node)
{
    file << node.get_text();
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomClause& node)
{
    Nodecl::NodeclBase arguments = node.get_arguments();

    file << node.get_text();

    if (!arguments.is_null())
    {
        file << "(";
        walk_list(arguments.as<Nodecl::List>(), ", ");
        file << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomDeclaration& node)
{
    Nodecl::NodeclBase pragma_line = node.get_pragma_line();
    TL::Symbol symbol = node.get_symbol();

    indent();

    // FIXME  parallel|for must be printed as parallel for
    file << "/* decl: #pragma " << node.get_text() << " ";
    walk(pragma_line);
    file << "'" << symbol.get_qualified_name() << "' */\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomDirective& node)
{
    Nodecl::NodeclBase pragma_line = node.get_pragma_line();

    indent();
    file << "#pragma " << node.get_text() << " ";
    walk(pragma_line);
    file << "\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomLine& node)
{
    Nodecl::NodeclBase parameters = node.get_parameters();
    Nodecl::NodeclBase clauses = node.get_clauses();

    file << node.get_text();

    if (!parameters.is_null())
    {
        file << "(";
        walk_list(parameters.as<Nodecl::List>(), ", ");
        file << ")";
    }
    else
    {
        file << " ";
    }

    walk_list(clauses.as<Nodecl::List>(), " ");
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomStatement& node)
{
    Nodecl::NodeclBase pragma_line = node.get_pragma_line();
    Nodecl::NodeclBase statement = node.get_statements();

    indent();
    // FIXME  parallel|for must be printed as parallel for
    file << "#pragma " << node.get_text() << " ";
    walk(pragma_line);
    file << "\n";
    walk(statement);
}

CxxBase::Ret CxxBase::visit(const Nodecl::PseudoDestructorName& node)
{
    Nodecl::NodeclBase lhs = node.get_accessed();
    Nodecl::NodeclBase rhs = node.get_destructor_name();

    char needs_parentheses = operand_has_lower_priority(node, lhs); 
    if (needs_parentheses) 
    { 
        file << "(";
    } 
    walk(lhs); 
    if (needs_parentheses) 
    { 
        file << ")";
    } 
    file << ".";
    walk(rhs); 
}

CxxBase::Ret CxxBase::visit(const Nodecl::Range& node)
{
    Nodecl::NodeclBase lb_expr = node.get_lower();
    Nodecl::NodeclBase ub_expr = node.get_upper();
    Nodecl::NodeclBase step_expr = node.get_stride();

    walk(lb_expr);
    file << ":";
    walk(ub_expr);
    file << ":";
    walk(step_expr);
}

CxxBase::Ret CxxBase::visit(const Nodecl::ReturnStatement& node)
{
    Nodecl::NodeclBase expression = node.get_value();

    indent();
    file << "return ";

    walk(expression);

    file << ";\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Shaping& node)
{
    Nodecl::NodeclBase postfix = node.get_postfix();
    Nodecl::List seq_exp = node.get_shape().as<Nodecl::List>();
   
    for (Nodecl::List::iterator it = seq_exp.begin();
            it != seq_exp.end();
            it++)
    {
        file << "[";
        walk(*it);
        file << "]";
    }
    
    file << " ";
    walk(postfix);
}

CxxBase::Ret CxxBase::visit(const Nodecl::Sizeof& node)
{
    TL::Type t = node.get_size_type().get_type();

    file << "sizeof(" << this->get_declaration(t, this->get_current_scope(),  "") << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Alignof& node)
{
    TL::Type t = node.get_align_type().get_type();

    file << "__alignof__(" << this->get_declaration(t, this->get_current_scope(),  "") << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::StringLiteral& node)
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

CxxBase::Ret CxxBase::visit(const Nodecl::StructuredValue& node)
{
    Nodecl::List items = node.get_items().as<Nodecl::List>();
    TL::Type type = node.get_type();

    enum structured_value_kind 
    {
        INVALID = 0,
        // (T) { expr-list }
        GCC_POSTFIX,
        // T(single-expression)
        CXX03_EXPLICIT,
        // T{expr-list}
        CXX1X_EXPLICIT,
    } kind = INVALID;

    if (IS_C_LANGUAGE)
    {
        kind = GCC_POSTFIX;
    }
    else if (IS_CXX03_LANGUAGE)
    {
        if ((items.empty()
                    || (items.size() == 1)
                && (type.is_named()
                    || type.no_ref().is_builtin()))
                && !(type.is_class()
                    && type.is_aggregate()))
        {
            kind = CXX03_EXPLICIT;
        }
        else
        {
            kind = GCC_POSTFIX;
        }
    }
    else if (IS_CXX1X_LANGUAGE)
    {
        if (type.no_ref().is_vector())
        {
            // This is nonstandard, lets fallback to gcc
            kind = GCC_POSTFIX;
        }
        else if (type.is_named())
        {
            kind = CXX1X_EXPLICIT;
        }
        else
        {
            // If this is not a named type fallback to gcc
            kind = GCC_POSTFIX;
        }
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    if (state.inside_structured_value)
    {
        kind = GCC_POSTFIX;
    }

    switch (kind)
    {
        // (T) { expr-list }
        case GCC_POSTFIX:
            {
                if (!state.inside_structured_value)
                {
                    file << "(" << this->get_declaration(type, this->get_current_scope(),  "") << ")";
                }

                char inside_structured_value = state.inside_structured_value;
                state.inside_structured_value = 1;

                file << "{ ";
                walk_expression_list(items);
                file << " }";

                state.inside_structured_value = inside_structured_value;
                break;
            }
            // T(expr-list)
        case CXX03_EXPLICIT:
            {
                file << this->get_declaration(type, this->get_current_scope(),  "");

                if (items.empty())
                {
                    file << "()";
                }
                else
                {
                    file << "(";

                    char inside_structured_value = state.inside_structured_value;
                    state.inside_structured_value = 0;

                    walk_expression_list(items);

                    state.inside_structured_value = inside_structured_value;

                    file << ")";
                }

                break;
            }
            // T{expr-list}
        case CXX1X_EXPLICIT:
            {
                file << this->get_declaration(type, this->get_current_scope(),  "");

                char inside_structured_value = state.inside_structured_value;
                state.inside_structured_value = 1;

                file << "{ ";
                walk_expression_list(items);
                file << " }";

                state.inside_structured_value = inside_structured_value;
                break;
            }
        default:
            {
                internal_error("Code unreachable", 0);
            }
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::SwitchStatement& node)
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

CxxBase::Ret CxxBase::visit(const Nodecl::Symbol& node)
{
    TL::Symbol entry = node.get_symbol();

    if (entry.is_member())
    {
        define_symbol_always(entry.get_class_type().get_symbol());
    }

    bool must_derref = is_non_language_reference_variable(entry)
        && !entry.get_type().references_to().is_array()
        && !state.do_not_derref_rebindable_reference;

    if (must_derref)
    {
        file << "(*";
    }

    C_LANGUAGE()
    {
        file << entry.get_name();
    }
    CXX_LANGUAGE()
    {
        if (entry.is_builtin())
        {
            // Builtins cannot be qualified
            file << entry.get_name();
        }
        else
        {
            file << entry.get_qualified_name(this->get_current_scope());
        }
    }

    if (must_derref)
    {
        file << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::Text& node)
{
    file << node.get_text();
}

CxxBase::Ret CxxBase::visit(const Nodecl::Throw& node)
{
    Nodecl::NodeclBase expr = node.get_rhs();

    file << "throw";

    if (!expr.is_null())
    {
        file << " ";
        walk(expr);
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::TopLevel& node)
{
    walk(node.get_top_level());
}

CxxBase::Ret CxxBase::visit(const Nodecl::TryBlock& node)
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

CxxBase::Ret CxxBase::visit(const Nodecl::Type& node)
{
    TL::Type type = node.get_type();
    file << this->get_declaration(type, this->get_current_scope(),  "");
}

CxxBase::Ret CxxBase::visit(const Nodecl::Typeid& node)
{
    Nodecl::NodeclBase expr = node.get_arg();

    file << "typeid(";
    walk(expr);
    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::VirtualFunctionCall& node)
{
    visit_function_call(node, /* is_virtual_call */ true);
}

CxxBase::Ret CxxBase::visit(const Nodecl::WhileStatement& node)
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

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDecl& node)
{
    TL::Symbol sym = node.get_symbol();
    state.must_be_object_init.erase(sym);

    if ((sym.is_class()
                || sym.is_enum())
            && (::is_incomplete_type(sym.get_type().get_internal_type())))
    {
        declare_symbol_always(sym);
    }
    else
    {
        define_symbol_always(sym);
    }
}
void CxxBase::codegen_explicit_instantiation(TL::Symbol sym,
        const Nodecl::NodeclBase & declarator_name,
        const Nodecl::NodeclBase & context,
        bool is_extern)
{
    if (sym.is_class())
    {
        std::string class_key;
        switch (sym.get_type().class_type_get_class_kind())
        {
            case TT_CLASS:
                class_key = "class";
                break;
            case TT_STRUCT:
                class_key = "struct";
                break;
            case TT_UNION:
                class_key = "union";
                break;
            default:
                internal_error("Invalid class kind", 0);
        }
        if (is_extern)
            file << "extern ";

        file << "template " << class_key << " " << sym.get_qualified_name(sym.get_scope()) << ";\n";

    }
    else if (sym.is_function())
    {
        move_to_namespace_of_symbol(context.retrieve_context().get_related_symbol());
        if (is_extern)
            file << "extern ";
        std::string original_declarator_name = codegen(declarator_name);
        file << "template " << get_declaration(sym.get_type(), sym.get_scope(), original_declarator_name) << ";\n";
    }
    else
    {
        internal_error("Invalid symbol", 0);
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxExplicitInstantiation& node)
{
    TL::Symbol sym = node.get_symbol();
    Nodecl::NodeclBase declarator_name = node.get_declarator_name();
    Nodecl::NodeclBase context = node.get_context();
    state.must_be_object_init.erase(sym);
    codegen_explicit_instantiation(sym, declarator_name, context);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxExternExplicitInstantiation& node)
{
    file << "extern ";
    TL::Symbol sym = node.get_symbol();
    Nodecl::NodeclBase declarator_name = node.get_declarator_name();
    Nodecl::NodeclBase context = node.get_context();
    state.must_be_object_init.erase(sym);
    codegen_explicit_instantiation(sym, declarator_name, context);
}

CxxBase::Ret CxxBase::visit(const Nodecl::Verbatim& node)
{
    file << node.get_text();
}

CxxBase::Ret CxxBase::visit(const Nodecl::UnknownPragma& node)
{
    file << "#pragma " << node.get_text() << "\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::GxxTrait& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    file << node.get_text() << "(";

    walk(lhs);

    if (!rhs.is_null())
    {
        file << ", ";
        walk(rhs);
    }

    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::AsmDefinition& node)
{
    indent();
    file << "asm(";
    walk(node.get_asm_text());
    file << ");\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::GccAsmDefinition& node)
{
    Nodecl::NodeclBase op0 = node.get_operands0();
    Nodecl::NodeclBase op1 = node.get_operands1();
    Nodecl::NodeclBase op2 = node.get_operands2();

    Nodecl::NodeclBase specs = node.get_specs();

    indent();
    file << "__asm__ ";
    walk_list(specs.as<Nodecl::List>(), ", ");
    file << "(";
    file << node.get_text();
    file << " : ";
    walk_list(op0.as<Nodecl::List>(), ", ");
    file << " : ";
    walk_list(op1.as<Nodecl::List>(), ", ");
    file << " : ";
    walk_list(op2.as<Nodecl::List>(), ", ");
    file << ");\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::GccAsmOperand& node)
{
    Nodecl::NodeclBase identifier = node.get_identifier();
    Nodecl::NodeclBase constraint = node.get_constraint();
    Nodecl::NodeclBase expr = node.get_expr();

    if (!identifier.is_null())
    {
        file << "[" << identifier.get_text() << "]";
    }

    walk(constraint);

    if (!expr.is_null())
    {
        file << "(";
        walk(expr);
        file << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::GccAsmSpec& node)
{
    file << " __asm(" << node.get_text() << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::GccBuiltinVaArg& node)
{
    file << "__builtin_va_arg(";
    walk(node.get_expr());
    file << ", ";
    walk(node.get_va_type());
    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::UpcSyncStatement& node)
{
    file << node.get_text() << "(";
    walk(node.get_expr()); 
    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::SourceComment& node)
{
    indent();
    file << "/* " << node.get_text() << " */\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::PreprocessorLine& node)
{
    file << node.get_text() << "\n";
}

bool CxxBase::symbol_is_same_or_nested_in(TL::Symbol symbol, TL::Symbol class_sym)
{
    if (symbol == class_sym)
    {
        return true;
    }
    else
    {
        if (symbol.is_member())
        {
            return symbol_is_same_or_nested_in(
                    symbol.get_class_type().get_symbol(),
                    class_sym);
        }
        else
        {
            return false;
        }
    }
}

bool CxxBase::symbol_is_nested_in_defined_classes(TL::Symbol symbol)
{
    for (TL::ObjectList<TL::Symbol>::iterator it = state.classes_being_defined.begin();
            it != state.classes_being_defined.end();
            it++)
    {
        // If the symbol we are going to define is nested in one of
        // the classes being defined, do not define now. It will be defined later
        if (symbol_is_same_or_nested_in(symbol, *it))
        {
            return true;
        }
    }

    return false;
} 

TL::ObjectList<TL::Symbol> CxxBase::define_required_before_class(TL::Symbol symbol)
{
    state.pending_nested_types_to_define.clear();

    if (state.being_checked_for_required.find(symbol) != state.being_checked_for_required.end())
        return TL::ObjectList<TL::Symbol>();

    state.being_checked_for_required.insert(symbol);

    if (symbol.is_class())
    {
        if (symbol.get_type().is_template_specialized_type()
                && symbol.get_type().template_specialized_type_get_template_arguments().get_num_parameters() != 0)
        {
            TL::TemplateParameters template_arguments = symbol.get_type().template_specialized_type_get_template_arguments();
            declare_all_in_template_arguments(template_arguments);

            TL::Type template_type = symbol.get_type().get_related_template_type();
            TL::Type primary_template = template_type.get_primary_template();
            TL::Symbol primary_symbol = primary_template.get_symbol();

            if (primary_symbol != symbol)
            {
                TL::Type t = primary_symbol.get_type();

                if (::is_complete_type(t.get_internal_type()))
                {
                    define_symbol_if_nonnested(primary_symbol);
                }
                else
                {
                    declare_symbol_if_nonnested(primary_symbol);
                }
            }
        }

        TL::ObjectList<TL::Type::BaseInfo> bases = symbol.get_type().get_bases();
            // We need to define all the bases first
        for (TL::ObjectList<TL::Type::BaseInfo>::iterator it = bases.begin();
                it != bases.end();
                it++)
        {
            TL::Symbol &base_class(it->base);
            define_symbol_if_nonnested(base_class);
        }

        TL::ObjectList<TL::Symbol> members = symbol.get_type().get_all_members();
        for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                it != members.end();
                it++)
        {
            TL::Symbol &member(*it);

            if (member.is_using_symbol())
            {
                //  Do nothing
            }
            else if (!member.is_class()
                    && !member.is_enum())
            {
                if (member.is_variable()
                        && member.is_static()
                        && !member.get_initialization().is_null())
                {
                    define_nonnested_entities_in_trees(member.get_initialization());
                }

                walk_type_for_symbols(
                        member.get_type(),
                        /* needs_def */ 1,
                        &CxxBase::declare_symbol_if_nonnested,
                        &CxxBase::define_symbol_if_nonnested,
                        &CxxBase::define_nonnested_entities_in_trees);

                if (member.is_function())
                {
                    // Define the types used by the function parameters
                    TL::ObjectList<TL::Type> parameters = member.get_type().parameters();
                    for (TL::ObjectList<TL::Type>::iterator it = parameters.begin();
                            it != parameters.end();
                            it++)
                    {
                        TL::Type current_parameter(*it);
                        walk_type_for_symbols(
                                current_parameter,
                                /* needs_def */ 1,
                                &CxxBase::declare_symbol_if_nonnested,
                                &CxxBase::define_symbol_if_nonnested,
                                &CxxBase::define_nonnested_entities_in_trees);

                    }

                    // Define the return type
                    walk_type_for_symbols(
                            member.get_type().returns(),
                            /* needs_def */ 1,
                            &CxxBase::declare_symbol_if_nonnested,
                            &CxxBase::define_symbol_if_nonnested,
                            &CxxBase::define_nonnested_entities_in_trees);
                }
            }
            else if (member.is_enum())
            {
                TL::ObjectList<TL::Symbol> enumerators = member.get_type().enum_get_enumerators();
                for (TL::ObjectList<TL::Symbol>::iterator it2 = enumerators.begin();
                        it2 != enumerators.end();
                        it2++)
                {
                    TL::Symbol &enumerator(*it2);
                    define_nonnested_entities_in_trees(enumerator.get_initialization());
                }
            }
        }


        TL::ObjectList<TL::Symbol> friends = symbol.get_type().class_get_friends();
        for (TL::ObjectList<TL::Symbol>::iterator it = friends.begin();
                it != friends.end();
                it++)
        {
            TL::Symbol &_friend(*it);
            walk_type_for_symbols(
                    _friend.get_type(),
                    /* needs_def */ 1,
                    &CxxBase::declare_symbol_if_nonnested,
                    &CxxBase::define_symbol_if_nonnested,
                    &CxxBase::define_nonnested_entities_in_trees);

            if (_friend.is_dependent_friend_function())
            {
                // In the field related symbol are stored the feasible candidates for this function
                // We should declare it
                TL::ObjectList<TL::Symbol> candidate_functions = _friend.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it2 = candidate_functions.begin();
                        it2 != candidate_functions.end();
                        it2++)
                {

                    TL::Symbol &_func(*it2);
                    walk_type_for_symbols(
                            _func.get_type(),
                            /* needs_def */ 1,
                            &CxxBase::declare_symbol_if_nonnested,
                            &CxxBase::define_symbol_if_nonnested,
                            &CxxBase::define_nonnested_entities_in_trees);
                }
            }

            if (!_friend.is_friend_declared())
            {
                // ??? Try defining?
                declare_symbol_if_nonnested(_friend);
            }
        }
    }
    else if (symbol.is_enum()
            || symbol.is_enumerator()
            || symbol.is_typedef()
            || symbol.is_variable())
    {
        walk_type_for_symbols(
                symbol.get_type(), /* needs_def */ 1, 
                &CxxBase::declare_symbol_if_nonnested, 
                &CxxBase::define_symbol_if_nonnested,
                &CxxBase::define_nonnested_entities_in_trees);
    }
    else 
    {
        internal_error("Unexpected symbol kind %s\n", symbol_kind_name(symbol.get_internal_symbol()));
    }

    // Here state.pending_nested_types_to_define has all the types that must be
    // defined inside the current class, so we review them, lest they required
    // something to be declared before the current class
    TL::ObjectList<TL::Symbol> result(state.pending_nested_types_to_define.begin(), state.pending_nested_types_to_define.end());

    // Remove current class if it appears
    TL::ObjectList<TL::Symbol>::iterator it = std::find(result.begin(), result.end(), symbol);

    if (it != result.end())
        result.erase(it);

    // Clear pending now as we are going to call define_required_before_class again
    state.pending_nested_types_to_define.clear();

    TL::ObjectList<TL::Symbol> must_be_defined_inside_class = result;

    for (TL::ObjectList<TL::Symbol>::iterator it = must_be_defined_inside_class.begin();
            it != must_be_defined_inside_class.end();
            it++)
    {
        TL::Symbol& entry(*it);
        // This indirectly fills state.pending_nested_types_to_define
        TL::ObjectList<TL::Symbol> pending_symbols = define_required_before_class(entry);
        result.insert(pending_symbols);
    }

    state.being_checked_for_required.erase(symbol);

    return result;
}

static bool is_member_type(TL::Symbol s)
{
    return s.is_enum()
        || s.is_class()
        || s.is_typedef();
}

static bool is_member_nontype(TL::Symbol t)
{
    return !is_member_type(t);
}


void CxxBase::define_class_symbol_aux(TL::Symbol symbol,
        TL::ObjectList<TL::Symbol> symbols_defined_inside_class,
        int level)
{
    set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);

    access_specifier_t default_access_spec = AS_UNKNOWN;

    std::string class_key;
    switch (symbol.get_type().class_type_get_class_kind())
    {
        case TT_CLASS:
            class_key = "class";
            default_access_spec = AS_PRIVATE;
            break;
        case TT_STRUCT:
            class_key = "struct";
            default_access_spec = AS_PUBLIC;
            break;
        case TT_UNION:
            class_key = "union";
            default_access_spec = AS_PUBLIC;
            break;
        default:
            internal_error("Invalid class kind", 0);
    }

    // 1. Declaration of the class key part
    C_LANGUAGE()
    {
        indent();
        // The symbol will be already called 'struct/union X' in C
        if (!symbol.is_anonymous_union())
        {
            file << symbol.get_name() << "\n";
        }
        else
        {
            file << class_key << "\n";
        }
        indent();
        file << "{\n";
    }

    char is_dependent_class = 0;
    TL::Type symbol_type = symbol.get_type();

    ERROR_CONDITION(::is_incomplete_type(symbol_type.get_internal_type()), "An incomplete class cannot be defined", 0);
    
    CXX_LANGUAGE()
    {
        char is_template_specialized = 0;
        char is_primary_template = 0;
        char defined_inside_class = symbol.is_defined_inside_class();

        TL::Type template_type(NULL);
        TL::Type primary_template(NULL);
        TL::Symbol primary_symbol(NULL);

        if (symbol_type.is_template_specialized_type()
                && symbol_type.template_specialized_type_get_template_arguments().get_num_parameters() != 0)
        {
            is_template_specialized = 1;
            template_type = symbol_type.get_related_template_type();
            primary_template = template_type.get_primary_template();
            primary_symbol = primary_template.get_symbol();

            if (primary_symbol == symbol)
            {
                is_primary_template = 1;
            }

            if (!symbol_type.class_type_is_complete_independent()
                    && !symbol_type.class_type_is_incomplete_independent())
            {
                is_dependent_class = 1;
            }
        }

        // *** From here everything required should have been declared ***

        move_to_namespace_of_symbol(symbol);

        if (is_template_specialized)
        {
            TL::TemplateParameters template_parameters(NULL);
            template_parameters = symbol.get_scope().get_template_parameters();

            if (!(symbol_type.class_type_is_complete_independent()
                        || symbol_type.class_type_is_incomplete_independent()))
            {
                if (defined_inside_class)
                {
                    // We only want the template header related to the current class
                    TL::Symbol enclosing_class = symbol.get_class_type().get_symbol();
                    codegen_template_headers_bounded(template_parameters,
                            enclosing_class.get_scope().get_template_parameters());
                }
                else
                {
                    // We want all template headers
                    codegen_template_headers_all_levels(template_parameters);
                }
            }
            else
            {
                while (template_parameters.is_valid() &&
                        template_parameters.get_is_explicit_specialization())
                {
                    indent();
                    file << "template <>\n";
                    template_parameters = template_parameters.get_enclosing_parameters();
                }
            }
        }
        else
        {
            if (!defined_inside_class)
            {
                // A special case: a class declaration or definition is inside an other template class
                TL::Symbol related_symbol = symbol.get_scope().get_related_symbol();
                TL::Type type_related_symbol = related_symbol.get_type();
                if (type_related_symbol.is_template_specialized_type())
                {
                    // If the symbol is an anonymous union then it must be defined in the enclosing class definition.
                    // Otherwise, this symbol must be defined in its namespace and we should generate all template headers.
                    if (!symbol.is_anonymous_union())
                    {
                        TL::TemplateParameters template_parameters = symbol.get_scope().get_template_parameters();
                        codegen_template_headers_all_levels(template_parameters);
                    }
                }
            }
        }

        indent();
        std::string qualified_name;
        if (level == 0)
        {
            qualified_name = symbol.get_class_qualification(/* without_template_id */ true);
        }
        else
        {
            qualified_name = symbol.get_name();
        }
        if (is_template_specialized
                && !is_primary_template)
        {
            qualified_name += get_template_arguments_str(symbol.get_internal_symbol(), symbol.get_scope().get_decl_context());
        }


        if (!symbol.is_anonymous_union())
        {
            file << class_key << " " << qualified_name;
        }
        else
        {
            file << class_key;
        }

        TL::ObjectList<TL::Type::BaseInfo> bases = symbol_type.get_bases();
        if (!bases.empty())
        {
            file << " : " ;
            for (TL::ObjectList<TL::Type::BaseInfo>::iterator it = bases.begin();
                    it != bases.end();
                    it++)
            {
                if (it != bases.begin())
                {
                    file << ", ";
                }

                TL::Symbol &base(it->base);
                bool is_virtual(it->is_virtual);
                access_specifier_t current_access_spec(it->access_specifier);

                if (is_virtual)
                {
                    file << "virtual ";
                }

                if (current_access_spec != default_access_spec)
                {
                    if (current_access_spec == AS_PUBLIC)
                    {
                        file << "public ";
                    }
                    else if (current_access_spec == AS_PRIVATE)
                    {
                        file << "private ";
                    }
                    else if (current_access_spec == AS_PROTECTED)
                    {
                        file << "protected ";
                    }
                    else
                    {
                        internal_error("Unreachable code", 0);
                    }
                }
                
                file << base.get_qualified_name(symbol.get_scope());
            }
        }

        file << "\n";
        indent();
        file << "{\n";
    }

    // 2. Now declare members
    TL::ObjectList<TL::Symbol> members = symbol_type.get_all_members();

    access_specifier_t current_access_spec = default_access_spec;

    struct iteration_member_tag
    {
        bool (*filter)(TL::Symbol);
    } filter_set[] = { 
        { is_member_type }, 
        { is_member_nontype }, 
        { NULL } 
    };

    // We have to iterate several times
    int i = 0;
    while (filter_set[i].filter != NULL)
    {
        for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                it != members.end();
                it++)
        {
            TL::Symbol &member(*it);
            if (!(filter_set[i].filter)(member))
                continue;

            access_specifier_t access_spec = member.get_access_specifier();

            CXX_LANGUAGE()
            {
                inc_indent();
                if (current_access_spec != access_spec)
                {
                    current_access_spec = access_spec;

                    indent();
                    if (current_access_spec == AS_PUBLIC)
                    {
                        file << "public:\n";
                    }
                    else if (current_access_spec == AS_PRIVATE)
                    {
                        file << "private:\n";
                    }
                    else if (current_access_spec == AS_PROTECTED)
                    {
                        file << "protected:\n";
                    }
                    else
                    {
                        internal_error("Unreachable code", 0);
                    }
                }
            }

            inc_indent();

            char old_in_member_declaration = state.in_member_declaration;
            state.in_member_declaration = 1;

            C_LANGUAGE()
            {
                if (member.get_type().is_named_class()
                        && member.get_type().get_symbol().is_anonymous_union())
                {
                    TL::Symbol class_sym = member.get_type().get_symbol();
                    define_class_symbol_aux(class_sym, symbols_defined_inside_class, level + 1);
                    set_codegen_status(class_sym, CODEGEN_STATUS_DEFINED);
                }
                else
                {
                    define_symbol_always(member);
                }
                set_codegen_status(member, CODEGEN_STATUS_DEFINED);
            }
            CXX_LANGUAGE()
            {
                if (member.is_class())
                {
                    if (member.get_type().is_template_specialized_type()
                            && member.get_type().template_specialized_type_get_template_arguments().get_num_parameters() != 0)
                    {
                        TL::Type related_template = member.get_type().get_related_template_type();
                        TL::Type primary_template = related_template.get_primary_template();
                        char is_primary_template = (primary_template.get_symbol() == member);

                        // FIXME - This comment below is obsolete, right?
                        // *****
                        //
                        // C++ has a problem here: we cannot explicitly
                        // specialize a member template class in non namespace
                        // scope but we need the definition, here EXCEPTIONALLY
                        // we will emit dependent code because this language
                        // quirk. Other solutions (like flattening the members)
                        // are more cumbersome and more painstaking than this
                        // one.
                        //
                        // *****
                        // FIXME - That comment above is obsolete, right?
                        if (is_primary_template)
                        {
                            // Try hard to avoid emitting dependent code
                            //
                            // Check every complete specialization
                            char one_specialization_defined_inside_the_class = 0;

                            TL::ObjectList<TL::Type> specializations = related_template.get_specializations();

                            for (TL::ObjectList<TL::Type>::iterator it = specializations.begin();
                                    it != specializations.end();
                                    it++)
                            {
                                TL::Type& current_spec(*it);
                                TL::Symbol current_spec_symbol = current_spec.get_symbol();

                                if (it->class_type_is_complete_independent()
                                        && symbols_defined_inside_class.contains(current_spec_symbol))
                                {
                                    one_specialization_defined_inside_the_class = 1;
                                }
                            }

                            if (one_specialization_defined_inside_the_class
                                    || is_dependent_class)
                            {
                                if (is_complete_type(member.get_type().get_internal_type()))
                                {
                                    define_class_symbol_aux(member, symbols_defined_inside_class, level + 1);
                                    set_codegen_status(member, CODEGEN_STATUS_DEFINED);
                                }
                                else
                                {
                                    declare_symbol_always(member);
                                    set_codegen_status(member, CODEGEN_STATUS_DECLARED);
                                }
                            }
                            else
                            {
                                declare_symbol_always(member);
                                set_codegen_status(member, CODEGEN_STATUS_DECLARED);
                            }
                        }
                        else
                        {
                            // Do not emit anything but mark the symbols
                            if (symbols_defined_inside_class.contains(member))
                            {
                                set_codegen_status(member, CODEGEN_STATUS_DEFINED);
                            }
                            else
                            {
                                set_codegen_status(member, CODEGEN_STATUS_DECLARED);
                            }
                        }
                    }
                    else
                    {
                        if (member.is_defined_inside_class() || symbols_defined_inside_class.contains(member))
                        {
                            define_class_symbol_aux(member, symbols_defined_inside_class, level + 1);
                            set_codegen_status(member, CODEGEN_STATUS_DEFINED);
                        }
                        else
                        {
                            declare_symbol_always(member);
                            set_codegen_status(member, CODEGEN_STATUS_DECLARED);
                        }
                    }
                }
                else if (member.is_using_symbol())
                {
                    indent();
                    ERROR_CONDITION(!member.get_type().is_unresolved_overload(), "Invalid SK_USING symbol\n", 0);

                    TL::ObjectList<TL::Symbol> unresolved = member.get_type().get_unresolved_overload_set();

                    TL::Symbol entry = unresolved[0];

                    file << "using " << entry.get_qualified_name(/* without_template */ 1) << ";\n";
                }
                else if (member.is_enum()
                        || member.is_typedef())
                {
                    define_symbol_always(member);
                    set_codegen_status(member, CODEGEN_STATUS_DEFINED);
                }
                else
                {
                    declare_symbol_always(member);
                    if (member.is_variable()
                            && (!member.is_static()
                                || ((member.get_type().is_integral_type()
                                        || member.get_type().is_enum()
                                    && member.get_type().is_const()))))
                    {
                        set_codegen_status(member, CODEGEN_STATUS_DEFINED);
                    }
                    else
                    {
                        set_codegen_status(member, CODEGEN_STATUS_DECLARED);
                    }
                }
            }
            state.in_member_declaration = old_in_member_declaration;

            dec_indent();

            CXX_LANGUAGE()
            {
                dec_indent();
            }
        }

        i++;
    }

    // 3. Declare friends
    TL::ObjectList<TL::Symbol> friends = symbol_type.class_get_friends();

    for (TL::ObjectList<TL::Symbol>::iterator it = friends.begin();
            it != friends.end();
            it++)
    {
        TL::Symbol &_friend(*it);

         //The user did not declare it, ignore it
        if (_friend.is_friend_declared() &&
                  !(_friend.is_dependent_friend_class() ||
                    _friend.is_dependent_friend_function()))
            continue;

        char is_primary_template = 0;
        TL::Symbol template_symbol(NULL);
        TL::Type friend_type = _friend.get_type();
        inc_indent();

        // A. Generate a template header if this friend declaration has one
        if (friend_type.is_template_specialized_type())
        {
            TL::Type template_type = friend_type.get_related_template_type();
            template_symbol = template_type.get_related_template_symbol();
            TL::Type primary_template = template_type.get_primary_template();
            TL::Symbol primary_symbol = primary_template.get_symbol();

            if (friend_type.is_dependent())
            {
                TL::TemplateParameters template_parameters(NULL);
                if (_friend == primary_symbol)
                {
                    is_primary_template = 1;
                    template_parameters = template_type.template_type_get_template_parameters();
                }
                else
                {
                    template_parameters = _friend.get_scope().get_template_parameters();
                }
                codegen_template_headers_bounded(template_parameters, symbol.get_scope().get_template_parameters());
            }
        }

        // B. Generate the function or class declaration
        indent();
        file << "friend ";

        if (_friend.is_class())
        {
            std::string friend_class_key;
            switch (friend_type.class_type_get_class_kind())
            {
                case TT_CLASS:
                    friend_class_key = "class";
                    break;
                case TT_STRUCT:
                    friend_class_key = "struct";
                    break;
                case TT_UNION:
                    friend_class_key = "union";
                    break;
                default:
                    internal_error("Invalid class kind", 0);
            }

            file << friend_class_key << " ";
            if (!is_primary_template)
            {
                file << _friend.get_qualified_name(symbol.get_scope());
            }
            else
            {
                file << template_symbol.get_qualified_name(symbol.get_scope());
            }
        }
        else if(_friend.is_dependent_friend_class())
        {
            enum type_tag_t class_key_tag;
            if (friend_type.is_dependent_typename())
            {
                class_key_tag = get_dependent_entry_kind(friend_type.get_internal_type());
            }
            else
            {
                class_key_tag = friend_type.class_type_get_class_kind();
            }

            std::string friend_class_key;
            switch (class_key_tag)
            {
                case TT_CLASS:
                    friend_class_key = "class";
                    break;
                case TT_STRUCT:
                    friend_class_key = "struct";
                    break;
                case TT_UNION:
                    friend_class_key = "union";
                    break;
                default:
                    internal_error("Invalid class kind", 0);
            }

            if (friend_type.is_dependent_typename())
            {
                file << this->get_declaration(friend_type, symbol.get_scope(), "")<<std::endl;
            }
            else if(friend_type.is_template_specialized_type())
            {
                file << friend_class_key << " "
                     << _friend.get_qualified_name(symbol.get_scope(), /* without template id */true);
            }
            else
            {
                file << friend_class_key << " "
                     << this->get_declaration(friend_type, symbol.get_scope(), "")<<std::endl;
            }
        }
        else if (_friend.is_function())
        {
            TL::Type real_type = friend_type;
            if (symbol.is_conversion_function())
            {
                real_type = get_new_function_type(NULL, NULL, 0);
            }

            std::string function_name = (is_primary_template) ?
                unmangle_symbol_name(_friend) : _friend.get_qualified_name();

            file << this->get_declaration(real_type, symbol.get_scope(), function_name);
        }
        else if (_friend.is_dependent_friend_function())
        {
            TL::Type real_type = friend_type;
            if (symbol.is_conversion_function())
            {
                real_type = get_new_function_type(NULL, NULL, 0);
            }

            std::string function_name;
            if (friend_type.is_template_specialized_type())
            {
                function_name =
                    _friend.get_qualified_name(symbol.get_scope(), /* without template id */ true);
            }
            else
            {
                function_name =
                    _friend.get_qualified_name(symbol.get_scope(), /* without template id */ false);
            }

            file << this->get_declaration(real_type, symbol.get_scope(), function_name);
        }
        else
        {
            internal_error("Invalid friend symbol kind '%s'\n", symbol_kind_name(_friend.get_internal_symbol()));
        }

        file << ";\n";
        dec_indent();
    }

    indent();

    if (symbol.has_gcc_attributes())
    {
        file << "} " << gcc_attributes_to_str(symbol) << ";\n";
    }
    else
    {
        file << "};\n";
    }
}

void CxxBase::define_class_symbol(TL::Symbol symbol)
{
    if (symbol.is_anonymous_union() && !symbol.is_user_declared())
    {
        // This anonymous already union has been defined in a class scope
        return;
    }

    std::set<TL::Symbol> current_pending = state.pending_nested_types_to_define;

    state.classes_being_defined.push_back(symbol);

    // This indirectly fills state.pending_nested_types_to_define
    TL::ObjectList<TL::Symbol> symbols_defined_inside_class = define_required_before_class(symbol);

    define_class_symbol_aux(symbol, symbols_defined_inside_class, /* level */ 0);

    state.classes_being_defined.pop_back();

    state.pending_nested_types_to_define = current_pending;
}

bool CxxBase::is_local_symbol(TL::Symbol entry)
{
    return entry.is_valid()
        && (entry.get_scope().is_block_scope()
                || entry.get_scope().is_function_scope()
                || (entry.is_member() && is_local_symbol(entry.get_class_type().get_symbol())));
}

bool CxxBase::is_prototype_symbol(TL::Symbol entry)
{
    return entry.is_valid()
        && entry.get_scope().is_prototype_scope();
}

bool CxxBase::all_enclosing_classes_are_user_declared(TL::Symbol entry)
{
    bool result = true;
    TL::Symbol class_entry = entry;
    while (result && class_entry.is_member())
    {
        class_entry = class_entry.get_class_type().get_symbol();
        result = class_entry.is_user_declared();
    }
    return result;
}

void CxxBase::define_symbol_if_local(TL::Symbol symbol)
{
    if (is_local_symbol(symbol))
    {
        define_symbol_always(symbol);
    }
}

void CxxBase::declare_symbol_if_local(TL::Symbol symbol)
{
    if (is_local_symbol(symbol))
    {
        declare_symbol_always(symbol);
    }
}

void CxxBase::define_symbol_if_nonlocal(TL::Symbol symbol)
{
    if (!is_local_symbol(symbol))
    {
        define_symbol_always(symbol);
    }
}

void CxxBase::define_symbol_if_nonlocal_nonprototype(TL::Symbol symbol)
{
    if (!is_local_symbol(symbol)
            && !is_prototype_symbol(symbol))
    {
        define_symbol_always(symbol);
    }
}

void CxxBase::define_symbol_if_nonprototype(TL::Symbol symbol)
{
    if (!is_prototype_symbol(symbol))
    {
        define_symbol_always(symbol);
    }
}

void CxxBase::declare_symbol_if_nonlocal(TL::Symbol symbol)
{
    if (!is_local_symbol(symbol))
    {
        declare_symbol_always(symbol);
    }
}

void CxxBase::declare_symbol_if_nonlocal_nonprototype(TL::Symbol symbol)
{
    if (!is_local_symbol(symbol)
            && !is_prototype_symbol(symbol))
    {
        declare_symbol_always(symbol);
    }
}

void CxxBase::declare_symbol_if_nonprototype(TL::Symbol symbol)
{
    if (!is_prototype_symbol(symbol))
    {
        declare_symbol_always(symbol);
    }
}

void CxxBase::define_symbol_if_nonnested(TL::Symbol symbol)
{
    if (!symbol_is_nested_in_defined_classes(symbol))
    {
        define_symbol_always(symbol);
    }
    else
    {
        // This symbol is nested in a defined class
        // We cannot define it now, so we keep it 
        state.pending_nested_types_to_define.insert(symbol);
    }
}

void CxxBase::declare_symbol_if_nonnested(TL::Symbol symbol)
{
    if (!symbol_is_nested_in_defined_classes(symbol)
            || !symbol.is_member())
    {
        declare_symbol_always(symbol);
    }
}

void CxxBase::define_nonnested_entities_in_trees(Nodecl::NodeclBase const& node)
{
    define_generic_entities(node,
            &CxxBase::declare_symbol_if_nonnested,
            &CxxBase::define_symbol_if_nonnested,
            &CxxBase::define_nonnested_entities_in_trees,
            &CxxBase::entry_just_define);
}

void CxxBase::define_or_declare_if_complete(TL::Symbol sym,
        void (CxxBase::* symbol_to_declare)(TL::Symbol),
        void (CxxBase::* symbol_to_define)(TL::Symbol))
{
    if ((sym.is_class()
                || sym.is_enum())
            && ( ::is_incomplete_type(sym.get_type().get_internal_type()) 
                || symbol_is_nested_in_defined_classes(sym)))
    {
        (this->*symbol_to_declare)(sym);
    }
    else
    {
        (this->*symbol_to_define)(sym);
    }
}

void CxxBase::define_specializations_user_declared(TL::Symbol sym)
{
    if (sym.is_template_parameter())
        return;

    ERROR_CONDITION(!sym.is_template(), "must be a template symbol", 0);

    set_codegen_status(sym, CODEGEN_STATUS_DEFINED);

    TL::ObjectList<TL::Type> specializations = sym.get_type().get_specializations();
    for (TL::ObjectList<TL::Type>::iterator it = specializations.begin();
            it != specializations.end();
            ++it)
    {
        TL::Type type_spec = *it;
        TL::Symbol sym_spec = type_spec.get_symbol();

        if (sym_spec.is_user_declared())
        {
            define_or_declare_if_complete(sym_spec, &CxxBase::declare_symbol_always, &CxxBase::define_symbol_always);
        }
    }
}

void CxxBase::define_symbol_always(TL::Symbol symbol)
{
    if (state.emit_declarations == State::EMIT_NO_DECLARATIONS)
        return;

    if (state.emit_declarations == State::EMIT_CURRENT_SCOPE_DECLARATIONS)
    {
        if (this->get_current_scope().get_decl_context().current_scope != symbol.get_scope().get_decl_context().current_scope)
            return;
    }

    if (symbol.not_to_be_printed())
        return;

    if (symbol.is_injected_class_name())
        symbol = symbol.get_class_type().get_symbol();

    if (symbol.get_type().is_template_specialized_type()
            && all_enclosing_classes_are_user_declared(symbol))
    {
        TL::Symbol template_symbol =
            symbol
            .get_type()
            .get_related_template_type()
            .get_related_template_symbol();
        if (get_codegen_status(template_symbol) != CODEGEN_STATUS_DEFINED)
        {
            //We may need to define or declare the template arguments
            TL::TemplateParameters template_arguments =
                symbol.get_type().template_specialized_type_get_template_arguments();
            declare_all_in_template_arguments(template_arguments);

            //We must define all user declared specializations
            define_specializations_user_declared(template_symbol);
            return;
        }
    }

    if (symbol.is_dependent_entity())
    {
        TL::Symbol entry(NULL);
        Nodecl::NodeclBase n = Nodecl::NodeclBase::null();
        symbol.get_type().dependent_typename_get_components(entry, n);
        define_symbol_if_nonnested(entry);
        return;
    }

    if (symbol.is_member())
    {
        TL::Symbol class_entry = symbol.get_class_type().get_symbol();
        if (!symbol_is_nested_in_defined_classes(class_entry))
        {
            define_symbol_if_nonnested(class_entry);
        }
    }

    // Do nothing if already defined
    if (get_codegen_status(symbol) == CODEGEN_STATUS_DEFINED
            // It is a symbol that will be object-inited
            || state.must_be_object_init.find(symbol) != state.must_be_object_init.end())
        return;

    // We only generate user declared code
    if (!symbol.is_user_declared())
        return;

    if (symbol.is_variable())
    {
        declare_symbol_always(symbol);
    }
    else if (symbol.is_typedef())
    {
        // Template parameters are not to be defined, ever
        if (!symbol.is_template_parameter())
        {
            move_to_namespace_of_symbol(symbol);
            indent();
            file << "typedef "
                << this->get_declaration(symbol.get_type(),
                        symbol.get_scope(),
                        symbol.get_name())
                << ";\n";
        }
    }
    else if (symbol.is_enumerator())
    {
        define_symbol_always(symbol.get_type().get_symbol());
    }
    else if (symbol.is_enum())
    {
        move_to_namespace_of_symbol(symbol);
        C_LANGUAGE()
        {
            // the symbol will be already called 'enum X' in C
            indent();
            file << symbol.get_name() << "\n";
            indent();
            file << "{\n";
        }
        CXX_LANGUAGE()
        {
            indent();
            file << "enum " << symbol.get_name() << "\n";
            indent();
            file << "{\n";
        }
        inc_indent();

        TL::ObjectList<TL::Symbol> enumerators = symbol.get_type().enum_get_enumerators();
        for (TL::ObjectList<TL::Symbol>::iterator it = enumerators.begin();
                it != enumerators.end();
                it++)
        {
            TL::Symbol &enumerator(*it);
            push_scope(enumerator.get_scope());
            if (it != enumerators.begin())
            {
                file << ",\n";
            }
            indent();
            file << enumerator.get_name();

            if (!enumerator.get_initialization().is_null())
            {
                file << " = ";
                walk(enumerator.get_initialization());
            }
            pop_scope();
        }

        dec_indent();

        file << "\n";
        indent();
        file << "};\n";
    }
    else if (symbol.is_class())
    {
        define_class_symbol(symbol);
    }
    else if (symbol.is_function())
    {
        // Functions are not defined but only declared
        declare_symbol_always(symbol);
    }
    else if (symbol.is_template_parameter())
    {
        // Do nothing
    }
    else
    {
        internal_error("I do not know how to define a %s\n", symbol_kind_name(symbol.get_internal_symbol()));
    }

    set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);
}

void CxxBase::declare_symbol_always(TL::Symbol symbol)
{
    if (state.emit_declarations == State::EMIT_NO_DECLARATIONS)
        return;

    if (state.emit_declarations == State::EMIT_CURRENT_SCOPE_DECLARATIONS)
    {
        if (this->get_current_scope().get_decl_context().current_scope != symbol.get_scope().get_decl_context().current_scope)
            return;
    }

    if (symbol.is_injected_class_name())
        symbol = symbol.get_class_type().get_symbol();

    if (symbol.not_to_be_printed())
        return;

    if (symbol.get_type().is_template_specialized_type()
            && !symbol.is_user_declared())
    {
        //We may need to define or declare the template arguments
        TL::TemplateParameters template_arguments =
            symbol.get_type().template_specialized_type_get_template_arguments();
        declare_all_in_template_arguments(template_arguments);

        //We must declare ONLY the primary template
        TL::Symbol primary_symbol =
            symbol
            .get_type()
            .get_related_template_type()
            .get_primary_template()
            .get_symbol();


        define_or_declare_if_complete(primary_symbol, &CxxBase::declare_symbol_always, &CxxBase::define_symbol_always);
        return;
    }

    if (symbol.is_member())
    {
        TL::Symbol class_entry = symbol.get_class_type().get_symbol();
        if (!symbol_is_nested_in_defined_classes(class_entry))
        {
            define_symbol_if_nonnested(class_entry);
        }
    }

    // We only generate user declared code
    if (!symbol.is_user_declared())
        return;

    // Do nothing if already defined or declared
    if (get_codegen_status(symbol) == CODEGEN_STATUS_DEFINED
            || get_codegen_status(symbol) == CODEGEN_STATUS_DECLARED
            // It is a symbol that will be object-inited
            || state.must_be_object_init.find(symbol) != state.must_be_object_init.end()
            )
        return;

    set_codegen_status(symbol, CODEGEN_STATUS_DECLARED);

    if (symbol.is_variable())
    {
        // Builtins, anonymous unions and non-user declared varibles are not printed
        if (!(symbol.is_builtin()
                    || (symbol.get_type().is_named_class()
                        && symbol.get_type().get_symbol().is_anonymous_union())))
        {
            std::string decl_specifiers;
            std::string gcc_attributes;
            std::string declarator;
            std::string bit_field;

            bool requires_extern_linkage = false;
            CXX_LANGUAGE()
            {
                requires_extern_linkage = (!symbol.is_member() 
                        && symbol.has_nondefault_linkage());

                if (requires_extern_linkage)
                {
                    file << "extern " + symbol.get_linkage() + "\n";
                    indent();
                    file << "{\n";

                    inc_indent();
                }
            }

            if (symbol.is_static())
            {
                decl_specifiers += "static ";
            }
            else if (symbol.is_extern())
            {
                decl_specifiers += "extern ";
            }
            
            // If this not a member, or if it is, is nonstatic, then it has already been defined
            if (!symbol.is_member()
                    || !symbol.is_static())
            {
                set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);
            }

            if (symbol.is_thread())
            {
                decl_specifiers += "__thread ";
            }
            if (symbol.is_mutable())
            {
                decl_specifiers += "mutable ";
            }
            if (symbol.is_register())
            {
                decl_specifiers += "register ";
            }
            if (symbol.is_bitfield())
            {
                unsigned int bits_of_bitfield =  const_value_cast_to_4(
                        nodecl_get_constant(symbol.get_bitfield_size().get_internal_nodecl()));

                std::stringstream ss;
                ss << ":" << bits_of_bitfield;

                bit_field = ss.str();
            }

            declarator = this->get_declaration(symbol.get_type(),
                    symbol.get_scope(),
                    unmangle_symbol_name(symbol));

            // Emit the initializer for nonmembers and nonstatic members in
            // non member declarations or member declarations if they have
            // integral or enum type
            char emit_initializer = 0;
            if (!symbol.get_initialization().is_null()
                    && (!symbol.is_member()
                        || (symbol.is_static()
                            && (!state.in_member_declaration
                                || (symbol.get_type().is_integral_type() 
                                    || symbol.get_type().is_enum())
                                && symbol.get_type().is_const()))))
            {
                emit_initializer = 1;
                if (symbol.is_member()
                        // FIXME -> || !is_local_symbol(symbol))
                        || (!symbol.get_scope().is_block_scope()
                            && !symbol.get_scope().is_function_scope()))
                {
                    // This is a member or nonlocal symbol
                    define_nonnested_entities_in_trees(symbol.get_initialization());
                }
                else
                {
                    // This is a local symbol
                    define_local_entities_in_trees(symbol.get_initialization());
                }
            }

            if (symbol.has_gcc_attributes())
            {
                gcc_attributes = gcc_attributes_to_str(symbol) + " ";
            }

            move_to_namespace_of_symbol(symbol);
            indent();
            file << decl_specifiers << gcc_attributes << declarator << bit_field;

            // Initializer
            if (emit_initializer)
            {
                push_scope(symbol.get_scope());

                char equal_is_needed = 0;
                char is_call_to_self_constructor = 0;
                C_LANGUAGE()
                {
                    equal_is_needed = 1;
                }

                CXX03_LANGUAGE()
                {
                    // We only need = if the initializer is a structured one
                    // and this is C++03, in C++1x syntax { } is always allowed
                    equal_is_needed = 1;

                    if (nodecl_calls_to_constructor(symbol.get_initialization(), symbol.get_type()))
                    {
                        equal_is_needed = 0;
                        is_call_to_self_constructor = 1;

                        // But it might happen the user wrote
                        //
                        // A a = A(); which looks like A a(A()); and it will be parsed as a function declarator
                        Nodecl::List nodecl_args = symbol.get_initialization()
                            .as<Nodecl::FunctionCall>()
                            .get_arguments()
                            .as<Nodecl::List>(); 

                        char zero_arg_types = 1;

                        for (Nodecl::List::iterator it = nodecl_args.begin();
                                it != nodecl_args.end() && zero_arg_types;
                                it++)
                        {
                            Nodecl::NodeclBase current = *it;
                            if (current.is<Nodecl::Conversion>())
                                current = current.as<Nodecl::Conversion>().get_nest();

                            if (!nodecl_is_zero_args_call_to_constructor(current)
                                    && !nodecl_is_zero_args_structured_value(current))
                            {
                                zero_arg_types = 0;
                            }
                        }

                        // In this case, to avoid printing something like
                        //
                        // A a(A(), B(), C());
                        //
                        // where A, B and C are types
                        //
                        // we mandate an equal so it looks like
                        //
                        // A a = A(A(), B(), C());
                        if (nodecl_args.size() != 0
                                && zero_arg_types)
                        {
                            equal_is_needed = 1;
                        }
                    }
                }

                if (equal_is_needed)
                {
                    Nodecl::NodeclBase init = symbol.get_initialization();

                    //The equal will be inserted in the CxxEqualInitializer visitor
                    if (!init.is<Nodecl::CxxEqualInitializer>())
                    {
                        file << " = ";
                    }

                    if (is_call_to_self_constructor)
                    {
                        // Ignore the top constructor call
                        Nodecl::List nodecl_args = init
                            .as<Nodecl::FunctionCall>()
                            .get_arguments()
                            .as<Nodecl::List>();

                        if (!nodecl_args.empty())
                        {
                            walk_expression_list(nodecl_args);
                        }
                    }
                    else if (init.is<Nodecl::StructuredValue>())
                    {
                        if (!symbol.get_type().is_aggregate()
                                && init
                                .as<Nodecl::StructuredValue>()
                                .get_items()
                                .as<Nodecl::List>()
                                .size() == 1)
                        {
                            // We can ignore '{' and '}'
                            walk_expression_list(init
                                    .as<Nodecl::StructuredValue>()
                                    .get_items()
                                    .as<Nodecl::List>());
                        }
                        else
                        {
                            char old_inside_struct = state.inside_structured_value;
                            state.inside_structured_value = 1;

                            walk(init);

                            state.inside_structured_value = old_inside_struct;
                        }
                    }
                    else
                    {
                        char top_is_comma = init.is<Nodecl::Comma>();

                        if (top_is_comma)
                        {
                            file << "(";
                        }

                        bool is_non_language_ref = (is_non_language_reference_variable(symbol)
                                && !symbol.get_type().references_to().is_array());

                        if (is_non_language_ref)
                        {
                            file << "(&(";
                        }
                        walk(init);
                        if (is_non_language_ref)
                        {
                            file << "))";
                        }

                        if (top_is_comma)
                        {
                            file << ")";
                        }
                    }
                }
                else
                {
                    if (is_call_to_self_constructor)
                    {
                        // Do not print the top constructor call
                        Nodecl::List nodecl_args = symbol
                            .get_initialization()
                            .as<Nodecl::FunctionCall>()
                            .get_arguments()
                            .as<Nodecl::List>();

                        if (!nodecl_args.empty())
                        {
                            file << "(";
                            walk_expression_list(nodecl_args);
                            file << ")";
                        }
                    }
                    else
                    {
                        walk(symbol.get_initialization());
                    }
                }

                pop_scope();
            }

            if (!state.in_condition)
            {
                file << ";\n";
            }

            CXX_LANGUAGE()
            {
                if (requires_extern_linkage)
                {
                    dec_indent();
                    indent();
                    file << "}\n";
                }
            }
        }
    }
    else if (symbol.is_class())
    {
        C_LANGUAGE()
        {
            // the symbol will be already called 'struct/union X' in C
            indent();
            file << symbol.get_name();
        }

        CXX_LANGUAGE()
        {
            if (symbol.is_member())
            {
                // A nested symbol can be declared only if we are
                // defining its immediate enclosing class, otherwise request for a definition of the enclosing class
                if (state.classes_being_defined.empty()
                        || (state.classes_being_defined.back() != 
                            symbol.get_class_type().get_symbol()))
                {
                    define_symbol_always(symbol.get_class_type().get_symbol());
                    return;
                }
            }

            char is_template_specialized = 0;
            char is_primary_template = 0;

            TL::Type template_type(NULL);
            TL::Type primary_template(NULL);
            TL::Symbol primary_symbol(NULL);

            if (symbol.get_type().is_template_specialized_type()
                    && symbol.get_type().template_specialized_type_get_template_arguments().get_num_parameters() != 0)
            {
                is_template_specialized = 1;
                template_type = symbol.get_type().get_related_template_type();
                primary_template = template_type.get_primary_template();
                primary_symbol = primary_template.get_symbol();
                declare_symbol_always(primary_symbol);

                if (primary_symbol != symbol)
                {
                    declare_symbol_always(primary_symbol);
                }
                else
                {
                    is_primary_template = 1;
                }

                TL::TemplateParameters template_arguments = symbol.get_type().template_specialized_type_get_template_arguments();
                declare_all_in_template_arguments(template_arguments);

                if (!symbol.get_type().class_type_is_complete_independent()
                        && !symbol.get_type().class_type_is_incomplete_independent())
                {
                    // If this is dependent and it is not the primary template do
                    // not continue, declaring the primary should have been enough
                    //
                    // This may happen for template functions which implicitly name
                    // dependent specializations (such as those defined using
                    // default template arguments). It also may be caused by a bug
                    // in the frontend, though
                    if (!is_primary_template)
                    {
                        return; 
                    }
                }
            }

            // TODO - Namespaces
            std::string class_key;
            switch (symbol.get_type().class_type_get_class_kind())
            {
                case TT_CLASS:
                    class_key = "class";
                    break;
                case TT_STRUCT:
                    class_key = "struct";
                    break;
                case TT_UNION:
                    class_key = "union";
                    break;
                default:
                    internal_error("Invalid class kind", 0);
            }

            move_to_namespace_of_symbol(symbol);

            if (is_template_specialized)
            {
                if (symbol.get_type().class_type_is_complete_independent()
                        || symbol.get_type().class_type_is_incomplete_independent())
                {
                    indent();
                    file << "template <>\n";
                }
                else
                {
                    ERROR_CONDITION(!is_primary_template, "Only the primary template is allowed "
                            "as a dependent template specialized type!\n", 0);

                    TL::TemplateParameters template_parameters = symbol.get_type().template_specialized_type_get_template_arguments();
                    codegen_template_header(template_parameters);
                }
            }
            
            // A union inside a class always must be defined
            if (class_key == "union" &&
                    symbol.get_scope().is_class_scope())
            {
                define_symbol_always(symbol);
                return;
            }

            indent();
            file << class_key << " " << symbol.get_name();

            if (is_template_specialized
                    && !is_primary_template)
            {
                file << get_template_arguments_str(symbol.get_internal_symbol(), symbol.get_scope().get_decl_context());
            }
        }

        file << ";\n";
    }
    else if (symbol.is_enumerator())
    {
        declare_symbol_always(symbol.get_type().get_symbol());
    }
    else if (symbol.is_enum())
    {
        // Enums cannot be only declared but defined
        define_symbol_always(symbol);
    }
    else if (symbol.is_typedef())
    {
        // Typedefs can't be simply declared
        define_symbol_always(symbol);
    }
    else if (symbol.is_function())
    {
        // The user did not declare it, ignore it
        if (symbol.is_friend_declared())
            return;

        walk_type_for_symbols(
                symbol.get_type(),
                /* needs_def */ 1,
                &CxxBase::declare_symbol_if_nonlocal,
                &CxxBase::define_symbol_if_nonlocal,
                &CxxBase::define_nonprototype_entities_in_trees);

        char is_primary_template = 0;
        bool requires_extern_linkage = false;
        CXX_LANGUAGE()
        {
            move_to_namespace_of_symbol(symbol);

            requires_extern_linkage = (!symbol.is_member() 
                    && symbol.has_nondefault_linkage());

            if (requires_extern_linkage)
            {
                file << "extern " + symbol.get_linkage() + "\n";
                indent();
                file << "{\n";

                inc_indent();
            }

            if (symbol.get_type().is_template_specialized_type()
                    && symbol.get_type().template_specialized_type_get_template_arguments().get_num_parameters() != 0)
            {
                TL::Type template_type = symbol.get_type().get_related_template_type();
                TL::Type primary_template = template_type.get_primary_template();
                TL::Symbol primary_symbol = primary_template.get_symbol();
                declare_symbol_always(primary_symbol);

                if (primary_symbol != symbol)
                {
                    indent();
                    file << "template <>\n";
                }
                else
                {
                    TL::TemplateParameters template_parameters = template_type.template_type_get_template_parameters();
                    codegen_template_header(template_parameters);
                    is_primary_template = 1;
                }
            }
        }

        TL::ObjectList<TL::Symbol> related_symbols = symbol.get_related_symbols();
        TL::ObjectList<std::string> parameter_names(related_symbols.size());
        TL::ObjectList<std::string> parameter_attributes(related_symbols.size());
        int i = 0;
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++, i++)
        {
            TL::Symbol current_param = *it;
            if (current_param.is_valid())
            {
                parameter_names[i] = current_param.get_name();
                if (current_param.has_gcc_attributes())
                {
                    parameter_attributes[i] = gcc_attributes_to_str(current_param); 
                }
            }
        }

        std::string decl_spec_seq;
        if (symbol.is_static())
        {
            decl_spec_seq += "static ";
        }
        if (symbol.is_extern())
        {
            decl_spec_seq += "extern ";
        }
        if (symbol.is_virtual())
        {
            decl_spec_seq += "virtual ";
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

        if (symbol.is_explicit_constructor())
        {
            decl_spec_seq += "explicit ";
        }

        std::string gcc_attributes = gcc_attributes_to_str(symbol);
        std::string asm_specification = gcc_asm_specifier_to_str(symbol);


        TL::Type real_type = symbol.get_type();
        if (symbol.is_conversion_function()
                || symbol.is_destructor())
        {
            // FIXME
            real_type = get_new_function_type(NULL, NULL, 0);
        }

        std::string function_name = unmangle_symbol_name(symbol);

        if (symbol.get_type().is_template_specialized_type()
                // Conversions do not allow templates
                && !is_primary_template
                && !symbol.is_conversion_function())
        {
            function_name += template_arguments_to_str(symbol);
        }

        std::string declarator = "";
        if (!real_type.lacks_prototype())
        {
            // declarator = this->get_declaration(
            //         real_type,
            //         symbol.get_scope(),
            //         function_name);
            declarator = this->get_declaration_with_parameters(real_type, symbol.get_scope(), 
                    function_name, 
                    parameter_names, 
                    parameter_attributes);

            if (symbol.is_virtual()
                    && symbol.is_pure())
            {
                declarator += " = 0";
            }
        }
        else
        {
            declarator = this->get_declaration(
                    real_type.returns(),
                    symbol.get_scope(),
                    function_name);
            declarator += "(";
            TL::ObjectList<TL::Symbol> args = symbol.get_related_symbols();
            for (TL::ObjectList<TL::Symbol>::iterator it = args.begin();
                    it != args.end();
                    it++)
            {
                if (it != args.begin())
                    declarator += ", ";

                declarator += it->get_name();
            }

            bool has_ellipsis = false;
            real_type.parameters(has_ellipsis);

            if (has_ellipsis)
            {
                if (!args.empty())
                {
                    declarator += ", ";
                }

                declarator += "...";
            }

            declarator += ")";
        }

        std::string exception_spec = exception_specifier_to_str(symbol);


        indent();
        file << decl_spec_seq << declarator << exception_spec << asm_specification << gcc_attributes << ";\n";

        CXX_LANGUAGE()
        {
            if (requires_extern_linkage)
            {
                dec_indent();
                indent();
                file << "}\n";
            }
        }
    }
    else if (symbol.is_template_parameter())
    {
        // Do nothing
    }
    else if (symbol.is_dependent_entity())
    {
        TL::Symbol entry(NULL);
        Nodecl::NodeclBase n = Nodecl::NodeclBase::null();

        symbol.get_type().dependent_typename_get_components(entry, n);

        declare_symbol_always(entry);
    }
    else if(symbol.get_type().is_dependent_typename() || symbol.is_dependent_friend_function())
    {
        // Do nothing
    }
    else
    {
        internal_error("Do not know how to declare a %s\n", symbol_kind_name(symbol.get_internal_symbol()));
    }
}

void CxxBase::define_generic_entities(Nodecl::NodeclBase node,
        void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
        void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
        void (CxxBase::*define_entities_fun)(const Nodecl::NodeclBase& node),
        void (CxxBase::*define_entry_fun)(
            const Nodecl::NodeclBase &node, TL::Symbol entry,
            void (CxxBase::*def_sym_fun_2)(TL::Symbol symbol))
        )
{
    if (node.is_null())
        return;

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
           it != children.end(); 
           it++)
    {
        define_generic_entities(
                *it,
                decl_sym_fun,
                def_sym_fun,
                define_entities_fun,
                define_entry_fun
                );
    }

    TL::Symbol entry = node.get_symbol();
    if (entry.is_valid()
            && entry.get_type().is_valid())
    {
        walk_type_for_symbols(entry.get_type(), 
                /* needs_def */ 1, 
                decl_sym_fun,
                def_sym_fun,
                define_entities_fun
                );

        (this->*define_entry_fun)(node, entry, def_sym_fun);
        
        if (state.walked_symbols.find(entry) == state.walked_symbols.end())
        {
            state.walked_symbols.insert(entry);
            
            define_generic_entities(entry.get_initialization(),
                    decl_sym_fun,
                    def_sym_fun,
                    define_entities_fun,
                    define_entry_fun
                    );
            
            state.walked_symbols.erase(entry);
        }
    }

    TL::Type type = node.get_type();
    if (type.is_valid())
    {
        walk_type_for_symbols(
                type, 
                /* needs_def */ 1, 
                decl_sym_fun,
                def_sym_fun,
                define_entities_fun);
    }

#if 0
    if (node.is<Nodecl::Conversion>())
    {
        // Special cases for conversion nodes 
        //
        // When a pointer or reference to class type (or pointer to member) is
        // converted from a derived class to a base class, it requires both
        // classes be defined but since they are pointers, generic
        // walk_type_for_symbols does not realize this fact. NODECL_CONVERSION
        // nodes appear where a standard conversion has been applied by the
        // frontend during typechecking
        TL::Type dest_type = node.get_type();
        TL::Type source_type = node.as<Nodecl::Conversion>().get_nest().get_type();

        if ((dest_type.is_any_reference_to_class()
                    && source_type.is_any_reference_to_class()
                    && dest_type.no_ref().is_base_class(source_type.no_ref()))
                || (dest_type.no_ref().is_pointer_to_class()
                    && source_type.no_ref().is_pointer_to_class()
                    && dest_type.no_ref().points_to().is_base_class(
                        source_type.no_ref().points_to()))
                || (dest_type.no_ref().is_pointer_to_member()
                    && source_type.no_ref().is_pointer_to_member()
                    // This is OK, for pointers to members conversion is Base to Derived (not Derived to Base)
                    && source_type.no_ref().pointed_class().is_base_class(
                        dest_type.no_ref().pointed_class())))
        {
            TL::Type base_class(NULL);
            TL::Type derived_class(NULL);

            if (dest_type.is_any_reference_to_class()
                        && source_type.is_any_reference_to_class())
            {
                base_class = dest_type.no_ref();
                derived_class = source_type.no_ref();
            }
            else if (dest_type.no_ref().is_pointer_to_class()
                    && source_type.no_ref().is_pointer_to_class())
            {
                base_class = dest_type.no_ref().points_to();
                derived_class = source_type.no_ref().points_to();
            }
            else if (dest_type.no_ref().is_pointer_to_member()
                    && source_type.no_ref().is_pointer_to_member())
            {
                base_class = source_type.no_ref().pointed_class();
                derived_class = dest_type.no_ref().pointed_class();
            }
            else
            {
                internal_error("Code unreachable", 0);
            }


            walk_type_for_symbols(base_class, /* needs_def */ 1, 
                    decl_sym_fun, 
                    def_sym_fun,
                    define_entities_fun);
            walk_type_for_symbols(derived_class, /* needs_def */ 1, 
                    decl_sym_fun, 
                    def_sym_fun,
                    define_entities_fun);
        }
    }
#endif
}

void CxxBase::entry_just_define(
        const Nodecl::NodeclBase&, 
        TL::Symbol entry,
        void (CxxBase::*def_sym_fun)(TL::Symbol))
{
    (this->*def_sym_fun)(entry);
}

void CxxBase::entry_local_definition(
        const Nodecl::NodeclBase& node,
        TL::Symbol entry,
        void (CxxBase::*def_sym_fun)(TL::Symbol))
{
    // FIXME - Improve this
    if (this->get_current_scope().get_decl_context().current_scope 
            == entry.get_scope().get_decl_context().current_scope)
    {
        if (node.is<Nodecl::ObjectInit>()
                || node.is<Nodecl::CxxDecl>())
        {
            // If this is an object init (and the traversal ensures that
            // they will be seen first) we assume it's already been defined
            state.must_be_object_init.insert(entry);
        }
        else
        {
            (this->*def_sym_fun)(entry);
        }
    }
}

void CxxBase::define_all_entities_in_trees(const Nodecl::NodeclBase& node)
{
    define_generic_entities(node, 
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always,
            &CxxBase::define_all_entities_in_trees,
            &CxxBase::entry_just_define);
}

void CxxBase::define_nonlocal_entities_in_trees(const Nodecl::NodeclBase& node)
{
    define_generic_entities(node, 
            &CxxBase::declare_symbol_if_nonlocal,
            &CxxBase::define_symbol_if_nonlocal,
            &CxxBase::define_nonlocal_entities_in_trees,
            &CxxBase::entry_just_define);
}

void CxxBase::define_nonprototype_entities_in_trees(const Nodecl::NodeclBase& node)
{
    define_generic_entities(node, 
            &CxxBase::declare_symbol_if_nonprototype,
            &CxxBase::define_symbol_if_nonprototype,
            &CxxBase::define_nonprototype_entities_in_trees,
            &CxxBase::entry_just_define);
}

void CxxBase::define_nonlocal_nonprototype_entities_in_trees(const Nodecl::NodeclBase& node)
{
    define_generic_entities(node, 
            &CxxBase::declare_symbol_if_nonlocal_nonprototype,
            &CxxBase::define_symbol_if_nonlocal_nonprototype,
            &CxxBase::define_nonlocal_nonprototype_entities_in_trees,
            &CxxBase::entry_just_define);
}

void CxxBase::define_local_entities_in_trees(const Nodecl::NodeclBase& node)
{
    define_generic_entities(node, 
            &CxxBase::declare_symbol_if_local,
            &CxxBase::define_symbol_if_local,
            &CxxBase::define_local_entities_in_trees,
            &CxxBase::entry_local_definition);
}

void CxxBase::walk_type_for_symbols(TL::Type t, 
        bool needs_def, 
        void (CxxBase::* symbol_to_declare)(TL::Symbol),
        void (CxxBase::* symbol_to_define)(TL::Symbol),
        void (CxxBase::* define_entities_in_tree)(const Nodecl::NodeclBase&))
{
    if (!t.is_valid())
        return;

    if (state.walked_types.find(t) != state.walked_types.end())
        return;

    // This effectively poisons return, do not return from this function
#define return 1=1;

    state.walked_types.insert(t);

    // This must be checked first since all query functions ignore typedefs
    if (t.is_named()
            && t.get_symbol().is_typedef())
    {
        walk_type_for_symbols(
                t.get_symbol().get_type(),
                needs_def, 
                symbol_to_declare, 
                symbol_to_define,
                define_entities_in_tree);

        (this->*symbol_to_define)(t.get_symbol());
    }
    else if (t.is_pointer())
    {
        walk_type_for_symbols(t.points_to(), /* needs_def */ 1, symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (t.is_pointer_to_member())
    {
        walk_type_for_symbols(t.pointed_class(), /* needs_def */ 1, symbol_to_declare, symbol_to_define,
                define_entities_in_tree);

        walk_type_for_symbols(t.points_to(), /* needs_def */ 1, symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (t.is_array())
    {
        (this->*define_entities_in_tree)(t.array_get_size());
        walk_type_for_symbols(t.array_element(), 
                /* needs_def */ 1, symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (t.is_lvalue_reference()
            || t.is_rvalue_reference())
    {
        walk_type_for_symbols(t.references_to(), 
                needs_def, 
                symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (t.is_function())
    {
        walk_type_for_symbols(t.returns(),
                /* needs_def */ 1, symbol_to_declare, symbol_to_define, define_entities_in_tree);
        TL::ObjectList<TL::Type> params = t.parameters();
        for (TL::ObjectList<TL::Type>::iterator it = params.begin();
                it != params.end();
                it++)
        {
            walk_type_for_symbols(*it,
                    /* needs_def */ 1, 
                    symbol_to_declare,
                    symbol_to_define,
                    &CxxBase::define_nonprototype_entities_in_trees);
        }
    }
    else if (t.is_vector())
    {
        walk_type_for_symbols(t.vector_element(), /* needs_def */ 1, symbol_to_declare, symbol_to_define, define_entities_in_tree);
    }
    else if (t.is_named_class())
    {
        TL::Symbol class_entry = t.get_symbol();
        define_or_declare_if_complete(class_entry, symbol_to_declare, symbol_to_define);
    }
    else if (t.is_unnamed_class())
    {
        // Special case for nested members
        TL::ObjectList<TL::Symbol> members = t.get_all_members();

        for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                it != members.end();
                it++)
        {
            walk_type_for_symbols(it->get_type(), /* needs_def */ 1, symbol_to_declare, symbol_to_define, define_entities_in_tree);
        }
    }
    else if (t.is_unnamed_enum())
    {
        TL::ObjectList<TL::Symbol> enumerators = t.enum_get_enumerators();
        for (TL::ObjectList<TL::Symbol>::iterator it = enumerators.begin();
                it != enumerators.end();
                it++)
        {
            TL::Symbol &enumerator(*it);
            (this->*define_entities_in_tree)(enumerator.get_initialization());
        }
    }
    else if (t.is_named_enum())
    {
        TL::Symbol enum_entry = t.get_symbol();

        walk_type_for_symbols(enum_entry.get_type(), /* needs_def */ 1, symbol_to_declare, symbol_to_define, define_entities_in_tree);
        define_or_declare_if_complete(enum_entry, symbol_to_declare, symbol_to_define);
    }
    else if (t.is_unresolved_overload())
    {
        TL::ObjectList<TL::Symbol> unresolved_set = t.get_unresolved_overload_set();

        for (TL::ObjectList<TL::Symbol>::iterator it = unresolved_set.begin();
                it != unresolved_set.end();
                it++)
        {
            if (needs_def)
            {
                (this->*symbol_to_define)(*it);
            }
            else
            {
                (this->*symbol_to_declare)(*it);
            }
        }
    }
    else if (t.is_dependent_typename())
    {
        Nodecl::NodeclBase nodecl_parts = Nodecl::NodeclBase::null();
        TL::Symbol dependent_entry(NULL);
        t.dependent_typename_get_components(dependent_entry, nodecl_parts);

        if (needs_def
                && !(dependent_entry.is_class()
                    && ::is_incomplete_type(dependent_entry.get_type().get_internal_type())))
        {
            (this->*symbol_to_define)(dependent_entry);
        }
        else
        {
            (this->*symbol_to_declare)(dependent_entry);
        }
    }
    else
    {
        // Do nothing as it should be a builtin type
    }
#undef return

    state.walked_types.erase(t);
}

void CxxBase::set_codegen_status(TL::Symbol sym, codegen_status_t status)
{
    _codegen_status[sym] = status;
}

codegen_status_t CxxBase::get_codegen_status(TL::Symbol sym)
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

void CxxBase::codegen_fill_namespace_list_rec(
        scope_entry_t* namespace_sym, 
        scope_entry_t** list, 
        int* position)
{
    ERROR_CONDITION(namespace_sym == NULL, "Invalid symbol", 0);
    ERROR_CONDITION(namespace_sym->kind != SK_NAMESPACE, "Symbol '%s' is not a namespace", namespace_sym->symbol_name);
    if (namespace_sym == state.global_namespace.get_internal_symbol())
    {
        *position = 0;
    }
    else
    {
        codegen_fill_namespace_list_rec(
                namespace_sym->decl_context.current_scope->related_entry,
                list,
                position);
        ERROR_CONDITION(*position == MCXX_MAX_SCOPES_NESTING, "Too many scopes", 0);
        list[*position] = namespace_sym;
        (*position)++;
    }
}

void CxxBase::codegen_move_namespace_from_to(TL::Symbol from, TL::Symbol to)
{
    scope_entry_t* namespace_nesting_from[MCXX_MAX_SCOPES_NESTING] = { 0 };
    scope_entry_t* namespace_nesting_to[MCXX_MAX_SCOPES_NESTING] = { 0 };

    int num_from = 0;
    codegen_fill_namespace_list_rec(from.get_internal_symbol(), namespace_nesting_from, &num_from);
    int num_to = 0;
    codegen_fill_namespace_list_rec(to.get_internal_symbol(), namespace_nesting_to, &num_to);

    // We only have to close and open the noncommon suffixes
    int common;
    for (common = 0; 
            (common < num_from) 
            && (common < num_to) 
            && (namespace_nesting_from[common] == namespace_nesting_to[common]); 
            common++)
    {
        // Empty body
    }

    int i;
    for (i = common; i < num_from; i++)
    {
        dec_indent();
        indent();
        file << "}\n";
    }

    for (i = common; i < num_to; i++)
    {
        std::string real_name = namespace_nesting_to[i]->symbol_name;
        //
        // Anonymous namespace has special properties that we want to preserve
        if (real_name == "(unnamed)")
        {
            real_name = "/* anonymous */";
        }

        indent();
        file << "namespace " << real_name << " {\n";
        if ((i + 1) < num_from)
        {
            file << " ";
        }
        inc_indent();
    }
}

void CxxBase::move_to_namespace_of_symbol(TL::Symbol symbol)
{
    C_LANGUAGE()
    {
        return;
    }

    // Get the namespace where this symbol has been declared
    scope_t* enclosing_namespace = symbol.get_internal_symbol()->decl_context.namespace_scope;
    scope_entry_t* namespace_sym = enclosing_namespace->related_entry;

    // First close the namespaces
    codegen_move_namespace_from_to(state.opened_namespace, namespace_sym);
    state.opened_namespace = namespace_sym;
}

void CxxBase::indent()
{
    for (int i = 0; i < state._indent_level; i++)
    {
        file << "  ";
    }
}

void CxxBase::inc_indent(int n)
{
    state._indent_level += n;
}

void CxxBase::dec_indent(int n)
{
    state._indent_level -= n;
}

int CxxBase::get_indent_level()
{
    return state._indent_level;
}

void CxxBase::set_indent_level(int n)
{
    state._indent_level = n;
}

void CxxBase::walk_list(const Nodecl::List& list, const std::string& separator)
{
    Nodecl::List::const_iterator it = list.begin();

    while (it != list.end())
    {
        if (it != list.begin())
        {
            file << separator;
        }

        walk(*it);
        it++;
    }
}

void CxxBase::walk_expression_list(const Nodecl::List& node)
{
    walk_list(node, ", ");
}

template <typename Iterator>
void CxxBase::walk_expression_unpacked_list(Iterator begin, Iterator end)
{
    Iterator it = begin;
    while (it != end)
    {
        if (it != begin)
        {
            file << ", ";
        }
        walk(*it);
        it++;
    }
}

int CxxBase::get_rank_kind(node_t n, const std::string& text)
{
    switch (n)
    {
        case NODECL_SYMBOL:
        case NODECL_STRING_LITERAL:
        case NODECL_INTEGER_LITERAL:
        case NODECL_FLOATING_LITERAL:
        case NODECL_BOOLEAN_LITERAL:
        case NODECL_STRUCTURED_VALUE:
        case NODECL_PARENTHESIZED_EXPRESSION:
        case NODECL_COMPOUND_EXPRESSION:

        case NODECL_CXX_DEP_NAME_SIMPLE:
        case NODECL_CXX_DEP_NAME_NESTED:
        case NODECL_CXX_DEP_GLOBAL_NAME_NESTED:
        case NODECL_CXX_DEP_TEMPLATE_ID:
            {
                return -1;
            }
        case NODECL_ARRAY_SUBSCRIPT:
        case NODECL_FUNCTION_CALL:
        case NODECL_CLASS_MEMBER_ACCESS:
        case NODECL_PSEUDO_DESTRUCTOR_NAME:
        case NODECL_TYPEID:
        case NODECL_POSTINCREMENT:
        case NODECL_POSTDECREMENT:
            {
                return -2;
            }
        case NODECL_REFERENCE:
        case NODECL_DERREFERENCE:
        case NODECL_PLUS:
        case NODECL_NEG:
        case NODECL_LOGICAL_NOT:
        case NODECL_BITWISE_NOT:
        case NODECL_SIZEOF:
        case NODECL_NEW:
        case NODECL_DELETE:
        case NODECL_PREINCREMENT:
        case NODECL_PREDECREMENT:
        case NODECL_REAL_PART:
        case NODECL_IMAG_PART:
            // FIXME: Missing GCC nodes 
            // FIXME: Do we want them or we can use builtins?
            // case NODECL_ALIGNOF
            // case NODECL_LABEL_ADDR
            {
                return -3;
            }
            // This one is special as we keep several casts in a single node
        case NODECL_CAST:
            {
                if (IS_C_LANGUAGE
                        || (text == "C"))
                {
                    return -4;
                }
                else
                {
                    // These casts are postfix expressions actually
                    // static_cast, dynamic_cast, reinterpret_cast, const_cast
                    return -2;
                }
            }
            // This is a pointer to member
        case NODECL_OFFSET:
            return -5;
        case NODECL_MUL:
        case NODECL_DIV:
        case NODECL_MOD:
            return -6;
        case NODECL_ADD:
        case NODECL_MINUS:
            return -7;
        case NODECL_SHL:
        case NODECL_SHR:
            return -8;
        case NODECL_LOWER_THAN:
        case NODECL_LOWER_OR_EQUAL_THAN:
        case NODECL_GREATER_THAN:
        case NODECL_GREATER_OR_EQUAL_THAN:
            return -9;
        case NODECL_EQUAL:
        case NODECL_DIFFERENT:
            return -10;
        case NODECL_BITWISE_AND:
            return -11;
        case NODECL_BITWISE_XOR:
            return -12;
        case NODECL_BITWISE_OR:
            return -13;
        case NODECL_LOGICAL_AND:
            return -14;
        case NODECL_LOGICAL_OR:
            return -15;
        case NODECL_CONDITIONAL_EXPRESSION:
            return -16;
        case NODECL_ASSIGNMENT:
        case NODECL_MUL_ASSIGNMENT :
        case NODECL_DIV_ASSIGNMENT:
        case NODECL_ADD_ASSIGNMENT:
        case NODECL_SUB_ASSIGNMENT:
        case NODECL_SHL_ASSIGNMENT:
        case NODECL_SHR_ASSIGNMENT:
        case NODECL_BITWISE_AND_ASSIGNMENT:
        case NODECL_BITWISE_OR_ASSIGNMENT:
        case NODECL_BITWISE_XOR_ASSIGNMENT:
        case NODECL_THROW:
            return -17;
        case NODECL_COMMA:
            return -18;
        default:
            // Lowest priority possible. This is a conservative approach that
            // will work always albeit it will introduce some unnecessary
            // parentheses for unknown expressions
            return -1000;
    }
    return -1000;
}

int CxxBase::get_rank(const Nodecl::NodeclBase &n)
{
    if (n.is<Nodecl::Conversion>())
    {
        return get_rank(n.as<Nodecl::Conversion>().get_nest());
    }
    else
    {
        return get_rank_kind(n.get_kind(), n.get_text());
    }
}

static char is_bitwise_bin_operator(node_t n)
{
    return n == NODECL_BITWISE_AND
        || n == NODECL_BITWISE_OR
        || n == NODECL_BITWISE_XOR;
}

static char is_logical_bin_operator(node_t n)
{
    return n == NODECL_LOGICAL_AND
        || n == NODECL_LOGICAL_OR;
}

static char is_shift_bin_operator(node_t n)
{
    return n == NODECL_SHL
        || n == NODECL_SHR;
}

static char is_additive_bin_operator(node_t n)
{
    return n == NODECL_ADD
        || n == NODECL_MINUS;
}

bool CxxBase::same_operation(Nodecl::NodeclBase current_operator, Nodecl::NodeclBase operand)
{
    if (current_operator.is<Nodecl::Conversion>())
    {
        current_operator = current_operator.as<Nodecl::Conversion>().get_nest();
    }
    if (operand.is<Nodecl::Conversion>())
    {
        operand = operand.as<Nodecl::Conversion>().get_nest();
    }

    int rank_current = get_rank(current_operator);
    int rank_operand = get_rank(operand);

    return (current_operator.get_kind() == operand.get_kind())
        || (rank_current == rank_operand);
}

bool CxxBase::operand_has_lower_priority(Nodecl::NodeclBase current_operator, Nodecl::NodeclBase operand)
{
    if (current_operator.is<Nodecl::Conversion>())
    {
        current_operator = current_operator.as<Nodecl::Conversion>().get_nest();
    }
    if (operand.is<Nodecl::Conversion>())
    {
        operand = operand.as<Nodecl::Conversion>().get_nest();
    }

    int rank_current = get_rank(current_operator);
    int rank_operand = get_rank(operand);

    node_t current_kind = current_operator.get_kind();
    node_t operand_kind = operand.get_kind();

    // For the sake of clarity and to avoid warnings emitted by gcc
    if (0 
            // a || b && c  -> a || (b && c)
            || (is_logical_bin_operator(current_kind) && is_logical_bin_operator(operand_kind))
            // a | b & c  -> a | (b & c)
            || (is_bitwise_bin_operator(current_kind) && is_bitwise_bin_operator(operand_kind))
            // a << b - c   -> a << (b - c)
            || (is_shift_bin_operator(current_kind) && is_additive_bin_operator(operand_kind))
            // a + b & c -> (a + b) & c
            || (is_bitwise_bin_operator(current_kind) && is_additive_bin_operator(operand_kind))
            )
    {
        return 1;
    }

    return rank_operand < rank_current;
}

std::string CxxBase::quote_c_string(int* c, int length, char is_wchar)
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
            result += "\\?";
        }
        else if (current ==  '\\')
        {
            result += "\\\\";
        }
        else if (current ==  '\a')
        {
            result += "\\a";
        }
        else if (current ==  '\b')
        {
            result += "\\b";
        }
        else if (current ==  '\f')
        {
            result += "\\f";
        }
        else if (current ==  '\n')
        {
            result += "\\n";
        }
        else if (current ==  '\r')
        {
            result += "\\r";
        }
        else if (current ==  '\t')
        {
            result += "\\t";
        }
        else if (current ==  '\v')
        {
            result += "\\v";
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
                ss << "\\" 
                    << std::oct << std::setw(3) << std::setfill('0') 
                    << (unsigned int) current 
                    << std::setw(0) << std::dec;
                result += ss.str();
            }
            else
            {
                ss << "\\U" 
                    << std::hex << std::setw(8) << std::setfill('0') 
                    << current 
                    << std::dec << std::setw(0);
                result += ss.str();
            }
        }
    }

    result += "\"";

    return result;
}

bool CxxBase::nodecl_calls_to_constructor(const Nodecl::NodeclBase& node, TL::Type t)
{
    if (node.is<Nodecl::FunctionCall>())
    {
        TL::Symbol called_sym = node.as<Nodecl::FunctionCall>().get_called().get_symbol();

        if (called_sym.is_valid()
                && called_sym.is_constructor())
        {
            return (!t.is_valid())
                || (t.no_ref()
                        .get_unqualified_type()
                        .is_same_type(called_sym.get_class_type().get_unqualified_type()));
        }
    }
    return 0;
}

bool CxxBase::nodecl_is_zero_args_call_to_constructor(Nodecl::NodeclBase node)
{
    return (nodecl_calls_to_constructor(node, TL::Type(NULL))
            && node.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>().empty());
}

bool CxxBase::nodecl_is_zero_args_structured_value(Nodecl::NodeclBase node)
{
    return (node.is<Nodecl::StructuredValue>()
            && (node.as<Nodecl::StructuredValue>().get_items().is_null()
                || node.as<Nodecl::StructuredValue>().get_items().as<Nodecl::List>().empty()));
}


std::string CxxBase::unmangle_symbol_name(TL::Symbol symbol)
{
    return ::unmangle_symbol_name(symbol.get_internal_symbol());
}

void CxxBase::declare_all_in_template_arguments(TL::TemplateParameters template_arguments)
{
    int i, n = template_arguments.get_num_parameters();

    for (i = 0; i < n; i++)
    {
        TL::TemplateArgument argument = template_arguments.get_argument_num(i);

        switch (argument.get_kind())
        {
            case TPK_TYPE:
                {
                    walk_type_for_symbols(
                            argument.get_type(),
                            /* needs_def */ 1,
                            &CxxBase::declare_symbol_if_nonnested,
                            &CxxBase::define_symbol_if_nonnested,
                            &CxxBase::define_nonnested_entities_in_trees);
                    break;
                }
            case TPK_NONTYPE:
                {
                    walk_type_for_symbols(
                            argument.get_type(),
                            /* needs_def */ 1,
                            &CxxBase::declare_symbol_if_nonnested,
                            &CxxBase::define_symbol_if_nonnested,
                            &CxxBase::define_nonnested_entities_in_trees);
                    define_nonnested_entities_in_trees(argument.get_value());
                    break;
                }
            case TPK_TEMPLATE:
                {
                    declare_symbol_always(argument.get_type().get_symbol());
                    break;
                }
            default:
                {
                    internal_error("Code unreachable", 0);
                }
        }
    }
}

void CxxBase::declare_all_in_template_header(TL::TemplateParameters template_parameters)
{
    for (int i = 0; i < template_parameters.get_num_parameters(); i++)
    {
        std::pair<TL::Symbol,
            TL::TemplateParameters::TemplateParameterKind>
                tpl_param = template_parameters.get_parameter_num(i);
        TL::Symbol symbol = tpl_param.first;

        switch (tpl_param.second)
        {
            case TPK_NONTYPE:
                {
                    walk_type_for_symbols(
                            tpl_param.first.get_type(),
                            /* needs_def */ 1,
                            &CxxBase::declare_symbol_if_nonnested,
                            &CxxBase::define_symbol_if_nonnested,
                            &CxxBase::define_nonnested_entities_in_trees);
                    break;
                }
            case TPK_TYPE:
            case TPK_TEMPLATE:
                {
                    break;
                }
            default:
                {
                    internal_error("Invalid template parameter kind", 0);
                }
        }

        if (template_parameters.has_argument(i))
        {
            TL::TemplateArgument temp_arg =
                template_parameters.get_argument_num(i);
            if (temp_arg.is_default())
            {
                switch (tpl_param.second)
                {
                    case TPK_TYPE:
                        {
                            TL::Type temp_arg_type = temp_arg.get_type();
                            walk_type_for_symbols(
                                    temp_arg_type,
                                    /* needs_def */ 1,
                                    &CxxBase::declare_symbol_if_nonnested,
                                    &CxxBase::define_symbol_if_nonnested,
                                    &CxxBase::define_nonnested_entities_in_trees);

                            break;
                        }
                    case TPK_TEMPLATE:
                    case TPK_NONTYPE:
                        {
                            Nodecl::NodeclBase nodecl_arg = temp_arg.get_value();
                            define_all_entities_in_trees(nodecl_arg);
                            break;
                        }
                    default:
                        {
                            internal_error("code unreachable", 0);
                        }
                }
            }
        }
    }
}


void CxxBase::codegen_template_headers_bounded(TL::TemplateParameters template_parameters, TL::TemplateParameters lim)
{
    if (template_parameters != lim)
    {
        if (template_parameters.has_enclosing_parameters())
        {
            TL::TemplateParameters enclosing_template_parameters = template_parameters.get_enclosing_parameters();
            codegen_template_headers_bounded(enclosing_template_parameters, lim);
        }
        codegen_template_header(template_parameters);
    }
}

void CxxBase::codegen_template_headers_all_levels(TL::TemplateParameters template_parameters)
{
    if (template_parameters.has_enclosing_parameters())
    {
        TL::TemplateParameters enclosing_template_parameters = template_parameters.get_enclosing_parameters();
        codegen_template_headers_all_levels(enclosing_template_parameters);
    }
    codegen_template_header(template_parameters);
}

void CxxBase::codegen_template_header(TL::TemplateParameters template_parameters, bool endline)
{
    indent();
    if (template_parameters.get_is_explicit_specialization())
    {
        file << "template <>";
        if (endline)
            file << "\n";
        return;
    }

    // First traversal to ensure that everything is declared
    declare_all_in_template_header(template_parameters);
    file << "template < ";
    for (int i = 0; i < template_parameters.get_num_parameters(); i++)
    {
        std::pair<TL::Symbol,
                  TL::TemplateParameters::TemplateParameterKind>
                      tpl_param = template_parameters.get_parameter_num(i);
        TL::Symbol symbol = tpl_param.first;

        if (i != 0)
        {
            file << ", ";
        }

        switch (tpl_param.second)
        {
            case TPK_TYPE:
                {
                    file << "typename " << symbol.get_name();
                    break;
                }
            case TPK_NONTYPE:
                {
                    std::string declaration = this->get_declaration(symbol.get_type(),
                            symbol.get_scope(),
                            symbol.get_name());

                    file << declaration;
                    break;
                }
            case TPK_TEMPLATE:
                {
                    TL::Type template_type = symbol.get_type();
                    codegen_template_header(symbol.get_type().template_type_get_template_parameters(), /* endline */ false);
                    file << " class " << symbol.get_name();
                    break;
                }
            default:
                {
                    internal_error("Invalid template parameter kind", 0);
                }
        }

        // Has this template parameter a default value?
        if (template_parameters.has_argument(i))
        {
            TL::TemplateArgument temp_arg = template_parameters.get_argument_num(i);
            if (temp_arg.is_default())
            {
                file << " = ";
                switch (tpl_param.second)
                {
                    case TPK_TYPE:
                    case TPK_TEMPLATE:
                        {
                            TL::Type temp_arg_type = temp_arg.get_type();
                            file << print_type_str(temp_arg_type.get_internal_type(), symbol.get_scope().get_decl_context());
                            break;
                        }
                    case TPK_NONTYPE:
                        {
                            push_scope(symbol.get_scope());
                            walk(temp_arg.get_value());
                            pop_scope();
                            break;
                        }
                    default:
                        {
                            internal_error("code unreachable", 0);
                        }
                }
            }
        }
    }

    file << " >";
    if (endline)
        file << "\n";
}

std::string CxxBase::gcc_attributes_to_str(TL::Symbol symbol)
{
    std::string result;
    TL::ObjectList<TL::GCCAttribute> gcc_attr_list = symbol.get_gcc_attributes();
    int i = 0;
    for (TL::ObjectList<TL::GCCAttribute>::iterator it = gcc_attr_list.begin();
            it != gcc_attr_list.end();
            it++, i++)
    {
        if (i > 0) 
            result += " ";

        if (it->get_expression_list().is_null())
        {
            result += "__attribute__((" + it->get_attribute_name() + "))";
        }
        else
        {
            result += "__attribute__((" + it->get_attribute_name() + "(";

            std::string old_str = file.str();

            file.clear();
            file.str("");

            walk_expression_list(it->get_expression_list().as<Nodecl::List>());

            result += file.str();

            file.clear();
            file.str(old_str);
            // Go to the end of the stream...
            file.seekp(0, std::ios_base::end);

            result += ")))";
        }
    }

    return result;
}

std::string CxxBase::gcc_asm_specifier_to_str(TL::Symbol symbol)
{
    std::string result;
    if (!symbol.get_asm_specification().is_null())
    {
        std::string old_str = file.str();

        file.clear();
        file.str("");

        walk(symbol.get_asm_specification());
        result = file.str();

        file.clear();
        file.str(old_str);
        // Go to the end of the stream...
        file.seekp(0, std::ios_base::end);
    }
    return result;
}

std::string CxxBase::exception_specifier_to_str(TL::Symbol symbol)
{
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
                exception_spec += this->get_declaration(*it, symbol.get_scope(), "");
            }

            exception_spec += ")";
        }
    }
    return exception_spec;
}

std::string CxxBase::template_arguments_to_str(TL::Symbol symbol)
{
    return ::get_template_arguments_str(symbol.get_internal_symbol(), symbol.get_scope().get_decl_context());
}

CxxBase::Ret CxxBase::unhandled_node(const Nodecl::NodeclBase & n)
{
    indent();
    file << "/* >>> " << ast_print_node_type(n.get_kind()) << " >>> */\n";

    inc_indent();

    TL::ObjectList<Nodecl::NodeclBase> children = n.children();

    int i = 0;
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++, i++)
    {
        indent();
        file << "/* Children " << i << " */\n";

        walk(*it);
    }

    dec_indent();

    indent();
    file << "/* <<< " << ast_print_node_type(n.get_kind()) << " <<< */\n";
}

std::string CxxBase::get_declaration(TL::Type t, TL::Scope scope, const std::string& name)
{
    t = fix_references(t);

    return t.get_declaration(scope,  name);
}

std::string CxxBase::get_declaration_with_parameters(TL::Type t, TL::Scope scope,
        const std::string& name, TL::ObjectList<std::string>& names, TL::ObjectList<std::string>& parameter_attributes)
{
    t = fix_references(t);

    return t.get_declaration_with_parameters(scope, name, names, parameter_attributes);
}

TL::Type CxxBase::fix_references(TL::Type t)
{
    if (is_non_language_reference_type(t))
    {
        TL::Type ref = t.references_to();
        if (ref.is_array())
        {
            // T (&a)[10] -> T * const 
            // T (&a)[10][20] -> T (* const)[20]
            ref = ref.array_element();
        }
        
        // T &a -> T * const a
        TL::Type ptr = ref.get_pointer_to();
        if (!t.is_rebindable_reference())
        {
            ptr = ptr.get_const_type();
        }
        return ptr;
    }
    else if (t.is_array())
    {
        // Arrays should not have references as elements
        return t;
    }
    else if (t.is_pointer())
    {
        // Pointers cannot point to references
        return t;
    }
    else if (t.is_function())
    {
        // Do not fix unprototyped functions
        if (t.lacks_prototype())
            return t;

        cv_qualifier_t cv_qualif = get_cv_qualifier(t.get_internal_type());
        TL::Type fixed_result = fix_references(t.returns());
        bool has_ellipsis = 0;
        TL::ObjectList<TL::Type> fixed_parameters = t.parameters(has_ellipsis);

        for (TL::ObjectList<TL::Type>::iterator it = fixed_parameters.begin();
                it != fixed_parameters.end();
                it++)
        {
            *it = fix_references(*it);
        }

        TL::Type fixed_function = fixed_result.get_function_returning(fixed_parameters, has_ellipsis);

        fixed_function = TL::Type(get_cv_qualified_type(fixed_function.get_internal_type(), cv_qualif));

        return fixed_function;
    }
    // Note: we are not fixing classes
    else
    {
        // Anything else must be left untouched
        return t;
    }
}

} // Codegen

EXPORT_PHASE(Codegen::CxxBase)
