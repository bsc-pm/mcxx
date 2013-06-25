/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
#include "codegen-prune.hpp"
#include "tl-objectlist.hpp"
#include "tl-type.hpp"
#include "cxx-cexpr.h"
#include "cxx-entrylist.h"
#include "string_utils.h"
#include "tl-compilerpipeline.hpp"
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

    state.nontype_template_argument_needs_parentheses =
        old_state.nontype_template_argument_needs_parentheses;

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

void CxxBase::codegen_cleanup()
{
    // In some cases, we use the same codegen object to compile one or more
    // sources (for example, it happens in ompss transformation). For this
    // reason, we need to restore the codegen status of every symbol.
    _codegen_status.clear();
}

void CxxBase::handle_parameter(int n, void* data)
{
     switch (n)
     {
         case CODEGEN_PARAM_NONTYPE_TEMPLATE_ARGUMENT:
             {
                 ERROR_CONDITION(data == NULL, "data cannot be NULL\n", 0);
                 state.nontype_template_argument_needs_parentheses = *((char *)data);
                 break;
             };
         default:
             {
                 internal_error("undefined codegen parameter\n", 0);
                 break;
             };
     }
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
    PREFIX_UNARY_EXPRESSION(Dereference, "*") \
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
    BINARY_EXPRESSION_EX(GreaterThan, " > ") \
    BINARY_EXPRESSION_EX(GreaterOrEqualThan, " >= ") \
    BINARY_EXPRESSION(LogicalAnd, " && ") \
    BINARY_EXPRESSION(LogicalOr, " || ") \
    BINARY_EXPRESSION(BitwiseAnd, " & ") \
    BINARY_EXPRESSION(BitwiseOr, " | ") \
    BINARY_EXPRESSION(BitwiseXor, " ^ ") \
    BINARY_EXPRESSION(BitwiseShl, " << ") \
    BINARY_EXPRESSION_EX(BitwiseShr, " >> ") \
    BINARY_EXPRESSION_EX(ArithmeticShr, " >> ") \
    BINARY_EXPRESSION_ASSIG(Assignment, " = ") \
    BINARY_EXPRESSION_ASSIG(MulAssignment, " *= ") \
    BINARY_EXPRESSION_ASSIG(DivAssignment, " /= ") \
    BINARY_EXPRESSION_ASSIG(AddAssignment, " += ") \
    BINARY_EXPRESSION_ASSIG(MinusAssignment, " -= ") \
    BINARY_EXPRESSION_ASSIG(BitwiseShlAssignment, " <<= ") \
    BINARY_EXPRESSION_ASSIG_EX(BitwiseShrAssignment, " >>= ") \
    BINARY_EXPRESSION_ASSIG_EX(ArithmeticShrAssignment, " >>= ") \
    BINARY_EXPRESSION_ASSIG(BitwiseAndAssignment, " &= ") \
    BINARY_EXPRESSION_ASSIG(BitwiseOrAssignment, " |= ") \
    BINARY_EXPRESSION_ASSIG(BitwiseXorAssignment, " ^= ") \
    BINARY_EXPRESSION_ASSIG(ModAssignment, " %= ") \
    BINARY_EXPRESSION(Offset, ".*") \
    BINARY_EXPRESSION(CxxDotPtrMember, ".*") \
    BINARY_EXPRESSION(CxxArrowPtrMember, "->*") \

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
    return this->is_file_output()
        && ((IS_C_LANGUAGE && type.is_any_reference())
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
    Nodecl::NodeclBase rhs = node.get_rhs();
    char needs_parentheses = operand_has_lower_priority(node, rhs);

    bool old_do_not_derref_rebindable_ref = state.do_not_derref_rebindable_reference;

    if (rhs.get_type().is_rebindable_reference())
    {
        state.do_not_derref_rebindable_reference = true;
    }
    else if (is_non_language_reference_variable(rhs))
    {
        state.do_not_derref_rebindable_reference = true;

        // Emit a casting here
        file << "(" << this->get_declaration(node.get_type(), this->get_current_scope(), "") << ") ";
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
        BINARY_EXPRESSION_IMPL(_name, _operand) \
    }

#define BINARY_EXPRESSION_IMPL(_name, _operand) \
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
   }

// In some cases (i. e. when the operator of a binary expression contains the
// character '>') nontype template arguments may need an extra parentheses
// Example:
//      template < bool b>
//      struct A {};
//      A < (3 > 2) > a;
#define BINARY_EXPRESSION_EX(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name& node) \
    { \
        if (state.nontype_template_argument_needs_parentheses) \
        {\
            file << "("; \
        }\
        BINARY_EXPRESSION_IMPL(_name, _operand) \
        if (state.nontype_template_argument_needs_parentheses) \
        {\
            file << ")"; \
        }\
    }

#define BINARY_EXPRESSION_ASSIG(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name& node) \
    { \
        BINARY_EXPRESSION_ASSIG_IMPL(_name, _operand) \
    }

#define BINARY_EXPRESSION_ASSIG_IMPL(_name, _operand) \
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

// In some cases (i. e. when the operator of a binary expression assignment
// contains the character '>') nontype template arguments may need an extra
// parentheses
#define BINARY_EXPRESSION_ASSIG_EX(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name& node) \
    { \
        if (state.nontype_template_argument_needs_parentheses) \
        {\
            file << "("; \
        }\
        BINARY_EXPRESSION_ASSIG_IMPL(_name, _operand) \
        if (state.nontype_template_argument_needs_parentheses) \
        {\
            file << ")"; \
        }\
    }

OPERATOR_TABLE
#undef POSTFIX_UNARY_EXPRESSION
#undef PREFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION
#undef BINARY_EXPRESSION_IMPL
#undef BINARY_EXPRESSION_EX
#undef BINARY_EXPRESSION_ASSIG
#undef BINARY_EXPRESSION_ASSIG_IMPL
#undef BINARY_EXPRESSION_ASSIG_EX

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
    TL::Type t = fix_references(node.get_type());
    Nodecl::NodeclBase nest = node.get_rhs();

    if (IS_C_LANGUAGE
            || cast_kind == "C")
    {
        bool is_non_ref = is_non_language_reference_type(node.get_type());
        if (is_non_ref)
        {
            if (node.get_type().no_ref().is_array())
            {
                // Special case for arrays, themselves are an address
                file << "(";
            }
            else
            {
                file << "(*";
            }

            // This avoids a warning in some compilers which complain on (T* const)e
            t = t.get_unqualified_type();
        }
        file << "(" << this->get_declaration(t, this->get_current_scope(),  "") << ")";

        if (is_non_ref)
        {
            file << "&(";
        }

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

        if (is_non_ref)
        {
            file << "))";
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
    Nodecl::NodeclBase member_form = node.get_member_form();

    TL::Symbol sym = rhs.get_symbol();

    /*
     *     .
     *    / \
     *   .   c
     *  / \
     * a   <<anon>>
     *
     * We want a.<<anon>>.c become a.c
     */

    bool is_anonymous_union_accessor = sym.is_valid()
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

    if (!is_anonymous_union_accessor)
    {
        // Right part can be a reference but we do not want to derref it
        state.do_not_derref_rebindable_reference = true;

        file << "."
            << /* template tag if needed */ node.get_text();

        needs_parentheses = operand_has_lower_priority(node, rhs);
        if (needs_parentheses)
        {
            file << "(";
        }

        // This will only emit the unqualified name
        TL::Symbol rhs_symbol = rhs.get_symbol();
        if (!rhs_symbol.is_valid()
                || (!member_form.is_null()
                    && member_form.is<Nodecl::CxxMemberFormQualified>()))
        {
            // This will print the qualified name
            walk(rhs);
        }
        else
        {

            // Simply print the name
            file << rhs_symbol.get_name();
        }

        if (needs_parentheses)
        {
            file << ")";
        }

        state.do_not_derref_rebindable_reference = old_do_not_derref_rebindable_ref;
    }

    if (must_derref_all)
    {
        file << ")";
    }
}

void CxxBase::visit(const Nodecl::Comma & node)
{
    file << "(";

    Nodecl::NodeclBase lhs = node.children()[0];
    Nodecl::NodeclBase rhs = node.children()[1];
    if (state.in_condition && state.condition_top == node)
    {
        file << "(";
    }
    char needs_parentheses = operand_has_lower_priority(node, lhs) || same_operation(node, lhs);
    if (needs_parentheses)
    {
        file << "(";
    }
    walk(lhs);
    if (needs_parentheses)
    {
        file << ")";
    }
    file << ", ";
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
    if (state.in_condition && state.condition_top == node)
    {
        file << ")";
    }

    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::ComplexLiteral& node)
{
    const_value_t* cval = node.get_constant();

    const_value_t* real_part = const_value_complex_get_real_part(cval);
    const_value_t* imag_part = const_value_complex_get_imag_part(cval);

    ERROR_CONDITION(!const_value_is_zero(real_part), "Invalid complex constant in C with nonzero real component", 0);

    if (const_value_is_integer(imag_part))
    {
        emit_integer_constant(imag_part, node.get_type().complex_get_base_type());
    }
    else if (const_value_is_floating(imag_part))
    {
        emit_floating_constant(imag_part, node.get_type().complex_get_base_type());
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

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

    if (get_rank(cond) < get_rank_kind(NODECL_LOGICAL_OR, ""))
    {
        // This expression is a logical-or-expression, so an assignment (or comma)
        // needs parentheses
        file << "(";
    }
    walk(cond);
    if (get_rank(cond) < get_rank_kind(NODECL_LOGICAL_OR, ""))
    {
        file << ")";
    }

    file << " ? ";

    // This is a top level expression, no parentheses should be required
    walk(then);

    file << " : ";

    if (get_rank(cond) < get_rank_kind(NODECL_ASSIGNMENT, ""))
    {
        // Only comma operator could get here actually
        file << "(";
    }
    walk(_else);
    if (get_rank(cond) < get_rank_kind(NODECL_ASSIGNMENT, ""))
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

CxxBase::Ret CxxBase::visit(const Nodecl::CxxArrow& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_member();

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

    file << "->"
         << /* template tag if needed */ node.get_text();


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

CxxBase::Ret CxxBase::visit(const Nodecl::CxxBracedInitializer& node)
{
    file << "{ ";
    if (!node.get_init().is_null())
    {
        walk_list(node.get_init().as<Nodecl::List>(), ", ");
    }
    file << " }";
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
    TL::TemplateParameters tpl = node.get_template_parameters();

    file << ::template_arguments_to_str(
            tpl.get_internal_template_parameter_list(),
            /* first_template_argument_to_be_printed */ 0,
            /* print_first_level_bracket */ 1,
            this->get_current_scope().get_decl_context());

    // The function 'template_arguments_to_str' does not print anything when
    // template arguments are empty. For this reason, we add the empty list '<>'
    if (tpl.get_num_parameters() == 0)
    {
        file << "<>";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxInitializer& node)
{
    walk(node.get_init());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxEqualInitializer& node)
{
    file << " = ";
    walk(node.get_init());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxMemberInit& node)
{
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

CxxBase::Ret CxxBase::visit(const Nodecl::CxxAlignof& node)
{
    if (IS_CXX1X_LANGUAGE)
    {
        file << "alignof(";
    }
    else
    {
        file << "__alignof__(";
    }
    walk(node.get_expr());
    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxSizeof& node)
{
    file << "sizeof(";
    walk(node.get_expr());
    file << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::DefaultArgument& node)
{
    internal_error("Code unreachable", 0);
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
                node.get_locus_str().c_str());
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

void CxxBase::emit_floating_constant(const_value_t* cval, TL::Type t)
{
    int precision = floating_type_get_info(t.get_internal_type())->p + 1;

    // FIXME - We are relying on a (really handy) GNU extension
    if (const_value_is_float(cval))
    {
        const char* floating = NULL;
        uniquestr_sprintf(&floating, "%.*ef", precision, const_value_cast_to_float(cval));
        file << floating;
    }
    else if (const_value_is_double(cval))
    {
        const char* floating = NULL;
        uniquestr_sprintf(&floating, "%.*e", precision, const_value_cast_to_double(cval));
        file << floating;
    }
    else if (const_value_is_long_double(cval))
    {
        const char* floating = NULL;
        uniquestr_sprintf(&floating, "%.*LeL", precision, const_value_cast_to_long_double(cval));
        file << floating;
    }
#ifdef HAVE_QUADMATH_H
    else if (const_value_is_float128(cval))
    {
        __float128 f128 = const_value_cast_to_float128(cval);
        int n = quadmath_snprintf (NULL, 0, "%.*Qe", precision, f128);
        char *c = new char[n + 1];
        quadmath_snprintf (c, n, "%.*Qe", precision, f128);
        c[n] = '\0';
        file << c << "Q";
        delete[] c;
    }
#endif
}

CxxBase::Ret CxxBase::visit(const Nodecl::FloatingLiteral& node)
{
    const_value_t* value = nodecl_get_constant(node.get_internal_nodecl());
    ERROR_CONDITION(value == NULL, "Invalid value", 0);

    emit_floating_constant(value, nodecl_get_type(node.get_internal_nodecl()));
}

void CxxBase::emit_range_loop_header(
        Nodecl::RangeLoopControl lc,
        Nodecl::NodeclBase statement,
        const std::string& rel_op)
{
    TL::Symbol ind_var = lc.get_induction_variable().get_symbol();
    std::string ind_var_name = this->get_qualified_name(ind_var);

    indent();
    file << "for (";

    // I = L
    file << ind_var_name;
    file << " = ";
    walk(lc.get_lower());
    file << "; ";

    // I <= L     (or   I >= L)
    file << ind_var_name << rel_op;
    walk(lc.get_upper());
    file << "; ";

    // I += S
    file << ind_var_name << " += ";
    if (!lc.get_step().is_null())
        walk(lc.get_step());
    else
        file << "1";

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

        if (step.is_null()
                || step.is_constant())
        {
            std::string rel_op = " <= ";
            const_value_t* v = NULL;
            if (step.is_null())
            {
                v = const_value_get_signed_int(1);
            }
            else
            {
                v = step.get_constant();
            }
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
CxxBase::Ret CxxBase::codegen_function_call_arguments(
        Iterator begin,
        Iterator end,
        TL::Type function_type,
        int ignore_n_first)
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
    bool default_arguments = false;
    while (arg_it != end)
    {
        Nodecl::NodeclBase actual_arg = *arg_it;

        if (actual_arg.is<Nodecl::DefaultArgument>())
        {
            // Default arguments are printed in a comment
            if (!default_arguments)
            {
                file << "/* ";
                default_arguments = true;
            }

            actual_arg = actual_arg.as<Nodecl::DefaultArgument>().get_argument();
        }

        if (arg_it != begin)
        {
            file << ", ";
        }

        bool old_do_not_derref_rebindable_ref = state.do_not_derref_rebindable_reference;
        state.do_not_derref_rebindable_reference = false;

        bool do_reference = false;

        if (type_it != type_end
                && type_it->is_valid())
        {
            while (actual_arg.is<Nodecl::Conversion>())
            {
                actual_arg = actual_arg.as<Nodecl::Conversion>().get_nest();
            }

            bool param_is_ref = is_non_language_reference_type(*type_it);

            bool arg_is_ref = is_non_language_reference_variable(actual_arg);

            if (param_is_ref && !arg_is_ref)
            {
                if (actual_arg.is<Nodecl::Dereference>())
                {
                    // Consider this case                 [ Emitted C ]
                    // void f(int (&v)[10])    ->         void f(int * const v)
                    // {
                    // }
                    //
                    // void g()                           void g()
                    // {                                  {
                    //    int (*k)[10];                      int (*k)[10];
                    //    f(*k);                             f(*k); // Emitting f(k) would be wrong
                    // }                                  }
                    //
                    // Note that "*k" has type "int[10]" but then it gets converted into "int*"
                    // conversely "k" is just "int(*)[10]" but this cannot be converted into "int*"
                    bool is_array_argument =
                        (actual_arg.get_type().is_array()
                         || (actual_arg.get_type().is_any_reference()
                             && actual_arg.get_type().references_to().is_array()));
                    if (!is_array_argument)
                    {
                        actual_arg = actual_arg.as<Nodecl::Dereference>().get_rhs();
                    }
                }
                else if (type_it->references_to().is_array()
                        && actual_arg.get_type().is_valid()
                        && actual_arg.get_type().no_ref().is_array())
                {
                    // Consider this case                 [ Emitted C ]
                    // void f(int (&v)[10])    ->         void f(int * const v)
                    // {
                    // }
                    //
                    // void g()                           void g()
                    // {                                  {
                    //    int k[10];                         int k[10];
                    //    f(k);                              f(k); // Emitting f(&k) would yield a wrong
                    //                                             // type (with the proper value, though)
                    // }                                  }
                    //
                    // Note that "k" has type "int[10]" (not a reference)
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

    // Close the comment if needed
    if (default_arguments)
        file << " */";
}

template <typename Node>
void CxxBase::visit_function_call_form_template_id(const Node& node)
{
    Nodecl::NodeclBase function_form = node.get_function_form();
    TL::Symbol called_symbol = node.get_called().get_symbol();

    if (!function_form.is_null()
            && function_form.is<Nodecl::CxxFunctionFormTemplateId>())
    {
        TL::TemplateParameters template_args = function_form.get_template_parameters();
        TL::TemplateParameters deduced_template_args =
            called_symbol.get_type().template_specialized_type_get_template_arguments();

        if (template_args.get_num_parameters() == deduced_template_args.get_num_parameters())
        {
            // First case: the user's code specifies all template arguments
            file << ::template_arguments_to_str(
                    template_args.get_internal_template_parameter_list(),
                    /* first_template_argument_to_be_printed */ 0,
                    /* print_first_level_bracket */ 1,
                    called_symbol.get_scope().get_decl_context());
        }
        else
        {
            // Second case: the user's code specifies some template arguments but not all
            std::string template_args_str =
                ::template_arguments_to_str(
                        template_args.get_internal_template_parameter_list(),
                        /* first_template_argument_to_be_printed */ 0,
                        /* print_first_level_bracket */ 0,
                        called_symbol.get_scope().get_decl_context());

            std::string deduced_template_args_str =
                ::template_arguments_to_str(
                        deduced_template_args.get_internal_template_parameter_list(),
                        /* first_template_argument_to_be_printed */ template_args.get_num_parameters(),
                        /* print_first_level_bracket */ 1,
                        called_symbol.get_scope().get_decl_context());

            // Reason of this: A<::B> it's not legal
            if (template_args_str.length() != 0 && template_args_str[0] == ':')
            {
                file << "< ";
            }
            else
            {
                file << "<";
            }
            file << template_args_str << "/*, " << deduced_template_args_str <<"*/>";
        }
    }
    else
    {
        // Third case: the user's code does not specify any template argument
        // We generate a comment with the deduced template arguments
        if (called_symbol != NULL
                && called_symbol.get_type().is_template_specialized_type())
        {
            TL::TemplateParameters deduced_template_args =
                called_symbol.get_type().template_specialized_type_get_template_arguments();
            file << "/*";
            file << ::template_arguments_to_str(
                    deduced_template_args.get_internal_template_parameter_list(),
                    /* first_template_argument_to_be_printed */ 0,
                    /* print_first_level_bracket */ 1,
                    called_symbol.get_scope().get_decl_context());
            file << "*/";
        }
    }
}

// Explicit specialitzation for Nodecl::CxxDepFunctionCall because this kind of node has not a function form
template <>
void CxxBase::visit_function_call_form_template_id<Nodecl::CxxDepFunctionCall>(const Nodecl::CxxDepFunctionCall& node)
{
}


template <typename Node>
bool CxxBase::is_implicit_function_call(const Node& node) const
{
    return (!node.get_function_form().is_null()
            && node.get_function_form().template is<Nodecl::CxxFunctionFormImplicit>());
}

// Explicit specialitzation for Nodecl::CxxDepFunctionCall because this kind of node has not a function form
template <>
bool CxxBase::is_implicit_function_call<Nodecl::CxxDepFunctionCall>(const Nodecl::CxxDepFunctionCall& node) const
{
    return 0;
}


template <typename Node>
bool CxxBase::is_binary_infix_operator_function_call(const Node& node)
{
    return (!node.get_function_form().is_null()
            && node.get_function_form().template is<Nodecl::CxxFunctionFormBinaryInfix>());
}

// Explicit specialitzation for Nodecl::CxxDepFunctionCall because this kind of node has not a function form
template <>
bool CxxBase::is_binary_infix_operator_function_call<Nodecl::CxxDepFunctionCall>(const Nodecl::CxxDepFunctionCall& node)
{
    return 0;
}


template <typename Node>
bool CxxBase::is_unary_prefix_operator_function_call(const Node& node)
{
    return (!node.get_function_form().is_null()
            && node.get_function_form().template is<Nodecl::CxxFunctionFormUnaryPrefix>());
}

// Explicit specialitzation for Nodecl::CxxDepFunctionCall because this kind of node has not a function form
template <>
bool CxxBase::is_unary_prefix_operator_function_call<Nodecl::CxxDepFunctionCall>(const Nodecl::CxxDepFunctionCall& node)
{
    return 0;
}


template <typename Node>
bool CxxBase::is_unary_postfix_operator_function_call(const Node& node)
{
    return (!node.get_function_form().is_null()
            && node.get_function_form().template is<Nodecl::CxxFunctionFormUnaryPostfix>());
}

// Explicit specialitzation for Nodecl::CxxDepFunctionCall because this kind of node has not a function form
template <>
bool CxxBase::is_unary_postfix_operator_function_call<Nodecl::CxxDepFunctionCall>(const Nodecl::CxxDepFunctionCall& node)
{
    return 0;
}


template <typename Node>
bool CxxBase::is_operator_function_call(const Node& node)
{
    return (is_unary_prefix_operator_function_call(node)
            || is_unary_postfix_operator_function_call(node)
            || is_binary_infix_operator_function_call(node));
}

template <typename Node>
CxxBase::Ret CxxBase::visit_function_call(const Node& node, bool is_virtual_call)
{

    Nodecl::NodeclBase called_entity = node.get_called();

    if (is_implicit_function_call(node))
    {
        // We don't want to generate the current function call because It has
        // been added by the compiler. We should ignore it!
        if (!node.get_arguments().is_null())
        {
            walk(node.get_arguments().template as<Nodecl::List>()[0]);
        }
        return;
    }

    TL::Symbol called_symbol = called_entity.get_symbol();
    Nodecl::List arguments = node.get_arguments().template as<Nodecl::List>();

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
        walk_expression_list(arguments);
        file << ")";
        return;
    }

    enum call_kind
    {
        INVALID_CALL = 0,
        ORDINARY_CALL,
        NONSTATIC_MEMBER_CALL,
        STATIC_MEMBER_CALL,
        CONSTRUCTOR_INITIALIZATION,
        BINARY_INFIX_OPERATOR,
        UNARY_PREFIX_OPERATOR,
        UNARY_POSTFIX_OPERATOR
    } kind = ORDINARY_CALL;

    if (called_symbol.is_valid())
    {
        if (is_unary_prefix_operator_function_call(node))
        {
            kind = UNARY_PREFIX_OPERATOR;
        }
        else if (is_unary_postfix_operator_function_call(node))
        {
            kind = UNARY_POSTFIX_OPERATOR;
        }
        else if (is_binary_infix_operator_function_call(node))
        {
            kind = BINARY_INFIX_OPERATOR;
        }
        else if (called_symbol.is_function()
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
            else
            {
                kind = STATIC_MEMBER_CALL;
            }
        }
    }

    bool is_non_language_ref = is_non_language_reference_type(function_type.returns());
    if (is_non_language_ref)
        file << "(*(";

    bool old_visiting_called_entity_of_function_call =
        state.visiting_called_entity_of_function_call;

    // We are going to visit the called entity of the current function call.
    // The template arguments of this function (if any) will be printed by the
    // function 'visit_function_call_form_template_id' and not by the visitor
    // of the symbol
    state.visiting_called_entity_of_function_call = true;

    int ignore_n_first_arguments;
    switch (kind)
    {
        case ORDINARY_CALL:
        case STATIC_MEMBER_CALL:
            {
                bool needs_parentheses = operand_has_lower_priority(node, called_entity);
                if (needs_parentheses)
                    file << "(";
                walk(called_entity);
                if (needs_parentheses)
                    file << ")";

                ignore_n_first_arguments = 0;
                break;
            }
        case NONSTATIC_MEMBER_CALL:
            {
                ERROR_CONDITION(!(arguments.size() >= 1), "A nonstatic member call lacks the implicit argument", 0);

                bool needs_parentheses = (get_rank(arguments[0]) < get_rank_kind(NODECL_CLASS_MEMBER_ACCESS, ""));
                if (needs_parentheses)
                    file << "(";
                walk(arguments[0]);
                if (needs_parentheses)
                    file << ")";

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

                ignore_n_first_arguments = 1;
                break;
            }
        case CONSTRUCTOR_INITIALIZATION:
            {
                TL::Symbol class_symbol = called_symbol.get_class_type().get_symbol();
                file << this->get_qualified_name(class_symbol);
                ignore_n_first_arguments = 0;
                break;
            }
        case UNARY_PREFIX_OPERATOR:
            {
                std::string called_operator = called_symbol.get_name().substr(std::string("operator ").size());
                state.visiting_called_entity_of_function_call =
                    old_visiting_called_entity_of_function_call;

                // We need this to avoid - - 1 to become --1
                file << " " << called_operator;

                bool needs_parentheses = operand_has_lower_priority(node, arguments[0]);
                if (needs_parentheses)
                    file << "(";
                walk(arguments[0]);
                if (needs_parentheses)
                    file << ")";

                if (is_non_language_ref)
                    file << "))";

                return;
            }
        case UNARY_POSTFIX_OPERATOR:
            {
                std::string called_operator = called_symbol.get_name().substr(std::string("operator ").size());
                state.visiting_called_entity_of_function_call =
                    old_visiting_called_entity_of_function_call;

                bool needs_parentheses = operand_has_lower_priority(node, arguments[0]);
                if (needs_parentheses)
                    file << "(";

                walk(arguments[0]);

                if (needs_parentheses)
                    file << ")";

                file << called_operator;

                if (is_non_language_ref)
                    file << "))";

                return;
            }
        case BINARY_INFIX_OPERATOR:
            {
                std::string called_operator = called_symbol.get_name().substr(std::string("operator ").size());
                state.visiting_called_entity_of_function_call =
                    old_visiting_called_entity_of_function_call;

                bool needs_parentheses = operand_has_lower_priority(node, arguments[0]);
                if (needs_parentheses)
                    file << "(";
                walk(arguments[0]);
                if (needs_parentheses)
                    file << ")";

                file << " " << called_operator << " ";

                needs_parentheses = operand_has_lower_priority(node, arguments[1]);
                if (needs_parentheses)
                    file << "(";
                walk(arguments[1]);
                if (needs_parentheses)
                    file << ")";

                if (is_non_language_ref)
                    file << "))";

                return;
            }
        default:
            {
                internal_error("Unhandled function call kind", 0);
            }
    }

    visit_function_call_form_template_id(node);

    file << "(";

    state.visiting_called_entity_of_function_call = old_visiting_called_entity_of_function_call;

    codegen_function_call_arguments(arguments.begin(), arguments.end(), function_type, ignore_n_first_arguments);

    file << ")";

    if (is_non_language_ref)
        file << "))";
}

CxxBase::Ret CxxBase::visit(const Nodecl::FunctionCall& node)
{
    visit_function_call(node, /* is_virtual_call */ false);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepFunctionCall& node)
{
    visit_function_call(node, /* is_virtual_call */ false);
}

// Bug in GCC 4.4
template CxxBase::Ret CxxBase::visit_function_call<Nodecl::FunctionCall>(const Nodecl::FunctionCall& node, bool is_virtual_call);
template CxxBase::Ret CxxBase::visit_function_call<Nodecl::CxxDepFunctionCall>(const Nodecl::CxxDepFunctionCall& node, bool is_virtual_call);

CxxBase::Ret CxxBase::visit(const Nodecl::TemplateFunctionCode& node)
{
    // We are going to generate dependent code
    state.in_dependent_template_function_code = true;

    Nodecl::Context context = node.get_statements().as<Nodecl::Context>();
    Nodecl::List statement_seq = context.get_in_context().as<Nodecl::List>();
    Nodecl::NodeclBase initializers = node.get_initializers();

    if (statement_seq.size() != 1)
    {
        internal_error("C/C++ functions only have one statement", 0);
    }

    Nodecl::NodeclBase statement = statement_seq[0];

    TL::Symbol symbol = node.get_symbol();

    // We don't define twice a symbol
    if (get_codegen_status(symbol) == CODEGEN_STATUS_DEFINED)
        return;

    // Two return cases for C++:
    //  - The symbol is defined inside a certain class and we are not defining this class yet
    //  - The symbol is not defined inside a class and, currently, we are defining one or more
    if (IS_CXX_LANGUAGE
            && ((symbol.is_defined_inside_class()
                    && (state.classes_being_defined.empty()
                        || state.classes_being_defined.back() != symbol.get_class_type().get_symbol()))
                || (!symbol.is_defined_inside_class()
                    && !state.classes_being_defined.empty())))
        return;

    TL::Type symbol_type = symbol.get_type();
    TL::Scope function_scope = context.retrieve_context();


    ERROR_CONDITION(!symbol.is_function()
            && !symbol.is_dependent_friend_function(), "Invalid symbol", 0);

    if (!symbol.is_friend())
    {
        if (symbol.is_member())
        {
            TL::Symbol class_symbol = symbol.get_class_type().get_symbol();
            do_define_symbol(class_symbol,
                    &CxxBase::declare_symbol_always,
                    &CxxBase::define_symbol_always);
        }
        else
        {
            if (symbol_type.is_template_specialized_type())
            {
                TL::Type template_type = symbol_type.get_related_template_type();
                TL::Type primary_type = template_type.get_primary_template();
                TL::Symbol primary_symbol = primary_type.get_symbol();

                do_declare_symbol(primary_symbol,
                        &CxxBase::declare_symbol_always,
                        &CxxBase::define_symbol_always);
            }
        }
    }

    state.current_symbol = symbol;

    int num_parameters = symbol.get_related_symbols().size();
    TL::ObjectList<std::string> parameter_names(num_parameters);
    TL::ObjectList<std::string> parameter_attributes(num_parameters);
    fill_parameter_names_and_parameter_attributes(symbol, parameter_names, parameter_attributes);

    std::string decl_spec_seq;

    if (symbol.is_friend()
            // The function is friend of a class
            && symbol.is_defined_inside_class()
            // The friend function is defined within this class
            && !state.classes_being_defined.empty()
            && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())
            // The friend function is defined in the current being defined class
    {
        decl_spec_seq += "friend ";
    }

    if (symbol.is_static()
            && (!symbol.is_member()
                || (!state.classes_being_defined.empty()
                    && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())))
    {
        decl_spec_seq += "static ";
    }

    if (symbol.is_extern() && symbol.get_value().is_null())
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

    if (symbol.is_explicit_constructor()
            && symbol.is_defined_inside_class())
    {
        decl_spec_seq += "explicit ";
    }

    std::string gcc_attributes;
    if (symbol.has_gcc_attributes())
    {
        gcc_attributes = gcc_attributes_to_str(symbol) + " ";
    }

    std::string gcc_extension;
    if (symbol.has_gcc_extension())
    {
        gcc_extension = "__extension__ ";
    }

    std::string asm_specification = gcc_asm_specifier_to_str(symbol);

    std::string declarator_name;

    if (!symbol.is_defined_inside_class())
    {
        declarator_name = symbol.get_class_qualification(function_scope, /* without_template */ true);
    }
    else
    {
        declarator_name = unmangle_symbol_name(symbol);
    }

    TL::Type real_type = symbol_type;

    if (symbol.is_conversion_function()
            || symbol.is_destructor())
    {
        // FIXME - Use TL::Type to build this type
        real_type = ::get_new_function_type(NULL, NULL, 0);
        if (symbol.is_conversion_function())
        {
            if (symbol.get_type().is_const())
            {
                real_type = real_type.get_const_type();
            }
        }
    }

    std::string declarator;
    declarator = this->get_declaration_with_parameters(real_type, function_scope, declarator_name, parameter_names, parameter_attributes);

    std::string exception_spec = exception_specifier_to_str(symbol);

    move_to_namespace_of_symbol(symbol);

    TL::TemplateParameters template_parameters = function_scope.get_template_parameters();
    if (symbol.is_defined_inside_class())
    {
        TL::Symbol class_sym = symbol.get_class_type().get_symbol();
        codegen_template_headers_bounded(
                template_parameters,
                class_sym.get_scope().get_template_parameters(),
                /*show default variables*/ false);
    }
    else
    {
        codegen_template_headers_all_levels(
                template_parameters,
                /*show default values*/ false);
    }

    bool requires_extern_linkage = (!symbol.is_member()
            && symbol.has_nondefault_linkage());

    if (requires_extern_linkage)
    {
        file << "extern " + symbol.get_linkage() + "\n";
        indent();
        file << "{\n";

        inc_indent();
    }

    if (!symbol.is_member()
            && asm_specification != "")
    {
        // gcc does not like asm specifications appear in the
        // function-definition so emit a declaration before the definition
        indent();
        file << gcc_extension << decl_spec_seq << gcc_attributes << declarator << exception_spec << asm_specification << ";\n";
    }

    indent();
    file << gcc_extension << decl_spec_seq << gcc_attributes << declarator << exception_spec << "\n";

    set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);

    if (!initializers.is_null())
    {
        push_scope(function_scope);
        inc_indent();

        indent();
        file << ": ";

        walk_list(initializers.as<Nodecl::List>(), ", ");

        dec_indent();

        file << "\n";
        pop_scope();
    }

    this->walk(context);

    if (requires_extern_linkage)
    {
        dec_indent();
        indent();
        file << "}\n";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::FunctionCode& node)
{
    if (_prune_saved_variables)
    {
        PruneVLAVisitor prune_vla;
        prune_vla.walk(node);
    }

    //Only independent code
    Nodecl::Context context = node.get_statements().as<Nodecl::Context>();
    Nodecl::List statement_seq = context.get_in_context().as<Nodecl::List>();
    Nodecl::NodeclBase initializers = node.get_initializers();

    if (statement_seq.size() != 1)
    {
        internal_error("C/C++ functions only have one statement", 0);
    }

    Nodecl::NodeclBase statement = statement_seq[0];

    TL::Symbol symbol = node.get_symbol();

    // We don't define twice a symbol
    if (get_codegen_status(symbol) == CODEGEN_STATUS_DEFINED)
        return;

    // Two return cases for C++:
    //  - The symbol is defined inside a certain class and we are not defining this class yet
    //  - The symbol is not defined inside a class and, currently, we are defining one or more
    if (IS_CXX_LANGUAGE
            && ((symbol.is_defined_inside_class()
                    && (state.classes_being_defined.empty()
                        || state.classes_being_defined.back() != symbol.get_class_type().get_symbol()))
                || (!symbol.is_defined_inside_class()
                    && !state.classes_being_defined.empty())))
        return;

    TL::Type symbol_type = symbol.get_type();

    C_LANGUAGE()
    {
        walk_type_for_symbols(
                symbol_type,
                &CxxBase::declare_symbol_if_nonlocal_nonprototype,
                &CxxBase::define_symbol_if_nonlocal_nonprototype,
                &CxxBase::define_nonlocal_nonprototype_entities_in_trees);
    }

    TL::Scope symbol_scope = symbol.get_scope();

    ERROR_CONDITION(!symbol.is_function()
            && !symbol.is_dependent_friend_function(), "Invalid symbol", 0);

    bool is_template_specialized = symbol_type.is_template_specialized_type();

    if(!symbol.is_friend())
    {
        if (symbol.is_member())
        {
            TL::Symbol class_symbol = symbol.get_class_type().get_symbol();
            do_define_symbol(class_symbol,
                    &CxxBase::declare_symbol_always,
                    &CxxBase::define_symbol_always);
        }
        else
        {
            if (is_template_specialized)
            {
                TL::Type template_type = symbol_type.get_related_template_type();
                TL::Type primary_type = template_type.get_primary_template();
                TL::Symbol primary_symbol = primary_type.get_symbol();
                do_declare_symbol(primary_symbol,
                        &CxxBase::declare_symbol_always,
                        &CxxBase::define_symbol_always);
            }
        }
    }

    state.current_symbol = symbol;


    // At this point, we mark the function as defined. It must be done here to
    // avoid the useless declaration of the function being defined and other
    // related problems.
    set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);

    C_LANGUAGE()
    {
        bool has_ellipsis = false;
        TL::ObjectList<TL::Type> parameter_list = symbol_type.parameters(has_ellipsis);

        for (TL::ObjectList<TL::Type>::iterator it = parameter_list.begin();
                it != parameter_list.end();
                it++)
        {
            walk_type_for_symbols(*it,
                    &CxxBase::declare_symbol_always,
                    &CxxBase::define_symbol_always,
                    &CxxBase::define_nonlocal_nonprototype_entities_in_trees);
        }

        define_nonlocal_nonprototype_entities_in_trees(statement);
    }

    int num_parameters = symbol.get_related_symbols().size();
    TL::ObjectList<std::string> parameter_names(num_parameters);
    TL::ObjectList<std::string> parameter_attributes(num_parameters);
    fill_parameter_names_and_parameter_attributes(symbol, parameter_names, parameter_attributes);

    std::string decl_spec_seq;

    if (symbol.is_friend()
            // The function is friend of a class
            && symbol.is_defined_inside_class()
            // The friend function is defined within this class
            && !state.classes_being_defined.empty()
            && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())
            // The friend function is defined in the current being defined class
    {
        decl_spec_seq += "friend ";
    }

    if (symbol.is_static()
            && (!symbol.is_member()
                || (!state.classes_being_defined.empty()
                    && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())))
    {
        decl_spec_seq += "static ";
    }

    if (symbol.is_extern() && symbol.get_value().is_null())
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

    if (symbol.is_explicit_constructor()
            && symbol.is_defined_inside_class())
    {
        decl_spec_seq += "explicit ";
    }

    std::string gcc_attributes;
    if (symbol.has_gcc_attributes())
    {
        gcc_attributes = gcc_attributes_to_str(symbol) + " ";
    }

    std::string gcc_extension;
    if (symbol.has_gcc_extension())
    {
        gcc_extension = "__extension__ ";
    }

    std::string asm_specification = gcc_asm_specifier_to_str(symbol);

    std::string declarator_name;
    if (IS_C_LANGUAGE)
    {
        declarator_name = unmangle_symbol_name(symbol);
    }
    else
    {
        if (!symbol.is_defined_inside_class())
        {
            declarator_name = symbol.get_class_qualification(symbol_scope, /* without_template */ true);
        }
        else
        {
            declarator_name = unmangle_symbol_name(symbol);
        }
    }

    if (is_template_specialized
            && !symbol.is_conversion_function())
    {
        declarator_name += template_arguments_to_str(symbol);
    }

    TL::Type real_type = symbol_type;

    if (symbol.is_conversion_function()
            || symbol.is_destructor())
    {
        // FIXME - Use TL::Type to build this type
        real_type = ::get_new_function_type(NULL, NULL, 0);

        if (symbol.is_conversion_function())
        {
            if (symbol.get_type().is_const())
            {
                real_type = real_type.get_const_type();
            }
        }
    }

    std::string declarator = this->get_declaration_with_parameters(
            real_type, symbol_scope, declarator_name, parameter_names, parameter_attributes);

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
    if (IS_CXX_LANGUAGE
            || cuda_emit_always_extern_linkage())
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
        file << gcc_extension << decl_spec_seq << gcc_attributes << declarator << exception_spec << asm_specification << ";\n";
    }

    indent();
    file << gcc_extension << decl_spec_seq << gcc_attributes << declarator << exception_spec << "\n";


    if (!initializers.is_null())
    {
        push_scope(symbol_scope);
        inc_indent();

        indent();
        file << ": ";

        walk_list(initializers.as<Nodecl::List>(), ", ");

        dec_indent();

        file << "\n";
        pop_scope();
    }

    this->walk(context);

    if (IS_CXX_LANGUAGE
            || cuda_emit_always_extern_linkage())
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

void CxxBase::emit_integer_constant(const_value_t* cval, TL::Type t)
{
    unsigned long long int v = const_value_cast_to_8(cval);

    // Since we may be representing data with too many bits, let's discard
    // uppermost bits for unsigned values
    if (!const_value_is_signed(cval))
    {
        int bits = 8 * t.get_size();
        unsigned long long int mask = 0;
        if (bits < 64)
            mask = ((~0ULL) << bits);
        v &= ~mask;
    }

    if (t.is_signed_int())
    {
        file << *(signed long long*)&v;
    }
    else if (t.is_unsigned_int())
    {
        file << (unsigned long long)v << "U";
    }
    else if (t.is_signed_long_int())
    {
        file << *(signed long long*)&v << "L";
    }
    else if (t.is_unsigned_long_int())
    {
        file << (unsigned long long)v << "LU";
    }
    else if (t.is_signed_long_long_int())
    {
        file << *(signed long long*)&v << "LL";
    }
    else if (t.is_unsigned_long_long_int())
    {
        file << (unsigned long long)v << "LLU";
    }
    else
    {
        // Remaining integers like 'short'
        if (const_value_is_signed(cval))
        {
            file << *(signed long long*)&v;
        }
        else
        {
            file << (unsigned long long)v;
        }
    }
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
        emit_integer_constant(value, t);
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

    Nodecl::List init_list = init.as<Nodecl::List>();
    if (!init_list.empty())
    {
        Nodecl::List::iterator it = init_list.begin();
        if(!it->is<Nodecl::ObjectInit>())
        {
            walk(init);
        }
        else
        {
            TL::ObjectList<TL::Symbol> object_init_symbols;
            for(; it != init_list.end(); ++it)
            {
                ERROR_CONDITION(!it->is<Nodecl::ObjectInit>(),
                        "unexpected node '%s'", ast_print_node_type(it->get_kind()));

                object_init_symbols.append(it->as<Nodecl::ObjectInit>().get_symbol());
            }
            define_or_declare_variables(object_init_symbols, /* is definition */ true);
        }
    }

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

    if (entry.is_class())
    {
        // Use the qualified name, do not rely on class-scope unqualified lookup
        file << entry.get_qualified_name() << "(";
    }
    else
    {
        // Otherwise the name must not be qualified
        file << entry.get_name() << "(";
    }

    TL::Type type = entry.get_type();
    if (entry.is_class())
    {
        type = entry.get_user_defined_type();
    }

    if (nodecl_calls_to_constructor(init_expr, type))
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
    Nodecl::NodeclBase initializer = node.get_init();
    ERROR_CONDITION(initializer.is_null(), "New lacks initializer", 0);

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

    Nodecl::NodeclBase type = node.get_init_real_type();
    TL::Type init_real_type = type.get_type();

    file << "(" << this->get_declaration(init_real_type, this->get_current_scope(),  "") << ")";

    // new[] cannot have an initializer, so just print the init_real_type
    if (init_real_type.is_array())
        return;

    if (initializer.is<Nodecl::CxxEqualInitializer>()
            || initializer.is<Nodecl::CxxBracedInitializer>()
            || initializer.is<Nodecl::CxxParenthesizedInitializer>())
    {
        // Dependent cases are always printed verbatim
        walk(initializer);
    }
    else if (IS_CXX03_LANGUAGE
            && init_real_type.is_aggregate()
            && initializer.is<Nodecl::StructuredValue>())
    {
        // int a[] = { 1, 2, 3 };
        // struct foo { int x; int y; } a = {1, 2};
        //
        // Note that C++11 allows struct foo { int x; int y; } a{1,2};
        file << " = ";

        bool old = state.inside_structured_value;
        state.inside_structured_value = true;
        walk(initializer);
        state.inside_structured_value = old;
    }
    else if (init_real_type.is_array()
            && !initializer.is<Nodecl::StructuredValue>())
    {
        // Only for char and wchar_t
        // const char c[] = "1234";
        file << " = ";
        walk(initializer);
    }
    else if (init_real_type.is_array()
            && initializer.is<Nodecl::StructuredValue>())
    {
        // char c = { 'a' };
        // int x = { 1 };
        file << " = ";
        bool old = state.inside_structured_value;
        state.inside_structured_value = true;
        walk(initializer);
        state.inside_structured_value = old;
    }
    else if (state.in_condition)
    {
        // This is something like if (bool foo = expression)
        file << " = ";
        walk(initializer);
    }
    else
    {
        file << "(";
        // A a; we cannot emmit it as A a(); since this would declare a function () returning A
        if (nodecl_calls_to_constructor(initializer, init_real_type))
        {
            Nodecl::List constructor_args = initializer.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>();

            // Here we add extra parentheses lest the direct-initialization looked like
            // as a function declarator (faced with this ambiguity, C++ chooses the latter!)
            //
            // A x( (A()) ); cannot become A x( A() ); because it would declare 'x' as a
            // "function (pointer to function() returning A) returning A"
            // [extra blanks added for clarity in the example above]
            walk_list(constructor_args, ", ", /* parenthesize_elements */ true);
        }
        else
        {
            walk(initializer);
        }
        file << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepNew& node)
{
    Nodecl::NodeclBase initializer = node.get_init();
    ERROR_CONDITION(initializer.is_null(), "Dependent new lacks initializer", 0);

    if (node.get_text() == "global")
        file << "::";

    file << "new ";

    Nodecl::NodeclBase placement = node.get_placement();
    if (!placement.is_null())
    {
        file << "(";
        walk_expression_list(placement.as<Nodecl::List>());
        file << ")";
    }

    Nodecl::NodeclBase type = node.get_init_real_type();
    TL::Type init_real_type = type.get_type();

   file << this->get_declaration(init_real_type, this->get_current_scope(),  "");

    // new[] cannot have an initializer, so just print the type
    if (init_real_type.is_array())
        return;

    walk(initializer);
}

CxxBase::Ret CxxBase::visit(const Nodecl::ObjectInit& node)
{
    TL::Symbol sym = node.get_symbol();

    C_LANGUAGE()
    {
        walk_type_for_symbols(sym.get_type(),
                &CxxBase::declare_symbol_always,
                &CxxBase::define_symbol_always,
                &CxxBase::define_all_entities_in_trees);
    }

    state.must_be_object_init.erase(sym);
    do_define_symbol(sym,
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always);
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

    file << "&" << this->get_qualified_name(symbol);
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
    Nodecl::NodeclBase nested_pragma = node.get_nested_pragma();
    TL::Symbol symbol = node.get_symbol();

    indent();

    // FIXME  parallel|for must be printed as parallel for
    file << "/* decl: #pragma " << node.get_text() << " ";
    walk(pragma_line);
    file << "' " << this->get_qualified_name(symbol) << "' */\n";
    walk(nested_pragma);
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

    // Do not emit stride 1 because it looks weird in C
    if (!step_expr.is_constant()
            || (const_value_cast_to_signed_int(step_expr.get_constant()) != 1))
    {
        file << ":";
        walk(step_expr);
    }
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
    const_value_string_unpack_to_int(v, &bytes, &length);

    type_t* element_type = array_type_get_element_type(no_ref(nodecl_get_type(node.get_internal_nodecl())));
    char is_wchar = !is_unsigned_char_type(element_type)
        && !is_signed_char_type(element_type);

    file << quote_c_string(bytes, length, is_wchar);

    ::xfree(bytes);
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
                    || ((items.size() == 1)
                && (type.is_named()
                    || type.no_ref().is_builtin())))
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
                // Special cases that are not valid here
                //   short int
                //   long int
                //   unsigned int
                //   signed int

                if (type.is_signed_short_int())
                {
                    file << "short";
                }
                else if (type.is_signed_long_int())
                {
                    file << "long";
                }
                else if (type.is_unsigned_int())
                {
                    file << "unsigned";
                }
                else
                {
                    // No effort done for invalid cases that the syntax does not allow
                    file << this->get_declaration(type, this->get_current_scope(),  "");
                }

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
                if (type.is_signed_short_int())
                {
                    file << "short";
                }
                else if (type.is_signed_long_int())
                {
                    file << "long";
                }
                else if (type.is_unsigned_int())
                {
                    file << "unsigned";
                }
                else
                {
                    // No effort done for invalid cases that the syntax does not allow
                    file << this->get_declaration(type, this->get_current_scope(),  "");
                }

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

    C_LANGUAGE()
    {
        if (entry.is_member())
        {
            do_define_symbol(entry.get_class_type().get_symbol(),
                    &CxxBase::declare_symbol_always,
                    &CxxBase::define_symbol_always);
        }
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
        if (entry.is_builtin()
                // Builtins cannot be qualified
                || (entry.is_function() && entry.is_friend_declared()))
        {
            file << entry.get_name();
        }
        else if (entry.is_function())
        {
            if (node.get_type().is_valid()
                    && node.get_type().is_unresolved_overload())
            {
                file << this->get_qualified_name(entry,
                        this->get_current_scope().get_decl_context(),
                        /* without_template_id */ true);

                TL::TemplateParameters template_arguments =
                    node.get_type().unresolved_overloaded_type_get_explicit_template_arguments();

                if (template_arguments.is_valid())
                {
                    file << ::template_arguments_to_str(
                            template_arguments.get_internal_template_parameter_list(),
                            /* first_template_argument_to_be_printed */ 0,
                            /* print_first_level_bracket */ 1,
                            this->get_current_scope().get_decl_context());
                }
            }
            else
            {
                // If we are visiting the called entity of a function call, the template arguments
                // (if any) will be added by 'visit_function_call_form_template_id' function
                file << this->get_qualified_name(entry,
                        this->get_current_scope(),
                        /* without template id */ state.visiting_called_entity_of_function_call);
            }
        }
        else if (!entry.is_dependent_entity())
        {
            file << this->get_qualified_name(entry, this->get_current_scope());
        }
        else
        {
            ERROR_CONDITION(entry.get_value().is_null(), "A dependent entity must have a tree of its name!", 0);
            walk(entry.get_value());
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
        if (get_rank(expr) < get_rank_kind(NODECL_ASSIGNMENT, ""))
        {
            // A comma operator could slip in
            file << "(";
        }
        walk(expr);
        if (get_rank(expr) < get_rank_kind(NODECL_ASSIGNMENT, ""))
        {
            file << ")";
        }
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

// Bug in GCC 4.4
template CxxBase::Ret CxxBase::visit_function_call<Nodecl::VirtualFunctionCall>(const Nodecl::VirtualFunctionCall& node, bool is_virtual_call);

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

    TL::Scope* context_of_declaration = NULL;
    TL::Scope context;

    Nodecl::NodeclBase nodecl_context = node.get_context();
    if (!nodecl_context.is_null())
    {
        context = nodecl_context.as<Nodecl::Context>().retrieve_context();
        context_of_declaration = &context;
    }

    state.must_be_object_init.erase(sym);

    do_declare_symbol(sym,
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always,
            context_of_declaration);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDef& node)
{
    TL::Symbol sym = node.get_symbol();
    state.must_be_object_init.erase(sym);

    TL::Scope* context_of_declaration = NULL;
    TL::Scope context;

    Nodecl::NodeclBase nodecl_context = node.get_context();
    if (!nodecl_context.is_null())
    {
        context = nodecl_context.as<Nodecl::Context>().retrieve_context();
        context_of_declaration = &context;
    }

    do_define_symbol(sym,
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always,
            context_of_declaration);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxUsingDecl& node)
{
    TL::Scope context = node.get_context().retrieve_context();
    TL::Symbol sym = node.get_symbol();

    if (context.is_namespace_scope())
    {
        // We define de namespace if it has not been defined yet.
        // C++ only allows the definition of a namespace inside an other
        // namespace or in the global scope
        do_define_symbol(sym,
                &CxxBase::declare_symbol_always,
                &CxxBase::define_symbol_always);

        move_to_namespace(context.get_related_symbol());
    }

    indent();
    file << "using " << this->get_qualified_name(sym) << ";\n";
}
CxxBase::Ret CxxBase::visit(const Nodecl::CxxUsingNamespace & node)
{
    TL::Scope context = node.get_context().retrieve_context();
    TL::Symbol sym = node.get_symbol();

   ERROR_CONDITION(!sym.is_namespace(),
           "This symbol '%s' is not a namespace",
           sym.get_name().c_str());

   if (context.is_namespace_scope())
    {
        // We define de namespace if it has not been defined yet.
        // C++ only allows the definition of a namespace inside an other
        // namespace or in the global scope
        do_define_symbol(sym,
                &CxxBase::declare_symbol_always,
                &CxxBase::define_symbol_always);

        move_to_namespace(context.get_related_symbol());
    }

   indent();
    file << "using namespace " << this->get_qualified_name(sym) << ";\n";
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

        file << "template " << class_key << " " << this->get_qualified_name(sym, sym.get_scope()) << ";\n";

    }
    else if (sym.is_function())
    {
        decl_context_t decl_context = context.retrieve_context().get_decl_context();
        move_to_namespace(decl_context.namespace_scope->related_entry);
        if (is_extern)
            file << "extern ";
        std::string original_declarator_name = codegen(declarator_name);
        file << "template " << this->get_declaration(sym.get_type(), sym.get_scope(), original_declarator_name) << ";\n";
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
    TL::Symbol sym = node.get_symbol();
    Nodecl::NodeclBase declarator_name = node.get_declarator_name();
    Nodecl::NodeclBase context = node.get_context();
    state.must_be_object_init.erase(sym);
    codegen_explicit_instantiation(sym, declarator_name, context, /* is_extern */ true);
}

CxxBase::Ret CxxBase::visit(const Nodecl::Verbatim& node)
{
    file << node.get_text();
}

CxxBase::Ret CxxBase::visit(const Nodecl::VlaWildcard& node)
{
    file << "*";
}

CxxBase::Ret CxxBase::visit(const Nodecl::UnknownPragma& node)
{
    move_to_namespace(node.retrieve_context().get_decl_context().namespace_scope->related_entry);

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
    if (!op2.is_null())
    {
        file << " : ";
        walk_list(op2.as<Nodecl::List>(), ", ");
    }
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

bool CxxBase::symbol_or_its_bases_are_nested_in_defined_classes(TL::Symbol symbol)
{
    if (symbol_is_nested_in_defined_classes(symbol))
    {
        return true;
    }

    if (symbol.is_class())
    {
        TL::ObjectList<TL::Symbol> bases = symbol.get_type().get_bases_class_symbol_list();
        for (TL::ObjectList<TL::Symbol>::iterator it = bases.begin();
                it != bases.end();
                it++)
        {
            if (symbol_is_nested_in_defined_classes(*it))
            {
                return true;
            }
        }
    }
    return false;
}



// This function is only for C
TL::ObjectList<TL::Symbol> CxxBase::define_required_before_class(TL::Symbol symbol,
        void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
        void (CxxBase::*def_sym_fun)(TL::Symbol symbol))
{
    state.pending_nested_types_to_define.clear();

    if (state.being_checked_for_required.find(symbol) != state.being_checked_for_required.end())
        return TL::ObjectList<TL::Symbol>();

    state.being_checked_for_required.insert(symbol);

    if (symbol.is_class())
    {
        TL::ObjectList<TL::Symbol> members = symbol.get_type().get_all_members();
        for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                it != members.end();
                it++)
        {
            TL::Symbol &member(*it);

            if (member.is_class()
                    && !member.is_defined_inside_class())
                continue;

            if (member.is_enum())
            {
                TL::ObjectList<TL::Symbol> enumerators = member.get_type().enum_get_enumerators();
                for (TL::ObjectList<TL::Symbol>::iterator it2 = enumerators.begin();
                        it2 != enumerators.end();
                        it2++)
                {
                    TL::Symbol &enumerator(*it2);
                    define_nonnested_entities_in_trees(enumerator.get_value());
                }
            }
            else if(member.is_class())
            {
                walk_type_for_symbols(
                        member.get_type(),
                        &CxxBase::declare_symbol_if_nonnested,
                        &CxxBase::define_symbol_if_nonnested,
                        &CxxBase::define_nonnested_entities_in_trees);
            }
            else
            {
                if (member.is_variable()
                        && member.is_static()
                        && !member.get_value().is_null())
                {
                    define_nonnested_entities_in_trees(member.get_value());
                }

                walk_type_for_symbols(
                        member.get_type(),
                        &CxxBase::declare_symbol_if_nonnested,
                        &CxxBase::define_symbol_if_nonnested,
                        &CxxBase::define_nonnested_entities_in_trees);

                if (member.is_function())
                {
                    // Define the types used by the function parameters
                    TL::ObjectList<TL::Type> parameters = member.get_type().parameters();
                    for (TL::ObjectList<TL::Type>::iterator it2 = parameters.begin();
                            it2 != parameters.end();
                            it2++)
                    {
                        TL::Type current_parameter(*it2);
                        walk_type_for_symbols(
                                current_parameter,
                                &CxxBase::declare_symbol_if_nonnested,
                                &CxxBase::define_symbol_if_nonnested,
                                &CxxBase::define_nonnested_entities_in_trees);

                    }

                    // Define the return type
                    walk_type_for_symbols(
                            member.get_type().returns(),
                            &CxxBase::declare_symbol_if_nonnested,
                            &CxxBase::define_symbol_if_nonnested,
                            &CxxBase::define_nonnested_entities_in_trees);
                }
            }
        }
    }
    else if (symbol.is_enum()
            || symbol.is_enumerator()
            || symbol.is_variable())
    {
        walk_type_for_symbols(
                symbol.get_type(),
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

    for (it = must_be_defined_inside_class.begin();
            it != must_be_defined_inside_class.end();
            it++)
    {
        TL::Symbol& entry(*it);
        // This indirectly fills state.pending_nested_types_to_define
        TL::ObjectList<TL::Symbol> pending_symbols =
            define_required_before_class(entry, decl_sym_fun, def_sym_fun);
        result.insert(pending_symbols);
    }

    state.being_checked_for_required.erase(symbol);

    return result;
}

void CxxBase::define_class_symbol_aux(TL::Symbol symbol,
        TL::ObjectList<TL::Symbol> symbols_defined_inside_class,
        int level,
        TL::Scope* scope)
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

    std::string gcc_attributes;
    if (symbol.has_gcc_attributes())
    {
        gcc_attributes = gcc_attributes_to_str(symbol) + " ";
    }

    std::string gcc_extension;
    if (symbol.has_gcc_extension())
    {
        gcc_extension = "__extension__ ";
    }

    // 1. Declaration of the class key part
    C_LANGUAGE()
    {
        indent();

        // We generate the gcc attributes at this point (and not at the end of
        // the class definition) because the attribute "visibility" is a bit
        // special and needs to be placed here.
        file << gcc_extension << class_key << " " << gcc_attributes << ms_attributes_to_str(symbol);
        if (!symbol.is_anonymous_union())
        {
            // The symbol is called 'struct/union X' in C. We should ignore the
            // class key because it has been already printed.
            file << " " << symbol.get_name().substr(class_key.length() + 1);
        }
        file << "\n";

        indent();
        file << "{\n";
    }

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

        if (symbol_type.is_template_specialized_type())
        {
            is_template_specialized = 1;
            template_type = symbol_type.get_related_template_type();
            primary_template = template_type.get_primary_template();
            primary_symbol = primary_template.get_symbol();

            if (primary_symbol == symbol)
            {
                is_primary_template = 1;
            }
        }

        // *** From here everything required should have been declared ***

        move_to_namespace_of_symbol(symbol);

        if (!is_local_symbol(symbol))
        {
            if (is_template_specialized)
            {
                TL::TemplateParameters template_parameters =
                    (scope != NULL) ? scope->get_template_parameters() : symbol.get_scope().get_template_parameters();

                if (!(symbol_type.class_type_is_complete_independent()
                            || symbol_type.class_type_is_incomplete_independent()))
                {
                    if (defined_inside_class)
                    {
                        // We only want the template header related to the current class
                        TL::Symbol enclosing_class = symbol.get_class_type().get_symbol();
                        codegen_template_headers_bounded(template_parameters,
                                enclosing_class.get_scope().get_template_parameters(),
                                /* show_default_values */ true);
                    }
                    else
                    {
                        // We want all template headers
                        codegen_template_headers_all_levels(template_parameters, /* show_default_values */ true);
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
                            codegen_template_headers_all_levels(template_parameters, /* show_default_values */ true);
                        }
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

        // We generate the gcc attributes at this point (and not at the end of
        // the class definition) because the attribute "visibility" is a bit
        // special and needs to be placed here.
        file << gcc_extension << class_key << " " << gcc_attributes << ms_attributes_to_str(symbol);
        if (!symbol.is_anonymous_union())
        {
            file << " " << qualified_name;
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

                file << this->get_qualified_name(base, symbol.get_scope());
            }
        }

        file << "\n";
        indent();
        file << "{\n";
    }

    // 2. Now declare members:
    // 2.1 We need to forward declare all member classes (only for C++) because
    // the member list does not contain enough information to decide in what
    // order we should generate the members
    TL::ObjectList<TL::Symbol> members = symbol_type.get_all_members();
    access_specifier_t current_access_spec = default_access_spec;

    CXX_LANGUAGE()
    {
        state.in_forwarded_member_declaration = true;
        for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                it != members.end();
                it++)
        {
            TL::Symbol &member(*it);
            if (!member.is_class())
                continue;

            if (!member.is_defined_inside_class())
                continue;

            CXX_LANGUAGE()
            {
                access_specifier_t access_spec = member.get_access_specifier();
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

            do_declare_symbol(member,
                    &CxxBase::declare_symbol_always,
                    &CxxBase::define_symbol_always);

            state.in_member_declaration = old_in_member_declaration;

            dec_indent();
            dec_indent();
        }

        state.in_forwarded_member_declaration = false;
    }

    // 2.2 Declare members as usual
    for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
            it != members.end();
            it++)
    {
        TL::Symbol &member(*it);
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
                state.classes_being_defined.push_back(class_sym);
                define_class_symbol_aux(class_sym, symbols_defined_inside_class, level + 1, scope);
                state.classes_being_defined.pop_back();
            }
            else
            {
                do_define_symbol(member,
                        &CxxBase::declare_symbol_always,
                        &CxxBase::define_symbol_always);
            }
            set_codegen_status(member, CODEGEN_STATUS_DEFINED);
        }
        CXX_LANGUAGE()
        {
            if (member.is_class())
            {
                if (member.get_type().is_template_specialized_type())
                {
                    // We should declare all user template specializations
                    if (member.is_user_declared())
                    {
                        // Could this specialization be defined?
                        if (member.is_defined_inside_class() &&
                                is_complete_type(member.get_type().get_internal_type()))
                        {
                            state.classes_being_defined.push_back(member);
                            define_class_symbol_aux(member, symbols_defined_inside_class, level + 1);
                            state.classes_being_defined.pop_back();
                        }
                        else
                        {
                            do_declare_symbol(member,
                                    &CxxBase::declare_symbol_always,
                                    &CxxBase::define_symbol_always);
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
                        state.classes_being_defined.push_back(member);
                        define_class_symbol_aux(member, symbols_defined_inside_class, level + 1);
                        state.classes_being_defined.pop_back();
                    }
                    else
                    {
                        do_declare_symbol(member,
                                &CxxBase::declare_symbol_always,
                                &CxxBase::define_symbol_always);
                        set_codegen_status(member, CODEGEN_STATUS_DECLARED);
                    }
                }
            }
            else if (member.is_using_symbol()
                    || member.is_using_typename_symbol())
            {
                indent();
                ERROR_CONDITION(!member.get_type().is_unresolved_overload(), "Invalid SK_USING symbol\n", 0);

                TL::ObjectList<TL::Symbol> unresolved = member.get_type().get_unresolved_overload_set();

                TL::Symbol entry = unresolved[0];
                file << "using ";

                if (member.is_using_typename_symbol())
                    file << "typename ";

                file << this->get_qualified_name(entry, /* without_template */ 1) << ";\n";
            }
            else if (member.is_enum()
                    || member.is_typedef())
            {
                do_define_symbol(member,
                        &CxxBase::declare_symbol_always,
                        &CxxBase::define_symbol_always);
                set_codegen_status(member, CODEGEN_STATUS_DEFINED);
            }
            else if (member.is_function())
            {
                if (!member.get_function_code().is_null() &&
                        member.is_defined_inside_class())
                {
                    walk(member.get_function_code());
                }
                else
                {
                    do_declare_symbol(member,
                            &CxxBase::declare_symbol_always,
                            &CxxBase::define_symbol_always);
                }
            }
            else
            {
                bool member_declaration_does_define = true;

                // If we are declaring a static member it is not a definition
                // unless it has been declared inside the class
                if (member.is_variable()
                        && member.is_static())
                {
                    member_declaration_does_define = member.is_defined_inside_class();
                }

                if (member_declaration_does_define)
                {
                    do_define_symbol(member,
                            &CxxBase::declare_symbol_always,
                            &CxxBase::define_symbol_always);
                    set_codegen_status(member, CODEGEN_STATUS_DEFINED);
                }
                else
                {
                    do_declare_symbol(member,
                            &CxxBase::declare_symbol_always,
                            &CxxBase::define_symbol_always);
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

    // 3. Declare friends
    TL::ObjectList<TL::Symbol> friends = symbol_type.class_get_friends();
    for (TL::ObjectList<TL::Symbol>::iterator it = friends.begin();
            it != friends.end();
            it++)
    {
        TL::Symbol &_friend(*it);
        inc_indent();
        if ((_friend.is_function() || _friend.is_dependent_friend_function())
                && !_friend.get_function_code().is_null()
                && _friend.is_defined_inside_class())
        {
            walk(_friend.get_function_code());
        }
        else
        {
            declare_friend_symbol(*it, symbol);
        }
        dec_indent();
    }

    indent();
    file << "};\n";
}

void CxxBase::define_class_symbol(TL::Symbol symbol,
        void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
        void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
        TL::Scope *scope)
{
    if (symbol.is_anonymous_union() && !symbol.is_user_declared())
    {
        // This anonymous already union has been defined in a class scope
        return;
    }

    std::set<TL::Symbol> current_pending = state.pending_nested_types_to_define;

    state.classes_being_defined.push_back(symbol);

    // This indirectly fills state.pending_nested_types_to_define
    TL::ObjectList<TL::Symbol> symbols_defined_inside_class;

    C_LANGUAGE()
    {
        symbols_defined_inside_class =
            define_required_before_class(symbol, decl_sym_fun, def_sym_fun);
    }

    define_class_symbol_aux(symbol, symbols_defined_inside_class, /* level */ 0, scope);

    state.classes_being_defined.pop_back();

    state.pending_nested_types_to_define = current_pending;
}

void CxxBase::declare_friend_symbol(TL::Symbol friend_symbol, TL::Symbol class_symbol)
{
    ERROR_CONDITION(!friend_symbol.is_friend(), "This symbol must be a friend", 0);

    bool is_template_friend_declaration = false;

    if (friend_symbol.is_template())
    {
        is_template_friend_declaration = true;
        TL::Type primary_template = friend_symbol.get_type().get_primary_template();
        TL::Symbol primary_symbol = primary_template.get_symbol();
        friend_symbol = primary_symbol;
    }

    TL::Type friend_type = friend_symbol.get_type();

    // A. Generate a template header if this friend declaration has one
    if (friend_symbol.is_dependent_friend_class() && friend_symbol.has_alias_to())
    {
        // It's a special case: friend_symbol contains the right context
        // of the template friend declaration and an alias to the real friend symbol
        TL::Symbol pointed_symbol = friend_symbol.get_alias_to();
        if (pointed_symbol.is_template())
        {
            is_template_friend_declaration = true;
            TL::Type primary_template = pointed_symbol.get_type().get_primary_template();
            TL::Symbol primary_symbol = primary_template.get_symbol();
            pointed_symbol = primary_symbol;
        }

        codegen_template_headers_bounded(
                friend_symbol.get_scope().get_template_parameters(),
                class_symbol.get_scope().get_template_parameters(),
                /* show default values */ false);

        // Now, we should change the fake friend symbol by the real pointed symbol
        friend_symbol = pointed_symbol;
        friend_type = pointed_symbol.get_type();
    }
    else if (friend_type.is_template_specialized_type())
    {
        TL::Type template_type = friend_type.get_related_template_type();
        TL::Type primary_template = template_type.get_primary_template();
        TL::Symbol primary_symbol = primary_template.get_symbol();

        if (friend_type.is_dependent())
        {
            TL::TemplateParameters template_parameters =
                friend_symbol.get_scope().get_template_parameters();

            codegen_template_headers_bounded(
                    template_parameters,
                    class_symbol.get_scope().get_template_parameters(),
                    /* show default values */ false);
        }
    }

    // B. Generate the function or class declaration
    indent();
    file << "friend ";

    if (friend_symbol.is_class())
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

        if ((!friend_type.is_template_specialized_type()
                    && get_codegen_status(friend_symbol) == CODEGEN_STATUS_NONE)
                || (friend_type.is_template_specialized_type()
                    && get_codegen_status(friend_type
                        .get_related_template_type()
                        .get_primary_template()
                        .get_symbol()) == CODEGEN_STATUS_NONE))
        {
            // The class_symbol has not been declared or defined before this friend declaration
            // We cannot print its qualified
            file << friend_symbol.get_name();
        }
        else
        {
            file << this->get_qualified_name(
                    friend_symbol,
                    class_symbol.get_scope(),
                    /*without template id */ is_template_friend_declaration);
        }
    }
    else if(friend_symbol.is_dependent_friend_class())
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

        file << this->get_declaration(friend_type, friend_symbol.get_scope(), "");
    }
    else if (friend_symbol.is_function()
            || friend_symbol.is_dependent_friend_function())
    {
        std::string exception_spec = exception_specifier_to_str(friend_symbol);
        TL::Type real_type = friend_type;
        if (class_symbol.is_conversion_function())
        {
            real_type = get_new_function_type(NULL, NULL, 0);
        }

        std::string function_name;
        if (friend_type.is_template_specialized_type()
                && !friend_type.is_dependent())
        {
              function_name = this->get_qualified_name(
                      friend_symbol,
                      class_symbol.get_scope(),
                      /* without template id */ false);
        }
        else if(get_codegen_status(friend_symbol) == CODEGEN_STATUS_NONE)
        {
            function_name = friend_symbol.get_name();
        }
        else
        {
             function_name = this->get_qualified_name(
                     friend_symbol,
                     class_symbol.get_scope(),
                     /* without template id */ true);
        }

        // Dirty trick to remove the firsts two colons if the name of the function has them
        if (function_name.size() >= 2 &&
                function_name[0] == ':' &&
                function_name[1] == ':')
        {
            function_name = function_name.substr(2);
        }

        file << this->get_declaration(real_type, friend_symbol.get_scope(), function_name) << exception_spec;
    }
    else if (friend_symbol.is_dependent_friend_function())
    {
        std::string exception_spec = exception_specifier_to_str(friend_symbol);
        TL::Type real_type = friend_type;
        if (class_symbol.is_conversion_function())
        {
            real_type = get_new_function_type(NULL, NULL, 0);
        }

        std::string function_name;
        TL::ObjectList<TL::Symbol> candidates_set = friend_symbol.get_related_symbols();
        if (candidates_set.size() == 1 &&
                (get_codegen_status(candidates_set[0]) == CODEGEN_STATUS_DECLARED ||
                 get_codegen_status(candidates_set[0]) == CODEGEN_STATUS_DEFINED))
        {
            function_name = this->get_qualified_name(
                    friend_symbol,
                    candidates_set[0].get_scope(),
                    /* without template id */ (friend_type.is_template_specialized_type()));
        }
        else
        {
            function_name = friend_symbol.get_name();
        }

        file << this->get_declaration(real_type, friend_symbol.get_scope(), function_name) << exception_spec;
    }
    else
    {
        internal_error("Invalid friend class_symbol kind '%s'\n", symbol_kind_name(friend_symbol.get_internal_symbol()));
    }

    file << ";\n";
}

bool CxxBase::is_local_symbol(TL::Symbol entry)
{
    return entry.is_valid()
        && (entry.get_scope().is_block_scope()
                || entry.get_scope().is_function_scope()
                || (entry.is_member() && is_local_symbol(entry.get_class_type().get_symbol())));
}

bool CxxBase::is_local_symbol_but_local_class(TL::Symbol entry)
{
    return is_local_symbol(entry)
        // C++ local classes are not considered here
        && !(IS_CXX_LANGUAGE && entry.is_class() && entry.get_scope().is_block_scope());
}

// Note: is_nonlocal_symbol_but_local_class is NOT EQUIVALENT to !is_local_symbol_but_local_class
bool CxxBase::is_nonlocal_symbol_but_local_class(TL::Symbol entry)
{
    return !is_local_symbol(entry)
        // C++ Local classes are obiously non local
        && !(IS_CXX_LANGUAGE && entry.is_class() && entry.get_scope().is_block_scope());
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

void CxxBase::define_symbol_always(TL::Symbol symbol)
{
    do_define_symbol(symbol,
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always);
}

void CxxBase::declare_symbol_always(TL::Symbol symbol)
{
    do_declare_symbol(symbol,
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always);
}

void CxxBase::define_symbol_if_local(TL::Symbol symbol)
{
    if (is_local_symbol_but_local_class(symbol))
    {
        do_define_symbol(symbol,
                &CxxBase::declare_symbol_if_local,
                &CxxBase::define_symbol_if_local);
    }
}

void CxxBase::declare_symbol_if_local(TL::Symbol symbol)
{
    if (is_local_symbol_but_local_class(symbol))
    {
        do_declare_symbol(symbol,
                &CxxBase::declare_symbol_if_local,
                &CxxBase::define_symbol_if_local);
    }
}

void CxxBase::define_symbol_if_nonlocal(TL::Symbol symbol)
{
    if (is_nonlocal_symbol_but_local_class(symbol))
    {
        do_define_symbol(symbol,
                &CxxBase::declare_symbol_if_nonlocal,
                &CxxBase::define_symbol_if_nonlocal);
    }
}

void CxxBase::declare_symbol_if_nonlocal(TL::Symbol symbol)
{
    if (is_nonlocal_symbol_but_local_class(symbol))
    {
        do_declare_symbol(symbol,
                &CxxBase::declare_symbol_if_nonlocal,
                &CxxBase::define_symbol_if_nonlocal);
    }
}

void CxxBase::define_symbol_if_nonlocal_nonprototype(TL::Symbol symbol)
{
    if (is_nonlocal_symbol_but_local_class(symbol)
            && !is_prototype_symbol(symbol))
    {
        do_define_symbol(symbol,
                &CxxBase::declare_symbol_if_nonlocal_nonprototype,
                &CxxBase::define_symbol_if_nonlocal_nonprototype);
    }
}

void CxxBase::declare_symbol_if_nonlocal_nonprototype(TL::Symbol symbol)
{
    if (is_nonlocal_symbol_but_local_class(symbol)
            && !is_prototype_symbol(symbol))
    {
        do_declare_symbol(symbol,
                &CxxBase::declare_symbol_if_nonlocal_nonprototype,
                &CxxBase::define_symbol_if_nonlocal_nonprototype);
    }
}

void CxxBase::define_symbol_if_nonprototype(TL::Symbol symbol)
{
    if (!is_prototype_symbol(symbol))
    {
        do_define_symbol(symbol,
                &CxxBase::declare_symbol_if_nonprototype,
                &CxxBase::define_symbol_if_nonprototype);
    }
}

void CxxBase::declare_symbol_if_nonprototype(TL::Symbol symbol)
{
    if (!is_prototype_symbol(symbol))
    {
        do_define_symbol(symbol,
                &CxxBase::declare_symbol_if_nonprototype,
                &CxxBase::define_symbol_if_nonprototype);
    }
}

void CxxBase::define_symbol_if_nonnested(TL::Symbol symbol)
{
    if (!symbol_or_its_bases_are_nested_in_defined_classes(symbol))
    {
        do_define_symbol(symbol,
                &CxxBase::declare_symbol_if_nonnested,
                &CxxBase::define_symbol_if_nonnested);
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
    if (!symbol_or_its_bases_are_nested_in_defined_classes(symbol)
            || !symbol.is_member())
    {
        do_declare_symbol(symbol,
                &CxxBase::declare_symbol_if_nonnested,
                &CxxBase::define_symbol_if_nonnested);
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
                || symbol_or_its_bases_are_nested_in_defined_classes(sym)))
    {
        (this->*symbol_to_declare)(sym);
    }
    else
    {
        (this->*symbol_to_define)(sym);
    }
}

void CxxBase::define_or_declare_variable_emit_initializer(TL::Symbol& symbol, bool is_definition)
{
    // Emit the initializer for nonmembers and nonstatic members in
    // non member declarations or member declarations if they have
    // integral or enum type
    char emit_initializer = 0;
    if (!symbol.get_value().is_null()
            && (!symbol.is_member()
                || (state.in_member_declaration == symbol.is_defined_inside_class())))
    {
        emit_initializer = 1;
    }

    // Initializer
    if (emit_initializer)
    {
        push_scope(symbol.get_scope());

        // We try to always emit direct-initialization syntax
        // except when infelicities in the syntax prevent us to do that
        Nodecl::NodeclBase init = symbol.get_value();

        if (is_definition)
        {
            C_LANGUAGE()
            {
                file << " = ";

                bool old = state.inside_structured_value;
                if (init.is<Nodecl::StructuredValue>())
                {
                    state.inside_structured_value = true;
                }

                walk(init);

                state.inside_structured_value = old;
            }
            CXX_LANGUAGE()
            {
                if (init.is<Nodecl::CxxEqualInitializer>()
                        || init.is<Nodecl::CxxBracedInitializer>()
                        || init.is<Nodecl::CxxParenthesizedInitializer>())
                {
                    // Dependent cases are always printed verbatim
                    walk(init);
                }
                else if (IS_CXX03_LANGUAGE
                        && symbol.get_type().is_aggregate()
                        && init.is<Nodecl::StructuredValue>())
                {
                    // int a[] = { 1, 2, 3 };
                    // struct foo { int x; int y; } a = {1, 2};
                    //
                    // Note that C++11 allows struct foo { int x; int y; } a{1,2};
                    file << " = ";

                    bool old = state.inside_structured_value;
                    state.inside_structured_value = true;
                    walk(init);
                    state.inside_structured_value = old;
                }
                else if (symbol.get_type().is_array()
                        && !init.is<Nodecl::StructuredValue>())
                {
                    // Only for char and wchar_t
                    // const char c[] = "1234";
                    file << " = ";
                    walk(init);
                }
                else if (!symbol.get_type().is_array()
                        && init.is<Nodecl::StructuredValue>())
                {
                    // char c = { 'a' };
                    // int x = { 1 };
                    file << " = ";
                    bool old = state.inside_structured_value;
                    state.inside_structured_value = true;
                    walk(init);
                    state.inside_structured_value = old;
                }
                else if (symbol.is_member()
                        && symbol.is_static()
                        && state.in_member_declaration)
                {
                    // This is an in member declaration initialization
                    file << " = ";
                    walk(init);
                }
                else if (state.in_condition)
                {
                    // This is something like if (bool foo = expression)
                    file << " = ";
                    walk(init);
                }
                // Workaround for g++ <=4.5, >=4.7 (4.6 seems to work fine, but we'll do it anyways)
                else if (nodecl_expr_is_value_dependent(init.get_internal_nodecl())
                        && symbol.get_type().is_integral_type())
                {
                    file << " = ";
                    walk(init);
                }
                else
                {
                    file << "(";
                    // A a; we cannot emmit it as A a(); since this would declare a function () returning A
                    if (nodecl_calls_to_constructor(init, symbol.get_type()))
                    {
                        Nodecl::List constructor_args = init.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>();

                        // Here we add extra parentheses lest the direct-initialization looked like
                        // as a function declarator (faced with this ambiguity, C++ chooses the latter!)
                        //
                        // A x( (A()) ); cannot become A x( A() ); because it would declare 'x' as a
                        // "function (pointer to function() returning A) returning A"
                        // [extra blanks added for clarity in the example above]
                        walk_list(constructor_args, ", ", /* parenthesize_elements */ true);
                    }
                    else
                    {
                        walk(init);
                    }
                    file << ")";
                }
            }
        }
        pop_scope();
    }
}


std::string CxxBase::define_or_declare_variable_get_name_variable(TL::Symbol& symbol)
{
    bool has_been_declared = (get_codegen_status(symbol) == CODEGEN_STATUS_DECLARED
            || get_codegen_status(symbol) == CODEGEN_STATUS_DEFINED);

    std::string variable_name;
    if (!has_been_declared)
    {
        variable_name = symbol.get_name();
    }
    else
    {
        variable_name = symbol.get_class_qualification(symbol.get_scope(),
                /* without_template */ false);
    }

    return variable_name;
}

void CxxBase::emit_declarations_of_initializer(TL::Symbol symbol)
{
    if (!symbol.get_value().is_null()
            && (!symbol.is_member()
                || ((symbol.is_static()
                        && (!state.in_member_declaration
                            || ((symbol.get_type().is_integral_type()
                                    || symbol.get_type().is_enum())
                                && symbol.get_type().is_const())))
                    || symbol.is_defined_inside_class())))
    {
        if (symbol.is_member()
                // FIXME -> || !is_local_symbol_but_local_class(symbol))
            || (!symbol.get_scope().is_block_scope()
                    && !symbol.get_scope().is_function_scope()))
                    {
                        // This is a member or nonlocal symbol
                        define_nonnested_entities_in_trees(symbol.get_value());
                    }
        else
        {
            // This is a local symbol
            define_local_entities_in_trees(symbol.get_value());
        }
    }
}

void CxxBase::define_or_declare_variables(TL::ObjectList<TL::Symbol>& symbols, bool is_definition)
{
    codegen_status_t codegen_status =
        (is_definition) ? CODEGEN_STATUS_DEFINED : CODEGEN_STATUS_DECLARED;

    for (TL::ObjectList<TL::Symbol>::iterator it = symbols.begin();
            it != symbols.end();
            it++)
    {
        TL::Symbol &symbol (*it);
        emit_declarations_of_initializer(symbol);
    }

    define_or_declare_variable(symbols[0], is_definition);
    // We ignore the first symbol as it has been already declared
    for (TL::ObjectList<TL::Symbol>::iterator it = (symbols.begin() + 1);
            it != symbols.end();
            it++)
    {
        TL::Symbol &symbol (*it);
        std::string variable_name =
            define_or_declare_variable_get_name_variable(symbol);

        std::string declarator = this->get_declaration_only_declarator(symbol.get_type(),
            symbol.get_scope(),
            variable_name);

        set_codegen_status(symbol, codegen_status);

        file <<  ", " << declarator;

        define_or_declare_variable_emit_initializer(symbol, is_definition);
    }
}

void CxxBase::define_or_declare_variable(TL::Symbol symbol, bool is_definition)
{
    ERROR_CONDITION(!symbol.is_variable(), "must be a variable", 0);

    // Builtins, anonymous unions and non-user declared varibles are not printed
    if ((symbol.is_builtin()
                || (symbol.get_type().is_named_class()
                    && symbol.get_type().get_symbol().is_anonymous_union())))
    {
        set_codegen_status(symbol, CODEGEN_STATUS_DECLARED);
        return;
    }

    std::string decl_specifiers;
    std::string gcc_attributes;
    std::string declarator;
    std::string bit_field;

    bool requires_extern_linkage = false;
    if (IS_CXX_LANGUAGE
            || cuda_emit_always_extern_linkage())
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

    if (symbol.is_static()
            && (!symbol.is_member()
                || (!state.classes_being_defined.empty()
                    && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())))
    {
        decl_specifiers += "static ";
    }

    else if (symbol.is_extern() || !is_definition)
    {
        decl_specifiers += "extern ";
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

    std::string variable_name = define_or_declare_variable_get_name_variable(symbol);

    if (is_definition)
    {
        set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);
    }
    else
    {
        set_codegen_status(symbol, CODEGEN_STATUS_DECLARED);
    }
    emit_declarations_of_initializer(symbol);

    move_to_namespace_of_symbol(symbol);

    // Generate the template headers if needed
    CXX_LANGUAGE()
    {
        if (symbol.is_member()
                && !symbol.is_defined_inside_class()
                && state.classes_being_defined.empty())
        {
            TL::TemplateParameters template_parameters = symbol.get_scope().get_template_parameters();
            codegen_template_headers_all_levels(template_parameters, false);
        }
    }

    declarator = this->get_declaration(symbol.get_type(),
            symbol.get_scope(),
            variable_name);

    if (symbol.has_gcc_attributes())
    {
        gcc_attributes = gcc_attributes_to_str(symbol) + " ";
    }

    std::string gcc_extension;
    if (symbol.has_gcc_extension())
    {
        gcc_extension = "__extension__ ";
    }

    if (!state.in_condition)
        indent();

    if (_emit_saved_variables_as_unused
            && symbol.is_saved_expression())
    {
        gcc_attributes += "__attribute__((unused)) ";
    }

    file << gcc_extension << decl_specifiers << gcc_attributes << declarator << bit_field;

    define_or_declare_variable_emit_initializer(symbol, is_definition);

    if (!state.in_condition)
    {
        file << ";\n";
    }

    if (IS_CXX_LANGUAGE
            || cuda_emit_always_extern_linkage())
    {
        if (requires_extern_linkage)
        {
            dec_indent();
            indent();
            file << "}\n";
        }
    }

}

void CxxBase::do_define_symbol(TL::Symbol symbol,
        void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
        void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
        TL::Scope* scope)
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
            && !symbol.is_user_declared())
        return;

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
        if (!symbol_or_its_bases_are_nested_in_defined_classes(class_entry))
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
        define_or_declare_variable(symbol, /* is definition */ true);
    }
    else if (symbol.is_typedef())
    {
        // Template parameters are not to be defined, ever
        if (!symbol.is_template_parameter())
        {
            std::string gcc_attributes;
            if (symbol.has_gcc_attributes())
            {
                gcc_attributes = gcc_attributes_to_str(symbol) + " ";
            }
            std::string gcc_extension;
            if (symbol.has_gcc_extension())
            {
                gcc_extension = "__extension__ ";
            }

            move_to_namespace_of_symbol(symbol);
            indent();
            file << gcc_extension
                << "typedef "
                << gcc_attributes
                << this->get_declaration(symbol.get_type(),
                        symbol.get_scope(),
                        symbol.get_name())
                << ";\n";
        }
    }
    else if (symbol.is_enumerator())
    {
        (this->*def_sym_fun)(symbol.get_type().get_symbol());
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

            if (!enumerator.get_value().is_null())
            {
                file << " = ";
                walk(enumerator.get_value());
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
        define_class_symbol(symbol, decl_sym_fun, def_sym_fun, scope);
    }
    else if (symbol.is_function())
    {
        (this->*decl_sym_fun)(symbol);
        // Functions are not defined but only declared
        // We early return here otherwise this function would be marked as defined
        return;
    }
    else if (symbol.is_namespace())
    {
            move_to_namespace_of_symbol(symbol);
            indent();
            file << "namespace " << symbol.get_name() << " { }\n";
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

void CxxBase::do_declare_symbol(TL::Symbol symbol,
        void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
        void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
        TL::Scope* scope)
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

    if (symbol.is_member())
    {
        TL::Symbol class_entry = symbol.get_class_type().get_symbol();
        if (!symbol_or_its_bases_are_nested_in_defined_classes(class_entry))
        {
            define_symbol_if_nonnested(class_entry);
        }

        // If the member symbol has been defined inside a class and
        // and this class is not currently being defined we do nothing
        if (symbol.is_defined_inside_class() &&
                (state.classes_being_defined.empty()
                        || (state.classes_being_defined.back() != class_entry)))
            return;
    }

    // We only generate user declared code
    if (!symbol.is_user_declared())
        return;

    // Do nothing if:
    //  - The symbol has been declared or defined and
    //  - It is not a template specialized class
    if ((get_codegen_status(symbol) == CODEGEN_STATUS_DEFINED
                || get_codegen_status(symbol) == CODEGEN_STATUS_DECLARED)
                && !(symbol.is_class()
                    && symbol.get_type().is_template_specialized_type()))
        return;

    // It is a symbol that will be object-inited
    if (state.must_be_object_init.find(symbol) != state.must_be_object_init.end())
        return;

    if (symbol.is_variable())
    {
        define_or_declare_variable(symbol, /* is definition */ false);
        return;
    }

    // If the symbol is already defined we should not change its codegen status
    if (get_codegen_status(symbol) == CODEGEN_STATUS_NONE)
        set_codegen_status(symbol, CODEGEN_STATUS_DECLARED);

    if (symbol.is_class())
    {
        C_LANGUAGE()
        {
            // the symbol will be already called 'struct/union X' in C
            indent();
            file << symbol.get_name() << ";\n";
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
                    (this->*def_sym_fun)(symbol.get_class_type().get_symbol());
                    return;
                }
            }

            char is_template_specialized = 0;
            char is_primary_template = 0;

            TL::Type template_type(NULL);
            TL::Type primary_template(NULL);
            TL::Symbol primary_symbol(NULL);

            if (symbol.get_type().is_template_specialized_type())
            {
                is_template_specialized = 1;
                template_type = symbol.get_type().get_related_template_type();
                primary_template = template_type.get_primary_template();
                primary_symbol = primary_template.get_symbol();

                if (primary_symbol != symbol)
                {
                    // Before the declaration of this specialization we should ensure
                    // that the primary specialization has been defined
                    (this->*decl_sym_fun)(primary_symbol);
                }
                else
                {
                    is_primary_template = 1;
                }
            }

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
                    ERROR_CONDITION(!symbol.is_user_declared(),
                            "Only user declared template specializations are allowed "
                            "as a dependent template specialized type!\n", 0);

                    TL::TemplateParameters template_parameters(NULL);
                    if (scope != NULL)
                    {
                        template_parameters = scope->get_template_parameters();
                    }
                    else
                    {
                     template_parameters = is_primary_template ?
                         template_type.get_related_template_symbol().get_type().template_type_get_template_parameters()
                         : symbol.get_type().template_specialized_type_get_template_parameters();
                    }

                    codegen_template_header(template_parameters,
                            /* show_default_values */ !state.in_forwarded_member_declaration);
                }
            }

            // A union inside a class must be defined if its not a forward
            // member declaration
            if (class_key == "union"
                    && symbol.get_scope().is_class_scope()
                    && !state.in_forwarded_member_declaration)
            {
                (this->*def_sym_fun)(symbol);
                return;
            }

            if (!symbol.is_anonymous_union())
            {
                indent();
                file << class_key << " " << symbol.get_name();

                if (is_template_specialized
                        && !is_primary_template)
                {
                    file << get_template_arguments_str(symbol.get_internal_symbol(), symbol.get_scope().get_decl_context());
                }
                file << ";\n";
            }
        }
    }
    else if (symbol.is_enumerator())
    {
        (this->*decl_sym_fun)(symbol.get_type().get_symbol());
    }
    else if (symbol.is_enum())
    {
        // Enums cannot be only declared but defined
        (this->*def_sym_fun)(symbol);
    }
    else if (symbol.is_typedef())
    {
        // Typedefs can't be simply declared
        (this->*def_sym_fun)(symbol);
    }
    else if (symbol.is_function())
    {
        // The user did not declare it, ignore it
        if (symbol.is_friend_declared())
            return;

        C_LANGUAGE()
        {
            walk_type_for_symbols(
                    symbol.get_type(),
                    &CxxBase::declare_symbol_if_nonlocal_nonprototype,
                    &CxxBase::define_symbol_if_nonlocal_nonprototype,
                    &CxxBase::define_nonlocal_nonprototype_entities_in_trees);
        }

        char is_primary_template = 0;
        bool requires_extern_linkage = false;
        if (IS_CXX_LANGUAGE
                || cuda_emit_always_extern_linkage())
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
        }

        CXX_LANGUAGE()
        {
            if (symbol.get_type().is_template_specialized_type())
            {
                TL::Type template_type = symbol.get_type().get_related_template_type();
                TL::Type primary_template = template_type.get_primary_template();
                TL::Symbol primary_symbol = primary_template.get_symbol();
                (this->*decl_sym_fun)(primary_symbol);

                if (primary_symbol != symbol)
                {
                    indent();
                    file << "template <>\n";
                }
                else
                {
                    TL::TemplateParameters template_parameters = template_type.template_type_get_template_parameters();
                    codegen_template_header(template_parameters,
                            /*show default values*/ false);
                    is_primary_template = 1;
                }
            }
        }

        int num_parameters = symbol.get_related_symbols().size();
        TL::ObjectList<std::string> parameter_names(num_parameters);
        TL::ObjectList<std::string> parameter_attributes(num_parameters);
        fill_parameter_names_and_parameter_attributes(symbol, parameter_names, parameter_attributes);

        std::string decl_spec_seq;

        if (symbol.is_static()
                && (!symbol.is_member()
                    || (!state.classes_being_defined.empty()
                        && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())))
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

        if (symbol.is_explicit_constructor()
                && symbol.is_defined_inside_class())
        {
            decl_spec_seq += "explicit ";
        }

        if (symbol.is_nested_function())
        {
            decl_spec_seq += "auto ";
        }

        std::string gcc_attributes = gcc_attributes_to_str(symbol);
        std::string asm_specification = gcc_asm_specifier_to_str(symbol);


        TL::Type real_type = symbol.get_type();
        if (symbol.is_conversion_function()
                || symbol.is_destructor())
        {
            // FIXME
            real_type = get_new_function_type(NULL, NULL, 0);

            if (symbol.is_conversion_function())
            {
                if (symbol.get_type().is_const())
                {
                    real_type = real_type.get_const_type();
                }
            }
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
        std::string pure_spec = "";
        if (!real_type.lacks_prototype())
        {
            declarator = this->get_declaration_with_parameters(real_type, symbol.get_scope(),
                    function_name,
                    parameter_names,
                    parameter_attributes);


            if (symbol.is_virtual()
                    && symbol.is_pure())
            {
                pure_spec += " = 0 ";
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
        file << decl_spec_seq << declarator << exception_spec << pure_spec << asm_specification << gcc_attributes << ";\n";

        if (IS_CXX_LANGUAGE
                || cuda_emit_always_extern_linkage())
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

        (this->*decl_sym_fun)(entry);
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

bool CxxBase::is_pointer_arithmetic_add_helper(TL::Type op1, TL::Type op2)
{
    if (op1.is_lvalue_reference())
        op1 = op1.references_to();

    if (op2.is_lvalue_reference())
        op2 = op2.references_to();

    return (op1.is_pointer() && op2.is_integral_type());
}

bool CxxBase::is_pointer_arithmetic_add(const Nodecl::Add &node, TL::Type &pointer_type)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    if ( is_pointer_arithmetic_add_helper(lhs.get_type(), rhs.get_type()))
    {
        pointer_type = lhs.get_type();
    }
    else if (is_pointer_arithmetic_add_helper(rhs.get_type(), lhs.get_type()))
    {
        pointer_type = rhs.get_type();
    }
    else
    {
        return false;
    }

    if (pointer_type.is_lvalue_reference())
        pointer_type = pointer_type.references_to();

    return true;
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

    if (node.is<Nodecl::List>())
    {
        Nodecl::List l =  node.as<Nodecl::List>();
        for (Nodecl::List::iterator it = l.begin();
                it != l.end();
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
    }
    else
    {
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
                && entry.get_type().is_valid()
                && state.walked_symbols.find(entry) == state.walked_symbols.end())
        {
            state.walked_symbols.insert(entry);

            C_LANGUAGE()
            {
                walk_type_for_symbols(entry.get_type(),
                        decl_sym_fun,
                        def_sym_fun,
                        define_entities_fun
                        );

                (this->*define_entry_fun)(node, entry, def_sym_fun);
            }


            define_generic_entities(entry.get_value(),
                    decl_sym_fun,
                    def_sym_fun,
                    define_entities_fun,
                    define_entry_fun
                    );

            state.walked_symbols.erase(entry);
        }

        C_LANGUAGE()
        {
            TL::Type type = node.get_type();
            if (type.is_valid())
            {
                walk_type_for_symbols(
                        type,
                        decl_sym_fun,
                        def_sym_fun,
                        define_entities_fun);
            }

            // Special case for pointer arithmetic
            if (node.is<Nodecl::Add>())
            {
                Nodecl::Add add = node.as<Nodecl::Add>();

                TL::Type pointer_type;
                if (is_pointer_arithmetic_add(add, /* out */ pointer_type))
                {
                    walk_type_for_symbols(
                            pointer_type.points_to(),
                            decl_sym_fun,
                            def_sym_fun,
                            define_entities_fun);
                }
            }
        }

    }
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
                || node.is<Nodecl::CxxDecl>()
                || node.is<Nodecl::CxxDef>())
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

// This function is only for C
// Do not call it in C++
void CxxBase::walk_type_for_symbols(TL::Type t,
        void (CxxBase::* symbol_to_declare)(TL::Symbol),
        void (CxxBase::* symbol_to_define)(TL::Symbol),
        void (CxxBase::* define_entities_in_tree)(const Nodecl::NodeclBase&),
        bool needs_definition)
{
    if (!t.is_valid())
        return;

    // This effectively poisons return, do not return from this function
#define return 1=1;

    // This must be checked first since all query functions ignore typedefs
    if (t.is_named()
            && t.get_symbol().is_typedef())
    {
        walk_type_for_symbols(
                t.get_symbol().get_type(),
                symbol_to_declare,
                symbol_to_define,
                define_entities_in_tree,
                needs_definition);

        (this->*symbol_to_define)(t.get_symbol());
    }
    else if (t.is_indirect())
    {
        walk_type_for_symbols(
                t.get_symbol().get_type(),
                symbol_to_declare,
                symbol_to_define,
                define_entities_in_tree,
                needs_definition);
    }
    else if (t.is_pointer())
    {
        walk_type_for_symbols(t.points_to(), symbol_to_declare, symbol_to_define,
                define_entities_in_tree,
                /* needs_definition */ false);
    }
    else if (t.is_pointer_to_member())
    {
        walk_type_for_symbols(t.pointed_class(), symbol_to_declare, symbol_to_define,
                define_entities_in_tree, /* needs_definition */ false);

        walk_type_for_symbols(t.points_to(), symbol_to_declare, symbol_to_define,
                define_entities_in_tree, /* needs_definition */ false);
    }
    else if (t.is_array())
    {
        (this->*define_entities_in_tree)(t.array_get_size());
        walk_type_for_symbols(t.array_element(),
                symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (t.is_lvalue_reference()
            || t.is_rvalue_reference())
    {
        walk_type_for_symbols(t.references_to(),
                symbol_to_declare, symbol_to_define,
                define_entities_in_tree);
    }
    else if (t.is_function())
    {
        walk_type_for_symbols(t.returns(),
                symbol_to_declare, symbol_to_define, define_entities_in_tree);

        TL::ObjectList<TL::Type> params = t.parameters();
        for (TL::ObjectList<TL::Type>::iterator it = params.begin();
                it != params.end();
                it++)
        {
            walk_type_for_symbols(*it,
                    symbol_to_declare,
                    symbol_to_define,
                    define_entities_in_tree);
        }

        TL::ObjectList<TL::Type> nonadjusted_params = t.nonadjusted_parameters();
        for (TL::ObjectList<TL::Type>::iterator it = nonadjusted_params.begin();
                it != nonadjusted_params.end();
                it++)
        {
            walk_type_for_symbols(*it,
                    symbol_to_declare,
                    symbol_to_define,
                    define_entities_in_tree);
        }
    }
    else if (t.is_vector())
    {
        walk_type_for_symbols(t.vector_element(), symbol_to_declare, symbol_to_define, define_entities_in_tree);
    }
    else if (t.is_named_class())
    {
        TL::Symbol class_entry = t.get_symbol();
        if (needs_definition)
        {
            define_or_declare_if_complete(class_entry, symbol_to_declare, symbol_to_define);
        }
        else
        {
            (this->*symbol_to_declare)(class_entry);
        }
    }
    else if (t.is_unnamed_class())
    {
        // Special case for nested members

        // Bases
        TL::ObjectList<TL::Type::BaseInfo> bases = t.get_bases();
        for (TL::ObjectList<TL::Type::BaseInfo>::iterator it = bases.begin();
                it != bases.end();
                it++)
        {
            TL::Symbol &base_class(it->base);
            define_or_declare_if_complete(base_class, symbol_to_declare, symbol_to_define);
        }

        // Members
        TL::ObjectList<TL::Symbol> members = t.get_all_members();
        for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                it != members.end();
                it++)
        {
            walk_type_for_symbols(it->get_type(), symbol_to_declare, symbol_to_define, define_entities_in_tree);
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
            (this->*define_entities_in_tree)(enumerator.get_value());
        }
    }
    else if (t.is_named_enum())
    {
        TL::Symbol enum_entry = t.get_symbol();

        // Only walk the enumerators if not already defined
        if (get_codegen_status(enum_entry) != CODEGEN_STATUS_DEFINED)
        {
            walk_type_for_symbols(enum_entry.get_type(), symbol_to_declare, symbol_to_define, define_entities_in_tree);
        }
        define_or_declare_if_complete(enum_entry, symbol_to_declare, symbol_to_define);
    }
    else if (t.is_unresolved_overload())
    {
        TL::ObjectList<TL::Symbol> unresolved_set = t.get_unresolved_overload_set();

        for (TL::ObjectList<TL::Symbol>::iterator it = unresolved_set.begin();
                it != unresolved_set.end();
                it++)
        {
            (this->*symbol_to_define)(*it);
        }
    }
    else if (t.is_dependent_typename())
    {
        Nodecl::NodeclBase nodecl_parts = Nodecl::NodeclBase::null();
        TL::Symbol dependent_entry(NULL);
        t.dependent_typename_get_components(dependent_entry, nodecl_parts);

        if (!(dependent_entry.is_class()
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

        set_codegen_status(namespace_nesting_to[i], CODEGEN_STATUS_DEFINED);

        // Anonymous namespace has special properties that we want to preserve
        if (real_name == "(unnamed)")
        {
            real_name = "/* anonymous */";
        }

        std::string gcc_attributes = "";
        if (namespace_nesting_to[i]->entity_specs.num_gcc_attributes > 0)
        {
            gcc_attributes =
                " " + gcc_attributes_to_str(namespace_nesting_to[i]);
        }

        indent();
        file << "namespace " << real_name << gcc_attributes << " {\n";
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

void CxxBase::move_to_namespace(TL::Symbol namespace_sym)
{
    C_LANGUAGE()
    {
        return;
    }

    ERROR_CONDITION(!namespace_sym.is_namespace(), "This is not a namespace", 0);

    // First close the namespaces
    codegen_move_namespace_from_to(state.opened_namespace, namespace_sym.get_internal_symbol());
    state.opened_namespace = namespace_sym.get_internal_symbol();
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

void CxxBase::walk_list(const Nodecl::List& list, const std::string& separator, bool parenthesize_elements)
{
    Nodecl::List::const_iterator it = list.begin(), begin = it;
    bool default_argument = false;
    while (it != list.end())
    {
        Nodecl::NodeclBase current_node = *it;

        if (current_node.is<Nodecl::DefaultArgument>())
        {
            if (!default_argument)
            {
                default_argument = true;
                file << "/* ";
            }
            current_node = current_node.as<Nodecl::DefaultArgument>().get_argument();
        }

        if (it != begin)
            file << separator;

        if (parenthesize_elements)
            file << "(";

        walk(current_node);

        if (parenthesize_elements)
            file << ")";

        it++;
    }

    if (default_argument)
        file << " */";
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

template < typename Node>
node_t CxxBase::get_kind_of_operator_function_call(const Node & node)
{
    ERROR_CONDITION(!is_operator_function_call(node), "This function is not an operator\n", 0);

    TL::Symbol called_sym = node.get_called().get_symbol();
    std::string operator_name = called_sym.get_name().substr(std::string("operator ").size());

    if (is_binary_infix_operator_function_call(node))
    {
        if (operator_name == "->*")      return NODECL_CXX_ARROW_PTR_MEMBER;
        else if (operator_name == ".*")  return NODECL_CXX_DOT_PTR_MEMBER;
        else if (operator_name == "*")   return NODECL_MUL;
        else if (operator_name == "/")   return NODECL_DIV;
        else if (operator_name == "%")   return NODECL_MOD;
        else if (operator_name == "+")   return NODECL_ADD;
        else if (operator_name == "-")   return NODECL_MINUS;
        else if (operator_name == "<<")  return NODECL_BITWISE_SHL;
        else if (operator_name == ">>")  return NODECL_BITWISE_SHR;
        else if (operator_name == "<")   return NODECL_LOWER_THAN;
        else if (operator_name == "<=")  return NODECL_LOWER_OR_EQUAL_THAN;
        else if (operator_name == ">")   return NODECL_GREATER_THAN;
        else if (operator_name == ">=")  return NODECL_GREATER_OR_EQUAL_THAN;
        else if (operator_name == "==")  return NODECL_EQUAL;
        else if (operator_name == "!=")  return NODECL_DIFFERENT;
        else if (operator_name == "&")   return NODECL_BITWISE_AND;
        else if (operator_name == "^")   return NODECL_BITWISE_XOR;
        else if (operator_name == "|")   return NODECL_BITWISE_OR;
        else if (operator_name == "&&")  return NODECL_LOGICAL_AND;
        else if (operator_name == "||")  return NODECL_LOGICAL_OR;
        else if (operator_name == "?")   return NODECL_CONDITIONAL_EXPRESSION;
        else if (operator_name == "=")   return NODECL_ASSIGNMENT;
        else if (operator_name == "*=")  return NODECL_MUL_ASSIGNMENT;
        else if (operator_name == "/=")  return NODECL_DIV_ASSIGNMENT;
        else if (operator_name == "%=")  return NODECL_MOD_ASSIGNMENT;
        else if (operator_name == "+=")  return NODECL_ADD_ASSIGNMENT;
        else if (operator_name == "-=")  return NODECL_MINUS_ASSIGNMENT;
        else if (operator_name == "<<=") return NODECL_BITWISE_SHL_ASSIGNMENT;
        else if (operator_name == ">>=") return NODECL_BITWISE_SHR_ASSIGNMENT;
        else if (operator_name == "&=")  return NODECL_BITWISE_AND;
        else if (operator_name == "|=")  return NODECL_BITWISE_OR;
        else if (operator_name == "^=")  return NODECL_BITWISE_XOR;
        else if (operator_name == ",")   return NODECL_COMMA;
    }
    else
    {
        if (operator_name == "&")      return NODECL_REFERENCE;
        else if (operator_name == "*") return NODECL_DEREFERENCE;
        else if (operator_name == "+") return NODECL_PLUS;
        else if (operator_name == "-") return NODECL_NEG;
        else if (operator_name == "!") return NODECL_LOGICAL_NOT;
        else if (operator_name == "~") return NODECL_BITWISE_NOT;
        else if (operator_name == "++" && is_unary_prefix_operator_function_call(node))  return NODECL_PREINCREMENT;
        else if (operator_name == "++" && is_unary_postfix_operator_function_call(node)) return NODECL_POSTINCREMENT;
        else if (operator_name == "--" && is_unary_prefix_operator_function_call(node))  return NODECL_PREDECREMENT;
        else if (operator_name == "--" && is_unary_postfix_operator_function_call(node)) return NODECL_POSTDECREMENT;
    }
    internal_error("function operator '%s' is not supported yet\n", called_sym.get_name().c_str());

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
        case NODECL_VIRTUAL_FUNCTION_CALL:
        case NODECL_CLASS_MEMBER_ACCESS:
        case NODECL_PSEUDO_DESTRUCTOR_NAME:
        case NODECL_TYPEID:
        case NODECL_POSTINCREMENT:
        case NODECL_POSTDECREMENT:

        case NODECL_CXX_ARROW:
        case NODECL_CXX_POSTFIX_INITIALIZER:
        case NODECL_CXX_ARRAY_SECTION_RANGE:
        case NODECL_CXX_ARRAY_SECTION_SIZE:
        case NODECL_CXX_EXPLICIT_TYPE_CAST:
        case NODECL_CXX_DEP_FUNCTION_CALL:
            {
                return -2;
            }
        case NODECL_REFERENCE:
        case NODECL_DEREFERENCE:
        case NODECL_PLUS:
        case NODECL_NEG:
        case NODECL_LOGICAL_NOT:
        case NODECL_BITWISE_NOT:
        case NODECL_SIZEOF:
        case NODECL_NEW:
        case NODECL_DELETE:
        case NODECL_DELETE_ARRAY:
        case NODECL_PREINCREMENT:
        case NODECL_PREDECREMENT:
        case NODECL_REAL_PART:
        case NODECL_IMAG_PART:
            // FIXME: Missing GCC nodes
            // FIXME: Do we want them or we can use builtins?
            // case NODECL_ALIGNOF
            // case NODECL_LABEL_ADDR
        case NODECL_CXX_SIZEOF:
        case NODECL_CXX_ALIGNOF:
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

        case NODECL_CXX_ARROW_PTR_MEMBER:
        case NODECL_CXX_DOT_PTR_MEMBER:
            return -5;
        case NODECL_MUL:
        case NODECL_DIV:
        case NODECL_MOD:
            return -6;
        case NODECL_ADD:
        case NODECL_MINUS:
            return -7;
        case NODECL_BITWISE_SHL:
        case NODECL_BITWISE_SHR:
        case NODECL_ARITHMETIC_SHR:
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
        case NODECL_ASSIGNMENT:
        case NODECL_MUL_ASSIGNMENT:
        case NODECL_DIV_ASSIGNMENT:
        case NODECL_MOD_ASSIGNMENT:
        case NODECL_ADD_ASSIGNMENT:
        case NODECL_MINUS_ASSIGNMENT:
        case NODECL_BITWISE_SHL_ASSIGNMENT:
        case NODECL_BITWISE_SHR_ASSIGNMENT:
        case NODECL_ARITHMETIC_SHR_ASSIGNMENT:
        case NODECL_BITWISE_AND_ASSIGNMENT:
        case NODECL_BITWISE_OR_ASSIGNMENT:
        case NODECL_BITWISE_XOR_ASSIGNMENT:
        case NODECL_THROW:
            return -16;
        case NODECL_COMMA:
            return -17;
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
        node_t kind;
        if (n.is<Nodecl::FunctionCall>()
                && is_operator_function_call(n.as<Nodecl::FunctionCall>()))
        {
            kind = get_kind_of_operator_function_call(n.as<Nodecl::FunctionCall>());
        }
        else if (n.is<Nodecl::VirtualFunctionCall>()
                && is_operator_function_call(n.as<Nodecl::VirtualFunctionCall>()))
        {
            kind = get_kind_of_operator_function_call(n.as<Nodecl::VirtualFunctionCall>());
        }
        else
        {
            kind = n.get_kind();
        }
        return get_rank_kind(kind, n.get_text());
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
    return n == NODECL_BITWISE_SHL
        || n == NODECL_BITWISE_SHR
        || n == NODECL_ARITHMETIC_SHR;
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

void CxxBase::codegen_template_headers_bounded(
        TL::TemplateParameters template_parameters,
        TL::TemplateParameters lim,
        bool show_default_values)
{
    if (!template_parameters.is_valid())
        return;

    if (template_parameters != lim)
    {
        if (template_parameters.has_enclosing_parameters())
        {
            TL::TemplateParameters enclosing_template_parameters =
                template_parameters.get_enclosing_parameters();

            // We only print the default arguments of the innermost template header.
            // For this reason we set show_default_values to false
            codegen_template_headers_bounded(
                    enclosing_template_parameters, lim,
                    /* show_default_values */ false);
        }
        codegen_template_header(template_parameters, show_default_values);
    }
}

void CxxBase::codegen_template_headers_all_levels(
        TL::TemplateParameters template_parameters,
        bool show_default_values)
{
    if (!template_parameters.is_valid())
        return;

    if (template_parameters.has_enclosing_parameters())
    {
        TL::TemplateParameters enclosing_template_parameters =
            template_parameters.get_enclosing_parameters();

        // We only print the default arguments of the innermost template header
        // For this reason we set show_default_values to false
        codegen_template_headers_all_levels(
                enclosing_template_parameters,
                /* show_default_values */ false);
    }
    codegen_template_header(template_parameters, show_default_values);
}

void CxxBase::codegen_template_header(
        TL::TemplateParameters template_parameters,
        bool show_default_values,
        bool endline)
{
    if (!template_parameters.is_valid())
        return;

    indent();
    if (template_parameters.get_is_explicit_specialization())
    {
        file << "template <>";
        if (endline)
            file << "\n";
        return;
    }

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
                    codegen_template_header(
                            symbol.get_type().template_type_get_template_parameters(),
                            show_default_values,
                            /* endline */ false);
                    file << " class " << symbol.get_name();
                    break;
                }
            default:
                {
                    internal_error("Invalid template parameter kind", 0);
                }
        }

        // Has this template parameter a default value?
        if (show_default_values &&
                template_parameters.has_argument(i))
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
                            file <<
                                this->print_type_str(
                                        temp_arg_type.get_internal_type(),
                                        symbol.get_scope().get_decl_context(),
                                        /* we need to store the current codegen */ (void*) this);
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
    int attributes_counter = 0;
    bool print_cuda_attributes = cuda_print_special_attributes();

    for (TL::ObjectList<TL::GCCAttribute>::iterator it = gcc_attr_list.begin();
            it != gcc_attr_list.end();
            it++)
    {
        if (attributes_counter > 0)
            result += " ";

        if (!print_cuda_attributes
                && (it->get_attribute_name() == "host"
                    || it->get_attribute_name() == "device"
                    || it->get_attribute_name() == "shared"
                    || it->get_attribute_name() == "constant"
                    || it->get_attribute_name() == "global"))
             continue;

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
        attributes_counter++;
    }
    return result;
}

std::string CxxBase::ms_attributes_to_str(TL::Symbol symbol)
{
    std::string result;
    TL::ObjectList<TL::MSAttribute> ms_attr_list = symbol.get_ms_attributes();
    int attributes_counter = 0;

    for (TL::ObjectList<TL::MSAttribute>::iterator it = ms_attr_list.begin();
            it != ms_attr_list.end();
            it++)
    {
        if (attributes_counter > 0)
            result += " ";

        if (it->get_expression_list().is_null())
        {
            result += "__declspec(" + it->get_attribute_name() + ")";
        }
        else
        {
            result += "__declspec(" + it->get_attribute_name() + "(";

            std::string old_str = file.str();

            file.clear();
            file.str("");

            walk_expression_list(it->get_expression_list().as<Nodecl::List>());

            result += file.str();

            file.clear();
            file.str(old_str);
            // Go to the end of the stream...
            file.seekp(0, std::ios_base::end);

            result += "))";
        }
        attributes_counter++;
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

const char* CxxBase::print_name_str(scope_entry_t* sym, decl_context_t decl_context, void *data)
{
    // We obtain the current codegen from the data
    CxxBase* _this = (CxxBase*) data;

    // The variable data must contain a valid pointer to the current codegen
    ERROR_CONDITION(_this == NULL, "Invalid this", 0);
    ERROR_CONDITION(sym == NULL, "Invalid symbol", 0);

    const char* result = NULL;
    if (IS_CXX_LANGUAGE
            && _this->get_codegen_status(sym) == CODEGEN_STATUS_NONE
            && ((sym->kind == SK_CLASS && !is_template_specialized_type(sym->type_information))
                || sym->kind == SK_ENUM))
    {
        result = sym->symbol_name;

        if (sym->kind == SK_ENUM)
        {
            result = strappend("enum ", result);
        }
        else if (sym->kind == SK_CLASS)
        {
            switch (class_type_get_class_kind(sym->type_information))
            {
                case TT_UNION:
                    result = strappend("union ", result); break;
                case TT_STRUCT:
                    result = strappend("struct ", result); break;
                case TT_CLASS:
                    result = strappend("class ", result); break;
                default:
                    internal_error("Code unreachable", 0);
            }
        }
    }
    else
    {
        char is_dependent = 0;
        int max_level = 0;
        result =
            get_fully_qualified_symbol_name_ex(sym,
                    decl_context, &is_dependent, &max_level,
                    /* no_templates */ 0, /* only_classes */ 0,
                    /* do_not_emit_template_keywords */ 0,
                    print_type_str,
                    data);

        // If is a dependent name and it is qualified then it can be
        // given a "typename" keyword (in some cases one must do that)
        if (is_dependent && max_level > 0)
        {
            result = strappend("typename ", result);
        }

        if (IS_CXX_LANGUAGE
                && (sym->kind == SK_CLASS
                    || sym->kind == SK_ENUM))
        {

            // It may happen that a function is hiding our typename in this scope
            scope_entry_list_t* entry_list = query_in_scope_str(sym->decl_context, sym->symbol_name);
            entry_list = filter_symbol_using_predicate(entry_list,
                    is_function_or_template_function_name_or_extern_variable, NULL);

            // It seems somebody is hiding our name in this scope
            if (entry_list != NULL)
            {
                if (sym->kind == SK_ENUM)
                {
                    result = strappend("enum ", result);
                }
                else if (sym->kind == SK_CLASS)
                {
                    switch (class_type_get_class_kind(sym->type_information))
                    {
                        case TT_UNION:
                            result = strappend("union ", result); break;
                        case TT_STRUCT:
                            result = strappend("struct ", result); break;
                        case TT_CLASS:
                            result = strappend("class ", result); break;
                        default:
                            internal_error("Code unreachable", 0);
                    }
                }
            }

            entry_list_free(entry_list);
        }
    }
    return result;
}

const char* CxxBase::print_type_str(type_t* t, decl_context_t decl_context, void *data)
{
    const char* result = NULL;
    if (t == NULL)
    {
        result = uniquestr("< unknown type >");
    }
    else
    {
        result = get_declaration_string_ex(t,
                decl_context, /* symbol_name */"",
                /* initializer */ "",
                /* semicolon */ 0,
                /* num_parameter_names */ 0,
                /* parameter_names */ NULL,
                /* parameter_attributes */ NULL,
                /* is_parameter */ 0,
                print_name_str,
                data);
    }
    return result;
}

std::string CxxBase::get_declaration(TL::Type t, TL::Scope scope, const std::string& name)
{
    t = fix_references(t);

    return get_declaration_string_ex(t.get_internal_type(), scope.get_decl_context(),
            name.c_str(), "", 0, 0, NULL, NULL, /* is_parameter */ 0, print_name_str,
            /* we need to store the current codegen */ (void*) this);
}

std::string CxxBase::get_declaration_only_declarator(TL::Type t, TL::Scope scope, const std::string& name)
{
    t = fix_references(t);

    return get_declarator_name_string_ex(
            scope.get_decl_context(),
            t.get_internal_type(),
            name.c_str(),
            /* num_parameter_names */ 0,
            /* parameter_names */ NULL,
            /* parameter_attributes */ NULL,
            /* is_parameter */ 0,
            print_name_str,
            /* we need to store the current codegen */ (void*) this);
}

std::string CxxBase::get_qualified_name(TL::Symbol sym, bool without_template_id) const
{
    return this->get_qualified_name(sym, sym.get_scope(), without_template_id);
}

std::string CxxBase::get_qualified_name(TL::Symbol sym, TL::Scope sc, bool without_template_id) const
{
    if (sym.get_internal_symbol()->symbol_name == NULL)
    {
        return std::string("");
    }
    else
    {
        const char* result = NULL;
        int max_level = 0;
        char is_dependent = 0;
        if (without_template_id)
        {
            result = get_fully_qualified_symbol_name_ex(sym.get_internal_symbol(),
                    sc.get_decl_context(), &is_dependent, &max_level,
                    /* no_templates */ 1, /* only_classes */ 0,
                    /* do_not_emit_template_keywords */ 0,
                    print_type_str,
                    /* we need to store the current codegen */ (void*) this);
        }
        else
        {
            result = get_fully_qualified_symbol_name_ex(sym.get_internal_symbol(),
                    sc.get_decl_context(), &is_dependent, &max_level,
                    /* no_templates */ 0, /* only_classes */ 0,
                    /* do_not_emit_template_keywords */ 0,
                    print_type_str,
                    /* we need to store the current codegen */ (void*) this);
        }
        return std::string(result);
    }
}

void CxxBase::fill_parameter_names_and_parameter_attributes(TL::Symbol symbol,
        TL::ObjectList<std::string>& parameter_names,
        TL::ObjectList<std::string>& parameter_attributes)
{
    ERROR_CONDITION(!symbol.is_function()
            && !symbol.is_dependent_friend_function(), "This symbol should be a function\n", -1);

    int i = 0;
    TL::ObjectList<TL::Symbol> related_symbols = symbol.get_related_symbols();
    for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
            it != related_symbols.end();
            it++, i++)
    {
        TL::Symbol current_param = *it;
        if (current_param.is_valid())
        {
            if(!current_param.not_to_be_printed())
            {
                parameter_names[i] = current_param.get_name();
            }

            if (current_param.has_gcc_attributes())
            {
                parameter_attributes[i] = gcc_attributes_to_str(current_param);
            }

            if (get_codegen_status(current_param) != CODEGEN_STATUS_DEFINED
                    && symbol.has_default_argument_num(i))
            {
                parameter_attributes[i] += " = " + codegen(symbol.get_default_argument_num(i));
            }
            set_codegen_status(current_param, CODEGEN_STATUS_DEFINED);
        }
    }
}

std::string CxxBase::get_declaration_with_parameters(TL::Type t,
        TL::Scope scope,
        const std::string& symbol_name,
        TL::ObjectList<std::string>& parameters,
        TL::ObjectList<std::string>& parameter_attributes)
{
    t = fix_references(t);

    int num_parameters = t.parameters().size();

    const char** parameter_names  = new const char*[num_parameters + 1];
    const char** param_attributes = new const char*[num_parameters + 1];

    for (int i = 0; i < num_parameters; i++)
    {
        parameter_names[i] = NULL;
        param_attributes[i] = NULL;
    }

    int orig_size = parameters.size();
    for (int i = 0; i < orig_size; i++)
    {
        parameter_names[i] = uniquestr(parameters[i].c_str());
        param_attributes[i] = uniquestr(parameter_attributes[i].c_str());
    }

    const char* result = get_declaration_string_ex(t.get_internal_type(),
            scope.get_decl_context(), symbol_name.c_str(), "", 0,
            num_parameters, parameter_names, param_attributes,
            /* is_parameter */ 1, print_name_str,
            /* we need to store the current codegen */ (void*) this);

    for (int i = 0; i < num_parameters; i++)
    {
        if (i < orig_size)
        {
            parameters[i] = parameter_names[i];
        }
        else
        {
            if (parameter_names[i] != NULL)
                parameters.append(parameter_names[i]);
            else
                parameters.append("");
        }
    }

    delete[] parameter_names;
    delete[] param_attributes;

    return result;
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
        if (t.array_is_region())
        {
            Nodecl::NodeclBase lb, reg_lb, ub, reg_ub;
            t.array_get_bounds(lb, ub);
            t.array_get_region_bounds(reg_lb, reg_ub);
            TL::Scope sc = array_type_get_region_size_expr_context(t.get_internal_type());

            return fix_references(t.array_element()).get_array_to_with_region(lb, ub, reg_lb, reg_ub, sc);
        }
        else
        {
            Nodecl::NodeclBase size = t.array_get_size();
            TL::Scope sc = array_type_get_array_size_expr_context(t.get_internal_type());

            return fix_references(t.array_element()).get_array_to(size, sc);
        }
    }
    else if (t.is_pointer())
    {
        TL::Type fixed = fix_references(t.points_to()).get_pointer_to();

        fixed = ::get_cv_qualified_type(fixed.get_internal_type(),
                get_cv_qualifier(t.get_internal_type()));

        return fixed;
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

        TL::ObjectList<TL::Type> nonadjusted_fixed_parameters = t.nonadjusted_parameters();
        for (TL::ObjectList<TL::Type>::iterator it = nonadjusted_fixed_parameters.begin();
                it != nonadjusted_fixed_parameters.end();
                it++)
        {
            *it = fix_references(*it);
        }

        TL::Type fixed_function = fixed_result.get_function_returning(
                fixed_parameters,
                nonadjusted_fixed_parameters,
                has_ellipsis);

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

bool CxxBase::cuda_print_special_attributes()
{
    return false;
}

bool CxxBase::cuda_emit_always_extern_linkage()
{
    return false;
}

CxxBase::CxxBase()
{
    set_phase_name("C/C++ codegen");
    set_phase_description("This phase emits in C/C++ the intermediate representation of the compiler");

    _emit_saved_variables_as_unused = false;
    register_parameter("emit_saved_variables_as_unused",
            "Emits saved-expression variables as __attribute__((unused))",
            _emit_saved_variables_as_unused_str,
            "0").connect(functor(&CxxBase::set_emit_saved_variables_as_unused, *this));

    _prune_saved_variables = true;
    register_parameter("prune_saved_variables",
            "Disables removal of unused saved-expression variables. If you need to enable this, please report a ticket",
            _prune_saved_variables_str,
            "1").connect(functor(&CxxBase::set_prune_saved_variables, *this));
}

void CxxBase::set_emit_saved_variables_as_unused(const std::string& str)
{
    TL::parse_boolean_option("emit_saved_variables_as_unused", str, _emit_saved_variables_as_unused, "Assuming false.");
}

void CxxBase::set_prune_saved_variables(const std::string& str)
{
    TL::parse_boolean_option("prune_saved_variables", str, _prune_saved_variables, "Assuming true.");
}

} // Codegen

EXPORT_PHASE(Codegen::CxxBase)
