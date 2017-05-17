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

#include "codegen-cxx.hpp"
#include "codegen-prune.hpp"
#include "tl-objectlist.hpp"
#include "tl-type.hpp"
#include "tl-member-decl.hpp"
#include "cxx-cexpr.h"
#include "cxx-entrylist.h"
#include "string_utils.h"
#include "tl-compilerpipeline.hpp"
#include "tl-counters.hpp"
#include "cxx-intelsupport.h"
#include <iomanip>
#ifdef HAVE_QUADMATH_H
MCXX_BEGIN_DECLS
#include <quadmath.h>
MCXX_END_DECLS
#endif

#include "tl-nodecl-utils.hpp"

#include "cxx-printscope.h"
#include "cxx-gccbuiltins.h"
namespace Codegen {

void CxxBase::codegen(const Nodecl::NodeclBase &n, const State &new_state, std::ostream* out)
{
    if (n.is_null())
        return;

    // Keep the state
    State old_state = state;
    state = new_state;
    std::ostream* old_out = file;

    state.nontype_template_argument_needs_parentheses =
        old_state.nontype_template_argument_needs_parentheses;

    const decl_context_t* decl_context = this->get_current_scope().get_decl_context();

    state.global_namespace = decl_context->global_scope->related_entry;
    state.opened_namespace = decl_context->namespace_scope->related_entry;

    state.emit_declarations = this->is_file_output() ? State::EMIT_ALL_DECLARATIONS : State::EMIT_NO_DECLARATIONS;

    file = out;

    walk(n);

    // Make sure the starting namespace is closed
    codegen_move_namespace_from_to(state.opened_namespace, decl_context->namespace_scope->related_entry);

    // Restore previous state
    file = old_out;
    state = old_state;
}

namespace {
    Nodecl::NodeclBase no_conv(Nodecl::NodeclBase n)
    {
        if (n.is<Nodecl::Conversion>() && n.get_text() == "")
            return no_conv(n.as<Nodecl::Conversion>().get_nest());
        return n;
    }
}

void CxxBase::codegen(const Nodecl::NodeclBase &n, std::ostream* out)
{
    codegen(n, State(), out);
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
    PREFIX_UNARY_EXPRESSION(Preincrement, " ++") \
    PREFIX_UNARY_EXPRESSION(Predecrement, " --") \
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



#if 0
    BINARY_EXPRESSION(VectorAdd, " + ") \
    BINARY_EXPRESSION(VectorMul, " * ") \
    BINARY_EXPRESSION(VectorDiv, " / ") \
    BINARY_EXPRESSION(VectorMod, " % ") \
    BINARY_EXPRESSION(VectorMinus, " - ") \
    BINARY_EXPRESSION(VectorEqual, " == ") \
    BINARY_EXPRESSION(VectorDifferent, " != ") \
    BINARY_EXPRESSION(VectorLowerThan, " < ") \
    BINARY_EXPRESSION(VectorLowerOrEqualThan, " <= ") \
    BINARY_EXPRESSION_EX(VectorGreaterThan, " > ") \
    BINARY_EXPRESSION_EX(VectorGreaterOrEqualThan, " >= ") \
    BINARY_EXPRESSION(VectorLogicalAnd, " && ") \
    BINARY_EXPRESSION(VectorLogicalOr, " || ") \
    BINARY_EXPRESSION(VectorBitwiseAnd, " & ") \
    BINARY_EXPRESSION(VectorBitwiseOr, " | ") \
    BINARY_EXPRESSION(VectorBitwiseXor, " ^ ") \
    BINARY_EXPRESSION(VectorBitwiseShl, " << ") \
    BINARY_EXPRESSION_EX(VectorBitwiseShr, " >> ") \
    BINARY_EXPRESSION_EX(VectorArithmeticShr, " >> ") \
    BINARY_EXPRESSION_ASSIG(VectorAssignment, " = ") \
    BINARY_EXPRESSION_ASSIG(VectorMaskAssignment, " = ") \

#endif
 
#define PREFIX_UNARY_EXPRESSION(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name &node) \
    { \
        emit_line_marker(node); \
        Nodecl::NodeclBase rhs = node.children()[0]; \
        char needs_parentheses = operand_has_lower_priority(node, rhs); \
        *(file) << _operand; \
        if (needs_parentheses) \
        { \
            *(file) << "("; \
        } \
        walk(rhs); \
        if (needs_parentheses) \
        { \
            *(file) << ")"; \
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

void CxxBase::emit_saved_locus()
{
    const locus_t* locus = state._saved_locus;

    if (locus == NULL)
        return;

    // Do we need a newline?
    if (!last_is_newline())
        *file << "\n";

    std::string filename = locus_get_filename(locus);
    int line = locus_get_line(locus);

    // #line linenum filename
    *file << "#line " << line << " \"" << filename << "\"\n";
    *file << TL::pad_to_column(locus_get_column(locus));
}

void CxxBase::emit_line_marker(const locus_t* locus)
{
    if (!CURRENT_CONFIGURATION->line_markers)
        return;

    bool save_locus =
        (is_file_output()
         && locus != NULL
         && locus_get_line(locus) != 0
         && *locus_get_filename(locus) != '\0');

    if (save_locus)
    {
        std::string filename = locus_get_filename(locus);
        const std::string internal_source = "MERCURIUM_INTERNAL_SOURCE";
        if (filename.size() >= internal_source.size())
        {
            std::string prefix = filename.substr(0, internal_source.size());
            if (prefix == internal_source)
                save_locus = false;
        }
    }

    if (save_locus)
    {
        state._saved_locus = locus;
    }

    emit_saved_locus();
}

void CxxBase::emit_line_marker(Nodecl::NodeclBase n)
{
    emit_line_marker(n.get_locus());
}

void CxxBase::visit(const Nodecl::Reference &node)
{
    Nodecl::NodeclBase rhs = node.get_rhs();
    bool needs_parentheses = operand_has_lower_priority(node, rhs);
    bool needs_this = false;

    CXX_LANGUAGE()
    {
        // struct A
        // {
        //    int x;
        //    void f(int *);
        //    void g();
        // };
        //
        // void A::g()
        // {
        //    f(&x);
        //    // Emitting f(&A::x) is wrong but f(&(A::x)) is fine...
        // }
        //
        // But versions before g++ 4.7 do not honor the parentheses this and
        // ignore the parentheses so it is safer to do &(*this).A::x

        needs_this = (rhs.is<Nodecl::Symbol>()
                && rhs.get_symbol().is_member()
                && !rhs.get_symbol().is_static()
                && !node.get_type().is_pointer_to_member());
    }

    bool old_do_not_derref_rebindable_ref = state.do_not_derref_rebindable_reference;

    if (rhs.get_type().is_rebindable_reference())
    {
        state.do_not_derref_rebindable_reference = true;
    }
    else if (is_non_language_reference_variable(rhs))
    {
        state.do_not_derref_rebindable_reference = true;

        // Emit a casting here
        *(file) << "(" << this->get_declaration(node.get_type(), this->get_current_scope(), "") << ") ";
    }
    else
    {
        *(file) << "&";
    }

    if (needs_parentheses)
    {
        *(file) << "(";
    }
    if (needs_this)
    {
        *(file) << "(*this).";
    }
    walk(rhs);
    if (needs_parentheses)
    {
        *(file) << ")";
    }

    state.do_not_derref_rebindable_reference = old_do_not_derref_rebindable_ref;
}

#define POSTFIX_UNARY_EXPRESSION(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name& node) \
    { \
        emit_line_marker(node); \
        Nodecl::NodeclBase rhs = node.children()[0]; \
        char needs_parentheses = operand_has_lower_priority(node, rhs); \
        if (needs_parentheses) \
        { \
            *(file) << "("; \
        } \
        walk(rhs); \
        if (needs_parentheses) \
        { \
            *(file) << ")"; \
        } \
        *(file) << _operand; \
    }

#define BINARY_EXPRESSION(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name& node) \
    { \
        BINARY_EXPRESSION_IMPL(_name, _operand) \
    }

#define BINARY_EXPRESSION_IMPL(_name, _operand) \
   emit_line_marker(node); \
   Nodecl::NodeclBase lhs = node.children()[0]; \
   Nodecl::NodeclBase rhs = node.children()[1]; \
   char needs_parentheses = operand_has_lower_priority(node, lhs); \
   if (needs_parentheses) \
   { \
       *(file) << "("; \
   } \
   walk(lhs); \
   if (needs_parentheses) \
   { \
       *(file) << ")"; \
   } \
   *(file) << _operand; \
   needs_parentheses = operand_has_lower_priority(node, rhs) || same_operation(node, rhs); \
   if (needs_parentheses) \
   { \
       *(file) << "("; \
   } \
   walk(rhs); \
   if (needs_parentheses) \
   { \
       *(file) << ")"; \
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
        bool need_parentheses = state.nontype_template_argument_needs_parentheses; \
        if (need_parentheses) \
        {\
            *(file) << "("; \
        }\
        BINARY_EXPRESSION_IMPL(_name, _operand) \
        if (need_parentheses) \
        {\
            *(file) << ")"; \
        }\
    }

#define BINARY_EXPRESSION_ASSIG(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name& node) \
    { \
        BINARY_EXPRESSION_ASSIG_IMPL(_name, _operand) \
    }

#define BINARY_EXPRESSION_ASSIG_IMPL(_name, _operand) \
   emit_line_marker(node); \
   Nodecl::NodeclBase lhs = node.children()[0]; \
   Nodecl::NodeclBase rhs = node.children()[1]; \
   if (state.in_condition && state.condition_top == node) \
   { \
       *(file) << "("; \
   } \
   char needs_parentheses = get_rank(lhs) < get_rank_kind(NODECL_LOGICAL_OR, ""); \
   if (needs_parentheses) \
   { \
       *(file) << "("; \
   } \
   walk(lhs); \
   if (needs_parentheses) \
   { \
       *(file) << ")"; \
   } \
   *(file) << _operand; \
   needs_parentheses = get_rank(rhs) < get_rank_kind(NODECL_ASSIGNMENT, ""); \
   if (needs_parentheses) \
   { \
       *(file) << "("; \
   } \
   walk(rhs); \
   if (needs_parentheses) \
   { \
       *(file) << ")"; \
   } \
   if (state.in_condition && state.condition_top == node) \
   { \
       *(file) << ")"; \
   } \

// In some cases (i. e. when the operator of a binary expression assignment
// contains the character '>') nontype template arguments may need an extra
// parentheses
#define BINARY_EXPRESSION_ASSIG_EX(_name, _operand) \
    void CxxBase::visit(const Nodecl::_name& node) \
    { \
        bool need_parentheses = state.nontype_template_argument_needs_parentheses; \
        if (need_parentheses) \
        {\
            *(file) << "("; \
        }\
        BINARY_EXPRESSION_ASSIG_IMPL(_name, _operand) \
        if (need_parentheses) \
        {\
            *(file) << ")"; \
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
    emit_line_marker(node);
    Nodecl::NodeclBase subscripted = node.get_subscripted();
    Nodecl::List subscript = node.get_subscripts().as<Nodecl::List>();

    if (operand_has_lower_priority(node, subscripted))
    {
        *(file) << "(";
    }
    walk(subscripted);
    if (operand_has_lower_priority(node, subscripted))
    {
        *(file) << ")";
    }

    // We keep a list instead of a single dimension for multidimensional arrays
    // alla Fortran
    for(Nodecl::List::iterator it = subscript.begin();
           it != subscript.end();
           it++)
    {
        *(file) << "[";
        walk(*it);
        *(file) << "]";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::VectorSubscript& node)
{
    emit_line_marker(node);
    Nodecl::NodeclBase subscripted = node.get_subscripted();
    Nodecl::NodeclBase subscript = node.get_subscript();

    if (operand_has_lower_priority(node, subscripted))
    {
        *(file) << "(";
    }
    walk(subscripted);
    if (operand_has_lower_priority(node, subscripted))
    {
        *(file) << ")";
    }

    *(file) << "[";
    walk(subscript);
    *(file) << "]";
}

CxxBase::Ret CxxBase::visit(const Nodecl::BooleanLiteral& node)
{
    emit_line_marker(node);
    const_value_t* val = nodecl_get_constant(node.get_internal_nodecl());

    if (const_value_is_zero(val))
    {
        *(file) << "false";
    }
    else
    {
        *(file) << "true";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::BreakStatement& node)
{
    emit_line_marker(node);
    indent();
    *(file) << "break;\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::C99DesignatedInitializer& node)
{
    walk(node.get_designation());
    *(file) << " = ";
    walk(node.get_init());
}

CxxBase::Ret CxxBase::visit(const Nodecl::C99FieldDesignator& node)
{
    *(file) << ".";
    walk(node.get_name());
}

CxxBase::Ret CxxBase::visit(const Nodecl::C99IndexDesignator& node)
{
    *(file) << "[";
    walk(node.get_expr());
    *(file) << "]";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CaseStatement& node)
{
    Nodecl::NodeclBase expression = node.get_case();
    Nodecl::NodeclBase statement = node.get_statement();

    emit_line_marker(node);
    indent();
    *(file) << "case ";
    walk(expression);
    *(file) << " :\n";

    walk(statement);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CatchHandler& node)
{
    Nodecl::NodeclBase name = node.get_name();
    Nodecl::NodeclBase statement = node.get_statement();
    TL::Type type = node.get_type();

    emit_line_marker(node);
    indent();
    *(file) << "catch (";

    if (name.is_null())
    {
        // FIXME: Is this always safe?
        *(file) << this->get_declaration(type, this->get_current_scope(),  "");
    }
    else
    {
        int old_condition = state.in_condition;
        Nodecl::NodeclBase old_condition_top = state.condition_top;
        int old_indent = get_indent_level();
        set_indent_level(0);

        state.in_condition = 1;
        state.condition_top = no_conv(name);

        walk(name);

        set_indent_level(old_indent);
        state.condition_top = old_condition_top;
        state.in_condition = old_condition;
    }

    *(file) << ")\n";

    walk(statement);
}

CxxBase::Ret CxxBase::visit(const Nodecl::ClassMemberAccess& node)
{
    emit_line_marker(node);
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_member();
    Nodecl::NodeclBase member_literal = node.get_member_literal();

    // If this class member access sports a member literal, make sure we skip
    // all classes in the lhs
    if (!member_literal.is_null())
    {
        while (lhs.is<Nodecl::ClassMemberAccess>()
                && lhs.as<Nodecl::ClassMemberAccess>().get_member().is<Nodecl::Symbol>()
                && lhs.as<Nodecl::ClassMemberAccess>().get_member().get_symbol().is_class())
            lhs = lhs.as<Nodecl::ClassMemberAccess>().get_lhs();
    }

    TL::Symbol rhs_sym = rhs.get_symbol();

    /*
     *     .
     *    / \
     *   .   c
     *  / \
     * a   <<anon>>
     *
     * We want a.<<anon>>.c become a.c
     */

    bool is_anonymous_union_accessor = rhs_sym.is_valid()
        && rhs_sym.get_type().is_named_class()
        && rhs_sym.get_type().get_symbol().is_anonymous_union();

    bool must_derref_all = (rhs_sym.is_valid()
            && is_non_language_reference_variable(rhs_sym)
            && !rhs_sym.get_type().references_to().is_array()
            && !state.do_not_derref_rebindable_reference);

    bool old_do_not_derref_rebindable_ref = state.do_not_derref_rebindable_reference;

    if (must_derref_all)
    {
        *(file) << "(*";
    }

    // If this is like (*this).x and we cannot emit it, ignore lhs
    bool lhs_is_derref_this = (state._do_not_emit_this
            && lhs.is<Nodecl::Dereference>()
            && lhs.as<Nodecl::Dereference>().get_rhs().get_symbol().is_valid()
            && lhs.as<Nodecl::Dereference>().get_rhs().get_symbol().get_name() == "this");

    if (lhs_is_derref_this)
    {
        // do nothing
    }
    else
    {
        bool needs_parentheses = operand_has_lower_priority(node, lhs);
        if (needs_parentheses)
        {
            *(file) << "(";
        }
        // Left hand side does not care about the top level reference status
        state.do_not_derref_rebindable_reference = false;
        walk(lhs);
        if (needs_parentheses)
        {
            *(file) << ")";
        }
    }

    if (!is_anonymous_union_accessor)
    {
        // Right part can be a reference but we do not want to derref it
        state.do_not_derref_rebindable_reference = true;

        if (member_literal.is_null()
                && lhs.is<Nodecl::ClassMemberAccess>()
                && lhs.as<Nodecl::ClassMemberAccess>().get_member().is<Nodecl::Symbol>()
                && lhs.as<Nodecl::ClassMemberAccess>().get_member().get_symbol().is_class())
        {
            *(file) << "::";
        }
        else if (lhs_is_derref_this)
        {
            // skip any separator
        }
        else
        {
            *(file) << ".";
        }

        bool needs_parentheses = operand_has_lower_priority(node, rhs);
        if (needs_parentheses)
        {
            *(file) << "(";
        }

        if (!member_literal.is_null())
        {
            // Use the literal member-name
            walk(member_literal);
        }
        else
        {
            ERROR_CONDITION(!rhs_sym.is_valid(), "Invalid symbol", 0);
            // Simply print the name

            if (rhs_sym.is_class())
            {
                *(file) << rhs_sym.get_name();
                if (rhs_sym.get_type().is_template_specialized_type())
                {
                    *(file) << get_template_arguments_str(rhs_sym.get_internal_symbol(), rhs_sym.get_scope().get_decl_context());
                }
            }
            else if (rhs_sym.is_variable())
            {
                *(file) << rhs_sym.get_name();
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }

        if (member_literal.is_null()
                && node.get_type().is_unresolved_overload())
        {
            TL::TemplateParameters template_arguments =
                node.get_type().unresolved_overloaded_type_get_explicit_template_arguments();

            if (template_arguments.is_valid())
            {
                *(file) << ::template_arguments_to_str(
                        template_arguments.get_internal_template_parameter_list(),
                        /* first_template_argument_to_be_printed */ 0,
                        /* print_first_level_bracket */ 1,
                        this->get_current_scope().get_decl_context());
            }
        }

        if (needs_parentheses)
        {
            *(file) << ")";
        }

        state.do_not_derref_rebindable_reference = old_do_not_derref_rebindable_ref;
    }

    if (must_derref_all)
    {
        *(file) << ")";
    }
}

void CxxBase::visit(const Nodecl::Comma & node)
{
    emit_line_marker(node);
    *(file) << "(";

    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();
    if (state.in_condition && state.condition_top == node)
    {
        *(file) << "(";
    }
    char needs_parentheses = operand_has_lower_priority(node, lhs) || same_operation(node, lhs);
    if (needs_parentheses)
    {
        *(file) << "(";
    }
    walk(lhs);
    if (needs_parentheses)
    {
        *(file) << ")";
    }
    *(file) << ", ";
    needs_parentheses = operand_has_lower_priority(node, rhs);
    if (needs_parentheses)
    {
        *(file) << "(";
    }
    walk(rhs);
    if (needs_parentheses)
    {
        *(file) << ")";
    }
    if (state.in_condition && state.condition_top == node)
    {
        *(file) << ")";
    }

    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::ComplexLiteral& node)
{
    const_value_t* cval = node.get_constant();

    const_value_t* real_part = const_value_complex_get_real_part(cval);
    const_value_t* imag_part = const_value_complex_get_imag_part(cval);

    ERROR_CONDITION(!const_value_is_zero(real_part), "Invalid complex constant in C with nonzero real component", 0);

    if (const_value_is_integer(imag_part))
    {
        emit_integer_constant(imag_part, node.get_type().no_ref().complex_get_base_type());
    }
    else if (const_value_is_floating(imag_part))
    {
        emit_floating_constant(imag_part, node.get_type().no_ref().complex_get_base_type());
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    *(file) << "i";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CompoundExpression& node)
{
    emit_line_marker(node);
    *(file) << " (";

    Nodecl::Context context = node.get_nest().as<Nodecl::Context>();
    Nodecl::List statements = context.get_in_context().as<Nodecl::List>();

    this->push_scope(context.retrieve_context());

    *(file) << "{\n";
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
    *(file) << "}";

    this->pop_scope();

    *(file) << ") ";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CompoundStatement& node)
{
    emit_line_marker(node);
    indent();
    *(file) << "{\n";
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
    *(file) << "}\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::ConditionalExpression& node)
{
    emit_line_marker(node);
    Nodecl::NodeclBase cond = node.get_condition();
    Nodecl::NodeclBase then = node.get_true();
    Nodecl::NodeclBase _else = node.get_false();

    bool condition_must_be_parenthesized = get_rank(cond) < get_rank_kind(NODECL_LOGICAL_OR, "");
    if (condition_must_be_parenthesized)
    {
        // This expression is a logical-or-expression, so an assignment (or comma)
        // needs parentheses
        *(file) << "(";
    }
    walk(cond);
    if (condition_must_be_parenthesized)
    {
        *(file) << ")";
    }

    *(file) << " ? ";

    // This is a top level expression, no parentheses should be required
    walk(then);

    *(file) << " : ";

    node_t priority_node = NODECL_CONDITIONAL_EXPRESSION;
    CXX_LANGUAGE()
    {
        // C++ is more liberal in the syntax of the conditional operator than C99
        priority_node = NODECL_ASSIGNMENT;
    }

    bool else_part_must_be_parenthesized = get_rank(_else) < get_rank_kind(priority_node, "");
    if (else_part_must_be_parenthesized)
    {
        *(file) << "(";
    }
    walk(_else);
    if (else_part_must_be_parenthesized)
    {
        *(file) << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::Context& node)
{
    this->push_scope(node.retrieve_context());

    ERROR_CONDITION(!node.get_in_context().is<Nodecl::List>(), "invalid node", 0);
    Nodecl::List l = node.get_in_context().as<Nodecl::List>();

    // Transient kludge
    bool emit_decls =
        (l.size() != 1
         && !(l[0].is<Nodecl::CompoundStatement>()
             || l[0].is<Nodecl::ForStatement>()
             || l[0].is<Nodecl::WhileStatement>()
             || l[0].is<Nodecl::SwitchStatement>()
             || l[0].is<Nodecl::IfElseStatement>()));

    if (emit_decls)
    {
        indent();
        *file << "/* << fake context >> { */\n";
    }

    if (emit_decls
            || (
                /* Sometimes we need to emit declarations when the condition/loop
                   header defines a type

                   if (((union { int x; float y; }){.x = a}).y > 3.4f)
                   {
                   }

                   This will create a mcc_union_anon_X, which is in the context that surrounds
                   the if statement.

                   Of course, we do not want to do this for
                   compound-statements, because them already define local
                   entities.
                 */
                l.size() == 1
                && !l[0].is<Nodecl::CompoundStatement>()))
    {
        define_local_entities_in_trees(l);
    }

    walk(l);

    if (emit_decls)
    {
        indent();
        *file << "/* } << fake context >> */\n";
    }

    this->pop_scope();
}

CxxBase::Ret CxxBase::visit(const Nodecl::ContinueStatement& node)
{
    emit_line_marker(node);
    indent();
    *(file) << "continue;\n";
}

void CxxBase::emit_explicit_cast(Nodecl::NodeclBase node, Nodecl::NodeclBase nest)
{
    std::string cast_kind = node.get_text();
    TL::Type t = fix_references(node.get_type());

    emit_line_marker(node);
    if (cast_kind == "C")
    {
        bool is_non_ref = is_non_language_reference_type(node.get_type());
        if (is_non_ref)
        {
            if (node.get_type().no_ref().is_array())
            {
                // Special case for arrays, themselves are an address
                *(file) << "(";
            }
            else
            {
                *(file) << "(*";
            }

            // This avoids a warning in some compilers which complain on (T* const)e
            t = t.get_unqualified_type();
        }
        *(file) << "(" << this->get_declaration(t, this->get_current_scope(),  "") << ")";

        if (is_non_ref)
        {
            *(file) << "&(";
        }

        char needs_parentheses = operand_has_lower_priority(node, nest);
        if (needs_parentheses)
        {
            *(file) << "(";
        }
        walk(nest);
        if (needs_parentheses)
        {
            *(file) << ")";
        }

        if (is_non_ref)
        {
            *(file) << "))";
        }
    }
    else if (cast_kind == "static_cast"
            || cast_kind == "dynamic_cast"
            || cast_kind == "reinterpret_cast"
            || cast_kind == "const_cast")
    {
        std::string decl = this->get_declaration(t, this->get_current_scope(),  "");
        if (!decl.empty())
        {
            if (decl[0] == ':')
            {
                decl = " " + decl;
            }
            if (decl[decl.length() - 1] == '>')
            {
                decl += " ";
            }
        }

        *(file) << cast_kind << "<" << decl << ">(";
        walk(nest);
        *(file) << ")";
    }
    else
    {
        internal_error("Code unreachable '%s'", cast_kind.c_str());
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxCast& node)
{
    emit_explicit_cast(node, node.get_rhs());
}

CxxBase::Ret CxxBase::visit(const Nodecl::Conversion& node)
{
    if (node.get_text() == "")
    {
        // Implicit conversion
        walk(node.get_nest());
    }
    else
    {
        emit_explicit_cast(node, node.get_nest());
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxArrow& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_member();

    char needs_parentheses = operand_has_lower_priority(node, lhs);
    if (needs_parentheses)
    {
        *(file) << "(";
    }
    walk(lhs);

    if (needs_parentheses)
    {
        *(file) << ")";
    }

    *(file) << "->"
         << /* template tag if needed */ node.get_text();


    needs_parentheses = operand_has_lower_priority(node, rhs);
    if (needs_parentheses)
    {
        *(file) << "(";
    }
    walk(rhs);

    if (needs_parentheses)
    {
        *(file) << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxClassMemberAccess& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_member();
    //
    // If this is like (*this).x and we cannot emit it, ignore lhs
    bool lhs_is_derref_this = (state._do_not_emit_this
            && lhs.is<Nodecl::Dereference>()
            && lhs.as<Nodecl::Dereference>().get_rhs().get_symbol().is_valid()
            && lhs.as<Nodecl::Dereference>().get_rhs().get_symbol().get_name() == "this");

    if (lhs_is_derref_this)
    {
        // Do nothing
    }
    else
    {

        bool needs_parentheses = operand_has_lower_priority(node, lhs);
        if (needs_parentheses)
        {
            *(file) << "(";
        }
        walk(lhs);

        if (needs_parentheses)
        {
            *(file) << ")";
        }

        *(file) << "."
            << /* template tag if needed */ node.get_text();
    }

    bool needs_parentheses = operand_has_lower_priority(node, rhs);
    if (needs_parentheses)
    {
        *(file) << "(";
    }
    walk(rhs);

    if (needs_parentheses)
    {
        *(file) << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxBracedInitializer& node)
{
    *(file) << "{ ";
    if (!node.get_init().is_null())
    {
        walk_list(node.get_init().as<Nodecl::List>(), ", ");
    }
    *(file) << " }";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepGlobalNameNested& node)
{
    *(file) << "::";
    visit(node.as<Nodecl::CxxDepNameNested>());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepNameConversion& node)
{
    *(file)
        << "operator "
        << this->get_declaration(node.get_conversion_type().get_type(), this->get_current_scope(), "");
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepNameNested& node)
{
    walk_list(node.get_items().as<Nodecl::List>(), "::");
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepNameSimple& node)
{
    *(file) << node.get_text();
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepTemplateId& node)
{
    *(file) << node.get_text();

    walk(node.get_name());
    TL::TemplateParameters tpl = node.get_template_parameters();

    if (tpl.is_valid()
            && tpl.get_num_parameters() > 0)
    {
        *(file) << ::template_arguments_to_str(
                tpl.get_internal_template_parameter_list(),
                /* first_template_argument_to_be_printed */ 0,
                /* print_first_level_bracket */ 1,
                this->get_current_scope().get_decl_context());
    }
    else
    {
        *(file) << "<>";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepDecltype& node)
{
    (*file) <<
        print_type_str(
                node.get_type().get_internal_type(),
                node.retrieve_context().get_decl_context(),
                (void*) this);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxInitializer& node)
{
    walk(node.get_init());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxEqualInitializer& node)
{
    *(file) << " = ";
    walk(node.get_init());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxMemberInit& node)
{
    walk(node.get_name());
    walk(node.get_initializer());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxExplicitTypeCast& node)
{
    TL::Type type = node.get_type();

    bool add_double_parentheses = false;

    if (type.is_signed_short_int())
    {
        *(file) << "short";
    }
    else if (type.is_signed_long_int())
    {
        *(file) << "long";
    }
    else if (type.is_unsigned_int())
    {
        *(file) << "unsigned";
    }
    else
    {
        if (type.is_named()
                && ::symbol_is_member_of_dependent_class(type.get_symbol().get_internal_symbol()))
        {
            // It will be started by a 'typename' so wrap it inside a
            // parentheses for 'syntactic' security
            *(file) << "((";
            add_double_parentheses = true;
        }

        *(file) << this->get_declaration(type, this->get_current_scope(),  "");
    }

    walk(node.get_init_list());

    if (add_double_parentheses)
        *(file) << "))";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxParenthesizedInitializer& node)
{
    *(file) << "(";
    if (!node.get_init().is_null())
    {
        walk_expression_list(node.get_init().as<Nodecl::List>());
    }
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxAlignof& node)
{
    if (IS_CXX11_LANGUAGE)
    {
        *(file) << "alignof(";
    }
    else
    {
        *(file) << "__alignof__(";
    }
    walk(node.get_expr());
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxSizeof& node)
{
    *(file) << "sizeof(";
    walk(node.get_expr());
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxSizeofPack& node)
{
    *(file) << "sizeof...(";
    walk(node.get_expr());
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::DefaultArgument& node)
{
    internal_error("Code unreachable", 0);
}

CxxBase::Ret CxxBase::visit(const Nodecl::DefaultStatement& node)
{
    Nodecl::NodeclBase statement = node.get_statement();

    emit_line_marker(node);
    indent();
    *(file) << "default :\n";

    walk(statement);
}

CxxBase::Ret CxxBase::visit(const Nodecl::DoStatement& node)
{
    Nodecl::NodeclBase statement = node.get_statement();
    Nodecl::NodeclBase condition = node.get_condition();

    emit_line_marker(node);
    indent();
    *(file) << "do\n";

    inc_indent();
    walk(statement);
    dec_indent();

    indent();
    *(file) << "while (";
    walk(condition);
    *(file) << ");\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::EmptyStatement& node)
{
    emit_line_marker(node);
    indent();
    *(file) << ";\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::ErrExpr& node)
{
    if (!this->is_file_output())
    {
        *(file) << "<<error expression>>";
    }
    else
    {
        internal_error("%s: error: <<error expression>> found when the output is a file",
                node.get_locus_str().c_str());
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::ErrStatement& node)
{
    if (!this->is_file_output())
    {
        *(file) << "<<error statement>>";
    }
    else
    {
        internal_error("%s: error: <<error statement>> found when the output is a file",
                node.get_locus_str().c_str());
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::ExpressionStatement& node)
{
    Nodecl::NodeclBase expression = node.get_nest();

    emit_line_marker(node);
    indent();

    bool need_extra_parentheses = false;

    // Parentheses may be needed to avoid these expressions become declarations
    need_extra_parentheses = expression.is<Nodecl::CxxExplicitTypeCast>()
            || (expression.is<Nodecl::FunctionCall>()
                && expression.as<Nodecl::FunctionCall>().get_called().get_symbol().is_valid()
                && expression.as<Nodecl::FunctionCall>().get_called().get_symbol().is_constructor()
                && expression.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>().size() == 1);

    if (need_extra_parentheses)
    {
        (*file) << "( ";
    }

    walk(expression);

    if (need_extra_parentheses)
    {
        (*file) << " )";
    }

    *(file) << ";\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::FieldDesignator& node)
{
    Nodecl::NodeclBase field = node.get_field();
    Nodecl::NodeclBase next = node.get_next();

    if (!(field.get_symbol().get_type().is_named_class()
                && field.get_symbol().get_type().get_symbol().is_anonymous_union()))
    {
        if (IS_CXX_LANGUAGE)
        {
            *(file) << start_inline_comment();
        }

        *(file) << ".";
        walk(field);

        if (!next.is<Nodecl::FieldDesignator>()
                && !next.is<Nodecl::IndexDesignator>())
        {
            *(file) << " = ";
        }

        if (IS_CXX_LANGUAGE)
        {
            *(file) << end_inline_comment();
        }
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
        *(file) << floating;
    }
    else if (const_value_is_double(cval))
    {
        const char* floating = NULL;
        uniquestr_sprintf(&floating, "%.*e", precision, const_value_cast_to_double(cval));
        *(file) << floating;
    }
    else if (const_value_is_long_double(cval))
    {
        const char* floating = NULL;
        uniquestr_sprintf(&floating, "%.*LeL", precision, const_value_cast_to_long_double(cval));
        *(file) << floating;
    }
#ifdef HAVE_QUADMATH_H
    else if (const_value_is_float128(cval))
    {
        __float128 f128 = const_value_cast_to_float128(cval);
        int n = quadmath_snprintf (NULL, 0, "%.*Qe", precision, f128);
        char *c = new char[n + 1];
        quadmath_snprintf (c, n, "%.*Qe", precision, f128);
        c[n] = '\0';
        *(file) << c << "Q";
        delete[] c;
    }
#endif
}

CxxBase::Ret CxxBase::visit(const Nodecl::FloatingLiteral& node)
{
    const_value_t* value = nodecl_get_constant(node.get_internal_nodecl());
    ERROR_CONDITION(value == NULL, "Invalid value", 0);

    emit_floating_constant(value, node.get_type().no_ref());
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxForRanged& node)
{
    emit_line_marker(node);
    indent();


    *file << "for (";

    bool old_in_condition = state.in_condition;
    state.in_condition = 1;
    define_or_declare_variable(node.get_symbol(), /* is_definition */ 1);
    state.in_condition = old_in_condition;

    *file << " : ";

    Nodecl::CxxEqualInitializer eq_init = node.get_range().as<Nodecl::CxxEqualInitializer>();
    ERROR_CONDITION(!eq_init.is<Nodecl::CxxEqualInitializer>(), "Invalid node", 0);
    walk(eq_init.get_init());

    *file << ")\n";

    inc_indent();
    walk(node.get_statement());
    dec_indent();
}

CxxBase::Ret CxxBase::visit(const Nodecl::ForStatement& node)
{
    Nodecl::NodeclBase loop_control = node.get_loop_header();
    Nodecl::NodeclBase statement = node.get_statement();

    if (loop_control.is<Nodecl::LoopControl>())
    {
        emit_line_marker(node);
        indent();
        *(file) << "for (";
        walk(loop_control);
        *(file) << ")\n";

        inc_indent();
        walk(statement);
        dec_indent();
    }
    else if (loop_control.is<Nodecl::UnboundedLoopControl>())
    {
        // This only happens for DO without loop-control
        emit_line_marker(node);
        indent();
        *(file) << "for (;;)\n";
        inc_indent();
        walk(statement);
        dec_indent();
    }
    // C++2011
    else if (loop_control.is<Nodecl::IteratorLoopControl>())
    {
        indent();
        *(file) << "for (";
        walk(loop_control);
        *(file) << ")\n";

        inc_indent();
        walk(statement);
        dec_indent();
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
                *(file) << start_inline_comment();
                default_arguments = true;
            }

            actual_arg = actual_arg.as<Nodecl::DefaultArgument>().get_argument();
        }

        if (arg_it != begin)
        {
            *(file) << ", ";
        }

        bool old_do_not_derref_rebindable_ref = state.do_not_derref_rebindable_reference;
        state.do_not_derref_rebindable_reference = false;

        bool do_reference = false;

        if (type_it != type_end
                && type_it->is_valid())
        {
            actual_arg = no_conv(actual_arg);

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
            *(file) << "&(";
        }
        walk(actual_arg);
        if (do_reference)
        {
            *(file) << ")";
        }

        if (type_it != type_end)
            type_it++;

        arg_it++;

        state.do_not_derref_rebindable_reference = old_do_not_derref_rebindable_ref;
    }

    // Close the comment if needed
    if (default_arguments)
        *(file) << end_inline_comment();
}

/* Adapters used in visit_function_call */
template <typename Node> Nodecl::NodeclBase get_alternate_name(const Node&)
{
    return Nodecl::NodeclBase::null();
}
template <> Nodecl::NodeclBase get_alternate_name(const Nodecl::FunctionCall& n)
{
    return n.get_alternate_name();
}

template <typename Node>
void CxxBase::visit_function_call_form_template_id(const Node& node)
{
    Nodecl::NodeclBase function_form = node.get_function_form();
    TL::Symbol called_symbol = node.get_called().get_symbol();

    Nodecl::NodeclBase alternate_name = get_alternate_name(node);
    if (!alternate_name.is_null()
            && (alternate_name.is<Nodecl::CxxDepNameNested>()
                || alternate_name.is<Nodecl::CxxDepGlobalNameNested>()))
        return;

    if (!function_form.is_null()
            && function_form.is<Nodecl::CxxFunctionFormTemplateId>()
            && called_symbol.get_type().is_template_specialized_type())
    {
        TL::TemplateParameters template_args = function_form.get_template_parameters();
        TL::TemplateParameters deduced_template_args =
            called_symbol.get_type().template_specialized_type_get_template_arguments();

        if (template_args.get_num_parameters() == deduced_template_args.get_num_parameters())
        {
            // First case: the user's code specifies all template arguments
            *(file) << ::template_arguments_to_str(
                    template_args.get_internal_template_parameter_list(),
                    /* first_template_argument_to_be_printed */ 0,
                    /* print_first_level_bracket */ 1,
                    node.retrieve_context().get_decl_context());
        }
        else
        {
            // Second case: the user's code specifies some template arguments but not all
            std::string template_args_str =
                ::template_arguments_to_str(
                        template_args.get_internal_template_parameter_list(),
                        /* first_template_argument_to_be_printed */ 0,
                        /* print_first_level_bracket */ 0,
                        node.retrieve_context().get_decl_context());

            std::string deduced_template_args_str =
                ::template_arguments_to_str(
                        deduced_template_args.get_internal_template_parameter_list(),
                        /* first_template_argument_to_be_printed */ template_args.get_num_parameters(),
                        /* print_first_level_bracket */ 0,
                        called_symbol.get_scope().get_decl_context());

            // Reason of this: A<::B> it's not legal
            if (template_args_str.length() != 0 && template_args_str[0] == ':')
            {
                *(file) << "< ";
            }
            else
            {
                *(file) << "<";
            }

            *(file) << template_args_str;

            if (deduced_template_args_str != "")
            {
                *(file) << start_inline_comment();
                *(file) << ", " << deduced_template_args_str << " ";
                *(file) << end_inline_comment();
            }

            *(file) << ">";
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
            *(file) << start_inline_comment();
            *(file) << ::template_arguments_to_str(
                    deduced_template_args.get_internal_template_parameter_list(),
                    /* first_template_argument_to_be_printed */ 0,
                    /* print_first_level_bracket */ 1,
                    node.retrieve_context().get_decl_context());
            *(file) << end_inline_comment();
        }
    }
}

// Explicit specialitzation for Nodecl::CxxDepFunctionCall because this kind of node has not a function form
template <>
void CxxBase::visit_function_call_form_template_id<Nodecl::CxxDepFunctionCall>(const Nodecl::CxxDepFunctionCall& node)
{
}

Nodecl::NodeclBase CxxBase::advance_implicit_function_calls(Nodecl::NodeclBase node)
{
    while (node.is<Nodecl::FunctionCall>()
            && is_implicit_function_call(node.as<Nodecl::FunctionCall>())
            && !node.as<Nodecl::FunctionCall>().get_arguments().is_null())
    {
        node = node.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>()[0];
    }

    return node;
}

template <typename Node>
bool CxxBase::is_implicit_function_call(const Node& node)
{
    return (!node.get_function_form().is_null()
            && node.get_function_form().template is<Nodecl::CxxFunctionFormImplicit>());
}

// Explicit specialitzation for Nodecl::CxxDepFunctionCall because this kind of node has not a function form
template <>
bool CxxBase::is_implicit_function_call<Nodecl::CxxDepFunctionCall>(const Nodecl::CxxDepFunctionCall& node)
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

bool CxxBase::is_assignment_operator(const std::string& operator_name)
{
    return (operator_name == "=")
        || (operator_name == "*=")
        || (operator_name == "/=")
        || (operator_name == "%=")
        || (operator_name == "+=")
        || (operator_name == "-=")
        || (operator_name == "<<=")
        || (operator_name == ">>=")
        || (operator_name == "&=")
        || (operator_name == "|=")
        || (operator_name == "^=");
}

template <typename Node>
CxxBase::Ret CxxBase::visit_function_call(const Node& node, bool is_virtual_call)
{
    Nodecl::NodeclBase called_entity = node.get_called();

    if (is_implicit_function_call(node))
    {
        // We don't want to generate the current function call because It has
        // been added by the compiler
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

        // Non-koenig dependent calls: emit parentheses to avoid them looking as koenig
        needs_parentheses = needs_parentheses
            || (node.template is <Nodecl::CxxDepFunctionCall>()
                    && called_entity.is<Nodecl::CxxDepNameSimple>()
                    && called_entity.get_type().is_valid());

        if (needs_parentheses)
        {
            *(file) << "(";
        }
        walk(called_entity);
        if (needs_parentheses)
        {
            *(file) << ")";
        }
        *(file) << "(";
        walk_expression_list(arguments);
        *(file) << ")";
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

        if (!is_virtual_call
                && (kind == ORDINARY_CALL
                    || kind == STATIC_MEMBER_CALL
                    || kind == NONSTATIC_MEMBER_CALL))
        {
            // Use the alternate name
            Nodecl::NodeclBase alternate_name = get_alternate_name(node);
            if (!alternate_name.is_null())
            {
                called_entity = alternate_name;
                called_symbol = alternate_name.get_symbol();
            }
        }
    }

    bool is_non_language_ref = is_non_language_reference_type(function_type.returns());
    if (is_non_language_ref)
        *(file) << "(*(";


    int ignore_n_first_arguments;
    switch (kind)
    {
        case ORDINARY_CALL:
        case STATIC_MEMBER_CALL:
            {
                bool needs_parentheses = operand_has_lower_priority(node, called_entity);

                if (needs_parentheses)
                    *(file) << "(";

                // We are going to visit the called entity of the current function call.
                // The template arguments of this function (if any) will be printed by the
                // function 'visit_function_call_form_template_id' and not by the visitor
                // of the symbol
                bool old_visiting_called_entity_of_function_call =
                    state.visiting_called_entity_of_function_call;
                state.visiting_called_entity_of_function_call = called_entity.is<Nodecl::Symbol>();
                walk(called_entity);
                state.visiting_called_entity_of_function_call = old_visiting_called_entity_of_function_call;

                if (needs_parentheses)
                    *(file) << ")";

                ignore_n_first_arguments = 0;
                break;
            }
        case NONSTATIC_MEMBER_CALL:
            {
                ERROR_CONDITION(!(arguments.size() >= 1), "A nonstatic member call lacks the implicit argument", 0);

                bool needs_parentheses = (get_rank(arguments[0]) < get_rank_kind(NODECL_CLASS_MEMBER_ACCESS, ""));
                if (needs_parentheses)
                    *(file) << "(";

                walk(arguments[0]);

                if (needs_parentheses)
                    *(file) << ")";

                *(file) << ".";

                if (is_virtual_call)
                {
                    ERROR_CONDITION(!called_symbol.is_valid(), "Virtual call lacks called symbol", 0);
                    *(file) << unmangle_symbol_name(called_symbol);
                }
                else
                {
                    bool old_visiting_called_entity_of_function_call =
                        state.visiting_called_entity_of_function_call;
                    state.visiting_called_entity_of_function_call = true;
                    walk(called_entity);
                    state.visiting_called_entity_of_function_call = old_visiting_called_entity_of_function_call;
                }

                ignore_n_first_arguments = 1;
                break;
            }
        case CONSTRUCTOR_INITIALIZATION:
            {
                TL::Symbol class_symbol = called_symbol.get_class_type().get_symbol();
                *(file) << this->get_qualified_name(class_symbol, this->get_current_scope());
                ignore_n_first_arguments = 0;
                break;
            }
        case UNARY_PREFIX_OPERATOR:
            {
                std::string called_operator = called_symbol.get_name().substr(std::string("operator ").size());

                // We need this to avoid - - 1 to become --1
                *(file) << " " << called_operator;

                bool needs_parentheses = operand_has_lower_priority(node, arguments[0]);
                if (needs_parentheses)
                    *(file) << "(";
                walk(arguments[0]);
                if (needs_parentheses)
                    *(file) << ")";

                if (is_non_language_ref)
                    *(file) << "))";

                return;
            }
        case UNARY_POSTFIX_OPERATOR:
            {
                std::string called_operator = called_symbol.get_name().substr(std::string("operator ").size());

                bool needs_parentheses = operand_has_lower_priority(node, arguments[0]);
                if (needs_parentheses)
                    *(file) << "(";

                walk(arguments[0]);

                if (needs_parentheses)
                    *(file) << ")";

                *(file) << called_operator;

                if (is_non_language_ref)
                    *(file) << "))";

                return;
            }
        case BINARY_INFIX_OPERATOR:
            {
                std::string called_operator = called_symbol.get_name().substr(std::string("operator ").size());

                bool assignment_operator = is_assignment_operator(called_operator);

                bool needs_parentheses = operand_has_lower_priority(node, arguments[0]);
                if (assignment_operator)
                    needs_parentheses = needs_parentheses || same_operation(node, arguments[0]);

                if (needs_parentheses)
                    *(file) << "(";
                walk(arguments[0]);
                if (needs_parentheses)
                    *(file) << ")";

                *(file) << " " << called_operator << " ";

                needs_parentheses = operand_has_lower_priority(node, arguments[1]);
                if (!assignment_operator)
                    needs_parentheses = needs_parentheses
                        || same_operation(node, arguments[1]);

                if (needs_parentheses)
                    *(file) << "(";
                walk(arguments[1]);
                if (needs_parentheses)
                    *(file) << ")";

                if (is_non_language_ref)
                    *(file) << "))";

                return;
            }
        default:
            {
                internal_error("Unhandled function call kind", 0);
            }
    }

    visit_function_call_form_template_id(node);

    *(file) << "(";

    codegen_function_call_arguments(arguments.begin(), arguments.end(), function_type, ignore_n_first_arguments);

    *(file) << ")";

    if (is_non_language_ref)
        *(file) << "))";
}

CxxBase::Ret CxxBase::visit(const Nodecl::FunctionCall& node)
{
    visit_function_call(node, /* is_virtual_call */ false);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepFunctionCall& node)
{
    visit_function_call(node, /* is_virtual_call */ false);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxValuePack& node)
{
    walk(node.get_pack());
    (*file) << " ...";
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

    state.friend_function_declared_but_not_defined.erase(symbol);

    ERROR_CONDITION(!symbol.is_function()
            && !symbol.is_dependent_friend_function(), "Invalid symbol", 0);

    if (!symbol.get_class_type().is_valid()
            || !is_friend_of_class(symbol, symbol.get_class_type().get_symbol()))
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
    fill_parameter_names_and_parameter_attributes(symbol, parameter_names, parameter_attributes, true);

    std::string decl_spec_seq;

    if (symbol.get_class_type().is_valid()
            // The function is friend of a class
            && is_friend_of_class(symbol, symbol.get_class_type().get_symbol())
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
        decl_spec_seq += "inline ";
    }

    if (symbol.is_virtual() && symbol.is_defined_inside_class())
    {
        decl_spec_seq += "virtual ";
    }

    if (symbol.is_constexpr())
    {
        decl_spec_seq += "constexpr ";
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

    std::string trailing_type_specifier;
    if (symbol.get_type().is_trailing_return())
    {
        trailing_type_specifier = " -> ";
        trailing_type_specifier += print_type_str(symbol.get_type().returns().get_internal_type(),
                function_scope.get_decl_context(),
                (void*) this);
    }

    TL::Type real_type;
    if (symbol.is_conversion_function()
            || symbol.is_destructor())
    {
        // FIXME - Use TL::Type to build this type
        real_type = ::get_new_function_type(NULL, NULL, 0, REF_QUALIFIER_NONE);
        if (symbol.is_conversion_function())
        {
            real_type = real_type.get_as_qualified_as(symbol.get_type());
        }
    }
    else
    {
        // Transform 'f() -> T' into 'auto f()' and then we will add '-> T'
        real_type = coerce_parameter_types_of_function_type(symbol);
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
                /*show default variables*/ IS_CXX11_LANGUAGE);
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
        *(file) << "extern " + symbol.get_linkage() + "\n";
        indent();
        *(file) << "{\n";

        inc_indent();
    }

    if (!symbol.is_member()
            && asm_specification != "")
    {
        // gcc does not like asm specifications appear in the
        // function-definition so emit a declaration before the definition
        indent();
        *(file) << gcc_extension << decl_spec_seq << gcc_attributes << declarator << exception_spec
            << asm_specification << trailing_type_specifier << ";\n";
    }

    indent();
    *(file) << gcc_extension << decl_spec_seq << gcc_attributes << declarator << exception_spec << trailing_type_specifier << "\n";

    set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);

    if (!initializers.is_null())
    {
        push_scope(function_scope);
        inc_indent();

        indent();
        *(file) << ": ";

        walk_list(initializers.as<Nodecl::List>(), ", ");

        dec_indent();

        *(file) << "\n";
        pop_scope();
    }

    this->walk(context);

    if (requires_extern_linkage)
    {
        dec_indent();
        indent();
        *(file) << "}\n";
    }
}

TL::Type CxxBase::coerce_parameter_types_of_function_type(TL::Symbol sym)
{
    // This function returns a new function type built using the types of the
    // parameters This is needed only in function definitions where
    // cv-qualifiers or types may be different in the declaration (which is
    // what sym.get_type() returns) and the definition.

    // Always advance over typedefs
    TL::Type function_type = sym.get_type().advance_over_typedefs();

    // Ignore unprototyped functions
    if (function_type.lacks_prototype())
        return function_type;

    bool has_ellipsis = false;
    TL::ObjectList<TL::Type> parameter_types = function_type.parameters(has_ellipsis);
    TL::ObjectList<TL::Symbol> parameter_symbols = sym.get_related_symbols();

    TL::ObjectList<TL::Type>::iterator it_type = parameter_types.begin();
    TL::ObjectList<TL::Symbol>::iterator it_symbol = parameter_symbols.begin();
    for (;
            it_symbol != parameter_symbols.end() && it_type != parameter_types.end();
            it_symbol++, it_type++)
    {
        // In C++ there may not be a symbol for a given parameter
        if (!it_symbol->is_valid())
            continue;

        *it_type = it_symbol->get_type();
    }

    // Now rebuild the type
    TL::Type result_type;
    if (function_type.is_trailing_return())
    {
        result_type = TL::Type::get_auto_type().get_function_returning(parameter_types, has_ellipsis,
                function_type.get_reference_qualifier());
    }
    else
    {
        result_type = function_type.returns().get_function_returning(parameter_types, has_ellipsis,
                function_type.get_reference_qualifier());
    }

    result_type = result_type.get_as_qualified_as(function_type);

    return result_type;
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

    state.friend_function_declared_but_not_defined.erase(symbol);

    ERROR_CONDITION(!symbol.is_function()
            && !symbol.is_dependent_friend_function(), "Invalid symbol", 0);

    bool is_template_specialized = symbol_type.is_template_specialized_type();

    bool is_primary = false;
    if (!symbol.get_class_type().is_valid()
            || !is_friend_of_class(symbol, symbol.get_class_type().get_symbol()))
    {
        if (!symbol.is_member()
                && is_template_specialized)
        {
            TL::Type template_type = symbol_type.get_related_template_type();
            TL::Type primary_type = template_type.get_primary_template();
            TL::Symbol primary_symbol = primary_type.get_symbol();

            is_primary = (primary_symbol == symbol);
        }
    }

    state.current_symbol = symbol;

    // At this point, we mark the function as defined. It must be done here to
    // avoid the useless declaration of the function being defined and other
    // related problems.
    codegen_status_t old_status = get_codegen_status(symbol);
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

    move_to_namespace_of_symbol(symbol);

    // We may need zero or more empty template headers
    bool emit_default_arguments = true;
    TL::TemplateParameters tpl = symbol_scope.get_template_parameters();
    while (tpl.is_valid())
    {
        if (!tpl.get_is_explicit_instantiation()
                && tpl.get_is_explicit_specialization())
        {
             indent();
             *(file) << "template <>\n";
        }
        tpl = tpl.get_enclosing_parameters();
        emit_default_arguments = false;
    }

    int num_parameters = symbol.get_related_symbols().size();
    TL::ObjectList<std::string> parameter_names(num_parameters);
    TL::ObjectList<std::string> parameter_attributes(num_parameters);
    fill_parameter_names_and_parameter_attributes(symbol, parameter_names, parameter_attributes, emit_default_arguments);

    std::string decl_spec_seq;

    if (symbol.get_class_type().is_valid()
            // The function is friend of a class
            && is_friend_of_class(symbol, symbol.get_class_type().get_symbol())
            && symbol.is_defined_inside_class()
            // The friend function is defined within this class
            && !state.classes_being_defined.empty()
            && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())
        // The friend function is defined in the current being defined class
     {
         decl_spec_seq += "friend ";
     }

    if (symbol.is_static()
            // Specializations other than the primary cannot have storage specifier
            && !(is_template_specialized
                && !is_primary)
            && (!symbol.is_member()
                || (!state.classes_being_defined.empty()
                    && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())))
    {
        decl_spec_seq += "static ";
    }

    if (symbol.is_extern()
            && symbol.get_value().is_null()
            // Specializations other than the primary cannot have storage specifier
            && !(is_template_specialized
                && !is_primary))
    {
        decl_spec_seq += "extern ";
    }
    if (symbol.is_virtual() && symbol.is_defined_inside_class())
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

    if (symbol.is_constexpr())
    {
        decl_spec_seq += "constexpr ";
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

    std::string trailing_type_specifier;
    if (symbol.get_type().is_trailing_return())
    {
        trailing_type_specifier = " -> ";
        trailing_type_specifier += print_type_str(
                symbol.get_type().returns().get_internal_type(),
                symbol_scope.get_decl_context(),
                (void*) this);
    }

    TL::Type real_type;
    if (symbol.is_conversion_function()
            || symbol.is_destructor())
    {
        // FIXME - Use TL::Type to build this type
        real_type = ::get_new_function_type(NULL, NULL, 0, REF_QUALIFIER_NONE);

        if (symbol.is_conversion_function())
        {
            real_type = real_type.get_as_qualified_as(symbol.get_type());
        }
    }
    else
    {
        // Transform 'f() -> T' into 'auto f()' and then we will add '-> T'
        real_type = coerce_parameter_types_of_function_type(symbol);
    }

    std::string declarator = this->get_declaration_with_parameters(
            real_type, symbol_scope, declarator_name, parameter_names, parameter_attributes);

    std::string exception_spec = exception_specifier_to_str(symbol);

    bool requires_extern_linkage = false;
    if (IS_CXX_LANGUAGE
            || cuda_emit_always_extern_linkage())
    {
        requires_extern_linkage = (!symbol.is_member()
                && symbol.has_nondefault_linkage());

        if (requires_extern_linkage)
        {
            *(file) << "extern " + symbol.get_linkage() + "\n";
            indent();
            *(file) << "{\n";

            inc_indent();
        }
    }

    if (!symbol.is_member()
            && asm_specification != ""
            // Only emit this extra declaration if we were not declared earlier at all
            && old_status == CODEGEN_STATUS_NONE)
    {
        // gcc does not like asm specifications appear in the
        // function-definition so emit a declaration before the definition
        indent();
        if (CURRENT_CONFIGURATION->xl_compatibility)
        {
            // IBM XL is very picky regarding attribute location
            *(file) << gcc_extension << decl_spec_seq << declarator
                << exception_spec << " " << gcc_attributes << " " << asm_specification << trailing_type_specifier << ";\n";
        }
        else
        {
            *(file) << gcc_extension << decl_spec_seq << gcc_attributes << declarator
                << exception_spec << asm_specification << trailing_type_specifier << ";\n";
        }
    }

    emit_line_marker(node);
    indent();
    *(file) << gcc_extension << decl_spec_seq << gcc_attributes << declarator
        << exception_spec << trailing_type_specifier << "\n";


    if (!initializers.is_null())
    {
        Nodecl::List initializer_list = initializers.as<Nodecl::List>();

        // Count explicit member initializers
        int i = 0;
        for (Nodecl::List::iterator it = initializer_list.begin();
                it != initializer_list.end();
                it++)
        {
            if (it->is<Nodecl::ImplicitMemberInit>())
                continue;
            i++;
        }

        if (i > 0)
        {
            push_scope(symbol_scope);
            inc_indent();

            indent();
            *(file) << ": ";

            i = 0;
            for (Nodecl::List::iterator it = initializer_list.begin();
                    it != initializer_list.end();
                    it++)
            {
                // Skip implicit member initializers
                if (it->is<Nodecl::ImplicitMemberInit>())
                    continue;

                if (i > 0)
                    *(file) << ", ";

                walk(*it);

                i++;
            }

            dec_indent();

            *(file) << "\n";
            pop_scope();
        }
    }

    this->walk(context);

    if (IS_CXX_LANGUAGE
            || cuda_emit_always_extern_linkage())
    {
        if (requires_extern_linkage)
        {
            dec_indent();
            indent();
            *(file) << "}\n";
        }
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::GotoStatement& node)
{
    TL::Symbol label_sym = node.get_symbol();

    emit_line_marker(node);
    indent();
    *(file) << "goto " << label_sym.get_name() << ";\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::IfElseStatement& node)
{
    Nodecl::NodeclBase condition = node.get_condition();
    Nodecl::NodeclBase then = node.get_then();
    Nodecl::NodeclBase _else = node.get_else();

    emit_line_marker(node);
    indent();

    *(file) << "if (";
    int old_condition = state.in_condition;
    int old_indent = get_indent_level();
    Nodecl::NodeclBase old_condition_top = state.condition_top;

    set_indent_level(0);
    state.in_condition = 1;
    state.condition_top = no_conv(condition);

    walk(condition);

    set_indent_level(old_indent);
    state.in_condition = old_condition;
    state.condition_top = old_condition_top;

    *(file) << ")\n";

    inc_indent();
    walk(then);
    dec_indent();

    if (!_else.is_null())
    {
        indent();
        *(file) << "else\n";
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
        *(file) << start_inline_comment();
    }

    *(file) << "[";
    walk(_index);
    *(file) << "]";


    if (!next.is<Nodecl::FieldDesignator>()
            && !next.is<Nodecl::IndexDesignator>())
    {
        *(file) << " = ";
    }
    if (IS_CXX_LANGUAGE)
    {
        *(file) << end_inline_comment();
    }

    walk(next);
}

void CxxBase::visit(const Nodecl::CxxLambda& node)
{
    (*file) << "[";

    Nodecl::List explicit_captures = node.get_explicit_captures().as<Nodecl::List>();

    for (Nodecl::List::iterator it = explicit_captures.begin();
            it != explicit_captures.end();
            it++)
    {
        if (it != explicit_captures.begin())
            (*file) << ", ";

        if (it->get_symbol().get_name() == "this")
        {
            (*file) << "this";
            continue;
        }

        if (it->is<Nodecl::CxxCaptureReference>())
        {
                (*file) << "&";
        }

        (*file) << it->get_symbol().get_name();

        if (it->get_symbol().is_variable_pack())
            (*file) << " ...";
    }

    *file << "]";

    // This is a fake symbol used only to keep things around
    TL::Symbol lambda_symbol = node.get_symbol();
    std::string exception_specifier = this->exception_specifier_to_str(lambda_symbol);

    TL::Type real_type = lambda_symbol.get_type();

    std::string trailing_type_specifier;
    trailing_type_specifier = " -> ";
    trailing_type_specifier += print_type_str(
            real_type.returns().get_internal_type(),
            lambda_symbol.get_scope().get_decl_context(),
            (void*) this);

    // Remove return type because we will just print ( P )
    real_type = function_type_replace_return_type(real_type.get_internal_type(), NULL);

    int num_parameters = lambda_symbol.get_related_symbols().size();
    TL::ObjectList<std::string> parameter_names(num_parameters);
    TL::ObjectList<std::string> parameter_attributes(num_parameters);
    fill_parameter_names_and_parameter_attributes(lambda_symbol,
            parameter_names,
            parameter_attributes,
            /* emit_default_arguments */ true);

    std::string declarator;
    declarator = this->get_declaration_with_parameters(real_type, lambda_symbol.get_scope(),
            "", // No function name
            parameter_names,
            parameter_attributes);

    *file << declarator << " ";

    if (lambda_symbol.is_mutable())
        *file << "mutable ";

    *file << exception_specifier;

    Nodecl::Context context = lambda_symbol.get_function_code().as<Nodecl::Context>();
    Nodecl::List statements = context.get_in_context().as<Nodecl::List>();

    this->push_scope(lambda_symbol.get_related_scope());

    *(file) << "{\n";
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
    *(file) << "}";

    this->pop_scope();
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
        *(file) << *(signed long long*)&v;
    }
    else if (t.is_unsigned_int())
    {
        *(file) << (unsigned long long)v << "U";
    }
    else if (t.is_signed_long_int())
    {
        *(file) << *(signed long long*)&v << "L";
    }
    else if (t.is_unsigned_long_int())
    {
        *(file) << (unsigned long long)v << "LU";
    }
    else if (t.is_signed_long_long_int())
    {
        *(file) << *(signed long long*)&v << "LL";
    }
    else if (t.is_unsigned_long_long_int())
    {
        *(file) << (unsigned long long)v << "LLU";
    }
    else
    {
        // Remaining integers like 'short'
        if (const_value_is_signed(cval))
        {
            *(file) << *(signed long long*)&v;
        }
        else
        {
            *(file) << (unsigned long long)v;
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
            case '\'' : { *(file) << "'\\''"; break; }
            case '\\': { *(file) <<  "'\\\\'"; break; }
            case '\a' : { *(file) << "'\\a'"; break; }
            case '\b' : { *(file) << "'\\b'"; break; }
            case '\e' : { *(file) << "'\\e'"; break; } // GCC extension
            case '\f' : { *(file) << "'\\f'"; break; }
            case '\n' : { *(file) << "'\\n'"; break; }
            case '\r' : { *(file) << "'\\r'"; break; }
            case '\t' : { *(file) << "'\\t'"; break; }
            case '\v' : { *(file) << "'\\v'"; break; }
            case '\"' : { *(file) << "'\\\"'"; break; }
            default: {
                         if (isprint(b))
                         {
                             if (t.is_signed_char())
                             {
                                 *(file) << "'" << (signed char) b << "'";
                             }
                             else
                             {
                                 *(file) << "'" << (unsigned char) b << "'";
                             }
                         }
                         else
                         {
                             *(file) << "'\\"
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
        *(file) << "L'\\x"
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

    emit_line_marker(node);
    indent();
    *(file) << label_sym.get_name() << " : ";

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

    // // No condition top as "for((i=0); ...)" looks unnecessary ugly
    int old = state.in_for_stmt_decl;
    state.in_for_stmt_decl = 1;

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

    // But it is desirable for the condition in "for( ... ; (i = x) ; ...)"
    state.in_for_stmt_decl = old;
    *(file) << "; ";

    old = state.in_condition;
    Nodecl::NodeclBase old_condition_top = state.condition_top;

    state.in_condition = 1;
    state.condition_top = no_conv(cond);

    walk(cond);
    *(file) << "; ";

    state.condition_top = old_condition_top;
    state.in_condition = old;

    // Here we do not care about parentheses "for ( ... ; ... ; i = i + 1)"
    walk(next);
}

CxxBase::Ret CxxBase::visit(const Nodecl::IteratorLoopControl& node)
{
    Nodecl::NodeclBase node_iterator_symbol = node.get_range_iterator();
    TL::Symbol iterator_symbol = node_iterator_symbol.get_symbol();

    bool old_in_condition = state.in_condition;
    state.in_condition = 1;

    define_or_declare_variable(iterator_symbol, /* is_definition */ 1);

    state.in_condition = old_in_condition;

    *file << " : ";

    walk(node.get_initializer());
}

CxxBase::Ret CxxBase::visit(const Nodecl::MemberInit& node)
{
    TL::Symbol entry = node.get_symbol();
    Nodecl::NodeclBase init_expr = node.get_init_expr();

    if (entry.is_class())
    {
        // Use the qualified name, do not rely on class-scope unqualified lookup
        *(file) << entry.get_qualified_name();
    }
    else
    {
        // Otherwise the name must not be qualified
        *(file) << entry.get_name();
    }

    if (nodecl_calls_to_constructor(init_expr))
    {
        // Ignore top level constructor
        *(file) << "(";
        walk_expression_list(nodecl_calls_to_constructor_get_arguments(init_expr));
        *(file) << ")";
    }
    else if (init_expr.is<Nodecl::StructuredValue>())
    {
        bool braces = false;
        if (!init_expr.as<Nodecl::StructuredValue>().get_form().is_null()
                && (init_expr.as<Nodecl::StructuredValue>().get_form().is<Nodecl::StructuredValueBracedImplicit>()
                    || init_expr.as<Nodecl::StructuredValue>().get_form().is<Nodecl::StructuredValueBracedTypecast>()))
            braces = true;

        if (braces)
            *(file) << "{";
        else
            *(file) << "(";

        walk_expression_list(init_expr.as<Nodecl::StructuredValue>().get_items().as<Nodecl::List>());

        if (braces)
            *(file) << "}";
        else
            *(file) << ")";
    }
    else
    {
        *(file) << "(";
        walk(init_expr);
        *(file) << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::New& node)
{
    Nodecl::NodeclBase initializer = node.get_init();
    ERROR_CONDITION(initializer.is_null(), "New lacks initializer", 0);

    Nodecl::NodeclBase placement = node.get_placement();
    // Nodecl::NodeclBase operator_new = nodecl_get_child(node, 2);

    if (node.get_text() == "global")
        *(file) << "::";

    *(file) << "new ";

    if (!placement.is_null())
    {
        *(file) << "(";
        walk_expression_list(placement.as<Nodecl::List>());
        *(file) << ")";
    }

    Nodecl::NodeclBase type = node.get_init_real_type();
    TL::Type init_real_type = type.get_type();

    if (init_real_type.is_array()
            && !init_real_type.array_get_size().is_constant())
    {
        // If the array is nonconstant do not emit parentheses
        *(file) << this->get_declaration(init_real_type, this->get_current_scope(),  "");
    }
    else
    {
        *(file) << "(" << this->get_declaration(init_real_type, this->get_current_scope(),  "") << ")";
    }

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
    else
    {
        *(file) << "(";
        // A a; we cannot emmit it as A a(); since this would declare a function () returning A
        if (nodecl_calls_to_constructor(initializer))
        {
            Nodecl::List constructor_args = nodecl_calls_to_constructor_get_arguments(initializer);

            // Here we add extra parentheses lest the direct-initialization looked like
            // as a function declarator (faced with this ambiguity, C++ chooses the latter!)
            //
            // A x( (A()) ); cannot become A x( A() ); because it would declare 'x' as a
            // "function (pointer to function() returning A) returning A"
            // [extra blanks added for clarity in the example above]
            walk_initializer_list(constructor_args, ", ");
        }
        else
        {
            walk(initializer);
        }
        *(file) << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxNoexcept& node)
{
    *(file) << "noexcept(";
    walk(node.get_expr());
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDepNew& node)
{
    Nodecl::NodeclBase initializer = node.get_init();
    ERROR_CONDITION(initializer.is_null(), "Dependent new lacks initializer", 0);

    if (node.get_text() == "global")
        *(file) << "::";

    *(file) << "new ";

    Nodecl::NodeclBase placement = node.get_placement();
    if (!placement.is_null())
    {
        *(file) << "(";
        walk_expression_list(placement.as<Nodecl::List>());
        *(file) << ")";
    }

    Nodecl::NodeclBase type = node.get_init_real_type();
    TL::Type init_real_type = type.get_type();

   *(file) << this->get_declaration(init_real_type, this->get_current_scope(),  "");

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
    *(file) << "__builtin_offsetof(";

    walk(node.get_offset_type());

    *(file) << ", ";

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

    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::ParenthesizedExpression& node)
{
    Nodecl::NodeclBase nest = node.get_nest();
    *(file) << "(";
    walk(nest);
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::PointerToMember& node)
{
    TL::Symbol symbol = node.get_symbol();

    *(file) << "&" << this->get_qualified_name(symbol);
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaClauseArg& node)
{
    *(file) << node.get_text();
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomClause& node)
{
    Nodecl::NodeclBase arguments = node.get_arguments();

    *(file) << node.get_text();

    if (!arguments.is_null())
    {
        *(file) << "(";
        walk_list(arguments.as<Nodecl::List>(), ", ");
        *(file) << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomDeclaration& node)
{
    Nodecl::NodeclBase pragma_line = node.get_pragma_line();
    Nodecl::NodeclBase nested_pragma = node.get_nested_pragma();
    TL::Symbol symbol = node.get_symbol();

    indent();

    // FIXME  parallel|for must be printed as parallel for
    *(file) << start_inline_comment();
    *(file) << "decl: #pragma " << node.get_text() << " ";
    walk(pragma_line);
    *(file) << "' " << this->get_qualified_name(symbol) << "'";
    *(file) << end_inline_comment();
    *(file) << "\n";
    walk(nested_pragma);
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomDirective& node)
{
    Nodecl::NodeclBase pragma_line = node.get_pragma_line();

    indent();
    *(file) << "#pragma " << node.get_text() << " ";
    walk(pragma_line);
    *(file) << "\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomLine& node)
{
    Nodecl::NodeclBase parameters = node.get_parameters();
    Nodecl::NodeclBase clauses = node.get_clauses();

    *(file) << node.get_text();

    if (!parameters.is_null())
    {
        *(file) << "(";
        walk_list(parameters.as<Nodecl::List>(), ", ");
        *(file) << ")";
    }
    else
    {
        *(file) << " ";
    }

    walk_list(clauses.as<Nodecl::List>(), " ");
}

CxxBase::Ret CxxBase::visit(const Nodecl::PragmaCustomStatement& node)
{
    Nodecl::NodeclBase pragma_line = node.get_pragma_line();
    Nodecl::NodeclBase statement = node.get_statements();

    emit_line_marker(node);
    indent();
    // FIXME  parallel|for must be printed as parallel for
    *(file) << "#pragma " << node.get_text() << " ";
    walk(pragma_line);
    *(file) << "\n";
    walk(statement);
}

CxxBase::Ret CxxBase::visit(const Nodecl::PseudoDestructorName& node)
{
    Nodecl::NodeclBase lhs = node.get_accessed();

    TL::Type t = lhs.get_type().no_ref();
    ERROR_CONDITION(!t.is_class(), "Invalid pseudo destructor name", 0);

    TL::Symbol destructor = class_type_get_destructor(t.get_internal_type());
    ERROR_CONDITION(!destructor.is_valid(), "Invalid class", 0);

    char needs_parentheses = operand_has_lower_priority(node, lhs);
    if (needs_parentheses)
    {
        *(file) << "(";
    }
    walk(lhs);
    if (needs_parentheses)
    {
        *(file) << ")";
    }
    *(file) << ".";
    *file << this->get_qualified_name(destructor);
}

CxxBase::Ret CxxBase::visit(const Nodecl::Range& node)
{
    Nodecl::NodeclBase lb_expr = node.get_lower();
    Nodecl::NodeclBase ub_expr = node.get_upper();
    Nodecl::NodeclBase step_expr = node.get_stride();

    Nodecl::NodeclBase parent = node.get_parent();
    bool enclose_in_square_brackets = (!parent.is<Nodecl::List>()
            || !parent.get_parent().is<Nodecl::ArraySubscript>());

    // Print the bracket when the range is not within an ArraySubscript (this is used by Analysis)
    if(enclose_in_square_brackets)
    {
        *(file) << "[";
    }

    walk(lb_expr);
    *(file) << ":";
    walk(ub_expr);

    if(enclose_in_square_brackets)
    {
        // When we enclose_in_square_brackets we also want to emit the stride,
        // regardless of it being one
        *(file) << ":";
        walk(step_expr);
        *(file) << "]";
    }
    else
    {
        // Do not emit stride 1 because it looks weird in C
        if (!step_expr.is_constant()
            || (const_value_cast_to_signed_int(step_expr.get_constant()) != 1))
        {
            *(file) << ":";
            walk(step_expr);
        }
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::ReturnStatement& node)
{
    Nodecl::NodeclBase expression = node.get_value();

    emit_line_marker(node);
    indent();
    *(file) << "return ";
    walk(expression);
    *(file) << ";\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Shaping& node)
{
    Nodecl::NodeclBase postfix = node.get_postfix();
    Nodecl::List seq_exp = node.get_shape().as<Nodecl::List>();

    for (Nodecl::List::iterator it = seq_exp.begin();
            it != seq_exp.end();
            it++)
    {
        *(file) << "[";
        walk(*it);
        *(file) << "]";
    }

    *(file) << " ";
    walk(postfix);
}

CxxBase::Ret CxxBase::visit(const Nodecl::Sizeof& node)
{
    TL::Type t = node.get_size_type().get_type();

    // This is not very precise but should do most of the time
    if (is_non_language_reference_type(t))
        t = t.no_ref();

    *(file) << "sizeof(" << this->get_declaration(t, this->get_current_scope(),  "") << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Alignof& node)
{
    Nodecl::NodeclBase align = node.get_align_type();

    if (IS_CXX11_LANGUAGE)
    {
        *(file) << "alignof(";
    }
    else
    {
        *(file) << "__alignof__(";
    }

    if (align.is<Nodecl::Type>())
    {
        TL::Type t = align.get_type();

        // This is not very precise but should do most of the time
        if (is_non_language_reference_type(t))
            t = t.no_ref();

        *(file) << this->get_declaration(t, this->get_current_scope(),  "");
    }
    else
    {
        walk(node.get_align_type());
    }
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Analysis::EmptyRange& node)
{
    *(file) << "∅";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Analysis::MinusInfinity& node)
{
    *(file) << "-INF";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Analysis::PlusInfinity& node)
{
    *(file) << "+INF";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Analysis::Maximum& node)
{
    *(file) << "max(";
    Nodecl::List expressions = node.get_expressions().as<Nodecl::List>();
    for(Nodecl::List::iterator it = expressions.begin(); it != expressions.end(); )
    {
        walk(*it);
        ++it;
        if(it != expressions.end())
            *(file) << ", ";
    }
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Analysis::Minimum& node)
{
    *(file) << "min(";
    Nodecl::List expressions = node.get_expressions().as<Nodecl::List>();
    for(Nodecl::List::iterator it = expressions.begin(); it != expressions.end(); )
    {
        walk(*it);
        ++it;
        if(it != expressions.end())
            *(file) << ", ";
    }
    *(file) << ")";    
}

CxxBase::Ret CxxBase::visit(const Nodecl::Analysis::Phi& node)
{
    *(file) << "Φ(";
    Nodecl::List expressions = node.get_expressions().as<Nodecl::List>();
    for(Nodecl::List::iterator it = expressions.begin(); it != expressions.end(); )
    {
        walk(*it);
        ++it;
        if(it != expressions.end())
            *(file) << ", ";
    }
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Analysis::RangeIntersection& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    bool lhs_need_parenthesis = lhs.is<Nodecl::Analysis::RangeUnion>();
    Nodecl::NodeclBase rhs = node.get_rhs();
    bool rhs_need_parenthesis = rhs.is<Nodecl::Analysis::RangeUnion>();
    
    if(lhs_need_parenthesis)
        *(file) << "(";
    walk(lhs);
    if(lhs_need_parenthesis)
        *(file) << ")";
    *(file) << " ∩ ";
    if(rhs_need_parenthesis)
        *(file) << "(";
    walk(rhs);
    if(rhs_need_parenthesis)
        *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Analysis::RangeSub& node)
{
    walk(node.get_lhs());
    *(file) << " - ";
    walk(node.get_rhs());
}

CxxBase::Ret CxxBase::visit(const Nodecl::Analysis::RangeUnion& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    bool lhs_need_parenthesis = lhs.is<Nodecl::Analysis::RangeIntersection>();
    Nodecl::NodeclBase rhs = node.get_rhs();
    bool rhs_need_parenthesis = rhs.is<Nodecl::Analysis::RangeIntersection>();
    
    if(lhs_need_parenthesis)
        *(file) << "(";
    walk(lhs);
    if(lhs_need_parenthesis)
        *(file) << ")";
    *(file) << " ∪ ";
    if(rhs_need_parenthesis)
        *(file) << "(";
    walk(rhs);
    if(rhs_need_parenthesis)
        *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::StringLiteral& node)
{
    const_value_t* v = nodecl_get_constant(node.get_internal_nodecl());

    int *bytes = NULL;
    int length = 0;
    char is_null_ended = 0;
    const_value_string_unpack_to_int(v, &bytes, &length, &is_null_ended);

    type_t* base_type = get_unqualified_type(
            array_type_get_element_type(no_ref(nodecl_get_type(node.get_internal_nodecl())))
            );

    std::string prefix;
    if (is_char_type(base_type)
            || is_signed_char_type(base_type)
            || is_unsigned_char_type(base_type))
    {
        // No prefix
    }
    else if (is_wchar_t_type(base_type))
    {
        prefix = "L";
    }
    else if (is_char16_t_type(base_type))
    {
        prefix = "u";
    }
    else if (is_char32_t_type(base_type))
    {
        prefix = "U";
    }
    else if (IS_C_LANGUAGE && is_integral_type(base_type))
    {
        prefix = "L";
    }

    *(file) << quote_c_string(bytes, length, prefix);

    DELETE(bytes);
}

CxxBase::Ret CxxBase::visit(const Nodecl::StructuredValue& node)
{
    Nodecl::List items = node.get_items().as<Nodecl::List>();
    TL::Type type = node.get_type().no_ref();

    Nodecl::NodeclBase form = node.get_form();
    enum structured_form_kind_t
    {
        INVALID = 0,
        TYPECAST, // C, C++  (T)expression
        COMPOUND_LITERAL, // C99 (struct X){initializer-clause}
        EXPLICIT_TYPECAST_PARENTHESIZED, // C++03   T(1, 2)
        EXPLICIT_TYPECAST_BRACED, // C++11   T{1, 2}
        IMPLICIT_BRACED, // C++11   {1, 2}
        UNKNOWN, // The node does not have an explicit form
    } structured_value_form = INVALID;

    if (!form.is_null())
    {
        if (form.is<Nodecl::StructuredValueBracedTypecast>())
        {
            structured_value_form = EXPLICIT_TYPECAST_BRACED;
        }
        else if (form.is<Nodecl::StructuredValueBracedImplicit>())
        {
            structured_value_form = IMPLICIT_BRACED;
        }
        else if (form.is<Nodecl::StructuredValueParenthesized>())
        {
            structured_value_form = EXPLICIT_TYPECAST_PARENTHESIZED;
        }
        else if (form.is<Nodecl::StructuredValueCompoundLiteral>())
        {
            structured_value_form = COMPOUND_LITERAL;
        }
    }
    else
    {
        structured_value_form = UNKNOWN;
    }

    if (structured_value_form == UNKNOWN)
    {
        // Best effort
        CXX_LANGUAGE()
        {
            structured_value_form = EXPLICIT_TYPECAST_PARENTHESIZED;
        }
        C_LANGUAGE()
        {
            structured_value_form = TYPECAST;
        }
    }

    if (structured_value_form == TYPECAST
            || structured_value_form == COMPOUND_LITERAL)
    {
        // ( type )
        *(file) << "(" << this->get_declaration(type, this->get_current_scope(), "") << ")";
    }

    bool wrapped_in_compound_expression = false;

    if (structured_value_form == EXPLICIT_TYPECAST_PARENTHESIZED
            || (structured_value_form == EXPLICIT_TYPECAST_BRACED
                && !state.inside_structured_value))
    {
        if (type.is_signed_short_int())
        {
            *(file) << "short";
        }
        else if (type.is_signed_long_int())
        {
            *(file) << "long";
        }
        else if (type.is_unsigned_int())
        {
            *(file) << "unsigned";
        }
        else if ((!type.is_named()
                    && (type.is_pointer()
                        || type.is_any_reference()
                        || type.is_array()
                        || type.is_pointer_to_member()))
                || get_cv_qualifier(type.get_internal_type()) != CV_NONE

                // Builtin types requiring more than one token
                || (type.is_char() && !(type.is_same_type(::get_char_type())))
                || type.is_unsigned_char()
                || type.is_unsigned_int()
                || type.is_unsigned_short_int()
                || type.is_unsigned_long_int()
                || type.is_signed_long_long_int()
                || type.is_unsigned_long_long_int()
                || type.is_long_double()
                )
        {
            // This is a hack to workaround the somewhat limited expressivity
            // of C++ in this syntax
            wrapped_in_compound_expression = true;
            TL::Counter &c = TL::CounterManager::get_counter("auxiliar_type");

            std::stringstream ss;
            ss << "__aux_type" << (int)c;
            c++;

            *(file) << "({ typedef "
                << this->get_declaration(type, this->get_current_scope(), ss.str()) << "; "
                << ss.str();
        }
        else
        {
            // Usual case
            *(file) << this->get_declaration(type, this->get_current_scope(),  "");
        }
    }

    if (structured_value_form == EXPLICIT_TYPECAST_PARENTHESIZED
            || structured_value_form == TYPECAST)
    {
        *(file) << "(";
    }
    if (structured_value_form == EXPLICIT_TYPECAST_BRACED
            || structured_value_form == IMPLICIT_BRACED
            || structured_value_form == COMPOUND_LITERAL)
    {
        *(file) << "{";
    }

    bool inside_structured_value = state.inside_structured_value;
    state.inside_structured_value = true;
    walk_expression_list(items);
    state.inside_structured_value = inside_structured_value;

    if (structured_value_form == EXPLICIT_TYPECAST_BRACED
            || structured_value_form == IMPLICIT_BRACED
            || structured_value_form == COMPOUND_LITERAL)
    {
        *(file) << "}";
    }
    if (structured_value_form == EXPLICIT_TYPECAST_PARENTHESIZED
            || structured_value_form == TYPECAST)
    {
        *(file) << ")";
    }

    if (wrapped_in_compound_expression)
    {
        *(file) << "; })";
    }
}

#if 0
CxxBase::Ret CxxBase::visit(const Nodecl::StructuredValue& node)
{
    Nodecl::List items = node.get_items().as<Nodecl::List>();
    TL::Type type = node.get_type().no_ref();

    enum structured_value_kind
    {
        INVALID = 0,
        // (T) { expr-list }
        GCC_POSTFIX,
        // T(single-expression)
        CXX03_EXPLICIT,
        // T{expr-list}
        CXX11_EXPLICIT,
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
                    || type.is_builtin())))
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
    else if (IS_CXX11_LANGUAGE)
    {
        if (type.is_vector())
        {
            // This is nonstandard, lets fallback to gcc
            kind = GCC_POSTFIX;
        }
        else if (type.is_named())
        {
            kind = CXX11_EXPLICIT;
        }
        if ((items.empty()
                    || ((items.size() == 1)
                && (type.is_named()
                    || type.is_builtin())))
                && !(type.is_class()
                    && type.is_aggregate()))
        {
            kind = CXX11_EXPLICIT;
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
                    *(file) << "(" << this->get_declaration(type, this->get_current_scope(),  "") << ")";
                }

                char inside_structured_value = state.inside_structured_value;
                state.inside_structured_value = true;

                *(file) << "{ ";
                walk_expression_list(items);
                *(file) << " }";

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
                    *(file) << "short";
                }
                else if (type.is_signed_long_int())
                {
                    *(file) << "long";
                }
                else if (type.is_unsigned_int())
                {
                    *(file) << "unsigned";
                }
                else
                {
                    // No effort done for invalid cases that the syntax does not allow
                    *(file) << this->get_declaration(type, this->get_current_scope(),  "");
                }

                if (items.empty())
                {
                    *(file) << "()";
                }
                else
                {
                    *(file) << "(";

                    char inside_structured_value = state.inside_structured_value;
                    state.inside_structured_value = 0;

                    walk_expression_list(items);

                    state.inside_structured_value = inside_structured_value;

                    *(file) << ")";
                }

                break;
            }
            // T{expr-list}
        case CXX11_EXPLICIT:
            {
                if (type.is_signed_short_int())
                {
                    *(file) << "short";
                }
                else if (type.is_signed_long_int())
                {
                    *(file) << "long";
                }
                else if (type.is_unsigned_int())
                {
                    *(file) << "unsigned";
                }
                else
                {
                    // No effort done for invalid cases that the syntax does not allow
                    *(file) << this->get_declaration(type, this->get_current_scope(),  "");
                }

                char inside_structured_value = state.inside_structured_value;
                state.inside_structured_value = 1;

                *(file) << "{ ";
                walk_expression_list(items);
                *(file) << " }";

                state.inside_structured_value = inside_structured_value;
                break;
            }
        default:
            {
                internal_error("Code unreachable", 0);
            }
    }
}
#endif

CxxBase::Ret CxxBase::visit(const Nodecl::SwitchStatement& node)
{
    Nodecl::NodeclBase expression = node.get_switch();
    Nodecl::NodeclBase statement = node.get_statement();

    emit_line_marker(node);
    indent();
    *(file) << "switch (";
    Nodecl::NodeclBase old_condition_top = state.condition_top;
    int old_condition = state.in_condition;
    int old_indent = get_indent_level();

    set_indent_level(0);
    state.in_condition = 1;
    state.condition_top = no_conv(expression);

    walk(expression);

    set_indent_level(old_indent);
    state.in_condition = old_condition;
    state.condition_top = old_condition_top;

    *(file) << ")\n";

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
        *(file) << "(*";
    }

    C_LANGUAGE()
    {
        *(file) << entry.get_name();
    }
    CXX_LANGUAGE()
    {
        // Builtins cannot be qualified
        if (entry.is_builtin()
                || (entry.is_function() &&
                    // Friend declared functions cannot be qualified
                    (entry.is_friend_declared()
                     // Not friend declared functions (because we will
                     // eventually find their function definition in this same
                     // file) but not yet defined but seen only in a friend
                     // declaration, cannot be qualified either
                     || (state.friend_function_declared_but_not_defined.find(entry)
                         != state.friend_function_declared_but_not_defined.end()))))
        {
            *(file) << entry.get_name();
        }
        else if (entry.is_function())
        {
            if (node.get_type().is_valid()
                    && node.get_type().is_unresolved_overload())
            {
                *(file) << this->get_qualified_name(entry,
                        this->get_current_scope().get_decl_context(),
                        /* without_template_id */ true);

                TL::TemplateParameters template_arguments =
                    node.get_type().unresolved_overloaded_type_get_explicit_template_arguments();

                if (template_arguments.is_valid())
                {
                    *(file) << ::template_arguments_to_str(
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
                *(file) << this->get_qualified_name(entry,
                        this->get_current_scope(),
                        /* without template id */ state.visiting_called_entity_of_function_call);
            }
        }
        else if (!entry.is_dependent_entity())
        {
            *(file) << this->get_qualified_name(entry, this->get_current_scope());
        }
        else
        {
            ERROR_CONDITION(entry.get_value().is_null(), "A dependent entity must have a tree of its name!", 0);
            walk(entry.get_value());
        }
    }

    if (must_derref)
    {
        *(file) << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::Text& node)
{
    *(file) << node.get_text();
}

CxxBase::Ret CxxBase::visit(const Nodecl::Throw& node)
{
    Nodecl::NodeclBase expr = node.get_rhs();

    *(file) << "throw";

    if (!expr.is_null())
    {
        *(file) << " ";
        if (get_rank(expr) < get_rank_kind(NODECL_ASSIGNMENT, ""))
        {
            // A comma operator could slip in
            *(file) << "(";
        }
        walk(expr);
        if (get_rank(expr) < get_rank_kind(NODECL_ASSIGNMENT, ""))
        {
            *(file) << ")";
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

    *(file) << "try\n";

    walk(statement);
    walk(catch_handlers);

    if (!any_catch_handler.is_null())
    {
        indent();
        *(file) << "catch (...)\n";
        walk(any_catch_handler);
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::Type& node)
{
    TL::Type type = node.get_type();
    *(file) << this->get_declaration(type, this->get_current_scope(),  "");
}

CxxBase::Ret CxxBase::visit(const Nodecl::Typeid& node)
{
    Nodecl::NodeclBase expr = node.get_arg();

    *(file) << "typeid(";
    walk(expr);
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Unknown& node)
{
    *(file) << "UNKNOWN";
}

CxxBase::Ret CxxBase::visit(const Nodecl::VirtualFunctionCall& node)
{
    visit_function_call(node, /* is_virtual_call */ true);
}

#if 0
CxxBase::Ret CxxBase::visit(const Nodecl::VectorAlignRight& node)
{
    indent();
    *(file) << "{(";

    walk(node.get_left_vector());

    *(file) << ", ";

    walk(node.get_right_vector());

    *(file) << ") >> ";

    walk(node.get_num_elements());

    *(file) << "}";
}

CxxBase::Ret CxxBase::visit(const Nodecl::VectorConversion& node)
{
    // Do nothing
    walk(node.get_nest());
}

CxxBase::Ret CxxBase::visit(const Nodecl::VectorLaneId& node)
{
    indent();
    *(file) << "VECTOR_LANE_ID";
}

CxxBase::Ret CxxBase::visit(const Nodecl::VectorLiteral& node)
{
    indent();
    *(file) << "{";
    
    Nodecl::List scalar_values = node.get_scalar_values().as<Nodecl::List>();
    for(Nodecl::List::iterator it = scalar_values.begin();
            it != scalar_values.end();
            it++)
    {
        walk(*it);

        if ((it+1) != scalar_values.end())
            *(file) << ", ";
    }

    *(file) << "}";
}

CxxBase::Ret CxxBase::visit(const Nodecl::VectorLoad& node)
{
    indent();
    *(file) << "VL{";
    walk(node.get_rhs());
    *(file) << "}";
}


CxxBase::Ret CxxBase::visit(const Nodecl::VectorPromotion& node)
{
    indent();
    *(file) << "{";
    walk(node.get_rhs());
    *(file) << "}";
}
#endif

// Bug in GCC 4.4
template CxxBase::Ret CxxBase::visit_function_call<Nodecl::VirtualFunctionCall>(const Nodecl::VirtualFunctionCall& node, bool is_virtual_call);

CxxBase::Ret CxxBase::visit(const Nodecl::WhileStatement& node)
{
    Nodecl::NodeclBase condition = node.get_condition();
    Nodecl::NodeclBase statement = node.get_statement();

    emit_line_marker(node);
    indent();
    *(file) << "while (";

    int old = state.in_condition;
    Nodecl::NodeclBase old_condition_top = state.condition_top;
    int old_indent = get_indent_level();
    set_indent_level(0);
    state.in_condition = 1;
    state.condition_top = no_conv(condition);

    emit_line_marker(condition);
    walk(condition);

    set_indent_level(old_indent);
    state.in_condition = old;
    state.condition_top = old_condition_top;
    *(file) << ")\n";

    inc_indent();
    walk(statement);
    dec_indent();
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxImplicitInstantiation& node)
{
    *file << start_inline_comment();
    *file << "Instantiation of ";
    TL::Symbol sym = node.get_symbol();
    if (sym.is_class())
    {
        *file << "class template";
        *file << " '";
        *file << this->get_qualified_name(node.get_symbol());
        *file << "'";
    }
    else if (sym.is_function())
    {
       *file << "template function";
        *file << " '";
        *file << this->get_declaration(
                node.get_symbol().get_type(),
                node.get_symbol().get_scope(),
                this->get_qualified_name(node.get_symbol()));
        *file << "'";
    }
    else
    {
        *file << "<<unexpected-symbol-kind>>";
    }
    *file << end_inline_comment() << "\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxDecl& node)
{
    TL::Symbol sym = node.get_symbol();

    C_LANGUAGE()
    {
        walk_type_for_symbols(sym.get_type(),
                &CxxBase::declare_symbol_always,
                &CxxBase::define_symbol_always,
                &CxxBase::define_all_entities_in_trees);
    }

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
        move_to_namespace(context.get_related_symbol());
    }

    indent();
    *(file) << "using " << this->get_qualified_name(sym, context) << ";\n";
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
    *(file) << "using namespace " << this->get_qualified_name(sym) << ";\n";
}

void CxxBase::codegen_explicit_instantiation(TL::Symbol sym,
        const Nodecl::NodeclBase & declarator_name,
        const Nodecl::NodeclBase & context,
        bool is_extern)
{
    if (sym.is_class())
    {
        indent();
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
            *(file) << "extern ";

        std::string attributes;
        if (sym.has_gcc_attributes())
        {
            attributes = gcc_attributes_to_str(sym) + " ";
        }
        if (sym.has_alignas())
        {
            attributes += alignas_attributes_to_str(sym) + " ";
        }

        *(file) << "template " << class_key << " " << attributes << this->get_qualified_name(sym, sym.get_scope()) << ";\n";

    }
    else if (sym.is_function())
    {
        indent();
        const decl_context_t* decl_context = context.retrieve_context().get_decl_context();
        move_to_namespace(decl_context->namespace_scope->related_entry);

        if (is_extern)
            *(file) << "extern ";

        std::string gcc_attributes;
        if (sym.has_gcc_attributes())
        {
            gcc_attributes = gcc_attributes_to_str(sym) + " ";
        }

        TL::Type real_type = sym.get_type();
        if (sym.is_conversion_function()
                || sym.is_destructor())
        {
            // FIXME - Use TL::Type to build this type
            real_type = get_new_function_type(NULL, NULL, 0, REF_QUALIFIER_NONE);
            if (sym.is_conversion_function())
            {
                real_type = real_type.get_as_qualified_as(sym.get_type());
            }
        }

        std::string original_declarator_name = this->codegen_to_str(declarator_name, sym.get_scope());
        *(file) << "template " << gcc_attributes << this->get_declaration(real_type, sym.get_scope(), original_declarator_name) << ";\n";
    }
    else if (sym.is_variable())
    {
        indent();
        // This should be a nonstatic member
        if (is_extern)
            *(file) << "extern ";

        std::string gcc_attributes;
        if (sym.has_gcc_attributes())
        {
            gcc_attributes = gcc_attributes_to_str(sym) + " ";
        }

        std::string original_declarator_name = this->codegen_to_str(declarator_name, sym.get_scope());
        *(file) << "template " << gcc_attributes << this->get_declaration(sym.get_type(), sym.get_scope(), original_declarator_name) << ";\n";
    }
    else
    {
        internal_error("Invalid symbol %s", sym.get_name().c_str());
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxExplicitInstantiationDef& node)
{
    TL::Symbol sym = node.get_symbol();
    Nodecl::NodeclBase declarator_name = node.get_declarator_name();
    Nodecl::NodeclBase context = node.get_context();
    state.must_be_object_init.erase(sym);

    move_to_namespace_of_symbol(sym);

    codegen_explicit_instantiation(sym, declarator_name, context);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxExplicitInstantiationDecl& node)
{
    TL::Symbol sym = node.get_symbol();
    Nodecl::NodeclBase declarator_name = node.get_declarator_name();
    Nodecl::NodeclBase context = node.get_context();
    state.must_be_object_init.erase(sym);

    move_to_namespace_of_symbol(sym);

    codegen_explicit_instantiation(sym, declarator_name, context, /* is_extern */ true);
}

CxxBase::Ret CxxBase::visit(const Nodecl::CxxStaticAssert& node)
{
    emit_line_marker(node);
    indent();
    *file << "static_assert(";
    walk(node.get_predicate());
    if (!node.get_message().is_null())
    {
        *file << ", ";
        walk(node.get_message());
    }
    *file << ");\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::Verbatim& node)
{
    *(file) << node.get_text();
}

CxxBase::Ret CxxBase::visit(const Nodecl::VlaWildcard& node)
{
    *(file) << "*";
}

CxxBase::Ret CxxBase::visit(const Nodecl::UnknownPragma& node)
{
    move_to_namespace(node.retrieve_context().get_decl_context()->namespace_scope->related_entry);

    *(file) << "#pragma " << node.get_text() << "\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::GxxTrait& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    *(file) << node.get_text() << "(";

    walk(lhs);

    if (!rhs.is_null())
    {
        *(file) << ", ";

        TL::Type t = rhs.get_type();

        if (!is_sequence_of_types(t.get_internal_type()))
        {
            walk(rhs);
        }
        else
        {
            int n = sequence_of_types_get_num_types(t.get_internal_type());

            for (int i = 0; i < n; i++)
            {
                if (i > 0)
                {
                    (*file) << ", ";
                }

                TL::Type type = sequence_of_types_get_type_num(t.get_internal_type(), i);
                *(file) << this->get_declaration(type, this->get_current_scope(),  "");
            }
        }
    }

    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::AsmDefinition& node)
{
    indent();
    *(file) << "asm ";
    Nodecl::List l = node.get_asm_text().as<Nodecl::List>();
    if (l.size() == 1)
    {
        (*file) << "(";
        walk(l[0]); // usual asm
    }
    else if (l.size() == 2)
    {
        walk(l[0]); // volatile
        (*file) << "(";
        walk(l[1]); // usual asm here
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
    *(file) << ");\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::GccAsmDefinition& node)
{
    Nodecl::NodeclBase op0 = node.get_operands0();
    Nodecl::NodeclBase op1 = node.get_operands1();
    Nodecl::NodeclBase op2 = node.get_operands2();

    Nodecl::NodeclBase specs = node.get_specs();

    indent();
    *(file) << "__asm__ ";
    walk_list(specs.as<Nodecl::List>(), ", ");
    *(file) << "(";
    *(file) << node.get_text();
    *(file) << " : ";
    walk_list(op0.as<Nodecl::List>(), ", ");
    *(file) << " : ";
    walk_list(op1.as<Nodecl::List>(), ", ");
    if (!op2.is_null())
    {
        *(file) << " : ";
        walk_list(op2.as<Nodecl::List>(), ", ");
    }
    *(file) << ");\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::GccAsmOperand& node)
{
    Nodecl::NodeclBase identifier = node.get_identifier();
    Nodecl::NodeclBase constraint = node.get_constraint();
    Nodecl::NodeclBase expr = node.get_expr();

    if (!identifier.is_null())
    {
        *(file) << "[" << identifier.get_text() << "]";
    }

    walk(constraint);

    if (!expr.is_null())
    {
        *(file) << "(";
        walk(expr);
        *(file) << ")";
    }
}

CxxBase::Ret CxxBase::visit(const Nodecl::GccAsmSpec& node)
{
    *(file) << " __asm(" << node.get_text() << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::GccBuiltinVaArg& node)
{
    *(file) << "__builtin_va_arg(";
    walk(node.get_expr());
    *(file) << ", ";
    walk(node.get_va_type());
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::UpcSyncStatement& node)
{
    *(file) << node.get_text() << "(";
    walk(node.get_expr());
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::SourceComment& node)
{
    indent();
    *(file) << start_inline_comment();
    *(file) << node.get_text();
    *(file) << end_inline_comment();
    *(file) << "\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::PreprocessorLine& node)
{
    *(file) << node.get_text() << "\n";
}

CxxBase::Ret CxxBase::visit(const Nodecl::IntelAssume& node)
{
    *(file) << "__assume(";
    walk(node.get_assumed());
    *(file) << ")";
}

CxxBase::Ret CxxBase::visit(const Nodecl::IntelAssumeAligned& node)
{
    *(file) << "__assume_aligned(";
    walk(node.get_pointer());
    *(file) << ", ";
    walk(node.get_alignment());
    *(file) << ")";
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

void CxxBase::define_class_symbol_using_member_declarations_aux(TL::Symbol symbol,
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
    if (symbol.has_alignas())
    {
        gcc_attributes += alignas_attributes_to_str(symbol) + " ";
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
        *(file) << gcc_extension << class_key << " " << gcc_attributes << ms_attributes_to_str(symbol);
        if (!symbol.is_anonymous_union())
        {
            // The symbol is called 'struct/union X' in C. We should ignore the
            // class key because it has been already printed.
            *(file) << " " << symbol.get_name().substr(class_key.length() + 1);
        }
        *(file) << "\n";

        indent();
        *(file) << "{\n";
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
                    while (template_parameters.is_valid()
                            && !template_parameters.get_is_explicit_instantiation()
                            && template_parameters.get_is_explicit_specialization())
                    {
                        indent();
                        *(file) << "template <>\n";
                        template_parameters = template_parameters.get_enclosing_parameters();
                    }
                }
            }
            else if (symbol.get_type().is_dependent()
                        || !CURRENT_CONFIGURATION->explicit_instantiation)
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
        *(file) << gcc_extension << class_key << " " << gcc_attributes << ms_attributes_to_str(symbol);
        if (!symbol.is_anonymous_union()
                && !(symbol.is_member()
                    && symbol.get_class_type().get_symbol().is_anonymous_union()))
        {
            *(file) << " " << qualified_name;
        }

        // class-virt-specifier
        if (symbol.is_final())
        {
            (*file) << " final";
        }
        else if (symbol.is_explicit_class())
        {
            (*file) << " explicit";
        }

        TL::ObjectList<TL::Type::BaseInfo> bases = symbol_type.get_bases();
        if (!bases.empty())
        {
            *(file) << " : " ;
            for (TL::ObjectList<TL::Type::BaseInfo>::iterator it = bases.begin();
                    it != bases.end();
                    it++)
            {
                if (it != bases.begin())
                {
                    *(file) << ", ";
                }

                TL::Symbol &base(it->base);
                bool is_virtual(it->is_virtual);
                access_specifier_t current_access_spec(it->access_specifier);

                if (is_virtual)
                {
                    *(file) << "virtual ";
                }

                if (current_access_spec != default_access_spec)
                {
                    if (current_access_spec == AS_PUBLIC)
                    {
                        *(file) << "public ";
                    }
                    else if (current_access_spec == AS_PRIVATE)
                    {
                        *(file) << "private ";
                    }
                    else if (current_access_spec == AS_PROTECTED)
                    {
                        *(file) << "protected ";
                    }
                    else
                    {
                        internal_error("Unreachable code", 0);
                    }
                }

                *(file) << this->get_qualified_name(base, symbol.get_scope());

                if (it->is_expansion)
                    (*file) << " ...";
            }
        }

        *(file) << "\n";
        indent();
        *(file) << "{\n";
    }

    TL::ObjectList<TL::MemberDeclarationInfo> members = symbol_type.get_member_declarations();
    access_specifier_t current_access_spec = default_access_spec;

    // 2.2 Mark friend functions as declared but not defined.
    //
    // We need to do this for member functions defined inside the class that
    // call a friend because we do not keep friends ordered
    TL::ObjectList<TL::Symbol> friends = symbol_type.class_get_friends();
    for (TL::ObjectList<TL::Symbol>::iterator it = friends.begin();
            it != friends.end();
            it++)
    {
        TL::Symbol &_friend_decl(*it);

        TL::Symbol _friend = _friend_decl;
        if (_friend_decl.is_friend_function()
                || _friend_decl.is_friend_class())
            _friend = _friend_decl.get_alias_to();

        if ((_friend.is_function() || _friend.is_dependent_friend_function()))
        {
            if (!_friend.get_function_code().is_null()
                    && _friend.is_defined_inside_class())
            {
                // Do nothing
            }
            else
            {
                if (get_codegen_status(_friend) == CODEGEN_STATUS_NONE)
                {
                    state.friend_function_declared_but_not_defined.insert(_friend);
                }
            }
        }
    }

    // 2.3 Declare members as usual
    bool previous_was_just_member_declarator_name = false;

    for (TL::ObjectList<TL::MemberDeclarationInfo>::iterator it = members.begin();
            it != members.end();
            it++)
    {
        TL::Symbol member(it->get_symbol());
        TL::Scope member_decl_scope(it->get_scope());
        // Note that declaration_is_definition is only used for classes and enums
        bool declaration_is_definition = it->get_is_definition();
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
                    *(file) << "public:\n";
                }
                else if (current_access_spec == AS_PRIVATE)
                {
                    *(file) << "private:\n";
                }
                else if (current_access_spec == AS_PROTECTED)
                {
                    *(file) << "protected:\n";
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
                if (previous_was_just_member_declarator_name)
                {
                    previous_was_just_member_declarator_name = false;
                    (*file) << ";\n";
                }

                if (!declaration_is_definition)
                {
                    do_declare_symbol(member,
                            &CxxBase::declare_symbol_always,
                            &CxxBase::define_symbol_always,
                            &member_decl_scope);
                }
                else
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
            }
            else if (member.is_using_symbol()
                    || member.is_using_typename_symbol())
            {
                if (previous_was_just_member_declarator_name)
                {
                    previous_was_just_member_declarator_name = false;
                    (*file) << ";\n";
                }

                indent();
                ERROR_CONDITION(!member.get_type().is_unresolved_overload(), "Invalid SK_USING symbol\n", 0);

                TL::ObjectList<TL::Symbol> unresolved = member.get_type().get_unresolved_overload_set();

                TL::Symbol entry = unresolved[0];
                *(file) << "using ";

                if (member.is_using_typename_symbol())
                    *(file) << "typename ";

                *(file) << this->get_qualified_name(entry, /* without_template */ 1) << ";\n";
            }
            else if (member.is_enum())
            {
                if (previous_was_just_member_declarator_name)
                {
                    previous_was_just_member_declarator_name = false;
                    (*file) << ";\n";
                }

                if (!declaration_is_definition)
                {
                    state.in_forwarded_member_declaration = true;

                    do_declare_symbol(member,
                            &CxxBase::declare_symbol_always,
                            &CxxBase::define_symbol_always);

                    state.in_forwarded_member_declaration = false;
                }
                else
                {
                    do_define_symbol(member,
                            &CxxBase::declare_symbol_always,
                            &CxxBase::define_symbol_always);
                    set_codegen_status(member, CODEGEN_STATUS_DEFINED);
                }
            }
            else if (member.is_typedef())
            {
                if (previous_was_just_member_declarator_name)
                {
                    previous_was_just_member_declarator_name = false;
                    (*file) << ";\n";
                }

                do_define_symbol(member,
                        &CxxBase::declare_symbol_always,
                        &CxxBase::define_symbol_always);
                set_codegen_status(member, CODEGEN_STATUS_DEFINED);
            }
            else if (member.is_function())
            {
                if (previous_was_just_member_declarator_name)
                {
                    previous_was_just_member_declarator_name = false;
                    (*file) << ";\n";
                }

                if (!member.get_function_code().is_null()
                        && member.is_defined_inside_class()
                        // Do not emit the empty bodies of defaulted functions
                        && !member.is_defaulted())
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
            else if (member.is_member_static_assert())
            {
                if (previous_was_just_member_declarator_name)
                {
                    previous_was_just_member_declarator_name = false;
                    (*file) << ";\n";
                }

                Nodecl::CxxStaticAssert cxx_static_assert = member.get_value().as<Nodecl::CxxStaticAssert>();
                push_scope(member.get_scope());

                indent();
                *file << "static_assert(";
                walk(cxx_static_assert.get_predicate());
                if (!cxx_static_assert.get_message().is_null())
                {
                    *file << ", ";
                    walk(cxx_static_assert.get_message());
                }
                *file << ");\n";

                pop_scope();
            }
            else
            {
                bool member_declaration_does_define = true;

                if (symbol.is_anonymous_union()
                        && member.get_type().basic_type().is_named_class()
                        && !member.get_type().basic_type().get_symbol().is_anonymous_union()
                        && member.get_type().basic_type().get_symbol().is_member()
                        && member.get_type().basic_type().get_symbol().get_class_type().get_symbol() == symbol)
                {
                    // But we only emit the name and an end for the declaration
                    if (previous_was_just_member_declarator_name)
                        (*file) << ", ";
                    (*file) << get_declaration_only_declarator(member.get_type(), symbol.get_scope(), member.get_name());
                    previous_was_just_member_declarator_name = true;
                }
                else
                {
                    if (previous_was_just_member_declarator_name)
                    {
                        previous_was_just_member_declarator_name = false;
                        (*file) << ";\n";
                    }

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
        }
        state.in_member_declaration = old_in_member_declaration;

        dec_indent();

        CXX_LANGUAGE()
        {
            dec_indent();
        }
    }

    // 3. Declare friends
    for (TL::ObjectList<TL::Symbol>::iterator it = friends.begin();
            it != friends.end();
            it++)
    {
        TL::Symbol &_friend(*it);
        inc_indent();
        if (_friend.is_friend_function()
                && !_friend.get_alias_to().get_function_code().is_null()
                && _friend.get_alias_to().is_defined_inside_class())
        {
            walk(_friend.get_alias_to().get_function_code());
        }
        else if (_friend.is_dependent_friend_function()
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

    // 4. Declare inherited constructors C++11
    TL::ObjectList<TL::Symbol> inherited_constructors =
        symbol_type.class_get_inherited_constructors();
    for (TL::ObjectList<TL::Symbol>::iterator it = inherited_constructors.begin();
            it != inherited_constructors.end();
            it++)
    {
        // This is not a constructor but a class symbol
        TL::Symbol &inherited_constructor(*it);
        inc_indent();
        indent();
        (*file) << "using " << this->get_qualified_name(inherited_constructor)
            << "::" << inherited_constructor.get_name() << ";\n";
        dec_indent();
    }

    indent();

    // Somehow it was left open
    if (previous_was_just_member_declarator_name)
    {
        (*file) << ";\n";
    }

    if (symbol.is_member()
            && !symbol.is_anonymous_union()
            && symbol.get_class_type().get_symbol().is_anonymous_union())
    {
        // Weird case of a struct/class/union inside an anonymous union
        // there should be a member after it.
        *(file) << "} ";
    }
    else
    {
        // Usual case
        *(file) << "};\n";
    }
}

void CxxBase::define_class_symbol_aux(TL::Symbol symbol,
        TL::ObjectList<TL::Symbol> symbols_defined_inside_class,
        int level,
        TL::Scope* scope)
{
    define_class_symbol_using_member_declarations_aux(
            symbol,
            symbols_defined_inside_class,
            level,
            scope);
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

void CxxBase::declare_dependent_friend_class(TL::Symbol friend_symbol, TL::Symbol class_symbol)
{
    TL::TemplateParameters template_parameters =
        friend_symbol.get_scope().get_template_parameters();

    codegen_template_headers_bounded(template_parameters,
            class_symbol.get_scope().get_template_parameters(),
            /* show default values */ false);

    if (friend_symbol.get_type().is_unnamed_class()
            || friend_symbol.get_type().is_template_type()
            || friend_symbol.get_type().is_named_class()
            || (friend_symbol.get_type().is_named()
                && friend_symbol.get_type().get_symbol().is_template()
                && friend_symbol.get_type().get_symbol().get_type().get_primary_template().is_named_class()))
    {
        // template <typename T>
        // struct B
        // {
        //    friend struct A;
        // };

        // template <typename T>
        // struct B
        // {
        //    template <typename S>
        //    friend struct A;
        // };

        std::string friend_class_key;
        TL::Type underlying_class = friend_symbol.get_type();

        if (underlying_class.is_template_type())
            underlying_class = underlying_class.get_primary_template();
        else if (underlying_class.is_named()
                && underlying_class.get_symbol().is_template())
            underlying_class = underlying_class.get_symbol().get_type().get_primary_template();

        switch (underlying_class.class_type_get_class_kind())
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

        indent();
        if (friend_symbol.get_type().is_named())
        {
            *file << "friend " << friend_class_key << " " << 
                this->get_qualified_name(
                        friend_symbol.get_type().get_symbol(),
                        friend_symbol.get_scope());
        }
        else
        {
            *file << "friend " << friend_class_key << " " << friend_symbol.get_name();
        }
    }
    else if (friend_symbol.get_type().is_named()
            && friend_symbol.get_type().get_symbol().is_dependent_entity())
    {
        indent();
        *file << "friend " << this->get_declaration(
                friend_symbol.get_type().get_symbol().get_type(),
                friend_symbol.get_scope(), "");
    }
    else // C++2011
    {
        indent();
        *file << "friend " << this->get_declaration(friend_symbol.get_type(), friend_symbol.get_scope(), "");
    }

    *file << ";\n";
}

void CxxBase::declare_nondependent_friend_class(TL::Symbol friend_decl_symbol, TL::Symbol class_symbol)
{
    TL::Symbol friend_symbol = friend_decl_symbol.get_alias_to();
    if (friend_symbol.is_class()
            || friend_symbol.is_template())
    {
        std::string friend_class_key;
        TL::Type underlying_class = friend_symbol.get_type();
        if (underlying_class.is_template_type())
            underlying_class = underlying_class.get_primary_template();

        switch (underlying_class.class_type_get_class_kind())
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

        TL::TemplateParameters template_parameters =
            friend_decl_symbol.get_scope().get_template_parameters();
        codegen_template_headers_bounded(template_parameters,
                class_symbol.get_scope().get_template_parameters(),
                /* show default values */ false);

        std::string symbol_name;
        if (friend_symbol.is_friend_declared()
                || (!friend_symbol.is_template()
                    && !friend_symbol.get_type().is_template_specialized_type()
                    && get_codegen_status(friend_symbol) == CODEGEN_STATUS_NONE)
                || (friend_symbol.is_template()
                    && get_codegen_status(friend_symbol
                        .get_type()
                        .get_primary_template()
                        .get_symbol()) == CODEGEN_STATUS_NONE))
        {
            symbol_name = friend_symbol.get_name();
        }
        else
        {
            symbol_name = this->get_qualified_name(
                    friend_symbol,
                    friend_decl_symbol.get_scope(),
                    /* without template id */ false);
        }

        indent();
        *file << "friend " << friend_class_key << " " << symbol_name << ";\n";
    }
    else // C++2011
    {
        indent();
        *file << "friend " << this->get_declaration(friend_symbol.get_type(), friend_symbol.get_scope(), "") << ";\n";
    }
}

void CxxBase::declare_dependent_friend_function(TL::Symbol friend_symbol, TL::Symbol class_symbol)
{
    TL::Type friend_type = friend_symbol.get_type();

    if (friend_type.is_template_specialized_type())
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

    std::string exception_spec = exception_specifier_to_str(friend_symbol);
    TL::Type real_type = friend_type;
    if (class_symbol.is_conversion_function())
    {
        // ??? - What about the qualifier?
        real_type = get_new_function_type(NULL, NULL, 0, REF_QUALIFIER_NONE);
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
    else if (get_codegen_status(friend_symbol) == CODEGEN_STATUS_NONE)
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

    // Protect this declarator because the decl-specifier seq might end with an
    // id-expression that would end being "pasted" to the declarator-name
    if (function_name.size() >= 2 &&
            function_name[0] == ':' &&
            function_name[1] == ':')
    {
        function_name = "(" + function_name + ")";
    }

    indent();
    *(file) << "friend ";
    *(file) << this->get_declaration(real_type, friend_symbol.get_scope(), function_name) << exception_spec;
    *(file) << ";\n";
}

void CxxBase::declare_nondependent_friend_function(TL::Symbol friend_symbol_decl, TL::Symbol class_symbol)
{
    TL::Symbol friend_symbol = friend_symbol_decl.get_alias_to();
    declare_dependent_friend_function(friend_symbol, class_symbol);
}

void CxxBase::declare_friend_symbol(TL::Symbol friend_symbol, TL::Symbol class_symbol)
{
    if (friend_symbol.is_dependent_friend_class())
    {
        declare_dependent_friend_class(friend_symbol, class_symbol);
    }
    else if (friend_symbol.is_dependent_friend_function())
    {
        declare_dependent_friend_function(friend_symbol, class_symbol);
    }
    else if (friend_symbol.is_friend_class())
    {
        declare_nondependent_friend_class(friend_symbol, class_symbol);
    }
    else if (friend_symbol.is_friend_function())
    {
        declare_nondependent_friend_function(friend_symbol, class_symbol);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

bool CxxBase::is_local_symbol(TL::Symbol entry)
{
    return entry.is_valid()
        && ((entry.get_scope().is_block_scope()
                    && entry.get_scope() == this->get_current_scope())
                || entry.get_scope().is_function_scope()
                || (entry.is_member() && is_local_symbol(entry.get_class_type().get_symbol())));
}

// Note: is_nonlocal_symbol is NOT EQUIVALENT to !is_local_symbol
bool CxxBase::is_nonlocal_symbol(TL::Symbol entry)
{
    return !(entry.is_valid()
            && (entry.get_scope().is_block_scope()
                || entry.get_scope().is_function_scope()
                || (entry.is_member() && is_local_symbol(entry.get_class_type().get_symbol()))));
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
    if (is_local_symbol(symbol))
    {
        do_define_symbol(symbol,
                &CxxBase::declare_symbol_if_local,
                &CxxBase::define_symbol_if_local);
    }
}

void CxxBase::declare_symbol_if_local(TL::Symbol symbol)
{
    if (is_local_symbol(symbol))
    {
        do_declare_symbol(symbol,
                &CxxBase::declare_symbol_if_local,
                &CxxBase::define_symbol_if_local);
    }
}

void CxxBase::define_symbol_if_nonlocal(TL::Symbol symbol)
{
    if (is_nonlocal_symbol(symbol))
    {
        do_define_symbol(symbol,
                &CxxBase::declare_symbol_if_nonlocal,
                &CxxBase::define_symbol_if_nonlocal);
    }
}

void CxxBase::declare_symbol_if_nonlocal(TL::Symbol symbol)
{
    if (is_nonlocal_symbol(symbol))
    {
        do_declare_symbol(symbol,
                &CxxBase::declare_symbol_if_nonlocal,
                &CxxBase::define_symbol_if_nonlocal);
    }
}

void CxxBase::define_symbol_if_nonlocal_nonprototype(TL::Symbol symbol)
{
    if (is_nonlocal_symbol(symbol)
            && !is_prototype_symbol(symbol))
    {
        do_define_symbol(symbol,
                &CxxBase::declare_symbol_if_nonlocal_nonprototype,
                &CxxBase::define_symbol_if_nonlocal_nonprototype);
    }
}

void CxxBase::declare_symbol_if_nonlocal_nonprototype(TL::Symbol symbol)
{
    if (is_nonlocal_symbol(symbol)
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
    static std::set<TL::Symbol> visited_symbols;
    static int nesting = 0;
    nesting++;

    define_generic_entities(node,
            &CxxBase::declare_symbol_if_nonnested,
            &CxxBase::define_symbol_if_nonnested,
            &CxxBase::define_nonnested_entities_in_trees,
            &CxxBase::entry_just_define,
            visited_symbols);

    nesting--;
    if (nesting == 0)
        visited_symbols.clear();
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
        Nodecl::NodeclBase init = no_conv(symbol.get_value());

        if (is_definition)
        {
            C_LANGUAGE()
            {
                *(file) << " = ";

                bool old = state.inside_structured_value;
                if (init.is<Nodecl::StructuredValue>())
                {
                    state.inside_structured_value = true;
                }

                bool extra_addr = false;
                if (is_non_language_reference_variable(symbol))
                {
                    if (!symbol.get_type().no_ref().is_array())
                    {
                        extra_addr = true;
                    }
                }

                if (extra_addr)
                {
                    *file << "&(";
                }

                walk(init);

                if (extra_addr)
                {
                    *file << ")";
                }

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
                    *(file) << " = ";

                    bool old = state.inside_structured_value;
                    state.inside_structured_value = true;
                    walk(init);
                    state.inside_structured_value = old;
                }
                else if (symbol.get_type().is_array()
                        && !init.is<Nodecl::StructuredValue>())
                {
                    if (nodecl_calls_to_constructor(init)
                            && nodecl_calls_to_constructor_default_init(init))
                    {
                        // Do not emit anything here
                    }
                    else
                    {
                        // Only for char and wchar_t
                        // const char c[] = "1234";
                        *(file) << " = ";
                        walk(init);
                    }
                }
                else if (!symbol.get_type().is_array()
                        && init.is<Nodecl::StructuredValue>())
                {
                    // char c = { 'a' };
                    // int x = { 1 };
                    *(file) << " = ";
                    bool old = state.inside_structured_value;
                    state.inside_structured_value = true;
                    walk(init);
                    state.inside_structured_value = old;
                }
                else if (symbol.get_type().is_array()
                        && init.is<Nodecl::StructuredValue>())
                {
                    // int c[] = {1, 2, 3};
                    *(file) << " = ";
                    walk(init);
                }
                else if (symbol.is_member()
                        && (symbol.is_static() || symbol.is_defined_inside_class())
                        && state.in_member_declaration)
                {
                    // This is an in member declaration initialization
                    if (IS_CXX11_LANGUAGE
                            && nodecl_calls_to_constructor_indirectly(init))
                    {
                        *(file) << " { ";
                        Nodecl::List constructor_args = nodecl_calls_to_constructor_get_arguments(init);
                        walk(constructor_args[0]);
                        *(file) << " }";
                    }
                    else
                    {
                        *(file) << " = ";
                        walk(init);
                    }
                }
                else if (state.in_condition)
                {
                    // This is something like if (bool foo = expression)
                    *(file) << " = ";
                    walk(init);
                }
                // Workaround for g++ <=4.5, >=4.7 (4.6 seems to work fine, but we'll do it anyways)
                else if (nodecl_expr_is_value_dependent(init.get_internal_nodecl())
                        && symbol.get_type().is_integral_type())
                {
                    *(file) << " = ";
                    walk(init);
                }
                else
                {
                    if (nodecl_calls_to_constructor(init))
                    {
                        Nodecl::List constructor_args = nodecl_calls_to_constructor_get_arguments(init);
                        bool is_default_init = nodecl_calls_to_constructor_default_init(init);
                        if (is_default_init)
                        {
                            *file << start_inline_comment();
                        }

                        std::string lparen, rparen;
                        if (nodecl_calls_to_constructor_default_init_braced(init))
                        {
                            lparen = "{";
                            rparen = "}";
                        }
                        else
                        {
                            // Here we add extra parentheses lest the direct-initialization looked like
                            // as a function declarator (faced with this ambiguity, C++ chooses the latter!)
                            //
                            // A x( (A()) ); cannot become A x( A() ); because it would declare 'x' as a
                            // "function (pointer to function() returning A) returning A"
                            // [extra blanks added for clarity in the example above]
                            lparen = "(";
                            rparen = ")";
                        }

                        *(file) << lparen;
                        walk_initializer_list(constructor_args, ", ");
                        *(file) << rparen;

                        if (is_default_init)
                        {
                            *file << end_inline_comment();
                        }
                    }
                    else if (nodecl_is_parenthesized_explicit_type_conversion(init)
                            || nodecl_calls_to_constructor_indirectly(init))
                    {
                        // Same reason above
                        *file << "((";
                        walk(init);
                        *file << "))";
                    }
                    else
                    {
                        *file << "(";
                        walk(init);
                        *file << ")";
                    }
                }
            }
        }
        pop_scope();
    }
}


std::string CxxBase::define_or_declare_variable_get_name_variable(TL::Symbol& symbol)
{
    // Unnamed bitfields do not have a visible name
    if (symbol.is_unnamed_bitfield())
        return "";

    bool has_been_declared = (get_codegen_status(symbol) == CODEGEN_STATUS_DECLARED
            || get_codegen_status(symbol) == CODEGEN_STATUS_DEFINED);

    // If this symbol is a static member but its class is not being emitted,
    // then it has actually (possibly implicitly) been declared
    has_been_declared = has_been_declared
        || (symbol.is_member()
                && symbol.is_static()
                && state.classes_being_defined.empty());

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
                // FIXME -> || !is_local_symbol(symbol))
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

        *(file) <<  ", " << declarator;

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
    std::string std_attributes;
    std::string declarator;
    std::string bit_field;

    move_to_namespace_of_symbol(symbol);

    bool requires_extern_linkage = false;
    if (IS_CXX_LANGUAGE
            || cuda_emit_always_extern_linkage())
    {
        requires_extern_linkage = (!symbol.is_member()
                && symbol.has_nondefault_linkage());

        if (requires_extern_linkage)
        {
            *(file) << "extern " + symbol.get_linkage() + "\n";
            indent();
            *(file) << "{\n";

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

    else if (symbol.is_extern() ||
            (!symbol.is_member() && !is_definition))
    {
        decl_specifiers += "extern ";
    }

    if (symbol.is_thread())
    {
        decl_specifiers += "__thread ";
    }
    if (symbol.is_thread_local())
    {
        decl_specifiers += "thread_local ";
    }
    if (symbol.is_mutable())
    {
        decl_specifiers += "mutable ";
    }
    if (symbol.is_register())
    {
        decl_specifiers += "register ";
    }
    if (symbol.is_constexpr())
    {
        decl_specifiers += "constexpr ";
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


    // Generate the template headers if needed
    CXX_LANGUAGE()
    {
        if (symbol.is_member()
                && !symbol.is_defined_inside_class()
                && state.classes_being_defined.empty())
        {
            TL::Type class_type = symbol.get_class_type();
            TL::TemplateParameters template_parameters = symbol.get_scope().get_template_parameters();

            if (!(class_type.class_type_is_complete_independent()
                        || class_type.class_type_is_incomplete_independent()))
            {
                codegen_template_headers_all_levels(template_parameters, false);
            }
            else
            {
                while (template_parameters.is_valid())
                {
                    indent();
                    *(file) << "template <>\n";
                    template_parameters = template_parameters.get_enclosing_parameters();
                }

            }
        }
    }

    declarator = this->get_declaration(symbol.get_type(),
            symbol.get_scope(),
            variable_name);

    if (symbol.has_gcc_attributes())
    {
        if (CURRENT_CONFIGURATION->xl_compatibility)
        {
            gcc_attributes = " " + gcc_attributes_to_str(symbol);
        }
        else
        {
            gcc_attributes = gcc_attributes_to_str(symbol) + " ";
        }
    }
    if (symbol.has_alignas())
    {
        std_attributes = alignas_attributes_to_str(symbol) + " ";
    }

    std::string gcc_extension;
    if (symbol.has_gcc_extension())
    {
        gcc_extension = "__extension__ ";
    }

    if (!state.in_condition
            && !state.in_for_stmt_decl)
        indent();

    if (_emit_saved_variables_as_unused
            && symbol.is_saved_expression())
    {
        gcc_attributes += "__attribute__((unused)) ";
    }

    std::string virt_specifiers;

    if (symbol.is_member())
    {
        if (symbol.is_explicit_override())
        {
            virt_specifiers += " override";
        }
        if (symbol.is_final())
        {
            virt_specifiers += " final";
        }
    }

    if (CURRENT_CONFIGURATION->xl_compatibility)
    {
        // IBM XL C/C++ only understands attributes before the initializer...
        *(file) << gcc_extension << decl_specifiers << std_attributes << declarator << gcc_attributes << virt_specifiers << bit_field;
    }
    else
    {
        *(file) << gcc_extension << decl_specifiers << gcc_attributes << std_attributes << declarator << virt_specifiers << bit_field;
    }

    define_or_declare_variable_emit_initializer(symbol, is_definition);

    if (!state.in_condition
            && !state.in_for_stmt_decl)
    {
        *(file) << ";\n";
    }

    if (IS_CXX_LANGUAGE
            || cuda_emit_always_extern_linkage())
    {
        if (requires_extern_linkage)
        {
            dec_indent();
            indent();
            *(file) << "}\n";
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
        if (this->get_current_scope().get_decl_context()->current_scope != symbol.get_scope().get_decl_context()->current_scope)
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

    // We only emit members of classes currently being emitted
    if (symbol.is_member()
            && (state.classes_being_defined.empty()
                || state.classes_being_defined.back() != symbol.get_class_type().get_symbol())
            // but some declarations of members happen at non-class scope, and these have
            // to be emitted always
            && (scope == NULL
                || !scope->is_namespace_scope()))
        return;

    // Do nothing if already defined
    if (get_codegen_status(symbol) == CODEGEN_STATUS_DEFINED
            // It is a symbol that will be object-inited
            || state.must_be_object_init.find(symbol) != state.must_be_object_init.end())
        return;

    // We only generate user declared code
    if (!symbol.is_user_declared())
        return;

    emit_line_marker(symbol.get_locus());

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
            *(file) << gcc_extension
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
            *(file) << symbol.get_name() << "\n";
            indent();
            *(file) << "{\n";
        }
        CXX_LANGUAGE()
        {

            indent();
            *(file) << "enum ";
            if (is_scoped_enum_type(symbol.get_type().get_internal_type()))
            {
                *(file) << "struct ";
            }
            *(file) << symbol.get_name();
            if (enum_type_get_underlying_type_is_fixed(symbol.get_type().get_internal_type()))
            {
                *(file)
                    << " : "
                    << print_type_str(symbol.get_type().get_enum_underlying_type().get_internal_type(),
                            symbol.get_scope().get_decl_context(),
                            /* we need to store the current codegen */ (void*) this)
                    << " "
                    ;
            }
            *(file) << "\n";
            indent();
            *(file) << "{\n";
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
                *(file) << ",\n";
            }
            indent();
            *(file) << enumerator.get_name();

            if (!enumerator.get_value().is_null())
            {
                *(file) << " = ";
                walk(enumerator.get_value());
            }
            pop_scope();
        }

        dec_indent();

        *(file) << "\n";
        indent();
        *(file) << "};\n";
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
        TL::Symbol aliased_namespace = symbol.get_related_scope().get_related_symbol();

        if (aliased_namespace != symbol)
        {
            // Make sure the target namespace has been defined
            do_define_symbol(aliased_namespace,
                    decl_sym_fun,
                    def_sym_fun);

            move_to_namespace_of_symbol(symbol);
            // This is a namespace alias
            indent();
            (*file) << "namespace " << symbol.get_name() << " = " << this->get_qualified_name(aliased_namespace) << ";\n";
        }
        else
        {
            move_to_namespace_of_symbol(symbol);
            indent();
            *(file)
                << (symbol.is_inline() ? "inline " : "")
                << "namespace "
                << symbol.get_name() << " { }\n";
        }

    }
    else if (symbol.is_template_parameter())
    {
        // Do nothing
    }
    else if (symbol.is_template_alias())
    {
        move_to_namespace_of_symbol(symbol);

        TL::TemplateParameters template_parameters = symbol.get_scope().get_template_parameters();
        codegen_template_header(template_parameters, /* show_default_values */ true);

        indent();
        *(file) << "using " << symbol.get_name() << " = ";
        TL::Type type = symbol.get_type();
        *(file) << this->get_declaration(type, symbol.get_scope(),  "");
        *(file) << ";\n";
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
        if (this->get_current_scope().get_decl_context()->current_scope != symbol.get_scope().get_decl_context()->current_scope)
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
            *(file) << symbol.get_name() << ";\n";
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

                is_primary_template = primary_symbol == symbol;
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
                    *(file) << "template <>\n";
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
                std::string gcc_attributes;
                if (symbol.has_gcc_attributes())
                {
                    gcc_attributes = gcc_attributes_to_str(symbol) + " ";
                }

                indent();
                *(file) << class_key << " " << gcc_attributes << symbol.get_name();

                if (is_template_specialized
                        && !is_primary_template)
                {
                    *(file) << get_template_arguments_str(symbol.get_internal_symbol(), symbol.get_scope().get_decl_context());
                }
                *(file) << ";\n";
            }
        }
    }
    else if (symbol.is_enumerator())
    {
        (this->*decl_sym_fun)(symbol.get_type().get_symbol());
    }
    else if (symbol.is_enum())
    {
        if (IS_C_LANGUAGE)
        {
            indent();
            // It already contains "enum"
            (*file) << symbol.get_name() << ";\n";
        }
        else if (IS_CXX03_LANGUAGE
                || (IS_CXX11_LANGUAGE
                    && !enum_type_get_underlying_type_is_fixed(symbol.get_type().get_internal_type())))
        {
            // This should not have happened, emit a definition and hope for
            // the best
            (this->*def_sym_fun)(symbol);
        }
        else if (IS_CXX11_LANGUAGE)
        {
            indent();
            (*file) << "enum "
                << (is_scoped_enum_type(symbol.get_type().get_internal_type()) ? "struct " : "")
                << symbol.get_name()
                << " : "
                << this->get_declaration(
                        enum_type_get_underlying_type(
                            symbol.get_type().get_internal_type()),
                        symbol.get_scope(),
                        "")
                << ";\n";
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
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

        bool is_template_function = false;
        bool is_primary_template = false;
        bool member_of_explicit_template_class = false;

        bool requires_extern_linkage = false;
        if (IS_CXX_LANGUAGE
                || cuda_emit_always_extern_linkage())
        {
            move_to_namespace_of_symbol(symbol);

            requires_extern_linkage = (!symbol.is_member()
                    && symbol.has_nondefault_linkage());

            if (requires_extern_linkage)
            {
                *(file) << "extern " + symbol.get_linkage() + "\n";
                indent();
                *(file) << "{\n";

                inc_indent();
            }
        }

        CXX_LANGUAGE()
        {
            TL::TemplateParameters template_parameters =
                (scope != NULL) ? scope->get_template_parameters() : symbol.get_scope().get_template_parameters();
            if (symbol.get_type().is_template_specialized_type())
            {
                is_template_function = true;

                TL::Type template_type = symbol.get_type().get_related_template_type();
                TL::Type primary_template = template_type.get_primary_template();
                TL::Symbol primary_symbol = primary_template.get_symbol();
                (this->*decl_sym_fun)(primary_symbol);

                if (primary_symbol == symbol)
                {
                    is_primary_template = true;
                }
                if (symbol.is_member())
                {
                    if (scope != NULL
                            && scope->is_namespace_scope())
                    {
                        codegen_template_headers_all_levels(template_parameters,
                                /* show_default_values */ is_primary_template);
                    }
                    else
                    {
                        codegen_template_headers_bounded(template_parameters,
                                symbol.get_class_type().get_symbol().get_scope().get_template_parameters(),
                                /* show_default_values */ is_primary_template);
                    }
                }
                else
                {
                    codegen_template_headers_all_levels(template_parameters,
                            /* show_default_values */ is_primary_template);
                }
            }
            else if (symbol.is_member())
            {
                if (scope != NULL
                        && scope->is_namespace_scope())
                {
                    if (symbol.is_defaulted())
                    {
                        // Special case for dependent defaulted special members
                        // defined out of the class specifier
                        //
                        // template <typename T>
                        // struct A
                        // {
                        //    A();
                        // };
                        //
                        // template <typename T>   // <-- we have to emit this
                        // A<T>::A() = default;
                        codegen_template_headers_all_levels(template_parameters,
                                /* show_default_values */ false);
                    }
                    else
                    {
                        // We may need zero or more empty template headers
                        TL::TemplateParameters tpl = template_parameters;
                        while (tpl.is_valid())
                        {
                            if (!tpl.get_is_explicit_instantiation()
                                    && tpl.get_is_explicit_specialization())
                            {
                                indent();
                                *(file) << "template <>\n";
                                member_of_explicit_template_class = true;
                            }
                            tpl = tpl.get_enclosing_parameters();
                        }
                    }
                }
            }
        }

        int num_parameters = symbol.get_related_symbols().size();
        TL::ObjectList<std::string> parameter_names(num_parameters);
        TL::ObjectList<std::string> parameter_attributes(num_parameters);
        fill_parameter_names_and_parameter_attributes(symbol, parameter_names, parameter_attributes,
                // We want default arguments if this is a primary template
                is_primary_template
                // otherwise we want them only for nontemplate functions
                || (!is_template_function && !member_of_explicit_template_class)
                );

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
        if (symbol.is_virtual()
                && !state.classes_being_defined.empty()
                && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())
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
                && !state.classes_being_defined.empty()
                && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())
        {
            decl_spec_seq += "explicit ";
        }

        if (symbol.is_constexpr())
        {
            decl_spec_seq += "constexpr ";
        }

        if (symbol.is_nested_function())
        {
            decl_spec_seq += "auto ";
        }

        std::string gcc_attributes = gcc_attributes_to_str(symbol);
        if (gcc_attributes != "")
            gcc_attributes = " " + gcc_attributes;
        std::string asm_specification = gcc_asm_specifier_to_str(symbol);

        TL::Type real_type = symbol.get_type();

        std::string trailing_type_specifier;
        if (symbol.get_type().is_trailing_return())
        {
            trailing_type_specifier = " -> ";
            trailing_type_specifier += print_type_str(
                    symbol.get_type().returns().get_internal_type(),
                    symbol.get_scope().get_decl_context(),
                    (void*) this);
        }

        if (symbol.is_conversion_function()
                || symbol.is_destructor())
        {
            // FIXME - Use TL::Type to build this type
            real_type = get_new_function_type(NULL, NULL, 0, REF_QUALIFIER_NONE);

            if (symbol.is_conversion_function())
            {
                real_type = real_type.get_as_qualified_as(symbol.get_type());
            }
        }

        if (real_type.is_trailing_return())
        {
            // Transform 'f() -> T' into 'auto f()' and then we will add '-> T'
            real_type = function_type_replace_return_type(real_type.get_internal_type(), get_auto_type());
        }

        // We do not want typedefs in function declarations
        // because they break the syntax
        real_type = real_type.advance_over_typedefs();

        std::string function_name = unmangle_symbol_name(symbol);
        if  (!symbol.is_member())
        {
            function_name = unmangle_symbol_name(symbol);
        }
        else
        {
            if (!state.classes_being_defined.empty()
                    && state.classes_being_defined.back() == symbol.get_class_type().get_symbol())
            {
                function_name = unmangle_symbol_name(symbol);
            }
            else
            {
                function_name = symbol.get_class_qualification(symbol.get_scope(), /* without_template */ true);
            }
        }

        if (symbol.get_type().is_template_specialized_type()
                // Conversions do not allow templates
                && !is_primary_template
                && !symbol.is_conversion_function())
        {
            function_name += template_arguments_to_str(symbol);
        }

        std::string declarator = "";
        std::string pure_spec = "";
        declarator = this->get_declaration_with_parameters(real_type, symbol.get_scope(),
                function_name,
                parameter_names,
                parameter_attributes);


        if (symbol.is_virtual()
                && symbol.is_pure())
        {
            pure_spec += " = 0 ";
        }
        else if (symbol.is_deleted())
        {
            pure_spec += " = delete ";
        }
        else if (symbol.is_defaulted()
                // Must be a special member
                && symbol.is_member())
        {
            if (symbol.is_defined_inside_class() // (A)
                    || (scope != NULL
                        && scope->is_namespace_scope()) // (B)
               )
            {
                // (A) Defaulted inside the class specifier
                // (B) Defaulted but not inside the class specifier. But we are not inside the
                // class specifier either.
                pure_spec += " = default ";
            }
            else
            {
                // (C) Not defined inside class but we are inside the class
                // specifier, let's pretend we did not declare it so a later
                // CxxDecl will go through (B)
                set_codegen_status(symbol, CODEGEN_STATUS_NONE);
            }
        }

        std::string exception_spec = exception_specifier_to_str(symbol);

        std::string virt_specifiers;

        if (symbol.is_member())
        {
            if (symbol.is_explicit_override())
            {
                virt_specifiers += " override";
            }
            if (symbol.is_final())
            {
                virt_specifiers += " final";
            }
        }

        indent();
        if (CURRENT_CONFIGURATION->xl_compatibility)
        {
            // IBM XL requires asm_specification after the attributes, just the opposite
            // as GCC
            *(file) << decl_spec_seq << declarator << exception_spec << virt_specifiers
                << gcc_attributes << asm_specification << pure_spec << trailing_type_specifier << ";\n";
        }
        else
        {
            *(file) << decl_spec_seq << declarator << exception_spec << virt_specifiers
                    << asm_specification << gcc_attributes << pure_spec << trailing_type_specifier << ";\n";
        }

        if (IS_CXX_LANGUAGE
                || cuda_emit_always_extern_linkage())
        {
            if (requires_extern_linkage)
            {
                dec_indent();
                indent();
                *(file) << "}\n";
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

bool CxxBase::is_friend_of_class(TL::Symbol sym, TL::Symbol class_sym)
{
    ERROR_CONDITION(!class_sym.is_class(), "Invalid symbol", 0);

    TL::ObjectList<TL::Symbol> friends = class_sym.get_type().class_get_friends();

    if (sym.is_dependent_friend_function()
            || sym.is_dependent_friend_class())
        return friends.contains(sym);
    else
        return friends.map<TL::Symbol>(&TL::Symbol::get_alias_to).contains(sym);
}

void CxxBase::define_generic_entities(Nodecl::NodeclBase node,
        void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
        void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
        void (CxxBase::*define_entities_fun)(const Nodecl::NodeclBase& node),
        void (CxxBase::*define_entry_fun)(
            const Nodecl::NodeclBase &node, TL::Symbol entry,
            void (CxxBase::*def_sym_fun_2)(TL::Symbol symbol)),
        std::set<TL::Symbol>& visited_symbols
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
                    define_entry_fun,
                    visited_symbols
                    );
        }
    }
    else
    {
        Nodecl::NodeclBase::Children children = node.children();
        for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                it != children.end();
                it++)
        {
            define_generic_entities(
                    *it,
                    decl_sym_fun,
                    def_sym_fun,
                    define_entities_fun,
                    define_entry_fun,
                    visited_symbols
                    );
        }

        TL::Symbol entry = node.get_symbol();
        if (entry.is_valid()
                && visited_symbols.find(entry) == visited_symbols.end()
                && entry.get_type().is_valid()
                && state.walked_symbols.find(entry) == state.walked_symbols.end())
        {
            state.walked_symbols.insert(entry);
            visited_symbols.insert(entry);

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
                    define_entry_fun,
                    visited_symbols
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
    if (this->get_current_scope().get_decl_context()->current_scope
            == entry.get_scope().get_decl_context()->current_scope)
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
    static std::set<TL::Symbol> visited_symbols;
    static int nesting = 0;
    nesting++;

    define_generic_entities(node,
            &CxxBase::declare_symbol_always,
            &CxxBase::define_symbol_always,
            &CxxBase::define_all_entities_in_trees,
            &CxxBase::entry_just_define,
            visited_symbols);

    nesting--;
    if (nesting == 0)
        visited_symbols.clear();
}

void CxxBase::define_nonlocal_entities_in_trees(const Nodecl::NodeclBase& node)
{
    static std::set<TL::Symbol> visited_symbols;
    static int nesting = 0;
    nesting++;

    define_generic_entities(node,
            &CxxBase::declare_symbol_if_nonlocal,
            &CxxBase::define_symbol_if_nonlocal,
            &CxxBase::define_nonlocal_entities_in_trees,
            &CxxBase::entry_just_define,
            visited_symbols);

    nesting--;
    if (nesting == 0)
        visited_symbols.clear();
}

void CxxBase::define_nonprototype_entities_in_trees(const Nodecl::NodeclBase& node)
{
    static std::set<TL::Symbol> visited_symbols;
    static int nesting = 0;
    nesting++;

    define_generic_entities(node,
            &CxxBase::declare_symbol_if_nonprototype,
            &CxxBase::define_symbol_if_nonprototype,
            &CxxBase::define_nonprototype_entities_in_trees,
            &CxxBase::entry_just_define,
            visited_symbols);

    nesting--;
    if (nesting == 0)
        visited_symbols.clear();
}

void CxxBase::define_nonlocal_nonprototype_entities_in_trees(const Nodecl::NodeclBase& node)
{
    static std::set<TL::Symbol> visited_symbols;
    static int nesting = 0;
    nesting++;

    define_generic_entities(node,
            &CxxBase::declare_symbol_if_nonlocal_nonprototype,
            &CxxBase::define_symbol_if_nonlocal_nonprototype,
            &CxxBase::define_nonlocal_nonprototype_entities_in_trees,
            &CxxBase::entry_just_define,
            visited_symbols);

    nesting--;
    if (nesting == 0)
        visited_symbols.clear();
}

void CxxBase::define_local_entities_in_trees(const Nodecl::NodeclBase& node)
{
    static std::set<TL::Symbol> visited_symbols;
    static int nesting = 0;
    nesting++;

    define_generic_entities(node,
            &CxxBase::declare_symbol_if_local,
            &CxxBase::define_symbol_if_local,
            &CxxBase::define_local_entities_in_trees,
            &CxxBase::entry_local_definition,
            visited_symbols);

    nesting--;
    if (nesting == 0)
        visited_symbols.clear();
}

void CxxBase::walk_mask_type(TL::Type t,
        void (CxxBase::* symbol_to_declare)(TL::Symbol),
        void (CxxBase::* symbol_to_define)(TL::Symbol),
        void (CxxBase::* define_entities_in_tree)(const Nodecl::NodeclBase&),
        bool needs_definition)
{
        if (CURRENT_CONFIGURATION->enable_intel_vector_types)
        {
            TL::Symbol mask_sym;
            switch (8 * t.get_size())
            {
                case 16:
                    mask_sym = TL::Scope::get_global_scope().get_symbol_from_name("__mmask16");
                    break;
                case 8:
                    mask_sym = TL::Scope::get_global_scope().get_symbol_from_name("__mmask8");
                    break;
                default:
                    break;
            }

            if (mask_sym.is_valid()
                    && mask_sym.is_typedef())
            {
                walk_type_for_symbols(
                        get_user_defined_type(mask_sym.get_internal_symbol()),
                        symbol_to_declare,
                        symbol_to_define,
                        define_entities_in_tree);
            }
        }
}

void CxxBase::walk_vector_type(TL::Type t,
        void (CxxBase::* symbol_to_declare)(TL::Symbol),
        void (CxxBase::* symbol_to_define)(TL::Symbol),
        void (CxxBase::* define_entities_in_tree)(const Nodecl::NodeclBase&),
        bool needs_definition)
{
    bool done = false;
    // FIXME: Move this out of here
    if (CURRENT_CONFIGURATION->enable_intel_vector_types)
    {
        scope_entry_t* intel_typedef = vector_type_get_intel_vector_typedef(t.get_internal_type());
        if (intel_typedef != NULL)
        {
            walk_type_for_symbols(
                    get_user_defined_type(intel_typedef),
                    symbol_to_declare,
                    symbol_to_define,
                    define_entities_in_tree);
            done = true;
        }
    }

    if (!done)
    {
        walk_type_for_symbols(t.vector_element(), symbol_to_declare, symbol_to_define, define_entities_in_tree);
    }
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
    else if (t.is_vector()
            && !is_intel_vector_struct_type(t.get_internal_type(), NULL))
    {
        walk_vector_type(t,
                symbol_to_declare,
                symbol_to_define,
                define_entities_in_tree,
                needs_definition);
    }
    else if (t.is_mask())
    {
        walk_mask_type(t,
                symbol_to_declare,
                symbol_to_define,
                define_entities_in_tree,
                needs_definition);
    }
    else if (t.is_named_class()
                // Anonymous unions are handled as unnamed classes
                && !t.get_symbol().is_anonymous_union())
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
    else if (t.is_unnamed_class()
            // Anonymous unions are handled as unnamed classes
            || (t.is_named_class()
                && t.get_symbol().is_anonymous_union()))
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
                namespace_sym->decl_context->current_scope->related_entry,
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
        *(file) << "}\n";
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
        if (symbol_entity_specs_get_num_gcc_attributes(namespace_nesting_to[i]) > 0)
        {
            gcc_attributes =
                " " + gcc_attributes_to_str(namespace_nesting_to[i]);
        }

        indent();
        *(file)
            << (symbol_entity_specs_get_is_inline(namespace_nesting_to[i]) ? "inline " : "")
            << "namespace "
            << real_name << gcc_attributes << " {\n";
        if ((i + 1) < num_from)
        {
            *(file) << " ";
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
    scope_t* enclosing_namespace = symbol.get_internal_symbol()->decl_context->namespace_scope;
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
        *(file) << "  ";
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
                *(file) << start_inline_comment();
            }
            current_node = current_node.as<Nodecl::DefaultArgument>().get_argument();
        }

        if (it != begin)
            *(file) << separator;

        walk(current_node);

        it++;
    }

    if (default_argument)
        *(file) << end_inline_comment();
}

bool CxxBase::looks_like_braced_list(Nodecl::NodeclBase n)
{
    n = no_conv(n);

    if (n.is<Nodecl::CxxBracedInitializer>())
        return true;

    if (n.is<Nodecl::StructuredValue>()
            && n.as<Nodecl::StructuredValue>().get_form().is<Nodecl::StructuredValueBracedImplicit>())
        return true;

    if (n.is<Nodecl::FunctionCall>()
            && n.as<Nodecl::FunctionCall>().get_function_form().is<Nodecl::CxxFunctionFormImplicit>()
            && !n.as<Nodecl::FunctionCall>().get_arguments().is_null())
    {
        return looks_like_braced_list(n.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>()[0]);
    }

    return false;
}

void CxxBase::walk_initializer_list(const Nodecl::List& list, const std::string& separator)
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
                *(file) << start_inline_comment();
            }
            current_node = current_node.as<Nodecl::DefaultArgument>().get_argument();
        }

        if (it != begin)
            *(file) << separator;

        bool emit_parentheses = !looks_like_braced_list(*it);
        if (emit_parentheses)
            *(file) << "(";

        walk(current_node);

        if (emit_parentheses)
            *(file) << ")";

        it++;
    }

    if (default_argument)
        *(file) << end_inline_comment();
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
            *(file) << ", ";
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
        else if (operator_name == "&=")  return NODECL_BITWISE_AND_ASSIGNMENT;
        else if (operator_name == "|=")  return NODECL_BITWISE_OR_ASSIGNMENT;
        else if (operator_name == "^=")  return NODECL_BITWISE_XOR_ASSIGNMENT;
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

        case NODECL_CXX_LAMBDA:
        case NODECL_CXX_DEP_NAME_SIMPLE:
        case NODECL_CXX_DEP_NAME_CONVERSION:
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

        case NODECL_CXX_CLASS_MEMBER_ACCESS:
        case NODECL_CXX_ARROW:
        case NODECL_CXX_POSTFIX_INITIALIZER:
        case NODECL_CXX_EXPLICIT_TYPE_CAST:
        case NODECL_CXX_DEP_FUNCTION_CALL:

        case NODECL_VECTOR_SUBSCRIPT:
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
        case NODECL_CXX_CAST:
        case NODECL_CONVERSION:
            {
                if (text == "C")
                {
                    return -4;
                }
                else if (text != "")
                {
                    // These casts are postfix expressions actually
                    // static_cast, dynamic_cast, reinterpret_cast, const_cast
                    return -2;
                }
                /* fall-through when text == "" */
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
        case NODECL_THROW:
        case NODECL_CONDITIONAL_EXPRESSION:
            return -16;
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

int CxxBase::get_rank(Nodecl::NodeclBase n)
{
    n = no_conv(advance_implicit_function_calls(no_conv(n)));

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

static char is_relational_operator(node_t n)
{
    return n == NODECL_EQUAL
        || n == NODECL_DIFFERENT
        || n == NODECL_LOWER_THAN
        || n == NODECL_LOWER_OR_EQUAL_THAN
        || n == NODECL_GREATER_THAN
        || n == NODECL_GREATER_OR_EQUAL_THAN;
}

bool CxxBase::same_operation(Nodecl::NodeclBase current_operator, Nodecl::NodeclBase operand)
{
    current_operator = no_conv(current_operator);
    operand = no_conv(operand);

    int rank_current = get_rank(current_operator);
    int rank_operand = get_rank(operand);

    return (rank_current == rank_operand);
}

bool CxxBase::operand_has_lower_priority(Nodecl::NodeclBase current_operator, Nodecl::NodeclBase operand)
{
    current_operator = no_conv(current_operator);
    operand = no_conv(operand);

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
            // a #1 b #2 c -> (a #1 b) #2 c   [where #1 and #2 are ==, <, <=, >=, >, !=]
            || (is_relational_operator(current_kind) && is_relational_operator(operand_kind))
            // a #1 b #2 c -> (a #1 b) #2 c   [where #1 is |, &  #2 is ==, <, <=, >=, >, !=]
            || (is_bitwise_bin_operator(current_kind) && is_relational_operator(operand_kind))
            )
    {
        return 1;
    }

    return rank_operand < rank_current;
}

std::string CxxBase::quote_c_string(int* c, int length, const std::string& prefix)
{
    std::string result;
    result += prefix;

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
            if (current < 256)
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

bool CxxBase::nodecl_is_parenthesized_explicit_type_conversion(Nodecl::NodeclBase node)
{
    return node.is<Nodecl::CxxExplicitTypeCast>()
        && !node.as<Nodecl::CxxExplicitTypeCast>().get_init_list().is_null()
        && node.as<Nodecl::CxxExplicitTypeCast>().get_init_list().is<Nodecl::CxxParenthesizedInitializer>();
}

bool CxxBase::nodecl_calls_to_constructor_indirectly(Nodecl::NodeclBase node)
{
    return nodecl_calls_to_constructor(node)
        || (node.is<Nodecl::FunctionCall>()
                && node.as<Nodecl::FunctionCall>().get_function_form().is<Nodecl::CxxFunctionFormImplicit>()
                && !node.as<Nodecl::FunctionCall>().get_arguments().is_null()
                && nodecl_calls_to_constructor_indirectly(
                    node.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>()[0]));
}


Nodecl::List CxxBase::nodecl_calls_to_constructor_get_arguments(Nodecl::NodeclBase node)
{
    node = no_conv(node);

    ERROR_CONDITION(!node.is<Nodecl::FunctionCall>(), "Invalid node", 0);

    return node.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>();
}

bool CxxBase::nodecl_calls_to_constructor(Nodecl::NodeclBase node)
{
    node = no_conv(node);

    if (node.is<Nodecl::FunctionCall>())
    {
        TL::Symbol called_sym = node.as<Nodecl::FunctionCall>().get_called().get_symbol();

        return (called_sym.is_valid()
                && called_sym.is_constructor());
    }

    return 0;
}

bool CxxBase::nodecl_calls_to_constructor_default_init(Nodecl::NodeclBase node)
{
    node = no_conv(node);

    return (node.is<Nodecl::FunctionCall>()
            && node.as<Nodecl::FunctionCall>().get_function_form().is<Nodecl::CxxFunctionFormDefaultInit>());
}

bool CxxBase::nodecl_calls_to_constructor_default_init_braced(Nodecl::NodeclBase node)
{
    node = no_conv(node);

    return (node.is<Nodecl::FunctionCall>()
            && node.as<Nodecl::FunctionCall>().get_function_form().is<Nodecl::CxxFunctionFormDefaultInitBraced>());
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
    if (template_parameters.get_is_explicit_instantiation())
    {
        return;
    }
    else if (template_parameters.get_is_explicit_specialization())
    {
        *(file) << "template <>";
        if (endline)
            *(file) << "\n";
        return;
    }

    *(file) << "template < ";
    for (int i = 0; i < template_parameters.get_num_parameters(); i++)
    {
        std::pair<TL::Symbol,
                  TL::TemplateParameters::TemplateParameterKind>
                      tpl_param = template_parameters.get_parameter_num(i);
        TL::Symbol symbol = tpl_param.first;

        if (i != 0)
        {
            *(file) << ", ";
        }

        switch (tpl_param.second)
        {
            case TPK_TYPE:
                {
                    *(file) << "typename " << symbol.get_name();
                    break;
                }
            case TPK_TYPE_PACK:
                {
                    *(file) << "typename ..." << symbol.get_name();
                    break;
                }
            case TPK_NONTYPE:
                {
                    std::string declaration = this->get_declaration(symbol.get_type(),
                            symbol.get_scope(),
                            symbol.get_name());

                    *(file) << declaration;
                    break;
                }
            case TPK_NONTYPE_PACK:
                {
                    std::string declaration = this->get_declaration(symbol.get_type(),
                            symbol.get_scope(),
                            // this is a bit puny but will do
                            "... " + symbol.get_name());

                    *(file) << declaration;
                    break;
                }
            case TPK_TEMPLATE:
                {
                    TL::Type template_type = symbol.get_type();
                    codegen_template_header(
                            symbol.get_type().template_type_get_template_parameters(),
                            show_default_values,
                            /* endline */ false);
                    *(file) << " class " << symbol.get_name();
                    break;
                }
            case TPK_TEMPLATE_PACK:
                {
                    TL::Type template_type = symbol.get_type();
                    codegen_template_header(
                            symbol.get_type().template_type_get_template_parameters(),
                            show_default_values,
                            /* endline */ false);
                    *(file) << " class ..." << symbol.get_name();
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
                *(file) << " = ";
                switch (tpl_param.second)
                {
                    case TPK_TYPE:
                    case TPK_TEMPLATE:
                        {
                            TL::Type temp_arg_type = temp_arg.get_type();
                            *(file) <<
                                this->print_type_str(
                                        temp_arg_type.get_internal_type(),
                                        symbol.get_scope().get_decl_context(),
                                        /* we need to store the current codegen */ (void*) this);
                            break;
                        }
                    case TPK_NONTYPE:
                        {
                            push_scope(symbol.get_scope());
                            bool old_nontype_template_arguments_needs_parentheses = state.nontype_template_argument_needs_parentheses;
                            state.nontype_template_argument_needs_parentheses = 1;
                            walk(temp_arg.get_value());
                            state.nontype_template_argument_needs_parentheses = old_nontype_template_arguments_needs_parentheses;
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

    *(file) << " >";
    if (endline)
        *(file) << "\n";
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

            std::stringstream ss_out;
            std::ostream *tmp_out = &ss_out;

            bool b = this->is_file_output();
            this->set_is_file_output(false);
            std::swap(file, tmp_out);

            walk_expression_list(it->get_expression_list().as<Nodecl::List>());

            std::swap(file, tmp_out);
            this->set_is_file_output(b);

            result += ss_out.str();

            result += ")))";
        }
        attributes_counter++;
    }
    return result;
}

std::string CxxBase::alignas_attributes_to_str(TL::Symbol symbol)
{
    Nodecl::NodeclBase alignas_expr = symbol.get_alignas();

    if (alignas_expr.is_null())
        return "";

    std::stringstream ss;
    std::ostream *tmp_out = &ss;

    bool b = this->is_file_output();
    this->set_is_file_output(false);
    std::swap(file, tmp_out);

    push_scope(symbol.get_scope());

    if (alignas_expr.is<Nodecl::CxxAlignas>())
    {
        Nodecl::List l = alignas_expr.as<Nodecl::CxxAlignas>().get_values().as<Nodecl::List>();
        int i = 0;
        for (Nodecl::List::iterator it = l.begin();
                it != l.end();
                it++, i++)
        {
            if (i > 0)
                ss << " ";
            ss << "alignas(";
            walk(*it);
            ss << ")";
        }
    }
    else
    {
        ss << "alignas(";

        walk(alignas_expr);

        ss << ")";
    }

    pop_scope();

    std::swap(file, tmp_out);
    this->set_is_file_output(b);

    return ss.str();
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

            std::stringstream ss_out;
            std::ostream *tmp_out = &ss_out;

            bool b = this->is_file_output();
            this->set_is_file_output(false);
            std::swap(file, tmp_out);

            walk_expression_list(it->get_expression_list().as<Nodecl::List>());

            std::swap(file, tmp_out);
            this->set_is_file_output(b);

            result += ss_out.str();

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
        std::stringstream ss_out;
        std::ostream *tmp_out = &ss_out;

        bool b = this->is_file_output();
        this->set_is_file_output(false);
        std::swap(file, tmp_out);

        walk(symbol.get_asm_specification());

        std::swap(file, tmp_out);
        this->set_is_file_output(b);

        result = ss_out.str();
    }
    return result;
}

std::string CxxBase::exception_specifier_to_str(TL::Symbol symbol)
{
    std::string exception_spec;
    CXX_LANGUAGE()
    {
        if (!symbol.function_noexcept().is_null())
        {
            exception_spec += " noexcept(";

            State new_state(state);
            new_state._do_not_emit_this = true;

            if (CURRENT_CONFIGURATION->line_markers)
            {
                std::stringbuf strbuf;
                CodegenStreambuf<char> codegen_streambuf(&strbuf, this);
                std::ostream out(&codegen_streambuf);

                push_scope(symbol.get_scope());
                this->set_last_is_newline(false); // we are right after noexcept
                this->codegen(symbol.function_noexcept(), new_state, &out);
                pop_scope();

                exception_spec += strbuf.str();
            }
            else
            {
                std::stringstream ss;

                push_scope(symbol.get_scope());
                this->codegen(symbol.function_noexcept(), new_state, &ss);
                pop_scope();

                exception_spec += ss.str();
            }
            exception_spec += ")";
        }
        else if (!symbol.function_throws_any_exception())
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
    *file << ast_print_node_type(n.get_kind()) << "(";
    Nodecl::NodeclBase::Children children = n.children();
    int i = 0;
    for (Nodecl::NodeclBase::Children::iterator it = children.begin();
            it != children.end();
            it++)
    {
        if (!it->is_null())
        {
            if (i > 0)
                *file << ", ";
            if (it->is<Nodecl::List>())
            {
                Nodecl::List l = it->as<Nodecl::List>();
                *file << "[";
                for (Nodecl::List::iterator it_list = l.begin(); it_list != l.end(); it_list++)
                {
                    walk(*it_list);
                    if (it_list + 1 != l.end())
                    {
                        *file << ", ";
                    }
                }
                *file << "]";
            }
            else
            {
                walk(*it);
            }
            i++;
        }
    }
    *file << ")";
}

const char* CxxBase::print_name_str(scope_entry_t* sym, const decl_context_t* decl_context, void *data)
{
    // We obtain the current codegen from the data
    CxxBase* _this = (CxxBase*) data;

    // The variable data must contain a valid pointer to the current codegen
    ERROR_CONDITION(_this == NULL, "Invalid this", 0);
    ERROR_CONDITION(sym == NULL, "Invalid symbol", 0);

    const char* result = NULL;
    if (IS_CXX_LANGUAGE
            && _this->get_codegen_status(sym) == CODEGEN_STATUS_NONE
            && !_this->symbol_is_nested_in_defined_classes(sym)
            && ((sym->kind == SK_CLASS
                    && !is_template_specialized_type(sym->type_information))
                || sym->kind == SK_ENUM)
            && !symbol_entity_specs_get_is_member(sym))
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
            scope_entry_list_t* entry_list = query_in_scope_str(sym->decl_context, sym->symbol_name, NULL);
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

const char* CxxBase::print_type_str(type_t* t, const decl_context_t* decl_context, void *data)
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
                /* unparenthesize_ptr_operator */ 0,
                print_name_str,
                data);
    }
    return result;
}

std::string CxxBase::get_declaration(TL::Type t, TL::Scope scope, const std::string& name)
{
    t = fix_references(t);

    return get_declaration_string_ex(t.get_internal_type(), scope.get_decl_context(),
            name.c_str(), "", 0, 0, NULL, NULL, /* is_parameter */ 0, /* unparenthesize_ptr_operator */ 0,
            print_name_str, /* we need to store the current codegen */ (void*) this);
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
        TL::ObjectList<std::string>& parameter_attributes,
        bool emit_default_arguments)
{
    ERROR_CONDITION(!symbol.is_function()
            && !symbol.is_dependent_friend_function()
            && !symbol.is_lambda(), "This symbol should be a function\n", -1);

    int i = 0;
    TL::ObjectList<TL::Symbol> related_symbols = symbol.get_related_symbols();
    for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
            it != related_symbols.end();
            it++, i++)
    {
        TL::Symbol current_param = *it;
        if (current_param.is_valid())
        {
            if (!current_param.not_to_be_printed())
            {
                parameter_names[i] = current_param.get_name();
            }

            if (current_param.has_gcc_attributes())
            {
                parameter_attributes[i] = gcc_attributes_to_str(current_param);
            }

            if (emit_default_arguments
                    && get_codegen_status(current_param) != CODEGEN_STATUS_DEFINED
                    && symbol.has_default_argument_num(i)
                    && !symbol.has_hidden_default_argument_num(i))
            {
                TL::Scope scope_of_param = current_param.get_scope();
                if (symbol.is_member())
                {
                    scope_of_param = symbol.get_scope();
                }

                // Note that we add redundant parentheses because of a g++ 4.3 problem
                parameter_attributes[i] += " = (" + this->codegen_to_str(symbol.get_default_argument_num(i), 
                            scope_of_param) + ")";
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
    for (int i = 0; i < orig_size && i < num_parameters; i++)
    {
        parameter_names[i] = uniquestr(parameters[i].c_str());
        param_attributes[i] = uniquestr(parameter_attributes[i].c_str());
    }

    const char* result = get_declaration_string_ex(t.get_internal_type(),
            scope.get_decl_context(), symbol_name.c_str(), "", 0,
            num_parameters, parameter_names, param_attributes,
            /* is_parameter */ 1,
            /* unparenthesize_ptr_operator */ 0,
            print_name_str,
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
    if (!this->is_file_output())
        return t;
    return t.fix_references();
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
            "0").connect(std::bind(&CxxBase::set_emit_saved_variables_as_unused, this, std::placeholders::_1));

    _prune_saved_variables = true;
    register_parameter("prune_saved_variables",
            "Disables removal of unused saved-expression variables. If you need to enable this, please report a ticket",
            _prune_saved_variables_str,
            "1").connect(std::bind(&CxxBase::set_prune_saved_variables, this, std::placeholders::_1));
}

void CxxBase::set_emit_saved_variables_as_unused(const std::string& str)
{
    TL::parse_boolean_option("emit_saved_variables_as_unused", str, _emit_saved_variables_as_unused, "Assuming false.");
}

void CxxBase::set_prune_saved_variables(const std::string& str)
{
    TL::parse_boolean_option("prune_saved_variables", str, _prune_saved_variables, "Assuming true.");
}

std::string CxxBase::start_inline_comment()
{
    if (state._inline_comment_nest++ == 0)
        return " /* ";
    else
        return "";
}

std::string CxxBase::end_inline_comment()
{
    if (--state._inline_comment_nest == 0)
        return " */ ";
    else
        return "";
    ERROR_CONDITION(state._inline_comment_nest < 0, "Wrong nesting of comments", 0);
}


} // Codegen

EXPORT_PHASE(Codegen::CxxBase)
