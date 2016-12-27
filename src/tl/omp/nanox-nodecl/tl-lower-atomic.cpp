/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include "tl-atomics.hpp"

#include "tl-lowering-visitor.hpp"
#include "tl-counters.hpp"

#include "tl-nodecl-utils.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace Nanox {

    namespace {

        std::string nanos_get_api_name(Nodecl::NodeclBase lhs, Nodecl::NodeclBase op)
        {
            std::string result = "nanos_atomic_";

            node_t op_kind = op.get_kind();

            std::map<node_t, std::string> op_names;
            op_names.insert(std::make_pair(NODECL_ADD, "add"));
            op_names.insert(std::make_pair(NODECL_MINUS, "sub"));
            op_names.insert(std::make_pair(NODECL_MUL, "mul"));
            op_names.insert(std::make_pair(NODECL_DIV, "div"));
            op_names.insert(std::make_pair(NODECL_LOGICAL_AND, "land"));
            op_names.insert(std::make_pair(NODECL_LOGICAL_OR, "lor"));
            op_names.insert(std::make_pair(NODECL_EQUAL, "eq"));
            op_names.insert(std::make_pair(NODECL_DIFFERENT, "neq"));
            op_names.insert(std::make_pair(NODECL_FUNCTION_CALL, "__intrin__"));

            std::map<node_t, std::string>::iterator it_1 = op_names.find(op_kind);
            ERROR_CONDITION(it_1 == op_names.end(), "Unhandled op", 0);

            if (it_1->second == "__intrin__")
            {
                std::map<std::string, std::string> intrin_names;
                intrin_names.insert(std::make_pair("max",  "max"));
                intrin_names.insert(std::make_pair("min",  "min"));
                intrin_names.insert(std::make_pair("ieor", "bxor"));
                intrin_names.insert(std::make_pair("ior",  "bor"));
                intrin_names.insert(std::make_pair("iand", "band"));

                TL::Symbol funct_sym = op.as<Nodecl::FunctionCall>().get_called().get_symbol();
                ERROR_CONDITION(!funct_sym.is_valid(), "Symbol is invalid", 0);
                std::string funct_name = funct_sym.get_name();;

                std::map<std::string, std::string>::iterator it_3 = intrin_names.find(funct_name);
                ERROR_CONDITION(it_3 == intrin_names.end(), "Unhandled intrinsic %s", funct_name.c_str());

                result += it_3->second;
            }
            else
            {
                result += it_1->second;
            }

            TL::Type t = lhs.get_type().no_ref();

            std::map<TL::Type, std::string> type_names;
            type_names.insert(std::make_pair(get_signed_char_type(), "schar"));
            type_names.insert(std::make_pair(get_signed_byte_type(), "schar"));
            type_names.insert(std::make_pair(get_bool_type(), "schar"));
            type_names.insert(std::make_pair(get_signed_short_int_type(), "short"));
            type_names.insert(std::make_pair(get_signed_int_type(), "int"));
            type_names.insert(std::make_pair(get_signed_long_int_type(), "long"));
            type_names.insert(std::make_pair(get_signed_long_long_int_type(), "longlong"));
            type_names.insert(std::make_pair(get_unsigned_char_type(), "uchar"));
            type_names.insert(std::make_pair(get_unsigned_byte_type(), "uchar"));
            type_names.insert(std::make_pair(get_unsigned_short_int_type(), "ushort"));
            type_names.insert(std::make_pair(get_unsigned_int_type(), "uint"));
            type_names.insert(std::make_pair(get_unsigned_long_int_type(), "ulong"));
            type_names.insert(std::make_pair(get_unsigned_long_long_int_type(), "ulonglong"));
            type_names.insert(std::make_pair(get_float_type(), "float" ));
            type_names.insert(std::make_pair(get_double_type(), "double" ));
            type_names.insert(std::make_pair(get_long_double_type(), "ldouble" ));
            type_names.insert(std::make_pair(get_complex_type(get_float_type()), "cfloat" ));
            type_names.insert(std::make_pair(get_complex_type(get_double_type()), "cdouble" ));
            type_names.insert(std::make_pair(get_complex_type(get_long_double_type()), "cldouble" ));

            // Fortran logical types
            type_names.insert(std::make_pair(get_bool_of_integer_type(get_signed_byte_type()), "bytebool"));
            type_names.insert(std::make_pair(get_bool_of_integer_type(get_signed_short_int_type()), "shortbool"));
            type_names.insert(std::make_pair(get_bool_of_integer_type(get_signed_int_type()), "intbool"));
            type_names.insert(std::make_pair(get_bool_of_integer_type(get_signed_long_int_type()), "longbool"));
            type_names.insert(std::make_pair(get_bool_of_integer_type(get_signed_long_long_int_type()), "longlongbool"));

            std::map<TL::Type, std::string>::iterator it_2 = type_names.find(t);
            ERROR_CONDITION(it_2 == type_names.end(), "Unhandled type %s", print_declarator(t.get_internal_type()));

            result += "_" + it_2->second;

            return result;
        }

        Nodecl::NodeclBase nanos_api_call(Nodecl::NodeclBase expr)
        {
            Nodecl::NodeclBase result;

            Nodecl::NodeclBase lhs_assig = expr.as<Nodecl::Assignment>().get_lhs();
            Nodecl::NodeclBase rhs_assig = expr.as<Nodecl::Assignment>().get_rhs();

            node_t op_kind = rhs_assig.get_kind();

            Nodecl::NodeclBase lhs, rhs;

            switch (op_kind)
            {
                case NODECL_FUNCTION_CALL:
                    {
                        Source src;

                        Nodecl::List args = rhs_assig.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>();
                        lhs = args[0];
                        rhs = args[1];

                        if (lhs.is<Nodecl::FortranActualArgument>())
                            lhs = lhs.as<Nodecl::FortranActualArgument>().get_argument();
                        if (rhs.is<Nodecl::FortranActualArgument>())
                            rhs = rhs.as<Nodecl::FortranActualArgument>().get_argument();
                    }
                    break;
                case NODECL_ADD:
                case NODECL_MUL:
                case NODECL_MINUS:
                case NODECL_DIV:
                case NODECL_LOGICAL_AND:
                case NODECL_LOGICAL_OR:
                case NODECL_EQUAL:
                case NODECL_DIFFERENT:
                    {
                        lhs = rhs_assig.as<Nodecl::Add>().get_lhs();
                        rhs = rhs_assig.as<Nodecl::Add>().get_rhs();
                    }
                    break;
                default:
                    internal_error("Unhandled case '%s'\n", ast_print_node_type(op_kind));
            }

            Nodecl::NodeclBase value;

            if (Nodecl::Utils::structurally_equal_nodecls(lhs_assig, lhs, /* skip_conversion_nodecls */ true))
                value = rhs;
            else
                value = lhs;

            std::string api_name = nanos_get_api_name(expr, rhs_assig);

            Source src;
            src << api_name << "(" << as_expression(lhs_assig.shallow_copy()) << ", "
                << as_expression(value.shallow_copy()) << ");"
                ;

            if (IS_FORTRAN_LANGUAGE)
            {
                Source::source_language = SourceLanguage::C;
            }
            result = src.parse_statement(expr);
            if (IS_FORTRAN_LANGUAGE)
            {
                Source::source_language = SourceLanguage::Current;
            }

            return result;
        }
    }

    void LoweringVisitor::visit(const Nodecl::OpenMP::Atomic& construct)
    {
        Nodecl::List statements = construct.get_statements().as<Nodecl::List>();

        walk(statements);

        // Get the new statements
        statements = construct.get_statements().as<Nodecl::List>();
        ERROR_CONDITION(!statements.as<Nodecl::List>()[0].is<Nodecl::Context>(), "Invalid node", 0);
        statements = statements.as<Nodecl::List>()[0].as<Nodecl::Context>().get_in_context().as<Nodecl::List>();

        Nodecl::List replacements;

        for (Nodecl::List::iterator it = statements.begin(); it != statements.end(); it++)
        {
            Nodecl::NodeclBase stmt(*it);
            if (!stmt.is<Nodecl::ExpressionStatement>())
            {
                error_printf_at(stmt.get_locus(),
                        "'atomic' directive requires an expression statement\n");
            }
            else
            {
                Nodecl::NodeclBase expr = stmt.as<Nodecl::ExpressionStatement>().get_nest();
                Nodecl::NodeclBase atomic_tree;

                bool using_builtin = false;
                bool using_nanos_api = false;
                if (!allowed_expression_atomic(expr, using_builtin, using_nanos_api))
                {
                    warn_printf_at(expr.get_locus(), "'atomic' expression cannot be implemented efficiently: a critical region will be used instead\n");
                    std::string lock_name = "nanos_default_critical_lock";
                    atomic_tree = emit_critical_region(lock_name, construct, statements);
                }
                else
                {
                    if (using_nanos_api)
                    {
                        atomic_tree = nanos_api_call(expr);
                        info_printf_at(expr.get_locus(), "'atomic' directive implemented using Nanos++ API calls\n");
                    }
                    else if (using_builtin)
                    {
                        atomic_tree = builtin_atomic_int_op(expr);
                        info_printf_at(expr.get_locus(), "'atomic' directive implemented using GCC atomic builtins\n");
                    }
                    else
                    {
                        atomic_tree = compare_and_exchange(expr);
                        info_printf_at(expr.get_locus(), "'atomic' directive implemented using GCC compare and exchange\n");
                    }
                }

                replacements.append(atomic_tree);
            }
        }
        construct.replace(replacements);
    }
} }
