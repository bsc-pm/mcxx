/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "tl-omp-core.hpp"
#include "tl-omp-deps.hpp"

namespace TL { namespace OpenMP {

    static Symbol get_symbol_of_data_reference(Expression expr)
    {
        if (expr.is_id_expression())
        {
            IdExpression id_expr = expr.get_id_expression();
            return id_expr.get_symbol();
        }
        else if (expr.is_unary_operation())
        {
            if (expr.get_operation_kind() == Expression::DERREFERENCE)
            {
                Expression ref = expr.get_unary_operand();

                if (ref.is_unary_operation()
                        && ref.get_operation_kind() == Expression::REFERENCE)
                    return get_symbol_of_data_reference(ref.get_unary_operand());
                else
                    return get_symbol_of_data_reference(ref);
            }
        }
        else if (expr.is_array_subscript())
        {
            return get_symbol_of_data_reference(expr.get_subscripted_expression());
        }
        else if (expr.is_array_section())
        {
            return get_symbol_of_data_reference(expr.array_section_item());
        }
        else if (expr.is_shaping_expression())
        {
            return get_symbol_of_data_reference(expr.shaped_expression());
        }
        else
        {
            internal_error("Invalid expression kind", 0);
        }
    }

    static void add_data_sharings(ObjectList<Expression> &expression_list, 
            DataSharingEnvironment& data_sharing, 
            DependencyDirection attr)
    {
        for (ObjectList<Expression>::iterator it = expression_list.begin();
                it != expression_list.end();
                it++)
        {
            DataReference expr(*it);
            if (expr.is_valid())
            {
                std::cerr << expr.get_ast().get_locus() 
                    << ": warning: skipping invalid dependency expression '" << expr.prettyprint() << "'" << std::endl;
                continue;
            }

            DependencyItem dep_item(*it, attr);

            Symbol sym = expr.get_base_symbol();
            if (sym.is_valid())
            {
                DataSharingAttribute attr = data_sharing.get(sym);

                if (((attr & DS_PRIVATE) == DS_PRIVATE)
                        && ((attr & DS_IMPLICIT) != DS_IMPLICIT))
                {
                    std::cerr << expr.get_ast().get_locus()
                        << ": warning: dependency expression '" 
                        << expr.prettyprint() << "' names a private variable, making it shared" << std::endl;

                    data_sharing.set(sym, attr);
                }

                data_sharing.set(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
            }
            else
            {
                std::cerr << expr.get_ast().get_locus() 
                    << ": warning: skipping invalid dependency expression '" << expr.prettyprint() << "'" << std::endl;
                continue;
            }

            data_sharing.add_dependence(dep_item);
        }
    }

    static ObjectList<Expression> get_expression_list_of_function_call_arguments(
            const std::string& clause_name,
            PragmaCustomConstruct construct,
            DataSharingEnvironment& data_sharing)
    {
        ObjectList<Expression> result;

        PragmaCustomClause clause = construct.get_clause(clause_name);
        if (!clause.is_defined())
            return result;

        Statement compound_stmt = construct.get_statement();

        ObjectList<Statement> inner_stmts = compound_stmt.get_inner_statements();
        // The last one should contain the call
        Statement stm = inner_stmts[inner_stmts.size() - 1];

        Expression expr = stm.get_expression();

        ObjectList<Expression> args = expr.get_argument_list();

        ObjectList<std::string> arg_id_list = clause.get_arguments(ExpressionTokenizer());

        for (ObjectList<std::string>::iterator it = arg_id_list.begin();
                it != arg_id_list.end();
                it++)
        {
            int n = -1;
            std::stringstream ss;
            ss << *it;
            ss >> n;

            if (n >= 0 && n < args.size())
            {
                result.append(args[n]);
            }
        }

        return result;
    }

    void Core::get_dependences_info(PragmaCustomConstruct construct, DataSharingEnvironment& data_sharing)
    {
        if (!construct.get_clause("__function").is_defined())
        {
            PragmaCustomClause input_clause = construct.get_clause("input");
            get_dependences_info_clause(input_clause, data_sharing, DEP_DIR_INPUT);

            PragmaCustomClause output_clause = construct.get_clause("output");
            get_dependences_info_clause(output_clause, data_sharing, DEP_DIR_OUTPUT);

            PragmaCustomClause inout_clause = construct.get_clause("inout");
            get_dependences_info_clause(inout_clause, data_sharing, DEP_DIR_INOUT);
        }
        else
        {
            ObjectList<Expression> input_expr_list, output_expr_list, inout_expr_list;
            input_expr_list = get_expression_list_of_function_call_arguments("__input_args", construct, data_sharing);
            add_data_sharings(input_expr_list, data_sharing, DEP_DIR_INPUT);

            output_expr_list = get_expression_list_of_function_call_arguments("__output_args", construct, data_sharing);
            add_data_sharings(output_expr_list, data_sharing, DEP_DIR_OUTPUT);

            inout_expr_list = get_expression_list_of_function_call_arguments("__inout_args", construct, data_sharing);
            add_data_sharings(inout_expr_list, data_sharing, DEP_DIR_INOUT);
        }
    }

    void Core::get_dependences_info_clause(PragmaCustomClause clause,
           DataSharingEnvironment& data_sharing,
           DependencyDirection dep_attr)
    {
        if (clause.is_defined())
        {
            ObjectList<Expression> expr_list = clause.get_expression_list();
            add_data_sharings(expr_list, data_sharing, dep_attr);
        }
    }
} }
