/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include "tl-omp-core.hpp"
#include "hlt-collapse.hpp"
#include "tl-ast.hpp"

namespace TL
{
    namespace OpenMP
    {
        AST_t::callback_result remove_collapse_clause(const AST_t& a)
        {
            // Filter collapse clauses
            if ((a.internal_ast_type_() == AST_PRAGMA_CUSTOM_CLAUSE)
                    && a.get_text() == "collapse")
            {
                return AST_t::callback_result(true, "");
            }
            else
            {
                return AST_t::callback_result(false, "");
            }
        }

        void Core::collapse_loop_first(PragmaCustomConstruct& construct)
        {
            PragmaCustomClause collapse = construct.get_clause("collapse");

            if (!collapse.is_defined())
                return;

            ObjectList<Expression> expr_list = collapse.get_expression_list();
            if (expr_list.size() != 1)
            {
                running_error("%s: error: 'collapse' clause must have one argument\n",
                        construct.get_ast().get_locus().c_str());
            }

            Expression &expr = expr_list.front();
            if (!expr.is_constant())
            {
                running_error("%s: error: 'collapse' clause argument '%s' is not a constant expression\n",
                        expr.get_ast().get_locus().c_str(),
                        expr.prettyprint().c_str());
            }

            bool valid;
            int nest_level = expr.evaluate_constant_int_expression(valid);
            if (!valid)
            {
                running_error("%s: error: 'collapse' clause argument '%s' is not a constant expression\n",
                        expr.get_ast().get_locus().c_str(),
                        expr.prettyprint().c_str());
            }

            if (nest_level <= 0)
            {
                running_error("%s: error: nesting level of 'collapse' clause must be a nonzero positive integer\n",
                        expr.get_ast().get_locus().c_str());
            }

            if (!ForStatement::predicate(construct.get_statement().get_ast()))
            {
                running_error("%s: error: collapsed '#pragma omp for' or '#pragma omp parallel for' require a for-statement\n",
                        construct.get_statement().get_ast().get_locus().c_str());
            }

            ForStatement for_stmt(construct.get_statement().get_ast(), 
                    construct.get_scope_link());
            HLT::LoopCollapse loop_collapse(for_stmt);

            ObjectList<std::string> ancillary_names;

            Source header;
            loop_collapse
                .set_nesting_level(nest_level)
                .set_split_transform(header)
                .set_induction_private(true)
                .keep_ancillary_names(ancillary_names);

            Source collapsed_for = loop_collapse;

            Source transformed_code;
            AST_t pragma_placeholder;
            transformed_code
                << "{"
                << header
                << statement_placeholder(pragma_placeholder)
                << "}"
                ;

            AST_t tree = transformed_code.parse_statement(construct.get_ast(), construct.get_scope_link());

            Source new_firstprivate_entities;
            Source pragma_line;
            Source omp_part_src;
            omp_part_src
                << "#pragma omp " << pragma_line << new_firstprivate_entities << "\n"
                << collapsed_for
                ;

            new_firstprivate_entities << "firstprivate(" << concat_strings(ancillary_names, ",") << ")";

            pragma_line << construct.get_pragma_line().prettyprint_with_callback(functor(remove_collapse_clause));

            AST_t omp_part_tree = omp_part_src.parse_statement(pragma_placeholder, 
                    construct.get_scope_link());

            // Replace the pragma part
            pragma_placeholder.replace(omp_part_tree);

            // Replace the whole construct
            construct.get_ast().replace(tree);

            // Now overwrite the old construct with this new one
            construct = PragmaCustomConstruct(pragma_placeholder, construct.get_scope_link());
        }
    }
}
