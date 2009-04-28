/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-mypragma.hpp"
#include "tl-pragmasupport.hpp"
#include "tl-langconstruct.hpp"

#include <vector>
#include <stack>
#include <sstream>

namespace TL
{
    class MyPragmaPhase : public PragmaCustomCompilerPhase
    {
        public:
            MyPragmaPhase()
                : PragmaCustomCompilerPhase("mypragma")
            {
                register_construct("test");
                on_directive_post["test"].connect(functor(&MyPragmaPhase::construct_post, *this));
            }

            void construct_post(PragmaCustomConstruct pragma_custom_construct)
            {
                Source new_var;

                // Replace all occurrences of induction variable i with _i
                // for (i = 0; i < 100; i++)
                // -> for (_i = 0; _i < 100; _i++)

                Statement st = pragma_custom_construct.get_statement();

                if (!ForStatement::predicate(st.get_ast()))
                {
                    std::cerr << "This is not a for-statement, ignoring" << std::endl;
                }

                ForStatement for_stmt(st.get_ast(), st.get_scope_link());

                IdExpression ind_var = for_stmt.get_induction_variable();
                Symbol sym = ind_var.get_symbol();

                ReplaceSrcIdExpression replacements(for_stmt.get_scope_link());
                replacements.add_replacement(sym, "_" + sym.get_name());
                
                Source transformed, replaced_for;

                transformed
                    << "{"
                    << sym.get_type().get_declaration(sym.get_scope(), "_" + sym.get_name()) << ";"
                    << replaced_for
                    << "}"
                    ;

                replaced_for = replacements.replace(for_stmt);

                AST_t new_tree =
                    transformed.parse_statement(pragma_custom_construct.get_ast(),
                            pragma_custom_construct.get_scope_link());

                pragma_custom_construct.get_ast().replace(new_tree);
            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
