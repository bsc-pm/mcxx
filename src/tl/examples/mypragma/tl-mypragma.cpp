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
                Statement stmt = pragma_custom_construct.get_statement();

                PredicateAttr is_function_call(LANG_IS_FUNCTION_CALL);

                ObjectList<AST_t> stmt_list = stmt.get_ast().depth_subtrees(is_function_call);

                for (ObjectList<AST_t>::iterator it = stmt_list.begin();
                        it != stmt_list.end();
                        it++)
                {
                    std::cerr << "'" << it->prettyprint() << "'" << std::endl;
                    Expression expr(*it, pragma_custom_construct.get_scope_link());
                    Expression top_level = expr.get_top_enclosing_expression();

                    std::cerr << "enclosed by '" << top_level.prettyprint() << "' " << top_level.original_tree().internal_ast_type() << std::endl;
                }
            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
