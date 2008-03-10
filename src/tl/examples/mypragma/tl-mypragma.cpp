/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
                register_directive("test");
                on_directive_post["test"].connect(functor(&MyPragmaPhase::construct_post, *this));
            }

            void construct_post(PragmaCustomConstruct pragma_custom_construct)
            {
                Source src;

                AST_t inner_tree;

                src 
                    << "{" 
                    <<    "int __my_var;"
                    <<    placeholder_statement(inner_tree)
                    << "}"
                    ;

                AST_t parsed_tree = src.parse_statement(pragma_custom_construct.get_ast(),
                        pragma_custom_construct.get_scope_link());

                {
                    // Parsing this now is valid thanks to the strategically placed 'inner_tree' :)
                    Source src2;
                    src2
                        << "__my_var = __my_var + 1;"
                        ;

                    AST_t parsed_tree_2 = src2.parse_statement(inner_tree, 
                            pragma_custom_construct.get_scope_link());

                    inner_tree.replace(parsed_tree_2);
                }

                pragma_custom_construct.get_ast().replace(parsed_tree);
            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
