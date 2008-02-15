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
#include <cstdlib>

namespace TL
{
    class MyPragmaPhase : public PragmaCustomCompilerPhase
    {
        public:
            MyPragmaPhase()
                : PragmaCustomCompilerPhase("mypragma")
            {
                register_construct("blocking");
                on_directive_post["blocking"].connect(functor(&MyPragmaPhase::construct_post, *this));
            }

            bool check_loop_nest(AST_t)
            {
                return true;
            }

            void get_loop_nest_rec(AST_t loop_nest_tree, ScopeLink sl, ObjectList<ForStatement> &result, int max_level)
            {
                Statement st(loop_nest_tree, sl);

                if (st.is_compound_statement())
                {
                    ObjectList<Statement> inner_statements = st.get_inner_statements();

                    if (inner_statements.size() == 1)
                    {
                        get_loop_nest_rec(inner_statements[0].get_ast(), sl, result, max_level);
                    }
                }
                else if (ForStatement::predicate(loop_nest_tree))
                {
                    ForStatement for_statement(loop_nest_tree, sl);
                    result.append(for_statement);

                    get_loop_nest_rec(for_statement.get_loop_body().get_ast(), sl, result, max_level - 1);
                }
            }

            ObjectList<ForStatement> get_loop_nest(AST_t loop_nest_tree, ScopeLink sl, int max_level)
            {
                ObjectList<ForStatement> result;

                get_loop_nest_rec(loop_nest_tree, sl, result, max_level);

                return result;
            }

            Source build_blocked_loop_nest(ObjectList<ForStatement> loop_nest)
            {
                Source result, additional_declarations, blocked_loops;

                result 
                    << "{"
                    <<    additional_declarations
                    <<    blocked_loops
                    << "}"
                    ;

                Source block_factor;
                block_factor << 25;

                Source by_strip_loops, 
                       within_strip_loops, 
                       blocked_inner_body, 
                       compensating_braces;

                blocked_loops
                    << by_strip_loops
                    << within_strip_loops
                    << blocked_inner_body
                    << compensating_braces
                    ;

                ReplaceIdExpression replace_blocked_exprs;

                for (ObjectList<ForStatement>::iterator it = loop_nest.begin();
                        it != loop_nest.end();
                        it++)
                {
                    ForStatement &for_statement(*it);
                    IdExpression induction_var = for_statement.get_induction_variable();

                    Symbol sym = induction_var.get_symbol();
                    Type type = sym.get_type();

                    Source blocked_ivar = std::string("_blk_" + sym.get_name());
                    AST_t replacing_tree = blocked_ivar.parse_expression(induction_var.get_ast(), 
                            induction_var.get_scope_link());
                    replace_blocked_exprs.add_replacement(sym, replacing_tree);

                    // Add the proper declaration of the blocked induction var
                    additional_declarations 
                        << type.get_declaration(induction_var.get_scope(), blocked_ivar.get_source()) 
                        << ";"
                        ;

                    by_strip_loops
                        << "for(" << for_statement.get_iterating_init().prettyprint() /* << ";" */
                        <<        for_statement.get_iterating_condition().prettyprint() << ";"
                        <<        induction_var.prettyprint() << " += ((" << block_factor << ")*(" << for_statement.get_step().prettyprint() << "))"
                        <<    ")"
                        << "{"
                        ;

                    within_strip_loops
                        << "for(" << blocked_ivar << " = " << induction_var.prettyprint() << ";"
                        <<        blocked_ivar << " <= " 
                                  << "min(" << for_statement.get_upper_bound().prettyprint() << "," 
                                      << induction_var.prettyprint() 
                                           << " + (" <<  for_statement.get_step().prettyprint() << ")*(" << block_factor << " - 1)"
                                      << ");"
                        <<        blocked_ivar << "++" << ")"
                        << "{"
                        ;

                    // We have added two opening braces per loop
                    compensating_braces
                        << "}"
                        << "}"
                        ;
                }

                // Get the body of the innermost loop
                ForStatement &innermost_loop(*(loop_nest.rbegin()));

                Statement innermost_loop_body = innermost_loop.get_loop_body();
                Statement replaced_innermost_loop_body = replace_blocked_exprs.replace(innermost_loop_body);

                blocked_inner_body 
                    << replaced_innermost_loop_body.prettyprint();

                // Return
                return result;
            }

            void construct_post(PragmaCustomConstruct pragma_custom_construct)
            {
                AST_t loop_nest_tree = pragma_custom_construct.get_statement().get_ast();

                if (!check_loop_nest(loop_nest_tree))
                {
                    std::cerr << "Skipping invalid loop nest at " << loop_nest_tree.get_locus() << std::endl;
                    return;
                }

                ObjectList<ForStatement> loop_nest = get_loop_nest(loop_nest_tree, pragma_custom_construct.get_scope_link(),
                        /* reserved */ 0);
                Source blocked_loop_nest = build_blocked_loop_nest(loop_nest);

                AST_t blocked_loop_nest_tree = blocked_loop_nest.parse_statement(pragma_custom_construct.get_ast(),
                        pragma_custom_construct.get_scope_link());

                pragma_custom_construct.get_ast().replace(blocked_loop_nest_tree);
            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
