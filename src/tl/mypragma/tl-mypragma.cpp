/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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

#include <vector>
#include <stack>
#include <cstdlib>

namespace TL
{
    struct TestAInfo
    {
        PragmaCustomConstruct construct;
        bool immediate;

        TestAInfo(PragmaCustomConstruct pragma_construct)
            : construct(pragma_construct), immediate(false)
        {
        }
    };

    class TestBInfo
    {
        public:
            PragmaCustomConstruct construct;
            ObjectList<TestAInfo> testa_list;

            TestBInfo(PragmaCustomConstruct pragma_construct)
                 : construct(pragma_construct)
            {
            }
    };

    class MyPragmaPhase : public PragmaCustomCompilerPhase
    {
        private:
            std::stack<TestBInfo> testb_info;
        public:
            MyPragmaPhase()
                : PragmaCustomCompilerPhase("mypragma")
            {
                on_directive_pre["testA"].connect(functor(&MyPragmaPhase::testA_preorder, *this));
                on_directive_post["testA"].connect(functor(&MyPragmaPhase::testA_postorder, *this));

                on_directive_pre["testB"].connect(functor(&MyPragmaPhase::testB_preorder, *this));
                on_directive_post["testB"].connect(functor(&MyPragmaPhase::testB_postorder, *this));

                on_directive_post["unroll"].connect(functor(&MyPragmaPhase::unroll_postorder, *this));

                on_directive_post["test"].connect(functor(&MyPragmaPhase::test_postorder, *this));
            }

            void test_postorder(PragmaCustomConstruct test_construct)
            {
                if (test_construct.is_parameterized())
                {
                    ObjectList<Expression> parameter = test_construct.get_parameter();

                    for(ObjectList<Expression>::iterator it = parameter.begin();
                            it != parameter.end();
                            it++)
                    {
                        std::cerr << "-> " << it->prettyprint() << std::endl;
                    }

                    std::cerr << std::endl;
                }
                else
                {
                    std::cerr << "Not parameterized" << std::endl;
                }
            }

            void testB_preorder(PragmaCustomConstruct pragma_custom_construct)
            {
                TestBInfo testb(pragma_custom_construct);

                Statement st = pragma_custom_construct.get_statement();

                testb_info.push(testb);
            }

            void testA_preorder(PragmaCustomConstruct pragma_custom_construct)
            {
                std::cerr << "found testA --> " << pragma_custom_construct.get_ast().get_locus() << std::endl;
                TestBInfo& testb = testb_info.top();

                TestAInfo testa(pragma_custom_construct);

                check_level(testb.construct.get_statement(), testa);

                testb.testa_list.append(testa);
            }
            
            void check_level(Statement st, TestAInfo& testa)
            {
                if (!st.is_compound_statement())
                {
                    if (st.get_ast() == testa.construct.get_ast())
                    {
                        testa.immediate = true;
                        std::cerr << "immediate --> " << testa.construct.get_ast().get_locus() << std::endl;
                    }
                }
                else 
                {
                    ObjectList<Statement> inner_statements = st.get_inner_statements();
                    for (ObjectList<Statement>::iterator it = inner_statements.begin();
                            it != inner_statements.end();
                            it++)
                    {
                        check_level(*it, testa);
                    }
                }
            }

            void testA_postorder(PragmaCustomConstruct pragma_custom_construct)
            {
                Source src;

                src
                    << "printf(\"Hello world at " 
                    << pragma_custom_construct.get_ast().get_locus() 
                    << "\");"
                    ;

                AST_t replace_tree = src.parse_statement(pragma_custom_construct.get_ast(),
                        pragma_custom_construct.get_scope_link());

                pragma_custom_construct.get_ast().replace(replace_tree);
            }

            void testB_postorder(PragmaCustomConstruct pragma_custom_construct)
            {
                testb_info.pop();

                // pragma_custom_construct.get_ast().replace(pragma_custom_construct.get_statement().get_ast());
            }

            /*
             * For countable for-loops
             *
             * #pragma mypragma unroll times(4)
             * for (i = 0; i < 100; i++)
             * {
             *   ...
             * }
             *
             */
            void unroll_postorder(PragmaCustomConstruct pragma_construct)
            {
                // Get the for-statement under the hood of the pragma
                ForStatement for_statement = pragma_construct.get_statement();

                // Get the 'times(...)' clause
                PragmaCustomClause times_clause = pragma_construct.get_clause("times");

                // Now get its expression arguments
                ObjectList<Expression> times_arguments = times_clause.get_expression_list();
                // And get the value for the first expression
                int unroll_times = strtoint(times_arguments[0].prettyprint());

                // Get the induction variable, lower bound and upper bound of the for-loop
                IdExpression induction_var = for_statement.get_induction_variable();
                Expression upper_bound = for_statement.get_upper_bound();
                Expression loop_step = for_statement.get_step();
                Statement loop_body = for_statement.get_loop_body();

                // Create the layout of the resulting code
                Source replaced_for, unrolled_for, unrolled_for_body, tail_for;

                // The source code resulting of the transformation
                replaced_for
                    << "{"
                          // The unrolled for, filled below
                    <<    unrolled_for
                          // The unrolled part, filled below
                    <<    tail_for
                    << "}"
                    ;

                // Write the unrolled loop
                unrolled_for
                    << "for (" << for_statement.get_iterating_init().prettyprint() 
                    <<   "(" << induction_var.prettyprint() << " + " << unroll_times << " )<= (" << upper_bound.prettyprint() << ");"
                    <<    induction_var.prettyprint() << "+= ((" << loop_step.prettyprint() << ")*" << unroll_times << "))"
                    << "{"
                          // This will be filled below
                    <<    unrolled_for_body
                    << "}"
                    ;

                // At least one iteration will be in the unrolled_for
                unrolled_for_body << loop_body.prettyprint();

                Symbol induction_var_symbol = induction_var.get_symbol();
                for (int i = 1; i < unroll_times; i++)
                {
                    // Create a replacement map
                    ReplaceIdExpression replacements;

                    // Replace every 'ind-var' with its 'ind-var + i'
                    Source induction_replacement;
                    induction_replacement
                        << "(" << induction_var.prettyprint() << "+" << i << ")"
                        ;
                    replacements.add_replacement(induction_var_symbol, induction_replacement);

                    // Do tree replacement
                    Statement replaced_statement = replacements.replace(loop_body);

                    // And create the source
                    unrolled_for_body << replaced_statement.prettyprint();
                }

                // Create the tail for 
                tail_for
                    << "for ( ; " << for_statement.get_iterating_condition().prettyprint() << ";" 
                    <<     for_statement.get_iterating_expression().prettyprint() << ")"
                    <<     loop_body.prettyprint()
                    ;

                // Now parse the newly created source. A reference tree is given together
                // with its 'scope link'
                AST_t replaced_tree = replaced_for.parse_statement(pragma_construct.get_ast(),
                        pragma_construct.get_scope_link());

                pragma_construct.get_ast().replace(replaced_tree);
            }

            int strtoint(const std::string& str)
            {
                return atoi(str.c_str());
            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
