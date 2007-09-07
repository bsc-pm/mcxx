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
    class MyPragmaPhase : public PragmaCustomCompilerPhase
    {
        private:
        public:
            MyPragmaPhase()
                : PragmaCustomCompilerPhase("mypragma")
            {
                register_directive("directive");
                on_directive_post["directive"].connect(functor(&MyPragmaPhase::directive_post, *this));

                register_construct("construct");
                on_directive_post["construct"].connect(functor(&MyPragmaPhase::directive_post, *this));
            }

            void directive_post(PragmaCustomConstruct pragma_custom_directive)
            {
                std::cerr << "Location -> " << pragma_custom_directive.get_ast().get_locus() << std::endl;

                if (pragma_custom_directive.is_parameterized())
                {
                    std::cerr << "Construct has a parameter" << std::endl;
                    ObjectList<std::string> parameter_arguments = pragma_custom_directive.get_parameter_arguments();

                    for (ObjectList<std::string>::iterator it = parameter_arguments.begin();
                            it != parameter_arguments.end();
                            it++)
                    {
                        std::cerr << "  '" << *it << "'" << std::endl;
                    }

                    ObjectList<Expression> parameter_expressions = pragma_custom_directive.get_parameter_expressions();
                    for (ObjectList<Expression>::iterator it = parameter_expressions.begin();
                            it != parameter_expressions.end();
                            it++)
                    {
                        std::cerr << "  Expr: '" << it->prettyprint() << "'" << std::endl;
                    }
                }

                PragmaCustomClause clause = pragma_custom_directive.get_clause("clause");
                if (clause.is_defined())
                {
                    std::cerr << "Clause 'clause' has been given" << std::endl;
                    ObjectList<std::string> arguments = clause.get_arguments();

                    if (arguments.empty())
                    {
                        std::cerr << "Empty list of arguments" << std::endl;
                    }
                    else
                    {
                        std::cerr << "List of arguments" << std::endl;
                        for (ObjectList<std::string>::iterator it = arguments.begin();
                                it != arguments.end();
                                it++)
                        {
                            std::cerr << "  '" << *it << "'" << std::endl;
                        }

                        ObjectList<Expression> expression_list = clause.get_expression_list();
                        for (ObjectList<Expression>::iterator it = expression_list.begin();
                                it != expression_list.end();
                                it++)
                        {
                            std::cerr << "Expr:  '" << it->prettyprint() << "'" << std::endl;
                        }
                    }

                    
                }
                else
                {
                    std::cerr << "Clause 'clause' has not been given" << std::endl;
                }
            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
