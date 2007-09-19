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
                register_construct("construct");
                on_directive_post["construct"].connect(functor(&MyPragmaPhase::construct_post, *this));
            }

            void construct_post(PragmaCustomConstruct pragma_custom_construct)
            {
                std::cerr << "Construct found at " << pragma_custom_construct.get_ast().get_locus() << std::endl;
                PragmaCustomClause input_clause = pragma_custom_construct.get_clause("input");

                // This is an invalid tree
                AST_t reference_tree;

                if (pragma_custom_construct.is_function_definition())
                {
                    /*
                     * In a function definition parameters are signed up in the compound statement that
                     * is the body of the function (this is the reason why local variables cannot be named
                     * like parameters if they are in the outermost compound statement)
                     */
                    std::cerr << "It is a function definition" << std::endl;

                    AST_t decl = pragma_custom_construct.get_declaration();
                    FunctionDefinition function_def(decl, pragma_custom_construct.get_scope_link());
                    Statement st = function_def.get_function_body();

                    DeclaredEntity declared_entity = function_def.get_declared_entity();
                    bool has_ellipsis = false;
                    ObjectList<ParameterDeclaration> parameters = declared_entity.get_parameter_declarations(has_ellipsis);

                    for (ObjectList<ParameterDeclaration>::iterator it = parameters.begin();
                            it != parameters.end();
                            it++)
                    {
                        ParameterDeclaration& parameter = *it;

                        if (it->is_named())
                        {
                            std::cerr << "name -> " << parameter.get_name().prettyprint() << std::endl;
                        }
                    }

                    reference_tree = st.get_ast();

                }
                else // Function declaration
                {
                    /*
                     * In a function declaration parameters are signed up in a prototype scope that
                     * is completely unrelated (except for "nested in" relationship) with the enclosing
                     * scope. So we have to parse things using the scope of parameters (if any) because
                     * they are in this "aside" prototype.
                     */
                    std::cerr << "It should be a function declaration" << std::endl;
                    AST_t ast_decl = pragma_custom_construct.get_declaration();

                    Declaration decl(ast_decl, pragma_custom_construct.get_scope_link());

                    ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

                    if (declared_entities.size() == 1)
                    {
                        DeclaredEntity& decl_entity = declared_entities[0];

                        if (decl_entity.is_functional_declaration())
                        {
                            bool has_ellipsis = false;
                            ObjectList<ParameterDeclaration> parameter_declarations = 
                                decl_entity.get_parameter_declarations(has_ellipsis);

                            if (!parameter_declarations.empty())
                            {
                                // Use the first one
                                reference_tree = parameter_declarations[0].get_ast();
                            }
                        }
                    }
                }

                ObjectList<std::string> list = input_clause.get_arguments();

                for (ObjectList<std::string>::iterator it = list.begin();
                        it != list.end();
                        it++)
                {
                    Source src;
                    src << *it;

                    std::cerr << "Parsing '" << *it << "'" << std::endl;

                    AST_t tree = src.parse_expression(reference_tree, pragma_custom_construct.get_scope_link());

                    Expression expr(tree, pragma_custom_construct.get_scope_link());
                    IdExpression id_expr = expr.get_id_expression();
                    Symbol symbol = id_expr.get_symbol();

                    if (symbol.is_valid() && symbol.is_parameter())
                    {
                        Type type = symbol.get_type();
                        std::cerr << "Symbol -> " << symbol.get_name() 
                            << " position = " << symbol.get_parameter_position() 
                            <<  " type = " << "'" << type.get_declaration(pragma_custom_construct.get_scope(), "") << "'"
                            << std::endl;
                    }
                    else
                    {
                        std::cerr << "'" << *it << "' is not valid symbol?" << std::endl;
                    }
                }
            }

    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
