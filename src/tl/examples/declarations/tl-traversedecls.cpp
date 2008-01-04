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
#include "tl-compilerphase.hpp"
#include "tl-traverse.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    class DeclarationTraverseFunctor : public TraverseFunctor
    {
        public:
            virtual void postorder(Context ctx, AST_t node) 
            { 
                Declaration declaration(node, ctx.scope_link);

                std::cerr << "Found a declaration '" << node.prettyprint() << "' in '" << node.get_locus() << std::endl;

                ObjectList<DeclaredEntity> declared_entities = declaration.get_declared_entities();;

                for (ObjectList<DeclaredEntity>::iterator it = declared_entities.begin();
                        it != declared_entities.end();
                        it++)
                {
                    DeclaredEntity& entity = *it;
                    std::cerr << "  Declared '" << entity.get_declared_entity().prettyprint() << "'" << std::endl;
                    std::cerr << "  Is functional decl ? " << 
                        (entity.is_functional_declaration() ? "yes" : "no" )
                        << std::endl;
                    std::cerr << "  Has initializer ? " << 
                        (entity.has_initializer() ? "yes" : "no")
                        << std::endl;
                    std::cerr << std::endl;
                }
            }
    };

    class TraverseDecls : public CompilerPhase
    {
        private:
            std::string test_parameter;
        public:
            TraverseDecls()
            {
                register_parameter("test",
                        "This is a test parameter",
                        test_parameter,
                        "no-given-test-parameter");
            }

            void run(DTO& dto)
            {
                std::cerr << "TEST PARAMETR '" << test_parameter << "'" << std::endl;

                AST_t ast = dto["translation_unit"];
                ScopeLink scope_link = dto["scope_link"];

                DepthTraverse depth_traverse;

                DeclarationTraverseFunctor declaration_traverse_functor;

                PredicateAST<LANG_IS_DECLARATION> declaration_predicate;

                depth_traverse.add_predicate(declaration_predicate,
                        declaration_traverse_functor);

                depth_traverse.traverse(ast, scope_link);
            }
    };
}

EXPORT_PHASE(TL::TraverseDecls);
