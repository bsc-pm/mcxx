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
        public:
            TraverseDecls()
            {
            }

            void run(DTO& dto)
            {
                AST_t ast = dto["translation_unit"];
                ScopeLink scope_link = dto["scope_link"];

                DepthTraverse depth_traverse;

                DeclarationTraverseFunctor declaration_traverse_functor;

                PredicateBool<LANG_IS_DECLARATION> declaration_predicate;

                depth_traverse.add_predicate(declaration_predicate,
                        declaration_traverse_functor);

                depth_traverse.traverse(ast, scope_link);
            }
    };
}

EXPORT_PHASE(TL::TraverseDecls);
