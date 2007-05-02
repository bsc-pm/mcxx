#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::threadprivate_postorder(OpenMP::ThreadPrivateDirective threadprivate_directive)
    {
        // Given
        //
        //    int a, b, c;
        //    #pragma omp threadprivate(b)
        //
        // The compiler will create
        // 
        //    int a;
        //    int __thread b;
        //    int c;
        //

        // Get the threadprivate directive
        OpenMP::Directive directive = threadprivate_directive.directive();

        // And get its parameter clause (you can see the (...) as a
        // clause without name, we'll call it "parameter_clause")
        OpenMP::Clause clause = directive.parameter_clause();

        // Now get the list of symbols of this clause
        ObjectList<IdExpression> threadprivate_references = clause.id_expressions();

        // For every symbol in the clause
        for (ObjectList<IdExpression>::iterator it = threadprivate_references.begin();
                it != threadprivate_references.end();
                it++)
        {
            // Get its declaration
            Declaration decl = it->get_declaration();

            // A declaration has two parts, a DeclarationSpec and a list of
            // DeclaredEntity.
            //
            // const int static * a, b; 
            //  
            //    const int static -> DeclarationSpec
            //    *a, b            -> ObjectList<DeclaredEntity>
            //
            DeclarationSpec decl_spec = decl.get_declaration_specifiers();
            ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

            // This will hold the remade declaration
            Source remade_declaration;

            // For every entity declared
            for (ObjectList<DeclaredEntity>::iterator it2 = declared_entities.begin();
                    it2 != declared_entities.end();
                    it2++)
            {
                // Prettyprint the DeclarationSpec (to make it like it was before)
                remade_declaration << decl_spec.prettyprint() << " ";

                // And if the declaration appears in the threadprivate
                // add the non-portable decl-specifier "__thread". 
                //
                // Note that it must be at the end of the declaration
                // specifiers (this is a gcc requirement)
                if (it2->get_declared_entity().get_symbol() == it->get_symbol())
                {
                    remade_declaration << " __thread ";
                }

                remade_declaration << it2->prettyprint();

                // If the entity has an initializer like "b" below
                //
                //    int a, b = 3, c;
                //
                //  then, write it down
                if (it2->has_initializer())
                {
                    remade_declaration << it2->get_initializer().prettyprint()
                        ;
                }

                // End the declaration (obviously this only work for declaration statements,
                // arguments, at the moment, cannot be threadprivatized :)
                remade_declaration << ";"
                    ;
            }

            // Now parse the remade declarations
            AST_t redeclaration_tree = remade_declaration.parse_declaration(decl.get_ast(),
                    // And explicitly allow to redeclarate objects otherwise the compiler
                    // will complain (for debugging purposes)
                    scope_link, Source::ALLOW_REDECLARATION);

            // Now replace the whole declaration with this new one
            decl.get_ast().replace(redeclaration_tree);
        }

        // This directive must be removed
        threadprivate_directive.get_ast().remove_in_list();
    }
}
