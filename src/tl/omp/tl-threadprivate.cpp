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
#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
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
                // Register the symbol in the DataSharing of threadprivate 
                Symbol symbol = it->get_symbol();
                threadprivate_directive.add_data_attribute(symbol, OpenMP::DA_THREADPRIVATE);

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
                    if (it2->get_declared_symbol() == it->get_symbol())
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
                        remade_declaration << " = " << it2->get_initializer().prettyprint()
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
                        // will complain 
                        scope_link, Source::ALLOW_REDECLARATION);

                // Now replace the whole declaration with this new one
                decl.get_ast().replace(redeclaration_tree);
            }

            // This directive must be removed
            threadprivate_directive.get_ast().remove_in_list();
        }
    }
}
