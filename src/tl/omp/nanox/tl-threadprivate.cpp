/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

#include "tl-omp-nanox.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::threadprivate_postorder(PragmaCustomConstruct threadprivate_directive)
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

    // Now get the list of symbols of this clause

    ObjectList<Expression> parameter_expr = threadprivate_directive.get_parameter_expressions();

    ObjectList<IdExpression> threadprivate_references;
    for (ObjectList<Expression>::iterator it = parameter_expr.begin();
            it != parameter_expr.end();
            it++)
    {
        Expression &expr(*it);

        if (expr.is_id_expression())
        {
            threadprivate_references.append(expr.get_id_expression());
        }
        else
        {
            std::cerr << expr.get_ast().get_locus() 
                << ": warning: '" << expr << "' is not an id-expression, skipping" << std::endl;
        }
    }


    // For every symbol in the clause
    for (ObjectList<IdExpression>::iterator it = threadprivate_references.begin();
            it != threadprivate_references.end();
            it++)
    {
        Symbol symbol = it->get_symbol();

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
