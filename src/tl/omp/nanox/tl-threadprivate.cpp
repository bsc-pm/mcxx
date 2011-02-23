/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
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
#include "tl-nanos-threadprivate.hpp"

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

    std::cerr << "HOLA MON" << std::endl;

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
    ObjectList<Symbol> sym_list = threadprivate_references.map(functor(&IdExpression::get_symbol));

    Nanos::add_thread_to_declarations(sym_list, threadprivate_directive.get_scope_link());

    // This directive must be removed
    threadprivate_directive.get_ast().remove_in_list();
}
