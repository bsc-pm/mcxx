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

namespace TL
{
    namespace Nanox
    {
        Source create_outline(
                FunctionDefinition enclosing_function,
                Source outline_name,
                Source parameter_list,
                Source body)
        {
            Source result;

            Source forward_declaration;
            Symbol function_symbol = enclosing_function.get_function_symbol();

            if (!function_symbol.is_member())
            {
                Source template_header;

                IdExpression function_name = enclosing_function.get_function_name();
                Declaration point_of_decl = function_name.get_declaration();
                DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
                ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
                DeclaredEntity declared_entity = *(declared_entities.begin());

                forward_declaration 
                    << template_header
                    << decl_specs.prettyprint()
                    << " "
                    << declared_entity.prettyprint()
                    << ";";
            }


            result
                << forward_declaration
                << "void " << outline_name << "(" << parameter_list << ")"
                << "{"
                << body
                << "}"
                ;

            return result;
        }
    }
}
