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

#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::section_postorder(PragmaCustomConstruct section_construct)
        {
            int &num_sections = num_sections_stack.top();

            Source section_source, instrumentation_before, instrumentation_after;
            Statement construct_body = section_construct.get_statement();

            section_source
                << "case " << num_sections << ":"
                << "{"
                <<    instrumentation_before
                <<    construct_body.prettyprint()
                <<    instrumentation_after
                <<    "break;"
                << "}"
                ;

            AST_t section_tree = section_source.parse_statement(section_construct.get_ast(),
                    section_construct.get_scope_link());

            // One more section
            num_sections++;

            section_construct.get_ast().replace(section_tree);
        }
    }
}
