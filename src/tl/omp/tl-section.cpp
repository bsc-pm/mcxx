/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
        void OpenMPTransform::section_postorder(OpenMP::SectionConstruct section_construct)
        {
            int &num_sections = num_sections_stack.top();

            Source section_source, instrumentation_before, instrumentation_after;
            Statement construct_body = section_construct.body();

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
