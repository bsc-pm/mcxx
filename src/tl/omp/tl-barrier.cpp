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
        void OpenMPTransform::barrier_postorder(OpenMP::BarrierDirective barrier_directive)
        {
            Source barrier_source;

            Source instrumentation_code_before, instrumentation_code_after;

            if (instrumentation_requested())
            {
                instrumentation_code_before
                    << "int __previous_state = mintaka_get_state();"
                    << "mintaka_state_synch();"
                    ;

                instrumentation_code_after
                    << "mintaka_set_state(__previous_state);"
                    ;
            }

            barrier_source
                << "{"
                //                    <<    "extern void in__tone_barrier_();"
                <<    instrumentation_code_before
                <<    "in__tone_barrier_();"
                <<    instrumentation_code_after
                << "}"
                ;

            AST_t barrier_tree = barrier_source.parse_statement(barrier_directive.get_ast(),
                    barrier_directive.get_scope_link());

            barrier_directive.get_ast().replace(barrier_tree);
        }
    }
}
