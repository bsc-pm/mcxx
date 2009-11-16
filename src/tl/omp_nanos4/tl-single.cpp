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
        void OpenMPTransform::single_postorder(PragmaCustomConstruct single_construct)
        {
            Source single_source;
            Source barrier_code;

            Statement body_construct = single_construct.get_statement();

            single_source
                << "{"
                <<   "int nth_low;"
                <<   "int nth_upper;"
                <<   "int nth_step;"
                <<   "int nth_chunk;"
                <<   "int nth_schedule;"
                <<   "int nth_dummy1;"
                <<   "int nth_dummy2;"
                <<   "int nth_dummy3;"
                <<   "int nth_barrier; "

                <<   "nth_low = 0;"
                <<   "nth_upper = 0;"
                <<   "nth_step = 1;"
                <<   "nth_schedule = 2;" // Dynamic
                <<   "nth_chunk = 1;"

                //                    <<   "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
                //                    <<   "extern int in__tone_next_iters_(int*, int*, int*);"
                //                    <<   "extern void in__tone_end_for_(int*);"

                <<   "in__tone_begin_for_ (&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"
                <<   "while (in__tone_next_iters_ (&nth_dummy1, &nth_dummy2, &nth_dummy3) != 0)"
                <<   "{"
                <<       body_construct.prettyprint()
                <<   "}"
                <<   barrier_code
                << "}"
                ;

            PragmaCustomClause nowait_clause = single_construct.get_clause("nowait");
            barrier_code = get_loop_finalization(!(nowait_clause.is_defined()));

            AST_t single_tree = single_source.parse_statement(single_construct.get_ast(), 
                    single_construct.get_scope_link());

            single_construct.get_ast().replace(single_tree);
        }
    }
}
