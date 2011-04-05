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

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::barrier_postorder(PragmaCustomConstruct barrier_construct)
{
    std::cerr << barrier_construct.get_ast().get_locus() << ": warning: OpenMP barrier is not properly supported at this time" << std::endl;

    Source new_code;

    new_code
        << "{"
        <<     get_wait_completion(Source("nanos_current_wd()"), false, barrier_construct.get_ast())
        <<     "nanos_err_t err = nanos_team_barrier();"
        <<     "if (err != NANOS_OK) nanos_handle_error(err);"
        << "}"
        ;

    AST_t tree = new_code.parse_statement(barrier_construct.get_ast(), barrier_construct.get_scope_link());
    barrier_construct.get_ast().replace(tree);
}
