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
        void OpenMPTransform::taskgroup_postorder(PragmaCustomConstruct taskgroup_construct)
        {
            Source taskgroup_source;
            Statement taskgroup_body = taskgroup_construct.get_statement();


            taskgroup_source
                << "{"
                <<    "nthf_push_taskgroup_scope_();"
                <<    taskgroup_body.prettyprint()
                <<    "nthf_task_block_();"
                <<    "nthf_pop_taskgroup_scope_();"
                << "}"
                ;

            AST_t taskgroup_code = taskgroup_source.parse_statement(taskgroup_construct.get_ast(),
                    taskgroup_construct.get_scope_link());

            taskgroup_construct.get_ast().replace(taskgroup_code);
        }
    }
}
