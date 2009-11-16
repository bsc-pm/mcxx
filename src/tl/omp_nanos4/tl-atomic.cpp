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
        void OpenMPTransform::atomic_postorder(PragmaCustomConstruct atomic_construct)
        {
            // TODO - An atomic can be implemented better
            Source critical_source;

            Statement critical_body = atomic_construct.get_statement();

            critical_source
                << "{"
                <<   "static nth_word_t default_mutex_var;"
                //                    <<   "extern void nthf_spin_lock_(void*);"
                //                    <<   "extern void nthf_spin_unlock_(void*);"
                <<   "nthf_spin_lock_(&default_mutex_var);"
                <<   critical_body.prettyprint()
                <<   "nthf_spin_unlock_(&default_mutex_var);"
                << "}"
                ;

            AST_t atomic_tree = critical_source.parse_statement(atomic_construct.get_ast(),
                    atomic_construct.get_scope_link());

            atomic_construct.get_ast().replace(atomic_tree);
        }
    }
}
