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

// This is needed for NANOS_INIT_LOCK_FREE defined in nanox headers
#include "nanos.h"

using namespace TL;
using namespace TL::Nanox;

// This is so crude
static std::string macro_expanded_handling(const std::string& str)
{
    return std::string(str.begin() + 1, str.end() - 1);
}

void OMPTransform::critical_postorder(PragmaCustomConstruct critical_construct)
{
    std::string lock_name;
    if (critical_construct.is_parameterized())
    {
        lock_name = "_nx_u_" + critical_construct.get_parameter_arguments()[0] + "_critical_lock";
    }
    else
    {
        lock_name = "_nx_default_critical_lock";
    }

    if (_lock_names.find(lock_name) == _lock_names.end())
    {
        _lock_names.insert(lock_name);
        // We do this to use a macro from the code of nanox
#define STR_(x) #x
#define STR(x) STR_((x))
        std::string initializer = macro_expanded_handling(STR(NANOS_INIT_LOCK_FREE));
#undef STR
#undef STR_

        // We need to sign in the global lock
        Source global_lock_decl;

        // We need to make it weak
        global_lock_decl
            << "__attribute__((weak)) nanos_lock_t " << lock_name << " = " << initializer << ";"
            ;

        AST_t tree = global_lock_decl.parse_global(critical_construct.get_ast(), 
                critical_construct.get_scope_link());

        critical_construct.get_ast().prepend_sibling_global(tree);
    }

    Source critical_postorder_src;
    critical_postorder_src
        << "{"
        <<    "nanos_set_lock(&" << lock_name << ");"
        <<    critical_construct.get_statement()
        <<    "nanos_unset_lock(&" << lock_name << ");"
        << "}"
        ;

    AST_t tree = critical_postorder_src.parse_statement(critical_construct.get_ast(), critical_construct.get_scope_link());
    critical_construct.get_ast().replace(tree);
}
