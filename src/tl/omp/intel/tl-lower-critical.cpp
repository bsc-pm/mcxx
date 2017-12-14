/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "tl-lowering-visitor.hpp"
#include "tl-lowering-utils.hpp"

namespace TL { namespace Intel {

void LoweringVisitor::visit(const Nodecl::OpenMP::Critical& construct)
{
    TL::Symbol ident_symbol = Intel::new_global_ident_symbol(construct);
    TL::Scope global_scope = CURRENT_COMPILED_FILE->global_decl_context;

    Nodecl::NodeclBase environment = construct.get_environment();
    Nodecl::NodeclBase statements = construct.get_statements();

    walk(statements);

    statements = construct.get_statements();

    std::string lock_name = "default";


    Nodecl::OpenMP::CriticalName critical_name = construct.get_environment()
        .as<Nodecl::List>()
        .find_first<Nodecl::OpenMP::CriticalName>();


    if (!critical_name.is_null()) {
        lock_name = critical_name.get_text();
    }

    lock_name += "_critical_lock";

    TL::Symbol lock_sym = global_scope.get_symbol_from_name(lock_name);
    if (!lock_sym.is_valid()) {
        Source src_critical_lock;
        src_critical_lock
        << "kmp_critical_name " << lock_name << ";";
        Nodecl::NodeclBase tree_critical_lock = src_critical_lock.parse_statement(global_scope);
        lock_sym = global_scope.get_symbol_from_name(lock_name);

        if (IS_CXX_LANGUAGE)
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, tree_critical_lock);
        }
    }

    Source src_critical;
    src_critical
    << "__kmpc_critical(&" << as_symbol(ident_symbol)
        << ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol)
        << "), &" << as_symbol(lock_sym) << ");"
    << as_statement(statements.shallow_copy())
    << "__kmpc_end_critical(&" << as_symbol(ident_symbol)
        << ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol)
        << "), &" << as_symbol(lock_sym) << ");";

    Nodecl::NodeclBase tree_critical = src_critical.parse_statement(construct);
    construct.replace(tree_critical);
}

} }
