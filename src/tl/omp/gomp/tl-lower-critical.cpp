/*--------------------------------------------------------------------
  (C) Copyright 2006-2016 Barcelona Supercomputing Center
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

namespace TL
{
namespace GOMP
{

void LoweringVisitor::visit(const Nodecl::OpenMP::Critical &construct)
{
    Nodecl::OpenMP::CriticalName critical_name
        = construct.get_environment()
              .as<Nodecl::List>()
              .find_first<Nodecl::OpenMP::CriticalName>();

    Nodecl::NodeclBase stmt = construct.get_statements();

    Nodecl::NodeclBase new_code;
    if (critical_name.is_null())
    {
        Source src;
        src << "GOMP_critical_start();" << as_statement(stmt.shallow_copy())
            << "GOMP_critical_end();";

        new_code = src.parse_statement(construct.retrieve_context());
    }
    else
    {
        Source src;
        std::string gomp_critical_name = "__gomp_critical_name_"
                                         + critical_name.get_text();

        TL::Symbol sym
            = TL::Scope::get_global_scope().new_symbol(gomp_critical_name);
        sym.get_internal_symbol()->kind = SK_VARIABLE;
        sym.set_type(TL::Type::get_void_type().get_pointer_to());
        symbol_entity_specs_set_is_user_declared(sym.get_internal_symbol(), 1);
        gcc_attribute_t common_attr = { "common", nodecl_null() };
        symbol_entity_specs_add_gcc_attributes(sym.get_internal_symbol(),
                                               common_attr);

        src << "GOMP_critical_name_start(&" << gomp_critical_name << ");"
            << as_statement(stmt.shallow_copy()) << "GOMP_critical_name_end(&"
            << gomp_critical_name << ");";

        new_code = src.parse_statement(construct.retrieve_context());
    }

    construct.replace(new_code);
}
}
}
