/*--------------------------------------------------------------------
  (C) Copyright 2016-2016 Barcelona Supercomputing Center
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


#include "tl-nanos6-fortran-support.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include <set>

namespace
{

std::set<TL::Type> _used_types;

bool in_the_same_module(TL::Scope sc, TL::Symbol module)
{
    scope_t *current_scope = sc.get_decl_context()->current_scope;

    while (current_scope != NULL)
    {
        if (current_scope->related_entry == module.get_internal_symbol())
            return 1;

        current_scope = current_scope->contained_in;
    }

    return 0;
}

void add_used_types_rec(TL::Type t, TL::Scope sc)
{
    if (!t.is_valid())
        return;

    std::pair<std::set<TL::Type>::iterator, bool> p = _used_types.insert(t);
    if (!p.second)
        return;

    if (t.is_named_class())
    {
        if (t.get_symbol().is_from_module())
        {
            Nodecl::Utils::Fortran::append_module_to_scope(
                t.get_symbol().from_module(), sc);
        }
        else if (t.get_symbol().is_in_module()
                 && !in_the_same_module(sc, t.get_symbol().in_module()))
        {
            Nodecl::Utils::Fortran::append_module_to_scope(
                t.get_symbol().in_module(), sc);
        }
        else
        {
            TL::ObjectList<TL::Symbol> members = t.get_fields();
            for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                 it != members.end();
                 it++)
            {
                add_used_types_rec(it->get_type(), sc);
            }
        }
    }
    else if (t.is_lvalue_reference())
    {
        add_used_types_rec(t.references_to(), sc);
    }
    else if (t.is_pointer())
    {
        add_used_types_rec(t.points_to(), sc);
    }
    else if (t.is_array())
    {
        add_used_types_rec(t.array_element(), sc);
    }

    _used_types.erase(t);
}
}

void TL::Nanos6::fortran_add_types(const TL::ObjectList<TL::Symbol> &sym_list,
                                   TL::Scope dest_scope)
{
    for (TL::ObjectList<TL::Symbol>::const_iterator it = sym_list.begin();
         it != sym_list.end();
         it++)
    {
        add_used_types_rec(it->get_type(), dest_scope);
    }
}

Nodecl::List TL::Nanos6::duplicate_internal_subprograms(
        TL::ObjectList<Nodecl::NodeclBase> &internal_function_codes,
        TL::Scope scope_of_unpacked,
        Nodecl::Utils::SimpleSymbolMap &symbol_map)
{
    Nodecl::List output_statements;

    if (internal_function_codes.empty())
        return output_statements;

    for (TL::ObjectList<Nodecl::NodeclBase>::iterator
            it2 = internal_function_codes.begin();
            it2 != internal_function_codes.end();
            it2++)
    {
        ERROR_CONDITION(!it2->is<Nodecl::FunctionCode>(), "Invalid node", 0);

        TL::Symbol orig_sym = it2->get_symbol();

        TL::Symbol new_sym = scope_of_unpacked.new_symbol(orig_sym.get_name());
        symbol_map.add_map(orig_sym, new_sym);

        Nodecl::NodeclBase copied_node = 
                Nodecl::Utils::deep_copy(*it2,
                    scope_of_unpacked,
                    symbol_map);

        output_statements.append(copied_node);
    }

    return output_statements;
}

namespace
{

TL::Symbol fortran_new_vla_var(TL::Symbol sym,
                               TL::Scope sc,
                               Nodecl::Utils::SimpleSymbolMap &symbol_map)
{
    TL::Symbol new_vla_var = sc.new_symbol(sym.get_name());
    new_vla_var.get_internal_symbol()->kind = SK_VARIABLE;
    new_vla_var.set_type(sym.get_type());

    symbol_entity_specs_set_is_saved_expression(
        new_vla_var.get_internal_symbol(), 1);

    new_vla_var.set_value(
        Nodecl::Utils::deep_copy(sym.get_value(), sc, symbol_map));

    return new_vla_var;
}
}

void TL::Nanos6::fortran_add_extra_mappings_for_vla_types(
    TL::Type t, Scope sc, Nodecl::Utils::SimpleSymbolMap &symbol_map)
{
    if (!t.is_valid())
        return;

    if (t.is_array())
    {
        fortran_add_extra_mappings_for_vla_types(
            t.array_element(), sc, symbol_map);

        Nodecl::NodeclBase lower_bound, upper_bound;
        t.array_get_bounds(lower_bound, upper_bound);

        if (lower_bound.is<Nodecl::Symbol>()
            && lower_bound.get_symbol().is_saved_expression()
            // Not mapped already
            && symbol_map.map(lower_bound.get_symbol())
                   == lower_bound.get_symbol())
        {
            TL::Symbol new_vla_var
                = fortran_new_vla_var(lower_bound.get_symbol(), sc, symbol_map);

            symbol_map.add_map(lower_bound.get_symbol(), new_vla_var);
        }
        if (upper_bound.is<Nodecl::Symbol>()
            && upper_bound.get_symbol().is_saved_expression()
            // Not mapped already
            && symbol_map.map(upper_bound.get_symbol())
                   == upper_bound.get_symbol())
        {
            TL::Symbol new_vla_var
                = fortran_new_vla_var(upper_bound.get_symbol(), sc, symbol_map);

            symbol_map.add_map(upper_bound.get_symbol(), new_vla_var);
        }
    }
    else if (t.is_any_reference())
    {
        fortran_add_extra_mappings_for_vla_types(t.no_ref(), sc, symbol_map);
    }
    else if (t.is_pointer())
    {
        fortran_add_extra_mappings_for_vla_types(t.points_to(), sc, symbol_map);
    }
}
