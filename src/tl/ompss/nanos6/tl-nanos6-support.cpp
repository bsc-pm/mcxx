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


#include "tl-nanos6-support.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-symbol.hpp"
#include "tl-source.hpp"
#include "tl-type.hpp"
#include "cxx-cexpr.h"

namespace
{
TL::Symbol clone_vla_var(TL::Symbol sym,
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

void add_extra_mapping_for_dimension(Nodecl::NodeclBase vla_var,
                                     TL::Scope sc,
                                     /* out */
                                     Nodecl::Utils::SimpleSymbolMap &symbol_map,
                                     TL::ObjectList<TL::Symbol> &new_vlas)
{
    if (vla_var.is<Nodecl::Symbol>()
        && vla_var.get_symbol().is_saved_expression()
        // Not mapped already
        && symbol_map.map(vla_var.get_symbol()) == vla_var.get_symbol())
    {
        TL::Symbol new_vla_var
            = clone_vla_var(vla_var.get_symbol(), sc, symbol_map);

        symbol_map.add_map(vla_var.get_symbol(), new_vla_var);
        new_vlas.append(new_vla_var);
    }
}
}

namespace TL { namespace Nanos6 {

    TL::Symbol get_nanos6_class_symbol(const std::string &name)
    {
        TL::Symbol struct_sym = TL::Scope::get_global_scope().get_symbol_from_name(name);

        ERROR_CONDITION(!struct_sym.is_valid() ||
                        !(struct_sym.is_typedef() || struct_sym.is_class()),
                        "Symbol '%s' not found", name.c_str());

        return struct_sym;
    }

    TL::Symbol get_nanos6_function_symbol(const std::string &name)
    {
        TL::Symbol fun_sym = TL::Scope::get_global_scope().get_symbol_from_name(name);

        ERROR_CONDITION(!fun_sym.is_valid() ||
                !fun_sym.is_function(),
                "Symbol '%s' not found", name.c_str());

        return fun_sym;
    }

    void add_extra_mappings_for_vla_types(
            TL::Type t,
            TL::Scope sc,
            /* out */
            Nodecl::Utils::SimpleSymbolMap &symbol_map,
            TL::ObjectList<TL::Symbol> &new_vlas)
    {
        if (!t.is_valid())
            return;

        if (t.is_array())
        {
            add_extra_mappings_for_vla_types(
                    t.array_element(), sc, symbol_map, new_vlas);

            if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase lower_bound, upper_bound;
                t.array_get_bounds(lower_bound, upper_bound);

                add_extra_mapping_for_dimension(
                        lower_bound, sc, symbol_map, new_vlas);
                add_extra_mapping_for_dimension(
                        upper_bound, sc, symbol_map, new_vlas);
            }
            else if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase size = t.array_get_size();

                add_extra_mapping_for_dimension(size, sc, symbol_map, new_vlas);
            }
        }
        else if (t.is_any_reference())
        {
            add_extra_mappings_for_vla_types(t.no_ref(), sc, symbol_map, new_vlas);
        }
        else if (t.is_pointer())
        {
            add_extra_mappings_for_vla_types(
                    t.points_to(), sc, symbol_map, new_vlas);
        }
    }

    Nodecl::NodeclBase compute_call_to_nanos6_bzero(Nodecl::NodeclBase pointer_expr_to_be_initialized)
    {
        TL::Type type = pointer_expr_to_be_initialized.get_type().no_ref();
        ERROR_CONDITION(!type.is_pointer(), "This type should be a pointer type", 0);

        type = type.points_to();

        Nodecl::NodeclBase num_bytes;
        if (type.is_dependent())
        {
            num_bytes = Nodecl::Sizeof::make(
                    Nodecl::Type::make(type, pointer_expr_to_be_initialized.get_locus()),
                    Nodecl::NodeclBase::null(),
                    TL::Type::get_size_t_type(),
                    pointer_expr_to_be_initialized.get_locus());
        }
        else
        {
            num_bytes = const_value_to_nodecl_with_basic_type(
                    const_value_get_integer(
                        type.get_size(),
                        /* bytes */TL::Type::get_size_t_type().get_size(),
                        /* sign */ 0),
                    TL::Type::get_size_t_type().get_internal_type());
        }

        TL::Symbol nanos6_bzero_sym = get_nanos6_function_symbol("nanos6_bzero");
        Nodecl::NodeclBase call_to_nanos6_bzero =
            Nodecl::ExpressionStatement::make(
                    Nodecl::FunctionCall::make(
                        nanos6_bzero_sym.make_nodecl( /* set_ref_type */ true),
                        Nodecl::List::make(
                            pointer_expr_to_be_initialized,
                            num_bytes),
                        /* alternate symbol */ Nodecl::NodeclBase::null(),
                        /* alternate symbol */ Nodecl::NodeclBase::null(),
                        TL::Type::get_void_type(),
                        pointer_expr_to_be_initialized.get_locus()),
                    pointer_expr_to_be_initialized.get_locus());

        return call_to_nanos6_bzero;
    }
}}
