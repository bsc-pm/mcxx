/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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



#include "tl-lowering-utils.hpp"
#include "tl-type.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"

#include <sstream>

namespace TL { namespace Intel {

namespace
{
    TL::Type ident_t_type(NULL);
    TL::Type kmp_critical_name_type(NULL);
    TL::Type kmp_int32_type(NULL);
}

} // Intel

TL::Symbol Intel::new_global_ident_symbol(Nodecl::NodeclBase location)
{
    std::string filename = location.get_filename();
    int start_line = location.get_line();
    int end_line = location.get_line();

    if (!ident_t_type.is_valid())
    {
        ident_t_type = Source("ident_t").parse_c_type_id(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context));
    }
    if (!kmp_int32_type.is_valid())
    {
        kmp_int32_type = Source("kmp_int32").parse_c_type_id(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context));
    }

    ERROR_CONDITION(!ident_t_type.is_valid(), "Type ident_t not found", 0);

    TL::Counter &loc_num = TL::CounterManager::get_counter("intel-omp-ident");

    std::stringstream new_name;
    new_name << "_loc_" << (int)loc_num;
    loc_num++;

    // Due to location limitations of Mercurium itself end_line will always be start_line
    scope_entry_t* new_ident_sym = ::new_symbol(
            CURRENT_COMPILED_FILE->global_decl_context,
            CURRENT_COMPILED_FILE->global_decl_context->current_scope,
            uniquestr(new_name.str().c_str()));

    new_ident_sym->kind = SK_VARIABLE;
    new_ident_sym->type_information = ident_t_type.get_internal_type();
    symbol_entity_specs_set_is_user_declared(new_ident_sym, 1);
    new_ident_sym->defined = 1;
    symbol_entity_specs_set_is_static(new_ident_sym, 1);
    new_ident_sym->locus = make_locus(filename.c_str(), start_line, /* col */ 0);

    Source string_literal;
    string_literal << "\"" << filename << ";" << start_line << ";" << end_line << "\"";
    Nodecl::NodeclBase string_literal_tree =
        string_literal.parse_expression(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context));

    Nodecl::StructuredValue value = Nodecl::StructuredValue::make(
            Nodecl::List::make(
                /* reserved_1 */ Nodecl::IntegerLiteral::make(
                    kmp_int32_type,
                    const_value_get_zero(/* bytes */ 4, /* sign */1)),
                /* flags */ Nodecl::IntegerLiteral::make(
                    kmp_int32_type,
                    const_value_get_signed_int(2)), /* 0x02: c-style interface */
                /* reserved_2 */ Nodecl::IntegerLiteral::make(
                    kmp_int32_type,
                    const_value_get_zero(/* bytes */ 4, /* sign */1)),
                /* reserved_3 */ Nodecl::IntegerLiteral::make(
                    kmp_int32_type,
                    const_value_get_zero(/* bytes */ 4, /* sign */1)),
                /* psource */
                string_literal_tree),
            Nodecl::StructuredValueBracedImplicit::make(new_ident_sym->locus),
            ident_t_type,
            new_ident_sym->locus);

    new_ident_sym->value = value.get_internal_nodecl();

    Nodecl::NodeclBase object_init = Nodecl::ObjectInit::make(new_ident_sym);
    Nodecl::Utils::prepend_to_enclosing_top_level_location(location,
            object_init);

    return new_ident_sym;
}

TL::Symbol Intel::new_private_symbol(TL::Symbol original_symbol, TL::Scope private_scope)
{
    return new_private_symbol(original_symbol.get_name(),
            original_symbol.get_type(),
            original_symbol.get_internal_symbol()->kind,
            private_scope);
}

TL::Symbol Intel::new_private_symbol(const std::string& base_name,
        TL::Type type,
        enum cxx_symbol_kind kind,
        TL::Scope private_scope)
{
    TL::Counter &private_num = TL::CounterManager::get_counter("intel-omp-privates");

    std::stringstream new_name;
    new_name << "p_" << base_name << (int)private_num;
    private_num++;

    scope_entry_t* new_private_sym = ::new_symbol(
            private_scope.get_decl_context(),
            private_scope.get_decl_context()->current_scope,
            uniquestr(new_name.str().c_str()));

    new_private_sym->kind = kind;
    new_private_sym->type_information = type.get_internal_type();
    symbol_entity_specs_set_is_user_declared(new_private_sym, 1);
    new_private_sym->defined = 1;

    return new_private_sym;
}

namespace
{
    typedef std::map<std::string, TL::Symbol> lock_map_t;
    lock_map_t lock_map;
}

TL::Symbol Intel::get_global_lock_symbol(Nodecl::NodeclBase location, const std::string& name)
{
    TL::Symbol result;
    lock_map_t::iterator it = lock_map.find(name);
    if (it == lock_map.end())
    {
        if (!kmp_critical_name_type.is_valid())
        {
            kmp_critical_name_type = Source("kmp_critical_name")
                .parse_c_type_id(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context));
        }
        ERROR_CONDITION(!kmp_critical_name_type.is_valid(), "kmp_critical_name is not a valid type", 0);

        std::stringstream new_name;
        new_name << "_critical_" << name << "_";

        scope_entry_t* new_ident_sym = ::new_symbol(
                CURRENT_COMPILED_FILE->global_decl_context,
                CURRENT_COMPILED_FILE->global_decl_context->current_scope,
                uniquestr(new_name.str().c_str()));

        new_ident_sym->kind = SK_VARIABLE;
        new_ident_sym->type_information = kmp_critical_name_type.get_internal_type();
        symbol_entity_specs_set_is_user_declared(new_ident_sym, 1);
        new_ident_sym->defined = 1;
        new_ident_sym->locus = location.get_locus();

        gcc_attribute_t common_gcc_attr = { "common", nodecl_null() };

        symbol_entity_specs_add_gcc_attributes(new_ident_sym,
                common_gcc_attr);

        lock_map.insert(std::make_pair(name, new_ident_sym));

        Nodecl::StructuredValue value = Nodecl::StructuredValue::make(
                Nodecl::List::make(
                    Nodecl::IntegerLiteral::make(
                        kmp_int32_type,
                        const_value_get_zero(/* bytes */ 4, /* sign */1))
                    ),
                Nodecl::StructuredValueBracedImplicit::make(new_ident_sym->locus),
                kmp_critical_name_type,
                new_ident_sym->locus);
        new_ident_sym->value = value.get_internal_nodecl();

        result = new_ident_sym;

        CXX_LANGUAGE()
        {
            Nodecl::NodeclBase cxx_def = Nodecl::CxxDef::make(
                    Nodecl::NodeclBase::null(),
                    result,
                    location.get_locus());

            Nodecl::Utils::prepend_to_enclosing_top_level_location(location,
                    cxx_def);
        }
    }
    else
    {
        result = it->second;
    }

    return result;
}

TL::Symbol Intel::get_global_lock_symbol(Nodecl::NodeclBase location)
{
    return get_global_lock_symbol(location, "global");
}

void Intel::cleanup_lock_map()
{
    lock_map.clear();
}

static void gather_vla_symbol_type(TL::Type t,
        TL::ObjectList<TL::Symbol>& extra_symbols)
{
    if (!t.is_valid())
        return;

    if (t.is_array())
    {
        gather_vla_symbol_type(t.array_element(), extra_symbols);

        Nodecl::NodeclBase size = t.array_get_size();
        if (size.is<Nodecl::Symbol>()
                && size.get_symbol().is_saved_expression())
        {
            extra_symbols.insert(size.get_symbol());
        }
    }
    else if (t.is_pointer())
    {
        gather_vla_symbol_type(t.points_to(), extra_symbols);
    }
    else if (t.is_any_reference())
    {
        gather_vla_symbol_type(t.references_to(), extra_symbols);
    }
}

void Intel::gather_vla_symbols(TL::Symbol symbol,
        TL::ObjectList<TL::Symbol>& extra_symbols)
{
    gather_vla_symbol_type(symbol.get_type(), extra_symbols);
}

} // TL
