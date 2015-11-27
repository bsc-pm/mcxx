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

namespace TL {

TL::Symbol GOMP::new_private_symbol(TL::Symbol original_symbol, TL::Scope private_scope)
{
    return new_private_symbol(original_symbol.get_name(),
            original_symbol.get_type(),
            original_symbol.get_internal_symbol()->kind,
            private_scope);
}

TL::Symbol GOMP::new_private_symbol(const std::string& base_name,
        TL::Type type,
        enum cxx_symbol_kind kind,
        TL::Scope private_scope)
{
    TL::Counter &private_num = TL::CounterManager::get_counter("gomp-omp-privates");

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

void GOMP::gather_vla_symbols(TL::Symbol symbol,
        TL::ObjectList<TL::Symbol>& extra_symbols)
{
    gather_vla_symbol_type(symbol.get_type(), extra_symbols);
}

TL::Type GOMP::create_outline_struct(
        const TL::ObjectList<TL::Symbol>& all_passed_symbols,
        TL::Symbol enclosing_function,
        const locus_t* locus)
{
    std::string structure_name;
    {
        Counter& counter = CounterManager::get_counter("gomp-struct");
        std::stringstream ss;
        ss << "gomp_ol_args_" << (int)counter << "_t";
        counter++;
        structure_name = ss.str();
    }

    if (IS_C_LANGUAGE)
    {
        structure_name = "struct " + structure_name;
    }

    TL::Scope current_scope = enclosing_function.get_scope();

    TL::Symbol new_class_symbol = current_scope.new_symbol(structure_name);
    new_class_symbol.get_internal_symbol()->kind = SK_CLASS;
    type_t* new_class_type = get_new_class_type(current_scope.get_decl_context(), TT_STRUCT);
    symbol_entity_specs_set_is_user_declared(new_class_symbol.get_internal_symbol(), 1);
    const decl_context_t* class_context = new_class_context(new_class_symbol.get_scope().get_decl_context(),
            new_class_symbol.get_internal_symbol());
    class_type_set_inner_context(new_class_type, class_context);
    new_class_symbol.get_internal_symbol()->type_information = new_class_type;

    // Add members
    TL::Scope class_scope(class_context);

    for (TL::ObjectList<TL::Symbol>::const_iterator it = all_passed_symbols.begin();
            it != all_passed_symbols.end();
            it++)
    {
        std::string field_name = it->get_name();
        TL::Symbol field = class_scope.new_symbol(field_name);
        field.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(field.get_internal_symbol(), 1);

        TL::Type field_type = it->get_type().get_pointer_to();
        field.get_internal_symbol()->type_information = field_type.get_internal_type();

        symbol_entity_specs_set_is_member(field.get_internal_symbol(), 1);
        symbol_entity_specs_set_class_type(field.get_internal_symbol(),
                ::get_user_defined_type(new_class_symbol.get_internal_symbol()));
        symbol_entity_specs_set_access(field.get_internal_symbol(), AS_PUBLIC);

        field.get_internal_symbol()->locus = locus;

        class_type_add_member(new_class_type,
                field.get_internal_symbol(),
                class_scope.get_decl_context(),
                /* is_definition */ 1);
    }

    nodecl_t nodecl_output = nodecl_null();
    finish_class_type(new_class_type,
            ::get_user_defined_type(new_class_symbol.get_internal_symbol()),
            current_scope.get_decl_context(),
            locus,
            &nodecl_output);
    set_is_complete_type(new_class_type, /* is_complete */ 1);
    set_is_complete_type(get_actual_class_type(new_class_type), /* is_complete */ 1);

    if (!nodecl_is_null(nodecl_output))
    {
        std::cerr << "FIXME: finished class issues nonempty nodecl" << std::endl;
    }

    return new_class_symbol.get_user_defined_type();
}

TL::Type GOMP::create_outline_struct_task(
        const TL::ObjectList<TL::Symbol>& all_passed_symbols,
        const TL::ObjectList<TL::Symbol>& firstprivate_symbols,
        TL::Symbol enclosing_function,
        const locus_t* locus)
{
    std::string structure_name;
    {
        Counter& counter = CounterManager::get_counter("gomp-struct");
        std::stringstream ss;
        ss << "gomp_ol_args_" << (int)counter << "_t";
        counter++;
        structure_name = ss.str();
    }

    if (IS_C_LANGUAGE)
    {
        structure_name = "struct " + structure_name;
    }

    TL::Scope current_scope = enclosing_function.get_scope();

    TL::Symbol new_class_symbol = current_scope.new_symbol(structure_name);
    new_class_symbol.get_internal_symbol()->kind = SK_CLASS;
    type_t* new_class_type = get_new_class_type(current_scope.get_decl_context(), TT_STRUCT);
    symbol_entity_specs_set_is_user_declared(new_class_symbol.get_internal_symbol(), 1);
    const decl_context_t* class_context = new_class_context(new_class_symbol.get_scope().get_decl_context(),
            new_class_symbol.get_internal_symbol());
    class_type_set_inner_context(new_class_type, class_context);
    new_class_symbol.get_internal_symbol()->type_information = new_class_type;

    // Add members
    TL::Scope class_scope(class_context);

    for (TL::ObjectList<TL::Symbol>::const_iterator it = all_passed_symbols.begin();
            it != all_passed_symbols.end();
            it++)
    {
        std::string field_name = it->get_name();
        TL::Symbol field = class_scope.new_symbol(field_name);
        field.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(field.get_internal_symbol(), 1);

        TL::Type field_type = it->get_type();

        if (!firstprivate_symbols.contains(*it))
            field_type = field_type.get_pointer_to();

        field.get_internal_symbol()->type_information = field_type.get_internal_type();

        symbol_entity_specs_set_is_member(field.get_internal_symbol(), 1);
        symbol_entity_specs_set_class_type(field.get_internal_symbol(),
                ::get_user_defined_type(new_class_symbol.get_internal_symbol()));
        symbol_entity_specs_set_access(field.get_internal_symbol(), AS_PUBLIC);

        field.get_internal_symbol()->locus = locus;

        class_type_add_member(new_class_type,
                field.get_internal_symbol(),
                class_scope.get_decl_context(),
                /* is_definition */ 1);
    }

    nodecl_t nodecl_output = nodecl_null();
    finish_class_type(new_class_type,
            ::get_user_defined_type(new_class_symbol.get_internal_symbol()),
            current_scope.get_decl_context(),
            locus,
            &nodecl_output);
    set_is_complete_type(new_class_type, /* is_complete */ 1);
    set_is_complete_type(get_actual_class_type(new_class_type), /* is_complete */ 1);

    if (!nodecl_is_null(nodecl_output))
    {
        std::cerr << "FIXME: finished class issues nonempty nodecl" << std::endl;
    }

    return new_class_symbol.get_user_defined_type();
}

} // TL
