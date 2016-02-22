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
#include "tl-symbol-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-symbol.hpp"
#include "tl-source.hpp"
#include "tl-type.hpp"
#include "tl-compilerpipeline.hpp"
#include "cxx-cexpr.h"
#include "fortran03-buildscope.h"
#include "fortran03-typeutils.h"
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
TL::Type get_fake_explicit_shape_array(TL::Type t)
{
    if (t.is_fortran_array())
    {
        Nodecl::NodeclBase lower, upper;

        t.array_get_bounds(lower, upper);

        TL::Type element_type
            = get_fake_explicit_shape_array(t.array_element());

        if (t.array_requires_descriptor())
        {
            return element_type.get_array_to_with_descriptor(
                Nodecl::NodeclBase::null(),
                Nodecl::NodeclBase::null(),
                CURRENT_COMPILED_FILE->global_decl_context);
        }
        else
        {
            return element_type.get_array_to(
                const_value_to_nodecl(const_value_get_one(4, 1)),
                const_value_to_nodecl(const_value_get_one(4, 1)),
                CURRENT_COMPILED_FILE->global_decl_context);
        }
    }
    else
    {
        return t;
    }
}

// This is for Fortran only
TL::Symbol get_function_ptr_of_impl(std::string name,
                                    TL::Symbol sym,
                                    TL::Type return_type,
                                    TL::Type arg_type,
                                    bool lvalue_param,
                                    TL::Scope original_scope,
                                    /* out */ Nodecl::List &extra_c_code)
{
    static int num = 0;

    // FIXME - Avoid creating functions twice for a same arg_type
    std::stringstream ss;
    ss << name << "_" << std::hex
       << simple_hash_str(TL::CompilationProcess::get_current_file()
                              .get_filename(/* fullpath */ true)
                              .c_str()) << std::dec << "_" << num;

    num++;

    if (arg_type.is_any_reference())
        arg_type = arg_type.references_to();

    TL::ObjectList<std::string> parameter_names;
    parameter_names.append("nanox_target_phony");

    TL::Type argument_type = arg_type;

    if (arg_type.is_pointer())
    {
        // Do nothing. Use the original type
    }
    else if (arg_type.is_array())
    {
        argument_type = get_fake_explicit_shape_array(arg_type);
    }

    if (lvalue_param)
        argument_type = argument_type.get_lvalue_reference_to();

    TL::ObjectList<TL::Type> parameter_types;
    parameter_types.append(argument_type);

    TL::Symbol result = SymbolUtils::new_function_symbol(
        CURRENT_COMPILED_FILE->global_decl_context,
        ss.str(),
        /* result_name */ "nanox_pointer_phony",
        return_type,
        parameter_names,
        parameter_types);

    TL::ObjectList<TL::Symbol> parameters = result.get_related_symbols();
    // Propagate ALLOCATABLE attribute
    symbol_entity_specs_set_is_allocatable(parameters[0].get_internal_symbol(),
                                           sym.is_valid()
                                               && sym.is_allocatable());

    // Make sure we have a proper module info in the context of the parameter
    scope_entry_t *new_used_modules_info
        = ::get_or_create_used_modules_symbol_info(
            parameters[0].get_scope().get_decl_context());

    type_t *basic_type
        = no_ref(parameters[0].get_internal_symbol()->type_information);

    while (is_pointer_type(basic_type) || fortran_is_array_type(basic_type))
    {
        if (is_pointer_type(basic_type))
            basic_type = pointer_type_get_pointee_type(basic_type);
        else if (fortran_is_array_type(basic_type))
            basic_type = array_type_get_element_type(basic_type);
    }

    // The type may come from a module, emit a USE
    if (is_named_class_type(basic_type)
        && (symbol_entity_specs_get_in_module(named_type_get_symbol(basic_type))
            || symbol_entity_specs_get_from_module(
                   named_type_get_symbol(basic_type))))
    {
        scope_entry_t *orig_symbol = named_type_get_symbol(basic_type);

        scope_entry_t *module
            = symbol_entity_specs_get_from_module(orig_symbol);
        if (module == NULL)
            module = symbol_entity_specs_get_in_module(orig_symbol);

        // Insert the symbol from the module in the local scope
        scope_entry_t *used_symbol = insert_symbol_from_module(
            orig_symbol,
            parameters[0].get_scope().get_decl_context(),
            orig_symbol->symbol_name,
            module,
            NULL);

        // Update the type to refer to the USEd one and not the original
        // from the module
        parameters[0].get_internal_symbol()->type_information
            = fortran_update_basic_type_with_type(
                parameters[0].get_internal_symbol()->type_information,
                get_user_defined_type(used_symbol));

        // Add an explicit USE statement
        new_used_modules_info->value
            = nodecl_make_list_1(nodecl_make_fortran_use_only(
                nodecl_make_symbol(module, NULL),
                nodecl_make_list_1(nodecl_make_symbol(used_symbol, NULL)),
                NULL));
    }

    TL::Source src;
    src << "extern void* " << ss.str() << "_ (void*p)"
        << "{"
        << "   return p;"
        << "}";

    // Parse as C
    TL::Source::source_language = TL::SourceLanguage::C;
    Nodecl::List n = src.parse_global(original_scope).as<Nodecl::List>();
    TL::Source::source_language = TL::SourceLanguage::Current;

    extra_c_code.append(n);

    return result;
}

TL::Symbol get_copy_descriptor_function_impl(
    TL::Symbol dest_symbol,
    TL::Symbol source_symbol,
    TL::Scope original_scope,
    /* out */ Nodecl::List &extra_c_code)
{
    ERROR_CONDITION(
        !(source_symbol.get_type().no_ref().is_fortran_array()
          && source_symbol.get_type().no_ref().array_requires_descriptor())
            && !(source_symbol.get_type().no_ref().is_pointer()
                 && source_symbol.get_type()
                        .no_ref()
                        .points_to()
                        .is_fortran_array()),
        "Invalid source type it must be an array with descriptor or a pointer "
        "to array",
        0);
    ERROR_CONDITION(!dest_symbol.get_type().no_ref().is_fortran_array(),
                    "Invalid dest types, it must be an array",
                    0);
    ERROR_CONDITION(
        dest_symbol.get_type().no_ref().array_requires_descriptor(),
        "Invalid dest symbol, it should be an array NOT requiring a descriptor",
        0);
    ERROR_CONDITION(
        !dest_symbol.get_type().no_ref().array_has_size()
            || !dest_symbol.get_type().no_ref().array_get_size().is_constant(),
        "Invalid dest symbol, it should have a constant size",
        0);

    static int num = 0;
    // FIXME - Avoid creating functions twice for the same source_symbol type
    std::stringstream ss;
    ss << "nanox_copy_arr_desc_" << std::hex
       << simple_hash_str(TL::CompilationProcess::get_current_file()
                              .get_filename(/* fullpath */ true)
                              .c_str()) << std::dec << "_" << num;

    num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    // 1. Dest parameter
    TL::Type argument_type = dest_symbol.get_type().no_ref();
    argument_type = argument_type.get_lvalue_reference_to();

    parameter_types.append(argument_type);
    parameter_names.append("nanox_descriptor_copy");

    // 2. Source parameter
    argument_type = source_symbol.get_type().no_ref();
    argument_type = argument_type.get_lvalue_reference_to();

    parameter_types.append(argument_type);
    parameter_names.append("nanox_descriptor_source");

    // -- Create function
    TL::Symbol result = SymbolUtils::new_function_symbol(
        CURRENT_COMPILED_FILE->global_decl_context,
        ss.str(),
        /* return_name */ "",
        TL::Type::get_void_type(),
        parameter_names,
        parameter_types);

    TL::ObjectList<TL::Symbol> parameters = result.get_related_symbols();
    // Propagate ALLOCATABLE attribute of orig_symbol
    symbol_entity_specs_set_is_allocatable(
        parameters[1].get_internal_symbol(),
        source_symbol.is_valid() && source_symbol.is_allocatable());

    // Make sure we have a proper module info in the context of the parameter
    scope_entry_t *new_used_modules_info
        = ::get_or_create_used_modules_symbol_info(
            parameters[1].get_scope().get_decl_context());

    type_t *basic_type
        = no_ref(parameters[1].get_internal_symbol()->type_information);

    while (is_pointer_type(basic_type) || fortran_is_array_type(basic_type))
    {
        if (is_pointer_type(basic_type))
            basic_type = pointer_type_get_pointee_type(basic_type);
        else if (fortran_is_array_type(basic_type))
            basic_type = array_type_get_element_type(basic_type);
    }

    // The type may come from a module, emit a USE
    if (is_named_class_type(basic_type)
        && (symbol_entity_specs_get_in_module(named_type_get_symbol(basic_type))
            || symbol_entity_specs_get_from_module(
                   named_type_get_symbol(basic_type))))
    {
        scope_entry_t *orig_symbol = named_type_get_symbol(basic_type);

        scope_entry_t *module
            = symbol_entity_specs_get_from_module(orig_symbol);
        if (module == NULL)
            module = symbol_entity_specs_get_in_module(orig_symbol);

        // Insert the symbol from the module in the local scope
        scope_entry_t *used_symbol = insert_symbol_from_module(
            orig_symbol,
            parameters[1].get_scope().get_decl_context(),
            orig_symbol->symbol_name,
            module,
            NULL);

        // Update the type to refer to the USEd one and not the original
        // from the module
        parameters[1].get_internal_symbol()->type_information
            = fortran_update_basic_type_with_type(
                parameters[1].get_internal_symbol()->type_information,
                get_user_defined_type(used_symbol));

        // Add an explicit USE statement
        new_used_modules_info->value
            = nodecl_make_list_1(nodecl_make_fortran_use_only(
                nodecl_make_symbol(module, NULL),
                nodecl_make_list_1(nodecl_make_symbol(used_symbol, NULL)),
                NULL));
    }

    TL::Source src;
    src << "extern void " << ss.str() << "_ (void *dest, void *source)"
        << "{"
        << "__builtin_memcpy(dest, source, "
        << dest_symbol.get_type().no_ref().get_size() << ");"
        << "}";

    // Parse as C
    TL::Source::source_language = TL::SourceLanguage::C;
    Nodecl::List n = src.parse_global(original_scope).as<Nodecl::List>();
    TL::Source::source_language = TL::SourceLanguage::Current;

    extra_c_code.append(n);

    return result;
}
}

TL::Symbol TL::Nanos6::fortran_get_function_ptr_of(TL::Symbol sym,
                                                   TL::Scope original_scope,
                                                   Nodecl::List &extra_c_code)
{
    return get_function_ptr_of_impl(
        "nanox_ptr_of",
        sym,
        /* return_type */ TL::Type::get_void_type().get_pointer_to(),
        /* argument_type */ sym.get_type(),
        /* lvalue_param */ true,
        original_scope,
        extra_c_code);
}

TL::Symbol TL::Nanos6::fortran_get_function_ptr_of(TL::Type t,
                                                   TL::Scope original_scope,
                                                   Nodecl::List &extra_c_code)
{
    return get_function_ptr_of_impl(
        "nanox_ptr_of",
        Symbol(NULL),
        /* return_type */ TL::Type::get_void_type().get_pointer_to(),
        /* argument_type */ t,
        /* lvalue_param */ true,
        original_scope,
        extra_c_code);
}

// Returns a function which converts the argument type into the return
// type. If the lvalue_param is true, the argument will be passed by
// reference to the function. Otherwise, It will be passed by value
TL::Symbol TL::Nanos6::fortran_get_function_ptr_conversion(
    TL::Type return_type,
    TL::Type argument_type,
    TL::Scope original_scope,
    Nodecl::List &extra_c_code)
{
    return get_function_ptr_of_impl("nanox_ptr_conversion",
                                    Symbol(NULL),
                                    return_type,
                                    argument_type,
                                    /*lvalue_param*/ false,
                                    original_scope,
                                    extra_c_code);
}

TL::Symbol TL::Nanos6::fortran_get_copy_descriptor_function(
    TL::Symbol dest_symbol,
    TL::Symbol source_symbol,
    TL::Scope original_scope,
    Nodecl::List &extra_c_code)
{
    return get_copy_descriptor_function_impl(
        dest_symbol, source_symbol, original_scope, extra_c_code);
}
