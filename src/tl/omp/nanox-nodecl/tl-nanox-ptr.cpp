/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-nanox-ptr.hpp"
#include "tl-nanox-nodecl.hpp"

#include "tl-scope.hpp"
#include "tl-symbol-utils.hpp"

#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-scope.h"

#include "fortran03-scope.h"
#include "fortran03-typeutils.h"
#include "fortran03-buildscope.h"

#include "tl-compilerpipeline.hpp"

#include "tl-nodecl.hpp"
#include "tl-source.hpp"

namespace TL { namespace Nanox {

    namespace
    {
        TL::Type get_fake_explicit_shape_array(TL::Type t)
        {
            if (t.is_array())
            {
                Nodecl::NodeclBase lower, upper;

                t.array_get_bounds(lower, upper);

                TL::Type element_type = get_fake_explicit_shape_array(t.array_element());

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
        TL::Symbol get_function_ptr_of_impl(TL::Symbol sym, TL::Type t, TL::Scope original_scope)
        {
            static int num = 0;

            // FIXME - Avoid creating functions twice for a same t
            std::stringstream ss;
            ss << "nanox_ptr_of_" 
                << std::hex 
                << simple_hash_str(TL::CompilationProcess::get_current_file().get_filename(/* fullpath */ true).c_str())
                << std::dec
                << "_" 
                << num;

            num++;

            if (t.is_any_reference())
                t = t.references_to();

            TL::Type return_type = TL::Type::get_void_type().get_pointer_to();

            ObjectList<std::string> parameter_names;
            parameter_names.append("nanox_target_phony");

            TL::Type argument_type = t;

            if (t.is_pointer())
            {
                // Do nothing. Use the original type
            }
            else if (t.is_array())
            {
                argument_type = get_fake_explicit_shape_array(t);
            }

            argument_type = argument_type.get_lvalue_reference_to();

            ObjectList<TL::Type> parameter_types;
            parameter_types.append(argument_type);

            TL::Symbol result = SymbolUtils::new_function_symbol(
                    CURRENT_COMPILED_FILE->global_decl_context,
                    ss.str(),
                    /* has_return */ true,
                    /* return_name */ "nanox_pointer_phony",
                    return_type,
                    parameter_names,
                    parameter_types);

            ObjectList<TL::Symbol> parameters = result.get_related_symbols();
            // Propagate ALLOCATABLE attribute
            symbol_entity_specs_set_is_allocatable(parameters[0].get_internal_symbol(),
                    sym.is_valid() && sym.is_allocatable());

            // Make sure we have a proper module info in the context of the parameter
            scope_entry_t* new_used_modules_info =
                ::get_or_create_used_modules_symbol_info(parameters[0].get_scope().get_decl_context());

            type_t* basic_type = no_ref(parameters[0].get_internal_symbol()->type_information);

            while (is_pointer_type(basic_type)
                    || fortran_is_array_type(basic_type))
            {
                if (is_pointer_type(basic_type))
                    basic_type = pointer_type_get_pointee_type(basic_type);
                else if (fortran_is_array_type(basic_type))
                    basic_type = array_type_get_element_type(basic_type);
            }

            // The type may come from a module, emit a USE
            if (is_named_class_type(basic_type)
                    && (symbol_entity_specs_get_in_module(named_type_get_symbol(basic_type))
                        || symbol_entity_specs_get_from_module(named_type_get_symbol(basic_type))))
            {
                scope_entry_t* orig_symbol =
                        named_type_get_symbol(basic_type);

                scope_entry_t* module =
                    symbol_entity_specs_get_from_module(orig_symbol);
                if (module == NULL)
                    module = symbol_entity_specs_get_in_module(orig_symbol);

                // Insert the symbol from the module in the local scope
                scope_entry_t* used_symbol = insert_symbol_from_module(
                        orig_symbol,
                        parameters[0].get_scope().get_decl_context(),
                        orig_symbol->symbol_name,
                        module,
                        NULL);

                // Update the type to refer to the USEd one and not the original
                // from the module
                parameters[0].get_internal_symbol()->type_information =
                    fortran_update_basic_type_with_type(
                            parameters[0].get_internal_symbol()->type_information,
                            get_user_defined_type(used_symbol));

                // Add an explicit USE statement
                new_used_modules_info->value = nodecl_make_list_1(
                        nodecl_make_fortran_use_only(
                            nodecl_make_symbol(module, NULL),
                            nodecl_make_list_1(
                                nodecl_make_symbol(used_symbol, NULL)),
                            NULL));

            }

            Source src;
            src << "extern void* " << ss.str() << "_ (void*p)"
                << "{"
                << "   return p;"
                << "}"
                ;

            // Parse as C
            Source::source_language = SourceLanguage::C;
            Nodecl::List n = src.parse_global(original_scope).as<Nodecl::List>();
            Source::source_language = SourceLanguage::Current;

            Nodecl::List& extra_c_code = Lowering::get_extra_c_code();

            extra_c_code.append(n);


            return result;
        }

    }

} }

namespace TL {

        TL::Symbol Nanox::get_function_ptr_of(TL::Symbol sym, TL::Scope original_scope)
        {
            return get_function_ptr_of_impl(sym, sym.get_type(), original_scope);
        }

        TL::Symbol Nanox::get_function_ptr_of(TL::Type t, TL::Scope original_scope)
        {
            return get_function_ptr_of_impl(Symbol(NULL), t, original_scope);
        }
} 

