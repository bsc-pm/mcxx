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


#ifndef TL_NANOS6_SUPPORT_HPP
#define TL_NANOS6_SUPPORT_HPP

#include "tl-nanos6.hpp"

#include "tl-objectlist.hpp"
#include "tl-scope.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"

#include <fortran03-scope.h>
#include <fortran03-intrinsics.h>


namespace TL
{
namespace Nanos6
{
    namespace
    {
        template < unsigned int num_arguments>
        TL::Symbol get_fortran_intrinsic_symbol(const std::string &name, const Nodecl::List& actual_arguments, bool is_call)
        {
            // Note that this function is template to avoid to use VLAs in C++ or dynamic memory allocation
            nodecl_t arguments[num_arguments];

            int index = 0;
            for (Nodecl::List::const_iterator it = actual_arguments.begin();
                    it != actual_arguments.end();
                    it++)
            {
                arguments[index++]=it->get_internal_nodecl();
            }
            TL::Symbol intrinsic(
                    fortran_solve_generic_intrinsic_call(
                        fortran_query_intrinsic_name_str(TL::Scope::get_global_scope().get_decl_context(), name.c_str()),
                        arguments,
                        num_arguments,
                        is_call));

            return intrinsic;
        }
    }

    TL::Symbol get_nanos6_class_symbol(const std::string &name);
    TL::Symbol get_nanos6_function_symbol(const std::string &name);

    void add_extra_mappings_for_vla_types(
            TL::Type t,
            Scope sc,
            /* out */
            Nodecl::Utils::SimpleSymbolMap &symbol_map,
            TL::ObjectList<TL::Symbol> &vla_vars);

    Nodecl::NodeclBase compute_call_to_nanos6_bzero(Nodecl::NodeclBase pointer_expr_to_be_initialized);

    void create_static_variable_depending_on_function_context(
        const std::string &var_name,
        TL::Type var_type,
        Nodecl::NodeclBase context,
        LoweringPhase* phase,
        /* out */
        TL::Symbol &new_var);


    //! Create a detached symbol with the same name as the real one We need to
    //! do that otherwise Fortran codegen attempts to initialize this symbol
    //! (We may want to fix this somehow)
    Symbol fortran_create_detached_symbol_from_static_symbol(
        Symbol &static_symbol);

    Scope compute_scope_for_environment_structure(Symbol related_function);

    Symbol add_field_to_class(Symbol new_class_symbol,
        Scope class_scope,
        const std::string &var_name,
        const locus_t *var_locus,
        bool is_allocatable,
        Type field_type);


    struct EnvironmentCapture {
    private:
        typedef std::map<TL::Symbol, TL::Symbol> field_map_t;
        typedef std::map<TL::Symbol, TL::Symbol> array_descriptor_map_t;

        const locus_t* _originating_locus;
        Nodecl::NodeclBase _originating_context;

        TL::Scope _class_outer_scope;
        TL::Symbol _class_symbol;
        type_t* _inner_class_type;
        TL::Scope _class_scope;
        TL::Type _class_type;
        Nodecl::NodeclBase _size;

        bool _requires_initialization;
        bool _requires_duplication_function;
        bool _has_vlas;
        bool _requires_destruction_function;

        // It maps each captured symbol with its respective symbol in the arguments structure
        Nodecl::Utils::SimpleSymbolMap _captured_symbols_map;

        field_map_t _field_map;
        array_descriptor_map_t _array_descriptor_map;

        // This nodecl represents the extra storage that the runtime has to
        // allocate contiguously to the arguments structure to support VLAs
        Nodecl::NodeclBase _extra_storage;

        void clear();

    public:
        enum {
            VLA_OVERALLOCATION_ALIGN = 8
        };

        struct Accessor
        {
            TL::Symbol _original_symbol;

            TL::Symbol _environment_symbol;
            std::string _environment_name;
            TL::Type _environment_type;
            Nodecl::NodeclBase _environment_access;
        };

        EnvironmentCapture()
        {
            clear();
        }


        /*** Build the type ***/

        void begin_type_setup(
            std::string structure_name,
            TL::Symbol related_function,
            const locus_t* originating_locus,
            Nodecl::NodeclBase originating_context);

        void add_storage_for_private_symbol(TL::Symbol symbol);
        void add_storage_for_shared_symbol(TL::Symbol symbol);

        TL::Type end_type_setup();

        /********/


        // Expression with the size of the structure
        Nodecl::NodeclBase get_size() const
        {
            return _size;
        }


        Nodecl::NodeclBase rewrite_expression_using_args(
            TL::Symbol arg,
            Nodecl::NodeclBase expr,
            const TL::ObjectList<TL::Symbol>& shared,
            const TL::ObjectList<TL::Symbol>& local) const;

        TL::Type rewrite_type_using_args(
            TL::Symbol arg,
            TL::Type t,
            const TL::ObjectList<TL::Symbol>& shared,
            const TL::ObjectList<TL::Symbol>& local) const;


        // In the next two methods "object" can be either a structure instance or a pointer to it
        Accessor get_private_symbol_accessor(const TL::Symbol& object, const TL::Symbol& symbol, bool actual_storage_if_vla) const;
        Accessor get_shared_symbol_accessor(const TL::Symbol& object, const TL::Symbol& symbol) const;


        bool requires_initialization() const
        {
            return _requires_initialization;
        }
        bool requires_duplication_function() const
        {
            return _requires_duplication_function;
        }
        bool has_vlas() const
        {
            return _has_vlas;
        }
        bool requires_destruction_function() const
        {
            return _requires_destruction_function;
        }


        /*** structure to structure copies ***/

        // Emit the statements to perform the copy of a captured symbol from one structure to another
        Nodecl::List emit_copy_of_captured_symbol(
            TL::Scope context,
            const TL::Symbol& source_environment,
            const TL::Symbol& destination_environment,
            const TL::Symbol& original_symbol,
            /* inout */
            Nodecl::NodeclBase &vla_offset);

        // Emit the statements to perform the same allocation as in the source for an uncaptured private symbol
        Nodecl::List emit_copy_of_private_symbol_allocation(
            TL::Scope context,
            const TL::Symbol& source_environment,
            const TL::Symbol& destination_environment,
            const TL::Symbol& original_symbol,
            /* inout */
            Nodecl::NodeclBase &vla_offset);

        // Emit the statements to copy a reference to a shared symbol
        Nodecl::List emit_copy_of_shared_symbol_location(
            const TL::Symbol& source_environment,
            const TL::Symbol& destination_environment,
            const TL::Symbol& original_symbol);

        /********/


        /*** environment to structure copies ****/

        // Emit the statements to perform the capture of a captured symbol
        Nodecl::List emit_capture_of_captured_symbol(
            TL::Scope context,
            const TL::Symbol& destination_environment,
            const TL::Symbol& original_symbol,
            /* inout */
            Nodecl::NodeclBase &vla_offset);

        // Emit the statements to perform the allocation of a private (uncaptured) symbol
        Nodecl::List emit_private_symbol_allocation(
            TL::Scope context,
            const TL::Symbol& destination_environment,
            const TL::Symbol& original_symbol,
            /* inout */
            Nodecl::NodeclBase &vla_offset);

        // Emit the statements to capture a reference to a shared symbol
        Nodecl::List emit_caputure_of_shared_symbol_location(
            TL::Scope context,
            const TL::Symbol& destination_environment,
            const TL::Symbol& original_symbol,
            TL::Scope related_scope,
            /* inout */
            Nodecl::List &extra_c_code);

        /********/


        Nodecl::List emit_symbol_destruction(
            TL::Scope context,
            const TL::Symbol& source_environment,
            const TL::Symbol& original_symbol);
    };
}
}

#endif // TL_NANOS6_SUPPORT_HPP
