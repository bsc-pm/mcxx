/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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


#include "tl-nanos6-lower.hpp"
#include "tl-source.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"

namespace TL { namespace Nanos6 {

    void Lower::visit(const Nodecl::OpenMP::Critical& node)
    {
        walk(node.get_statements());

        Nodecl::NodeclBase environment = node.get_environment();

        std::string lock_name = "nanos_critical__lock_default";
        if (!environment.is_null())
        {
            Nodecl::NodeclBase critical_name_node = environment.as<Nodecl::List>().find_first<Nodecl::OpenMP::CriticalName>();
            if (!critical_name_node.is_null())
            {
                lock_name = "nanos_critical_lock_" + critical_name_node.get_text();
            }
        }

        TL::Symbol nanos_user_lock =
            TL::Scope::get_global_scope().get_symbol_from_name("nanos_user_lock");
        ERROR_CONDITION(!nanos_user_lock.is_valid()
                || !nanos_user_lock.is_function(),
                "Invalid symbol", 0);

        TL::Symbol nanos_user_unlock =
            TL::Scope::get_global_scope().get_symbol_from_name("nanos_user_unlock");
        ERROR_CONDITION(!nanos_user_unlock.is_valid()
                || !nanos_user_unlock.is_function(),
                "Invalid symbol", 0);

        TL::Symbol lock_sym = TL::Scope::get_global_scope().get_symbol_from_name(lock_name);
        if (!lock_sym.is_valid())
        {
            lock_sym = TL::Scope::get_global_scope().new_symbol(lock_name);
            lock_sym.get_internal_symbol()->kind = SK_VARIABLE;
            lock_sym.set_type(TL::Type::get_void_type().get_pointer_to());
            lock_sym.get_internal_symbol()->defined = 1;
            symbol_entity_specs_set_is_user_declared(lock_sym.get_internal_symbol(), 1);
            lock_sym.set_value(const_value_to_nodecl(const_value_get_signed_int(0)));

            gcc_attribute_t weak_attr = {"weak", nodecl_null()};
            symbol_entity_specs_add_gcc_attributes(
                    lock_sym.get_internal_symbol(),
                    weak_attr);

            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                Nodecl::Utils::prepend_to_enclosing_top_level_location(
                        node,
                        Nodecl::ObjectInit::make(
                            lock_sym)
                        );
                CXX_LANGUAGE()
                {
                    Nodecl::Utils::prepend_to_enclosing_top_level_location(
                            node,
                            Nodecl::CxxDef::make(
                                /* context */ Nodecl::NodeclBase::null(),
                                lock_sym)
                            );
                }
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                _phase->get_extra_c_code().append(
                        Nodecl::ObjectInit::make(lock_sym)
                        );
            }
        }

        const char* locus = locus_to_str(node.get_locus());

        Nodecl::List critical_tree;
        critical_tree.append(
                Nodecl::ExpressionStatement::make(
                    Nodecl::FunctionCall::make(
                        nanos_user_lock.make_nodecl(/* set_ref */ true),
                        Nodecl::List::make(
                            Nodecl::Reference::make(
                                lock_sym.make_nodecl(/* set_ref */ true),
                                lock_sym.get_type().get_pointer_to(),
                                node.get_locus()),
                            const_value_to_nodecl(
                                const_value_make_string_null_ended(
                                    locus,
                                    strlen(locus)))
                            ),
                        /* alternate-name */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        TL::Type::get_void_type(),
                        node.get_locus())));

        critical_tree.append(node.get_statements());

        critical_tree.append(
                Nodecl::ExpressionStatement::make(
                    Nodecl::FunctionCall::make(
                        nanos_user_unlock.make_nodecl(/* set_ref */ true),
                        Nodecl::List::make(
                            Nodecl::Reference::make(
                                lock_sym.make_nodecl(/* set_ref */ true),
                                lock_sym.get_type().get_pointer_to(),
                                node.get_locus())),
                        /* alternate-name */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        TL::Type::get_void_type(),
                        node.get_locus())));

        node.replace(critical_tree);
    }

} }
