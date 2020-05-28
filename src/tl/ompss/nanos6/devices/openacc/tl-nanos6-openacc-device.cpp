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

#include "tl-nanos6-openacc-device.hpp"
#include "tl-nanos6-support.hpp"

#include "tl-compilerpipeline.hpp"
#include "tl-nodecl-utils.hpp"

#include "tl-scope.hpp"
#include "tl-symbol.hpp"

#include "cxx-profile.h"
#include "cxx-cexpr.h"
#include "cxx-driver-utils.h"

#include <errno.h>
namespace TL { namespace Nanos6 {

OpenACCDevice::OpenACCDevice()
{}

OpenACCDevice::~OpenACCDevice()
{}

TL::Symbol OpenACCDevice::get_device_type_id() const
{
    TL::Symbol device_type_id = TL::Scope::get_global_scope().get_symbol_from_name("nanos6_openacc_device");

    ERROR_CONDITION(!device_type_id.is_valid(), "Invalid device type id", 0);
    return device_type_id;
}

bool OpenACCDevice::requires_arguments_translation() const
{
    return false;
}

Nodecl::NodeclBase OpenACCDevice::compute_specific_task_body(
        Nodecl::NodeclBase task_body,
        const DirectiveEnvironment &env,
        Nodecl::NodeclBase unpacked_function_code,
        const TL::Scope &unpacked_inside_scope,
        Nodecl::Utils::SimpleSymbolMap &symbol_map)
{
    Nodecl::NodeclBase new_task_body = task_body.shallow_copy();
    Nodecl::NodeclBase context = new_task_body.as<Nodecl::List>().front();
    ERROR_CONDITION(!context.is<Nodecl::Context>(),
            "Unexpected node\n", 0);

    Nodecl::NodeclBase compound_statement = context.as<Nodecl::Context>()
        .get_in_context().as<Nodecl::List>().front();
    ERROR_CONDITION(!compound_statement.is<Nodecl::CompoundStatement>(),
            "Unexpected node\n", 0);

    Nodecl::NodeclBase expr_stmt = compound_statement.as<Nodecl::CompoundStatement>()
        .get_statements().as<Nodecl::List>().front();
    ERROR_CONDITION(!expr_stmt.is<Nodecl::ExpressionStatement>(),
            "Unexpected node\n", 0);

    Nodecl::NodeclBase function_call = expr_stmt.as<Nodecl::ExpressionStatement>().get_nest();
    ERROR_CONDITION(!function_call.is<Nodecl::FunctionCall>(),
            "Unexpected node\n", 0);

    Nodecl::NodeclBase called = function_call.as<Nodecl::FunctionCall>().get_called();
    ERROR_CONDITION(function_call.is<Nodecl::Symbol>(),
            "Unexpected node\n", 0);

    // Get the *device_env* argument from the unpacked region
    TL::Symbol device_env = unpacked_inside_scope.get_symbol_from_name("device_env");

    // Detect the *nanos6_openacc_device_environment_t* and append definition to the code
    TL::Symbol dev_env_type_symbol = get_nanos6_class_symbol("nanos6_openacc_device_environment_t");
    context.prepend_sibling(
            Nodecl::CxxDef::make(
                Nodecl::NodeclBase::null(),
                dev_env_type_symbol));

    // Create the following statement: (declare and initialize new symbol)
    //
    // int async = (*((nanos6_openacc_device_environment_t *)device_env)).asyncId;
    //
    // env_expr will be for the right-hand side
    Nodecl::NodeclBase env_expr;
    env_expr = Nodecl::Conversion::make(
            device_env.make_nodecl(/* ref_type */true),
            dev_env_type_symbol.get_user_defined_type().get_pointer_to());
    env_expr.set_text("C");
    TL::ObjectList<TL::Symbol> fields = dev_env_type_symbol.get_type().get_nonstatic_data_members();

    TL::ObjectList<TL::Symbol> aux = fields.find<std::string>(&TL::Symbol::get_name, "asyncId");
    ERROR_CONDITION(aux.size() == 0, "'asyncId' member was not found", 0);
    // Now dereference the asyncId field:
    env_expr = Nodecl::ClassMemberAccess::make(
            Nodecl::Dereference::make(
                env_expr,
                env_expr.get_type().points_to().get_lvalue_reference_to()),
            aux[0].make_nodecl(/* ref_type */true),
            /* member literal */ Nodecl::NodeclBase::null(),
            aux[0].get_type().get_lvalue_reference_to());

    TL::Scope inner_scope = context.retrieve_context();

    // Create the new symbol for the left-hand side
    TL::Symbol async = inner_scope.new_symbol("nanos6_mcxx_async");
    async.get_internal_symbol()->kind = SK_VARIABLE;
    async.set_type(TL::Type::get_int_type());
    symbol_entity_specs_set_is_user_declared(async.get_internal_symbol(), 1);
    if (IS_CXX_LANGUAGE || IS_C_LANGUAGE)
        expr_stmt.prepend_sibling(
                Nodecl::CxxDef::make(
                    /*context*/ Nodecl::NodeclBase::null(),
                    async));

    // Use set_value (initializer) to produce the expression statement;
    // this removes the need to call make() factories for a complex construct
    async.set_value(env_expr);

    return Nodecl::Utils::deep_copy(new_task_body, unpacked_inside_scope, symbol_map);
}

} }
