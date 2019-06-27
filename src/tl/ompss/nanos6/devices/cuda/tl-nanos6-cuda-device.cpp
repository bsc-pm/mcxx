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

#include "tl-nanos6-cuda-device.hpp"
#include "tl-nanos6-support.hpp"

#include "tl-compilerpipeline.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-scope.hpp"

#include "codegen-phase.hpp"
#include "codegen-cuda.hpp"

#include "cxx-profile.h"
#include "cxx-cexpr.h"
#include "cxx-driver-utils.h"

#include <errno.h>
namespace TL { namespace Nanos6 {

CUDADevice::CUDADevice()
{}

CUDADevice::~CUDADevice()
{
    compile_cuda_code();
}

TL::Symbol CUDADevice::get_device_type_id() const
{
    TL::Symbol device_type_id =
        TL::Scope::get_global_scope().get_symbol_from_name("nanos6_cuda_device");

    ERROR_CONDITION(!device_type_id.is_valid(), "Invalid device type id", 0);
    return device_type_id;
}

bool CUDADevice::requires_arguments_translation() const
{
    return true;
}

namespace {
Nodecl::NodeclBase get_block_member_initialization(
        TL::Symbol dim_block, TL::Symbol member, Nodecl::NodeclBase global, Nodecl::NodeclBase local)
{
    Nodecl::NodeclBase lhs_expr =
        Nodecl::ClassMemberAccess::make(
                dim_block.make_nodecl(/*ref_type*/ true),
                member.make_nodecl(/*ref_type*/ true),
                /* member_literal */ Nodecl::NodeclBase::null(),
                member.get_type().get_lvalue_reference_to());

    Nodecl::NodeclBase rhs_expr;
    if (global.is_constant() &&
            const_value_cast_to_signed_int(global.get_constant()) == 1)
    {
        rhs_expr = const_value_to_nodecl(const_value_get_one(4, 1));
    }
    else
    {
        rhs_expr = Nodecl::ConditionalExpression::make(
                Nodecl::LowerOrEqualThan::make(
                    global.shallow_copy(),
                    local.shallow_copy(),
                    TL::Type::get_bool_type()),
                global.shallow_copy(),
                local.shallow_copy(),
                global.get_type());
    }

    return Nodecl::ExpressionStatement::make(
                Nodecl::Assignment::make(lhs_expr, rhs_expr, lhs_expr.get_type()));
}

Nodecl::NodeclBase get_grid_member_initialization(
        TL::Symbol dim_grid, TL::Symbol member, Nodecl::NodeclBase global, Nodecl::NodeclBase local)
{
    Nodecl::NodeclBase lhs_expr =
        Nodecl::ClassMemberAccess::make(
                dim_grid.make_nodecl(/*ref_type*/ true),
                member.make_nodecl(/*ref_type*/ true),
                /* member_literal */ Nodecl::NodeclBase::null(),
                member.get_type().get_lvalue_reference_to());

    Nodecl::NodeclBase rhs_expr;
    if (global.is_constant() &&
            const_value_cast_to_signed_int(global.get_constant()) == 1)
    {
        rhs_expr = const_value_to_nodecl(const_value_get_one(4, 1));
    }
    else
    {
        rhs_expr = Nodecl::ConditionalExpression::make(
                Nodecl::LowerOrEqualThan::make(
                    global.shallow_copy(),
                    local.shallow_copy(),
                    TL::Type::get_bool_type()),
                const_value_to_nodecl(const_value_get_one(4, 1)),
                Nodecl::Add::make(
                    Nodecl::Div::make(
                        global.shallow_copy(),
                        local.shallow_copy(),
                        global.get_type()),
                    Nodecl::Different::make(
                        Nodecl::Mod::make(
                            global.shallow_copy(),
                            local.shallow_copy(),
                            global.get_type()),
                        const_value_to_nodecl(const_value_get_zero(4, 1)),
                        TL::Type::get_bool_type()),
                    global.get_type()),
                global.get_type());
    }

    return Nodecl::ExpressionStatement::make(
                Nodecl::Assignment::make(lhs_expr, rhs_expr, lhs_expr.get_type()));
}
}


Nodecl::NodeclBase CUDADevice::compute_specific_task_body(
        Nodecl::NodeclBase task_body,
        const DirectiveEnvironment &env,
        Nodecl::NodeclBase unpacked_function_code,
        const TL::Scope &unpacked_inside_scope,
        Nodecl::Utils::SimpleSymbolMap &symbol_map)
{
    if (!env.task_is_taskcall // If it's an inline task
            || env.ndrange.empty()) // or it's an outline task without the 'ndrange' clause
    {
        return Device::compute_specific_task_body(task_body, env, unpacked_function_code, unpacked_inside_scope, symbol_map);
    }
    else
    {
        Nodecl::NodeclBase new_task_body = task_body.shallow_copy();
        Nodecl::NodeclBase context = new_task_body.as<Nodecl::List>().front();
        ERROR_CONDITION(!context.is<Nodecl::Context>(), "Unexpected node\n", 0);
        Nodecl::NodeclBase compound_statement = context.as<Nodecl::Context>().get_in_context().as<Nodecl::List>().front();
        ERROR_CONDITION(!compound_statement.is<Nodecl::CompoundStatement>(), "Unexpected node\n", 0);
        Nodecl::NodeclBase expr_stmt = compound_statement.as<Nodecl::CompoundStatement>().get_statements().as<Nodecl::List>().front();
        ERROR_CONDITION(!expr_stmt.is<Nodecl::ExpressionStatement>(), "Unexpected node\n", 0);
        Nodecl::NodeclBase function_call = expr_stmt.as<Nodecl::ExpressionStatement>().get_nest();
        ERROR_CONDITION(!function_call.is<Nodecl::FunctionCall>(), "Unexpected node\n", 0);

        if (IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase called = function_call.as<Nodecl::FunctionCall>().get_called();
            ERROR_CONDITION(function_call.is<Nodecl::Symbol>(), "Unexpected node\n", 0);
            unpacked_function_code.prepend_sibling(
                    Nodecl::CxxDecl::make(/* context */ Nodecl::NodeclBase::null(), called.as<Nodecl::Symbol>().get_symbol()));
        }

        TL::Symbol dim3_struct = TL::Scope::get_global_scope().get_symbol_from_name("dim3");
        TL::Type dim3_type = dim3_struct.get_user_defined_type();

        TL::Scope inner_scope = context.retrieve_context();

        TL::Symbol dim_block = inner_scope.new_symbol("dim_block");
        dim_block.get_internal_symbol()->kind = SK_VARIABLE;
        dim_block.get_internal_symbol()->type_information = dim3_type.get_internal_type();
        symbol_entity_specs_set_is_user_declared(dim_block.get_internal_symbol(), 1);

        if (IS_CXX_LANGUAGE)
            expr_stmt.prepend_sibling(Nodecl::CxxDef::make(/*context*/ Nodecl::NodeclBase::null(), dim_block));

        TL::Symbol dim_grid = inner_scope.new_symbol("dim_grid");
        dim_grid.get_internal_symbol()->kind = SK_VARIABLE;
        dim_grid.get_internal_symbol()->type_information = dim3_type.get_internal_type();
        symbol_entity_specs_set_is_user_declared(dim_grid.get_internal_symbol(), 1);
        if (IS_CXX_LANGUAGE)
            expr_stmt.prepend_sibling(Nodecl::CxxDef::make(/*context*/ Nodecl::NodeclBase::null(), dim_grid));


        // Prepare the kernel execution environment
        TL::Symbol dim3_x, dim3_y, dim3_z;
        {
            TL::ObjectList<TL::Symbol> non_static_data_members = dim3_struct.get_type().get_nonstatic_data_members();

            struct MemberInfo {
                const char *member_name;
                TL::Symbol &member;
            } dim3_members[] = {
                { "x", dim3_x },
                { "y", dim3_y },
                { "z", dim3_z },
            };

            for (MemberInfo *it = dim3_members;
                    it != ((MemberInfo*)((&dim3_members)+1));
                    it++)
            {
                TL::ObjectList<TL::Symbol> aux = non_static_data_members.find<std::string>(&TL::Symbol::get_name, it->member_name);
                ERROR_CONDITION(aux.empty(), "Field '%s' not found in 'dim3' struct type", it->member_name);
                it->member = aux[0];
            }
        }

        Nodecl::NodeclBase globals[3], locals[3];
        {
            // ndrange(N, global1, ..., gobalN, local1, ..., localN)
            int num_dims = const_value_cast_to_signed_int(env.ndrange[0].get_constant());
            int i;
            for (i = 0; i < num_dims; ++i)
            {
                globals[i] = env.ndrange[1 + i];
                locals[i]  = env.ndrange[1 + num_dims + i];
            }
            for (; i < 3; ++i)
            {
                globals[i] = const_value_to_nodecl(const_value_get_one(4, 1));
                locals[i]  = const_value_to_nodecl(const_value_get_one(4, 1));
            }
        }

        expr_stmt.prepend_sibling(get_block_member_initialization(dim_block, dim3_x, globals[0], locals[0]));
        expr_stmt.prepend_sibling(get_grid_member_initialization(dim_grid, dim3_x, globals[0], locals[0]));

        expr_stmt.prepend_sibling(get_block_member_initialization(dim_block, dim3_y, globals[1], locals[1]));
        expr_stmt.prepend_sibling(get_grid_member_initialization(dim_grid, dim3_y, globals[1], locals[1]));

        expr_stmt.prepend_sibling(get_block_member_initialization(dim_block, dim3_z, globals[2], locals[2]));
        expr_stmt.prepend_sibling(get_grid_member_initialization(dim_grid, dim3_z, globals[2], locals[2]));


        Nodecl::NodeclBase stream_expr;
        {
            TL::Symbol device_env = unpacked_inside_scope.get_symbol_from_name("device_env");

            TL::Symbol dev_env_type_symbol = get_nanos6_class_symbol("nanos6_cuda_device_environment_t");

            // Adding a definition of the nanos6_cuda_device_environment_t type
            _cuda_code.prepend(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), dev_env_type_symbol));

            stream_expr = Nodecl::Conversion::make(
                    device_env.make_nodecl(/* ref_type */true),
                    dev_env_type_symbol.get_user_defined_type().get_pointer_to());
            stream_expr.set_text("C");

            TL::ObjectList<TL::Symbol> fields = dev_env_type_symbol.get_type().get_nonstatic_data_members();

            TL::ObjectList<TL::Symbol> aux = fields.find<std::string>(&TL::Symbol::get_name, "stream");
            ERROR_CONDITION(aux.size() == 0, "'stream' member was not found", 0);

            stream_expr = Nodecl::ClassMemberAccess::make(
                    Nodecl::Dereference::make(
                        stream_expr,
                        stream_expr.get_type().points_to().get_lvalue_reference_to()),
                    aux[0].make_nodecl(/* ref_type */true),
                    /* member_literal */ Nodecl::NodeclBase::null(),
                    aux[0].get_type().get_lvalue_reference_to());
        }

        Nodecl::NodeclBase empty_stmt = Nodecl::EmptyStatement::make();
        expr_stmt.prepend_sibling(empty_stmt);
        Nodecl::Utils::remove_from_enclosing_list(expr_stmt);

        Nodecl::NodeclBase kernel_call = Nodecl::CudaKernelCall::make(
                Nodecl::List::make(
                    dim_grid.make_nodecl(/*ref_type */ true),
                    dim_block.make_nodecl( /*ref_type*/ true),
                    const_value_to_nodecl(const_value_get_zero(4, 1)),
                    stream_expr),
                function_call,
                function_call.get_type());

        expr_stmt.as<Nodecl::ExpressionStatement>().set_nest(kernel_call);

        empty_stmt.replace(expr_stmt);

        return Nodecl::Utils::deep_copy(new_task_body, unpacked_inside_scope, symbol_map);
    }
}

void CUDADevice::root_unpacked_function(
        TL::Symbol unpacked_function, Nodecl::NodeclBase unpacked_function_code)
{
    _cuda_code.append(unpacked_function_code);

    // The unpacked function should not be static neither inline
    symbol_entity_specs_set_is_static(unpacked_function.get_internal_symbol(), 0);
    symbol_entity_specs_set_is_inline(unpacked_function.get_internal_symbol(), 0);

    if (IS_C_LANGUAGE || IS_FORTRAN_LANGUAGE)
    {
        // Force the unpacked function to have C external linkage (note that it may be called
        // from C/Fortran but it will be always defined in CUDA).
        symbol_entity_specs_set_linkage_spec(unpacked_function.get_internal_symbol(), "\"C\"");
    }
}

void CUDADevice::compile_cuda_code() const
{
    if (_cuda_code.is_null())
        return;

    std::string original_filename = TL::CompilationProcess::get_current_file().get_filename();
    std::string new_filename = "cuda_" + original_filename.substr(0, original_filename.find("."))  + ".cu";

    FILE* ancillary_file = fopen(new_filename.c_str(), "w");
    if (ancillary_file == NULL)
    {
        fatal_error("%s: error: cannot open file '%s'. %s\n",
                original_filename.c_str(), new_filename.c_str(), strerror(errno));
    }

    compilation_configuration_t* configuration = ::get_compilation_configuration("cuda");
    ERROR_CONDITION (configuration == NULL, "'cuda' auxiliary profile should be available", 0);

    // Make sure phases are loaded (this is needed for codegen)
    load_compiler_phases(configuration);

    TL::CompilationProcess::add_file(new_filename, "cuda");

    //Remove the intermediate source file
    ::mark_file_for_cleanup(new_filename.c_str());

    Codegen::CudaGPU* phase = reinterpret_cast<Codegen::CudaGPU*>(configuration->codegen_phase);

    if (IS_FORTRAN_LANGUAGE)
        CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_C;

    phase->codegen_top_level(_cuda_code, ancillary_file, new_filename);

    if (IS_FORTRAN_LANGUAGE)
        CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_FORTRAN;

    fclose(ancillary_file);
}

} }
