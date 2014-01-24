/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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


#include "tl-devices.hpp"
#include "nanox-cuda.hpp"
#include "tl-nanos.hpp"
#include "tl-multifile.hpp"
#include "tl-compilerpipeline.hpp"
#include "cxx-profile.h"

#include "codegen-phase.hpp"
#include "codegen-cuda.hpp"
#include "cxx-cexpr.h"

#include <errno.h>
#include "cxx-driver-utils.h"

#include "tl-symbol-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"

using namespace TL;
using namespace TL::Nanox;

static std::string cuda_outline_name(const std::string & name)
{
    return "gpu_" + name;
}

bool DeviceCUDA::is_gpu_device() const
{
    return true;
}

void DeviceCUDA::update_ndrange_and_shmem_arguments(
        const TL::Symbol& called_task,
        const TL::Symbol& unpacked_function,
        const TargetInformation& target_info,
        Nodecl::Utils::SimpleSymbolMap* called_fun_to_outline_data_map,
        Nodecl::Utils::SimpleSymbolMap* outline_data_to_unpacked_fun_map,
        // out
        TL::ObjectList<Nodecl::NodeclBase>& new_ndrange_args,
        TL::ObjectList<Nodecl::NodeclBase>& new_shmem_args)
{
    TL::ObjectList<TL::Symbol> parameters_called = called_task.get_function_parameters();
    TL::ObjectList<TL::Symbol> parameters_unpacked = unpacked_function.get_function_parameters();

    TL::ObjectList<Nodecl::NodeclBase> ndrange_args = target_info.get_ndrange();
    TL::ObjectList<Nodecl::NodeclBase> shmem_args = target_info.get_shmem();
    if (IS_FORTRAN_LANGUAGE)
    {
        for (unsigned int i = 0; i < ndrange_args.size(); ++i)
        {
            Nodecl::NodeclBase argument = Nodecl::Utils::deep_copy(
                    ndrange_args[i],
                    unpacked_function.get_related_scope(),
                    *called_fun_to_outline_data_map);

            new_ndrange_args.append(Nodecl::Utils::deep_copy(
                        argument,
                        unpacked_function.get_related_scope(),
                        *outline_data_to_unpacked_fun_map));
        }

        for (unsigned int i = 0; i < shmem_args.size(); ++i)
        {
            Nodecl::NodeclBase argument = Nodecl::Utils::deep_copy(
                    shmem_args[i],
                    unpacked_function.get_related_scope(),
                    *called_fun_to_outline_data_map);

            new_shmem_args.append(Nodecl::Utils::deep_copy(
                        argument,
                        unpacked_function.get_related_scope(),
                        *outline_data_to_unpacked_fun_map));
        }
    }
    else
    {
        for (unsigned int i = 0; i < ndrange_args.size(); ++i)
        {

            new_ndrange_args.append(Nodecl::Utils::deep_copy(
                        ndrange_args[i],
                        unpacked_function.get_related_scope(),
                        *outline_data_to_unpacked_fun_map));
        }

        for (unsigned int i = 0; i < shmem_args.size(); ++i)
        {

            new_shmem_args.append(Nodecl::Utils::deep_copy(
                        shmem_args[i],
                        unpacked_function.get_related_scope(),
                        *outline_data_to_unpacked_fun_map));
        }
    }
}

void DeviceCUDA::generate_ndrange_additional_code(
        const TL::ObjectList<Nodecl::NodeclBase>& new_ndrange_args,
        TL::Source& code_ndrange)
{
    ERROR_CONDITION(!new_ndrange_args[0].is_constant(), "The first argument of the 'ndrange' clause must be a literal", 0);

    int num_args_ndrange = new_ndrange_args.size();
    int num_dim = const_value_cast_to_4(new_ndrange_args[0].get_constant());

    ERROR_CONDITION(num_dim < 1 || num_dim > 3, "invalid number of dimensions for 'ndrange' clause. Valid values: 1, 2 and 3." , 0);

    bool check_dim = !(new_ndrange_args[num_args_ndrange - 1].is_constant()
            && const_value_is_string(new_ndrange_args[num_args_ndrange - 1].get_constant())
            && (strcmp(const_value_string_unpack_to_string(new_ndrange_args[num_args_ndrange-1].get_constant()),"noCheckDim") == 0));

    ERROR_CONDITION(((num_dim * 2) + 1 + !check_dim) != num_args_ndrange, "invalid number of arguments for 'ndrange' clause", 0);

    code_ndrange << "dim3 dimGrid;";
    code_ndrange << "dim3 dimBlock;";
    const char* field[3] = { "x", "y", "z"};
    for (int i = 1; i <= 3; ++i)
    {
        if (check_dim)
        {
            if (i <= num_dim)
            {
                code_ndrange << "dimBlock." << field[i-1] << " = "
                    << "(("
                    << as_expression(new_ndrange_args[i])
                    << " < " << as_expression(new_ndrange_args[num_dim + i])
                    << ") ? (" << as_expression(new_ndrange_args[i])
                    << ") : (" << as_expression(new_ndrange_args[num_dim + i])
                    << "));";

                code_ndrange << "dimGrid."  << field[i-1] << " = "
                    << "(("
                    << as_expression(new_ndrange_args[i])
                    << " < " << as_expression(new_ndrange_args[num_dim + i])
                    << ") ? 1 : (("
                    << as_expression(new_ndrange_args[i]) << "/" << as_expression(new_ndrange_args[num_dim + i])
                    << ") + ((" << as_expression(new_ndrange_args[i]) << " %  " << as_expression(new_ndrange_args[num_dim + i])
                    << " == 0) ? 0 : 1)));";
            }
            else
            {
                code_ndrange << "dimBlock." << field[i-1] << " = 1;";
                code_ndrange << "dimGrid."  << field[i-1] << " = 1;";
            }
        }
        else
        {
            if (i <= num_dim)
            {
                code_ndrange << "dimBlock." << field[i-1] << " = " << as_expression(new_ndrange_args[num_dim + i]) << ";";
                code_ndrange << "dimGrid."  << field[i-1] << " = " << as_expression(new_ndrange_args[i]) << "/" << as_expression(new_ndrange_args[num_dim + i]) << ";";
            }
            else
            {
                code_ndrange << "dimBlock." << field[i-1] << " = 1;";
                code_ndrange << "dimGrid."  << field[i-1] << " = 1;";
            }
        }
    }
}

void DeviceCUDA::generate_ndrange_kernel_call(
        const Scope& scope,
        const TL::ObjectList<Nodecl::NodeclBase>& shmem_args,
        const Nodecl::NodeclBase& task_statements,
        Nodecl::NodeclBase& output_statements)
{
    ERROR_CONDITION(shmem_args.size() > 0 && shmem_args.size() != 1, "Unexpected number of arguments in the 'shmem' clause", 0);

    Nodecl::NodeclBase function_call_nodecl =
        task_statements.as<Nodecl::List>().begin()->as<Nodecl::ExpressionStatement>().get_nest();

    ObjectList<Nodecl::NodeclBase> cuda_kernel_config;
    Symbol dim_grid  = scope.get_symbol_from_name("dimGrid");
    Symbol dim_block = scope.get_symbol_from_name("dimBlock");
    Symbol exec_stream = scope.get_symbol_from_name("nanos_get_kernel_execution_stream");
    ERROR_CONDITION(!dim_grid.is_valid() || !dim_block.is_valid() || !exec_stream.is_valid(), "Unreachable code", 0);

    cuda_kernel_config.append(
            Nodecl::Symbol::make(dim_grid,
                task_statements.get_locus()));

    cuda_kernel_config.append(
            Nodecl::Symbol::make(dim_block,
                task_statements.get_locus()));

    if (shmem_args.size() == 1)
    {
        cuda_kernel_config.append(shmem_args[0]);
    }
    else
    {
        cuda_kernel_config.append(
                Nodecl::IntegerLiteral::make(
                    TL::Type::get_int_type(),
                    const_value_get_zero(TL::Type::get_int_type().get_size(), /* sign */ 1),
                    task_statements.get_locus()));
    }

    cuda_kernel_config.append(
            Nodecl::FunctionCall::make(
                Nodecl::Symbol::make(
                    exec_stream,
                    task_statements.get_locus()),
                /* arguments */ nodecl_null(),
                /* alternate_name */ nodecl_null(),
                /* function_form */ nodecl_null(),
                TL::Type::get_void_type(),
                task_statements.get_locus()));

    Nodecl::NodeclBase kernell_call =
        Nodecl::CudaKernelCall::make(
                Nodecl::List::make(cuda_kernel_config),
                function_call_nodecl,
                TL::Type::get_void_type(),
                task_statements.get_locus());

    Nodecl::NodeclBase expression_stmt =
        Nodecl::ExpressionStatement::make(
                kernell_call,
                task_statements.get_locus());

    // In this case, we should change the output statements!
    output_statements = expression_stmt;
}

// This visitor completes the configuration of every cuda function task
class UpdateKernelConfigsVisitor : public Nodecl::ExhaustiveVisitor<void>
{
    public:
        void visit(const Nodecl::CudaKernelCall& node)
        {
            Nodecl::List kernel_config = node.get_kernel_config().as<Nodecl::List>();

            ERROR_CONDITION(kernel_config.size() < 2
                    || kernel_config.size() > 4,
                    "A kernel call configuration must have between 2 and 4 parameters", 0);

            if (kernel_config.size() == 2
                    || kernel_config.size() == 3)
            {
                // We should complete the kernel configuration
                if (kernel_config.size() == 2)
                {
                    // Append to the kernel configuration the size of shared memory (0, in this case)
                    kernel_config.append(
                            Nodecl::IntegerLiteral::make(
                                TL::Type::get_int_type(),
                                const_value_get_zero(TL::Type::get_int_type().get_size(), /* sign */ 1),
                                node.get_locus()));
                }

                Symbol exec_stream =
                    node.retrieve_context().get_symbol_from_name("nanos_get_kernel_execution_stream");

                ERROR_CONDITION(!exec_stream.is_valid(), "Unreachable code", 0);

                // Append to the kernel configuration the stream
                kernel_config.append(
                        Nodecl::FunctionCall::make(
                            Nodecl::Symbol::make(
                                exec_stream,
                                node.get_locus()),
                            /* arguments */ nodecl_null(),
                            /* alternate_name */ nodecl_null(),
                            /* function_form */ nodecl_null(),
                            TL::Type::get_void_type(),
                            node.get_locus()));
            }
        }
};

class NanosGetCublasHandleVisitor : public Nodecl::ExhaustiveVisitor<void>
{
    private:
        bool _found;
    public:
        NanosGetCublasHandleVisitor() : _found(false) { }

        void visit(const Nodecl::ObjectInit& node)
        {
            if (_found)
                return;

            TL::Symbol sym = node.get_symbol();
            if (sym.get_value().is_null())
                return;

            walk(sym.get_value());
        }

        void visit(const Nodecl::FunctionCall& node)
        {
            if (_found)
                return;

            TL::Symbol called_symbol = node.get_called().get_symbol();
            if (called_symbol.is_valid()
                    && called_symbol.get_name() == "nanos_get_cublas_handle")
            {
                _found = true;
            }
        }

        bool get_is_nanos_get_cublas_handle()
        {
            return _found;
        }
};

void DeviceCUDA::is_nanos_get_cublas_handle_present(Nodecl::NodeclBase task_code)
{
    if (_is_nanos_get_cublas_handle)
        return;

    NanosGetCublasHandleVisitor visitor;
    visitor.walk(task_code);
    _is_nanos_get_cublas_handle = visitor.get_is_nanos_get_cublas_handle();
}

void DeviceCUDA::update_all_kernel_configurations(Nodecl::NodeclBase task_code)
{
    UpdateKernelConfigsVisitor update_kernel_visitor;
    update_kernel_visitor.walk(task_code);
}

void DeviceCUDA::create_outline(CreateOutlineInfo &info,
        Nodecl::NodeclBase &outline_placeholder,
        Nodecl::NodeclBase &output_statements,
        Nodecl::Utils::SimpleSymbolMap* &symbol_map)
{
    _cuda_tasks_processed = true;

    // Unpack DTO
    const std::string& device_outline_name = cuda_outline_name(info._outline_name);
    const TargetInformation& target_info = info._target_info;
    const Nodecl::NodeclBase& original_statements = info._original_statements;
    const Nodecl::NodeclBase& task_statements = info._task_statements;
    const TL::Symbol& called_task = info._called_task; // This symbol is only valid for function tasks
    bool is_function_task = called_task.is_valid();
    output_statements = task_statements;

    symbol_map = new Nodecl::Utils::SimpleSymbolMap(&_copied_cuda_functions);

    ERROR_CONDITION(is_function_task && !called_task.is_function(),
            "The '%s' symbol is not a function", called_task.get_name().c_str());

    TL::Symbol current_function =
        original_statements.retrieve_context().get_decl_context().current_scope->related_entry;

    if (current_function.is_nested_function())
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            running_error("%s: error: nested functions are not supported\n",
                    original_statements.get_locus_str().c_str());
    }

    // Update the kernel configurations of every cuda function call of the current task
    Nodecl::NodeclBase task_code =
        (is_function_task) ? called_task.get_function_code() : output_statements;

    if (!task_code.is_null())
    {
        is_nanos_get_cublas_handle_present(task_code);
        update_all_kernel_configurations(task_code);
    }

    // Add the user function to the intermediate file if It is a function task and It has not been added
    // to the file previously (This action must be done always after the update of the kernel configurations
    // because the code of the user function may be changed if It contains one or more cuda function calls)
    if (is_function_task)
    {
        if (_copied_cuda_functions.map(called_task) == called_task)
        {
            if (IS_CXX_LANGUAGE || IS_C_LANGUAGE)
            {
                if (!called_task.get_function_code().is_null())
                {
                    TL::Symbol new_function = SymbolUtils::new_function_symbol(called_task, called_task.get_name() + "_moved");

                    _copied_cuda_functions.add_map(called_task, new_function);

                    _cuda_file_code.append(Nodecl::Utils::deep_copy(
                                called_task.get_function_code(),
                                called_task.get_scope(),
                                *symbol_map));
                }
                else
                {
                    if (IS_CXX_LANGUAGE)
                    {
                        // Best effort: add a declaration for this function task to the intermediate file
                        _cuda_file_code.append(Nodecl::CxxDecl::make(
                                    /* optative context */ nodecl_null(),
                                    called_task,
                                    original_statements.get_locus()));
                    }

                }
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                std::string name_clause_arg = target_info.get_name();
                std::string called_task_name = !name_clause_arg.empty() ? name_clause_arg : called_task.get_name();
                TL::Symbol new_function = current_function.get_scope().new_symbol(called_task_name);
                scope_entry_t* new_function_internal = new_function.get_internal_symbol();

                new_function_internal->kind = called_task.get_internal_symbol()->kind;
                new_function_internal->type_information = called_task.get_type().get_internal_type();
                new_function_internal->entity_specs.is_user_declared = 1;
                new_function_internal->entity_specs.is_extern = 1;

                // if the 'ndrange' clause is defined, the called task is __global__
                if (target_info.get_ndrange().size() != 0)
                {
                    gcc_attribute_t intern_global_attr;
                    intern_global_attr.attribute_name = uniquestr("global");
                    intern_global_attr.expression_list = nodecl_null();

                    new_function_internal->entity_specs.num_gcc_attributes = 1;
                    new_function_internal->entity_specs.gcc_attributes =
                        (gcc_attribute_t*) xcalloc(1, sizeof(gcc_attribute_t));

                    memcpy(new_function_internal->entity_specs.gcc_attributes, &intern_global_attr, 1 * sizeof(gcc_attribute_t));
                }

                _copied_cuda_functions.add_map(called_task, new_function);
            }
        }
    }

    // Create the new unpacked function
    Source initial_statements, final_statements;
    TL::Symbol unpacked_function, forward_function;
    if (IS_FORTRAN_LANGUAGE)
    {
        forward_function = new_function_symbol_forward(
                current_function,
                device_outline_name + "_forward",
                info);

        // The unpacked function is defined in the cuda intermediate file, but It's declared and used
        // in the Fortran source. For some linkage reasons, the name of this function must end with an "_"
        unpacked_function = new_function_symbol_unpacked(
                current_function,
                device_outline_name + "_unpack_",
                info,
                // out
                symbol_map,
                initial_statements,
                final_statements);
    }
    else
    {
        unpacked_function = new_function_symbol_unpacked(
                current_function,
                device_outline_name + "_unpacked",
                info,
                // out
                symbol_map,
                initial_statements,
                final_statements);
    }

    Source ndrange_code;
    TL::ObjectList<Nodecl::NodeclBase> new_ndrange_args, new_shmem_args;
    if (is_function_task
            && target_info.get_ndrange().size() > 0)
    {

        Nodecl::Utils::SimpleSymbolMap param_to_args_map =
            info._target_info.get_param_arg_map();

        update_ndrange_and_shmem_arguments(called_task, unpacked_function, target_info,
                &param_to_args_map, symbol_map, new_ndrange_args, new_shmem_args);

        generate_ndrange_additional_code(new_ndrange_args, ndrange_code);
    }

    // The unpacked function must not be static and must have external linkage because
    // It's called from the original source but It's defined in cudacc_filename.cu
    unpacked_function.get_internal_symbol()->entity_specs.is_static = 0;
    unpacked_function.get_internal_symbol()->entity_specs.is_inline = 0;
    if (IS_C_LANGUAGE || IS_FORTRAN_LANGUAGE)
    {
        // The unpacked function is declared in the C/Fortran source but
        // defined in the Cuda file. For this reason, It has C linkage
        unpacked_function.get_internal_symbol()->entity_specs.linkage_spec = "\"C\"";
    }

    Nodecl::NodeclBase unpacked_function_code, unpacked_function_body;
    SymbolUtils::build_empty_body_for_function(unpacked_function,
            unpacked_function_code,
            unpacked_function_body);

    Source unpacked_source;
    unpacked_source
        << "{"
        <<      initial_statements
        <<      ndrange_code
        <<      statement_placeholder(outline_placeholder)
        <<      final_statements
        << "}"
        ;

    if (IS_FORTRAN_LANGUAGE)
        Source::source_language = SourceLanguage::C;

    Nodecl::NodeclBase new_unpacked_body =
        unpacked_source.parse_statement(unpacked_function_body);

    if (IS_FORTRAN_LANGUAGE)
        Source::source_language = SourceLanguage::Current;

    unpacked_function_body.replace(new_unpacked_body);

    if (is_function_task
            && target_info.get_ndrange().size() > 0)
    {
        generate_ndrange_kernel_call(
                outline_placeholder.retrieve_context(),
                new_shmem_args,
                task_statements,
                output_statements);
    }

    // Add the unpacked function to the intermediate cuda file
    _cuda_file_code.append(unpacked_function_code);

    // Add a declaration of the unpacked function symbol in the original source
    if (IS_CXX_LANGUAGE)
    {
        Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                /* optative context */ nodecl_null(),
                unpacked_function,
                original_statements.get_locus());
        Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
    }

    // Create the outline function
    //The outline function has always only one parameter which name is 'args'
    ObjectList<std::string> structure_name;
    structure_name.append("args");

    //The type of this parameter is an struct (i. e. user defined type)
    ObjectList<TL::Type> structure_type;
    structure_type.append(TL::Type(
                get_user_defined_type(
                    info._arguments_struct.get_internal_symbol())).get_lvalue_reference_to());

    TL::Symbol outline_function = SymbolUtils::new_function_symbol(
            current_function,
            device_outline_name,
            TL::Type::get_void_type(),
            structure_name,
            structure_type);

    Nodecl::NodeclBase outline_function_code, outline_function_body;
    SymbolUtils::build_empty_body_for_function(outline_function,
            outline_function_code,
            outline_function_body);

    // Prepare arguments for the call to the unpack (or forward in Fortran)
    TL::Scope outline_function_scope(outline_function_body.retrieve_context());
    TL::Symbol structure_symbol = outline_function_scope.get_symbol_from_name("args");
    ERROR_CONDITION(!structure_symbol.is_valid(), "Argument of outline function not found", 0);

    Source unpacked_arguments, cleanup_code;
    TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        switch ((*it)->get_sharing())
        {
            case OutlineDataItem::SHARING_PRIVATE:
                {
                    break;
                }
            case OutlineDataItem::SHARING_SHARED:
            case OutlineDataItem::SHARING_CAPTURE:
            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                {
                    TL::Type param_type = (*it)->get_in_outline_type();

                    Source argument;
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        // Normal shared items are passed by reference from a pointer,
                        // derreference here
                        if (
                                ((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED)
                                && !(IS_CXX_LANGUAGE && (*it)->get_symbol().get_name() == "this"))
                        {
                            if (!param_type.no_ref().depends_on_nonconstant_values())
                            {
                                argument << "*(args." << (*it)->get_field_name() << ")";
                            }
                            else
                            {
                                TL::Type ptr_type = (*it)->get_in_outline_type().references_to().get_pointer_to();
                                TL::Type cast_type = rewrite_type_of_vla_in_outline(ptr_type, data_items, structure_symbol);

                                argument << "*((" << as_type(cast_type) << ")args." << (*it)->get_field_name() << ")";
                            }
                        }
                        // Any other parameter is bound to the storage of the struct
                        else
                        {
                            if (!param_type.no_ref().depends_on_nonconstant_values())
                            {
                                argument << "args." << (*it)->get_field_name();
                            }
                            else
                            {
                                TL::Type cast_type = rewrite_type_of_vla_in_outline(param_type, data_items, structure_symbol);
                                argument << "(" << as_type(cast_type) << ")args." << (*it)->get_field_name();
                            }
                        }
                    }
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        argument << "args % " << (*it)->get_field_name();

                        bool is_allocatable = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE;
                        bool is_pointer = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_POINTER;

                        if (is_allocatable
                                || is_pointer)
                        {
                            cleanup_code
                                << "DEALLOCATE(args % " << (*it)->get_field_name() << ")\n"
                                ;
                        }
                    }
                    else
                    {
                        internal_error("running error", 0);
                    }

                    unpacked_arguments.append_with_separator(argument, ", ");
                    break;
                }
            case OutlineDataItem::SHARING_REDUCTION:
                {
                    // Pass the original reduced variable as if it were a shared
                    Source argument;
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        argument << "*(args." << (*it)->get_field_name() << ")";
                    }
                    else
                    {
                        internal_error("running error", 0);
                    }
                    unpacked_arguments.append_with_separator(argument, ", ");
                    break;
                }
            default:
                {
                    internal_error("Unexpected data sharing kind", 0);
                }
        }
    }

    Source outline_src,
           instrument_before,
           instrument_after;

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            outline_src
                << "{"
                <<      instrument_before
                <<      device_outline_name << "_unpacked(" << unpacked_arguments << ");"
                <<      instrument_after
                << "}"
                ;
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            Source outline_function_addr;

            // Remove the last character of the unpacked function name (It's always an "_")
            // Note: the unpacked function of the Fortran source and the unpacked function
            // of the CUDA source are different symbols
            std::string unpack_name = unpacked_function.get_name();
            std::string unpack_name_fortran = unpack_name.substr(0, unpack_name.size()-1);

            outline_src
                << "IMPLICIT NONE\n"
                << "EXTERNAL " << unpack_name_fortran << "\n"
                << instrument_before << "\n"
                << "CALL " << device_outline_name << "_forward(" << outline_function_addr << unpacked_arguments << ")\n"
                << instrument_after << "\n"
                << cleanup_code
                ;


            outline_function_addr << "LOC(" << unpack_name_fortran << ")";
            if (!unpacked_arguments.empty())
            {
                outline_function_addr << ", ";
            }

            // Copy USEd information to the outline and forward functions
            TL::Symbol *functions[] = { &outline_function, &forward_function, NULL };

            for (int i = 0; functions[i] != NULL; i++)
            {
                TL::Symbol &function(*functions[i]);

                Nodecl::Utils::Fortran::append_used_modules(original_statements.retrieve_context(),
                        function.get_related_scope());

                add_used_types(data_items, function.get_related_scope());
            }

            // Generate ancillary code in C
            add_forward_function_code_to_extra_c_code(device_outline_name, data_items, outline_placeholder);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

    if (instrumentation_enabled())
    {
        get_instrumentation_code(
                info._called_task,
                outline_function,
                outline_function_body,
                info._task_label,
                original_statements.get_locus(),
                instrument_before,
                instrument_after);
    }

    Nodecl::NodeclBase new_outline_body = outline_src.parse_statement(outline_function_body);
    outline_function_body.replace(new_outline_body);
    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, outline_function_code);
}

DeviceCUDA::DeviceCUDA()
    : DeviceProvider(/* device_name */ std::string("cuda")), _copied_cuda_functions(), _is_nanos_get_cublas_handle(false)
{
    set_phase_name("Nanox CUDA support");
    set_phase_description("This phase is used by Nanox phases to implement CUDA device support");
}

void DeviceCUDA::get_device_descriptor(DeviceDescriptorInfo& info,
        Source &ancillary_device_description,
        Source &device_descriptor,
        Source &fortran_dynamic_init UNUSED_PARAMETER)
{
    const std::string& device_outline_name = cuda_outline_name(info._outline_name);
    if (!Nanos::Version::interface_is_at_least("master", 5012))
        internal_error("Unsupported Nanos version.", 0);

    if (!IS_FORTRAN_LANGUAGE)
    {
        ancillary_device_description
            << comment("CUDA device descriptor")
            << "static nanos_smp_args_t "
            << device_outline_name << "_args = { (void(*)(void*))" << device_outline_name << "};"
            ;
        device_descriptor << "{ &nanos_gpu_factory, &" << device_outline_name << "_args }";
    }
    else
    {
        ancillary_device_description
            << "static nanos_smp_args_t " << device_outline_name << "_args;"
            ;

        device_descriptor
            << "{"
            // factory, arg
            << "0, 0"
            << "}"
            ;

        fortran_dynamic_init
            << device_outline_name << "_args.outline = (void(*)(void*))&" << device_outline_name << ";"
            << "nanos_wd_const_data.devices[" << info._fortran_device_index << "].factory = &nanos_gpu_factory;"
            << "nanos_wd_const_data.devices[" << info._fortran_device_index << "].arg = &" << device_outline_name << "_args;"
            ;


    }
}

bool DeviceCUDA::remove_function_task_from_original_source() const
{
    return true;
}

void DeviceCUDA::add_included_cuda_files(FILE* file)
{
    ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();
    std::string cuda_file_ext(".cu\"");
    std::string cuda_header_ext(".cuh\"");

    for (ObjectList<IncludeLine>::iterator it = lines.begin(); it != lines.end(); it++)
    {
        std::string line = (*it).get_preprocessor_line();
        size_t found = line.find_last_of(".");
        if (found != std::string::npos)
        {
            std::string extension = line.substr(found);

            if (extension == cuda_file_ext || extension == cuda_header_ext)
            {
                int output = fprintf(file, "%s\n", line.c_str());
                if (output < 0)
                    internal_error("Error trying to write the intermediate cuda file\n", 0);
            }
        }
    }
}

bool DeviceCUDA::allow_mandatory_creation()
{
    return true;
}

void DeviceCUDA::copy_stuff_to_device_file(const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied)
{
    for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = stuff_to_be_copied.begin();
            it != stuff_to_be_copied.end();
            ++it)
    {
        if (it->is<Nodecl::FunctionCode>()
                || it->is<Nodecl::TemplateFunctionCode>())
        {
            TL::Symbol function = it->get_symbol();
            TL::Symbol new_function = SymbolUtils::new_function_symbol(function, function.get_name() + "_moved");

            _copied_cuda_functions.add_map(function, new_function);
            _cuda_file_code.append(Nodecl::Utils::deep_copy(*it, *it, _copied_cuda_functions));
        }
        else
        {
            _cuda_file_code.append(Nodecl::Utils::deep_copy(*it, *it));
        }
    }
}

void DeviceCUDA::generate_outline_events_before(
        Source& function_name_instr,
        Source& extra_cast,
        Source& instrumentation_before)
{
    if (Nanos::Version::interface_is_at_least("instrumentation_api", 1001))
    {
        instrumentation_before << "err = nanos_instrument_raise_gpu_kernel_launch_event();";
    }
    else
    {
        DeviceProvider::generate_outline_events_before(function_name_instr, extra_cast, instrumentation_before);
    }
}

void DeviceCUDA::generate_outline_events_after(
        Source& function_name_instr,
        Source& extra_cast,
        Source& instrumentation_after)
{
    if (Nanos::Version::interface_is_at_least("instrumentation_api", 1001))
    {
        instrumentation_after << "err = nanos_instrument_close_gpu_kernel_launch_event();";
    }
    else
    {
        instrumentation_after << "err = nanos_instrument_close_user_fun_event();";
    }
}

void DeviceCUDA::phase_cleanup(DTO& data_flow)
{
    if (_cuda_tasks_processed)
    {
        Nodecl::NodeclBase root = data_flow["nodecl"];
        Source nanox_device_enable_section;
        nanox_device_enable_section << "__attribute__((weak)) char ompss_uses_cuda = 1;";

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::C;

        Nodecl::NodeclBase functions_section_tree = nanox_device_enable_section.parse_global(root);

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::Current;

        if (IS_FORTRAN_LANGUAGE)
        {
            _extra_c_code.prepend(functions_section_tree);
        }
        else
        {
            Nodecl::Utils::append_to_top_level_nodecl(functions_section_tree);
        }
        _cuda_tasks_processed = false;
    }

    if (_is_nanos_get_cublas_handle)
    {
        Nodecl::NodeclBase root = data_flow["nodecl"];
        Source nanox_device_enable_section;
        nanox_device_enable_section << "__attribute__((weak)) char gpu_cublas_init = 1;";

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::C;

        Nodecl::NodeclBase functions_section_tree = nanox_device_enable_section.parse_global(root);

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::Current;

        if (IS_FORTRAN_LANGUAGE)
        {
            _extra_c_code.prepend(functions_section_tree);
        }
        else
        {
            Nodecl::Utils::append_to_top_level_nodecl(functions_section_tree);
        }
    }

    if (!_cuda_file_code.is_null())
    {
        std::string original_filename = TL::CompilationProcess::get_current_file().get_filename();
        std::string new_filename = "cudacc_" + original_filename.substr(0, original_filename.find("."))  + ".cu";

        FILE* ancillary_file = fopen(new_filename.c_str(), "w");
        if (ancillary_file == NULL)
        {
            running_error("%s: error: cannot open file '%s'. %s\n",
                    original_filename.c_str(),
                    new_filename.c_str(),
                    strerror(errno));
        }

        CXX_LANGUAGE()
        {
            // Add to the new intermediate file the *.cu, *.cuh included files.
            // It must be done only in C++ language because the C++ codegen does
            // not deduce the set of used symbols
            add_included_cuda_files(ancillary_file);
        }

        compilation_configuration_t* configuration = ::get_compilation_configuration("cuda");
        ERROR_CONDITION (configuration == NULL, "cuda profile is mandatory when using mnvfc/mnvcc/mnvcxx", 0);

        // Make sure phases are loaded (this is needed for codegen)
        load_compiler_phases(configuration);

        TL::CompilationProcess::add_file(new_filename, "cuda");

        //Remove the intermediate source file
        ::mark_file_for_cleanup(new_filename.c_str());

        Codegen::CudaGPU* phase = reinterpret_cast<Codegen::CudaGPU*>(configuration->codegen_phase);

        bool is_fortan = IS_FORTRAN_LANGUAGE;
        if (is_fortan)
            Source::source_language = SourceLanguage::C;

        phase->codegen_top_level(_cuda_file_code, ancillary_file);

        if (is_fortan)
            Source::source_language = SourceLanguage::Fortran;

        fclose(ancillary_file);

        // Do not forget the clear the code for next files
        _cuda_file_code.get_internal_nodecl() = nodecl_null();

        // Clear the copied cuda functions map
        _copied_cuda_functions = Nodecl::Utils::SimpleSymbolMap();
    }

    if (!_extra_c_code.is_null())
    {
        std::string original_filename = TL::CompilationProcess::get_current_file().get_filename();
        std::string new_filename = "cuda_aux_nanox_outline_file_" + original_filename  + ".c";

        FILE* ancillary_file = fopen(new_filename.c_str(), "w");
        if (ancillary_file == NULL)
        {
            running_error("%s: error: cannot open file '%s'. %s\n",
                    original_filename.c_str(),
                    new_filename.c_str(),
                    strerror(errno));
        }

        CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_C;

        compilation_configuration_t* configuration = ::get_compilation_configuration("auxcc");
        ERROR_CONDITION (configuration == NULL, "auxcc profile is mandatory when using Fortran", 0);

        // Make sure phases are loaded (this is needed for codegen)
        load_compiler_phases(configuration);

        TL::CompilationProcess::add_file(new_filename, "auxcc");

        ::mark_file_for_cleanup(new_filename.c_str());

        Codegen::CodegenPhase* phase = reinterpret_cast<Codegen::CodegenPhase*>(configuration->codegen_phase);
        phase->codegen_top_level(_extra_c_code, ancillary_file);

        CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_FORTRAN;

        fclose(ancillary_file);
        // Do not forget the clear the code for next files
        _extra_c_code.get_internal_nodecl() = nodecl_null();
    }
}

void DeviceCUDA::pre_run(DTO& dto)
{
    _cuda_tasks_processed = false;
}

void DeviceCUDA::run(DTO& dto)
{
}

EXPORT_PHASE(TL::Nanox::DeviceCUDA);
