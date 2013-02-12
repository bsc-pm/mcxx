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

#include "nanox-opencl.hpp"
#include "tl-devices.hpp"
#include "tl-nanos.hpp"
#include "tl-multifile.hpp"
#include "tl-compilerpipeline.hpp"
#include "cxx-profile.h"
// #include "fortran03-scope.h"

//#include "cuda-aux.hpp"
//#include "tl-declarationclosure.hpp"

//#include "tl-cuda.hpp"
//#include "tl-omp-nanox.hpp"

#include "codegen-phase.hpp"
//#include "codegen-cuda.hpp"
#include "cxx-cexpr.h"
//#include "codegen-fortran.hpp"

//#include <iostream>
//#include <fstream>

#include <errno.h>
#include "cxx-driver-utils.h"

using namespace TL;
using namespace TL::Nanox;

static std::string ocl_outline_name(const std::string & name)
{
    return "ocl_" + name;
}

bool DeviceOpenCL::is_gpu_device() const
{
    return true;
}

static void build_empty_body_for_function(
        TL::Symbol function_symbol,
        Nodecl::NodeclBase &function_code,
        Nodecl::NodeclBase &empty_stmt
        )
{
    empty_stmt = Nodecl::EmptyStatement::make("", 0);
    Nodecl::List stmt_list = Nodecl::List::make(empty_stmt);

    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        Nodecl::CompoundStatement compound_statement =
            Nodecl::CompoundStatement::make(stmt_list,
                    /* destructors */ Nodecl::NodeclBase::null(),
                    "", 0);
        stmt_list = Nodecl::List::make(compound_statement);
    }

    Nodecl::NodeclBase context = Nodecl::Context::make(
            stmt_list,
            function_symbol.get_related_scope(), "", 0);

    function_symbol.get_internal_symbol()->defined = 1;

    function_code = Nodecl::FunctionCode::make(context,
            // Initializers
            Nodecl::NodeclBase::null(),
            // Internal functions
            Nodecl::NodeclBase::null(),
            function_symbol,
            "", 0);
}

static TL::Symbol new_function_symbol(
        TL::Symbol current_function,
        const std::string& name,
        TL::Type return_type,
        ObjectList<std::string> parameter_names,
        ObjectList<TL::Type> parameter_types)
{
    Scope sc = current_function.get_scope();

    // FIXME - Wrap
    decl_context_t decl_context = sc.get_decl_context();

    scope_entry_t* entry = new_symbol(decl_context, decl_context.current_scope, name.c_str());
    entry->entity_specs.is_user_declared = 1;

    entry->kind = SK_FUNCTION;
    entry->file = "";
    entry->line = 0;

    // Make it static
    entry->entity_specs.is_static = 1;

    // Make it member if the enclosing function is member
    if (current_function.is_member())
    {
        entry->entity_specs.is_member = 1;
        entry->entity_specs.class_type = current_function.get_class_type().get_internal_type();

        entry->entity_specs.access = AS_PUBLIC;

        ::class_type_add_member(entry->entity_specs.class_type, entry);
    }

    ERROR_CONDITION(parameter_names.size() != parameter_types.size(), "Mismatch between names and types", 0);

    decl_context_t function_context ;
    function_context = new_function_context(decl_context);
    function_context = new_block_context(function_context);

    function_context.function_scope->related_entry = entry;
    function_context.block_scope->related_entry = entry;

    entry->related_decl_context = function_context;

    parameter_info_t* p_types = new parameter_info_t[parameter_types.size()];

    parameter_info_t* it_ptypes = &(p_types[0]);
    ObjectList<TL::Type>::iterator type_it = parameter_types.begin();
    for (ObjectList<std::string>::iterator it = parameter_names.begin();
            it != parameter_names.end();
            it++, it_ptypes++, type_it++)
    {
        scope_entry_t* param = new_symbol(function_context, function_context.current_scope, it->c_str());
        param->entity_specs.is_user_declared = 1;
        param->kind = SK_VARIABLE;
        param->file = "";
        param->line = 0;

        param->defined = 1;

        symbol_set_as_parameter_of_function(param, entry, entry->entity_specs.num_related_symbols);

        param->type_information = get_unqualified_type(type_it->get_internal_type());

        P_LIST_ADD(entry->entity_specs.related_symbols,
                entry->entity_specs.num_related_symbols,
                param);

        it_ptypes->is_ellipsis = 0;
        it_ptypes->nonadjusted_type_info = NULL;
        it_ptypes->type_info = get_indirect_type(param);
    }

    type_t *function_type = get_new_function_type(
            return_type.get_internal_type(),
            p_types,
            parameter_types.size());

    entry->type_information = function_type;

    delete[] p_types;

    return entry;
}


static TL::Symbol new_function_symbol_unpacked(
        TL::Symbol current_function,
        const std::string& function_name,
        CreateOutlineInfo& info,
        Nodecl::Utils::SymbolMap*& out_symbol_map)
{
    Scope sc = current_function.get_scope();

    decl_context_t decl_context = sc.get_decl_context();
    decl_context_t function_context;

    function_context = new_function_context(decl_context);
    function_context = new_block_context(function_context);

    // Create all the symbols and an appropiate mapping

    Nodecl::Utils::SimpleSymbolMap *symbol_map = new Nodecl::Utils::SimpleSymbolMap();

    TL::ObjectList<TL::Symbol> parameter_symbols, private_symbols;

    TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::Symbol sym = (*it)->get_symbol();

        std::string name;
        if (sym.is_valid())
        {
            name = sym.get_name();
            if (IS_CXX_LANGUAGE
                    && name == "this")
            {
                name = "this_";
            }
        }
        else
        {
            name = (*it)->get_field_name();
        }

        bool already_mapped = false;

        switch ((*it)->get_sharing())
        {
            case OutlineDataItem::SHARING_PRIVATE:
                {
                    scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope, name.c_str());
                    private_sym->kind = SK_VARIABLE;
                    private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                    private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                    if (sym.is_valid())
                    {
                        symbol_map->add_map(sym, private_sym);

                        // Copy attributes that must be preserved
                        private_sym->entity_specs.is_allocatable = !sym.is_member() && sym.is_allocatable();
                    }

                    private_symbols.append(private_sym);
                    break;
                }
            case OutlineDataItem::SHARING_SHARED:
            case OutlineDataItem::SHARING_CAPTURE:
            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                {
                    scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                            name.c_str());
                    private_sym->kind = SK_VARIABLE;
                    private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                    private_sym->defined = private_sym->entity_specs.is_user_declared = 1;


                    if (sym.is_valid())
                    {
                        private_sym->entity_specs.is_optional = sym.is_optional();
                        private_sym->entity_specs.is_allocatable =
                            !sym.is_member() && sym.is_allocatable();
                        if (!already_mapped)
                        {
                            symbol_map->add_map(sym, private_sym);
                        }
                    }

                    private_sym->entity_specs.is_allocatable = 
                        sym.is_allocatable() ||
                        (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE) 
                         == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);

                    parameter_symbols.append(private_sym);

                    break;
                }
            case OutlineDataItem::SHARING_REDUCTION:
                {
                    // Original reduced variable. Passed as we pass shared parameters
                    TL::Type param_type = (*it)->get_in_outline_type();
                    scope_entry_t* shared_reduction_sym = ::new_symbol(function_context, function_context.current_scope,
                            (*it)->get_field_name().c_str());
                    shared_reduction_sym->kind = SK_VARIABLE;
                    shared_reduction_sym->type_information = param_type.get_internal_type();
                    shared_reduction_sym->defined = shared_reduction_sym->entity_specs.is_user_declared = 1;
                    parameter_symbols.append(shared_reduction_sym);

                    shared_reduction_sym->entity_specs.is_allocatable = sym.is_valid()
                        && !sym.is_member()
                        && sym.is_allocatable();

                    // Private vector of partial reductions. This is a local pointer variable
                    // rdv stands for reduction vector
                    TL::Type private_reduction_vector_type = (*it)->get_private_type();
                    if (IS_C_LANGUAGE
                            || IS_CXX_LANGUAGE)
                    {
                        // T*
                        private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }

                    scope_entry_t* private_reduction_vector_sym = ::new_symbol(function_context, function_context.current_scope,
                            ("rdv_" + name).c_str());
                    private_reduction_vector_sym->kind = SK_VARIABLE;
                    private_reduction_vector_sym->type_information = private_reduction_vector_type.get_internal_type();
                    private_reduction_vector_sym->defined = private_reduction_vector_sym->entity_specs.is_user_declared = 1;

                    // Local variable (rdp stands for reduction private)
                    scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                            ("rdp_" + name).c_str());
                    private_sym->kind = SK_VARIABLE;
                    private_sym->type_information = (*it)->get_private_type().get_internal_type();
                    private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                    // This variable must be initialized properly.
                    // Create a map for omp_orig and omp_priv
                    OpenMP::Reduction* red = (*it)->get_reduction_info();
                    Nodecl::Utils::SimpleSymbolMap reduction_init_map;
                    reduction_init_map.add_map(red->get_omp_priv(), private_sym);
                    reduction_init_map.add_map(red->get_omp_orig(), shared_reduction_sym);
                    private_sym->value = Nodecl::Utils::deep_copy((*it)->get_reduction_info()->get_initializer(),
                            Scope(function_context),
                            reduction_init_map).get_internal_nodecl();

                    if (sym.is_valid())
                    {
                        symbol_map->add_map(sym, private_sym);
                    }

                    break;
                }
            default:
                {
                    internal_error("Unexpected data sharing kind", 0);
                }
        }
    }

    // Update types of parameters (this is needed by VLAs)
    for (TL::ObjectList<TL::Symbol>::iterator it = parameter_symbols.begin();
            it != parameter_symbols.end();
            it++)
    {
        it->get_internal_symbol()->type_information =
            type_deep_copy(it->get_internal_symbol()->type_information,
                    function_context,
                    symbol_map->get_symbol_map());
    }
    // Update types of privates (this is needed by VLAs)
    for (TL::ObjectList<TL::Symbol>::iterator it = private_symbols.begin();
            it != private_symbols.end();
            it++)
    {
        it->get_internal_symbol()->type_information =
            type_deep_copy(it->get_internal_symbol()->type_information,
                    function_context,
                    symbol_map->get_symbol_map());
    }

    // Now everything is set to register the function
    scope_entry_t* new_function_sym = new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
    new_function_sym->entity_specs.is_user_declared = 1;

    new_function_sym->kind = SK_FUNCTION;
    new_function_sym->file = "";
    new_function_sym->line = 0;

    // Make it static
    new_function_sym->entity_specs.is_static = 1;

    // Make it member if the enclosing function is member
    if (current_function.is_member())
    {
        new_function_sym->entity_specs.is_member = 1;
        new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();

        new_function_sym->entity_specs.access = AS_PUBLIC;

        ::class_type_add_member(new_function_sym->entity_specs.class_type,
                new_function_sym);
    }

    function_context.function_scope->related_entry = new_function_sym;
    function_context.block_scope->related_entry = new_function_sym;

    new_function_sym->related_decl_context = function_context;

    parameter_info_t* p_types = new parameter_info_t[parameter_symbols.size()];

    parameter_info_t* it_ptypes = &(p_types[0]);
    for (ObjectList<TL::Symbol>::iterator it = parameter_symbols.begin();
            it != parameter_symbols.end();
            it++, it_ptypes++)
    {
        scope_entry_t* param = it->get_internal_symbol();

        symbol_set_as_parameter_of_function(param, new_function_sym, new_function_sym->entity_specs.num_related_symbols);

        P_LIST_ADD(new_function_sym->entity_specs.related_symbols,
                new_function_sym->entity_specs.num_related_symbols,
                param);

        it_ptypes->is_ellipsis = 0;
        it_ptypes->nonadjusted_type_info = NULL;

        // FIXME - We should do all the remaining lvalue adjustments
        type_t* param_type = get_unqualified_type(param->type_information);
        it_ptypes->type_info = param_type;
    }

    type_t *function_type = get_new_function_type(
            get_void_type(),
            p_types,
            parameter_symbols.size());

    new_function_sym->type_information = function_type;

    delete[] p_types;

    out_symbol_map = symbol_map;
    return new_function_sym;
}


void DeviceOpenCL::generate_ndrange_code(
        const TL::Symbol& called_task,
        const TL::Symbol& unpacked_function,
        const TL::ObjectList<Nodecl::NodeclBase>& ndrange_args,
        const std::string filename,
        TL::Source& code_ndrange)
{
    int num_args_ndrange = ndrange_args.size();
    Nodecl::Utils::SimpleSymbolMap translate_parameters_map;

    TL::ObjectList<TL::Symbol> parameters_called = called_task.get_function_parameters();
    TL::ObjectList<TL::Symbol> parameters_unpacked = unpacked_function.get_function_parameters();
    ERROR_CONDITION(parameters_called.size() != parameters_unpacked.size(), "Code unreachable", 0);

    int num_params = parameters_called.size();
    for (int i = 0; i < num_params; ++i)
    {
        translate_parameters_map.add_map(
                parameters_called[i],
                parameters_unpacked[i]);
    }

    TL::ObjectList<Nodecl::NodeclBase> new_ndrange;
    for (int i = 0; i < num_args_ndrange; ++i)
    {

        new_ndrange.append(Nodecl::Utils::deep_copy(
                    ndrange_args[i],
                    unpacked_function.get_related_scope(),
                    translate_parameters_map));
    }

    bool dim_const=new_ndrange[0].is_constant();
    
    bool check_dim = !(new_ndrange[num_args_ndrange - 1].is_constant()
            && const_value_is_string(new_ndrange[num_args_ndrange - 1].get_constant())
            && (strcmp(const_value_string_unpack_to_string(new_ndrange[num_args_ndrange-1].get_constant()),"noCheckDim") == 0));
    int num_dim = 0;
    
    if (dim_const){
       num_dim=const_value_cast_to_4(new_ndrange[0].get_constant());
       ERROR_CONDITION(num_dim < 1 || num_dim > 3, "invalid number of dimensions for 'ndrange' clause. Valid values: 1, 2 and 3." , 0);
       ERROR_CONDITION(((num_dim * 3) + 1 + !check_dim) != num_args_ndrange && ((num_dim * 2) + 1 + !check_dim) != num_args_ndrange, "invalid number of arguments for 'ndrange' clause", 0);
    }

   
    std::string compiler_opts;
    if (CURRENT_CONFIGURATION->opencl_build_options!=NULL){        
        compiler_opts=std::string(CURRENT_CONFIGURATION->opencl_build_options);
    }
    //Create OCL Kernel
    code_ndrange << "void* ompss_kernel_ocl = nanos_create_current_kernel(\"" << called_task.get_name() << "\",\"" << filename << "\",\"" <<  compiler_opts << "\");";
    //Prepare setArgs
    for (int i = 0; i < num_params; ++i) {
        //Check original function type
        if (parameters_called[i].get_type().is_pointer()) {
            code_ndrange << "nanos_opencl_set_bufferarg(ompss_kernel_ocl," << i << "," << as_symbol(parameters_unpacked[i]) <<");";
        } else {            
            code_ndrange << "nanos_opencl_set_arg(ompss_kernel_ocl," << i << ", "
                    "sizeof(" << as_symbol(parameters_unpacked[i]) << "),&" << as_symbol(parameters_unpacked[i]) <<");";
        }        
    }
    //Prepare ndrange calc pointers and arrays
    code_ndrange << "int num_dim=" << as_expression(new_ndrange[0]) <<";";
    code_ndrange << "size_t offset_tmp["<< as_expression(new_ndrange[0]) <<"];";
    code_ndrange << "size_t offset_arr["<< as_expression(new_ndrange[0]) <<"];";
    code_ndrange << "size_t local_size_arr["<< as_expression(new_ndrange[0]) <<"];";
    code_ndrange << "size_t global_size_arr["<< as_expression(new_ndrange[0]) <<"];";  
    code_ndrange << "size_t* local_size_ptr;"; 
    code_ndrange << "size_t* offset_ptr;"; 
    code_ndrange << "size_t* global_size_ptr;"; 
    code_ndrange << "size_t* final_local_size_ptr;"; 
    code_ndrange << "short local_size_zero=0;"; 
    code_ndrange << "int i=0;";
    int num_dim_offset=num_dim;
    //Build arrays with information from ndrange clause or pointing to the ndrange pointers
    if (!dim_const){
        if (num_args_ndrange==3){
            code_ndrange << "for (i=0;i< num_dim;++i){ offset_tmp[i]=0; };";
            code_ndrange << "offset_ptr=offset_tmp;";
            code_ndrange << "global_size_ptr=" << as_expression(new_ndrange[1]) << ";";
            code_ndrange << "local_size_ptr=" << as_expression(new_ndrange[2]) << ";";                    
        } else if (num_args_ndrange==4){            
            code_ndrange << "offset_ptr=" << as_expression(new_ndrange[1]) << ";";
            code_ndrange << "global_size_ptr=" << as_expression(new_ndrange[2]) << ";";
            code_ndrange << "local_size_ptr=" << as_expression(new_ndrange[3]) << ";"; 
        } else {
            WARNING_MESSAGE("Invalid number of parameters for ndrange, when number of dimensions is not const, it must be 3 or 4",0);
        }
    } else {
        code_ndrange << "size_t local_size_tmp[num_dim];";
        code_ndrange << "size_t global_size_tmp[num_dim];";    
        for (int i=1; i <= num_dim; ++i)
        {        
            if (((num_dim * 3) + 1 + !check_dim) != num_args_ndrange){   
                num_dim_offset=0;
                code_ndrange << "offset_tmp[" << i-1 << "] = 0;";
            } else {            
                code_ndrange << "offset_tmp[" << i-1 << "] = (" << as_expression(new_ndrange[i]) << ");";
            }
            code_ndrange << "local_size_tmp[" << i-1 << "] = " << as_expression(new_ndrange[num_dim + num_dim_offset + i]) << ";";
            code_ndrange << "global_size_tmp[" << i-1 << "] = " << as_expression(new_ndrange[num_dim_offset + i]) << ";";
        }   
        code_ndrange << "offset_ptr=offset_tmp;";
        code_ndrange << "local_size_ptr=local_size_tmp;";
        code_ndrange << "global_size_ptr=global_size_tmp;";
    }
    //Check if local_size has zeros
    code_ndrange << "for (i=0;i<num_dim;++i){ if (local_size_ptr[i]==0) local_size_zero=1; };";
    code_ndrange << "if (local_size_zero) { for (i=0;i<num_dim;++i){  local_size_ptr[i]=1; }}";

    //Now do the rounding
    if (check_dim)
    {
        code_ndrange << "for (i=0;i<num_dim;++i){";
        code_ndrange << "offset_arr[i] = " << "offset_ptr[i];";

        code_ndrange << "local_size_arr[i] = "
            << "(("
            << "global_size_ptr[i]"
            << " < " << "local_size_ptr[i]"
            << ") ? (" << "global_size_ptr[i]"
            << ") : (" << "local_size_ptr[i]"
            << "));";

        code_ndrange << "global_size_arr[i] = "
            << "(("                    
            << "global_size_ptr[i]"
            << " < " <<  "local_size_ptr[i]"
            << ") ? (" << "global_size_ptr[i]"
            << ") : ((" << "global_size_ptr[i]"
            << ") + ((" << "global_size_ptr[i]" << " %  " << "local_size_ptr[ i]"
            << " == 0) ? 0 : (" <<  "local_size_ptr[ i ]" << "-" << "global_size_ptr[ i]" << " %  " <<  "local_size_ptr[i]" <<"))));";
        code_ndrange << "}";
     }
    
    
    if (check_dim){
        code_ndrange << "if (local_size_zero) { final_local_size_ptr=0; } else {  final_local_size_ptr = local_size_arr; }";
        //Launch kernel/ it will be freed inside, with ndrange calculated inside the checkDim loop
        code_ndrange << "nanos_exec_kernel(ompss_kernel_ocl, " << num_dim << ",offset_arr,final_local_size_ptr,global_size_arr);";    
    } else {
        code_ndrange << "if (local_size_zero) { final_local_size_ptr=0; } else {  final_local_size_ptr = local_size_ptr; }";
        code_ndrange << "nanos_exec_kernel(ompss_kernel_ocl, " << num_dim << ",offset_ptr,final_local_size_ptr,global_size_ptr);";            
    }

}

void DeviceOpenCL::create_outline(CreateOutlineInfo &info,
        Nodecl::NodeclBase &outline_placeholder,
        Nodecl::NodeclBase &output_statements,
        Nodecl::Utils::SymbolMap* &symbol_map)
{
    if (IS_FORTRAN_LANGUAGE)
        running_error("Fortran for OpenCL devices is not supported yet\n", 0);
    
    // Unpack DTO
    const std::string& device_outline_name = ocl_outline_name(info._outline_name);
    const Nodecl::NodeclBase& original_statements = info._original_statements;
    const TL::Symbol& called_task = info._called_task;

    output_statements = original_statements;

    ERROR_CONDITION(called_task.is_valid() && !called_task.is_function(),
            "The '%s' symbol is not a function", called_task.get_name().c_str());

    TL::Symbol current_function =
        original_statements.retrieve_context().get_decl_context().current_scope->related_entry;

    if (current_function.is_nested_function())
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            running_error("%s: error: nested functions are not supported\n",
                    original_statements.get_locus().c_str());
    }

    Source unpacked_arguments, private_entities;

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
                        if ((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED
                                && !(IS_CXX_LANGUAGE && (*it)->get_symbol().get_name() == "this"))
                        {
                            argument << "*(args." << (*it)->get_field_name() << ")";
                        }
                        // Any other thing is passed by value
                        else
                        {
                            argument << "args." << (*it)->get_field_name();
                        }

                        if (IS_CXX_LANGUAGE
                                && (*it)->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
                        {
                            internal_error("Not yet implemented: call the destructor", 0);
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

    // Add the user function to the intermediate file
    if (called_task.is_valid()
            && !called_task.get_function_code().is_null())
    {
        //_ocl_file_code.append(Nodecl::Utils::deep_copy(
        //            called_task.get_function_code(),
        //            called_task.get_scope()));

        // Remove the user function definition from the original source because
        // It is used only in the intermediate file
        Nodecl::Utils::remove_from_enclosing_list(called_task.get_function_code());
    }

    // Create the new unpacked function
    TL::Symbol unpacked_function = new_function_symbol_unpacked(
            current_function,
            device_outline_name + "_unpacked",
            info,
            symbol_map);
    
    //Get file clause, if not present, use global file
    std::string file=info._target_info.get_file();
    if (file.empty()){
        ERROR_CONDITION(CURRENT_CONFIGURATION->opencl_code_file==NULL,"No file specified for kernel '%s', use file clause or --opencl-code-file mercurium flag",called_task.get_name().c_str());
        file=std::string(CURRENT_CONFIGURATION->opencl_code_file);
    }

    Source ndrange_code;
    if (called_task.is_valid()
            && info._target_info.get_ndrange().size() > 0)
    {
        generate_ndrange_code(called_task,
                unpacked_function,
                info._target_info.get_ndrange(),
                file,
                ndrange_code);
    }

    // The unpacked function must not be static and must have external linkage because
    // this function is called from the original source and but It is defined
    // in cudacc_filename.cu
    unpacked_function.get_internal_symbol()->entity_specs.is_static = 0;
    if (IS_C_LANGUAGE)
    {
        unpacked_function.get_internal_symbol()->entity_specs.linkage_spec = "\"C\"";
    }

    Nodecl::NodeclBase unpacked_function_code, unpacked_function_body;
    build_empty_body_for_function(unpacked_function,
            unpacked_function_code,
            unpacked_function_body);

    Source unpacked_source;
    unpacked_source
        << "{"
        << private_entities
        << ndrange_code
        << "}"
        ;

    Nodecl::NodeclBase new_unpacked_body =
        unpacked_source.parse_statement(unpacked_function_body);
    unpacked_function_body.replace(new_unpacked_body);

//    if (called_task.is_valid()
//            && info._target_info.get_ndrange().size() > 0)
//    {
//        generate_ndrange_kernel_call(
//                outline_placeholder.retrieve_context(),
//                original_statements,
//                output_statements);
//    }

    // Add the unpacked function
    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, unpacked_function_code);

    // Add a declaration of the unpacked function symbol in the original source
    if (IS_CXX_LANGUAGE)
    {
        Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                /* optative context */ nodecl_null(),
                unpacked_function,
                original_statements.get_filename(),
                original_statements.get_line());
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

    TL::Symbol outline_function = new_function_symbol(
            current_function,
            device_outline_name,
            TL::Type::get_void_type(),
            structure_name,
            structure_type);

    Nodecl::NodeclBase outline_function_code, outline_function_body;
    build_empty_body_for_function(outline_function,
            outline_function_code,
            outline_function_body);

    Source outline_src,
           instrument_before,
           instrument_after;

    outline_src
        << "{"
        <<      instrument_before
        <<      device_outline_name << "_unpacked(" << unpacked_arguments << ");"
        <<      instrument_after
        << "}"
        ;

    if (instrumentation_enabled())
    {
        get_instrumentation_code(
                info._called_task,
                outline_function,
                outline_function_body,
                info._task_label,
                original_statements.get_filename(),
                original_statements.get_line(),
                instrument_before,
                instrument_after);
    }

    Nodecl::NodeclBase new_outline_body = outline_src.parse_statement(outline_function_body);
    outline_function_body.replace(new_outline_body);
    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, outline_function_code);
    
    //Dummy function call placeholder
    Source unpacked_ndr_code;
    unpacked_ndr_code << statement_placeholder(outline_placeholder);
    Nodecl::NodeclBase new_unpacked_ndr_code = unpacked_ndr_code.parse_statement(unpacked_function_body);
    outline_placeholder=new_unpacked_ndr_code;
}

//
DeviceOpenCL::DeviceOpenCL()
    : DeviceProvider(/* device_name */ std::string("opencl")) //, _cudaFilename(""), _cudaHeaderFilename("")
{
    set_phase_name("Nanox OpenCL support");
    set_phase_description("This phase is used by Nanox phases to implement OpenCL device support");
}


void DeviceOpenCL::get_device_descriptor(DeviceDescriptorInfo& info,
        Source &ancillary_device_description,
        Source &device_descriptor,
        Source &fortran_dynamic_init UNUSED_PARAMETER)
{
    const std::string& device_outline_name = ocl_outline_name(info._outline_name);
    if (Nanos::Version::interface_is_at_least("master", 5012))
    {
        ancillary_device_description
            << comment("OpenCL device descriptor")
            << "static nanos_opencl_args_t "
            << device_outline_name << "_args;"                
            << device_outline_name << "_args.outline = (void(*)(void*))" << device_outline_name << ";"
            ;
    }
    else
    {
        internal_error("Unsupported Nanos version.", 0);
    }

    device_descriptor << "{ &nanos_opencl_factory, &" << device_outline_name << "_args }";
}


void DeviceOpenCL::add_included_opencl_files(FILE* file)
{
    ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();
    std::string cuda_file_ext(".cu\"");
    std::string cuda_header_ext(".cuh\"");

    for (ObjectList<IncludeLine>::iterator it = lines.begin(); it != lines.end(); it++)
    {
        std::string line = (*it).get_preprocessor_line();
        std::string extension = line.substr(line.find_last_of("."));

        if (extension == cuda_file_ext || extension == cuda_header_ext)
        {
            int output = fprintf(file, "%s\n", line.c_str());
            if (output < 0)
                internal_error("Error trying to write the intermediate opencl file\n", 0);
        }
    }
}

bool DeviceOpenCL::allow_mandatory_creation()
{
    return true;
}

void DeviceOpenCL::copy_stuff_to_device_file(const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied)
{
//    for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = stuff_to_be_copied.begin();
//            it != stuff_to_be_copied.end();
//            ++it)
//    {
//        _ocl_file_code.append(Nodecl::Utils::deep_copy(*it, *it));
//    }
}

void DeviceOpenCL::phase_cleanup(DTO& data_flow)
{
    if (!_ocl_file_code.is_null())
    {
//        std::string original_filename = TL::CompilationProcess::get_current_file().get_filename();
//        std::string new_filename = "oclcc_" + original_filename.substr(0, original_filename.find("."))  + ".cu";
//
//        FILE* ancillary_file = fopen(new_filename.c_str(), "w");
//        if (ancillary_file == NULL)
//        {
//            running_error("%s: error: cannot open file '%s'. %s\n",
//                    original_filename.c_str(),
//                    new_filename.c_str(),
//                    strerror(errno));
//        }
//
//        CXX_LANGUAGE()
//        {
//            // Add to the new intermediate file the *.cu, *.cuh included files.
//            // It must be done only in C++ language because the C++ codegen do
//            // not deduce the set of used symbols
//            add_included_opencl_files(ancillary_file);
//        }

//        compilation_configuration_t* configuration = ::get_compilation_configuration("cuda");
//        ERROR_CONDITION (configuration == NULL, "cuda profile is mandatory when using mnvcc/mnvcxx", 0);
//
//        // Make sure phases are loaded (this is needed for codegen)
//        load_compiler_phases(configuration);
//
////        TL::CompilationProcess::add_file(new_filename, "cuda");
//
//        //Remove the intermediate source file
//        ::mark_file_for_cleanup(new_filename.c_str());

//        Codegen::CudaGPU* phase = reinterpret_cast<Codegen::CudaGPU*>(configuration->codegen_phase);
//
//        phase->codegen_top_level(_ocl_file_code, ancillary_file);

       // fclose(ancillary_file);

        // Do not forget the clear the code for next files
        _ocl_file_code.get_internal_nodecl() = nodecl_null();
    }

}

void DeviceOpenCL::pre_run(DTO& dto)
{
}

void DeviceOpenCL::run(DTO& dto)
{
}

EXPORT_PHASE(TL::Nanox::DeviceOpenCL);
