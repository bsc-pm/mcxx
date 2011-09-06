/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include "tl-nanos.hpp"
#include "tl-parallel-common.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

static void fill_dimensions(int n_dims, int actual_dim, std::string* dim_sizes, Type dep_type, Source& dims_description, Scope sc);

Source TL::Nanox::common_parallel_code(const std::string& outline_name, 
        const std::string& struct_arg_type_name,
        Source num_threads,
        ScopeLink sl,
        DataEnvironInfo& data_environ_info,
        AST_t parallel_code,
        const ObjectList<std::string>& current_targets)
{
    Source result;

    Source fill_outline_arguments, fill_immediate_arguments;

    fill_data_args(
            "ol_args",
            data_environ_info, 
            ObjectList<OpenMP::DependencyItem>(), // empty
            /* is_pointer */ true,
            fill_outline_arguments);

    bool immediate_is_alloca = false;
    bool env_is_runtime_sized = data_environ_info.environment_is_runtime_sized();

    if (env_is_runtime_sized)
    {
        immediate_is_alloca = true;
    }

    fill_data_args(
            "imm_args",
            data_environ_info, 
            ObjectList<OpenMP::DependencyItem>(), // empty
            /* is_pointer */ immediate_is_alloca,
            fill_immediate_arguments);

    Source device_descriptor, 
           device_description, 
           device_description_line, 
           num_devices,
           ancillary_device_description;
    device_descriptor << outline_name << "_devices";
    device_description
        << ancillary_device_description
        << "nanos_device_t " << device_descriptor << "[] ="
        << "{"
        << device_description_line
        << "};"
        ;

    DeviceHandler &device_handler = DeviceHandler::get_device_handler();
    for (ObjectList<std::string>::const_iterator it = current_targets.begin();
            it != current_targets.end();
            it++)
    {
        DeviceProvider* device_provider = device_handler.get_device(*it);

        if (device_provider == NULL)
        {
            internal_error("invalid device '%s'\n",
                    it->c_str());
        }

        OutlineFlags outline_flags;

        outline_flags.leave_team = true;
        outline_flags.barrier_at_end = true;

        Source initial_setup, replaced_body;

        device_provider->do_replacements(data_environ_info,
                parallel_code,
                sl,
                initial_setup,
                replaced_body);

        device_provider->create_outline(outline_name,
                struct_arg_type_name,
                data_environ_info,
                outline_flags,
                parallel_code,
                sl,
                initial_setup,
                replaced_body);

        device_provider->get_device_descriptor(outline_name, 
                data_environ_info, 
                outline_flags,
                parallel_code,
                sl,
                ancillary_device_description, 
                device_description_line);
    }

    num_devices << current_targets.size();

    Source struct_runtime_size, struct_size;
    Source immediate_decl;

    if (!immediate_is_alloca)
    {
        immediate_decl
            << struct_arg_type_name << " imm_args;"
            ;
    }
    else
    {
        Source alloca_size;
        immediate_decl 
            << struct_arg_type_name << " * __restrict imm_args = (" << struct_arg_type_name << "*) __builtin_alloca(" << struct_size << ");"
            ;
    }

    Scope scope = sl.get_scope(parallel_code);

    if (env_is_runtime_sized)
    {
        struct_runtime_size
            << "int struct_runtime_size = "
            << "sizeof(" << struct_arg_type_name << ") + "
            << data_environ_info.sizeof_variable_part(scope)
            << ";"
            ;
        struct_size
            << "struct_runtime_size" 
            ;
    }
    else
    {
        struct_size
            << "sizeof("  << struct_arg_type_name << ")"
            ;
    }

    Source alignment;
    if (Nanos::Version::interface_is_at_least("master", 5004))
    {
        alignment <<  "__alignof__(" << struct_arg_type_name << "),"
            ;
    }

    // FIXME - This will be meaningful with 'copy_in' and 'copy_out'
    Source num_copies, copy_data, imm_copy_data;
    num_copies << "0";
    copy_data << "(nanos_copy_data_t**)0";
    imm_copy_data << "(nanos_copy_data_t*)0";

    Source xlate_arg;

    if (Nanos::Version::interface_is_at_least("master", 5005))
    {
        C_LANGUAGE()
        {
            xlate_arg << ", (void*)0"
                ;
        }
        CXX_LANGUAGE()
        {
            xlate_arg << ", 0"
                ;
        }
    }

    result
        << "{"
        <<   "unsigned int _nanos_num_threads = " << num_threads << ";"
        <<   "nanos_team_t _nanos_team = (nanos_team_t)0;"
        <<   "nanos_thread_t _nanos_threads[_nanos_num_threads];"
        <<   "nanos_err_t err;"
        <<   "err = nanos_create_team(&_nanos_team, (nanos_sched_t)0, &_nanos_num_threads,"
        <<              "(nanos_constraint_t*)0, /* reuse_current */ 1, _nanos_threads);"
        <<   "if (err != NANOS_OK) nanos_handle_error(err);"

        <<   device_description      

        <<   struct_runtime_size

        <<   "nanos_wd_props_t props;"
        <<   "__builtin_memset(&props, 0, sizeof(props));"
        <<   "props.mandatory_creation = 1;"
        <<   "int _i;"
        <<   "for (_i = 1; _i < _nanos_num_threads; _i++)"
        <<   "{"
        //   We have to create a wd tied to a thread
        <<      struct_arg_type_name << " *ol_args = 0;"
        <<      "props.tie_to = _nanos_threads[_i];"
        <<      "nanos_wd_t wd = 0;"
        <<      "err = nanos_create_wd(&wd, " << num_devices << ","
        <<                    device_descriptor << ", "
        <<                    struct_size << ","
        <<                    alignment
        <<                    "(void**)&ol_args,"
        <<                    "nanos_current_wd(), "
        <<                    "&props, " << num_copies << "," << copy_data << ");"
        <<      "if (err != NANOS_OK) nanos_handle_error(err);"
        <<      fill_outline_arguments
        <<      "err = nanos_submit(wd, 0, (nanos_dependence_t*)0, 0);"
        <<      "if (err != NANOS_OK) nanos_handle_error(err);"
        <<   "}"
        <<   "props.tie_to = &_nanos_threads[0];"
        <<   immediate_decl
        <<   fill_immediate_arguments
        <<   "err = nanos_create_wd_and_run(" << num_devices << ", "
        <<                              device_descriptor << ", "
        <<                              struct_size << ", " 
        <<                              alignment
        <<                              (immediate_is_alloca ? "imm_args" : "&imm_args") << ","
        <<                              "0,"
        <<                              "(nanos_dependence_t*)0, "
        <<                              "&props, " << num_copies << "," << imm_copy_data << xlate_arg << ");"
        <<   "if (err != NANOS_OK) nanos_handle_error(err);"
        <<   "err = nanos_end_team(_nanos_team);"
        <<   "if (err != NANOS_OK) nanos_handle_error(err);"
        << "}"
        ;

    return result;
}


void TL::Nanox::regions_spawn(Source& dependency_struct, Source& dependency_array, Source& dependency_regions, Source& num_dependences,
                              Source& fill_dependences_outline, Source& fill_dependences_immediate,
                              ObjectList<OpenMP::DependencyItem> dependences, DataEnvironInfo data_environ_info,
                              bool immediate_is_alloca, PragmaCustomConstruct ctr, bool is_task)
{
    if (Nanos::Version::interface_is_at_least("master", 6001))
    {
        dependency_struct << "nanos_data_access_t";
        
        if (!dependences.empty())
        {
            dependency_array << "_data_accesses";
            num_dependences << dependences.size();
            Source dependency_defs_outline, dependency_defs_immediate;
            fill_dependences_outline
                << dependency_regions
                << "nanos_data_access_t _data_accesses[" << num_dependences << "] = {"
                << dependency_defs_outline
                << "};"
                ;

            if (is_task)
            {    
                fill_dependences_immediate
                    << dependency_regions
                    << "nanos_data_access_t _data_accesses[" << num_dependences << "] = {"
                    << dependency_defs_immediate
                    << "};"
                    ;
            }
                    
            int num_dep = 0;
            for (ObjectList<OpenMP::DependencyItem>::iterator it = dependences.begin();
                    it != dependences.end();
                    it++)
            {
                // Set dependency flags
                
                Source dependency_flags;
                Source reduction_flag;
                dependency_flags << "{";
                OpenMP::DependencyDirection attr = it->get_kind();
               
                if (is_task)
                {
                    if (!((attr & OpenMP::DEP_REDUCTION) == OpenMP::DEP_REDUCTION))
                    {
                        reduction_flag << "0";

                        if ((attr & OpenMP::DEP_DIR_INPUT) == OpenMP::DEP_DIR_INPUT)
                        {
                            dependency_flags << "1,"; 
                        }
                        else
                        {
                            dependency_flags << "0,"; 
                        }
                        if ((attr & OpenMP::DEP_DIR_OUTPUT) == OpenMP::DEP_DIR_OUTPUT)
                        {
                            dependency_flags << "1,"; 
                        }
                        else
                        {
                            dependency_flags << "0,"; 
                        }
                    }
                    else 
                    {
                        reduction_flag << "1";
                        // Reduction behaves like an inout
                        dependency_flags << "1, 1,";
                    }
                    
                    if ((attr & OpenMP::DEP_REDUCTION) == OpenMP::DEP_REDUCTION)
                    {
                        // Reductions cannot be renamed
                        dependency_flags << "0,"
                            ;
                    }
                    else
                    {
                        // Can rename otherwise
                        dependency_flags << "1,"
                            ;
                    }

                    dependency_flags << reduction_flag;
                }
                else
                {
                    if ((attr & OpenMP::DEP_DIR_INPUT) == OpenMP::DEP_DIR_INPUT)
                    {
                        dependency_flags << "1,"; 
                    }
                    else
                    {
                        dependency_flags << "0,"; 
                    }
                    if ((attr & OpenMP::DEP_DIR_OUTPUT) == OpenMP::DEP_DIR_OUTPUT)
                    {
                        dependency_flags << "1,"; 
                    }
                    else
                    {
                        dependency_flags << "0,"; 
                    }
                    
                    // Can rename in this case
                    dependency_flags << "1";
                }

                dependency_flags << "}";


                Source dependency_field_name;
                DataReference data_ref = it->get_dependency_expression();
                Symbol sym = data_ref.get_base_symbol();

                DataEnvironItem data_env_item = data_environ_info.get_data_of_symbol(sym);

                if (data_env_item.get_symbol().is_valid())
                {
                    dependency_field_name
                        << data_env_item.get_field_name();
                }
                else
                {
                    internal_error("symbol without data environment info %s",
                            it->get_dependency_expression().prettyprint().c_str());
                }

                DataReference dependency_expression = it->get_dependency_expression();
                Source dims_description;

                Type dependency_type = dependency_expression.get_type();
                int num_dimensions = dependency_type.get_num_dimensions();
                
                // Compute the base type of the dependency and the array containing the size of each dimension
                Type dependency_base_type = dependency_type;
                std::string dimension_sizes[num_dimensions];         
                for (int dim = 0; dim < num_dimensions; dim++)
                {
                    dimension_sizes[dim] = dependency_base_type.array_get_size().prettyprint();
                    dependency_base_type = dependency_base_type.array_element();
                }
                std::string base_type_name = dependency_base_type.get_declaration(data_ref.get_scope(), "");
                
                
                // Generate the spawn
                dependency_regions << "nanos_region_dimension_t dimensions" << num_dep << "[" << std::max(num_dimensions,1) << "] = {"
                                   << dims_description << "};";
               
                if (num_dimensions == 0) 
                {
                    dims_description << "{" 
                                     << "sizeof(" << base_type_name << "), " 
                                     << "0, "
                                     << "sizeof(" << base_type_name << ")" 
                                     << "}"
                                     ;
                }
                else
                {
                    // Less significant dimension s computed in bytes
                    Type aux_type = dependency_type;             
                    while (aux_type.array_element().is_array()) 
                    {
                        aux_type = aux_type.array_element();
                    }
                              
                    if (aux_type.array_is_region())
                    {
                        AST_t lb, ub, size;
                        aux_type.array_get_region_bounds(lb, ub);
                        size = aux_type.array_get_region_size();
                        
                        dims_description << "{" 
                            << "sizeof(" << base_type_name << ") * (" << dimension_sizes[num_dimensions-1] << "), " 
                            << "sizeof(" << base_type_name << ") * (" << lb.prettyprint() << "), "
                            << "sizeof(" << base_type_name << ") * (" << size.prettyprint() << ")  /**/"
                            << "}"
                            ;
                    }
                    else
                    {
                        std::string lb;
                        if (IS_C_LANGUAGE)
                        {
                            lb = "0";
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            lb = "1";
                        }
                        
                        dims_description << "{" 
                            << "sizeof(" << base_type_name << ") * (" << dimension_sizes[num_dimensions-1] << "), " 
                            << lb << ", "
                            << "sizeof(" << base_type_name << ") * (" << dimension_sizes[num_dimensions-1] << ")"
                            << "}"
                            ;     
                    }
                   
                    // The rest of dimensions, if there are, are computed in terms of number of elements
                    if (num_dimensions > 1)
                    {    
                        fill_dimensions(num_dimensions, 0, dimension_sizes, dependency_type, 
                                        dims_description, dependency_expression.get_scope());
                    }
                }
     
                Source dependency_offset, imm_dependency_offset;

                if (num_dimensions == 0) num_dimensions++;
                dependency_defs_outline
                    << "{"
                    << "(void*)ol_args->" << dependency_field_name << ","
                    << dependency_flags << ","
                    << num_dimensions << ","
                    << "dimensions" << num_dep
                    << "}"
                    ;

                Source dep_expr_addr = data_ref.get_address();

                dependency_offset
                    << "((char*)(" << dep_expr_addr << ") - " << "(char*)ol_args->" << dependency_field_name << ")"
                    ;

                if (is_task)
                {
                    if (!immediate_is_alloca)
                    {
                        dependency_defs_immediate
                            << "{"
                            << "(void*)imm_args." << dependency_field_name << ","
                            << dependency_flags << ","
                            << num_dimensions << ","
                            << "dimensions" << num_dep
                            << "}"
                            ;

                        imm_dependency_offset
                            << "((char*)(" << dep_expr_addr << ") - " << "(char*)imm_args." << dependency_field_name << ")"
                            ;
                    }
                    else
                    {
                        dependency_defs_immediate
                            << "{"
                            << "(void*)imm_args->" << dependency_field_name << ","
                            << dependency_flags << ","
                            << num_dimensions << ","
                            << "dimensions" << num_dep
                            << "}"
                            ;

                        imm_dependency_offset
                            << "((char*)(" << dep_expr_addr << ") - " << "(char*)imm_args->" << dependency_field_name << ")"
                            ;
                    }
                }

                if ((it + 1) != dependences.end())
                {
                    dependency_defs_outline << ",";
                    if (is_task)
                    {    
                        dependency_defs_immediate << ",";
                    }
                }

                num_dep++;
            }
        }
        else
        {
            dependency_array << "0";
            num_dependences << "0";
        }
    }
    else
    {
        dependency_struct << "nanos_dependence_t";
        
        if (!dependences.empty())
        {
            dependency_array << "_dependences";
            num_dependences << dependences.size();
            Source dependency_defs_outline, dependency_defs_immediate;
            fill_dependences_outline
                << "nanos_dependence_t _dependences[" << num_dependences << "] = {"
                << dependency_defs_outline
                << "};"
                ;
            
            if (is_task)
            {
                fill_dependences_immediate
                    << "nanos_dependence_t _dependences[" << num_dependences << "] = {"
                    << dependency_defs_immediate
                    << "};"
                    ;
            }
                    
            int num_dep = 0;
            for (ObjectList<OpenMP::DependencyItem>::iterator it = dependences.begin();
                    it != dependences.end();
                    it++)
            {
                // Set dependency flags
                Source dependency_flags;
                Source reduction_flag;
                dependency_flags << "{";
                OpenMP::DependencyDirection attr = it->get_kind();
                
                if (is_task)
                {
                    if (!((attr & OpenMP::DEP_REDUCTION) == OpenMP::DEP_REDUCTION))
                    {
                        if (Nanos::Version::interface_is_at_least("master", 5001))
                        {
                            reduction_flag << "0";
                        }

                        if ((attr & OpenMP::DEP_DIR_INPUT) == OpenMP::DEP_DIR_INPUT)
                        {
                            dependency_flags << "1,"; 
                        }
                        else
                        {
                            dependency_flags << "0,"; 
                        }
                        if ((attr & OpenMP::DEP_DIR_OUTPUT) == OpenMP::DEP_DIR_OUTPUT)
                        {
                            dependency_flags << "1,"; 
                        }
                        else
                        {
                            dependency_flags << "0,"; 
                        }
                    }
                    else 
                    {
                        if (!Nanos::Version::interface_is_at_least("master", 5001))
                        {
                            fprintf(stderr,
                                    "%s: warning: the current version of Nanos does not"
                                    " support reduction dependencies in Superscalar\n",
                                    ctr.get_ast().get_locus().c_str());
                        }
                        else
                        {
                            reduction_flag << "1";
                        }
                        // Reduction behaves like an inout
                        dependency_flags << "1, 1,";
                    }
                    
                    if ((attr & OpenMP::DEP_REDUCTION) == OpenMP::DEP_REDUCTION)
                    {
                        // Reductions cannot be renamed
                        dependency_flags << "0,";
                    }
                    else
                    {
                        // Can rename otherwise
                        dependency_flags << "1,";
                    }

                    dependency_flags << reduction_flag;
                }
                else
                {
                    if ((attr & OpenMP::DEP_DIR_INPUT) == OpenMP::DEP_DIR_INPUT)
                    {
                        dependency_flags << "1,"; 
                    }
                    else
                    {
                        dependency_flags << "0,"; 
                    }
                    if ((attr & OpenMP::DEP_DIR_OUTPUT) == OpenMP::DEP_DIR_OUTPUT)
                    {
                        dependency_flags << "1,"; 
                    }
                    else
                    {
                        dependency_flags << "0,"; 
                    }
                    
                    // Can rename in this case
                    dependency_flags << "1";
                }

                dependency_flags << "}";

                Source dependency_field_name;

                DataReference data_ref = it->get_dependency_expression();
                Symbol sym = data_ref.get_base_symbol();

                DataEnvironItem data_env_item = data_environ_info.get_data_of_symbol(sym);

                if (data_env_item.get_symbol().is_valid())
                {
                    dependency_field_name
                        << data_env_item.get_field_name();
                }
                else
                {
                    internal_error("symbol without data environment info %s",
                            it->get_dependency_expression().prettyprint().c_str());
                }

                DataReference dependency_expression = it->get_dependency_expression();

                Source dep_size;
                dep_size << dependency_expression.get_sizeof();

                Source dependency_offset, imm_dependency_offset;

                dependency_defs_outline
                    << "{"
                    << "(void**)&ol_args->" << dependency_field_name << ","
                    << dependency_offset << ","
                    << dependency_flags << ","
                    << dep_size  
                    << "}"
                    ;

                Source dep_expr_addr = data_ref.get_address();

                dependency_offset
                    << "((char*)(" << dep_expr_addr << ") - " << "(char*)ol_args->" << dependency_field_name << ")"
                    ;

                if (is_task)
                {    
                    if (!immediate_is_alloca)
                    {
                        dependency_defs_immediate
                            << "{"
                            << "(void**)&imm_args." << dependency_field_name << ","
                            << imm_dependency_offset << ","
                            << dependency_flags << ","
                            << dep_size 
                            << "}"
                            ;

                        imm_dependency_offset
                            << "((char*)(" << dep_expr_addr << ") - " << "(char*)imm_args." << dependency_field_name << ")"
                            ;
                    }
                    else
                    {
                        dependency_defs_immediate
                            << "{"
                            << "(void**)imm_args->" << dependency_field_name << ","
                            << imm_dependency_offset << ","
                            << dependency_flags << ","
                            << dep_size 
                            << "}"
                            ;

                        imm_dependency_offset
                            << "((char*)(" << dep_expr_addr << ") - " << "(char*)imm_args->" << dependency_field_name << ")"
                            ;
                    }
                }

                if ((it + 1) != dependences.end())
                {
                    dependency_defs_outline << ",";
                    dependency_defs_immediate << ",";
                }

                num_dep++;
            }
        }
        else
        {
            dependency_array << "0";
            num_dependences << "0";
        }        
    }
}


static void fill_dimensions(int n_dims, int actual_dim, std::string* dim_sizes, Type dep_type, Source& dims_description, Scope sc)
{
    if (actual_dim > 1)
    {
        fill_dimensions(n_dims, actual_dim+1, dim_sizes, dep_type.array_element(), dims_description, sc);
    }
    
    if (dep_type.array_is_region())
    {
        AST_t lb, ub, size;
        dep_type.array_get_region_bounds(lb, ub);
        size = dep_type.array_get_region_size();
       
        dims_description << ", {" 
            << "(" << dim_sizes[actual_dim] << "), " 
            << "(" << lb.prettyprint() << "), "
            << "(" << size.prettyprint() << ")"
            << "}"
            ;
    }
    else
    {
        std::string lb;
        if (IS_C_LANGUAGE)
        {
            lb = "0";
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            lb = "1";
        }
      
        dims_description << ", {" 
            << "(" << dep_type.array_get_size().prettyprint() << "), " 
            << "(" << lb << "), "
            << "(" << dim_sizes[actual_dim] << ")"
            << "}"
            ;
    }
}
