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
#include "tl-counters.hpp"
#include "tl-parallel-common.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

static void fill_dimensions(int n_dims, int actual_dim, std::string* dim_sizes, Type dep_type, Source& dims_description, Scope sc);

Source TL::Nanox::common_parallel_code(
        PragmaCustomConstruct &ctr,
        const std::string& outline_name,
        const std::string& struct_arg_type_name,
        DataEnvironInfo& data_environ_info,
        AST_t parallel_code,
        const ObjectList<std::string>& current_targets)
{
    FunctionDefinition funct_def = ctr.get_enclosing_function();
    Symbol function_symbol = funct_def.get_function_symbol();
    
    ScopeLink sl = ctr.get_scope_link();
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

        outline_flags.parallel = true;

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
        alignment <<  "__alignof__(" << struct_arg_type_name << ")"
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
            xlate_arg << "(void*)0"
                ;
        }
        CXX_LANGUAGE()
        {
            xlate_arg << "0"
                ;
        }
    }

    Source num_threads;

    PragmaCustomClause num_threads_clause = ctr.get_clause("num_threads");
    if (num_threads_clause.is_defined())
    {
        num_threads << num_threads_clause.get_expression_list()[0];
    }
    else
    {
        num_threads << "nanos_omp_get_max_threads()";
    }

    Source if_code;
    PragmaCustomClause if_clause = ctr.get_clause("if");
    if (if_clause.is_defined())
    {
        ObjectList<Expression> expr_list = if_clause.get_expression_list();
        if (expr_list.size() != 1)
        {
            running_error("%s: error: clause 'if' requires just one argument\n",
                    ctr.get_ast().get_locus().c_str());
        }

        Expression &expr = expr_list[0];

        if_code
        << comment("A false 'if' clause evaluation means using current thread")
        << "_nanos_num_threads = (" << expr.prettyprint() << ") ? _nanos_num_threads : 1;";
    }

    Source data, imm_data, num_dependences, deps, nanos_create_wd, nanos_create_run_wd,
           properties_opt, decl_dyn_props_opt, modify_tie_to1, modify_tie_to2, device_description_opt,
           constant_code_opt, constant_struct_definition, constant_variable_declaration;

    num_dependences << "0";
    deps << "(nanos_dependence_t*)0";
    imm_data << (immediate_is_alloca ? "imm_args" : "&imm_args");
    data << "(void**)&ol_args";

    if ( Nanos::Version::interface_is_at_least("master", 5012))
    {
        nanos_create_wd = OMPTransform::get_nanos_create_wd_compact_code(struct_size, data, copy_data, Source("0"));

        nanos_create_run_wd = OMPTransform::get_nanos_create_and_run_wd_compact_code(
                struct_size, imm_data, num_dependences, deps, imm_copy_data, xlate_arg, Source("0"));

        decl_dyn_props_opt << "nanos_wd_dyn_props_t dyn_props = {0};";
        modify_tie_to1 << "dyn_props.tie_to = _nanos_threads[_i];";
        modify_tie_to2 << "dyn_props.tie_to = _nanos_threads[0];";

        constant_code_opt
            << constant_struct_definition
            <<  constant_variable_declaration
            ;

        constant_struct_definition
            << "struct nanos_const_wd_definition_local_t"
            << "{"
            <<      "nanos_const_wd_definition_t base;"
            <<      "nanos_device_t devices[" << num_devices << "];"
            << "};"
            ;

        constant_variable_declaration
            << "static struct nanos_const_wd_definition_local_t _const_def ="
            << "{"
            <<      "{"
            <<          "{"
            <<              "1, " /* mandatory_creation */
            <<              "0, " /* tied */
            <<              "0, " /* reserved0 */
            <<              "0, " /* reserved1 */
            <<              "0, " /* reserved2 */
            <<              "0, " /* reserved3 */
            <<              "0, " /* reserved4 */
            <<              "0, " /* reserved5 */
            ;
        if ( !Nanos::Version::interface_is_at_least("master", 5014))
        {
            constant_variable_declaration
                <<              "0, " /* priority */
                ;
        }
        constant_variable_declaration
            <<          "}, "
            <<          alignment   << ", "
            <<          num_copies  << ", "
            <<          num_devices << ", "
            <<      "},"
            <<      "{"
            <<          device_description_line
            <<      "}"
            << "};"
            ;
    }
    else
    {
        nanos_create_wd = OMPTransform::get_nanos_create_wd_code(num_devices,
                device_descriptor, struct_size, alignment, data, num_copies, copy_data);

        nanos_create_run_wd = OMPTransform::get_nanos_create_and_run_wd_code(num_devices, device_descriptor,
                struct_size, alignment, imm_data, num_dependences, deps, num_copies, imm_copy_data, xlate_arg);
        device_description_opt << device_description;

        properties_opt
            <<   "nanos_wd_props_t props = {0};"
            <<   "props.mandatory_creation = 1;";

        modify_tie_to1 << "props.tie_to = _nanos_threads[_i];";
        modify_tie_to2 << "props.tie_to = _nanos_threads[0];";
    }

    result
        << "{"
        <<   "unsigned int _nanos_num_threads = " << num_threads << ";"
        <<   if_code
        <<   "nanos_team_t _nanos_team = (nanos_team_t)0;"
        <<   "nanos_thread_t _nanos_threads[_nanos_num_threads];"
        <<   "nanos_err_t err;"
        <<   "err = nanos_create_team(&_nanos_team, (nanos_sched_t)0, &_nanos_num_threads,"
        <<              "(nanos_constraint_t*)0, /* reuse_current */ 1, _nanos_threads);"
        <<   "if (err != NANOS_OK) nanos_handle_error(err);"
        <<   ancillary_device_description
        <<   device_description_opt
        <<   struct_runtime_size
        <<   constant_code_opt
        <<   properties_opt
        <<   decl_dyn_props_opt
        <<   "unsigned _i;"
        <<   "for (_i = 1; _i < _nanos_num_threads; _i++)"
        <<   "{"
        //   We have to create a wd tied to a thread
        <<      struct_arg_type_name << " *ol_args = 0;"
        <<      modify_tie_to1
        <<      "nanos_wd_t wd = 0;"
        <<      nanos_create_wd
        <<      "if (err != NANOS_OK) nanos_handle_error(err);"
        <<      fill_outline_arguments
        <<      "err = nanos_submit(wd, 0, (nanos_dependence_t*)0, 0);"
        <<      "if (err != NANOS_OK) nanos_handle_error(err);"
        <<   "}"
        <<   modify_tie_to2
        <<   immediate_decl
        <<   fill_immediate_arguments
        <<   nanos_create_run_wd
        <<   "if (err != NANOS_OK) nanos_handle_error(err);"
        <<   "err = nanos_end_team(_nanos_team);"
        <<   "if (err != NANOS_OK) nanos_handle_error(err);"
        << "}"
        ;

    TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER)++;
    return result;
}


void TL::Nanox::regions_spawn(
        Source& dependency_struct, 
        Source& dependency_array, 
        Source& num_dependences,
        Source& fill_dependences_outline, 
        Source& fill_dependences_immediate,
        ObjectList<OpenMP::DependencyItem> dependences, 
        DataEnvironInfo data_environ_info,
        bool immediate_is_alloca, 
        PragmaCustomConstruct ctr, 
        bool is_task)
{
    if (Nanos::Version::interface_is_at_least("deps_api", 1000)) 
    {
        Source dependency_regions;
        Source immediate_dependency_regions;
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
                    << immediate_dependency_regions
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
                dependency_flags << "{";
                OpenMP::DependencyDirection attr = it->get_kind();

                if (is_task)
                {
                    // Set dependency_flags to "input,output,rename,"

                    if (((attr & OpenMP::DEP_CONCURRENT) == OpenMP::DEP_CONCURRENT) ||
                        ((attr & OpenMP::DEP_COMMUTATIVE) == OpenMP::DEP_COMMUTATIVE))
                    {
                        // Reduction and Commutative behave like inout, and cannot be renamed
                        dependency_flags << "1,1,0,";

                        if ((attr & OpenMP::DEP_COMMUTATIVE) == OpenMP::DEP_COMMUTATIVE)
                        {
                            if (!Nanos::Version::interface_is_at_least("deps_api", 1001))
                            {
                                fprintf(stderr,
                                        "%s: warning: the current version of Nanos does not"
                                        " support commutative dependencies in Superscalar\n",
                                        ctr.get_ast().get_locus().c_str());
                            }
                        }
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
                        // Can rename
                        dependency_flags << "1,";
                    }

                    // Add reduction flag to make dependency_flags "input,output,rename,concurrent"

                    if ((attr & OpenMP::DEP_CONCURRENT) == OpenMP::DEP_CONCURRENT)
                    {
                        dependency_flags << "1";
                    }
                    else
                    {
                        dependency_flags << "0";
                    }

                    // Add commutative flag to make dependency_flags "input,output,rename,concurrent,commutative"

                    if (Nanos::Version::interface_is_at_least("deps_api", 1001))
                    {
                        if ((attr & OpenMP::DEP_COMMUTATIVE) == OpenMP::DEP_COMMUTATIVE)
                        {
                            dependency_flags << ",1";
                        }
                        else
                        {
                            dependency_flags << ",0";
                        }
                    }
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
                Source dims_description, imm_dims_description;

                Type dependency_type = dependency_expression.get_type();
                int num_dimensions = dependency_type.get_num_dimensions();

                // Compute the base type of the dependency and the array containing the size of each dimension
                Type dependency_base_type = dependency_type;
                std::string dimension_sizes[num_dimensions + 1];
                for (int dim = 0; dim < num_dimensions; dim++)
                {
                    dimension_sizes[dim] = dependency_base_type.array_get_size().prettyprint();
                    dependency_base_type = dependency_base_type.array_element();
                }
                std::string base_type_name = dependency_base_type.get_declaration(data_ref.get_scope(), "");


                // Generate the spawn
                dependency_regions 
                    << "nanos_region_dimension_t dimensions" << num_dep << "[" << std::max(num_dimensions,1) << "] = {"
                    << dims_description << "};";

                immediate_dependency_regions 
                    << "nanos_region_dimension_t dimensions" << num_dep << "[" << std::max(num_dimensions,1) << "] = {"
                    << imm_dims_description << "};";

                if (num_dimensions == 0) 
                {
                    // A scalar
                    Source dependency_offset, imm_dependency_offset;
                    dims_description << "{" 
                        << "sizeof(" << base_type_name << "), " 
                        << dependency_offset << ", "
                        << "sizeof(" << base_type_name << ")" 
                        << "}"
                        ;
                    imm_dims_description << "{" 
                        << "sizeof(" << base_type_name << "), " 
                        << imm_dependency_offset << ", "
                        << "sizeof(" << base_type_name << ")" 
                        << "}"
                        ;

                    dependency_offset
                        << "((char*)(" << dependency_expression.get_address() << ") - " << "(char*)ol_args->" << dependency_field_name << ")"
                        ;

                    if (!immediate_is_alloca)
                    {
                        imm_dependency_offset
                            << "((char*)(" << dependency_expression.get_address() << ") - " << "(char*)imm_args." << dependency_field_name << ")"
                            ;
                    }
                    else
                    {
                        imm_dependency_offset
                            << "((char*)(" << dependency_expression.get_address() << ") - " << "(char*)imm_args->" << dependency_field_name << ")"
                            ;
                    }
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

                if (num_dimensions == 0)
                {
                    num_dimensions++;
                }

                Source dep_expr_addr = data_ref.get_address();
                dependency_offset
                    << "((char*)(" << dep_expr_addr << ") - " << "(char*)ol_args->" << dependency_field_name << ")"
                    ;

                dependency_defs_outline
                    << "{"
                    << "(void*)ol_args->" << dependency_field_name << ","
                    << dependency_flags << ","
                    << num_dimensions << ","
                    << "dimensions" << num_dep <<","
                    << dependency_offset  
                    << "}"
                    ;

                if (is_task)
                {
                    std::string access_operator = (!immediate_is_alloca) ? "." : "->";

                    imm_dependency_offset
                        << "((char*)(" << dep_expr_addr << ") - " 
                        << "(char*)imm_args" << access_operator << dependency_field_name << ")"
                        ;

                    dependency_defs_immediate
                        << "{"
                        << "(void*)imm_args"<< access_operator << dependency_field_name << ","
                        << dependency_flags << ","
                        << num_dimensions << ","
                        << "dimensions" << num_dep << ","
                        << imm_dependency_offset 
                        << "}"
                        ;
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
                dependency_flags << "{";
                OpenMP::DependencyDirection attr = it->get_kind();

                if (is_task)
                {
                    // Set dependency_flags to "input,output,rename"

                    if ((attr & OpenMP::DEP_CONCURRENT) == OpenMP::DEP_CONCURRENT)
                    {
                       // Reduction behaves like an inout, and cannot be renamed
                       dependency_flags << "1,1,0";
                       if (!Nanos::Version::interface_is_at_least("master", 5001))
                       {
                           fprintf(stderr,
                                   "%s: warning: the current version of Nanos does not"
                                   " support reduction dependencies in Superscalar\n",
                                   ctr.get_ast().get_locus().c_str());
                       }
                    }
                    else if ((attr & OpenMP::DEP_COMMUTATIVE) == OpenMP::DEP_COMMUTATIVE)
                    {
                       // Commutative behaves like an inout, and cannot be renamed
                       dependency_flags << "1,1,0";
                       if (!Nanos::Version::interface_is_at_least("deps_api", 1001))
                       {
                           fprintf(stderr,
                                   "%s: warning: the current version of Nanos does not"
                                   " support commutative dependencies in Superscalar\n",
                                   ctr.get_ast().get_locus().c_str());
                       }
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

                    if (Nanos::Version::interface_is_at_least("master", 5001))
                    {
                        // Add reduction flag to make dependency_flags "input,output,rename,reduction"

                        if ((attr & OpenMP::DEP_CONCURRENT) == OpenMP::DEP_CONCURRENT)
                        {
                            dependency_flags << ",1";
                        }
                        else
                        {
                            dependency_flags << ",0";
                        }

                        if (Nanos::Version::interface_is_at_least("deps_api", 1001))
                        {
                            // Add commutative flag to make dependency_flags "input,output,rename,reduction,commutative"
                            if ((attr & OpenMP::DEP_COMMUTATIVE) == OpenMP::DEP_COMMUTATIVE)
                            {
                                dependency_flags << ",1";
                            }
                            else
                            {
                                dependency_flags << ",0";
                            }
                        }
                    }
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
    if (actual_dim < (n_dims-1))
    {
        fill_dimensions(n_dims, actual_dim+1, dim_sizes, dep_type.array_element(), dims_description, sc);

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
}
