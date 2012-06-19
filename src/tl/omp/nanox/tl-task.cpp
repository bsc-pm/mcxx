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

#include "tl-omp-nanox.hpp"
#include "tl-data-env.hpp"
#include "tl-counters.hpp"
#include "tl-devices.hpp"
#include "tl-nanos.hpp"
#include "tl-parallel-common.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::task_postorder(PragmaCustomConstruct ctr)
{
    OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());
    
    //if the task is a function task rt_info must be changed
    OpenMP::RealTimeInfo rt_info = data_sharing.get_real_time_info();
         
    ObjectList<OpenMP::DependencyItem> dependences;
    data_sharing.get_all_dependences(dependences);

    DataEnvironInfo data_environ_info;
    compute_data_environment(data_sharing,
            ctr,
            data_environ_info,
            _converted_vlas);
    data_environ_info.set_local_copies(true);

    FunctionDefinition funct_def = ctr.get_enclosing_function();
    Symbol function_symbol = funct_def.get_function_symbol();

    std::string struct_arg_type_name;
    define_arguments_structure(ctr, struct_arg_type_name, data_environ_info, 
            dependences, Source());

    int outline_num = TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER);

    std::stringstream ss;
    ss << "_ol_" << function_symbol.get_name() << "_" << outline_num;
    std::string outline_name = fix_outline_name(ss.str());
    

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

    // Check for __symbol clause, and if found, get the task function symbol
    Symbol task_symbol = NULL;
    PragmaCustomClause function_clause = ctr.get_clause("__symbol");
    if (function_clause.is_defined())
    {
        ObjectList<Expression> expr_list = function_clause.get_expression_list();

        if (expr_list.size() != 1)
        {
                running_error("%s: internal error: clause '__symbol' requires just one argument\n",
                                ctr.get_ast().get_locus().c_str());
        }

        Expression &expr = expr_list[0];
        IdExpression id_expr = expr.get_id_expression();

        if (id_expr.get_computed_symbol().is_valid()) {
                task_symbol = id_expr.get_computed_symbol();
        }
    }

    OutlineFlags outline_flags;
    outline_flags.task_symbol = task_symbol;

    DeviceHandler &device_handler = DeviceHandler::get_device_handler();

    ObjectList<std::string> current_targets;
    data_sharing.get_all_devices(current_targets);
    for (ObjectList<std::string>::iterator it = current_targets.begin();
            it != current_targets.end();
            it++)
    {
        DeviceProvider* device_provider = device_handler.get_device(*it);

        if (device_provider == NULL)
        {
            internal_error("invalid device '%s' at '%s'\n",
                    it->c_str(), ctr.get_ast().get_locus().c_str());
        }

        Source initial_setup, replaced_body;

        device_provider->do_replacements(data_environ_info,
                ctr.get_statement().get_ast(),
                ctr.get_scope_link(),
                initial_setup,
                replaced_body);
        
        device_provider->create_outline(outline_name,
                struct_arg_type_name,
                data_environ_info,
                outline_flags,
                ctr.get_statement().get_ast(),
                ctr.get_scope_link(),
                initial_setup,
                replaced_body);

        device_provider->get_device_descriptor(outline_name, 
                data_environ_info, 
                outline_flags,
                ctr.get_statement().get_ast(),
                ctr.get_scope_link(),
                ancillary_device_description, 
                device_description_line);
    }

    int total_devices = current_targets.size();

    // If this is a function coming from a task try to get its devices with an
    // implementation already given
    if (function_clause.is_defined())
    {
        if (!task_symbol.is_function())
        {
            internal_error("%s: invalid symbol for __function clause\n",
                    ctr.get_ast().get_locus().c_str());
        }

        if (!function_task_set->is_function_task(task_symbol))
        {
            internal_error("%s: __function clause function is not task function\n",
                    ctr.get_ast().get_locus().c_str());
        }

        OpenMP::FunctionTaskInfo& function_task_info 
            = function_task_set->get_function_task(task_symbol);
        
        //sets the right value to rt_info 
        rt_info = function_task_info.get_real_time_info();

        ObjectList<OpenMP::FunctionTaskInfo::implementation_pair_t> implementation_list 
            = function_task_info.get_devices_with_implementation();

        total_devices += implementation_list.size();

        for (ObjectList<OpenMP::FunctionTaskInfo::implementation_pair_t>::iterator it = implementation_list.begin();
                it != implementation_list.end();
                it++)
        {
            DeviceProvider* device_provider = device_handler.get_device(it->first);

            if (device_provider == NULL)
            {
                internal_error("invalid device '%s' at '%s'\n",
                        it->first.c_str(), ctr.get_ast().get_locus().c_str());
            }

            Source replaced_call;
            ReplaceSrcIdExpression replace_call(ctr.get_scope_link());
            replace_call.add_replacement(task_symbol, it->second.get_qualified_name());
            replaced_call << replace_call.replace(ctr.get_statement());

            AST_t new_code = replaced_call.parse_statement(ctr.get_ast(), ctr.get_scope_link());

            std::stringstream ss;
            ss << "_ol_" << it->second.get_name() << "_" << outline_num;
            std::string implemented_outline_name = fix_outline_name(ss.str());

            Source initial_setup, replaced_body;

            device_provider->do_replacements(data_environ_info,
                            new_code,
                            ctr.get_scope_link(),
                            initial_setup,
                            replaced_body);

            OutlineFlags implemented_outline_flags;
            implemented_outline_flags.task_symbol = it->second;

            device_provider->create_outline(implemented_outline_name,
                            struct_arg_type_name,
                            data_environ_info,
                            implemented_outline_flags,
                            ctr.get_ast(),
                            ctr.get_scope_link(),
                            initial_setup,
                            replaced_body);

            device_provider->get_device_descriptor(
                    implemented_outline_name,
                    data_environ_info, 
                    implemented_outline_flags,
                    ctr.get_statement().get_ast(),
                    ctr.get_scope_link(),
                    ancillary_device_description, 
                    device_description_line);
        }
    }

    num_devices << total_devices;

    Source spawn_code;
    Source fill_outline_arguments, fill_immediate_arguments, 
           fill_dependences_outline,
           fill_dependences_immediate;

    Source dependency_array, num_dependences, dependency_struct;

    bool immediate_is_alloca = false;
    bool env_is_runtime_sized = data_environ_info.environment_is_runtime_sized();

    if (env_is_runtime_sized)
    {
        immediate_is_alloca = true;
    }
    if(function_symbol.is_member() && !function_symbol.is_static()) 
    {
            fill_outline_arguments << "ol_args->_this = this;";
            fill_immediate_arguments  << "imm_args" << (immediate_is_alloca ? "->" : ".") << "_this = this;";
    }
    fill_data_args("ol_args", 
            data_environ_info, 
            dependences, 
            /* is_pointer */ true,
            fill_outline_arguments);

    fill_data_args(
            "imm_args",
            data_environ_info, 
            dependences, 
            /* is_pointer */ immediate_is_alloca,
            fill_immediate_arguments);

    // Fill dependences, if any    
    regions_spawn(dependency_struct, dependency_array, num_dependences, 
                  fill_dependences_outline, fill_dependences_immediate, dependences, data_environ_info, 
                  immediate_is_alloca, ctr, /*is task*/ true);
    
    // Honour if clause
    Source if_expr_cond_start, if_expr_cond_end;
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

        if_expr_cond_start
            << "if (" << expr << ")"
            << "{"
            ;

        if_expr_cond_end << "}";
    }

    bool props_tied;
    Source tiedness, priority;
    PragmaCustomClause untied_clause = ctr.get_clause("untied");
    if (untied_clause.is_defined())
    {
        props_tied = 0;
        tiedness << "props.tied = 0;";
    }
    else
    {
        props_tied = 1;
        tiedness << "props.tied = 1;";
    }

    PragmaCustomClause priority_clause = ctr.get_clause("priority");
    std::string props_priority = "0";
    if (priority_clause.is_defined())
    {
        props_priority = priority_clause.get_arguments()[0];
        priority
            << "props.priority = " << props_priority << ";"
            ;
    }

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

    if (env_is_runtime_sized)
    {
        struct_runtime_size
            << "int struct_runtime_size = "
            << "sizeof(" << struct_arg_type_name << ") + "
            << "(" << data_environ_info.sizeof_variable_part(ctr.get_scope()) << ")"
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

    Source num_copies, num_copies_dimensions;

    ObjectList<OpenMP::CopyItem> copy_items = data_environ_info.get_copy_items();

    Source copy_data, copy_decl, copy_setup;
    Source copy_imm_data, copy_immediate_setup;

    Source set_translation_fun, translation_fun_arg_name;

    if (copy_items.empty())
    {
        num_copies << "0";
        num_copies_dimensions << "0";
        // Non immediate
        copy_data << "(nanos_copy_data_t**)0";
        // Immediate
        copy_imm_data << "(nanos_copy_data_t*)0";

        if (Nanos::Version::interface_is_at_least("master", 5005))
        {
            C_LANGUAGE()
            {
                translation_fun_arg_name << "(void*) 0"
                    ;
            }
            CXX_LANGUAGE()
            {
                translation_fun_arg_name << "0"
                    ;
            }
        }
    }
    else
    {
        num_copies << copy_items.size();

        // Non immediate
        copy_decl << "nanos_copy_data_t* copy_data = (nanos_copy_data_t*)0;"
            ;
        if (Nanos::Version::interface_is_at_least("copies_api", 1000))
        {
            copy_decl << "nanos_region_dimension_internal_t* nanos_copies_region_buffer = (nanos_region_dimension_internal_t*)0;"
                ;
        }
        Source copy_items_src;
        copy_setup << copy_items_src;
        copy_data << "&copy_data";

        // Immediate
        copy_immediate_setup << "nanos_copy_data_t imm_copy_data[" << num_copies << "];";
        if (Nanos::Version::interface_is_at_least("copies_api", 1000))
        {
            copy_immediate_setup << "nanos_region_dimension_internal_t nanos_copies_imm_region_buffer[" << num_copies_dimensions << "];";
                ;
        }
        copy_imm_data << "imm_copy_data";

        Source wd_param, wd_arg;
        if (Nanos::Version::interface_is_at_least("master", 5005))
        {
            wd_param
                << ", nanos_wd_t wd"
                ;
            wd_arg
                << ", wd"
                ;
        }

        Source translation_function, translation_statements;
        translation_function
            << "static void _xlate_copy_address_" << outline_num << "(void* data" << wd_param <<")"
            << "{"
            <<   "nanos_err_t cp_err;"
            <<   struct_arg_type_name << "* _args = (" << struct_arg_type_name << "*)data;"
            <<   translation_statements
            << "}"
            ;

        // We need to create a replacement here
        Source vla_adjustments;
        ReplaceSrcIdExpression replacement_xlate(ctr.get_scope_link());
        replacement_xlate.add_this_replacement("_args->_this");
        ObjectList<DataEnvironItem> data_items = data_environ_info.get_items();
        for (ObjectList<DataEnvironItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            DataEnvironItem& data_env_item(*it);
            if (data_env_item.is_vla_type())
            {
                ObjectList<Source> vla_dims = data_env_item.get_vla_dimensions();

                ObjectList<Source> arg_vla_dims;
                for (ObjectList<Source>::iterator it = vla_dims.begin();
                        it != vla_dims.end();
                        it++)
                {
                    Source new_dim;
                    new_dim << "_args->" << *it;

                    arg_vla_dims.append(new_dim);
                }

                Type type = compute_replacement_type_for_vla(data_env_item.get_symbol().get_type(),
                        arg_vla_dims.begin(), arg_vla_dims.end());

                vla_adjustments 
                    << type.get_declaration(ctr.get_scope(), data_env_item.get_field_name())
                    << "= (" << type.get_declaration(ctr.get_scope(), "") << ")"
                    << "(_args->" << data_env_item.get_field_name() << ")"
                    << ";"
                    ;
                replacement_xlate.add_replacement(data_env_item.get_symbol(), data_env_item.get_field_name() );
            }
            else
            {
                replacement_xlate.add_replacement(data_env_item.get_symbol(), "(_args-> " + data_env_item.get_field_name() + ")" );
            }
        }

        // Number of total dimensions
        int sum_copies_dim = 0;
        for (ObjectList<OpenMP::CopyItem>::iterator it = copy_items.begin();
                it != copy_items.end();
                it++)
        {
            DataReference copy_expr = it->get_copy_expression();
            Type copy_type = copy_expr.get_type();
            sum_copies_dim += std::max(copy_type.get_num_dimensions(), 1);
        }
        num_copies_dimensions << sum_copies_dim;

        int region_buffer_index = 0;
        int i = 0;
        for (ObjectList<OpenMP::CopyItem>::iterator it = copy_items.begin();
                it != copy_items.end();
                it++, i++)
        {
            int outline_region_buffer_index = region_buffer_index;
            int immediate_region_buffer_index = region_buffer_index;

            OpenMP::CopyItem& copy_item(*it);
            DataReference copy_expr = copy_item.get_copy_expression();

            DataEnvironItem data_env_item = data_environ_info.get_data_of_symbol(copy_expr.get_base_symbol());

            Source copy_direction_in, copy_direction_out;

            if (copy_item.get_kind() == OpenMP::COPY_DIR_IN)
            {
                copy_direction_in << 1;
                copy_direction_out << 0;
            }
            else if (copy_item.get_kind() == OpenMP::COPY_DIR_OUT)
            {
                copy_direction_in << 0;
                copy_direction_out << 1;
            }
            else if (copy_item.get_kind() == OpenMP::COPY_DIR_INOUT)
            {
                copy_direction_in << 1;
                copy_direction_out << 1;
            }

            DataReference data_ref = copy_item.get_copy_expression();

            Source replaced_address;

            if (!data_ref.is_id_expression())
            {
                AST_t base_addr = data_ref.get_address().parse_expression(data_ref.get_ast(), data_ref.get_scope_link());
                replaced_address = replacement_xlate.replace(base_addr);
            }
            else
            {
                // &_args->x is not what we want
                replaced_address = Source() << "_args->" << data_env_item.get_field_name();
            }

            OpenMP::DataSharingAttribute data_attr = data_sharing.get_data_sharing(data_ref.get_base_symbol());

            ERROR_CONDITION(data_attr == OpenMP::DS_UNDEFINED, "Invalid data sharing for copy", 0);

            // There used to be NANOS_PRIVATE but nobody knows what it meant
            Source copy_sharing;
            copy_sharing << "NANOS_SHARED";


            translation_statements
                << "{"
                // This should be a proper type
                << vla_adjustments
                << "signed long offset = "
                << "((char*)(" << replaced_address << ") - " << "((char*)_args->" << data_env_item.get_field_name() << "));"
                << "char * addr = (char*)" << replaced_address << ";"
                << "cp_err = nanos_get_addr(" << i << ", (void**)&addr " << wd_arg << ");"
                << "if (cp_err != NANOS_OK) nanos_handle_error(cp_err);"
                << "_args->" << data_env_item.get_field_name() << " = "
                << "(" << data_env_item.get_type().get_declaration(ctr.get_scope(), "") << ")"
                << "(addr - offset);"
                << "}"
                ;

            struct {
                Source *source;
                const char* array;
                const char* region_buffer;
                const char* struct_access;
                const char* struct_addr;
                int *counter;
            } fill_copy_data_info[] = {
                { &copy_items_src, "copy_data", "nanos_copies_region_buffer", "ol_args->", "ol_args", &outline_region_buffer_index },
                { &copy_immediate_setup, "imm_copy_data", "nanos_copies_imm_region_buffer",
                    immediate_is_alloca ? "imm_args->" : "imm_args.",
                    immediate_is_alloca ? "imm_args" : "&imm_args", 
                    &immediate_region_buffer_index },
                { NULL, "" },
            };

            if (Nanos::Version::interface_is_at_least("copies_api", 1000))
            {
                for (int j = 0; fill_copy_data_info[j].source != NULL; j++)
                {
                    int& current_region_buffer_index = *(fill_copy_data_info[j].counter);
                    Source expression_size, expression_address;
                    const char* array_name = fill_copy_data_info[j].array;
                    const char* region_buffer = fill_copy_data_info[j].region_buffer;
                    // 101 typedef struct {
                    // 102    void *address;
                    // 103    nanos_sharing_t sharing;
                    // 104    struct {
                    // 105       bool input: 1;
                    // 106       bool output: 1;
                    // 107    } flags;
                    // 108    short dimension_count;
                    // 109    nanos_region_dimension_internal_t const *dimensions;
                    // 110    ptrdiff_t offset;  <<<<<-- ???
                    // 111 } nanos_copy_data_internal_t;
                    Type copy_type = copy_expr.get_type();
                    int num_dimensions = copy_type.get_num_dimensions();
                    Type copy_base_type = copy_type;

                    TL::ObjectList<std::string> dimension_sizes;
                    TL::ObjectList<TL::Type> dimension_types;
                    for (int dim = 0; dim < num_dimensions; dim++)
                    {
                        dimension_sizes.append(copy_base_type.array_get_size().prettyprint());
                        dimension_types.append(copy_base_type);
                        copy_base_type = copy_base_type.array_element();
                    }

                    Source base_address;

                    TL::Symbol base_symbol = copy_expr.get_base_symbol();
                    if (num_dimensions != 0)
                    {
                        base_address << base_symbol.get_name();
                    }
                    else // scalar
                    {
                        base_address << "&" << base_symbol.get_name();
                    }

                    std::string base_type_name = copy_base_type.get_declaration(data_ref.get_scope(), "");

                    Source &target_source (*(fill_copy_data_info[j].source));
                    target_source
                        << array_name << "[" << i << "].address = (void*)(" << base_address << ");"
                        << array_name << "[" << i << "].sharing = " << copy_sharing << ";"
                        << array_name << "[" << i << "].flags.input = " << copy_direction_in << ";"
                        << array_name << "[" << i << "].flags.output = " << copy_direction_out << ";"
                        << array_name << "[" << i << "].dimension_count = " << num_dimensions << ";"
                        ;

                    // 32 typedef struct {
                    // 33    /* NOTE: The first dimension is represented in terms of bytes. */
                    // 34
                    // 35    /* Size of the dimension in terms of the size of the previous dimension. */
                    // 36    size_t size;
                    // 37
                    // 38    /* Lower bound in terms of the size of the previous dimension. */
                    // 39    size_t lower_bound;
                    // 40
                    // 41    /* Accessed length in terms of the size of the previous dimension. */
                    // 42    size_t accessed_length;
                    // 43 } nanos_region_dimension_internal_t;

                    int region_buffer_start_index = current_region_buffer_index;
                    if (num_dimensions == 0)
                    {
                        // A scalar
                        target_source
                            << region_buffer << "[" << current_region_buffer_index << "].size = sizeof(" << base_type_name << ");"
                            << region_buffer << "[" << current_region_buffer_index << "].lower_bound = 0;"
                            << region_buffer << "[" << current_region_buffer_index << "].accessed_length = sizeof(" << base_type_name << ");"
                            ;
                        current_region_buffer_index++;
                    }
                    else
                    {
                        for (int k = num_dimensions - 1; k >= 0; k--)
                        {
                            Type &current_type = dimension_types[k];
                            // First dimension in bytes
                            if (k == num_dimensions - 1)
                            {
                                if (current_type.array_is_region())
                                {
                                    AST_t lb, ub, size;
                                    current_type.array_get_region_bounds(lb, ub);
                                    size = current_type.array_get_region_size();

                                    target_source
                                        << region_buffer << "[" << current_region_buffer_index << "].size = "
                                        << "sizeof(" << base_type_name << ") * (" << dimension_sizes[k] << ");"
                                        << region_buffer << "[" << current_region_buffer_index << "].lower_bound = "
                                        << "sizeof(" << base_type_name << ") * (" << lb.prettyprint() << ");"
                                        << region_buffer << "[" << current_region_buffer_index << "].accessed_length = "
                                        << "sizeof(" << base_type_name << ") * (" << size.prettyprint() << ");"
                                        ;
                                    current_region_buffer_index++;
                                }
                                else
                                {
                                    std::string lb = "0";

                                    target_source
                                        << region_buffer << "[" << current_region_buffer_index << "].size = " 
                                        << "sizeof(" << base_type_name << ") * (" << dimension_sizes[k] << ");"
                                        << region_buffer << "[" << current_region_buffer_index << "].lower_bound = "
                                        << lb << ";"
                                        << region_buffer << "[" << current_region_buffer_index << "].accessed_length = "
                                        << "sizeof(" << base_type_name << ") * (" << dimension_sizes[k] << ");"
                                        ;
                                    current_region_buffer_index++;
                                }
                            }
                            else
                            {
                                if (current_type.array_is_region())
                                {
                                    AST_t lb, ub, size;
                                    current_type.array_get_region_bounds(lb, ub);
                                    size = current_type.array_get_region_size();

                                    target_source
                                        << region_buffer << "[" << current_region_buffer_index << "].size = "
                                        << "(" << dimension_sizes[k] << ");"
                                        << region_buffer << "[" << current_region_buffer_index << "].lower_bound = "
                                        << "(" << lb.prettyprint() << ");"
                                        << region_buffer << "[" << current_region_buffer_index << "].accessed_length = "
                                        << "(" << size.prettyprint() << ");"
                                        ;
                                    current_region_buffer_index++;
                                }
                                else
                                {
                                    std::string lb = 0;

                                    target_source
                                        << region_buffer << "[" << current_region_buffer_index << "].size = "
                                        << "(" << current_type.array_get_size().prettyprint() << ");"
                                        << region_buffer << "[" << current_region_buffer_index << "].lower_bound = "
                                        << "(" << lb << ");"
                                        << region_buffer << "[" << current_region_buffer_index << "].accessed_length = "
                                        << "(" << dimension_sizes[k] << ");"
                                        ;
                                    current_region_buffer_index++;
                                }
                            }
                        }
                    }

                    target_source
                        << array_name << "[" << i << "].dimensions = &" << region_buffer << "[" << region_buffer_start_index << "];"
                        << array_name << "[" << i << "].offset = (ptrdiff_t)("
                        << "(char*)" << copy_expr.get_address() << " - (char*)" << base_address << ");"
                        ;

                    expression_address << copy_expr.get_address();
                }

                // Update the outline region buffer index 
                region_buffer_index = outline_region_buffer_index;
            }
            else
            {
                for (int j = 0; fill_copy_data_info[j].source != NULL; j++)
                {
                    Source expression_size, expression_address;
                    const char* array_name = fill_copy_data_info[j].array;
                    (*(fill_copy_data_info[j].source))
                        << array_name << "[" << i << "].address = (uintptr_t)(" << expression_address << ");"
                        << array_name << "[" << i << "].sharing = " << copy_sharing << ";"
                        << array_name << "[" << i << "].flags.input = " << copy_direction_in << ";"
                        << array_name << "[" << i << "].flags.output = " << copy_direction_out << ";"
                        << array_name << "[" << i << "].size = " << expression_size << ";"
                        ;

                    expression_address << copy_expr.get_address();
                    expression_size << copy_expr.get_sizeof();
                }
            }
        }

        if (_do_not_create_translation_fun)
        {
            if (Nanos::Version::interface_is_at_least("master", 5005))
            {
                C_LANGUAGE()
                {
                    translation_fun_arg_name << "(void*) 0"
                        ;
                }
                CXX_LANGUAGE()
                {
                    translation_fun_arg_name << "0"
                        ;
                }
            }
        }
        else
        {
            if (Nanos::Version::interface_is_at_least("master", 5003))
            {
                Source translation_fun_name;
                translation_fun_name << "_xlate_copy_address_" << outline_num
                    ;
                // FIXME - Templates
                set_translation_fun
                    << "nanos_set_translate_function(wd, " << translation_fun_name << ");"
                    ;

                if (Nanos::Version::interface_is_at_least("master", 5005))
                {
                    translation_fun_arg_name
                        << "_xlate_copy_address_" << outline_num
                        ;
                }

                AST_t xlate_function_def = translation_function.parse_declaration(
                        ctr.get_ast().get_enclosing_function_definition_declaration().get_parent(),
                        ctr.get_scope_link());

                ctr.get_ast().prepend_sibling_function(xlate_function_def);
            }
        }
    }

    // Disallow GPU tasks to be executed at the time they are created
    // TODO: Implement the corresponding part in the runtime in order to allow create_wd_and_run
    // function work properly
    Source creation;
    bool props_mandatory_creation = 0;
    if ( current_targets.contains( "cuda" ) )
    {
        creation << "props.mandatory_creation = 1;"
            ;
        props_mandatory_creation = 1;
    }

    Source alignment;
    if (Nanos::Version::interface_is_at_least("master", 5004))
    {
        alignment <<  "__alignof__(" << struct_arg_type_name << ")"
            ;
    }

    Source fill_real_time_info;
    if(Nanos::Version::interface_is_at_least("realtime",1000))
    {
        Source release_after, deadline, onerror;
        fill_real_time_info
            << deadline
            << release_after
            << onerror
            ;
        //Adds release time information
        if(rt_info.has_release_time())
        {
            release_after << "props._release_after = "
                          << rt_info.get_time_release().prettyprint() << ";";
        }
        else
        {
            release_after << "props._release_after = -1;";
        }

        //Adds deadline time information
        if(rt_info.has_deadline_time())
        {
            deadline  << "props._deadline_time = "
                      << rt_info.get_time_deadline().prettyprint() << ";";
        }
        else
        {
            deadline << "props._deadline_time = -1;";
        }

        //Adds action error information
        //looking for the event 'OMP_DEADLINE_EXPIRED'
        std::string action =
            rt_info.get_action_error(OpenMP::RealTimeInfo::OMP_DEADLINE_EXPIRED);

        if(action != "")
        {
            onerror  << "props._onerror_action = " << action << ";";
        }
        else
        {
            //looking for the event 'OMP_ANY_EVENT'
            action = rt_info.get_action_error(OpenMP::RealTimeInfo::OMP_ANY_EVENT);
            if(action != "")
            {
                onerror  << "props._onerror_action = " << action << ";";
            }
            else
            {
                onerror  << "props._onerror_action = OMP_NO_ACTION;";
            }
        }
    }

    if(!_no_nanox_calls)
    {
        Source data, imm_data, deps, nanos_create_wd, nanos_create_run_wd,
               properties_opt, device_description_opt, decl_dyn_props_opt,
               constant_code_opt, constant_struct_definition, constant_variable_declaration;

        data << "(void**)&ol_args";
        imm_data << (immediate_is_alloca ? "imm_args" : "&imm_args");
        deps << "(" << dependency_struct << "*)" << dependency_array;
        
        if (Nanos::Version::interface_is_at_least("master", 5012))
        {
            nanos_create_wd = OMPTransform::get_nanos_create_wd_compact_code(struct_size, data, copy_data,
                    props_priority);

            nanos_create_run_wd = OMPTransform::get_nanos_create_and_run_wd_compact_code(
                    struct_size, imm_data, num_dependences, deps, copy_imm_data, translation_fun_arg_name,
                    props_priority);
            
            decl_dyn_props_opt << "nanos_wd_dyn_props_t dyn_props = {0};";
            
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
                <<              props_mandatory_creation << ", "
                <<              props_tied  << ", "
                <<              "0, " /* reserved0 */
                <<              "0, " /* reserved1 */
                <<              "0, " /* reserved2 */
                <<              "0, " /* reserved3 */
                <<              "0, " /* reserved4 */
                <<              "0, " /* reserved5 */
                ;

            if (!Nanos::Version::interface_is_at_least("master", 5014))
            {
                // After 5014, props is a dynamic property
                constant_variable_declaration
                    <<              props_priority
                    ;
            }
            constant_variable_declaration
                <<          "}, "
                <<          alignment   << ", "
                <<          num_copies  << ", "
                <<          num_devices << ", "
                ;

            if (Nanos::Version::interface_is_at_least("copies_api", 1000))
            {
                constant_variable_declaration 
                    << num_copies_dimensions << ","
                    ;
            }

            constant_variable_declaration
                <<      "},"
                <<      "{"
                <<          device_description_line
                <<      "}"
                << "};"
                ;
        }
        else
        {
            properties_opt
                << "nanos_wd_props_t props;"
                << "__builtin_memset(&props, 0, sizeof(props));"
                << creation
                << priority
                << tiedness
                << fill_real_time_info;

            device_description_opt
                << device_description;

            nanos_create_wd = OMPTransform::get_nanos_create_wd_code(num_devices,
                    device_descriptor, struct_size, alignment, data, num_copies, copy_data);

            nanos_create_run_wd = OMPTransform::get_nanos_create_and_run_wd_code(num_devices, device_descriptor,
                    struct_size, alignment, imm_data, num_dependences, deps, num_copies, copy_imm_data, translation_fun_arg_name);
        }

        spawn_code
            << "{"
            <<     ancillary_device_description
            <<     device_description_opt
            <<     struct_arg_type_name << "* ol_args = (" << struct_arg_type_name << "*)0;"
            <<     struct_runtime_size
            <<     "nanos_wd_t wd = (nanos_wd_t)0;"
            <<     constant_code_opt
            <<     properties_opt
            <<     decl_dyn_props_opt
            <<     copy_decl
            <<     "nanos_err_t err;"
            <<     if_expr_cond_start
            <<     nanos_create_wd
            <<     "if (err != NANOS_OK) nanos_handle_error (err);"
            <<     if_expr_cond_end
            <<     "if (wd != (nanos_wd_t)0)"
            <<     "{"
            <<        fill_outline_arguments
            <<        fill_dependences_outline
            <<        copy_setup
            <<        set_translation_fun
            <<        "err = nanos_submit(wd, " << num_dependences << ", (" << dependency_struct << "*)"
            <<                  dependency_array << ", (nanos_team_t)0);"
            <<        "if (err != NANOS_OK) nanos_handle_error (err);"
            <<     "}"
            <<     "else"
            <<     "{"
            <<        immediate_decl
            <<        fill_immediate_arguments
            <<        fill_dependences_immediate
            <<        copy_immediate_setup
            <<        nanos_create_run_wd
            <<        "if (err != NANOS_OK) nanos_handle_error (err);"
            <<     "}"
            << "}"
            ;
    }
    else
    {
        if(current_targets.contains("smp"))
        {
            std::stringstream smp_device_call;
            smp_device_call << "_smp_" << outline_name << "(&imm_args);";

            // The code generated must not contain calls to runtime. The execution will be serial
            spawn_code
                << "{"
                <<     immediate_decl
                <<     fill_immediate_arguments
                <<     smp_device_call.str()
                << "}"
                ;
        }
        else
        {
            running_error("%s: error: the code generation without calls to runtime only works in smp devices\n",
                    ctr.get_ast().get_locus().c_str());
        }
    }

    AST_t spawn_tree = spawn_code.parse_statement(ctr.get_ast(), ctr.get_scope_link());
    ctr.get_ast().replace(spawn_tree);
    
    TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER)++;
}

static void fix_dependency_expression_rec(Source &src, Expression expr, bool top_level, bool get_addr)
{
    if (expr.is_id_expression())
    {
        src << expr.prettyprint();
    }
    else if (expr.is_array_subscript())
    {
        fix_dependency_expression_rec(src, expr.get_subscripted_expression(), /* top_level */ false, /* get_addr */ true);

        src << "[" << expr.get_subscript_expression() << "]";
    }
    else if (expr.is_array_section_range()
            || expr.is_array_section_size())
    {
        fix_dependency_expression_rec(src, expr.array_section_item(), /* top_level */ false, /* get_addr */ true);

        src << "[" << expr.array_section_lower() << "]";
    }
    else if (expr.is_shaping_expression())
    {
        Type cast_type = expr.get_type();
        cast_type = cast_type.array_element().get_pointer_to();

        if (!top_level)
        {
            if (get_addr)
            {
                src <<"((" << cast_type.get_declaration(expr.get_scope(), "") << ")";
                fix_dependency_expression_rec(src, expr.shaped_expression(), /* top_level */ false, /* get_addr */ true);
                src << ")";
            }
            else
            {
                src <<"(*(" << cast_type.get_declaration(expr.get_scope(), "") << ")";
                fix_dependency_expression_rec(src, expr.shaped_expression(), /* top_level */ false, /* get_addr */ true);
                src << ")";
            }
        }
        else
        {
            fix_dependency_expression_rec(src, expr.shaped_expression(), /* top_level */ false, /* get_addr */ false);
        }
    }
}
