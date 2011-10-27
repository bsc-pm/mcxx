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
    TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER)++;

    std::stringstream ss;
    ss << "_ol_" << function_symbol.get_name() << "_" << outline_num;
    std::string outline_name = ss.str();
    

    Source device_descriptor, 
           device_description, 
           device_description_line, 
           num_devices,
           ancillary_device_description;
    device_descriptor << outline_name << "_devices";
    device_description
        << "static nanos_device_t " << device_descriptor << "[ " << num_devices << " ];"
        << "static void __register_" << device_descriptor << "(void *p __attribute__((unused)))"
        << "{"
        <<    ancillary_device_description
        <<    "nanos_device_t tmp[ " << num_devices << " ] = { " << device_description_line << "};"
        <<    "__builtin_memcpy(" << device_descriptor << ", tmp,"
        <<    "                 sizeof(" << device_descriptor << "));"
        << "}"
        << "__attribute__((section(\"nanos_post_init\"))) "
        << "nanos_init_desc_t __register_" << device_descriptor << "_list = { __register_" << device_descriptor << ", (void*)0 };"
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
            std::string implemented_outline_name = ss.str();

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

    Source dependency_array, num_dependences, dependency_struct, dependency_regions;

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
    regions_spawn(dependency_struct, dependency_array, dependency_regions, num_dependences, 
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

    Source tiedness, priority;
    PragmaCustomClause untied_clause = ctr.get_clause("untied");
    if (untied_clause.is_defined())
    {
        tiedness << "props.tied = 0;";
    }
    else
    {
        tiedness << "props.tied = 1;";
    }

    PragmaCustomClause priority_clause = ctr.get_clause("__priority");
    if (priority_clause.is_defined())
    {
        priority
            << "props.tied = " << priority_clause.get_arguments()[0] << ";"
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

    Source num_copies;

    ObjectList<OpenMP::CopyItem> copy_items = data_environ_info.get_copy_items();

    Source copy_data, copy_decl, copy_setup;
    Source copy_imm_data, copy_immediate_setup;

    Source set_translation_fun, translation_fun_arg_name;

    if (copy_items.empty())
    {
        num_copies << "0";
        // Non immediate
        copy_data << "(nanos_copy_data_t**)0";
        // Immediate
        copy_imm_data << "(nanos_copy_data_t*)0";

        if (Nanos::Version::interface_is_at_least("master", 5005))
        {
            C_LANGUAGE()
            {
                translation_fun_arg_name << ", (void*) 0"
                    ;
            }
            CXX_LANGUAGE()
            {
                translation_fun_arg_name << ", 0"
                    ;
            }
        }
    }
    else
    {
        num_copies << copy_items.size();

        // Non immediate
        copy_decl << "nanos_copy_data_t* copy_data = (nanos_copy_data_t*)0;";
        Source copy_items_src;
        copy_setup << copy_items_src;
        copy_data << "&copy_data";

        // Immediate
        copy_immediate_setup << "nanos_copy_data_t imm_copy_data[" << num_copies << "];";
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


        int i = 0;
        for (ObjectList<OpenMP::CopyItem>::iterator it = copy_items.begin();
                it != copy_items.end();
                it++)
        {
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
                const char* struct_access;
                const char* struct_addr;
            } fill_copy_data_info[] = {
                { &copy_items_src, "copy_data", "ol_args->", "ol_args" },
                { &copy_immediate_setup, "imm_copy_data", 
                    immediate_is_alloca ? "imm_args->" : "imm_args.", 
                    immediate_is_alloca ? "imm_args" : "&imm_args" },
                { NULL, "" },
            };

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

            i++;
        }

        if (_do_not_create_translation_fun)
        {
            if (Nanos::Version::interface_is_at_least("master", 5005))
            {
                C_LANGUAGE()
                {
                    translation_fun_arg_name << ", (void*) 0"
                        ;
                }
                CXX_LANGUAGE()
                {
                    translation_fun_arg_name << ", 0"
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
                        // Note this starting comma
                        << ", _xlate_copy_address_" << outline_num
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

    if ( current_targets.contains( "cuda" ) )
    {
        creation << "props.mandatory_creation = 1;"
            ;
    }

    Source alignment;
    if (Nanos::Version::interface_is_at_least("master", 5004))
    {
        alignment <<  "__alignof__(" << struct_arg_type_name << "),"
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
        // Global device
        AST_t device_tree = device_description.parse_declaration(ctr.get_ast().get_enclosing_function_definition_declaration().get_parent(), 
                ctr.get_scope_link());
        ctr.get_ast().prepend_sibling_function(device_tree);

        spawn_code
            << "{"
            <<     struct_arg_type_name << "* ol_args = (" << struct_arg_type_name << "*)0;"
            <<     struct_runtime_size
            <<     "nanos_wd_t wd = (nanos_wd_t)0;"
            <<     "nanos_wd_props_t props;"
            <<     "__builtin_memset(&props, 0, sizeof(props));"
            <<     creation
            <<     priority
            <<     tiedness
            <<     fill_real_time_info
            <<     copy_decl
            <<     "nanos_err_t err;"
            <<     if_expr_cond_start
            <<     "err = nanos_create_wd(&wd, " << num_devices << "," << device_descriptor << ","
            <<                 struct_size << ","
            <<                 alignment
            <<                 "(void**)&ol_args, nanos_current_wd(),"
            <<                 "&props, " << num_copies << ", " << copy_data << ");"
            <<     "if (err != NANOS_OK) nanos_handle_error (err);"
            <<     if_expr_cond_end
            <<     "if (wd != (nanos_wd_t)0)"
            <<     "{"
            <<        fill_outline_arguments
            <<        fill_dependences_outline
            <<        copy_setup
            <<        set_translation_fun
            <<        "err = nanos_submit(wd, " << num_dependences << ", (" << dependency_struct << "*)" 
            << dependency_array << ", (nanos_team_t)0);"
            <<        "if (err != NANOS_OK) nanos_handle_error (err);"
            <<     "}"
            <<     "else"
            <<     "{"
            <<        immediate_decl
            <<        fill_immediate_arguments
            <<        fill_dependences_immediate
            <<        copy_immediate_setup
            <<        "err = nanos_create_wd_and_run(" 
            <<                num_devices << ", " << device_descriptor << ", "
            <<                struct_size << ", " 
            <<                alignment
            <<                (immediate_is_alloca ? "imm_args" : "&imm_args") << ","
            <<                num_dependences << ", (" << dependency_struct << "*)" << dependency_array << ", &props,"
            <<                num_copies << "," << copy_imm_data 
            <<                translation_fun_arg_name << ");"
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
