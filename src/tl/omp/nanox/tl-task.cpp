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

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::task_postorder(PragmaCustomConstruct ctr)
{
    OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());

    ObjectList<OpenMP::DependencyItem> dependences;
    data_sharing.get_all_dependences(dependences);

    DataEnvironInfo data_environ_info;
    compute_data_environment(data_sharing,
            ctr.get_scope_link(),
            data_environ_info,
            _converted_vlas);

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
        << ancillary_device_description
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

    bool some_device_needs_copies = false;

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

        some_device_needs_copies = some_device_needs_copies
            || device_provider->needs_copies();
    }

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
        ObjectList<OpenMP::FunctionTaskInfo::implementation_pair_t> implementation_list 
            = function_task_info.get_devices_with_implementation();

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

            some_device_needs_copies = some_device_needs_copies
                    || device_provider->needs_copies();
        }
    }

    num_devices << current_targets.size();

    Source spawn_code;
    Source fill_outline_arguments, fill_immediate_arguments, 
           fill_dependences_outline,
           fill_dependences_immediate;

    Source dependency_array, num_dependences;

    fill_data_args("ol_args", 
            data_environ_info, 
            dependences, 
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
            dependences, 
            /* is_pointer */ immediate_is_alloca,
            fill_immediate_arguments);

    // Fill dependences, if any
    if (!dependences.empty())
    {
        num_dependences << dependences.size();
        Source dependency_defs_outline, dependency_defs_immediate;
        fill_dependences_outline
            << "nanos_dependence_t _dependences[" << num_dependences << "] = {"
            << dependency_defs_outline
            << "};"
            ;

        fill_dependences_immediate
            << "nanos_dependence_t _dependences[" << num_dependences << "] = {"
            << dependency_defs_immediate
            << "};"
            ;

        dependency_array << "_dependences";

        int num_dep = 0;
        for (ObjectList<OpenMP::DependencyItem>::iterator it = dependences.begin();
                it != dependences.end();
                it++)
        {
            Source dependency_flags;
            Source reduction_flag;
            dependency_flags << "{";
            OpenMP::DependencyDirection attr = it->get_kind();
            if (!((attr & OpenMP::DEP_REDUCTION) == OpenMP::DEP_REDUCTION))
            {
                reduction_flag << "0,";
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
                // Reduction behaves like an inout
                reduction_flag << "1,";
                dependency_flags << "1, 1,";
            }

            if (!Nanos::Version::interface_is_at_least("master", 5001))
            {
                fprintf(stderr,
                        "%s: warning: the current version of Nanos does not"
                        " support reduction dependencies in Superscalar\n",
                        ctr.get_ast().get_locus().c_str());
            }
            else
            {
                dependency_flags << reduction_flag;
            }

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

            if ((attr & OpenMP::DEP_REDUCTION) == OpenMP::DEP_REDUCTION)
            {
                // Reductions cannot be renamed
                dependency_flags << "0"
                    ;
            }
            else
            {
                // Can rename otherwise
                dependency_flags << "1"
                    ;
            }

            dependency_flags << "}"
                    ;

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

    if (copy_items.empty()
            || !some_device_needs_copies)
    {
        num_copies << "0";
        // Non immediate
        copy_data << "(nanos_copy_data_t**)0";
        // Immediate
        copy_imm_data << "(nanos_copy_data_t*)0";
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

        int i = 0;
        for (ObjectList<OpenMP::CopyItem>::iterator it = copy_items.begin();
                it != copy_items.end();
                it++)
        {
            Source copy_direction_in, copy_direction_out;

            if (it->get_kind() == OpenMP::COPY_DIR_IN)
            {
                copy_direction_in << 1;
                copy_direction_out << 0;
            }
            else if (it->get_kind() == OpenMP::COPY_DIR_OUT)
            {
                copy_direction_in << 0;
                copy_direction_out << 1;
            }
            else if (it->get_kind() == OpenMP::COPY_DIR_INOUT)
            {
                copy_direction_in << 1;
                copy_direction_out << 1;
            }

            DataReference data_ref = it->get_copy_expression();
            OpenMP::DataSharingAttribute data_attr = data_sharing.get_data_sharing(data_ref.get_base_symbol());

            ERROR_CONDITION(data_attr == OpenMP::DS_UNDEFINED, "Invalid data sharing for copy", 0);

            Source copy_sharing;
            if (it->is_shared())
            {
                copy_sharing << "NANOS_SHARED";
            }
            else
            {
                copy_sharing << "NANOS_PRIVATE";
            }

            struct {
                Source *source;
                const char* array;
                const char* struct_name;
            } fill_copy_data_info[] = {
                { &copy_items_src, "copy_data", "ol_args->" },
                { &copy_immediate_setup, "imm_copy_data", "imm_args." },
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

                DataReference copy_expr = it->get_copy_expression();

                if (it->is_shared())
                {
                    expression_address << copy_expr.get_address();
                }
                else
                {
                    DataEnvironItem data_env_item = data_environ_info.get_data_of_symbol(copy_expr.get_base_symbol());
                    // We have to use the value of the argument structure if it
                    // is private
                    expression_address 
                        << "&("
                        << fill_copy_data_info[j].struct_name 
                        << data_env_item.get_field_name()
                        << ")"
                        ;
                }
                expression_size << copy_expr.get_sizeof();
            }

            i++;
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

    spawn_code
        << "{"
        // Devices related to this task
        <<     device_description
        <<     struct_arg_type_name << "* ol_args = (" << struct_arg_type_name << "*)0;"
        <<     struct_runtime_size
        <<     "nanos_wd_t wd = (nanos_wd_t)0;"
        <<     "nanos_wd_props_t props;"
        <<     "__builtin_memset(&props, 0, sizeof(props));"
        <<     creation
        <<     priority
        <<     tiedness
        <<     copy_decl
        <<     "nanos_err_t err;"
        <<     if_expr_cond_start
        <<     "err = nanos_create_wd(&wd, " << num_devices << "," << device_descriptor << ","
        <<                 struct_size << ","
        <<                 "(void**)&ol_args, nanos_current_wd(),"
        <<                 "&props, " << num_copies << ", " << copy_data << ");"
        <<     "if (err != NANOS_OK) nanos_handle_error (err);"
        <<     if_expr_cond_end
        <<     "if (wd != (nanos_wd_t)0)"
        <<     "{"
        <<        fill_outline_arguments
        <<        fill_dependences_outline
        <<        copy_setup
        <<        "err = nanos_submit(wd, " << num_dependences << ", (nanos_dependence_t*)" << dependency_array << ", (nanos_team_t)0);"
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
        <<                struct_size << ", " << (immediate_is_alloca ? "imm_args" : "&imm_args") << ","
        <<                num_dependences << ", (nanos_dependence_t*)" << dependency_array << ", &props,"
        <<                num_copies << "," << copy_imm_data << ");"
        <<        "if (err != NANOS_OK) nanos_handle_error (err);"
        <<     "}"
        << "}"
        ;

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
    else if (expr.is_array_section())
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

