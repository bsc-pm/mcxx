/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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
#include "tl-outline-nanox.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::task_postorder(PragmaCustomConstruct ctr)
{
    OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());

    ObjectList<Symbol> shared_symbols;
    data_sharing.get_all_symbols(OpenMP::DS_SHARED, shared_symbols);

    ObjectList<Symbol> firstprivate_symbols;
    data_sharing.get_all_symbols(OpenMP::DS_FIRSTPRIVATE, firstprivate_symbols);

    ObjectList<Symbol> private_symbols;
    data_sharing.get_all_symbols(OpenMP::DS_PRIVATE, private_symbols);

    ObjectList<OpenMP::DependencyItem> dependences;
    data_sharing.get_all_dependences(dependences);

    DataEnvironInfo data_environ_info;
    compute_data_environment(firstprivate_symbols,
            shared_symbols,
            private_symbols,
            ctr.get_scope_link(),
            data_environ_info,
            _converted_vlas);

    Source struct_arg_type_decl_src, struct_fields;
    std::string struct_arg_type_name;
    fill_data_environment_structure(
            ctr.get_scope(),
            data_environ_info,
            struct_arg_type_decl_src,
            struct_fields,
            struct_arg_type_name, 
            dependences); // empty dependences

    FunctionDefinition funct_def = ctr.get_enclosing_function();
    Symbol function_symbol = funct_def.get_function_symbol();

    int outline_num = TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER);
    TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER)++;

    std::stringstream ss;
    ss << "_ol_" << function_symbol.get_name() << "_" << outline_num;
    std::string outline_name = ss.str();

    Source newly_generated_code;
    newly_generated_code
        << struct_arg_type_decl_src
        ;

    AST_t outline_code_tree
        = newly_generated_code.parse_declaration(funct_def.get_ast(), ctr.get_scope_link());
    ctr.get_ast().prepend_sibling_function(outline_code_tree);

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
    ObjectList<std::string> &current_targets = _target_ctx.back();
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

        OutlineFlags outline_flags;

        device_provider->create_outline(outline_name,
                struct_arg_type_name,
                data_environ_info,
                outline_flags,
                ctr.get_scope_link(),
                ctr.get_statement().get_ast());

        device_provider->get_device_descriptor(outline_name, 
                data_environ_info, 
                outline_flags,
                ancillary_device_description, 
                device_description_line);
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
            dependency_flags << "{";
            OpenMP::DependencyDirection attr = it->get_kind();
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

            Source dependency_field_name;

            if (it->is_symbol_dependence())
            {
                Symbol sym = it->get_symbol_dependence();

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

                // Can rename in this case
                dependency_flags << "1"
                    ;
            }
            else
            {
                dependency_field_name
                    << "dep_" << num_dep;

                // Cannot rename in this case
                dependency_flags << "0"
                    ;
            }

            dependency_flags << "}"
                    ;

            Expression dependency_expression = it->get_dependency_expression();
            Type size_type = it->get_dependency_expression().get_type();

            Source dep_size;
            dep_size
                << "sizeof(" << size_type.get_declaration(ctr.get_scope(), "") << ")"
                ;

            dependency_defs_outline
                << "{"
                << "(void**)&ol_args->" << dependency_field_name << ","
                << dependency_flags << ","
                << dep_size  
                << "}"
                ;

            if (!immediate_is_alloca)
            {
                dependency_defs_immediate
                    << "{"
                    << "(void**)&imm_args." << dependency_field_name << ","
                    << dependency_flags << ","
                    << dep_size 
                    << "}"
                    ;
            }
            else
            {
                dependency_defs_immediate
                    << "{"
                    << "(void**)imm_args->" << dependency_field_name << ","
                    << dependency_flags << ","
                    << dep_size 
                    << "}"
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

    // FIXME - This will be meaningful with 'copy_in' and 'copy_out'
    Source num_copies, copy_data;
    num_copies << "0";
    copy_data << "(nanos_copy_data_t*)0";

    spawn_code
        << "{"
        // Devices related to this task
        <<     device_description
        <<     struct_arg_type_name << "* ol_args = (" << struct_arg_type_name << "*)0;"
        <<     struct_runtime_size
        <<     "nanos_wd_t wd = (nanos_wd_t)0;"
        <<     "nanos_wd_props_t props = { 0 };"
        <<     priority
        <<     tiedness
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
        <<        "err = nanos_submit(wd, " << num_dependences << ", (nanos_dependence_t*)" << dependency_array << ", (nanos_team_t)0);"
        <<        "if (err != NANOS_OK) nanos_handle_error (err);"
        <<     "}"
        <<     "else"
        <<     "{"
        <<        immediate_decl
        <<        fill_immediate_arguments
        <<        fill_dependences_immediate
        <<        "err = nanos_create_wd_and_run(" 
        <<                num_devices << ", " << device_descriptor << ", "
        <<                struct_size << ", " << (immediate_is_alloca ? "imm_args" : "&imm_args") << ","
        <<                num_dependences << ", (nanos_dependence_t*)" << dependency_array << ", &props,"
        <<                num_copies << "," << copy_data << ");"
        <<        "if (err != NANOS_OK) nanos_handle_error (err);"
        <<     "}"
        << "}"
        ;

    AST_t spawn_tree = spawn_code.parse_statement(ctr.get_ast(), ctr.get_scope_link());
    ctr.get_ast().replace(spawn_tree);
}
