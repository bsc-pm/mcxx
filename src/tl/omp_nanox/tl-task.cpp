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

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::task_postorder(PragmaCustomConstruct ctr)
{
    OpenMP::DataSharing& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());

    ObjectList<Symbol> shared_symbols;
    data_sharing.get_all_symbols(OpenMP::DA_SHARED, shared_symbols);

    ObjectList<Symbol> firstprivate_symbols;
    data_sharing.get_all_symbols(OpenMP::DA_FIRSTPRIVATE, firstprivate_symbols);

    ObjectList<Symbol> private_symbols;
    data_sharing.get_all_symbols(OpenMP::DA_PRIVATE, private_symbols);
    Source private_decls;
    for (ObjectList<Symbol>::iterator it = private_symbols.begin();
            it != private_symbols.end();
            it++)
    {
        Symbol& sym(*it);
        Type type = sym.get_type();

        // In C++ private vars types must be default constructible
        private_decls
            << type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
            ;
    }

    ObjectList<OpenMP::DependencyItem> dependences;
    data_sharing.get_all_dependences(dependences);

    DataEnvironInfo data_environ_info;

    Source struct_arg_type_decl_src;
    std::string struct_arg_type_name;
    fill_data_environment_structure(firstprivate_symbols,
            shared_symbols,
            ctr.get_scope_link(),
            struct_arg_type_name,
            struct_arg_type_decl_src,
            data_environ_info);

    FunctionDefinition funct_def = ctr.get_enclosing_function();
    Symbol function_symbol = funct_def.get_function_symbol();

    int outline_num = TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER);
    TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER)++;
    Source outline_name;
    outline_name
        << "_ol_" << function_symbol.get_name() << "_" << outline_num
        ;

    Source initial_replace_code, replaced_body;

    do_outline_replacements(ctr.get_statement(),
            data_environ_info,
            replaced_body,
            initial_replace_code);

    Source device_description, device_descriptor, num_devices;

    Source outline_code, outline_parameters, outline_body;

    outline_parameters << struct_arg_type_name << "* __restrict _args";
    outline_body
        << private_decls
        << initial_replace_code
        << replaced_body
        ;

    outline_code = create_outline(
            funct_def,
            outline_name,
            outline_parameters,
            outline_body);
            

    // FIXME - Refactor this since it is quite common
    Source newly_generated_code;
    newly_generated_code
        << struct_arg_type_decl_src
        << outline_code
        ;

    // Device descriptor
    // FIXME - Currently only SMP is supported
    device_descriptor << outline_name << "_devices";
    device_description
        << "nanos_smp_args_t " << outline_name << "_smp_args = { (void(*)(void*))" << outline_name << "};"
        << "nanos_device_t " << device_descriptor << "[] ="
        << "{"
        // SMP
        << "{nanos_smp_factory, nanos_smp_dd_size, &" << outline_name << "_smp_args" << "},"
        << "};"
        ;

    // Currently only SMP is supported
    num_devices << 1;

    // Parse it in a sibling function context
    AST_t outline_code_tree
        = newly_generated_code.parse_declaration(funct_def.get_ast(), ctr.get_scope_link());
    ctr.get_ast().prepend_sibling_function(outline_code_tree);
    
    Source spawn_code;
    Source fill_outline_arguments, fill_immediate_arguments, 
           fill_dependences_outline,
           fill_dependences_immediate;

    Source dependency_array, num_dependences;

    fill_data_args("ol_args->", data_environ_info, fill_outline_arguments);
    fill_data_args("imm_args.", data_environ_info, fill_immediate_arguments);

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

        for (ObjectList<OpenMP::DependencyItem>::iterator it = dependences.begin();
                it != dependences.end();
                it++)
        {
            Source dependency_flags;
            dependency_flags << "{";
            OpenMP::DependencyItem::DependencyAttribute attr = it->get_kind();
            if ((attr & OpenMP::DependencyItem::INPUT) == OpenMP::DependencyItem::INPUT)
            {
                dependency_flags << "1,"; 
            }
            else
            {
                dependency_flags << "0,"; 
            }
            if ((attr & OpenMP::DependencyItem::OUTPUT) == OpenMP::DependencyItem::OUTPUT)
            {
                dependency_flags << "1,"; 
            }
            else
            {
                dependency_flags << "0,"; 
            }
            // can_rename not yet implemented
            dependency_flags << "0"
                << "}"
                ;

            DataEnvironItem data_env_item = data_environ_info.get_data_of_symbol(it->get_base_symbol());

            if (!data_env_item.get_symbol().is_valid())
            {
                internal_error("Could not get the data environment info of symbol '%s'\n",
                        it->get_base_symbol().get_name().c_str());
            }

            dependency_defs_outline
                << "{"
                << "(void**)&ol_args->" << data_env_item.get_field_name() << ","
                << dependency_flags << ","
                << "sizeof(" << it->get_dependency_expression().prettyprint() << ")"
                << "}"
                ;

            dependency_defs_immediate
                << "{"
                << "(void**)&imm_args." << data_env_item.get_field_name() << ","
                << dependency_flags << ","
                << "sizeof(" << it->get_dependency_expression().prettyprint() << ")"
                << "}"
                ;

            if ((it + 1) != dependences.end())
            {
                dependency_defs_outline << ",";
                dependency_defs_immediate << ",";
            }
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

    Source tiedness;
    PragmaCustomClause untied_clause = ctr.get_clause("untied");
    if (untied_clause.is_defined())
    {
        tiedness << "props.tied = 0;";
    }
    else
    {
        tiedness << "props.tied = 1;";
    }

    spawn_code
        << "{"
        // Devices related to this task
        <<     device_description
        <<     struct_arg_type_name << "* ol_args = (" << struct_arg_type_name << "*)0;"
        <<     "nanos_wd_t wd = (nanos_wd_t)0;"
        <<     "nanos_wd_props_t props = { 0 };"
        <<     tiedness
        <<     "nanos_err_t err;"
        <<     if_expr_cond_start
        <<     "err = nanos_create_wd(&wd, " << num_devices << "," << device_descriptor << ","
        <<                 "sizeof(" << struct_arg_type_name << "),"
        <<                 "(void**)&ol_args, nanos_current_wd(),"
        <<                 "&props);"
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
        <<        struct_arg_type_name << " imm_args;"
        <<        fill_immediate_arguments
        <<        fill_dependences_immediate
        <<        "err = nanos_create_wd_and_run(" 
        <<                num_devices << ", " << device_descriptor << ", "
        <<                "&imm_args, "
        <<                num_dependences << ", (nanos_dependence_t*)" << dependency_array << ", &props);"
        <<        "if (err != NANOS_OK) nanos_handle_error (err);"
        <<     "}"
        << "}"
        ;

    AST_t spawn_tree = spawn_code.parse_statement(ctr.get_ast(), ctr.get_scope_link());
    ctr.get_ast().replace(spawn_tree);
}
