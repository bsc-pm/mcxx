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

void OMPTransform::for_postorder(PragmaCustomConstruct ctr)
{
    ForStatement for_statement(ctr.get_statement().get_ast(), ctr.get_scope_link());
    Statement for_body = for_statement.get_loop_body();

    OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());

    ObjectList<Symbol> shared_symbols;
    data_sharing.get_all_symbols(OpenMP::DS_SHARED, shared_symbols);

    ObjectList<Symbol> firstprivate_symbols;
    data_sharing.get_all_symbols(OpenMP::DS_FIRSTPRIVATE, firstprivate_symbols);

    Source private_decls;
    ObjectList<Symbol> private_symbols;
    data_sharing.get_all_symbols(OpenMP::DS_PRIVATE, private_symbols);
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

    // FIXME - Reductions!!
    Source struct_fields;
    struct_fields
        << "nanos_loop_info_t loop_info;"
        ;

    DataEnvironInfo data_environ_info;
    compute_data_environment(firstprivate_symbols,
            shared_symbols,
            private_symbols,
            ctr.get_scope_link(),
            data_environ_info,
            _converted_vlas);

    Source struct_arg_type_decl_src;
    std::string struct_arg_type_name;
    fill_data_environment_structure(
            ctr.get_scope(),
            data_environ_info,
            struct_arg_type_decl_src,
            struct_fields,
            struct_arg_type_name, 
            ObjectList<OpenMP::DependencyItem>()); // empty dependences

    FunctionDefinition funct_def = ctr.get_enclosing_function();
    Symbol function_symbol = funct_def.get_function_symbol();

    int outline_num = TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER);
    TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER)++;
    Source outline_name;
    outline_name
        << "_ol_" << function_symbol.get_name() << "_" << outline_num
        ;

    Source initial_replace_code, replaced_body;

    do_outline_replacements(for_body,
            data_environ_info,
            replaced_body,
            initial_replace_code);

    initial_replace_code
        << "int _nth_lower = _args->loop_info.lower;"
        << "int _nth_upper = _args->loop_info.upper;"
        << "int _nth_step = _args->loop_info.step;"
        << "int _nth_step_sign = 1;"
        << "if (_nth_step < 0)"
        <<   "_nth_step_sign = -1;";
        ;

    Source final_barrier;

    if (!ctr.get_clause("nowait").is_defined())
    {
        final_barrier
            << "nanos_team_barrier();"
            ;
    }

    Source outline_body, outline_parameters, outline_code;

    Source induction_var_name = for_statement.get_induction_variable().prettyprint();

    outline_parameters << struct_arg_type_name << "* __restrict _args";
    outline_body
        << private_decls
        << initial_replace_code
        << "for ("
        <<    induction_var_name << "= _nth_lower;"
        <<    "(_nth_step_sign * " << induction_var_name << ")" << "<= _nth_upper;"
        <<    induction_var_name << "+= _nth_step"
        << ")"
        << "{"
        << replaced_body
        << "}"
        ;

    outline_code = create_outline(
            funct_def,
            outline_name,
            outline_parameters,
            outline_body);


    // Refactor!
    Source newly_generated_code;
    newly_generated_code
        << struct_arg_type_decl_src
        << outline_code
        ;
    
    // Currently only SMP is supported
    Source num_devices;
    num_devices << 1;
    
    // Parse it in a sibling function context
    AST_t outline_code_tree
        = newly_generated_code.parse_declaration(funct_def.get_ast(), ctr.get_scope_link());
    ctr.get_ast().prepend_sibling_function(outline_code_tree);

    Source num_threads;

    PragmaCustomClause num_threads_clause = ctr.get_clause("num_threads");
    if (num_threads_clause.is_defined())
    {
        num_threads << num_threads_clause.get_expression_list()[0];
    }
    else
    {
        // Do not know how to request the default parallel team thread number
        num_threads << "0";
    }

    Source current_slicer;
    Source chunk_value;
    chunk_value = Source("1");
    current_slicer = Source("dynamic_for");

    PragmaCustomClause schedule_clause = ctr.get_clause("schedule");
    if (schedule_clause.is_defined())
    {
        ObjectList<std::string> args = schedule_clause.get_arguments(ExpressionTokenizerTrim());

        if (args[0] == "static")
        {
            running_error("%s: error: 'static(schedule)' not implemented yet", 
                    ctr.get_ast().get_locus().c_str());
        }

        current_slicer = args[0] + "_for";

        if (args.size() > 1)
        {
            chunk_value = Source(args[1]);
        }
    }

    // if (!_registered_slicer[current_slicer.get_source()])
    // {
    //     Source register_slicer;
    //     register_slicer
    //         << "__attribute__((weak)) nanos_slicer_t " << current_slicer.get_source() << " = (nanos_slicer_t)0;"
    //         ;
    //     AST_t global_tree = ctr.get_ast().get_enclosing_global_tree();
    //     AST_t slicer_decl = register_slicer.parse_declaration(global_tree, ctr.get_scope_link());
    //     global_tree.prepend(slicer_decl);
    //     _registered_slicer[current_slicer.get_source()] = true;
    // }

    // FIXME - Move this to a tl-workshare.cpp
    Source spawn_source;

    Source device_descriptor,device_description;
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

    Source fill_outline_arguments;
    fill_data_args(
            "loop_data",
            data_environ_info, 
            ObjectList<OpenMP::DependencyItem>(), // empty
            /* is_pointer */ true,
            fill_outline_arguments);

    Source bool_type;
    C_LANGUAGE()
    {
        bool_type << "_Bool";
    }
    CXX_LANGUAGE()
    {
        bool_type << "bool";
    }

    // FIXME - This will be meaningful with 'copy_in' and 'copy_out'
    Source num_copies, copy_data;
    num_copies << "0";
    copy_data << "(nanos_copy_data_t*)0";

    spawn_source
        << "{"
        << bool_type << " single_guard;"
        << "nanos_err_t err = nanos_single_guard(&single_guard);"
        << "if (err != NANOS_OK) nanos_handle_error(err);"
        << "if (single_guard)"
        << "{"
        <<    "nanos_slicer_t " << current_slicer << " = nanos_find_slicer(\"" << current_slicer << "\");"
        <<    "nanos_err_t err;"
        <<    "nanos_wd_t wd = (nanos_wd_t)0;"
        <<    device_description
        <<    struct_arg_type_name << "*loop_data = ("<< struct_arg_type_name << "*)0;"
        <<    "nanos_wd_props_t props = {"
        <<         "/*.mandatory_creation =*/ 1,"
        <<         "/*.tied =*/ 0,"
        <<         "/*.tie_to =*/ 0"
        <<    "};"
        <<    "nanos_slicer_data_for_t* slicer_data_for = (nanos_slicer_data_for_t*)0;"
        <<    "err = nanos_create_sliced_wd(&wd, "
        <<          /* num_devices */ "1, " << device_descriptor << ", "
        <<          "sizeof(" << struct_arg_type_name << "),"
        <<          "(void**)&loop_data,"
        <<          "nanos_current_wd(),"
        <<          current_slicer << ","
        <<          "sizeof(nanos_slicer_data_for_t),"
        <<          "(nanos_slicer_t*) &slicer_data_for,"
        <<          "&props," << num_copies << "," << copy_data << ");"
        <<    "if (err != NANOS_OK) nanos_handle_error(err);"
        <<    fill_outline_arguments
        <<    "slicer_data_for->_lower = " << for_statement.get_lower_bound() << ";"
        <<    "slicer_data_for->_upper = " << for_statement.get_upper_bound() << ";"
        <<    "slicer_data_for->_step = " << for_statement.get_step() << ";"
        <<    "slicer_data_for->_chunk = " << chunk_value << ";"
        <<    "err = nanos_submit(wd, 0, (nanos_dependence_t*)0, 0);"
        <<    "if (err != NANOS_OK) nanos_handle_error(err);"
        << "}"
        << final_barrier
        << "}"
        ;

    AST_t spawn_tree = spawn_source.parse_statement(ctr.get_ast(), ctr.get_scope_link());
    ctr.get_ast().replace(spawn_tree);
}
