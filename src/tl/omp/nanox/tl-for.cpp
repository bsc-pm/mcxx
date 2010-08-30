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
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::for_postorder(PragmaCustomConstruct ctr)
{
    ForStatement for_statement(ctr.get_statement().get_ast(), ctr.get_scope_link());
    Statement for_body = for_statement.get_loop_body();

    OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());

    // FIXME - Reductions!!
    Source struct_fields;
    struct_fields
        << "nanos_loop_info_t loop_info;"
        ;

    DataEnvironInfo data_environ_info;
    compute_data_environment(
            data_sharing,
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

    Source newly_generated_code;
    newly_generated_code
        << struct_arg_type_decl_src
        ;
    
    FunctionDefinition funct_def = ctr.get_enclosing_function();
    AST_t outline_code_tree
        = newly_generated_code.parse_declaration(funct_def.get_ast(), ctr.get_scope_link());
    ctr.get_ast().prepend_sibling_function(outline_code_tree);

    Symbol function_symbol = funct_def.get_function_symbol();

    int outline_num = TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER);
    TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER)++;
    std::stringstream ss;
    ss << "_ol_" << function_symbol.get_name() << "_" << outline_num;

    std::string outline_name = ss.str();

    Source loop_distr_setup;
    loop_distr_setup
        << "int _nth_lower = _args->loop_info.lower;"
        << "int _nth_upper = _args->loop_info.upper;"
        << "int _nth_step = _args->loop_info.step;"
        << "int _nth_step_sign = 1;"
        << "if (_nth_step < 0)"
        <<   "_nth_step_sign = -1;"
        ;

    Source final_barrier;

    if (!ctr.get_clause("nowait").is_defined())
    {
        final_barrier
            << "nanos_wg_wait_completion(nanos_current_wd());"
            << "nanos_team_barrier();"
            ;
    }

    Source induction_var_name = for_statement.get_induction_variable().prettyprint();

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

    OutlineFlags outline_flags;

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
                for_statement.get_loop_body().get_ast(),
                ctr.get_scope_link(),
                initial_setup,
                replaced_body);

        Source outline_body;
        outline_body
            << loop_distr_setup
            << "for ("
            <<    induction_var_name << "= _nth_lower;"
            <<    "(_nth_step_sign * " << induction_var_name << ")" << "<= _nth_upper;"
            <<    induction_var_name << "+= _nth_step"
            << ")"
            << "{"
            << replaced_body
            << "}"
            ;

        device_provider->create_outline(outline_name,
                struct_arg_type_name,
                data_environ_info,
                outline_flags,
                ctr.get_statement().get_ast(),
                ctr.get_scope_link(),
                initial_setup,
                outline_body);

        device_provider->get_device_descriptor(outline_name, 
                data_environ_info, 
                outline_flags,
                ancillary_device_description, 
                device_description_line);
    }
    
    num_devices << current_targets.size();

    Source current_slicer;
    Source chunk_value;
    chunk_value = Source("0");
    current_slicer = Source("static_for");

    PragmaCustomClause schedule_clause = ctr.get_clause("schedule");
    if (schedule_clause.is_defined())
    {
        ObjectList<std::string> args = schedule_clause.get_arguments(ExpressionTokenizerTrim());

        current_slicer = args[0] + "_for";

        if (args.size() > 1)
        {
            chunk_value = Source(args[1]);
        }
    }

    // FIXME - Move this to a tl-workshare.cpp
    Source spawn_source;

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
    copy_data << "(nanos_copy_data_t**)0";

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
