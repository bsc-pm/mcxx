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
    ObjectList<OpenMP::DependencyItem> dependences;
    data_sharing.get_all_dependences(dependences);	
    Source loop_info_field;
    loop_info_field
        << "nanos_loop_info_t loop_info;"
        ;

    DataEnvironInfo data_environ_info;
    compute_data_environment(
            data_sharing,
            ctr,
            data_environ_info,
            _converted_vlas);

    std::string struct_arg_type_name;
    define_arguments_structure(ctr, struct_arg_type_name, data_environ_info, 
            ObjectList<OpenMP::DependencyItem>(), loop_info_field);
            
    FunctionDefinition funct_def = ctr.get_enclosing_function();
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

    if ( (!ctr.get_clause("nowait").is_defined() 
            && !ctr.get_clause("input").is_defined() 
            && !ctr.get_clause("output").is_defined() 
            && !ctr.get_clause("inout").is_defined() )
            || !data_environ_info.get_reduction_symbols().empty())
    {
        final_barrier << get_barrier_code(ctr.get_ast());
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
            <<    "(_nth_step_sign * " << induction_var_name << ")" << "<= (_nth_step_sign * _nth_upper);"
            <<    induction_var_name << "+= _nth_step"
            << ")"
            << "{"
            <<    replaced_body
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
                ctr.get_statement().get_ast(),
                ctr.get_scope_link(),
                ancillary_device_description, 
                device_description_line);
    }

    num_devices << current_targets.size();


    Source fill_outline_arguments, 
           fill_dependences_outline;

    Source dependency_array, num_dependences;

    fill_data_args("ol_args", 
            data_environ_info, 
            dependences, 
            /* is_pointer */ true,
            fill_outline_arguments);

    bool env_is_runtime_sized = data_environ_info.environment_is_runtime_sized();

    // Fill dependences, if any
    if (!dependences.empty())
    {
        num_dependences << dependences.size();
        Source dependency_defs_outline;
        fill_dependences_outline
            << "nanos_dependence_t _dependences[" << num_dependences << "] = {"
            << dependency_defs_outline
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

            // Can rename in this case
            dependency_flags << "1"
                ;

            dependency_flags << "}"
                ;

            DataReference dependency_expression = it->get_dependency_expression();

            Source dep_size;
            dep_size << dependency_expression.get_sizeof();

            Source dependency_offset;

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

            if ((it + 1) != dependences.end())
            {
                dependency_defs_outline << ",";
            }

            num_dep++;
        }
    }
    else
    {
        dependency_array << "0";
        num_dependences << "0";
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

    bool some_device_needs_copies = false;
    Source num_copies;

    ObjectList<OpenMP::CopyItem> copy_items = data_environ_info.get_copy_items();

    Source copy_data, copy_decl, copy_setup;

    if (copy_items.empty()
            || !some_device_needs_copies)
    {
        num_copies << "0";
        copy_data << "(nanos_copy_data_t**)0";
    }
    else
    {
        num_copies << copy_items.size();

        copy_decl << "nanos_copy_data_t* copy_data = (nanos_copy_data_t*)0;";
        Source copy_items_src;
        copy_setup << copy_items_src;
        copy_data << "&copy_data";

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

            bool has_shared_data_sharing = (data_attr & OpenMP::DS_SHARED) == OpenMP::DS_SHARED;

            Source copy_sharing;
            if (has_shared_data_sharing)
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

                if (has_shared_data_sharing)
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

    // FIXME - This will be meaningful with 'copy_in' and 'copy_out'
    Source num_copies1, copy_data1;    
    num_copies1 << "0";
    copy_data1 << "(nanos_copy_data_t**)0";

    Source creation;

    if ( current_targets.contains( "cuda" ) )
    {
        creation << "props.mandatory_creation = 1;"
            ;
    }

    ObjectList<OpenMP::ReductionSymbol> reduction_symbols = data_environ_info.get_reduction_symbols();
    
    Source reduction_join_arr_decls;
    if (!reduction_symbols.empty())
        reduction_join_arr_decls 
        << "int _nth_team = omp_get_num_threads();"
        ;


    if (!reduction_symbols.empty())
        reduction_join_arr_decls << "int rs_i;" ;
    for(ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_symbols.begin();
            it != reduction_symbols.end(); 
            it++)
    {
        Symbol rs = it->get_symbol();
        OpenMP::UDRInfoItem2 udr2 = it->get_udr_2();
        Source auxiliar_initializer;
        
        if (rs.get_type().is_class())
        {
            // When the symbol has class type, we must build an auxiliary variable initialized to the symbol's identity
            // in order to initialize the reduction's vector
            auxiliar_initializer  << "aux_" << rs.get_name();
            Source identity;
            reduction_join_arr_decls
                << rs.get_type().get_declaration(rs.get_scope(), "") << " " << auxiliar_initializer
                << identity 
                << ";"
                ;

            if (udr2.has_identity())
            {
                identity <<  udr2.get_identity().prettyprint() << ";"
                    ;
            }
        }
        else
        {
            auxiliar_initializer << udr2.get_identity().prettyprint();
        }
                  
        reduction_join_arr_decls
            << rs.get_type().get_declaration(rs.get_scope(), "") << " rdv_" << rs.get_name() << "[_nth_team];"
            << "for(rs_i=0; rs_i<_nth_team; rs_i++)"
            << "{"
        ;

        CXX_LANGUAGE()
        {
            if (udr2.has_identity())
            {
                if (udr2.get_need_equal_initializer())
                {
                    reduction_join_arr_decls 
                        << "rdv_" << rs.get_name() << "[rs_i] = " << auxiliar_initializer << ";"
                    ;
                }
                else
                {
                    if (udr2.get_is_constructor())
                    {
                        reduction_join_arr_decls 
                            << "rdv_" << rs.get_name() << "[rs_i] " << auxiliar_initializer << ";"
                        ;
                    }
                    else if (!rs.get_type().is_enum())
                    {
                        reduction_join_arr_decls 
                            << "rdv_" << rs.get_name() << "[rs_i] (" << auxiliar_initializer << ");"
                        ;
                    }
                }
            }
        }
        C_LANGUAGE()
        {
            reduction_join_arr_decls 
                << "rdv_" << rs.get_name() << "[rs_i] = " << auxiliar_initializer << ";"
            ;
        }
        
        reduction_join_arr_decls << "}";
    }

    Source omp_reduction_join, omp_reduction_argument;
    // Get reduction code
    for (ObjectList<std::string>::iterator it = current_targets.begin();
            it != current_targets.end();
            it++)
    {
        DeviceProvider* device_provider = device_handler.get_device(*it);
        
        if (device_provider->get_name()=="smp")
        {
            omp_reduction_join 
                << device_provider->get_reduction_code(reduction_symbols, ctr.get_scope_link())
            ;
        }
    }
    // Fill outline variables
    for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_symbols.begin();
            it != reduction_symbols.end();
            it++)
    {
        omp_reduction_argument
            << "ol_args->rdv_" << it->get_symbol().get_name() << " = rdv_" << it->get_symbol().get_name() << ";";
    }

    Source alignment, slicer_alignment;
    if (Nanos::Version::interface_is_at_least("master", 5004))
    {
        alignment <<  "__alignof__(" << struct_arg_type_name << "),"
            ;
        slicer_alignment <<  "__alignof__(nanos_slicer_data_for_t),"
            ;
    }

    Source create_sliced_wd, loop_information, decl_slicer_data_if_needed;

    if (Nanos::Version::interface_is_at_least("master", 5008))
    {
        create_sliced_wd
            <<"nanos_create_sliced_wd(&wd, "
            <<   /* num_devices */ "1, " << device_descriptor << ", "
            <<   "sizeof(" << struct_arg_type_name << "),"
            <<   alignment
            <<   "(void**)&ol_args,"
            <<   "nanos_current_wd(),"
            <<   current_slicer << ","
            <<   "&props," << num_copies1 << "," << copy_data1 << ");"
            ;
        loop_information
            << "ol_args->loop_info.lower = " << for_statement.get_lower_bound() << ";"
            << "ol_args->loop_info.upper = " << for_statement.get_upper_bound() << ";"
            << "ol_args->loop_info.step  = " << for_statement.get_step() << ";"
            << "ol_args->loop_info.chunk = " << chunk_value << ";"
            ;
    }
    else
    {
        create_sliced_wd
            << "nanos_create_sliced_wd(&wd, "
            <<   /* num_devices */ "1, " << device_descriptor << ", "
            <<   "sizeof(" << struct_arg_type_name << "),"
            <<   alignment
            <<   "(void**)&ol_args,"
            <<   "nanos_current_wd(),"
            <<   current_slicer << ","
            <<   "sizeof(nanos_slicer_data_for_t),"
            <<   slicer_alignment
            <<   "(nanos_slicer_t*) &slicer_data_for,"
            <<   "&props," << num_copies1 << "," << copy_data1 << ");"
            ;
        decl_slicer_data_if_needed
            << "nanos_slicer_data_for_t* slicer_data_for = (nanos_slicer_data_for_t*)0;"
            ;
        loop_information
            << "slicer_data_for->_lower = " << for_statement.get_lower_bound() << ";"
            << "slicer_data_for->_upper = " << for_statement.get_upper_bound() << ";"
            << "slicer_data_for->_step = " << for_statement.get_step() << ";"
            << "slicer_data_for->_chunk = " << chunk_value << ";"
            ;
    }
    if(!_no_nanox_calls)
    {
        spawn_source
            << "{"
            <<     get_single_guard("single_guard")
            <<     "if (err != NANOS_OK) nanos_handle_error(err);"
            <<     reduction_join_arr_decls
            <<     "if (single_guard)"
            <<     "{"
            <<        struct_arg_type_name << "* ol_args = (" << struct_arg_type_name << "*)0;"
            <<        struct_runtime_size
            <<        "nanos_wd_t wd = (nanos_wd_t)0;"
            <<        "nanos_wd_props_t props;"
            <<        "__builtin_memset(&props, 0, sizeof(props));"
            <<        creation
            <<        "props.mandatory_creation = 1;"
            <<        priority
            <<        tiedness
            <<        copy_decl
            <<        "static nanos_slicer_t " << current_slicer << " = 0;"
            <<        device_description
            <<        "if (!" << current_slicer << ") " << current_slicer <<  " = nanos_find_slicer(\"" << current_slicer << "\");"
            <<        decl_slicer_data_if_needed
            <<        "err = " << create_sliced_wd
            <<        "if (err != NANOS_OK) nanos_handle_error(err);"
            <<            fill_outline_arguments
            <<            omp_reduction_argument
            <<            fill_dependences_outline
            <<            copy_setup
            <<            loop_information
            <<            "err = nanos_submit(wd, " << num_dependences << ", (nanos_dependence_t*)" << dependency_array << ", (nanos_team_t)0);"
            <<            "if (err != NANOS_OK) nanos_handle_error (err);"
            <<     "}"
            <<     final_barrier
            <<     omp_reduction_join
            << "}"
            ;
    }
    else
    {
        if(current_targets.contains("smp"))
        {
            std::stringstream smp_device_call;
            smp_device_call << "_smp_" << outline_name << "(ol_args);";
            
            // The code generated must not contain calls to runtime. The execution will be serial
            spawn_source
                << "{"
                <<     struct_arg_type_name << "* ol_args = (" << struct_arg_type_name << "*)0;"
                <<     fill_outline_arguments
                <<     omp_reduction_argument
                <<     loop_information
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
    AST_t spawn_tree = spawn_source.parse_statement(ctr.get_ast(), ctr.get_scope_link());
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


