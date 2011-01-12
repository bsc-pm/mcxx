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
#include "tl-parallel-common.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::parallel_postorder(PragmaCustomConstruct ctr)
{
    OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());

    DataEnvironInfo data_environ_info;
    compute_data_environment(
            data_sharing,
            ctr.get_scope_link(),
            data_environ_info,
            _converted_vlas);

    FunctionDefinition funct_def = ctr.get_enclosing_function();
    Symbol function_symbol = funct_def.get_function_symbol();

    Scope scope_of_struct = ctr.get_scope();
    if (function_symbol.is_member())
    {
        // Fix the scope because it will end inside the class
        scope_of_struct = function_symbol.get_scope();
    }

    std::string struct_arg_type_name;
    define_arguments_structure(ctr, struct_arg_type_name, data_environ_info, 
            ObjectList<OpenMP::DependencyItem>(), Source());

    int outline_num = TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER);
    TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER)++;

    std::stringstream ss;
    std::string outline_name;
    ss << "_ol_" << function_symbol.get_name() << "_" << outline_num;
    outline_name = ss.str();

    Source initial_replace_code, replaced_body;

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

    AST_t parallel_code = ctr.get_statement().get_ast();

    ObjectList<std::string> current_targets;
    data_sharing.get_all_devices(current_targets);

    Source spawn_source = common_parallel_code(outline_name,
            struct_arg_type_name,
            num_threads,
            ctr.get_scope_link(),
            data_environ_info,
            parallel_code,
            current_targets);

    AST_t spawn_tree = spawn_source.parse_statement(ctr.get_ast(), ctr.get_scope_link());
    ctr.get_ast().replace(spawn_tree);
}

