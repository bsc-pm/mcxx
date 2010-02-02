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
#include "tl-parallel-common.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::parallel_postorder(PragmaCustomConstruct ctr)
{
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
    DataEnvironInfo data_environ_info;
    compute_data_environment(firstprivate_symbols,
            shared_symbols,
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

    do_outline_replacements(ctr.get_statement(),
            data_environ_info,
            replaced_body,
            initial_replace_code);

    Source final_barrier;

    final_barrier
        << "nanos_team_barrier();"
        ;

    Source outline_body, outline_parameters, outline_code;

    outline_parameters << struct_arg_type_name << "* __restrict _args";
    outline_body
        << private_decls
        << initial_replace_code
        << replaced_body
        << final_barrier
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

    Source spawn_source = common_parallel_spawn_code(num_devices,
            outline_name,
            struct_arg_type_name,
            num_threads,
            data_environ_info);

    AST_t spawn_tree = spawn_source.parse_statement(ctr.get_ast(), ctr.get_scope_link());
    ctr.get_ast().replace(spawn_tree);
}

