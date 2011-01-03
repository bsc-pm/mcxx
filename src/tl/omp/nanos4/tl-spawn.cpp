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



#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        AST_t OpenMPTransform::get_parallel_spawn_code(
                AST_t ref_tree, // Reference tree, needed for correct parsing
                FunctionDefinition function_definition,
                Scope scope,
                ScopeLink scope_link,
                ObjectList<ParameterInfo> parameter_info_list,
                ObjectList<OpenMP::ReductionSymbol> reduction_references,
                PragmaCustomClause if_clause,
                PragmaCustomClause num_threads_clause,
                PragmaCustomClause groups_clause,
                Source &instrument_code_before,
                Source &instrument_code_after)
        {
            return get_parallel_spawn_code_with_team(
                    ref_tree, // Reference tree, needed for correct parsing
                    function_definition,
                    scope,
                    scope_link,
                    parameter_info_list,
                    reduction_references,
                    if_clause,
                    num_threads_clause,
                    groups_clause,
                    instrument_code_before,
                    instrument_code_after);
        }

        AST_t OpenMPTransform::get_parallel_spawn_code_with_team(
                AST_t ref_tree, // Reference tree, needed for correct parsing
                FunctionDefinition function_definition,
                Scope scope,
                ScopeLink scope_link,
                ObjectList<ParameterInfo> parameter_info_list,
                ObjectList<OpenMP::ReductionSymbol> reduction_references,
                PragmaCustomClause if_clause,
                PragmaCustomClause num_threads_clause,
                PragmaCustomClause groups_clause,
                Source &instrument_code_before,
                Source &instrument_code_after)
        {
            Source spawn_code;
            Source reduction_vectors;

            // referenced parameters go to nth_create_new
            Source referenced_parameters;
            // outline_arguments go directly to the outline function
            Source outline_arguments;

            Source reduction_code;

            Source size_vector;

            Source src_num_args_val;
            Source src_num_args_ref;

            Source nth_creation_function;
            Source additional_declarations;

            Source outlined_function_name_decl;

            // Calculate the proper expression referring this function
            outlined_function_name_decl 
                << get_outline_function_reference(function_definition, parameter_info_list, 
                        /* team_parameter */ true);

            Source team_size_definition;

            Source master_outline_invocation;

            // The skeleton of the spawn code will be this one
            spawn_code
                << "{"
                <<    "nth_team_t nth_current_team;"
                <<    team_size_definition
                <<    "void* nth_team_data = (void*)0;"
                <<    "nth_desc *nth_selfv = nth_self();"
                <<    "nth_desc *nth_nosucc = (nth_desc*)0;"
                <<    "int nth_num_deps;"
                <<    "int nth_nargs_ref = " << src_num_args_ref << ";"
                <<    reduction_vectors
                <<    instrument_code_before
                <<    size_vector 
                <<    additional_declarations
                <<    comment("Creating team")
                <<    "nth_init_team(&nth_current_team, nth_team_size, nth_selfv, nth_team_data);"
                <<    "nth_num_deps = 0;"
                <<    "int nth_p;"
                <<    "for (nth_p = 1; nth_p < nth_team_size; nth_p++) "
                <<    "{"
                <<        nth_creation_function
                <<    "}"
                <<    master_outline_invocation
                <<    instrument_code_after
                <<    comment("Ending team")
                <<    "nth_end_team(&nth_current_team);"
                <<    reduction_code
                << "}"
                ;


            // For every entity in the reduction_references list
            ObjectList<OpenMP::ReductionSymbol> merged_reduction_references;

            merged_reduction_references.append(reduction_references);
            merged_reduction_references.insert(inner_reductions_stack.top(), functor(&OpenMP::ReductionSymbol::get_symbol));

            for (ObjectList<OpenMP::ReductionSymbol>::iterator it = merged_reduction_references.begin();
                    it != merged_reduction_references.end();
                    it++)
            {
                // create a reduction vector after the name of the mangled entity
                std::string reduction_vector_name = "rdv_" + it->get_symbol().get_name();

                // get its type
                Symbol reduction_symbol = it->get_symbol();
                Type reduction_type = reduction_symbol.get_type();
                if (!_new_udr)
                {
                    OpenMP::UDRInfoItem udr = it->get_udr();

                    if (udr.get_is_array_reduction() && reduction_type.is_pointer())
                    {
                        reduction_type = reduction_type.points_to();
                    }
                }

                // create a tree of expression 128
                // FIXME: hardcoded to 128 processors
                Source array_length;
                array_length << "128";
                AST_t array_length_tree = array_length.parse_expression(ref_tree,
                        scope_link);

                // and get an array of 128 elements
                Type reduction_vector_type = reduction_type.get_array_to(array_length_tree, 
                        scope);

                // now get the code that declares this reduction vector
                reduction_vectors
                    << comment("Reduction vector for '" + it->get_symbol().get_qualified_name() 
                            + "' at " + it->get_symbol().get_point_of_declaration().get_locus() )
                    << reduction_vector_type.get_declaration(scope, 
                            reduction_vector_name) << ";";
            }

            // Referenced parameters
            // Team is always passed so 'num_args_ref' is at least 1
            int num_args_ref = 1;
            referenced_parameters << "&nth_current_team" 
                ;
            outline_arguments << "&nth_current_team"
                ;

            if (is_nonstatic_member_function(function_definition))
            {
                referenced_parameters << ", this";
                outline_arguments << ", this";
                num_args_ref++;
            }

            // First the pointer ones
            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                if (it->kind != ParameterInfo::BY_POINTER)
                    continue;

                // Simply pass its reference (its address)
                referenced_parameters << ", " << it->argument_name;
                outline_arguments << "," << it->argument_name;

                num_args_ref++;
            }
            src_num_args_ref << num_args_ref;

            Source firstprivatized_data;
            Source copy_construction_part;

            // Now the value ones. We fill the nth_sizes vector here
            size_vector << "size_t nth_sizes[] = {0";
            int num_args_val = 0;
            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                if (it->kind != ParameterInfo::BY_VALUE)
                    continue;

                // Size and reference
                size_vector << ", sizeof(" << it->symbol.get_type().get_declaration(scope, "") << ")";

                referenced_parameters << ", &nth_sizes[" << (num_args_val + 1) << "], &" 
                    << it->symbol.get_qualified_name(scope);

                Type type = it->symbol.get_type();

                // Advance over references in C++
                if (type.is_reference())
                {
                    type = type.references_to();
                }

                // Save the firstprivatized data
                // FIXME - Fix this for C++
                // Shamelessly copied from tl-task.cpp
                if (!type.is_array())
                {
                    outline_arguments << ", &cv_" << it->symbol.get_name();
                    firstprivatized_data
                        << type.get_declaration_with_initializer(
                                scope,
                                "cv_" + it->symbol.get_name(),
                                it->symbol.get_qualified_name(scope)) 
                        << ";"
                        ;
                }
                else
                {
                    outline_arguments << ", cv_" << it->symbol.get_name();
                    Source src_array_copy = array_copy(type, "cv_" + it->symbol.get_name(),
                            it->symbol.get_qualified_name(scope), 0);

                    firstprivatized_data
                        << type.get_declaration(scope,
                                "cv_" + it->symbol.get_name())
                        << ";"
                        << src_array_copy
                        ;
                }

                CXX_LANGUAGE()
                {
                    if (type.is_class())
                    {
                        copy_construction_part
                            << "new (nth_arg_addr[" << (num_args_val + 1) << "])" 
                            << type.get_declaration(scope, "")
                            << "(" << it->symbol.get_qualified_name(scope) << ");"
                            ;
                    }
                }

                num_args_val++;
            }
            size_vector << "};";


            additional_declarations
                << "  int nth_task_type = NTH_DTYPE_LOCAL;"
                << "  int nth_nargs_val = " << src_num_args_val << ";"
                << "  void *nth_arg_addr[" << src_num_args_val << " + 1];"
                << "  void **nth_arg_addr_ptr = &(nth_arg_addr[1]);"
                ;
            nth_creation_function 
                << comment("Master creates team members")
                << "     nth_desc_t* nth = nth_create_new((void*)(" << outlined_function_name_decl << "), "
                << "            &nth_task_type, &nth_num_deps, &nth_p, &nth_nosucc, "
                << "            &nth_arg_addr_ptr, &nth_nargs_ref, &nth_nargs_val," << referenced_parameters << ");"
                << copy_construction_part
                << "     nth_submit(nth);"
                ;

            master_outline_invocation
                << "{"
                <<    comment ("Master invokes outline")
                <<    firstprivatized_data
                <<    "(" << outlined_function_name_decl << ")" << "(" << outline_arguments << ");"
                << "}"
                ;

            if (num_args_val == 0)
            {
                // Disable the declaration of the size vector to avoid a warning
                size_vector = TL::Source("");
            }

            src_num_args_val << num_args_val;

            // IF clause
            Source team_size;
            if (if_clause.is_defined())
            {
                team_size_definition
                    << "int nth_team_size = 1;"
                    << "if (" << if_clause.get_expression_list()[0].prettyprint() << ")"
                    << "{"
                    <<    "nth_team_size = " << team_size << ";"
                    << "}"
                    ;
            }
            else
            {
                // Nothing fancy has to be done here, just like we did before
                team_size_definition
                    << "int nth_team_size = " << team_size << ";"
                    ;
            }

            // Groups definition
            if (!groups_clause.is_defined() && !num_threads_clause.is_defined())
            {
                team_size << " nth_get_num_team_players()";
            }
            else if (num_threads_clause.is_defined())
            {
                ObjectList<Expression> clause_exprs = num_threads_clause.get_expression_list();

                std::string num_threads_value = clause_exprs[0].prettyprint();
                team_size << num_threads_value ;
            }
            else /* groups is defined */
            {
                std::cerr << groups_clause.get_ast().get_locus() << ": GROUPS not supported yet" << std::endl;
            }

            // Reduction code
            //
            // If there is any reduction reference
            reduction_code = get_noncritical_reduction_code(reduction_references, scope_link);

            // std::cerr << "SPAWN CODE" << std::endl;
            // std::cerr << spawn_code.get_source(true) << std::endl;
            // std::cerr << "End SPAWN CODE" << std::endl;

            // Parse the spawn code and return it
            AST_t result = spawn_code.parse_statement(ref_tree, scope_link);
            return result;
        }

    }
}
