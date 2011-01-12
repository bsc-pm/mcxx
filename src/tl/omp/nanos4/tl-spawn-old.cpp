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
        AST_t OpenMPTransform::get_parallel_spawn_code_without_team(
                AST_t ref_tree, // Reference tree, needed for correct parsing
                FunctionDefinition function_definition,
                Scope scope,
                ScopeLink scope_link,
                ObjectList<ParameterInfo> parameter_info_list,
                ObjectList<OpenMP::ReductionSymbol> reduction_references,
                PragmaCustomClause num_threads_clause,
                PragmaCustomClause groups_clause,
                Source &instrument_code_before,
                Source &instrument_code_after)
        {
            Source spawn_code;
            Source reduction_vectors;
            Source groups_definition;
            Source referenced_parameters;

            Source reduction_code;

            Source size_vector;

            Source src_num_args_val;
            Source src_num_args_ref;

            Source nth_creation_function;
            Source additional_declarations;

            Source outlined_function_name_decl;

            // Calculate the proper expression referring this function
            outlined_function_name_decl 
                << get_outline_function_reference(function_definition, 
                        parameter_info_list, /* team_parameter */ false);

            // The skeleton of the spawn code will be this one
            spawn_code
                << "{"
                << "  int nth_nprocs;"
                << "  nth_desc *nth_selfv;"
                << "  int nth_num_deps;"
                << "  int nth_p;"
                <<    reduction_vectors
                <<    instrument_code_before
                <<    groups_definition
                <<    size_vector 
                << "  int nth_nargs_ref = " << src_num_args_ref << ";"
                <<    additional_declarations
                << "  nth_selfv = nthf_self_();"
                << "  nthf_team_set_nplayers_ (&nth_nprocs);"
                << "  nth_num_deps = 0;"
                << "  for (nth_p = nth_nprocs-1; nth_p >= 0 ; nth_p-- ) "
                << "  {"
                <<       nth_creation_function
                << "  }"
                << "  nthf_block_();"
                <<    reduction_code
                <<    instrument_code_after
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
            //
            // "this" might be needed
            int num_args_ref = 0;
            if (is_nonstatic_member_function(function_definition))
            {
                referenced_parameters << ", this";
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

                num_args_ref++;
            }
            src_num_args_ref << num_args_ref;

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
                size_vector << ", sizeof(" << it->symbol.get_qualified_name(scope) << ")";

                referenced_parameters << ", &nth_sizes[" << (num_args_val + 1) << "], &" 
                    << it->symbol.get_qualified_name(scope);

                num_args_val++;
            }
            size_vector << "};";


            if ((num_args_val == 0)
                    && (!enable_nth_create))
            {
                nth_creation_function
                    << "     nthf_create_1s_vp_((void*)(" << outlined_function_name_decl << "), &nth_num_deps, &nth_p, &nth_selfv, 0, "
                    << "        &nth_nargs_ref " << referenced_parameters << ");"
                    ;
            }
            else // (num_args_val != 0 || enable_nth_create)
            {
                if (!enable_nth_create)
                {
                    // FIXME. We are giving an approximate locus but this
                    // should not be very important since in some near 
                    // future we will remove the old interface :)
                    std::cerr << get_phase_name() << ": Warning, OpenMP construct in function '" 
                        << function_definition.get_function_name().prettyprint() 
                        << "' at " 
                        << function_definition.get_ast().get_locus() 
                        << " requires using the new interface since something must be passed by value."
                        << std::endl;
                }

                additional_declarations
                    << "  int nth_task_type = 0x4;"
                    << "  int nth_nargs_val = " << src_num_args_val << ";"
                    << "  void *nth_arg_addr[" << src_num_args_val << " + 1];"
                    << "  void **nth_arg_addr_ptr = nth_arg_addr;"
                    ;
                nth_creation_function 
                    << "     nth_create((void*)(" << outlined_function_name_decl << "), "
                    << "            &nth_task_type, &nth_num_deps, &nth_p, &nth_selfv, "
                    << "            &nth_arg_addr_ptr, &nth_nargs_ref, &nth_nargs_val" << referenced_parameters << ");"
                    ;
            }

            if (num_args_val == 0)
            {
                // Disable the declaration of the size vector to avoid a warning
                size_vector = TL::Source("");
            }

            src_num_args_val << num_args_val;

            // Groups definition
            if (!groups_clause.is_defined() && !num_threads_clause.is_defined())
            {
                groups_definition 
                    << "nth_nprocs =  nthf_cpus_actual_();"
                    ;
            }
            else if (num_threads_clause.is_defined())
            {
                ObjectList<Expression> clause_exprs = num_threads_clause.get_expression_list();

                std::string num_threads_value = clause_exprs[0].prettyprint();
                groups_definition 
                    << "nth_nprocs =" << num_threads_value << ";"
                    ;
            }
            else /* groups is defined */
            {
                groups_definition << "int nth_groups_num;"
                    ;

                ObjectList<Expression> groups_expressions = groups_clause.get_expression_list();

                switch (groups_expressions.size())
                {
                    case 1 :
                        {
                            std::string num_groups = groups_expressions[0].prettyprint();

                            groups_definition 
                                << "nth_groups_num = " << num_groups << ";"
                                << "nthf_compute_uniform_groups_(&nthf_groups_num);"
                                ;
                            break;
                        }
                    case 2 :
                        {
                            std::string num_groups = groups_expressions[0].prettyprint();
                            std::string howmany_groups = groups_expressions[1].prettyprint();

                            groups_definition
                                << "nth_groups_num = " << num_groups << ";"
                                << "nthf_compute_groups_vec_(&nthf_groups_num, " << howmany_groups << ");"
                                ;

                            break;
                        }
                    case 3 :
                        {
                            std::string num_groups = groups_expressions[0].prettyprint();
                            std::string who_groups = groups_expressions[1].prettyprint();
                            std::string howmany_groups = groups_expressions[2].prettyprint();

                            groups_definition
                                << "nth_groups_num = " << num_groups << ";"
                                << "nthf_define_groups_(&nthf_groups_num, " << who_groups << ", " << howmany_groups << ");"
                                ;

                            break;
                        }
                    default:
                        break;
                }

                groups_definition
                    << "nth_nprocs = nth_groups_num;"
                    ;
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
