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

#include "tl-omptransform.hpp"
#include "tl-taskserialize.hpp"

namespace TL
{
    namespace Nanos4
    {
        static std::string get_representative_dependence_expr(Expression expr);
        static std::string get_size_dependence_expr(Expression expr);
        static std::string get_align_dependence_expr(Expression expr);

        void OpenMPTransform::task_preorder(PragmaCustomConstruct task_construct)
        {
            if (task_construct.get_declaration().is_valid())
            {
                running_error("%s: error: function tasks are not supported in Nanos 4", 
                        task_construct.get_ast().get_locus().c_str());
            }
            // Get the related statement of this task construct
            Statement construct_body = task_construct.get_statement();

            // Get the enclosing function definition
            FunctionDefinition function_definition = task_construct.get_enclosing_function();
            // and its scope
            Scope function_scope = function_definition.get_scope();

            // FIXME
            // Data sharing information
            ObjectList<Symbol> & captureaddress_references = 
                task_construct.get_data<ObjectList<Symbol> >("captureaddress_references");
            ObjectList<Symbol> & local_references = 
                task_construct.get_data<ObjectList<Symbol> >("local_references");
            ObjectList<Symbol> & captureprivate_references = 
                task_construct.get_data<ObjectList<Symbol> >("captureprivate_references");

            // Keep this, we might need it later
            AST_t& original_code =
                task_construct.get_data<AST_t>("original_code");
            original_code = construct_body.get_ast();

            OpenMP::DataSharingEnvironment &data_sharing 
                = openmp_info->get_data_sharing(task_construct.get_ast());

            data_sharing.get_all_symbols(OpenMP::DS_SHARED, captureaddress_references);
            data_sharing.get_all_symbols(OpenMP::DS_PRIVATE, local_references);
            data_sharing.get_all_symbols(OpenMP::DS_FIRSTPRIVATE, captureprivate_references);

            // Task dependence information
            ObjectList<Expression> & input_dependences =
                task_construct.get_data<ObjectList<Expression> >("input_dependences");
            ObjectList<Expression> & output_dependences =
                task_construct.get_data<ObjectList<Expression> >("output_dependences");

            if ( Nanos4::Version::is_family("trunk") &&
                    Nanos4::Version::version >= 4202 ) 
            {
                OpenMP::DataSharingEnvironment& current_data_sharing 
                    = openmp_info->get_data_sharing(task_construct.get_ast());
                handle_dependences(task_construct, 
                        current_data_sharing,
                        input_dependences, output_dependences, 
                        task_construct, 
                        captureaddress_references,
                        captureprivate_references);
            }
        }

        void OpenMPTransform::task_postorder(PragmaCustomConstruct task_construct)
        {
            // Ignore function tasks
            if (task_construct.get_declaration().is_valid())
                return;

            // Another parallel
            num_parallels++;

            // Get the related statement of this task construct
            Statement construct_body = task_construct.get_statement();

            // Get the enclosing function definition
            FunctionDefinition function_definition = task_construct.get_enclosing_function();
            // and its scope
            Scope function_scope = function_definition.get_scope();
            // and the id-expression of the function name
            IdExpression function_name = function_definition.get_function_name();
            // create the outlined function name
            Source outlined_function_name = get_outlined_function_name(function_name);

			bool create_outline = true;
			bool task_has_key = false;
			std::string task_key_str = "";
			PragmaCustomClause task_key_clause = task_construct.get_clause("task_key");
			if (task_key_clause.is_defined())
			{
				task_has_key = true;
				task_key_str = task_key_clause.get_arguments()[0];
				
				if (task_key_map.find(task_key_str) != task_key_map.end())
				{
					create_outline = false;
				}
			}

            Source task_queueing;
            if (!create_outline)
            {
				task_queueing = task_key_map[task_key_str];
            }
            else
            {

                // Data sharing information as filled by task_preorder
                ObjectList<Symbol> & captureaddress_references = 
                    task_construct.get_data<ObjectList<Symbol> >("captureaddress_references");
                ObjectList<Symbol> & local_references = 
                    task_construct.get_data<ObjectList<Symbol> >("local_references");
                ObjectList<Symbol> & captureprivate_references = 
                    task_construct.get_data<ObjectList<Symbol> >("captureprivate_references");
                AST_t& original_code =
                    task_construct.get_data<AST_t>("original_code");

                ObjectList<Symbol> empty;
                ObjectList<OpenMP::ReductionSymbol> reduction_empty;

                ObjectList<Symbol> captured_references;
                captured_references.insert(captureaddress_references);
                captured_references.insert(captureprivate_references);

                ObjectList<ParameterInfo> parameter_info_list;

                ReplaceIdExpression replace_references  = 
                    set_replacements(function_definition,
                            construct_body,
                            captured_references, // Captured entities (firstprivate and shared)
                            local_references, // Private entities (private clause)
                            empty,
                            empty,
                            reduction_empty,
                            reduction_empty,
                            empty,
                            empty,
                            parameter_info_list,
                            /* all_shared */ true);

                // Fix parameter_info_list
                // Currently set_replacement assumes that everything will be passed BY_POINTER,
                // every entity found in captureprivate_references will be set to BY_VALUE
                //
                // The proper way should be fixing "set_replacements" one day, but already
                // takes too much parameters so a more creative approach will be required
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (captureprivate_references.contains(it->symbol))
                    {
                        it->kind = ParameterInfo::BY_VALUE;
                    }
                }

                // Do not create outline if it is serialized
                if (!task_construct.get_clause("__serialize").is_defined())
                {
                    // Get the code of the outline
                    AST_t outline_code  = get_outline_task(
                            task_construct,
                            function_definition,
                            outlined_function_name, 
                            construct_body,
                            replace_references,
                            parameter_info_list,
                            local_references);

                    // Now prepend the outline
                    function_definition.get_ast().prepend_sibling_function(outline_code);
                }

                task_queueing = task_get_spawn_code(parameter_info_list,
                        function_definition, 
                        task_construct,
                        construct_body,
                        original_code);

				if (task_has_key)
				{
					task_key_map[task_key_str] = task_queueing;
				}
            }

            // Parse the code
            AST_t task_code = task_queueing.parse_statement(task_construct.get_ast(),
                    task_construct.get_scope_link());

            // And replace the whole thing
            task_construct.get_ast().replace(task_code);
        }

        Source OpenMPTransform::task_get_spawn_code(
                ObjectList<ParameterInfo> &parameter_info_list,
                FunctionDefinition &function_definition,
                PragmaCustomConstruct &task_construct,
                Statement &construct_body,
                AST_t &original_code)
        {
            // Here the spawning code will be created
            Source task_queueing;
            Source task_arguments;
            Source task_argument_list;

            Source size_vector;

            Source task_creation_instrumentation_start,
                   task_creation_instrumentation_end,
                   task_fp_instrumentation_start,
                   task_fp_instrumentation_end;

            if (instrumentation_requested())
            {
                task_creation_instrumentation_start
                    << "mintaka_event(8000001, 1);";
                task_creation_instrumentation_end
                    << "mintaka_event(8000001, 0);";

                task_fp_instrumentation_start
                    << "mintaka_event(8000002, 1);";
                task_fp_instrumentation_end
                    << "mintaka_event(8000002, 0);";
            }

            // For each capture address entity just pass a reference to it
            int num_reference_args = 0;

            // This might be needed for nonstatic member functions
            if (is_nonstatic_member_function(function_definition))
            {
                task_argument_list.append_with_separator("this", ",");
                num_reference_args++;
            }


            ObjectList<Expression> & input_dependences =
                task_construct.get_data<ObjectList<Expression> >("input_dependences");
            ObjectList<Expression> & output_dependences =
                task_construct.get_data<ObjectList<Expression> >("output_dependences");

            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                // Only those that are passed by pointer to a shared (non-local) value
                if (it->kind != ParameterInfo::BY_POINTER)
                    continue;

                // The parameter number is the total order in the array
                // of argument pointers
                it->parameter_position = num_reference_args;

                std::string argument_name = it->argument_name;
                task_argument_list.append_with_separator(argument_name, ",");
                num_reference_args++;
            }

            // This vector will hold the sizeof's of entities passed as
            // private references
            size_vector << "size_t nth_size[] = {0";
            int num_value_args = 0;

            Source copy_construction_part;

            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                // Only those that are passed by pointer to a local value
                if (it->kind != ParameterInfo::BY_VALUE)
                    continue;

                // The parameter number is the total order in the array
                // of argument pointers. Value parameters are always after
                // the reference ones
                it->parameter_position = num_reference_args + num_value_args;

                // Add the size in the vector
                size_vector << ", sizeof(" << it->symbol.get_qualified_name(task_construct.get_scope()) << ")"
                    ;

                // A reference to the vector
                Source vector_ref;
                vector_ref << "&nth_size[" << (num_value_args + 1) << "]"
                    ;

                std::string argument_name = it->argument_name;

                // First an address with the size must be passed
                task_argument_list.append_with_separator(vector_ref.get_source(), ",");
                task_argument_list.append_with_separator(argument_name, ",");

                CXX_LANGUAGE()
                {
                    Symbol sym = it->symbol;
                    Type type = sym.get_type();
                    if (type.is_reference())
                        type = type.references_to();

                    if (type.is_class())
                    {
                        copy_construction_part
                            << "new (nth_arg_addr[" << (it->parameter_position + 1) << "])" 
                            << type.get_declaration(task_construct.get_scope(), "")
                            << "(" << sym.get_qualified_name(task_construct.get_scope()) << ");"
                            ;
                    }
                }

                num_value_args++;
            }

            size_vector << "};"
                ;

            if (num_value_args == 0)
            {
                // If no value args are passed do not declare the size vector
                size_vector = TL::Source("");
            }

            // A comma is only needed when the parameter list is non empty
            if (!task_argument_list.empty())
            {
                task_arguments << ", " << task_argument_list;
            }

            // 'untied' clause support
            Source task_type;
            if (task_construct.get_clause("untied").is_defined())
            {
                task_type << "NTH_DTYPE_TEAM";
            }
            else
            {
                task_type << "NTH_DTYPE_TEAM_DO_LOCAL";
            }

            // This is the code that will be executed if the task cannot be created
            // (i.e. NTH_CANNOT_ALLOCATE_TASK is returned)
            Source fallback_capture_values;
            Source fallback_arguments, serialized_arguments;

            ReplaceSrcIdExpression fallback_replacements(task_construct.get_scope_link());

            // This might be needed for nonstatic member functions
            if (is_nonstatic_member_function(function_definition))
            {
                fallback_arguments.append_with_separator("this", ",");
            }

            // Capture address entities are easy, just pass the vector
            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                if (it->kind != ParameterInfo::BY_POINTER)
                    continue;

                fallback_arguments.append_with_separator(it->argument_name, ",");
                serialized_arguments.append_with_separator(it->argument_name, ",");
            }

            // For capture value we will be passing pointers to local copies
            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                if (it->kind != ParameterInfo::BY_VALUE)
                    continue;

                Symbol sym = it->symbol;
                Type type = sym.get_type();

                if (type.is_reference())
                {
                    type = type.references_to();
                }

                if (!type.is_array())
                {
                    C_LANGUAGE()
                    {
                        fallback_capture_values
                            << type.get_declaration_with_initializer(
                                    task_construct.get_scope(),
                                    "cval_" + it->symbol.get_name(),
                                    it->symbol.get_qualified_name(task_construct.get_scope())) 
                            << ";"
                            ;
                        fallback_arguments.append_with_separator("&cval_" + it->symbol.get_name(), ",");
                        serialized_arguments.append_with_separator("&" + it->symbol.get_qualified_name(task_construct.get_scope()), ",");

                        fallback_replacements.add_replacement(it->symbol, "cval_" + it->symbol.get_name());
                    }
                    CXX_LANGUAGE()
                    {
                        if (type.is_class())
                        {
                            std::string type_name = type.get_declaration(
                                    task_construct.get_scope(),
                                    "");

                            type = type.get_reference_to();

                            fallback_capture_values
                                << type.get_declaration_with_initializer(
                                        task_construct.get_scope(),
                                        "cval_" + it->symbol.get_name(),
                                        "*(new " + type_name + "(" + it->symbol.get_qualified_name(task_construct.get_scope()) + "))") 
                                << ";"
                                ;
                            fallback_arguments.append_with_separator("&cval_" + it->symbol.get_name(), ",");
                            serialized_arguments.append_with_separator(it->symbol.get_qualified_name(task_construct.get_scope()), ",");

                            fallback_replacements.add_replacement(it->symbol, "cval_" + it->symbol.get_name());
                        }
                        else
                        {
                            fallback_capture_values
                                << type.get_declaration_with_initializer(
                                        task_construct.get_scope(),
                                        "cval_" + it->symbol.get_name(),
                                        it->symbol.get_qualified_name(task_construct.get_scope())) 
                                << ";"
                                ;
                            fallback_arguments.append_with_separator("&cval_" + it->symbol.get_name(), ",");
                            serialized_arguments.append_with_separator("&" + it->symbol.get_qualified_name(task_construct.get_scope()), ",");

                            fallback_replacements.add_replacement(it->symbol, "cval_" + it->symbol.get_name());
                        }
                    }
                }
                else
                {
                    Source src_array_copy = array_copy(type, "cval_" + it->symbol.get_name(),
                            it->symbol.get_qualified_name(task_construct.get_scope()), 0);

                    fallback_capture_values
                        << type.get_declaration(task_construct.get_scope(),
                                "cval_" + it->symbol.get_name())
                        << ";"
                        << src_array_copy
                        ;
                    fallback_arguments.append_with_separator("cval_" + it->symbol.get_name(), ",");
                    serialized_arguments.append_with_separator(it->symbol.get_qualified_name(task_construct.get_scope()), ",");

                    fallback_replacements.add_replacement(it->symbol, "cval_" + it->symbol.get_name());
                }
            }

            Source outlined_function_reference;
            Source selector_cast;

            outlined_function_reference 
                << get_outline_function_reference(function_definition, parameter_info_list, /* team_parameter */ false);

            Source increment_task_level;
            Source decrement_task_level;
            Source file_params;

            std::string file_name;
            // int file_line;
            std::string mangled_function_name;

            Source serialized_code;
            if (serialized_functions_info.valid())
            {
                Source duplicated_code_src;
                duplicated_code_src
                    << "{"
                    << original_code.prettyprint()
                    << "}"
                    ;

                AST_t duplicated_code_tree = duplicated_code_src.parse_statement(original_code,
                        construct_body.get_scope_link());

                // Traversal
                DepthTraverse depth_traverse;

                // Remove any OpenMP vestige in the original code
                AnyOpenMPConstructButSynchronizations any_openmp_construct_pred(construct_body.get_scope_link());
                RemoveOpenMP remove_openmp_traverse_functor;
                depth_traverse.add_predicate(any_openmp_construct_pred, remove_openmp_traverse_functor);

                // And fix function calls
                PredicateAttr function_call_pred(LANG_IS_FUNCTION_CALL) ;
                FixFunctionCalls fix_function_calls(serialized_functions_info->serialized_functions);
                depth_traverse.add_predicate(function_call_pred, fix_function_calls);

                depth_traverse.traverse(duplicated_code_tree, construct_body.get_scope_link());

                if (serialize_with_data_env)
                {
                    serialized_code
                        << "{"
                        << fallback_capture_values
                        << fallback_replacements.replace(duplicated_code_tree)
                        << "}"
                        ;
                }
                else
                {
                    serialized_code
                        << duplicated_code_tree.prettyprint();
                }
            }
            else
            {
                if (!allow_inlining_of_outlines)
                {
                    serialized_code
                        << "{"
                        << comment("Call of the outline function")
                        << "(" << outlined_function_reference << ")" << "(" << serialized_arguments << ");"
                        << "}"
                        ;
                }
                else
                {
                    if (serialize_with_data_env)
                    {
                        serialized_code
                            << "{"
                            << comment("Full inlining of serialized path")
                            << fallback_capture_values
                            << fallback_replacements.replace(construct_body) 
                            << "}"
                            ;
                    }
                    else
                    {
                        serialized_code
                            << "{"
                            << construct_body.prettyprint()
                            << "}"
                            ;
                    }
                }
            }

            increment_task_level <<  "nth_task_ctx_t nth_ctx;";
            increment_task_level <<  "nth_push_task_ctx(&nth_ctx);"
                ;
            decrement_task_level <<  "nth_pop_task_ctx();"
                ;

            Source cutoff_call;
            if ( Nanos4::Version::is_family("trunk") &&
                    Nanos4::Version::version >= 4201 ) 
            {
                cutoff_call 
                    <<    comment("_cf_n is a return value")
                    <<    "int _cf_n;"
                    <<    "nth_cutoff = nth_cutoff_create(&_cf_n);";
            }
            else
            {
                cutoff_call << "nth_cutoff = nth_cutoff_create();"
                    ;
            }

            Source cutoff_code;
            cutoff_code
                <<    "nth_cutoff_res_t nth_cutoff = NTH_CUTOFF_IMMEDIATE;"
                ;

            PragmaCustomClause if_clause = task_construct.get_clause("if");
            Source if_clause_check;

            if (if_clause.is_defined())
            {
                Source else_instrument;
                cutoff_code
                    << comment("Check whether we have to create a task or just run it immediately")
                    << "if (" << if_clause.get_expression_list()[0].prettyprint() << ")"
                    << "{"
                    <<    cutoff_call
                    << "}"
                    << else_instrument
                    ;

                if (instrumentation_requested())
                {
                    else_instrument
                        << "nth_cutoff = 5;";
                }
            }
            else
            {
                cutoff_code
                    << cutoff_call;
            }

            // Final clause
            PragmaCustomClause final_clause = task_construct.get_clause("final");
            Source final_code, final_create, final_immediate;
            if (final_clause.is_defined())
            {

                final_code
                    << "char _task_is_final = (" << final_clause.get_expression_list()[0].prettyprint() << ");"
                    ;

                final_create
                    << "if (_task_is_final) nth_finalize(nth);"
                    ;

                final_immediate
                    << "if (_task_is_final) nth_finalize(nth_self());"
                    ;
            }

            // input dependences 
            // Task dependence information from task_preorder

            Source dependences_common;
            Source inputs_prologue, inputs_epilogue, inputs_immediate;
            Source outputs_prologue, outputs_epilogue, outputs_immediate;
            
            if (!input_dependences.empty()
                    || !output_dependences.empty())
            {
                dependences_common
                    << "struct nth_task_ctx_t * nth_self_task_ctx = nth_self()->task_ctx;"
                    ;
            }

            if (!input_dependences.empty())
            {
                ObjectList<Expression>::iterator it_begin = input_dependences.begin();
                ObjectList<Expression>::iterator it_end = input_dependences.end();
                ObjectList<Expression>::iterator it;

                int num_dep = 0;
                for ( it = it_begin; it != it_end; it++ ) 
                {
                    Expression &expr = *it;
                    Symbol sym = handle_dep_expr(expr);
                    Type pointer_type = sym.get_type().get_pointer_to();

                    // Find the position of the related symbol in the arguments
                    bool found = false;
                    int referred_num_ref = -1;

                    for (ObjectList<ParameterInfo>::iterator it2 = parameter_info_list.begin();
                            it2 != parameter_info_list.end();
                            it2++)
                    {
                        if (it2->symbol == sym)
                        {
                            found = true;
                            referred_num_ref = it2->parameter_position;
                            break;
                        }
                    }

                    if (!found)
                    {
                        internal_error("Not found proper symbol %s", sym.get_name().c_str());
                    }

                    inputs_prologue
                        << comment("Find an output to connect input '" + expr.prettyprint() + "'")
                        << "nth_outdep_t *connect_" << sym.get_name() << "_dep_" << num_dep << " = nth_find_output_dep_in_scope(nth_self_task_ctx,"
                        << get_representative_dependence_expr(expr)
                        << ");"
                        ;

                    inputs_epilogue
                        << comment("Register input dependence '" + expr.prettyprint() + "'")
                        << "nth_add_input_to_task(nth_self_task_ctx, nth->task_ctx, "
                        // NULL means let Nanos allocate it
                        <<                    "(void*)0,"
                        <<                    "connect_" << sym.get_name() << "_dep_" << num_dep << ", "
                        <<                    get_representative_dependence_expr(expr) << ", "
                        <<                    get_size_dependence_expr(expr) << ", "
                        <<                    get_align_dependence_expr(expr) << ", "
                        <<                    "nth_arg_addr[" << (referred_num_ref + 1) << "]);"
                        ;

                    inputs_immediate
                        << comment("Satisfy input dependence '" + expr.prettyprint() + "'")
                        << "nth_indep_t nth_" << sym.get_name() << "_indep_" << num_dep << ";"
                        << "nth_satisfy_input_dep(nth_self_task_ctx, &nth_ctx, "
                        <<         "&nth_" << sym.get_name() << "_indep_" << num_dep << ", "
                        <<         "connect_" << sym.get_name() << "_dep_" << num_dep << ", "
                        <<         get_representative_dependence_expr(expr) << ", "
                        <<         get_size_dependence_expr(expr) << ", "
                        <<         get_align_dependence_expr(expr) 
                        << ");"
                        ;

                    num_dep++;
                }
            }

            // output dependences 
            if (!output_dependences.empty())
            {
                ObjectList<Expression>::iterator it_begin = output_dependences.begin();
                ObjectList<Expression>::iterator it_end = output_dependences.end();
                ObjectList<Expression>::iterator it;

                for ( it = it_begin; it != it_end; it++ ) 
                {
                    Expression &expr = *it;
                    Symbol sym = handle_dep_expr(expr);

                    // Find the position of the related symbol in the arguments
                    bool found = false;
                    int referred_num_ref = -1;

                    for (ObjectList<ParameterInfo>::iterator it2 = parameter_info_list.begin();
                            it2 != parameter_info_list.end();
                            it2++)
                    {
                        if (it2->symbol == sym)
                        {
                            found = true;
                            referred_num_ref = it2->parameter_position;
                            break;
                        }
                    }

                    if (!found)
                    {
                        internal_error("Not found proper symbol %s", sym.get_name().c_str());
                    }

                    outputs_epilogue 
                        << comment("Register output dependence of symbol '" + expr.prettyprint() + "'")
                        << "nth_add_output_to_task(nth_self_task_ctx, nth->task_ctx, "
                        // NULL means let Nanos allocate it
                        <<                    "(void*)0,"
                        <<                    get_representative_dependence_expr(expr) << ", "
                        <<                    get_size_dependence_expr(expr) << ", "
                        <<                    get_align_dependence_expr(expr) << ", "
                        <<                    "nth_arg_addr[" << (referred_num_ref + 1) << "]);"
                        ;

                    outputs_immediate
                        << comment("Notify we have an output dependence of symbol '" + expr.prettyprint() + "'")
                        << "nth_shadow_output_dep(nth_self_task_ctx, "
                        <<        get_representative_dependence_expr(expr)
                        << ");"
                        ;
                }
            }

            Source immediate_instrumentation;

            if (!task_construct.get_clause("__serialize").is_defined())
            {
                task_queueing
                    << "if (NTH_MYSELF->task_ctx->final)"
                    // Serialized code already has braces
                    <<      serialized_code
                    << "else"
                    << "{"
                    // FIXME - I'd like there was a NTH_CUTOFF_INVALID (with zero
                    // value but this would break current interface)
                    <<    cutoff_code
                    <<    "switch (nth_cutoff)"
                    <<    "{"
                    <<      "case NTH_CUTOFF_CREATE:"
                    <<      "{"
                    <<          final_code
                    <<          comment("Create the task")
                    <<          "nth_desc * nth;"
                    <<          "int nth_type = " << task_type << ";"
                    <<          "int nth_ndeps = 0;"
                    <<          "int nth_vp = 0;"
                    <<          "nth_desc_t* nth_succ = (nth_desc_t*)0;"
                    <<          "int nth_nargs_ref = " << num_reference_args << ";"
                    <<          "int nth_nargs_val = " << num_value_args << ";"
                    <<          "void* nth_arg_addr[" << (num_value_args + num_reference_args) << " + 1];"
                    <<          "void** nth_arg_addr_ptr = &nth_arg_addr[1];"
                    <<          dependences_common
                    <<          inputs_prologue
                    <<          outputs_prologue
                    <<          size_vector
                    <<          "nth = nth_create_task_ci((void*)(" << outlined_function_reference << "), "
                    <<                 "&nth_type, &nth_ndeps, &nth_vp, &nth_succ, NTH_CI_ALL,"
                    <<                 "&nth_arg_addr_ptr, &nth_nargs_ref, &nth_nargs_val"  
                    <<                 task_arguments << ");"
                    <<          outputs_epilogue
                    <<          inputs_epilogue
                    <<          copy_construction_part
                    <<          final_create
                    <<          "nth_submit(nth);"
                    <<          "break;" 
                    <<      "}"
                    <<      "case NTH_CUTOFF_SERIALIZE:"
                    <<	        "__builtin_abort();"
                    <<          "break;"
                    <<      immediate_instrumentation
                    <<      "case NTH_CUTOFF_IMMEDIATE:"
                    <<      "{"
                    <<          comment("Run the task inline")
                    <<          final_code
                    <<          fallback_capture_values
                    <<          dependences_common
                    <<          inputs_prologue
                    <<          increment_task_level
                    <<          final_immediate
                    <<          outputs_immediate
                    <<          inputs_immediate
                    <<          "(" << outlined_function_reference << ")" << "(" << fallback_arguments << ");"
                    <<          decrement_task_level
                    <<          "break;"
                    <<      "}"
                    <<      "default: { " << comment("Invalid cutoff") << "__builtin_abort(); break; }"
                    <<    "}"
                    << "}"
                    ;

                if (instrumentation_requested())
                {
                    immediate_instrumentation
                        <<      "case 5:"
                        <<      "{"
                        <<          comment("Run the task inline plus instrumentation")
                        <<          task_fp_instrumentation_start
                        <<          fallback_capture_values
                        <<          task_fp_instrumentation_end
                        <<          task_creation_instrumentation_start
                        <<          final_code
                        <<          dependences_common
                        <<          inputs_prologue
                        <<          increment_task_level
                        <<          final_immediate
                        <<          outputs_immediate
                        <<          inputs_immediate
                        <<          task_creation_instrumentation_end
                        <<          "(" << outlined_function_reference << ")" << "(" << fallback_arguments << ");"
                        <<          task_creation_instrumentation_start
                        <<          decrement_task_level
                        <<          task_creation_instrumentation_end
                        <<          "break;"
                        <<      "}"
                        ;
                }
            }
            else
            {
                if (!serialize_with_data_env)
                {
                    task_queueing 
                        << "{"
                        << comment("Serialized task WITHOUT data environment")
                        << task_construct.get_statement()
                        << "}"
                        ;
                }
                else
                {
                    task_queueing 
                        << "{"
                        << comment("Serialized task with data environment")
                        << fallback_capture_values
                        << fallback_replacements.replace(task_construct.get_statement())
                        << "}"
                        ;
                }
            }

            return task_queueing;
        }

        AST_t OpenMPTransform::get_outline_task(
                PragmaCustomConstruct &construct,
                FunctionDefinition function_definition,
                Source outlined_function_name,
                Statement construct_body,
                ReplaceIdExpression replace_references,
                ObjectList<ParameterInfo> parameter_info_list,
                ObjectList<Symbol> local_references
                )
        {
            ObjectList<OpenMP::ReductionSymbol> reduction_references;

            Source outline_parallel;
            Source parallel_body;

            outline_parallel = get_outline_common(
                    function_definition,
                    parallel_body, // The body of the outline
                    outlined_function_name,
                    parameter_info_list, 
                    construct);

            // Replace references using set "replace_references" over construct body
            Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

            ObjectList<Symbol> empty;
            ObjectList<OpenMP::ReductionSymbol> reduction_empty;
            Source private_declarations = get_privatized_declarations(
                    construct,
                    local_references,
                    empty,
                    empty,
                    reduction_empty,
                    empty,
                    parameter_info_list
                    );

            Source destructor_calls;
            invoke_destructors(parameter_info_list, destructor_calls);

            parallel_body
                << private_declarations
                << modified_parallel_body_stmt.prettyprint()
                << destructor_calls
                ;
            
            return finish_outline(function_definition, 
                    outline_parallel, 
                    parameter_info_list, 
                    /* team_parameter */ false);
        }

        /* 
           Subset of expressions accepted as dependence expressions

           dep_expr -> scalar_dep_expr
                    -> scalar_dep_expr '[' expr : expr ']'

           scalar_dep_expr -> identifier
                           -> scalar_dep_expr '[' expr ']'
                           -> scalar_dep_expr '.' identifier
                           -> scalar_dep_expr '->' identifier
         */

        Symbol OpenMPTransform::handle_dep_expr(Expression expr)
        {
            // scalar_dep_expr '[' expr : expr ']'
            if (expr.is_array_section())
            {
                Expression item = expr.array_section_item();
                return handle_scalar_dep_expr(item);
            }
            // scalar_dep_expr
            else
            {
                return handle_scalar_dep_expr(expr);
            }
        }

        Symbol OpenMPTransform::handle_scalar_dep_expr(Expression expr)
        {
            // identifier
            if (expr.is_id_expression())
            {
                IdExpression id_expr = expr.get_id_expression();
                Symbol sym = id_expr.get_symbol();

                return sym;
            }
            // scalar_dep_expr '[' expr ']'
            else if (expr.is_array_subscript())
            {
                Expression subscripted = expr.get_subscripted_expression();
                return handle_scalar_dep_expr(subscripted);
            }
            // scalar_dep_expr '.' identifier 
            // scalar_dep_expr '-> identifier 
            else if (expr.is_member_access()
                    || expr.is_pointer_member_access())
            {
                // FIXME - This is wrong! 
                // We want the field as a Symbol but this is not currently possible
                Expression accessed = expr.get_accessed_entity();
                return handle_scalar_dep_expr(accessed);
            }
            else
            {
                std::cerr << expr.get_ast().get_locus() << ": warning: ignoring invalid dependence expression specification '" 
                    << expr.prettyprint() << "'" << std::endl;
                return Symbol(0);
            }
        }

        void OpenMPTransform::handle_dependences(PragmaCustomConstruct construct,
                OpenMP::DataSharingEnvironment& data_sharing,
                ObjectList<Expression> &input_dependences,
                ObjectList<Expression> &output_dependences,
                PragmaCustomConstruct &task_construct,
                ObjectList<Symbol>& captureaddress_references,
                ObjectList<Symbol>& captureprivate_references)
        {
            // input(x) clause support
            // input variables must be shared or firstprivate
            PragmaCustomClause input = construct.get_clause("input");
            if (input.is_defined()) 
            {
                ObjectList<Expression> dep_list = input.get_expression_list();

                if (dep_list.empty()) 
                {
                    std::cerr << input.get_ast().get_locus() <<
                        ": warning: empty input clause" << std::endl;
                }

                for (ObjectList<Expression>::iterator it = dep_list.begin();
                        it != dep_list.end();
                        it++)
                {
                    Expression &expr = *it;
                    Symbol sym = handle_dep_expr(expr);

                    if (!sym.is_valid())
                        continue;
                    
                    Type type = sym.get_type();

                    if (type.is_pointer()
                            && (expr.is_array_section()
                                || expr.is_array_subscript()))
                    {
                        // If we are passing an array section built after a pointer
                        // the pointer itself must be capturevalued.
                        data_sharing.set(sym, OpenMP::DS_FIRSTPRIVATE); 
                        captureprivate_references.insert(sym);
                    }
                    else
                    {
                        // Otherwise, capture its address (even if it is an
                        // array since they are already properly handled)
                        data_sharing.set(sym, OpenMP::DS_SHARED);
                        captureaddress_references.insert(sym);
                    }

                    // Add this as an input dependence
                    input_dependences.append(expr);
                }
            }

            // output(x) clause support
            // output variables must be shared
            PragmaCustomClause output = construct.get_clause("output");
            if (output.is_defined()) 
            {
                ObjectList<Expression> dep_list = output.get_expression_list();

                if (dep_list.empty()) 
                {
                    std::cerr << output.get_ast().get_locus() <<
                        ": warning: empty output clause" << std::endl;
                }

                for (ObjectList<Expression>::iterator it = dep_list.begin();
                        it != dep_list.end();
                        it++)
                {
                    Expression &expr = *it;
                    Symbol sym = handle_dep_expr(expr);

                    if (!sym.is_valid())
                        continue;

                    Type type = sym.get_type();

                    if (type.is_pointer()
                            && (expr.is_array_section()
                                || expr.is_array_subscript()))
                    {
                        // If we are passing an array section built after a pointer
                        // the pointer itself must be capturevalued.
                        data_sharing.set(sym, OpenMP::DS_FIRSTPRIVATE);
                        captureprivate_references.insert(sym);
                    }
                    else
                    {
                        // Otherwise, capture its address (even if it is an
                        // array since they are already properly handled)
                        data_sharing.set(sym, OpenMP::DS_SHARED);
                        captureaddress_references.insert(sym);
                    }

                    // Add this as an output dependence
                    output_dependences.append(expr);
                }
            }

            // inout(x) clause support
            // inout variables must be shared
            PragmaCustomClause inout = construct.get_clause("inout");
            if (inout.is_defined()) 
            {
                ObjectList<Expression> dep_list = inout.get_expression_list();

                if (dep_list.empty()) 
                {
                    std::cerr << inout.get_ast().get_locus() <<
                        ": warning: empty inout clause" << std::endl;
                }

                for (ObjectList<Expression>::iterator it = dep_list.begin();
                        it != dep_list.end();
                        it++)
                {
                    Expression &expr = *it;
                    Symbol sym = handle_dep_expr(expr);

                    if (!sym.is_valid())
                        continue;

                    Type type = sym.get_type();

                    if (type.is_pointer()
                            && (expr.is_array_section()
                                || expr.is_array_subscript()))
                    {
                        // If we are passing an array section built after a pointer
                        // the pointer itself must be capturevalued.
                        data_sharing.set(sym, OpenMP::DS_FIRSTPRIVATE);
                        captureprivate_references.insert(sym);
                    }
                    else
                    {
                        // Otherwise, capture its address (even if it is an
                        // array since they are already properly handled)
                        data_sharing.set(sym, OpenMP::DS_SHARED);
                        captureaddress_references.insert(sym);
                    }

                    // Insert the input and output dependence
                    input_dependences.append(expr);
                    output_dependences.append(expr);
                }
            }
        }

        static std::string get_representative_dependence_expr(Expression expr)
        {
            if (expr.is_array_section())
            {
                Expression array_section_lower = expr.array_section_lower();
                // Expression array_section_upper = expr.array_section_upper();
                Expression array_section_item = expr.array_section_item();

                return "&(" + array_section_item.prettyprint() + "[" + array_section_lower.prettyprint() + "])";
            }
            else 
            {
                Type t = expr.get_type();
                // Naming an array type here means the whole array
                if (t.is_array())
                {
                    return "&(" + expr.prettyprint() + "[0])";
                }
                else
                {
                    return "&(" + expr.prettyprint() + ")";
                }
            }

            return "<<invalid-expression(representative)>>";
        }

        static std::string get_size_dependence_expr(Expression expr)
        {
            if (expr.is_array_section())
            {
                Expression array_section_lower = expr.array_section_lower();
                Expression array_section_upper = expr.array_section_upper();
                Expression array_section_item = expr.array_section_item();

                return "(((" 
                    + array_section_upper.prettyprint() 
                    + ")-(" 
                    + array_section_lower.prettyprint() 
                    + ") + 1) * sizeof ( *" + array_section_item.prettyprint() + "))";
            }
            else
            {
                Type t = expr.get_type();
                // Naming an array type here means the whole array
                if (t.is_array())
                {
                    return "((" 
                        + t.array_dimension().prettyprint() 
                        + ") * sizeof( * " 
                        + expr.prettyprint() + "))";
                }
                else
                {
                    return "sizeof(" + expr.prettyprint() + ")";
                }
            }

            return "<<invalid-expression(size)>>";
        }

        static std::string get_align_dependence_expr(Expression expr)
        {
            if (expr.is_array_section())
            {
                Symbol sym = OpenMPTransform::handle_dep_expr(expr);
                Expression array_section_lower = expr.array_section_lower();
                // Expression array_section_upper = expr.array_section_upper();
                Expression array_section_item = expr.array_section_item();

                std::string base_address = "";
                if (sym.get_type().is_array())
                {
                    base_address = "(" + sym.get_name() + ")";
                }
                else
                {
                    base_address = "(&" + sym.get_name() + ")";
                }

                return "((intptr_t)(&(" + array_section_item.prettyprint() + "[" + array_section_lower.prettyprint() + "]))"
                    + "-"
                    + "(intptr_t)" + base_address
                    + ")";
            }
            else 
            {
                return "0";
            }

            return "<<invalid-expression(align)>>";
        }
    }
}
