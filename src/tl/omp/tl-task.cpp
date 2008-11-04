/*
   Mercurium C/C++ Compiler
   Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
   Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
   Universitat Politecnica de Catalunya

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
   */
#include "tl-omptransform.hpp"
#include "tl-taskserialize.hpp"

namespace TL
{
    namespace Nanos4
    {
        // Defined at the end of the file
        static Symbol dependence_expr_to_sym(Expression expr);
        static std::string get_representative_dependence_expr(Expression expr);
        static std::string get_size_dependence_expr(Expression expr);
        static std::string get_align_dependence_expr(Expression expr);

        void OpenMPTransform::task_preorder(OpenMP::CustomConstruct task_construct)
        {
            // Get the directive of the task construct
            OpenMP::Directive directive = task_construct.directive();

            // Get the related statement of this task construct
            Statement construct_body = task_construct.body();

            // Get the enclosing function definition
            FunctionDefinition function_definition = task_construct.get_enclosing_function();
            // and its scope
            Scope function_scope = function_definition.get_scope();

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

            task_compute_explicit_data_sharing(directive, captureaddress_references,
                    local_references,
                    captureprivate_references,
                    function_scope,
                    function_definition,
                    task_construct);

            task_compute_implicit_data_sharing(directive, captureaddress_references,
                    local_references,
                    captureprivate_references,
                    function_scope,
                    function_definition,
                    construct_body,
                    task_construct);
            
            // Task dependence information
            ObjectList<Expression> & input_dependences =
                task_construct.get_data<ObjectList<Expression> >("input_dependences");
            ObjectList<Expression> & output_dependences =
                task_construct.get_data<ObjectList<Expression> >("output_dependences");

            if ( Nanos4::Version::is_family("trunk") &&
                    Nanos4::Version::version >= 4202 ) 
            {
                handle_dependences(directive, 
                        input_dependences, output_dependences, 
                        task_construct, 
                        captureaddress_references);
            }
        }

        void OpenMPTransform::task_postorder(OpenMP::CustomConstruct task_construct)
        {
            // Another parallel
            num_parallels++;

            // Get the directive of the task construct
            OpenMP::Directive directive = task_construct.directive();

            // Get the related statement of this task construct
            Statement construct_body = task_construct.body();

            // Get the enclosing function definition
            FunctionDefinition function_definition = task_construct.get_enclosing_function();
            // and its scope
            Scope function_scope = function_definition.get_scope();
            // and the id-expression of the function name
            IdExpression function_name = function_definition.get_function_name();
            // create the outlined function name
            Source outlined_function_name = get_outlined_function_name(function_name);

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
                        directive,
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

            Source task_queueing;
            task_queueing = task_get_spawn_code(parameter_info_list,
                    function_definition, 
                    task_construct,
                    directive,
                    construct_body,
                    original_code);

            // Parse the code
            AST_t task_code = task_queueing.parse_statement(task_construct.get_ast(),
                    task_construct.get_scope_link());

            // And replace the whole thing
            task_construct.get_ast().replace(task_code);
        }

        Source OpenMPTransform::task_get_spawn_code(
                ObjectList<ParameterInfo> &parameter_info_list,
                FunctionDefinition &function_definition,
                OpenMP::Construct &task_construct,
                OpenMP::Directive &directive,
                Statement &construct_body,
                AST_t &original_code)
        {
            // Here the spawning code will be created
            Source task_queueing;
            Source task_arguments;
            Source task_argument_list;

            Source size_vector;

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
            if (directive.custom_clause("untied").is_defined())
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
            Source fallback_arguments;

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

                if (!type.is_array())
                {
                    fallback_capture_values
                        << type.get_declaration_with_initializer(
                                task_construct.get_scope(),
                                "cval_" + it->symbol.get_name(),
                                it->symbol.get_qualified_name(task_construct.get_scope())) 
                        << ";"
                        ;
                    fallback_arguments.append_with_separator("&cval_" + it->symbol.get_name(), ",");
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
                }
            }

            Source outlined_function_reference;
            Source selector_cast;

            outlined_function_reference 
                << get_outline_function_reference(function_definition, parameter_info_list, /* team_parameter */ false);

            Source instrument_code_task_creation;

            Source increment_task_level;
            Source decrement_task_level;
            Source file_params;

            std::string file_name;
            // int file_line;
            std::string mangled_function_name;

            Source serialized_code;

            // If we have such information fill serialized branch, otherwise don't do anything
            // and exploit the fall-through of the switch
            if (serialized_functions_info)
            {
                Source duplicated_code_src;
                duplicated_code_src
                    << comment("Fall-through serial!")
                    << "{"
                    << original_code.prettyprint()
                    << "break;"
                    << "}"
                    ;

                AST_t duplicated_code_tree = duplicated_code_src.parse_statement(original_code,
                        construct_body.get_scope_link());

                // Traversal
                DepthTraverse depth_traverse;

                // Remove any OpenMP vestige in the original code
                AnyOpenMPConstruct any_openmp_construct_pred;
                RemoveOpenMP remove_openmp_traverse_functor;
                depth_traverse.add_predicate(any_openmp_construct_pred, remove_openmp_traverse_functor);

                // And fix function calls
                PredicateAST<LANG_IS_FUNCTION_CALL> function_call_pred;
                FixFunctionCalls fix_function_calls(serialized_functions_info->serialized_functions);
                depth_traverse.add_predicate(function_call_pred, fix_function_calls);

                depth_traverse.traverse(duplicated_code_tree, construct_body.get_scope_link());

                serialized_code
                    << duplicated_code_tree.prettyprint();
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

            OpenMP::Clause if_clause = directive.if_clause();
            Source if_clause_check;

            if (if_clause.is_defined())
            {
                cutoff_code
                    << comment("Check whether we have to create a task or just run it immediately")
                    << "if (" << if_clause.get_expression_list()[0].prettyprint() << ")"
                    << "{"
                    <<    cutoff_call
                    << "}"
                    ;
            }
            else
            {
                cutoff_code
                    << cutoff_call;
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

                for ( it = it_begin; it != it_end; it++ ) 
                {
                    Expression &expr = *it;
                    Symbol sym = dependence_expr_to_sym(expr);
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
			<< comment("Find an output to connect input '" + sym.get_name() + "'")
			<< "nth_outdep_t *connect_" << sym.get_name() << "_dep = nth_find_output_dep_in_scope(nth_self_task_ctx,"
			<< get_representative_dependence_expr(expr)
			<< ");"
			;

                    inputs_epilogue
                        << comment("Register input dependency of symbol '" + sym.get_name() + "'")
                        << "nth_add_input_to_task(nth_self_task_ctx, nth->task_ctx, "
                        // NULL means let Nanos allocate it
                        <<                    "(void*)0,"
			<< 		      "connect_" << sym.get_name() << "_dep,"
                        <<                    get_representative_dependence_expr(expr) << ", "
                        <<                    get_size_dependence_expr(expr) << ", "
                        <<                    get_align_dependence_expr(expr) << ", "
                        <<                    "nth_arg_addr[" << (referred_num_ref + 1) << "]);"
                        ;

                    inputs_immediate
                        << comment("Satisfy input dependence of symbol '" + sym.get_name() + "'")
                        << "nth_indep_t nth_" << sym.get_name() << "_indep;"
                        << "nth_satisfy_input_dep(nth_self_task_ctx, &nth_ctx, "
                        <<         "&nth_" << sym.get_name() << "_indep, "
			<<	   "connect_" << sym.get_name() << "_dep,"
                        <<         get_representative_dependence_expr(expr) << ", "
                        <<         get_size_dependence_expr(expr) << ", "
                        <<         get_align_dependence_expr(expr) 
                        << ");"
                        ;
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
                    Symbol sym = dependence_expr_to_sym(expr);

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
                        << comment("Register output dependency of symbol '" + sym.get_name() + "'")
                        << "nth_add_output_to_task(nth_self_task_ctx, nth->task_ctx, "
                        // NULL means let Nanos allocate it
                        <<                    "(void*)0,"
                        <<                    get_representative_dependence_expr(expr) << ", "
                        <<                    get_size_dependence_expr(expr) << ", "
                        <<                    get_align_dependence_expr(expr) << ", "
                        <<                    "nth_arg_addr[" << (referred_num_ref + 1) << "]);"
                        ;

                    outputs_immediate
                        << comment("Notify we have an output dependency of symbol '" + sym.get_name() + "'")
                        << "nth_shadow_output_dep(nth_self_task_ctx, "
                        <<        get_representative_dependence_expr(expr)
                        << ");"
                        ;
                }
            }

            // FIXME: Instrumentation is still missing!!!
            task_queueing
                << "{"
                // FIXME - I'd like there was a NTH_CUTOFF_INVALID (with zero
                // value but this would break current interface)
                <<    cutoff_code
                <<    "switch (nth_cutoff)"
                <<    "{"
                <<      "case NTH_CUTOFF_CREATE:"
                <<      "{"
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
                <<          "nth_submit(nth);"
                <<          "break;" 
                <<      "}"
                <<      "case NTH_CUTOFF_SERIALIZE:"
                <<      serialized_code
                <<      "case NTH_CUTOFF_IMMEDIATE:"
                <<      "{"
                <<          comment("Run the task inline")
                <<          fallback_capture_values
                <<          dependences_common
                <<          inputs_prologue
                <<          increment_task_level
                <<		    outputs_immediate
                <<		    inputs_immediate
                <<          "(" << outlined_function_reference << ")" << "(" << fallback_arguments << ");"
                <<          decrement_task_level
                <<          "break;"
                <<      "}"
                <<      "default: { " << comment("Invalid cutoff") << "__builtin_abort(); break; }"
                <<    "}"

                << "}"
                ;

            return task_queueing;
        }

        AST_t OpenMPTransform::get_outline_task(
                OpenMP::Construct &construct,
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
                    parameter_info_list);

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

        // Data sharing computation for tasks.
        //
        // Tasks have slightly different requirements to other OpenMP constructs so their code
        // can't be merged easily

        void OpenMPTransform::task_compute_explicit_data_sharing(
                OpenMP::Directive &directive,
                ObjectList<Symbol> &captureaddress_references,
                ObjectList<Symbol> &local_references,
                ObjectList<Symbol> &captureprivate_references,
                Scope& function_scope,
                FunctionDefinition &function_definition,
                OpenMP::Construct &task_construct)
        {
            // Get references in local clause
            OpenMP::Clause private_clause = directive.private_clause();
            ObjectList<IdExpression> local_references_in_clause = private_clause.id_expressions();
            // Those stated by the user to be local are local_references
            local_references.insert(local_references_in_clause.map(functor(&IdExpression::get_symbol)));

            OpenMP::Clause shared_clause = directive.shared_clause();

            // Get all the identifiers of the captureaddress clause
            ObjectList<IdExpression> captureaddress_references_in_clause = shared_clause.id_expressions();

            {
                // Legacy check
                ObjectList<std::string> legacy_captureaddress_names;
                legacy_captureaddress_names.append("captureaddress");
                legacy_captureaddress_names.append("capture_address");

                OpenMP::CustomClause legacy_captureaddress_clause = directive.custom_clause(legacy_captureaddress_names);

                if (legacy_captureaddress_clause.is_defined())
                {
                    std::cerr << legacy_captureaddress_clause.get_ast().get_locus() 
                        << ": warning: clauses 'captureaddress' and 'capture_address' are deprecated, instead use 'shared'"
                        << std::endl;
                    // Now get the id-expressions for backward compatibility
                    captureaddress_references_in_clause.append(legacy_captureaddress_clause.id_expressions());
                }
            }

            {
                // We discard symbols here referenced in captureaddress
                // clause that can be referenced in the outline (thus, they
                // come from an outer scope to this whole function)
                for (ObjectList<IdExpression>::iterator it = captureaddress_references_in_clause.begin();
                        it != captureaddress_references_in_clause.end();
                        it++)
                {
                    Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

                    if (!global_sym.is_valid() 
                            || global_sym != it->get_symbol()
                            || is_unqualified_member_symbol(it->get_symbol(), function_definition))
                    {
                        // If the symbol found in the function scope is not
                        // the same as the one referenced in the
                        // captureaddress it will be really
                        // 'captureaddressed', otherwise it can be
                        // referenced from the outline
                        captureaddress_references.insert(it->get_symbol());
                    }
                }
            }

            // Set the data sharing attribute 'shared' to all captureaddress
            add_data_attribute_to_list(task_construct, captureaddress_references, OpenMP::DA_SHARED);

            OpenMP::Clause captureprivate_clause = directive.firstprivate_clause();
            // Get the identifiers of the capturevalue clause
            ObjectList<IdExpression> captureprivate_references_in_clause = captureprivate_clause.id_expressions();

            // Legacy check
            {
                ObjectList<std::string> legacy_captureprivate_names;
                legacy_captureprivate_names.append("captureprivate");
                legacy_captureprivate_names.append("capture_private");
                legacy_captureprivate_names.append("capturevalue");
                legacy_captureprivate_names.append("capture_value");

                OpenMP::CustomClause legacy_captureprivate_clause 
                    = directive.custom_clause(legacy_captureprivate_names);

                if (legacy_captureprivate_clause.is_defined())
                {
                    std::cerr << legacy_captureprivate_clause.get_ast().get_locus() << ": warning: clauses 'captureprivate',"
                        << "'capture_private', 'capturevalue' and 'capture_value' are deprecated. Instead use 'firstprivate'."
                        << std::endl;

                    // Now append the found things for joy of the user (does not check for repeated things!)
                    captureprivate_references_in_clause.append(legacy_captureprivate_clause.id_expressions());
                }
            }

            // As stated by the user, everything in the clause is already
            // capturevalued (no pruning here as we did for captureaddress)
            captureprivate_references.insert(captureprivate_references_in_clause.map(functor(&IdExpression::get_symbol)));

            // Set the data sharing attribute 'firstprivate' to all captureaddress
            add_data_attribute_to_list(task_construct, captureprivate_references, OpenMP::DA_FIRSTPRIVATE);
        }

        void OpenMPTransform::task_compute_implicit_data_sharing(
                OpenMP::Directive &directive,
                ObjectList<Symbol> &captureaddress_references,
                ObjectList<Symbol> &local_references,
                ObjectList<Symbol> &captureprivate_references,
                Scope& function_scope,
                FunctionDefinition &function_definition,
                Statement& construct_body,
                OpenMP::Construct &task_construct)
        {
            // These are used later
            OpenMP::Clause shared_clause = directive.shared_clause();
            ObjectList<IdExpression> captureaddress_references_in_clause = shared_clause.id_expressions();
            OpenMP::Clause captureprivate_clause = directive.firstprivate_clause();
            ObjectList<IdExpression> captureprivate_references_in_clause = captureprivate_clause.id_expressions();
            OpenMP::Clause private_clause = directive.private_clause();
            ObjectList<IdExpression> local_references_in_clause = private_clause.id_expressions();

            OpenMP::CustomClause input_clause = directive.custom_clause("input");
            ObjectList<Expression> input_references_in_clause = input_clause.get_expression_list();
            OpenMP::CustomClause output_clause = directive.custom_clause("output");
            ObjectList<Expression> output_references_in_clause = output_clause.get_expression_list();

            // Default calculus
            OpenMP::DefaultClause default_clause = directive.default_clause();
            enum 
            {
                DK_TASK_INVALID = 0,
                DK_TASK_UNDEFINED,
                DK_TASK_SHARED,
                DK_TASK_FIRSTPRIVATE,
                DK_TASK_PRIVATE,
                DK_TASK_NONE
            } default_task_data_sharing = DK_TASK_INVALID;

            ObjectList<std::string> captureprivate_names;
            captureprivate_names.append("firstprivate");
            if (!default_clause.is_defined())
            {
                // If not given then DK_TASK_UNDEFINED will trigger a more
                // complex calculus of the data sharing involving inherited
                // attributes
                default_task_data_sharing = DK_TASK_UNDEFINED;
            }
            else if (default_clause.is_none())
            {
                default_task_data_sharing = DK_TASK_NONE;
            }
            else if (default_clause.is_private())
            {
                default_task_data_sharing = DK_TASK_PRIVATE;
            }
            else if (default_clause.is_custom(captureprivate_names))
            {
                default_task_data_sharing = DK_TASK_FIRSTPRIVATE;
            }
            else if (default_clause.is_shared())
            {
                default_task_data_sharing = DK_TASK_SHARED;
            }
            else
            {
                std::cerr << default_clause.get_ast().get_locus() << ": warning: unknown default clause '" 
                    << default_clause.prettyprint() << "'. Assuming 'default(firstprivate)'."
                    << std::endl;
                default_task_data_sharing = DK_TASK_FIRSTPRIVATE;
            }

            // Now deal with the references of the body
            {
                // Get all id-expressions in the body construct
                ObjectList<IdExpression> references_body_all
                    = construct_body.non_local_symbol_occurrences(Statement::ONLY_VARIABLES);

                for (ObjectList<IdExpression>::iterator it = references_body_all.begin();
                        it != references_body_all.end();
                        it++)
                {
                    // If the variable has a sharing attribute of 'threadprivate' do not consider
                    // it for anything
                    if ((task_construct.get_data_attribute(it->get_symbol()) & OpenMP::DA_THREADPRIVATE)
                            == OpenMP::DA_THREADPRIVATE)
                    {
                        continue;
                    }


                    // If this symbol appears in any data-sharing clause,
                    // ignore it since it already has an explicit data
                    // sharing attribute
                    //
                    // Note that all captureaddressed things are in
                    // 'captureaddress_references_in_clause',
                    // 'captureaddress_references' might contain less of
                    // them if they are globally accessible
                    Expression expr(it->get_ast(), it->get_scope_link());
                    if (captureaddress_references_in_clause.contains(*it, functor(&IdExpression::get_symbol)) 
                            || captureprivate_references_in_clause.contains(*it, functor(&IdExpression::get_symbol))
                            || local_references_in_clause.contains(*it, functor(&IdExpression::get_symbol))
                            || input_references_in_clause.contains(expr, functor(dependence_expr_to_sym))
                            || output_references_in_clause.contains(expr, functor(dependence_expr_to_sym))
                            )
                        continue;

                    Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

                    bool will_be_visible_from_outline = false;
                    bool is_unqualified_member = false;
                    if (global_sym.is_valid()
                            && (global_sym == it->get_symbol()))
                    {
                        // If the function-scope accessible symbol is the same
                        // found then it must be implicitly captureaddress,
                        // instead of capturevalue but since it is accessible
                        // it does not have to be passed
                        //
                        // As an exception, member symbols must be passed as
                        // captureaddress and they will be converted to
                        // "_this->member"
                        will_be_visible_from_outline = true;
                        is_unqualified_member = is_unqualified_member_symbol(it->get_symbol(), function_definition);
                    }

                    switch ((int)default_task_data_sharing)
                    {
                        case DK_TASK_UNDEFINED :
                            {
                                /*
                                 * According to the standard when a variable is
                                 * referenced inside a task construct and no
                                 * default is given and the variable does not
                                 * appear in any data-sharing clause:
                                 *
                                 *  (1) if the task is orphaned and the variable is a
                                 *  parameter then it is 'firstprivate'
                                 *
                                 *  (2) otherwise, if the task is not orphaned and
                                 *  nested inside a parallel and the variable is
                                 *  private in that construct, then it is
                                 *  firstprivate in this task construct
                                 *
                                 *  (3) otherwise, if the task is not nested in a
                                 *  parallel and the variable is private in the
                                 *  enclosing function (this might happen because
                                 *  the induction variable of an enclosing loop or
                                 *  simply because the variable is local)
                                 *
                                 *  (4) otherwise the variable is shared in this
                                 *  task construct
                                 */
                                Symbol sym = it->get_symbol();
                                // This will use the inherited scope if any
                                OpenMP::DataAttribute data_attrib = task_construct.get_data_attribute(sym);

                                if (data_attrib != OpenMP::DA_UNDEFINED)
                                {
                                    // Some enclosing construct defined a data sharing for this attribute
                                    if ((data_attrib & OpenMP::DA_PRIVATE) == OpenMP::DA_PRIVATE)
                                    {
                                        // (2) if an enclosing construct defined private for it (e.g. a 'parallel')
                                        // (3) if an enclosing non-parallel construct defined private for it (e.g. a 'for')
                                        captureprivate_references.insert(sym);
                                        task_construct.add_data_attribute(sym, OpenMP::DA_FIRSTPRIVATE);
                                    }
                                    else if ((data_attrib & OpenMP::DA_SHARED) == OpenMP::DA_SHARED)
                                    {
                                        if (will_be_visible_from_outline)
                                        {
                                            // (4)
                                            // Do nothing, will be shared by scope
                                        }
                                        else 
                                        {
                                            // (4) An enclosing construct (e.g. a 'parallel') defined it shared
                                            captureaddress_references.insert(sym);
                                            task_construct.add_data_attribute(sym, OpenMP::DA_SHARED);
                                        }
                                    }
                                    else
                                    {
                                        // (4) If not shared or private in an
                                        // enclosing scope (but somebody
                                        // defined some data sharing for it) it
                                        // is shared in this task
                                        captureaddress_references.insert(sym);
                                        task_construct.add_data_attribute(sym, OpenMP::DA_SHARED);
                                    }
                                }
                                else
                                {
                                    // No data sharing was set by any enclosing construct
                                    if (will_be_visible_from_outline)
                                    {
                                        // (4) It is shared because it is a global symbol
                                    }
                                    else if (sym.is_static())
                                    {
                                        // (4) It is shared because it is a
                                        // static variable.  Note that this
                                        // static is for local variables, since
                                        // global variables should have
                                        // 'will_be_visible_from_outline' set
                                        // to true
                                        captureaddress_references.insert(sym);
                                        task_construct.add_data_attribute(sym, OpenMP::DA_SHARED);
                                    }
                                    else
                                    {
                                        Type t = sym.get_type();

                                        if (t.is_const()
                                                && (!t.is_class()
                                                    || !t.some_member_is_mutable()))
                                        {
                                            // (3) If it is shared in the enclosing scope (so,
                                            // it is a const variable of a class/struct without
                                            // any mutable member) then it is shared.
                                            captureaddress_references.insert(sym);
                                            task_construct.add_data_attribute(sym, OpenMP::DA_SHARED);
                                        }
                                        else
                                        {
                                            // Otherwise, this includes (1), the symbol is firstprivate
                                            captureprivate_references.insert(sym);
                                            task_construct.add_data_attribute(sym, OpenMP::DA_FIRSTPRIVATE);
                                        }
                                    }
                                }

                                break;
                            }
                        case DK_TASK_NONE :
                            {
                                std::cerr << it->get_ast().get_locus() << ": warning: '" 
                                    << it->prettyprint() << "' does not have a data sharing attribute "
                                    << "and 'default(none)' was specified. " 
                                    << "It will be considered firstprivate." << std::endl;
                                /* Fall through captureprivate */
                            }
                        case DK_TASK_FIRSTPRIVATE :
                            {
                                captureprivate_references.insert(it->get_symbol());
                                task_construct.add_data_attribute(it->get_symbol(), OpenMP::DA_FIRSTPRIVATE);
                                break;
                            }
                        case DK_TASK_SHARED :
                            {
                                // If is not visible from the outline (or
                                // if it is, it is an unqualified member)
                                // then add to the captureaddress
                                if (!will_be_visible_from_outline
                                        || is_unqualified_member)
                                {
                                    captureaddress_references.insert(it->get_symbol());
                                    task_construct.add_data_attribute(it->get_symbol(), OpenMP::DA_SHARED);
                                }
                                break;
                            }
                        case DK_TASK_PRIVATE :
                            {
                                local_references.insert(it->get_symbol());
                                break;
                            }
                        case DK_TASK_INVALID :
                        default:
                            break;
                    }
                }
            }
        }

        void OpenMPTransform::handle_dependences(OpenMP::Directive directive,
                ObjectList<Expression> &input_dependences,
                ObjectList<Expression> &output_dependences,
                OpenMP::Construct &task_construct,
                ObjectList<Symbol>& captureaddress_references)
        {
            // input(x) clause support
            // input variables must be shared or firstprivate
            OpenMP::CustomClause input = directive.custom_clause("input");
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
                    Symbol sym(NULL);
                    if (expr.is_id_expression())
                    {
                        IdExpression id_expr = expr.get_id_expression();
                        sym = id_expr.get_symbol();
                    }
                    else if (expr.is_array_section())
                    {
                        Expression sectioned_entity = expr.array_section_item();

                        if (!sectioned_entity.is_id_expression())
                        {
                            std::cerr << expr.get_ast().get_locus() << ": warning: invalid array-section expression "
                                << "'" << expr.prettyprint() << "'"
                                << "specification in input clause. Ignoring" << std::endl;
                            continue;
                        }

                        IdExpression id_expr = sectioned_entity.get_id_expression();
                        sym = id_expr.get_symbol();
                    }
                    else
                    {
                        std::cerr << expr.get_ast().get_locus() << ": warning: invalid expression "
                            << "'" << expr.prettyprint() << "'"
                            << "specification in input clause. Ignoring" << std::endl;
                        continue;
                    }

                    // Insert the symbol as a capture address (shared entity)
                    captureaddress_references.insert(sym);
                    input_dependences.append(expr);
                }
            }

            // output(x) clause support
            // output variables must be shared
            OpenMP::CustomClause output = directive.custom_clause("output");
            if (output.is_defined()) 
            {
                ObjectList<Expression> dep_list = output.get_expression_list();

                if (dep_list.empty()) 
                {
                    std::cerr << input.get_ast().get_locus() <<
                        ": warning: empty output clause" << std::endl;
                }

                for (ObjectList<Expression>::iterator it = dep_list.begin();
                        it != dep_list.end();
                        it++)
                {
                    Expression &expr = *it;
                    Symbol sym(NULL);
                    if (expr.is_id_expression())
                    {
                        IdExpression id_expr = expr.get_id_expression();
                        sym = id_expr.get_symbol();
                    }
                    else if (expr.is_array_section())
                    {
                        Expression sectioned_entity = expr.array_section_item();

                        if (!sectioned_entity.is_id_expression())
                        {
                            std::cerr << expr.get_ast().get_locus() << ": warning: invalid array-section expression "
                                << "'" << expr.prettyprint() << "'"
                                << "specification in output clause. Ignoring" << std::endl;
                            continue;
                        }

                        IdExpression id_expr = sectioned_entity.get_id_expression();
                        sym = id_expr.get_symbol();
                    }
                    else
                    {
                        std::cerr << expr.get_ast().get_locus() << ": warning: invalid expression "
                            << "'" << expr.prettyprint() << "'"
                            << "specification in output clause. Ignoring" << std::endl;
                        continue;
                    }

                    // Insert the symbol as a capture address (shared entity)
                    captureaddress_references.insert(sym);
                    output_dependences.append(expr);
                }
            }
        }

        static Symbol dependence_expr_to_sym(Expression expr)
        {
            Symbol invalid(NULL);
            if (expr.is_id_expression())
            {
                return expr.get_id_expression().get_symbol();
            }
            else if (expr.is_array_section())
            {
                Expression sectioned_entity = expr.array_section_item();

                if (!sectioned_entity.is_id_expression())
                {
                    return invalid;
                }
                return sectioned_entity.get_id_expression().get_symbol();
            }
            return invalid;
        }

        static std::string get_representative_dependence_expr(Expression expr)
        {
            Symbol sym = dependence_expr_to_sym(expr);
            if (expr.is_id_expression())
            {
                Type t = sym.get_type();
                // I don't like this because naming an array means naming its first element address
                // but naming an address in a dependence does not have any sense either
                // so we will understand that the array name is the array as a whole
                if (t.is_array())
                {
                    return "&(" + expr.prettyprint() + "[0])";
                }
                else
                {
                    return "&" + expr.prettyprint();
                }
            }
            else if (expr.is_array_section())
            {
                Expression array_section_lower = expr.array_section_lower();
                // Expression array_section_upper = expr.array_section_upper();
                Expression array_section_item = expr.array_section_item();

                return "&(" + array_section_item.prettyprint() + "[" + array_section_lower.prettyprint() + "])";
            }

            return "<<invalid-expression(representative)>>";
        }

        static std::string get_size_dependence_expr(Expression expr)
        {
            Symbol sym = dependence_expr_to_sym(expr);
            if (expr.is_id_expression())
            {
                Type t = sym.get_type();
                // I don't like this because naming an array means naming its first element address
                // but naming an address in a dependence does not have any sense either
                // so we will understand that the array name is the array as a whole
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
            else if (expr.is_array_section())
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

            return "<<invalid-expression(size)>>";
        }

        static std::string get_align_dependence_expr(Expression expr)
        {
            Symbol sym = dependence_expr_to_sym(expr);
            if (expr.is_id_expression())
            {
                return "0";
            }
            else if (expr.is_array_section())
            {
                Expression array_section_lower = expr.array_section_lower();
                // Expression array_section_upper = expr.array_section_upper();
                Expression array_section_item = expr.array_section_item();

                return "(int)(&(" + array_section_item.prettyprint() + "[" + array_section_lower.prettyprint() + "])"
                    + "-"
                    + "&(" + array_section_item.prettyprint() + "[0])"
                    + ")";
            }

            return "<<invalid-expression(align)>>";
        }
    }
}
