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
            Source task_parameters;
            Source task_parameter_list;

            Source size_vector;

            // For each capture address entity just pass a reference to it
            int num_reference_args = 0;

            // This might be needed for nonstatic member functions
            if (is_nonstatic_member_function(function_definition))
            {
                task_parameter_list.append_with_separator("this", ",");
                num_reference_args++;
            }

            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                if (it->kind != ParameterInfo::BY_POINTER)
                    continue;

                task_parameter_list.append_with_separator(it->argument_name, ",");
                num_reference_args++;
            }

            // This vector will hold the sizeof's of entities passed as
            // private references
            bool copy_construction_needed = false;
            size_vector << "size_t nth_size[] = {0";
            int vector_index = 1;
            int num_value_args = 0;

            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                if (it->kind != ParameterInfo::BY_VALUE)
                    continue;

                // Add the size in the vector
                size_vector << ", sizeof(" << it->symbol.get_qualified_name(task_construct.get_scope()) << ")"
                    ;

                // A reference to the vector
                Source vector_ref;
                vector_ref << "&nth_size[" << vector_index << "]"
                    ;

                // First an address with the size must be passed
                task_parameter_list.append_with_separator(vector_ref.get_source(), ",");
                task_parameter_list.append_with_separator(it->argument_name, ",");

                CXX_LANGUAGE()
                {
                    Symbol sym = it->symbol;
                    Type type = sym.get_type();

                    if (type.is_class())
                    {
                        copy_construction_needed = true;
                    }
                }

                vector_index++;
            }
            size_vector << "};"
                ;
            num_value_args = vector_index - 1;

            if (num_value_args == 0)
            {
                // If no value args are passed do not declare the size vector
                size_vector = TL::Source("");
            }

            // A comma is only needed when the parameter list is non empty
            if (!task_parameter_list.empty())
            {
                task_parameters << ", " << task_parameter_list;
            }

            // 'switch' clause support
            // FIXME: We could use an enum here instead of these two literals
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

            Source task_dependency;

            // For C++ only
            Source copy_construction_part;
            if (copy_construction_needed)
            {
                // The task cannot start immediately because first we have
                // to copy-construct capture valued entities
                task_dependency << "1";

                Source copy_sequence;

                copy_construction_part 
                    << "else"
                    << "{"
                    <<   "int nth_one_dep = 1;"
                    <<   copy_sequence
                    <<   "nth_depsub(&nth, &nth_one_dep);"
                    << "}"
                    ;

                int vector_index = 1;
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind != ParameterInfo::BY_VALUE)
                        continue;

                    Symbol sym = it->symbol;
                    Type type = sym.get_type();

                    if (type.is_class())
                    {
                        copy_sequence
                            << "new (nth_arg_addr[" << vector_index << "])" 
                            << type.get_declaration(task_construct.get_scope(), "")
                            << "(" << sym.get_qualified_name(task_construct.get_scope()) << ");"
                            ;
                    }

                    vector_index++;
                }
            }
            else
            {
                // No dependencies if no construction has to be performed,
                // i.e. the task can start immediately
                task_dependency << "0";
            }

            Source outlined_function_reference;
            Source selector_cast;

            outlined_function_reference << get_outline_function_reference(function_definition, parameter_info_list);

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
            // FIXME: Instrumentation is still missing!!!
            task_queueing
                << "{"

                <<    "nth_cutoff_res_t nth_cutoff = nth_cutoff_create();"
                <<    "switch (nth_cutoff)"
                <<    "{"
                <<      "case NTH_CUTOFF_CREATE:"
                <<      "{"
                <<          comment("Create the task")
                <<          "nth_desc * nth;"
                <<          "int nth_type = " << task_type << ";"
                <<          "int nth_ndeps = " << task_dependency << ";"
                <<          "int nth_vp = 0;"
                <<          "nth_desc_t* nth_succ = (nth_desc_t*)0;"
                <<          "int nth_nargs_ref = " << num_reference_args << ";"
                <<          "int nth_nargs_val = " << num_value_args << ";"
                <<          "void* nth_arg_addr[" << num_value_args << " + 1];"
                <<          "void** nth_arg_addr_ptr = &nth_arg_addr[1];"
                <<          size_vector
                <<          "nth = nth_create_task((void*)(" << outlined_function_reference << "), "
                <<                 "&nth_type, &nth_ndeps, &nth_vp, &nth_succ, &nth_arg_addr_ptr, "
                <<                 "&nth_nargs_ref, &nth_nargs_val" << task_parameters << ");"
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
                <<          increment_task_level
                <<          outlined_function_reference << "(" << fallback_arguments << ");"
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

            Source instrumentation_code_before, instrumentation_code_after;

            // We can't use the common 'instrument_outline' one because of
            // timestamps required here, so we replicate part of its code 
            // below
            Source instrumentation_start_task;
            if (instrumentation_requested())
            {
                std::string file_name = "\"" + function_definition.get_ast().get_file() + "\"";

                int file_line = construct_body.get_ast().get_line();

                std::string mangled_function_name = 
                    "\"" + function_definition.get_function_name().mangle_id_expression() + "\"";

                instrumentation_code_before
                    << "const int EVENT_CALL_USER_FUNCTION = 60000018;"
                    << "int _user_function_event = mintaka_index_get(" << file_name << "," << file_line << ");"
                    << "if (_user_function_event == -1)"
                    << "{"
                    << "     nthf_spin_lock_((nth_word_t*)&_nthf_unspecified_critical);"
                    << "     _user_function_event = mintaka_index_allocate2(" << file_name << "," 
                    <<                file_line << "," << mangled_function_name << ", EVENT_CALL_USER_FUNCTION);"
                    << "     nthf_spin_unlock_((nth_word_t*)&_nthf_unspecified_critical);"
                    << "}"
                    << "int __previous_state = mintaka_get_state();"
                    << "nth_desc * nth;"
                    << "nth = nthf_self_();"
                    << "intptr_t id_nth = (intptr_t)nth;"
                    << "uint64_t _timestamp = mintaka_get_ts();"
                    << "mintaka_receive_at(id_nth, 1, _timestamp);"
                    << "mintaka_set_state_at(MINTAKA_STATE_RUN, _timestamp);"
                    << "mintaka_event_at(EVENT_CALL_USER_FUNCTION, _user_function_event, _timestamp);"
                    ;

                instrumentation_code_after
                    << "mintaka_state_and_event(__previous_state, EVENT_CALL_USER_FUNCTION, 0);"
                    ;

                // Ensure it is defined
                define_global_mutex("_nthf_unspecified_critical", function_definition.get_ast(),
                        function_definition.get_scope_link());
            }

            parallel_body 
                << private_declarations
                << instrumentation_code_before
                << modified_parallel_body_stmt.prettyprint()
                << instrumentation_code_after
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
                    std::cerr << "Warning: Clauses 'captureaddress' and 'capture_address' "
                        << "(found in " << legacy_captureaddress_clause.get_ast().get_locus() << " ) are deprecated. "
                        << "Instead use 'shared'." << std::endl;
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
                    std::cerr << "Warning: Clauses 'captureprivate', 'capturevalue', 'capture_private' and 'capture_value'"
                        << "(found in '" << legacy_captureprivate_clause.get_ast().get_locus() << "')" 
                        << " are deprecated. Instead use 'firstprivate'." << std::endl;
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
                std::cerr << "Warning: Unknown default clause '" 
                    << default_clause.prettyprint() << "' at " << default_clause.get_ast().get_locus() << ". "
                    << "Assuming 'default(firstprivate)'."
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
                    if (captureaddress_references_in_clause.contains(*it, functor(&IdExpression::get_symbol)) 
                            || captureprivate_references_in_clause.contains(*it, functor(&IdExpression::get_symbol))
                            || local_references_in_clause.contains(*it, functor(&IdExpression::get_symbol)))
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
                                std::cerr << "Warning: '" << it->prettyprint() << "' in " << it->get_ast().get_locus() 
                                    << " does not have a data sharing attribute and 'default(none)' was specified. "
                                    << "It will be considered capturevalue." << std::endl;
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

    }
}
