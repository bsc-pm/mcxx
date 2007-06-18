/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "tl-taskchunk.hpp"

namespace TL
{
    void OpenMPTransform::task_postorder(OpenMP::CustomConstruct task_construct)
    {
        // EXPERIMENTAL. Task chunking enabled
        if (!task_while_stack.empty())
        {
            return task_postorder_with_chunk(task_construct);
        }
        
        // One more parallel seen
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

        // Get references in local clause
        OpenMP::Clause private_clause = directive.private_clause();
        ObjectList<IdExpression> local_references_in_clause = private_clause.id_expressions();
        // Those stated by the user to be local are local_references
        ObjectList<IdExpression> local_references = local_references_in_clause;

        OpenMP::Clause shared_clause = directive.shared_clause();

        // Get all the identifiers of the captureaddress clause
        ObjectList<IdExpression> captureaddress_references;
        ObjectList<IdExpression> captureaddress_references_in_clause = shared_clause.id_expressions();
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
                        || is_unqualified_member_symbol(*it, function_definition))
                {
                    // If the symbol found in the function scope is not
                    // the same as the one referenced in the
                    // captureaddress it will be really
                    // 'captureaddressed', otherwise it can be
                    // referenced from the outline
                    captureaddress_references.append(*it);
                }
            }
        }

        ObjectList<std::string> captureprivate_names;
        captureprivate_names.append("captureprivate");
        OpenMP::CustomClause captureprivate_clause = directive.custom_clause(captureprivate_names);
        // Get the identifiers of the capturevalue clause
        ObjectList<IdExpression> captureprivate_references_in_clause = captureprivate_clause.id_expressions();

        // As stated by the user, everything in the clause is already
        // capturevalued (no pruning here as we did for captureaddress)
        ObjectList<IdExpression> captureprivate_references = captureprivate_references_in_clause;

        OpenMP::DefaultClause default_clause = directive.default_clause();

        enum 
        {
            DK_TASK_INVALID = 0,
            DK_TASK_SHARED,
            DK_TASK_CAPTUREPRIVATE,
            DK_TASK_PRIVATE,
            DK_TASK_NONE
        } default_task_data_sharing = DK_TASK_INVALID;

        if (!default_clause.is_defined())
        {
            // By default captureprivate
            default_task_data_sharing = DK_TASK_CAPTUREPRIVATE;
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
            default_task_data_sharing = DK_TASK_CAPTUREPRIVATE;
        }
        else if (default_clause.is_shared())
        {
            default_task_data_sharing = DK_TASK_SHARED;
        }
        else
        {
            std::cerr << "Warning: Unknown default clause '" 
                << default_clause.prettyprint() << "' at " << default_clause.get_ast().get_locus() << ". "
                << "Assuming 'default(capturevalue)'."
                << std::endl;
            default_task_data_sharing = DK_TASK_CAPTUREPRIVATE;
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
                Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

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

                bool will_be_visible_from_outline = false;
                bool is_unqualified_member = false;
                if (global_sym.is_valid()
                        && (global_sym == it->get_symbol()))
                {
                    // If the function-scope accessible symbol is the same found
                    // then it must be implicitly captureaddress, instead of capturevalue
                    // but since it is accessible it does not have to be passed
                    //
                    // As an exception member symbols must be passed as
                    // captureaddress and they will be converted to
                    // _this->member
                    will_be_visible_from_outline = true;
                    is_unqualified_member = is_unqualified_member_symbol(*it, function_definition);
                }

                switch ((int)default_task_data_sharing)
                {
                    case DK_TASK_NONE :
                        {
                            std::cerr << "Warning: '" << it->prettyprint() << "' in " << it->get_ast().get_locus() 
                                << " does not have a data sharing attribute and 'default(none)' was specified. "
                                << "It will be considered capturevalue." << std::endl;
                            /* Fall through captureprivate */
                        }
                    case DK_TASK_CAPTUREPRIVATE :
                        {
                            captureprivate_references.insert(*it, functor(&IdExpression::get_symbol));
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
                                captureaddress_references.insert(*it, functor(&IdExpression::get_symbol));
                            }
                            break;
                        }
                    case DK_TASK_PRIVATE :
                        {
                            local_references.insert(*it, functor(&IdExpression::get_symbol));
                            break;
                        }
                    case DK_TASK_INVALID :
                    default:
                        break;
                }
            }
        }

        ObjectList<IdExpression> empty;
        ObjectList<OpenMP::ReductionIdExpression> reduction_empty;
        ObjectList<ParameterInfo> parameter_info_list;

        ObjectList<IdExpression> captured_references;
        captured_references.append(captureaddress_references);
        captured_references.append(captureprivate_references);

        ReplaceIdExpression replace_references  = 
            set_replacements(function_definition,
                    directive,
                    construct_body,
                    captured_references, // Captured entities (captureaddress and capturevalue)
                    local_references, // Private entities (local clause)
                    empty,
                    empty,
                    reduction_empty,
                    reduction_empty,
                    empty,
                    empty,
                    parameter_info_list,
                    /* all_shared */ true);

        // Fix parameter_info_list
        // Currently set_replacement assumes that everything will be passed BY_POINTER
        // for every entity found in captureprivate_references will be set to BY_VALUE
        //
        // The proper way should be fixing "set_replacements" one day, but already
        // takes too much parameters so a more creative approach will be required
        for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                it != parameter_info_list.end();
                it++)
        {
            if (captureprivate_references.contains(it->id_expression, functor(&IdExpression::get_symbol)))
            {
                it->kind = ParameterInfo::BY_VALUE;
            }
        }

        // Get the code of the outline
        AST_t outline_code  = get_outline_task(
                function_definition,
                outlined_function_name, 
                construct_body,
                replace_references,
                parameter_info_list,
                local_references);

        // Now prepend the outline
        function_definition.get_ast().prepend_sibling_function(outline_code);

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

            task_parameter_list.append_with_separator("&" + it->id_expression.prettyprint(), ",");
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
            size_vector << ", sizeof(" << it->id_expression.prettyprint() << ")"
                ;

            // A reference to the vector
            Source vector_ref;
            vector_ref << "&nth_size[" << vector_index << "]"
                ;

            // First an address with the size must be passed
            task_parameter_list.append_with_separator(vector_ref.get_source(), ",");
            task_parameter_list.append_with_separator("&" + it->id_expression.prettyprint(), ",");

            CXX_LANGUAGE()
            {
                Symbol sym = it->id_expression.get_symbol();
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

        // A comma only needed when the parameter list is non empty
        if (!task_parameter_list.empty())
        {
            task_parameters << ", " << task_parameter_list;
        }

        // 'switch' clause support
        Source task_type;
        if (directive.custom_clause("untied").is_defined())
        {
            task_type << "0xa";
        }
        else
        {
            task_type << "0xe";
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

            fallback_arguments.append_with_separator("&" + it->id_expression.prettyprint(), ",");
        }

        // For capture value we will be passing pointers to local copies
        for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                it != parameter_info_list.end();
                it++)
        {
            if (it->kind != ParameterInfo::BY_VALUE)
                continue;

            Symbol sym = it->id_expression.get_symbol();
            Type type = sym.get_type();

            if (!type.is_array())
            {
                fallback_capture_values
                    << type.get_declaration_with_initializer(
                            it->id_expression.get_scope(),
                            "cval_" + it->id_expression.mangle_id_expression(), 
                            it->id_expression.prettyprint()) 
                    << ";"
                    ;
            }
            else
            {
                Source src_array_copy = array_copy(type, "cval_" + it->id_expression.mangle_id_expression(),
                        it->id_expression.prettyprint(), 0);

                fallback_capture_values
                    << type.get_declaration(it->id_expression.get_scope(),
                            "cval_" + it->id_expression.mangle_id_expression())
                    << ";"
                    << src_array_copy
                    ;
            }

            fallback_arguments.append_with_separator("&cval_" + it->id_expression.mangle_id_expression(), ",");
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

                Symbol sym = it->id_expression.get_symbol();
                Type type = sym.get_type();

                if (type.is_class())
                {
                    copy_sequence
                        << "new (nth_arg_addr[" << vector_index << "])" 
                        << type.get_declaration(it->id_expression.get_scope(), "")
                        << "(" << it->id_expression.prettyprint() << ");"
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

        task_queueing
            << "{"
            <<    "nth_desc * nth;"
            <<    "int nth_type = " << task_type << ";"
            <<    "int nth_ndeps = " << task_dependency << ";"
            <<    "int nth_vp = 0;"
            <<    "nth_desc_t* nth_succ = (nth_desc_t*)0;"
            <<    "int nth_nargs_ref = " << num_reference_args << ";"
            <<    "int nth_nargs_val = " << num_value_args << ";"
            <<    "void* nth_arg_addr[" << num_value_args << " + 1];"
            <<    "void** nth_arg_addr_ptr = &nth_arg_addr[1];"

            <<    size_vector

            <<    "nth = nth_create((void*)(" << outlined_function_reference << "), "
            <<             "&nth_type, &nth_ndeps, &nth_vp, &nth_succ, &nth_arg_addr_ptr, "
            <<             "&nth_nargs_ref, &nth_nargs_val" << task_parameters << ");"
            <<    instrument_code_task_creation
            <<    "if (nth == NTH_CANNOT_ALLOCATE_TASK)"
            <<    "{"
            // <<       "fprintf(stderr, \"Cannot allocate task at '%s'\\n\", \"" << task_construct.get_ast().get_locus() << "\");"
            // <<       fallback_capture_values
            // <<       outlined_function_reference << "(" << fallback_arguments << ");"
			<<    construct_body.prettyprint()
            <<    "}"
            <<    copy_construction_part
            << "}"
            ;

        if (instrumentation_requested())
        {
            std::string file_name = "\"task enqueue: " + function_definition.get_ast().get_file() + "\"";

            int file_line = construct_body.get_ast().get_line();

            std::string mangled_function_name = 
                "\"" + function_definition.get_function_name().mangle_id_expression() + "\"";

            instrument_code_task_creation
                // TODO we want to know if threadswitch was enabled
                << "const int EVENT_TASK_ENQUEUE = 60000010;"
                << "int _user_function_event = mintaka_index_get(" << file_name << "," << file_line << ");"
                << "if (_user_function_event == -1)"
                << "{"
                << "     nthf_spin_lock_((nth_word_t*)&_nthf_unspecified_critical);"
                << "     _user_function_event = mintaka_index_allocate2(" << file_name << "," 
                <<                file_line << "," << mangled_function_name << ", EVENT_TASK_ENQUEUE);"
                << "     nthf_spin_unlock_((nth_word_t*)&_nthf_unspecified_critical);"
                << "}"
                << "mintaka_event(EVENT_TASK_ENQUEUE, _user_function_event);"
                // << "if (nth != NTH_CANNOT_ALLOCATE_TASK)"
                << "{"
                // Adjust to 32 bit
				<< "     nth_desc* nth2 = (nth == NTH_CANNOT_ALLOCATE_TASK) ? nthf_self_() : nth;"
                << "     uint32_t id_nth = (((intptr_t)(nth2)) >> (32*((sizeof(nth2)/4) - 1)));"
                << "     mintaka_send(id_nth, 1);"
                << "     mintaka_state_run();"
                << "}"
                ;

            define_global_mutex("_nthf_unspecified_critical", function_definition.get_ast(),
                    function_definition.get_scope_link());
        }

        // Parse the code
        AST_t task_code = task_queueing.parse_statement(task_construct.get_ast(),
                task_construct.get_scope_link());

        // And replace the whole thing
        task_construct.get_ast().replace(task_code);
    }

    void OpenMPTransform::taskwait_postorder(OpenMP::CustomConstruct taskwait_construct)
    {
        Source taskwait_source;
        Statement taskwait_body = taskwait_construct.body();

        Source instrumentation_code_before, instrumentation_code_after;

        if (instrumentation_requested())
        {
            instrumentation_code_before
                << "int __previous_state = mintaka_get_state();"
                << "mintaka_state_synch();"
                ;

            instrumentation_code_after
                << "mintaka_set_state(__previous_state);"
                ;
        }

        taskwait_source
            << "{"
            <<    instrumentation_code_before
            <<    "nthf_task_block_();"
            <<    instrumentation_code_after
            <<    taskwait_body.prettyprint() // This will avoid breakage if you did not write ';' after the taskwait pragma
            << "}"
            ;

        AST_t taskwait_code = taskwait_source.parse_statement(taskwait_construct.get_ast(),
                taskwait_construct.get_scope_link());

        taskwait_construct.get_ast().replace(taskwait_code);
    }

    void OpenMPTransform::taskyield_postorder(OpenMP::CustomConstruct taskyield_construct)
    {
        Source taskyield_source;
        Statement taskyield_body = taskyield_construct.body();

        taskyield_source
            << "{"
            <<    "nth_yield();"
            <<    taskyield_body.prettyprint() // This will avoid breakage if you did not write ';' after the taskyield pragma
            << "}"
            ;

        AST_t taskyield_code = taskyield_source.parse_statement(taskyield_construct.get_ast(),
                taskyield_construct.get_scope_link());

        taskyield_construct.get_ast().replace(taskyield_code);
    }

    void OpenMPTransform::taskgroup_postorder(OpenMP::CustomConstruct taskgroup_construct)
    {
        Source taskgroup_source;
        Statement taskgroup_body = taskgroup_construct.body();

        Source instrumentation_code_before, instrumentation_code_after;

        if (instrumentation_requested())
        {
            instrumentation_code_before
                << "int __previous_state = mintaka_get_state();"
                << "mintaka_state_synch();"
                ;

            instrumentation_code_after
                << "mintaka_set_state(__previous_state);"
                ;
        }

        taskgroup_source
            << "{"
            <<    "nthf_push_taskgroup_scope_();"
            <<    taskgroup_body.prettyprint()
            <<    instrumentation_code_before
            <<    "nthf_task_block_();"
            <<    instrumentation_code_after
            <<    "nthf_pop_taskgroup_scope_();"
            << "}"
            ;

        AST_t taskgroup_code = taskgroup_source.parse_statement(taskgroup_construct.get_ast(),
                taskgroup_construct.get_scope_link());

        taskgroup_construct.get_ast().replace(taskgroup_code);
    }

    Source OpenMPTransform::get_task_block_code()
    {
        Source task_block_code;
        Source instrumentation_task_block_before, instrumentation_task_block_after;

        if (instrumentation_requested())
        {
            instrumentation_task_block_before
                << "{"
                << "   int __previous_state = mintaka_get_state();"
                << "   mintaka_state_synch();"
                ;

            instrumentation_task_block_after
                << "   mintaka_set_state(__previous_state);"
                << "}"
                ;
        }

        task_block_code
            << instrumentation_task_block_before
            << "nthf_task_block_();"
            << instrumentation_task_block_after
            ;

        return task_block_code;
    }

    AST_t OpenMPTransform::get_outline_task(
            FunctionDefinition function_definition,
            Source outlined_function_name,
            Statement construct_body,
            ReplaceIdExpression replace_references,
            ObjectList<ParameterInfo> parameter_info_list,
            ObjectList<IdExpression> local_references
            )
    {
        ObjectList<OpenMP::ReductionIdExpression> reduction_references;

        Source outline_parallel;
        Source parallel_body;

        outline_parallel = get_outline_common(
                function_definition,
                parallel_body, // The body of the outline
                outlined_function_name,
                parameter_info_list);

        // Replace references using set "replace_references" over construct body
        Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

        ObjectList<IdExpression> empty;
        ObjectList<OpenMP::ReductionIdExpression> reduction_empty;
        Source private_declarations = get_privatized_declarations(
                local_references,
                empty,
                empty,
                reduction_empty,
                empty,
                parameter_info_list
                ); 

        Source instrumentation_code_before, instrumentation_code_after;

        instrumentation_outline(instrumentation_code_before,
                instrumentation_code_after, 
                function_definition,
                construct_body);

        Source instrumentation_start_task;
        if (instrumentation_requested())
        {
            instrumentation_start_task
                << "{"
                << "   nth_desc * nth;"
                << "   nth = nthf_self_();"
                << "   uint32_t id_nth = (((intptr_t)(nth)) >> (32*((sizeof(nth)/4) - 1)));"
                << "   mintaka_receive(id_nth, 1);"
                << "   mintaka_state_run();"
                << "}"
                ;
        }

        parallel_body 
            << private_declarations
            << instrumentation_code_before
            << instrumentation_start_task
            << modified_parallel_body_stmt.prettyprint()
            << instrumentation_code_after
            ;

        return finish_outline(function_definition, outline_parallel, parameter_info_list);
    }

}
