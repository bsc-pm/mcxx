#include "tl-omptransform.hpp"
#include "tl-taskchunk.hpp"

namespace TL
{
    /*
      Experimental '#pragma omp while'
     
      Example:
     
      #pragma omp while chunk(32)
      while (l != NULL)
      {
         // This task would get "chunked" by 32
         #pragma omp task
         {
           f(l);
         }
         l = l->next;
      }
     */
    std::stack<TaskWhileInfo> task_while_stack;
    void OpenMPTransform::task_while_preorder(OpenMP::CustomConstruct while_construct)
    {
        TaskWhileInfo task_while_info;
        OpenMP::Directive directive = while_construct.directive();

        OpenMP::CustomClause chunk_clause = directive.custom_clause("chunk");
        if (chunk_clause.is_defined())
        {
            ObjectList<Expression> expression_list = chunk_clause.get_expression_list();

            if (expression_list.size() > 0)
            {
                task_while_info.chunking = expression_list[0].prettyprint();
            }
        }

        if (task_while_info.chunking == "")
        {
            std::cerr << "Missing 'chunk' clause in " << while_construct.get_ast().get_locus() << " assuming 32" << std::endl;
            task_while_info.chunking = "32";
        }

        task_while_stack.push(task_while_info);
    }

    void OpenMPTransform::task_while_postorder(OpenMP::CustomConstruct while_construct)
    {
        Source task_while_src;
        TaskWhileInfo &task_while_info = task_while_stack.top();

        Statement statement = while_construct.body();

        task_while_src
            << "{"
            <<    task_while_info.pre_src
            <<    statement.prettyprint()
            <<    task_while_info.post_src
            << "}"
            ;

        AST_t task_while_tree = task_while_src.parse_statement(while_construct.get_ast(),
                while_construct.get_scope_link());

        while_construct.get_ast().replace_with(task_while_tree);
        task_while_stack.pop();
    }

    void OpenMPTransform::task_postorder_with_chunk(OpenMP::CustomConstruct task_construct)
    {
        TaskWhileInfo &task_while_info = task_while_stack.top();

        static int task_id_number = 0;
        task_id_number++;

        Source task_id;
        task_id << "nth_task_" << task_id_number
            ;

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
        ObjectList<std::string> local_names;
        local_names.append("local");
        local_names.append("taskprivate");
        local_names.append("task_private");
        OpenMP::CustomClause local_clause = directive.custom_clause(local_names);
        ObjectList<IdExpression> local_references_in_clause = local_clause.id_expressions();
        // Those stated by the user to be local are local_references
        ObjectList<IdExpression> local_references = local_references_in_clause;

        // Get references in captureaddress clause
        ObjectList<std::string> captureaddress_names;
        captureaddress_names.append("captureaddress");
        captureaddress_names.append("capture_address");
        OpenMP::CustomClause captureaddress_clause = directive.custom_clause(captureaddress_names);

        // Get all the identifiers of the captureaddress clause
        ObjectList<IdExpression> captureaddress_references;
        ObjectList<IdExpression> captureaddress_references_in_clause = captureaddress_clause.id_expressions();
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

        ObjectList<std::string> capturevalue_names;
        capturevalue_names.append("capturevalue");
        capturevalue_names.append("capture_value");
        OpenMP::CustomClause capturevalue_clause = directive.custom_clause(capturevalue_names);
        // Get the identifiers of the capturevalue clause
        ObjectList<IdExpression> capturevalue_references_in_clause = capturevalue_clause.id_expressions();

        // As stated by the user, everything in the clause is already
        // capturevalued (no pruning here as we did for captureaddress)
        ObjectList<IdExpression> capturevalue_references = capturevalue_references_in_clause;

        OpenMP::DefaultClause default_clause = directive.default_clause();

        enum 
        {
            DK_TASK_INVALID = 0,
            DK_TASK_CAPTUREADDRESS,
            DK_TASK_CAPTUREVALUE,
            DK_TASK_LOCAL,
            DK_TASK_NONE
        } default_task_data_sharing = DK_TASK_INVALID;

        if (!default_clause.is_defined())
        {
            // By default capturevalue
            default_task_data_sharing = DK_TASK_CAPTUREADDRESS;
        }
        else if (default_clause.is_none())
        {
            default_task_data_sharing = DK_TASK_NONE;
        }
        else if (default_clause.is_custom(local_names))
        {
            default_task_data_sharing = DK_TASK_LOCAL;
        }
        else if (default_clause.is_custom(capturevalue_names))
        {
            default_task_data_sharing = DK_TASK_CAPTUREVALUE;
        }
        else if (default_clause.is_custom(captureaddress_names))
        {
            default_task_data_sharing = DK_TASK_CAPTUREADDRESS;
        }
        else
        {
            std::cerr << "Warning: Unknown default clause '" 
                << default_clause.prettyprint() << "' at " << default_clause.get_ast().get_locus() << ". "
                << "Assuming 'default(capturevalue)'."
                << std::endl;
            default_task_data_sharing = DK_TASK_CAPTUREVALUE;
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
                        || capturevalue_references_in_clause.contains(*it, functor(&IdExpression::get_symbol))
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
                            /* Fall through capturevalue */
                        }
                    case DK_TASK_CAPTUREVALUE :
                        {
                            capturevalue_references.insert(*it, functor(&IdExpression::get_symbol));
                            break;
                        }
                    case DK_TASK_CAPTUREADDRESS :
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
                    case DK_TASK_LOCAL :
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
        captured_references.append(capturevalue_references);

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
        // for every entity found in capturevalue_references will be set to BY_VALUE
        //
        // The proper way should be fixing "set_replacements" one day, but already
        // takes too much parameters so a more creative approach will be required
        for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                it != parameter_info_list.end();
                it++)
        {
            if (capturevalue_references.contains(it->id_expression, functor(&IdExpression::get_symbol)))
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
            size_vector << ", sizeof(" << it->id_expression.prettyprint() << ") * (" << task_while_info.chunking << ")"
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
        if (directive.custom_clause("switch").is_defined())
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

        // For C++ only
        Source copy_construction_part;
        // if (copy_construction_needed)
        {
            // The task cannot start immediately because first we have
            // to copy-construct capture valued entities

            Source copy_sequence;

            copy_construction_part 
                <<   copy_sequence
                ;

            int vector_index = 0;
            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                if (it->kind != ParameterInfo::BY_VALUE)
                    continue;

                Symbol sym = it->id_expression.get_symbol();
                Type type = sym.get_type();

                CXX_LANGUAGE()
                {
                    if (type.is_class())
                    {
                        copy_sequence
                            << "new (nth_arg_addr[" << vector_index << "])" 
                            << type.get_declaration(it->id_expression.get_scope(), "")
                            << "(" << it->id_expression.prettyprint() << ");"
                            ;
                    }
                }

                C_LANGUAGE()
                {
                    Source type_cast;
                    copy_sequence
                        << "memcpy(((" << type_cast << ")" << task_id << "_arg_addr[" << vector_index << "]) + " << task_id << "_chunk, "
                        << "&(" << it->id_expression.prettyprint() << "),"
                        << "sizeof(" << it->id_expression.prettyprint() << "));"
                        ;

                    Type type = it->id_expression.get_symbol().get_type();
                    Type pointer_type = type.get_pointer_to();
                    type_cast
                         << pointer_type.get_declaration(it->id_expression.get_scope(), "");
                }

                vector_index++;
            }
        }

        Source outlined_function_reference;
        Source selector_cast;

        outlined_function_reference << get_outline_function_reference(function_definition, parameter_info_list);

        Source instrument_code_task_creation;

        task_while_info.pre_src
            <<    "nth_desc * "<< task_id << ";"
            <<    "int " << task_id << "_chunk = 0;"
            <<    "void* " << task_id << "_arg_addr[" << num_value_args << " + 1] = { 0 };"
            <<    "void** " << task_id << "_arg_addr_ptr = " << task_id << "_arg_addr;"
            ;

        task_while_info.post_src
            << "if (" << task_id << "_chunk != 0)"
            << "{"
            <<    "int nth_one_dep = 1;"
            <<    "nth_depsub(&" << task_id << ", &nth_one_dep);"
            << "}"
            ;

        task_queueing
            << "{"
            <<    "int nth_type = " << task_type << ";"
            <<    "int nth_ndeps = 1;" // Never start it running
            <<    "int nth_vp = 0;"
            <<    "nth_desc_t* nth_succ = (nth_desc_t*)0;"
            <<    "int nth_nargs_ref = " << num_reference_args << ";"
            <<    "int nth_nargs_val = " << num_value_args << ";"

            <<    size_vector

            <<    "if (" << task_id << "_chunk == 0)"
            <<    "{"
            <<         task_id <<  "= nth_create((void*)(" << outlined_function_reference << "), "
            <<                  "&nth_type, &nth_ndeps, &nth_vp, &nth_succ, &" << task_id << "_arg_addr_ptr, "
            <<                  "&nth_nargs_ref, &nth_nargs_val" << task_parameters << ");" \
            <<         instrument_code_task_creation
            <<         "if (" << task_id << " == NTH_CANNOT_ALLOCATE_TASK)"
            <<         "{"
            // <<            "fprintf(stderr, \"Cannot allocate task at '%s'\\n\", \"" << task_construct.get_ast().get_locus() << "\");"
            <<            fallback_capture_values
            <<            outlined_function_reference << "(" << fallback_arguments << ");"
            <<         "}"
            <<         "else"
            <<         "{"
            <<            copy_construction_part
            <<            task_id << "_chunk++;"
            <<         "}"
            <<    "}"
            <<    "else"
            <<    "{"
            <<       copy_construction_part
            <<       task_id << "_chunk++;"
            <<    "}"
            <<    "if (" << task_id << "_chunk == (" << task_while_info.chunking << "))"
            <<    "{"
            <<        "int nth_one_dep = 1;"
            <<        "nth_depsub(&" << task_id << ", &nth_one_dep);"
            <<        task_id << "_chunk = 0;"
            <<        "nth_yield();" // REMOVE THIS
            <<    "}"
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
                << "if (nth != NTH_CANNOT_ALLOCATE_TASK)"
                << "{"
                // Adjust to 32 bit
                << "     uint32_t id_nth = (((intptr_t)(nth)) >> (32*((sizeof(nth)/4) - 1)));"
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
}
