#include "tl-omp-core.hpp"
#include "tl-omp-fun-tasks.hpp"

namespace TL
{
namespace OpenMP
{
    FunctionTasks::FunctionTasks()
    {
        set_phase_name("OpenMP Task Functions");
        set_phase_description("This phase transforms calls to functions that "
                "have been declared with an OpenMP task construct into tasks "
                "themselves");

        on_directive_post["task"].connect(functor(&FunctionTasks::on_task_post, *this));
    }

    void FunctionTasks::pre_run(DTO& dto)
    {
    }

    void FunctionTasks::run(DTO& dto)
    {
        if (!dto.get_keys().contains("openmp_task_info"))
        {
            std::cerr << "OpenMP Task Info was not found in the pipeline" << std::endl;
            set_phase_status(PHASE_STATUS_ERROR);
            return;
        }

        _function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);

        if (!_function_task_set->empty())
        {
            openmp_core_run_next_time(dto);
        }

        // Run the parent phase which will simply remove the tasks
        OpenMPPhase::run(dto);

        // Convert calls
        convert_calls(dto);
    }

    void FunctionTasks::on_task_post(PragmaCustomConstruct construct)
    {
        // We do not deal with this kind of tasks
        if (!construct.get_declaration().is_valid())
        {
            return;
        }

        // Remove the task pragma

        AST_t function_task_body = construct.get_declaration();

        // Remove the task pragma but take into account potentially enclosing target pragmas
        AST_t current_construct = construct.get_ast();
        AST_t enclosing_tree = current_construct.get_parent();

        bool there_is_target = false;
        if (is_pragma_custom_construct("omp", "target", enclosing_tree, construct.get_scope_link()))
        {
            there_is_target = true;

            AST_t it = enclosing_tree.get_parent();
            // It might happen that it is not the outermost 'target'

            while (it.is_valid()
                    && is_pragma_custom_construct("omp", "target", it, construct.get_scope_link()))
            {
                enclosing_tree = it;
                it = it.get_parent();
            }
        }

        if (!enclosing_tree.is_valid())
        {
            internal_error("Invalid tree when pruning a function task", 0);
        }

        if (!there_is_target)
        {
            current_construct.replace(function_task_body);
        }
        else
        {
            enclosing_tree.replace(function_task_body);
        }
    }

    void FunctionTasks::convert_calls(DTO& dto)
    {
        AST_t translation_unit = dto["translation_unit"];
        ScopeLink scope_link = dto["scope_link"];

        struct IsFunctionCall : Predicate<AST_t>
        {
            ScopeLink _sl;
            RefPtr<FunctionTaskSet> _function_task_set;
            IsFunctionCall(ScopeLink sl, RefPtr<FunctionTaskSet> function_task_set)
                : _sl(sl), _function_task_set(function_task_set) { }

            bool do_(IsFunctionCall::ArgType a) const
            {
                bool result = false;
                if (Expression::predicate(a))
                {
                    Expression expr(a, _sl);
                    // We want a function call
                    if (expr.is_top_level_expression()
                            && expr.is_function_call()
                            // To an entity
                            && expr.get_called_expression().is_id_expression())
                    {
                        Symbol sym = expr.get_called_expression().get_id_expression().get_computed_symbol();

                        if (sym.is_valid()
                                && sym.is_function()
                                && _function_task_set->is_function_task(sym))
                        {
                            result = true;
                        }
                    }
                }
                return result;
            }
        };

        ObjectList<AST_t> eligible_function_calls 
            = translation_unit.depth_subtrees(IsFunctionCall(scope_link, _function_task_set));

        // This list will contain only calls to function tasks
        for (ObjectList<AST_t>::iterator it = eligible_function_calls.begin();
                it != eligible_function_calls.end();
                it++)
        {
            Expression expr(*it, scope_link);
            Symbol sym = expr.get_called_expression().get_id_expression().get_computed_symbol();

            FunctionTaskInfo& task_info = _function_task_set->get_function_task(sym);

            // Only void functions can be flagged as function tasks so we now
            // they will never be in an expression
            Statement stmt = expr.get_enclosing_statement();

            Source target_line;
            Source arg_clauses;
            Source new_stmt_src;
            Source additional_decls;
            Source new_call;
            new_stmt_src
                << "{"
                << additional_decls
                << target_line
                << "#line " << it->get_line() << " \"" << it->get_file() << "\"\n"
                << "#pragma omp task default(none) " << arg_clauses << "\n"
                << "{"
                << "\n"
                << "#line " << it->get_line() << " \"" << it->get_file() << "\"\n"
                << new_call << ";"
                << "}"
                << "}"
                ;

            ObjectList<FunctionTaskDependency> task_params = task_info.get_parameter_info();

            // Create new call
            Source new_arguments;
            new_call << expr.get_called_expression() << "(" << new_arguments << ")"
                ;

            ReplaceSrcIdExpression replace(scope_link);

            ObjectList<Type> parameter_types = sym.get_type().parameters();

            ObjectList<int> parameters_as_dependences;

            ObjectList<Symbol> sym_list = task_info.get_involved_parameters();
            ObjectList<Expression> argument_list = expr.get_argument_list();
            for (ObjectList<Symbol>::iterator it2 = sym_list.begin();
                    it2 != sym_list.end();
                    it2++)
            {
                Symbol &current_sym(*it2);
                if (current_sym.is_parameter())
                {
                    if (current_sym.get_parameter_position() < argument_list.size())
                    {
                        Source src;
                        src << "__tmp_" << current_sym.get_parameter_position();

                        Type param_type = 
                            parameter_types[current_sym.get_parameter_position()];

                        if (param_type.is_reference())
                        {
                            replace.add_replacement(current_sym, "(*" + src.get_source() + ")");
                        }
                        else
                        {
                            replace.add_replacement(current_sym, src.get_source());
                        }

                        if (param_type.is_pointer()
                                || param_type.is_array()
                                || param_type.is_reference())
                        {
                            parameters_as_dependences.insert(current_sym.get_parameter_position());
                        }
                    }
                    else
                    {
                        running_error("Invalid argument position %d >= %d",
                                current_sym.get_parameter_position(),
                                argument_list.size());

                    }
                }
            }

            // Now replace the inputs and outputs
            Source input_args;
            Source output_args;
            Source inout_args;

            for (ObjectList<FunctionTaskDependency>::iterator it2 = task_params.begin();
                    it2 != task_params.end();
                    it2++)
            {
                Source *args = NULL;
                switch (it2->get_direction())
                {
                    case DEP_DIR_INPUT :
                        {
                            args = &input_args;
                            break;
                        }
                    case DEP_DIR_OUTPUT :
                        {
                            args = &output_args;
                            break;
                        }
                    case DEP_DIR_INOUT :
                        {
                            args = &inout_args;
                            break;
                        }
                    default:
                        {
                            internal_error("Code unreachable", 0);
                        }
                }

                (*args).append_with_separator(
                        replace.replace(it2->get_expression()), 
                        ",");
            }

            // Now check parameters that do not appear in dependences since
            // they must appear in firstprivate
            Source firstprivate_args;
            for (unsigned int i = 0; i < argument_list.size(); i++)
            {
                if (!parameters_as_dependences.contains(i))
                {
                    firstprivate_args.append_with_separator(
                            Source("__tmp_") << i,
                            ",");
                }
            }

            if (!firstprivate_args.empty())
            {
                arg_clauses << " firstprivate(" << firstprivate_args << ")";
            }
            if (!input_args.empty())
            {
                arg_clauses << " __fp_input(" << input_args << ")";
            }
            if (!output_args.empty())
            {
                arg_clauses << " __fp_output(" << output_args << ")";
            }
            if (!inout_args.empty())
            {
                arg_clauses << " __fp_inout(" << inout_args << ")";
            }

            // Add the task symbol name to the clause
            arg_clauses << " __symbol(" << sym.get_name() << ")";

            int i = 0;
            for (ObjectList<Expression>::iterator it2 = argument_list.begin();
                    it2 != argument_list.end();
                    it2++)
            {
                Source addr, derref;
                Expression &current_expr(*it2);
                Type real_type = current_expr.get_type();
                if (parameter_types[i].is_reference())
                {
                    real_type = real_type.references_to().get_pointer_to();
                    addr << "&";
                    derref << "*";
                }

                if (real_type.is_reference())
                {
                    real_type = real_type.references_to();
                }

                if (real_type.is_array())
                {
                    real_type = real_type.array_element().get_pointer_to();
                }
                else if (real_type.is_function())
                {
                    real_type = real_type.get_pointer_to();
                }

                std::stringstream var;
                var << "__tmp_" << i;

                additional_decls
                    << "#line " << it->get_line() << " \"" << it->get_file() << "\"\n"
                    << real_type.get_declaration(it2->get_scope(), var.str()) << " = " << addr << current_expr << ";"
                    ;

                std::stringstream arg;
                arg << derref.get_source() << "__tmp_" << i;

                new_arguments.append_with_separator(arg.str(), ",");

                i++;
            }

            ObjectList<FunctionTaskInfo::implementation_pair_t> implemented_tasks = task_info.get_devices_with_implementation();

            for (ObjectList<FunctionTaskInfo::implementation_pair_t>::iterator it2 = implemented_tasks.begin();
                    it2 != implemented_tasks.end();
                    it2++)
            {
                arg_clauses << " __implemented(" << it2->first << ", " << it2->second.get_qualified_name() << ")"
                    ;
            }

            FunctionTaskTargetInfo target_info = task_info.get_target_info();

            if (!target_info.can_be_ommitted())
            {
                Source target_clauses;

                target_line
                    << "#pragma omp target " << target_clauses << "\n"
                    ;

                ObjectList<std::string> device_list = target_info.get_device_list();
                // This shall never be empty ...
                if (!device_list.empty())
                {
                    target_clauses << "device(" << TL::concat_strings(device_list, ",") << ")";
                }

                ObjectList<CopyItem> copy_in = target_info.get_copy_in();
                ObjectList<CopyItem> copy_out = target_info.get_copy_out();
                ObjectList<CopyItem> copy_inout = target_info.get_copy_inout();

                Source clause_in_args, clause_out_args, clause_inout_args;

                struct
                {
                    ObjectList<CopyItem> *list;
                    std::string clause_name;
                    Source *clause_args;
                } replace_copies[] = 
                {
                    { &copy_in,    "copy_in"    , &clause_in_args },
                    { &copy_out,   "copy_out"   , &clause_out_args },
                    { &copy_inout, "copy_inout" , &clause_inout_args },
                    { NULL, "", NULL }
                };

                for (int i = 0; replace_copies[i].list != NULL; i++)
                {
                    ObjectList<CopyItem> &copy_items = *(replace_copies[i].list);

                    if (copy_items.empty())
                        continue;

                    Source clause;
                    Source &clause_args(*(replace_copies[i].clause_args));

                    for (ObjectList<CopyItem>::iterator it = copy_items.begin();
                            it != copy_items.end();
                            it++)
                    {
                        clause_args.append_with_separator(replace.replace(it->get_copy_expression()), ",");
                    }
                }

                if (target_info.has_copy_deps())
                {
                    // We need to manually rewrite the dependences as copies
                    // but with the temporal names
                    int i = 0;
                    for (ObjectList<FunctionTaskDependency>::iterator it2 = task_params.begin();
                            it2 != task_params.end();
                            it2++)
                    {
                        Source *clause_args = NULL;
                        switch (it2->get_direction())
                        {
                            case DEP_DIR_INPUT :
                                {
                                    clause_args = &clause_in_args;
                                    break;
                                }
                            case DEP_DIR_OUTPUT :
                                {
                                    clause_args = &clause_out_args;
                                    break;
                                }
                            case DEP_DIR_INOUT :
                                {
                                    clause_args = &clause_inout_args;
                                    break;
                                }
                            default:
                                {
                                    internal_error("Code unreachable", 0);
                                }
                        }

                        Expression expr = it2->get_expression();

                        DataReference data_ref(expr);

                        if (!data_ref.is_valid())
                        {
                            std::cerr << expr.get_ast().get_locus() 
                                << ": warning: ignoring invalid data reference '" 
                                << expr.prettyprint() 
                                << "'" << std::endl;
                        }

                        Symbol sym = data_ref.get_base_symbol();

                        if (sym.is_parameter())
                        {
                            ReplaceSrcIdExpression replace_copies(replace);

                            Source src;
                            src << "__tmp_" << sym.get_parameter_position();

                            replace_copies.add_replacement(data_ref.get_base_symbol(), src.get_source());

                            (*clause_args).append_with_separator(
                                    replace_copies.replace(it2->get_expression()), 
                                    ",");

                        }
                    }
                }

                if (!clause_in_args.empty())
                {
                    target_clauses << "copy_in(" << clause_in_args << ") ";
                }
                if (!clause_out_args.empty())
                {
                    target_clauses << "copy_out(" << clause_out_args << ") ";
                }
                if (!clause_inout_args.empty())
                {
                    target_clauses << "copy_inout(" << clause_inout_args << ") ";
                }
            }

            AST_t new_stmt_tree = new_stmt_src.parse_statement(stmt.get_ast(),
                    scope_link);

            stmt.get_ast().replace(new_stmt_tree);
        }
    }

} }
