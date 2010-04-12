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

            bool do_(AST_t& a) const
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
                << "#line " << it->get_line() << " \"" << it->get_file() << "\"\n"
                << target_line
                << "#pragma omp task " << arg_clauses << "\n"
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

            // Now replace the inputs and outputs
            Source input_args;
            Source output_args;
            Source inout_args;

            ReplaceSrcIdExpression replace(scope_link);

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
                        replace.add_replacement(current_sym, 
                                "(" + argument_list[current_sym.get_parameter_position()].prettyprint() + ")");
                    }
                    else
                    {
                        running_error("Invalid argument position %d >= %d",
                                current_sym.get_parameter_position(),
                                argument_list.size());

                    }
                }
            }


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

            if (!input_args.empty())
            {
                arg_clauses << " input(" << input_args << ")";
            }
            if (!output_args.empty())
            {
                arg_clauses << " output(" << output_args << ")";
            }
            if (!inout_args.empty())
            {
                arg_clauses << " inout(" << inout_args << ")";
            }

            for (ObjectList<Expression>::iterator it2 = argument_list.begin();
                    it2 != argument_list.end();
                    it2++)
            {
                Expression &current_expr(*it2);
                new_arguments.append_with_separator(current_expr.prettyprint(), ",");
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

                if (target_info.has_copy_deps())
                {
                    target_clauses << "copy_deps";
                }

                ObjectList<CopyItem> copy_in = target_info.get_copy_in();

                struct
                {
                    ObjectList<CopyItem> (FunctionTaskTargetInfo::*pmf)() const;
                    std::string clause_name;
                } replace_copies[] = 
                {
                    { &FunctionTaskTargetInfo::get_copy_in,    "copy_in"    },
                    { &FunctionTaskTargetInfo::get_copy_out,   "copy_out"   },
                    { &FunctionTaskTargetInfo::get_copy_inout, "copy_inout" },
                    NULL,
                };

                for (int i = 0; replace_copies[i].pmf != NULL; i++)
                {
                    ObjectList<CopyItem> copy_items = (target_info.*(replace_copies[i].pmf))();

                    if (copy_items.empty())
                        continue;

                    Source clause, clause_args;

                    clause << replace_copies[i].clause_name << "(" << clause_args << ")";

                    for (ObjectList<CopyItem>::iterator it = copy_items.begin();
                            it != copy_items.end();
                            it++)
                    {
                        clause_args.append_with_separator(replace.replace(it->get_copy_expression()), ",");
                    }

                    target_clauses << " " << clause;
                }
            }

            AST_t new_stmt_tree = new_stmt_src.parse_statement(stmt.get_ast(),
                    scope_link);

            stmt.get_ast().replace(new_stmt_tree);
        }
    }

} }
