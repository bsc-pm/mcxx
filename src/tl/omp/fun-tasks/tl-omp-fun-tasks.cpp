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
        construct.get_ast().replace(construct.get_declaration());
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

            Source arg_clauses;
            Source new_stmt_src;
            Source additional_decls;
            Source new_call;
            new_stmt_src
                << "{"
                << additional_decls
                << "#line " << it->get_line() << " \"" << it->get_file() << "\"\n"
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

            Source firstprivate_args;

            int i = 0;
            for (ObjectList<Expression>::iterator it2 = argument_list.begin();
                    it2 != argument_list.end();
                    it2++)
            {
                Expression &current_expr(*it2);
                Type real_type = current_expr.get_type();
                if (real_type.is_array())
                {
                    real_type = real_type.array_element().get_pointer_to();
                }
                else if (real_type.is_function())
                {
                    real_type = real_type.get_pointer_to();
                }

                std::stringstream ss;
                ss << "__tmp_" << i;

                additional_decls
                    << "#line " << it->get_line() << " \"" << it->get_file() << "\"\n"
                    << real_type.get_declaration(it2->get_scope(), ss.str()) << " = " << current_expr << ";"
                    ;

                new_arguments.append_with_separator(ss.str(), ",");
                firstprivate_args.append_with_separator(ss.str(), ",");

                i++;
            }

            if (!firstprivate_args.empty())
            {
                arg_clauses << " firstprivate(" << firstprivate_args << ")"
                    ;
            }

            ObjectList<FunctionTaskInfo::implementation_pair_t> implemented_tasks = task_info.get_devices_with_implementation();

            for (ObjectList<FunctionTaskInfo::implementation_pair_t>::iterator it2 = implemented_tasks.begin();
                    it2 != implemented_tasks.end();
                    it2++)
            {
                arg_clauses << " __implemented(" << it2->first << ", " << it2->second.get_qualified_name() << ")"
                    ;
            }

            AST_t new_stmt_tree = new_stmt_src.parse_statement(stmt.get_ast(),
                    scope_link);

            stmt.get_ast().replace(new_stmt_tree);
        }
    }

} }
