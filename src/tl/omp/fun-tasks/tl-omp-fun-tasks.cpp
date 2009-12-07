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
                << "#pragma omp task __function " << arg_clauses << "\n"
                << "{"
                << "\n"
                << "#line " << it->get_line() << " \"" << it->get_file() << "\"\n"
                << new_call
                << "}"
                << "}"
                ;

            ObjectList<int> input_params;
            ObjectList<int> output_params;
            ObjectList<int> inout_params;

            ObjectList<FunctionTaskParameter> task_params = task_info.get_parameter_info();

            for (ObjectList<FunctionTaskParameter>::iterator it = task_params.begin();
                    it != task_params.end();
                    it++)
            {
                DependencyDirection direction(it->get_direction());
                Symbol sym(it->get_symbol());

                ObjectList<int>* list = NULL;
                switch (direction)
                {
                    case DEP_DIR_INPUT :
                        {
                            list = &input_params;
                            break;
                        }
                    case DEP_DIR_OUTPUT :
                        {
                            list = &output_params;
                            break;
                        }
                    case DEP_DIR_INOUT :
                        {
                            list = &inout_params;
                            break;
                        }
                }
                list->insert(sym.get_parameter_position());
            }

            struct GenerateClause
            {
                Source& _arg_clauses;
                GenerateClause(Source &arg_clauses)
                    : _arg_clauses(arg_clauses) { }

                void generate(const std::string& clause_name, ObjectList<int> &list)
                {
                    if (!list.empty())
                    {
                        if (!_arg_clauses.empty())
                            _arg_clauses << " ";

                        std::sort(list.begin(), list.end());
                        _arg_clauses 
                            << clause_name << "(" 
                            << concat_strings(list, ",")
                            << ")"
                            ;
                    }
                }
            };

            GenerateClause aux(arg_clauses);

            aux.generate("__input_args", input_params);
            aux.generate("__output_args", output_params);
            aux.generate("__inout_args", inout_params);

            // Create new call
            Source argument_list;
            new_call << expr.get_called_expression() << "(" << argument_list << ");"
                ;

            ObjectList<Expression> argument_expr = expr.get_argument_list();
            int i = 0;
            for (ObjectList<Expression>::iterator it = argument_expr.begin();
                    it != argument_expr.end();
                    it++)
            {
                bool is_lvalue = false;
                it->get_type(is_lvalue);

                Source argument;

                if (!is_lvalue)
                {
                    Type type = it->get_type();
                    if (type.is_reference())
                        type = type.references_to();

                    std::stringstream ss;
                    ss << "_dep_" << i;

                    additional_decls
                        << type.get_declaration(expr.get_scope(), ss.str()) << "=(" << it->prettyprint() << ");";

                    argument_list << ss.str();
                }
                else
                {
                    argument_list << it->prettyprint();
                }

                if ((it + 1) != argument_expr.end())
                {
                    argument_list << ",";
                }
                i++;
            }

            AST_t new_stmt_tree = new_stmt_src.parse_statement(stmt.get_ast(),
                    scope_link);

            stmt.get_ast().replace(new_stmt_tree);
        }
    }

} }
