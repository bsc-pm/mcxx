#include "hlt-task-aggregation.hpp"
#include "hlt-task-aggregation-common.hpp"
#include "tl-omp.hpp"
#include "tl-counters.hpp"

#include "hlt-unroll-omp.hpp"

/*
   FIXME - Still relying on explicit firstprivates :(
 */

using namespace TL;
using namespace HLT;
using namespace OpenMP;

static const std::string TASK_BUNDLE_COUNTER("hlt.task_bundle");

struct BundleGenerator
{
    private:
        const GuardTaskInfo &_info;
        ScopeLink _sl;
        int _bundling_amount;
    public:
        BundleGenerator(const GuardTaskInfo& info, ScopeLink sl, int bundling_amount)
            : _info(info), _sl(sl), _bundling_amount(bundling_amount)
        {
        }

        Source generate_bundle(Source &clear_indexes, bool unroll = false, bool empty = false)
        {
			// Do not unroll if bundling amount is huge
			if (_bundling_amount > 128)
				unroll = false;
			
            Source try_to_run_every_task;
            Source firstprivate_clause_src, firstprivate_args, code_of_all_tasks;
            Source switch_structure, loop_header;

            Source current_index_name;
            current_index_name
                << "_current_index_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
                ;

            Source global_task_index_name;
            global_task_index_name
                << "_global_task_index_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
                ;

            Source task_log_name;
            task_log_name << "_task_log_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
                ;

            Source bundle_info_name;
            bundle_info_name
                << "_bundle_info_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
                ;

			Source task_key;

			if (unroll)
			{
				task_key
					<< " task_key(" << task_log_name << ")"
					;
			}
			else
			{
				task_key
					<< " task_key(" << bundle_info_name << ")"
					;
			}

			Source task_body;
            try_to_run_every_task
                << "#pragma omp task" << firstprivate_clause_src 
				<< task_key << "\n"
				<< task_body
				;

			if (empty)
			{
				task_body
					<< ";"
					;
			}
			else
			{
				task_body
					<< "{"
					<< "int " << current_index_name << "  = 0;"
					<< loop_header
					<< switch_structure
					<< "}"
					;

				if (!unroll)
				{
					loop_header
						<< "for (" << current_index_name << " = 0; "
						<<       "" << current_index_name << " < " << global_task_index_name << "; "
						<<       "" << current_index_name << "++)"
						;
					switch_structure
						<< "{"
						<<    "switch (" << task_log_name << "[" << current_index_name << "])"
						<<    "{"
						<<        code_of_all_tasks
						<<        "default: break;"
						<<    "}"
						<< "}"
						;
				}
				else
				{
					for (int i = 0; i < _bundling_amount; i++)
					{
						switch_structure
							<< "switch (" << task_log_name << "[" << current_index_name << "])"
							<< "{"
							<<     code_of_all_tasks
							<<     "default: break;"
							<< "}"
							<< current_index_name << "++;";
						;
					}
				}

				ObjectList<Symbol> firstprivated_symbols;
				ObjectList<GuardedTask> guarded_task_list = _info.get_guarded_tasks();
				for (ObjectList<GuardedTask>::iterator it = guarded_task_list.begin();
						it != guarded_task_list.end();
						it++)
				{
					TaskConstruct current_task = it->get_task();
					Directive current_directive = current_task.directive();
					Clause current_firstprivate_clause = current_directive.firstprivate_clause();
					ReplaceSrcIdExpression replacements(_sl);

					Source extended_task_body, task_cleanup;

					ObjectList<IdExpression> firstprivate_id_expr_list = current_firstprivate_clause.id_expressions();
					for (ObjectList<IdExpression>::iterator fp_it = firstprivate_id_expr_list.begin();
							fp_it != firstprivate_id_expr_list.end();
							fp_it++)
					{
						IdExpression& id_expr(*fp_it);
						Symbol sym(id_expr.get_symbol());

						Source repl_src;
						C_LANGUAGE()
						{
							repl_src << "(" << bundle_info_name << "[" << current_index_name << "]._info_" << it->get_id() << "." << sym.get_name() << ")"
								;
							replacements.add_replacement(sym, repl_src);
						}
						CXX_LANGUAGE()
						{
							Type type = sym.get_type();
							if (type.is_class())
							{
								extended_task_body
									<< type.get_reference_to().get_declaration(sym.get_scope(), sym.get_name()) 
									<< "(*(" << type.get_pointer_to().get_declaration(sym.get_scope(), "") << ")" 
									<<  "(" << bundle_info_name << "[" << current_index_name << "]._info_" 
									<< it->get_id() << "._pt_" << sym.get_name() << ")"
									<< ");"
									;

								Symbol class_symbol = type.get_symbol();

								task_cleanup
									<< sym.get_name() << "." << type.get_declaration(sym.get_scope(), "") << "::~" << class_symbol.get_name() << "();"
									;
							}
							else
							{
								repl_src << "(" << bundle_info_name << "[" << current_index_name << "]._info_" 
									<< it->get_id() << "." << sym.get_name() << ")"
									;
								replacements.add_replacement(sym, repl_src);
							}
						}
						
					}

					code_of_all_tasks
						<< "case " << it->get_id() << ":"
						<< "{"
						<<    extended_task_body
						<<    task_cleanup
						<<    "break;"
						<< "}"
						;

					extended_task_body << replacements.replace(current_task.body());
				}

				// Bundle info
				firstprivate_args.append_with_separator(
						bundle_info_name,
						",");
				// Task log is always passed
				firstprivate_args.append_with_separator(
						task_log_name,
						",");
				// Global index is passed only if not unrolled
				if (!unroll)
				{
					firstprivate_args.append_with_separator(
							global_task_index_name,
							",");
				}
				if (!firstprivate_args.empty())
				{
					firstprivate_clause_src << " firstprivate(" << firstprivate_args << ")"
						;
				}
			}

            return try_to_run_every_task;
        }
};

struct GuardTaskGeneratorBundled : Functor<TL::AST_t::callback_result, TL::AST_t>
{
    private:
        ScopeLink _sl;
        const GuardTaskInfo& _info;
        int _bundling_amount;
		int *_task_num;
    public:
        GuardTaskGeneratorBundled(ScopeLink sl, const GuardTaskInfo& info, int bundling_amount = 4)
            : _sl(sl), 
            _info(info), 
            _bundling_amount(bundling_amount),
			_task_num(new int(0))
        {
        }

		~GuardTaskGeneratorBundled()
		{
			delete _task_num;
		}

        virtual AST_t::callback_result do_(TL::AST_t& a) const
        {
            if (TaskConstruct::predicate(a))
            {
				(*_task_num)++;
				
                Source result;
				result
					<< "{"
					;
                // Capture current firstprivate values
                // FIXME -> We are relying on explicit firstprivates
                TaskConstruct task_construct(a, _sl);
                int task_id = _info.get_task_id(task_construct);
                Directive directive = task_construct.directive();
                Clause firstprivate_clause = directive.firstprivate_clause();

                ObjectList<GuardedTask::additional_var> additional_vars;

                Source global_task_index_name;
                global_task_index_name << "_global_task_index_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
                    ;

                Source bundle_info_name;
                bundle_info_name
                    << "_bundle_info_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
                    ;

                Source task_log_name;
                task_log_name << "_task_log_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
                    ;

                result
                    << task_log_name << "[" << global_task_index_name << "] = " << task_id << ";"
                    ;

                if (firstprivate_clause.is_defined())
                {
                    ObjectList<TL::IdExpression> vars = firstprivate_clause.id_expressions();

                    for (ObjectList<TL::IdExpression>::iterator it = vars.begin();
                            it != vars.end();
                            it++)
                    {
                        Symbol sym(it->get_symbol());
						C_LANGUAGE()
						{
							result
								<< bundle_info_name << "[" << global_task_index_name << "]._info_" << task_id << "." << sym.get_name() 
								<< " = " << it->prettyprint() << ";"
								;
						}
						CXX_LANGUAGE()
						{
							Type type = sym.get_type();
							if (type.is_class())
							{
								result
									<< "new((void*)(" 
										<< bundle_info_name << "[" << global_task_index_name << "]._info_" << task_id << "._pt_" << sym.get_name() 
									<< "))" << type.get_declaration(sym.get_scope(), "") << "(" << it->prettyprint() << ");"
									;
							}
							else
							{
								result
									<< bundle_info_name << "[" << global_task_index_name << "]._info_" << task_id << "." << sym.get_name() 
									<< " = " << it->prettyprint() << ";"
									;
							}
						}
                    }
                }

                Source try_to_run_every_task, clear_indexes;

                result
                    << global_task_index_name << "++;"
                    << "if (" << global_task_index_name << "== " << _bundling_amount << ")"
                    << "{"
                    <<     try_to_run_every_task
                    <<     global_task_index_name << " = 0;"
                    <<     clear_indexes
                    << "}"
                    ;

                BundleGenerator bundle_gen(_info, _sl, _bundling_amount);

                try_to_run_every_task
                    << bundle_gen.generate_bundle(clear_indexes, /* unroll= */ true, /* empty */ ((*_task_num) != 1))
                    ;

				result
					<< "}"
					;


                return AST_t::callback_result(true, result);
            }
            else 
                return AST_t::callback_result(false, "");
        }
};

Source TaskAggregation::do_bundled_aggregation()
{
    CounterManager::get_counter(TASK_BUNDLE_COUNTER)++;

    GuardTaskInfo guard_task_info;
    guard_task_info.fill_guard_tasks_basic(_stmt);

    Source result,
           inline_task_counters,
           inline_finish_task,
           bundled_tasks;

    result
        << "{"
        << inline_task_counters
        << bundled_tasks
        << inline_finish_task
        << "}"
        ;

    Source global_task_index_name;
    global_task_index_name << "_global_task_index_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
        ;

    Source bundle_info_name;
    bundle_info_name
        << "_bundle_info_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
        ;

    Source task_log_name;
    task_log_name << "_task_log_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER)
        ;

    Source union_type_name;
    union_type_name
        << "_task_bundle_info_" << CounterManager::get_counter(TASK_BUNDLE_COUNTER) << "_t"
        ;


    Source &task_counters = (_global_bundling_src != NULL ? *_global_bundling_src : inline_task_counters);
    Source &bundle_remainder = (_finish_bundling_src != NULL ? *_finish_bundling_src : inline_finish_task);

    task_counters
        << "int " << task_log_name << "[" << _bundling_amount << "] = {0};"
        << union_type_name << " " << bundle_info_name << "[" << _bundling_amount << "]" << ";"
        ;

    Source union_definition, union_fields;

    union_definition
        << "typedef union {"
        <<   union_fields
        << "} " << union_type_name << ";"
        ;

    ObjectList<GuardedTask> guarded_tasks = guard_task_info.get_guarded_tasks();

    ObjectList<Symbol> firstprivate_syms;

    for (ObjectList<GuardedTask>::iterator it_guarded_task = guarded_tasks.begin();
            it_guarded_task != guarded_tasks.end();
            it_guarded_task++)
    {
        GuardedTask &guarded_task(*it_guarded_task);

        TaskConstruct task_construct = guarded_task.get_task();
        Directive task_directive = task_construct.directive();
        Clause firstprivate_clause = task_directive.firstprivate_clause();

        if (firstprivate_clause.is_defined())
        {
            ObjectList<IdExpression> id_expressions = firstprivate_clause.id_expressions();
            firstprivate_syms.insert(id_expressions.map(functor(&IdExpression::get_symbol)));

            Source union_task_fields;
            union_fields
                << "struct _task_info_" << guarded_task.get_id() << " {"
                << union_task_fields
                << "} _info_" << guarded_task.get_id() << ";"
                ;
            for (ObjectList<IdExpression>::iterator it = id_expressions.begin();
                    it != id_expressions.end();
                    it++)
            {
                Symbol sym(it->get_symbol());
                Type type(sym.get_type());

				C_LANGUAGE()
				{
					union_task_fields
						<< type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
						;
				}

				CXX_LANGUAGE()
				{
					if (type.is_class())
					{
						union_task_fields
							<< "char _pt_" << sym.get_name() << "[sizeof(" << type.get_declaration(sym.get_scope(), "") << ")];"
							;
					}
					else
					{
						union_task_fields
							<< type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
							;
					}
				}
            }
        }
    }

    // Define the union
    {
        if (!_enclosing_function_def_tree.is_valid())
        {
            _enclosing_function_def_tree = _stmt.get_ast().get_enclosing_function_definition();
        }

        AST_t union_def_tree = union_definition.parse_declaration(_enclosing_function_def_tree, _stmt.get_scope_link());
        _enclosing_function_def_tree.prepend(union_def_tree);
    }

    task_counters
        << "int " << global_task_index_name << " = 0;" 
        ; 

    GuardTaskGeneratorBundled guard_task_generator(_stmt.get_scope_link(), guard_task_info, _bundling_amount);
    bundled_tasks << _stmt.get_ast().prettyprint_with_callback(guard_task_generator);

    BundleGenerator bundle_gen(guard_task_info, _stmt.get_scope_link(), _bundling_amount);
    Source clear_indexes;
    bundle_remainder
        << "if (" << global_task_index_name << " != 0)"
        << "{"
        << bundle_gen.generate_bundle(clear_indexes, /*unroll=*/ false)
        // << clear_indexes
        << "}"
        ;

    return result;
}
