/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
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

static TL::Source get_timing_code()
{
	TL::Source result;
	result
		<< "_hlt_number_of_bundles++;"
		<< "{"
		<< "double _tmp = omp_get_wtime();"
		<< "_hlt_accum_time_bundle += (_tmp - _hlt_start_bundle);"
		<< "_hlt_start_bundle = _tmp;"
		<< "}"
		;
	return result;
}

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

        Source generate_bundle(Source &clear_indexes, bool unroll = false, bool empty = false, bool last_bundle = false)
        {
            Source try_to_run_every_task;
            Source firstprivate_clause_src, firstprivate_args, code_of_all_tasks;
            Source switch_structure, loop_header, loop_end;

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

			if (!last_bundle)
			{
				task_key
					<< " task_key(" << task_log_name << ")"
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
					<< "{ }"
					;
			}
			else
			{
                Source instrumentation_bundl_before, instrumentation_bundl_after;
                Source instrumentation_task_before, instrumentation_task_after;
				task_body
					<< "{"
                    << instrumentation_bundl_before
					<< "int " << current_index_name << "  = 0;"
					<< loop_header
					<< switch_structure
					<< loop_end
                    << instrumentation_bundl_after
					<< "}"
					;

                if (HLT::enable_instrumentation)
                {
                    instrumentation_bundl_before
                        << "mintaka_event(" << (int)HLT::TASK_OVERHEAD << ", 1);"
                        ;
                    instrumentation_bundl_after
                        << "mintaka_event(" << (int)HLT::TASK_OVERHEAD << ", 0);"
                        ;
                    instrumentation_task_before
                        << "mintaka_event(" << (int)HLT::TASK_OVERHEAD << ", 0);"
                        << "mintaka_event(" << (int)HLT::TASK_CODE << ", 1);"
                        ;
                    instrumentation_task_after
                        << "mintaka_event(" << (int)HLT::TASK_CODE << ", 0);"
                        << "mintaka_event(" << (int)HLT::TASK_OVERHEAD << ", 1);"
                        ;
                }

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
					int unroll_extent = _bundling_amount;
					if (_bundling_amount > 128)
					{
						unroll_extent = 128;

						loop_header
							<< "while (" << current_index_name << " < " << _bundling_amount << ")"
							<< "{"
							;

						loop_end
							<< "}"
							;
					}
					for (int i = 0; i < unroll_extent; i++)
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
					PragmaCustomConstruct current_task = it->get_task();
					PragmaCustomClause current_firstprivate_clause = current_task.get_clause("firstprivate");
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
                        <<    instrumentation_task_before
						<<    extended_task_body
                        <<    instrumentation_task_after
						<<    task_cleanup
						<<    "break;"
						<< "}"
						;

					extended_task_body << replacements.replace(current_task.get_statement());
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
		bool _do_not_create_tasks;
		bool _timing;
    public:
		GuardTaskGeneratorBundled(ScopeLink sl, const GuardTaskInfo& info, 
				int bundling_amount, bool do_not_create_tasks, bool timing)
            : _sl(sl), 
            _info(info), 
            _bundling_amount(bundling_amount),
			_task_num(new int(0)),
			_do_not_create_tasks(do_not_create_tasks),
			_timing(timing)
        {
        }

		~GuardTaskGeneratorBundled()
		{
			delete _task_num;
		}

        virtual AST_t::callback_result do_(GuardTaskGeneratorBundled::ArgType a) const
        {
            if (is_pragma_custom_construct("omp", "task", a, _sl))
            {
				(*_task_num)++;
				
                Source result;
				result
					<< "{"
					;
                // Capture current firstprivate values
                // FIXME -> We are relying on explicit firstprivates
                PragmaCustomConstruct task_construct(a, _sl);
                int task_id = _info.get_task_id(task_construct);
                PragmaCustomClause firstprivate_clause = task_construct.get_clause("firstprivate");

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

				Source timing_code;
				if (_timing)
				{
					timing_code = get_timing_code();
				}

                result
                    << global_task_index_name << "++;"
                    << "if (" << global_task_index_name << "== " << _bundling_amount << ")"
                    << "{"
					<<     timing_code
                    <<     try_to_run_every_task
                    <<     global_task_index_name << " = 0;"
                    <<     clear_indexes
                    << "}"
                    ;

                BundleGenerator bundle_gen(_info, _sl, _bundling_amount);

				if (!(this->_do_not_create_tasks))
				{
					try_to_run_every_task
						<< bundle_gen.generate_bundle(clear_indexes, /* unroll= */ true, /* empty */ ((*_task_num) != 1))
						;
				}
				else
				{
					std::cerr << "Do not create task " << __FILE__ << ":" << __LINE__ << std::endl;
				}

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

        PragmaCustomConstruct task_construct = guarded_task.get_task();
        PragmaCustomClause firstprivate_clause = task_construct.get_clause("firstprivate");

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

	if (_timing)
	{
		task_counters << "double _hlt_start_bundle = omp_get_wtime();"
			;
	}

    GuardTaskGeneratorBundled guard_task_generator(_stmt.get_scope_link(), 
			guard_task_info, _bundling_amount, _do_not_create_tasks, _timing);
    bundled_tasks << _stmt.get_ast().prettyprint_with_callback(guard_task_generator);

    BundleGenerator bundle_gen(guard_task_info, _stmt.get_scope_link(), _bundling_amount);
    Source clear_indexes, bundle_code;

	Source timing_code ;
	
	if (_timing)
	{
		timing_code = get_timing_code();
	}
	
    bundle_remainder
        << "if (" << global_task_index_name << " != 0)"
        << "{"
		<< timing_code
		<< bundle_code
        << "}"
        ;

	if (!(this->_do_not_create_tasks))
	{
		bundle_code << bundle_gen.generate_bundle(clear_indexes, /*unroll=*/ false, /* empty */ false, /* last_bundle = */ true );
	}
	else
	{
		std::cerr << "Do not create task " << __FILE__ << ":" << __LINE__ << std::endl;
	}

    return result;
}
