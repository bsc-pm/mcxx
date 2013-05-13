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

const std::string TASK_PREDICATION_COUNTER("hlt.task_predication");

struct GuardTaskGenerator : Functor<TL::AST_t::callback_result, TL::AST_t>
{
    private:
        ScopeLink _sl;
        GuardTaskInfo& _info;
    public:
        GuardTaskGenerator(ScopeLink sl, GuardTaskInfo& info)
            : _sl(sl), _info(info)
        {
        }

        virtual AST_t::callback_result do_(GuardTaskGenerator::ArgType a) const
        {
            if (is_pragma_custom_construct("omp", "task", a, _sl))
            {
                Source result;
                Source predicate_name;

                result
                    << "{"
                    ;

                predicate_name
                    << "_task_guard_" << _info._num_tasks
                    ;

                result
					<< "_global_pred_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) << " = "
                    << _info.get_guard_struct_var_name() << "." << predicate_name << "= 1;"
                    ;

                // Capture current firstprivate values
                // FIXME -> We are relying on explicit firstprivates
                PragmaCustomConstruct task_construct(a, _sl);
                PragmaCustomClause firstprivate_clause = task_construct.get_clause("firstprivate");

                ObjectList<GuardedTask::additional_var> additional_vars;

                if (firstprivate_clause.is_defined())
                {
                    ObjectList<TL::IdExpression> vars = firstprivate_clause.id_expressions();

                    for (ObjectList<TL::IdExpression>::iterator it = vars.begin();
                            it != vars.end();
                            it++)
                    {
                        Symbol sym = it->get_symbol();
                        Source new_name;

						Type type = sym.get_type();

						C_LANGUAGE()
						{
							new_name
								<< "(_pred_info_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) 
								<< "[" << _info._num_tasks << "]._task_" << _info._num_tasks << "." << sym.get_name() << ")"
								;
							result 
								<< new_name << " = " << (*it) << ";"
								;
						}
						CXX_LANGUAGE()
						{
							if (type.is_class())
							{
								new_name
									<< "(_pred_info_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) 
									<< "[" << _info._num_tasks << "]._task_" << _info._num_tasks << "._pt_" << sym.get_name() << ")"
									;
								result
									<< "new((void*)(" << new_name << "))" 
									<< type.get_declaration(sym.get_scope(), "") << "(" << (*it) << ");"
									;
							}
							else
							{
								new_name
									<< "(_pred_info_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) 
									<< "[" << _info._num_tasks << "]._task_" << _info._num_tasks << "." << sym.get_name() << ")"
									;
								result 
									<< new_name << " = " << sym.get_name() << ";"
									;
							}
						}

                        GuardedTask::additional_var new_var(new_name, sym);
                        additional_vars.append(new_var);
                    }
                }
                
                GuardedTask new_guarded_task(task_construct, additional_vars, predicate_name);
                _info._guarded_task_list.append(new_guarded_task);

                _info._num_tasks++;

                result
                    << "}"
                    ;

                return AST_t::callback_result(true, result);
            }
            else 
                return AST_t::callback_result(false, "");
        }
};

Source TaskAggregation::do_predicated_aggregation()
{
    CounterManager::get_counter(TASK_PREDICATION_COUNTER)++;

    GuardTaskInfo guard_task_info;
    GuardTaskGenerator guard_task_generator(_stmt.get_scope_link(), guard_task_info);

    Source result, temporal_values_declarations, guarded_tasks, predicated_task;
    Source guard_struct_src, guard_struct_fields, guard_struct_name, guard_struct_var_decl;
	Source predicated_info_struct, predicated_info_var;

    result
        << "{"
        << guard_struct_var_decl
		<< predicated_info_var
        << temporal_values_declarations
        << guarded_tasks
        << predicated_task
        << "}"
        ;

    guard_struct_src
        << "struct " << guard_struct_name
        << "{"
        << guard_struct_fields
        << "};"
		<< predicated_info_struct
        ;

    // Number of guard structs created so far
    if (!_enclosing_function_def_tree.is_valid())
    {
        _enclosing_function_def_tree = _stmt.get_ast().get_enclosing_function_definition();
    }
    FunctionDefinition enclosing_function_def(_enclosing_function_def_tree, _stmt.get_scope_link());

    guard_struct_name 
        << "_guard_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) 
        ;

    Source guard_struct_var_name = guard_task_info.get_guard_struct_var_name();
    guard_struct_var_decl << "struct " << guard_struct_name << " " << guard_struct_var_name << " = { 0 };"
		<< "char _global_pred_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) << " = 0;"
        ;

    // This fills all the guard task info, before it won't contain any guarded task
    guarded_tasks << _stmt.get_ast().prettyprint_with_callback(guard_task_generator);

    ObjectList<GuardedTask> guarded_task_list = guard_task_info.get_guarded_tasks();

    Source firstprivate_clause, firstprivate_args, predicated_body;
	Source task_construct;
    predicated_task
		<< "if (_global_pred_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) << ")"
		<< "{"
		<< task_construct
		<< "}"
        ;

	if (!_do_not_create_tasks)
	{
		task_construct
			<< "#pragma omp task" << firstprivate_clause << "\n"
			<< "{"
			<<    predicated_body
			<< "}"
			;
	}

    if (HLT::enable_instrumentation)
    {
        predicated_body
            << "mintaka_event(" << (int)HLT::TASK_OVERHEAD << ", 1);"
            ;
    }

	predicated_info_struct
		<< "union _pred_info_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) << "_t { " ;
	;

        
    Source instrumentation_before, instrumentation_after;
    if (HLT::enable_instrumentation)
    {
        instrumentation_before
            << "mintaka_event(" << (int)HLT::TASK_OVERHEAD << ", 0);"
            << "mintaka_event(" << (int)HLT::TASK_CODE << ", 1);"
            ;
        instrumentation_after
            << "mintaka_event(" << (int)HLT::TASK_CODE << ", 0);"
            << "mintaka_event(" << (int)HLT::TASK_OVERHEAD << ", 1);"
            ;
    }

	int num_tasks = 0;
    for (ObjectList<GuardedTask>::iterator it = guarded_task_list.begin();
            it != guarded_task_list.end();
            it++)
    {
        PragmaCustomConstruct task = it->get_task();

        Source replaced_body, local_binding, local_cleanup;

        Statement body = task.get_statement();
        predicated_body
            << "if (" << guard_task_info.get_guard_struct_var_name() << "." << it->get_predicate_name() << ")"
            << "{"
			<< local_binding
            << instrumentation_before
            << replaced_body
            << instrumentation_after
			<< local_cleanup
            << "}"
            ;

        guard_struct_fields
            << "char " << it->get_predicate_name() << ":1;"
            ;


        ReplaceSrcIdExpression replacements(_stmt.get_scope_link());
        ObjectList<GuardedTask::additional_var> additional_vars = it->get_additional_vars();

		if (!additional_vars.empty())
		{
			predicated_info_struct
				<< "struct {"
				;
		}
		
        for (ObjectList<GuardedTask::additional_var>::iterator it_additional_var = additional_vars.begin();
                it_additional_var != additional_vars.end();
                it_additional_var++)
        {
            GuardedTask::additional_var& additional_var(*it_additional_var);


            Symbol &sym(additional_var.second);
			Type type = sym.get_type();

			C_LANGUAGE()
			{
				predicated_info_struct
					<< type.get_declaration(sym.get_scope(), sym.get_name()) << ";";
				replacements.add_replacement(additional_var.second, additional_var.first);
			}
			CXX_LANGUAGE()
			{
				if (type.is_class())
				{
					Source name_class_ptr;
					name_class_ptr
						<<  "_pt_" << sym.get_name() 
						;
					predicated_info_struct
						<< "char " << name_class_ptr << "[sizeof(" << type.get_declaration(sym.get_scope(), "") << ")];"
						;

					Symbol class_symbol = type.get_symbol();
					Type type = sym.get_type();
					Type ref_type = type.get_reference_to();
					Type ptr_type = type.get_pointer_to();

					local_binding
						<< ref_type.get_declaration(sym.get_scope(), sym.get_name()) 
						<< "( *(" 
						<< "(" << ptr_type.get_declaration(sym.get_scope(), "") << ")"
						<< "_pred_info_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) 
						<< "[" << num_tasks << "]._task_" << num_tasks << "."
						<< name_class_ptr 
						<< "));"
						;

					local_cleanup
						<< sym.get_name() << "." << type.get_declaration(sym.get_scope(), "") << "::~" << class_symbol.get_name() << "();"
						;
					// No replacement
				}
				else
				{
					predicated_info_struct
						<< type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
						;
					replacements.add_replacement(additional_var.second, additional_var.first);
				}
			}
        }

		if (!additional_vars.empty())
		{
			predicated_info_struct
				<< "} _task_" << num_tasks << ";"
				;
		}

		num_tasks++;

		replaced_body << replacements.replace(body);
    }

    if (HLT::enable_instrumentation)
    {
        predicated_body
            << "mintaka_event(" << (int)HLT::TASK_OVERHEAD << ", 0);"
            ;
    }

	Source pred_info_name;
	pred_info_name
		<< "_pred_info_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER)
		;

	predicated_info_struct
		<< "};" 
		;

	predicated_info_var
		<< "union _pred_info_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) << "_t " 
		<<  pred_info_name << "[" << num_tasks << "];"
		;

	firstprivate_args.append_with_separator(pred_info_name, ",");
	firstprivate_args.append_with_separator(guard_struct_var_name, ",");
	firstprivate_clause << " firstprivate(" << firstprivate_args << ")"
		;

    // Now parse the guard struct and prepend it
    AST_t guard_struct_tree = guard_struct_src.parse_declaration(enclosing_function_def.get_ast(),
            _stmt.get_scope_link());

    _enclosing_function_def_tree.prepend(guard_struct_tree);

    return result;
}
