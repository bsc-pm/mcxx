/*--------------------------------------------------------------------
  (C) Copyright 2020-2020 Barcelona Supercomputing Center
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

#include "tl-nanos6-openacc-functions.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-omp-core.hpp"
#include "cxx-cexpr.h"

namespace TL
{
namespace Nanos6
{

OpenACCTasks::OpenACCTasks()
{
    set_phase_name("Nanos6/OpenACC specific pass");
    set_phase_description(
        "This pass makes sure all the device specific information that Nanos6 "
        "has is properly passed to OpenACC directives.");
}

void OpenACCTasks::append_async_parameter(TL::Symbol &sym)
{
	TL::Symbol new_param;	// Our new argument

	// We need to get the current parameters and their types;
	// The type of a function is: ret(urn)_type + List<argument_types>
	//
	// Therefore we will need to append the type to the second part (the List)
	// and update the function's (sym) Type

	TL::ObjectList<TL::Symbol> current_params = sym.get_related_symbols();
	TL::Type func_type = sym.get_type();
	TL::Type ret_type = func_type.returns();
	TL::ObjectList<TL::Type> param_types = func_type.parameters();

	// Unfortunately sym.get_related_scope() breaks, so we work around it by
	// getting the first parameter's scope, which is the one we need.
	TL::Scope sc = current_params.begin()->get_scope();

	new_param = sc.new_symbol("nanos6_mcxx_async_queue");
	new_param.get_internal_symbol()->kind = SK_VARIABLE;
	new_param.set_type(TL::Type::get_int_type());	// We know our type will be an int
	symbol_entity_specs_set_is_user_declared(new_param.get_internal_symbol(), 1);

	// Append new_param type to parameters' types list
	param_types.append(new_param.get_type());
	// Update the function Type to include the new one too
	func_type = ret_type.get_function_returning(param_types, param_types);
	sym.set_type(func_type);

	// Append the new_param Symbol to the parameters
	current_params.append(new_param);
	sym.set_related_symbols(current_params);
}

// Use UnknownPragmaVisitor to detect OpenACC pragmas and append the async clause
class UnknownPragmaVisitor : public Nodecl::ExhaustiveVisitor<void>
{
	private:
		TL::OmpSs::FunctionTaskSet &ompss_task_functions;
		TL::ObjectList<TL::Symbol> statements;

		std::string append_async(std::string pragma_str)
		{
			const std::string append_str = " async(nanos6_mcxx_async_queue)";
			// get the substring from the point acc starts, or it will result
			// in appending multiple #pragma #pragma
			std::string ret = pragma_str.substr(pragma_str.find("acc"));
			size_t length = ret.find("\n");
			ret = ret.erase(length, 1);	// remove eol to append the async
			ret += append_str;
			return ret;
		}

	public:
		UnknownPragmaVisitor(TL::OmpSs::FunctionTaskSet &ompss_task_functions_)
			: ompss_task_functions(ompss_task_functions_)
		{
		}

		virtual void visit(const Nodecl::UnknownPragma &node)
		{
			if (node.prettyprint().find(" acc parallel") != std::string::npos ||
					node.prettyprint().find(" acc kernels") != std::string::npos) {
				ERROR_CONDITION(
						node.prettyprint().find("async") != std::string::npos,
						"OpenACC async clause already present, please remove from user code", 0);
				std::string acc_pragma_str = node.prettyprint();
				acc_pragma_str = append_async(acc_pragma_str);
				const_cast<Nodecl::UnknownPragma&>(node).set_text(acc_pragma_str);
			}
		}
};

class FunctionDefinitionsVisitor : public Nodecl::ExhaustiveVisitor<void>
{
  private:
    TL::OmpSs::FunctionTaskSet &ompss_task_functions;
    TL::ObjectList<TL::Symbol> openacc_functions;

  public:
    FunctionDefinitionsVisitor(TL::OmpSs::FunctionTaskSet &ompss_task_functions_)
        : ompss_task_functions(ompss_task_functions_)
    {
    }

    virtual void visit(const Nodecl::FunctionCode &node)
    {
        TL::Symbol sym = node.get_symbol();
        if (!ompss_task_functions.is_function_task(sym))
            return;

        TL::OmpSs::FunctionTaskInfo &task_info
            = ompss_task_functions.get_function_task(sym);

        TL::OmpSs::TargetInfo &target_info = task_info.get_target_info();
        TL::ObjectList<std::string> devices = target_info.get_device_list();

        if (devices.contains("openacc"))
        {
            if (devices.size() == 1)
            {
                openacc_functions.append(sym);
            }
            else
            {
                error_printf_at(
                    node.get_locus(),
                    "OpenACC function task is using more than one device\n");
            }
        }
    }

    TL::ObjectList<TL::Symbol> get_openacc_functions_definitions() const
    {
        return openacc_functions;
    }
};

// We will use this visitor to introduce a check at the end of each
// OpenACC function. If provided async queue number is 0, it means we are
// in some final context, the queue is not managed by the runtime, so
// we'll just acc wait(0) for it. We don't integrate it to the
// FunctionDefinitionsVisitoras we need it to be the last thing we run,
// after the pragma editing.
class FunctionCodeVisitor : public Nodecl::ExhaustiveVisitor<void>
{
  private:
	TL::OmpSs::FunctionTaskSet &ompss_task_functions;

  public:
	FunctionCodeVisitor(TL::OmpSs::FunctionTaskSet &ompss_task_functions_)
		: ompss_task_functions(ompss_task_functions_)
	{
	}

	virtual void visit(const Nodecl::FunctionCode &node)
	{
		TL::Symbol sym = node.get_symbol();
		if (!ompss_task_functions.is_function_task(sym))
			return;

		TL::Symbol async = sym.get_related_symbols().begin()->
			get_scope().get_symbol_from_name("nanos6_mcxx_async_queue");
		// hack-ish way to get context, as sym.get_related_scope segfaults

		ERROR_CONDITION(!async.is_valid(),
				"async queue symbol not found\n", 0);

		Nodecl::Context context = node.get_statements().as<Nodecl::Context>();
		Nodecl::List statements = context.get_in_context().as<Nodecl::List>();
		Nodecl::CompoundStatement cm_statement = statements.front().as<Nodecl::CompoundStatement>();

		// create our pragma statement for OpenACC wait
		Nodecl::NodeclBase pragma_acc_wait = Nodecl::UnknownPragma::make("acc wait(0)");

		// create the if (nanos6_mcxx_async_queue == 0) check
		Nodecl::NodeclBase if_async_zero = Nodecl::IfElseStatement::make(
				Nodecl::Equal::make(
					async.make_nodecl(true),	// get symbol for comparison
					const_value_to_nodecl_with_basic_type(	// construct a const 0
						const_value_get_signed_int(0),		// for right side
						get_size_t_type()),
					get_bool_type()),
				Nodecl::List::make(		// List is required as a legacy of Fortran compatibility
					Nodecl::CompoundStatement::make( // Create a compound statement inside 'if' block,
						Nodecl::List::make(
							pragma_acc_wait,				// that will contain our pragma
							Nodecl::EmptyStatement::make()),// and an empty statement
						Nodecl::NodeclBase::null())),		// 'else' will be ommited
				Nodecl::NodeclBase::null());

		Nodecl::List stmt_list = cm_statement.get_statements().as<Nodecl::List>();
		//Add error condition if list is empty, which is unlikely
		ERROR_CONDITION(stmt_list.empty(), "Statement list appears empty\n", 0);
		stmt_list.append(if_async_zero);
	}
};

// Detect calls to openacc task functions and append the asymc queue to args
class FunctionCallsVisitor : public Nodecl::ExhaustiveVisitor<void>
{
  private:
    TL::OmpSs::FunctionTaskSet &ompss_task_functions;
    TL::ObjectList<TL::Symbol> openacc_functions;

  public:
    FunctionCallsVisitor(TL::OmpSs::FunctionTaskSet &ompss_task_functions_)
        : ompss_task_functions(ompss_task_functions_)
    {
    }

    virtual void visit(const Nodecl::FunctionCall &node)
    {
        Nodecl::NodeclBase called = node.get_called();
        if (!called.is<Nodecl::Symbol>())
            return;

        // Note: no need to walk the argument expressions because taks functions
        // can only be void so a call to one will always be a top-level
        // expression.

        TL::Symbol sym = called.get_symbol();
        if (!ompss_task_functions.is_function_task(sym))
            return;

        TL::OmpSs::FunctionTaskInfo &task_info
            = ompss_task_functions.get_function_task(sym);

        TL::OmpSs::TargetInfo &target_info = task_info.get_target_info();
        TL::ObjectList<std::string> devices = target_info.get_device_list();

		if (devices.contains("openacc"))
		{
			if (devices.size() == 1)
			{
				openacc_functions.append(sym);
			}
			else
			{
				error_printf_at(
						node.get_locus(),
						"OpenACC function task is using more than one device\n");
			}

			// Now we will append the new 'nanos6_mcxx_async' argument:
			// Check if symbol is present in the scope;
			// 	if yes, append it to function call arguments;
			// 	if not, we are probably in the nanos6_in_final block, so append 0
			// 	(queue 0 is not assigned by the runtime; it starts from 1, so we
			// 	can add a manual wait for it in the function code).
			//	Note: In case we are in the nanos6_in_final check of another task block that
			//		has a nanos6_mcxx_async symbol, then using this keeps the semantics intact
			//		as OpenACC queues are FIFO, so we can use the same queue with the parent task.
			//
			// set its type (to int always);
			// use Nodecl::Conversion as is the case in all arguments

			Nodecl::List arguments = node.get_arguments().as<Nodecl::List>();
			TL::Scope sc = node.retrieve_context();
			const std::string new_arg_name = "nanos6_mcxx_async";
			TL::Symbol async_symbol = sc.get_symbol_from_name(new_arg_name);
			if (async_symbol.is_valid()) {
				arguments.append(Nodecl::Conversion::make(
							async_symbol.make_nodecl(/*set_ref_type*/ true, node.get_locus()),
							TL::Type::get_int_type(),
							node.get_locus()));
			}
			else {
				arguments.append(Nodecl::Conversion::make(
							const_value_to_nodecl(const_value_get_zero(4, 1)),
							TL::Type::get_int_type(),
							node.get_locus()));
			}
		}
    }

    TL::ObjectList<TL::Symbol> get_openacc_functions_called() const
    {
        return openacc_functions;
    }
};

void OpenACCTasks::run(DTO &dto)
{
    Nodecl::NodeclBase translation_unit
        = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

    std::shared_ptr<TL::OmpSs::FunctionTaskSet> ompss_task_functions
        = std::static_pointer_cast<TL::OmpSs::FunctionTaskSet>(
            dto["openmp_task_info"]);
    ERROR_CONDITION(
        !ompss_task_functions, "OmpSs Task Functions not in the DTO", 0);

	// Detect function definitions
    FunctionDefinitionsVisitor functions_definition_visitor(
        *ompss_task_functions);
    functions_definition_visitor.walk(translation_unit);

	TL::ObjectList<TL::Symbol> acc_functions =
		functions_definition_visitor.get_openacc_functions_definitions();
	// Walk definitions to append the new parameter
	for (auto f : acc_functions)
		append_async_parameter(f);

	// Detect function calls and append the queue argument to each one
    FunctionCallsVisitor function_calls_visitor(
        *ompss_task_functions);
    function_calls_visitor.walk(translation_unit);

	// Search for unknown (potentially OpenACC) pragmas in the detected tasks only
	UnknownPragmaVisitor unknown_pragma_visitor(
			*ompss_task_functions);
	// Append the queue == 0 check for nanos6_in_final
	FunctionCodeVisitor code_visitor(
			*ompss_task_functions);
	for (auto f : acc_functions) {
		unknown_pragma_visitor.walk(f.get_function_code());
		code_visitor.walk(f.get_function_code());
	}

}

} // namespace Nanos6
} // namespace TL

EXPORT_PHASE(TL::Nanos6::OpenACCTasks)
