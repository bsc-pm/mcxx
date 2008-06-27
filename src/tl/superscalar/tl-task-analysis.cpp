/*
    Cell/SMP superscalar Compiler
    Copyright (C) 2007 Barcelona Supercomputing Center

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; version 2.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/


#include <iostream>

#include "tl-langconstruct.hpp"

#include "tl-source-bits.hpp"
#include "tl-task-analysis.hpp"
#include "tl-type-utils.hpp"


namespace TL
{
	CompilerPhase::PhaseStatus TaskAnalysis::_status;
	
	IdExpression TL::TaskAnalysis::get_base_id_expression(Expression expression)
	{
		if (expression.is_id_expression())
		{
			return expression.get_id_expression();
		}
		else if (expression.is_array_subscript())
		{
			return get_base_id_expression(expression.get_subscripted_expression());
		}
		
		throw NotFoundException();
	}
	
	
	ObjectList<Expression> TL::TaskAnalysis::get_array_access_indices(Expression expression)
	{
		ObjectList<Expression> result;
		if (expression.is_array_subscript())
		{
			result = get_array_access_indices(expression.get_subscripted_expression());
			result.push_back(expression.get_subscript_expression());
		}
		return result;
	}
	
	
	void TL::TaskAnalysis::process_task(PragmaCustomConstruct construct)
	{
		if (construct.is_function_definition())
		{
			process_task_definition(construct);
		}
		else
		{
			process_task_declaration(construct);
		}
	}
	
	
	std::string TL::TaskAnalysis::direction_to_name(ParameterDirection direction)
	{
		std::string direction_name;
		
		switch (direction)
		{
			case INPUT_DIR:
				direction_name = std::string("input");
				break;
			case OUTPUT_DIR:
				direction_name = std::string("output");
				break;
			case INOUT_DIR:
				direction_name = std::string("inout");
				break;
			default:
				direction_name = std::string("INTERNAL ERROR");
				break;
		}
		
		return direction_name;
	}
	
	
	void TL::TaskAnalysis::handle_definition_parameter(PragmaCustomConstruct &construct, FunctionDefinition &task_definition, FunctionInfo &function_info, std::string const &parameter_specification, ParameterDirection direction)
	{
		std::string const direction_name = direction_to_name(direction);
		
		Symbol parameter_symbol = Symbol::invalid();
		std::string parameter_name;
		
		try
		{
			Type parameter_type = SourceBits::handle_superscalar_declarator(task_definition.get_function_body().get_ast(), construct.get_scope_link(), parameter_specification, parameter_symbol);
			if (!parameter_symbol.is_parameter())
			{
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
			}
			unsigned int parameter_index = parameter_symbol.get_parameter_position();
			parameter_name = parameter_symbol.get_name(); // FIXME: C++ get_qualified_name()?
			
			ParameterInfo &parameter_info = function_info._parameters[parameter_index];
			
			parameter_info._symbol = parameter_symbol;
			parameter_info._augmented_definition_type = parameter_type;
			parameter_info._augmented_definition_locus = construct.get_ast().get_locus();
			parameter_info._directionality_definition_count++;
			
			if (parameter_info._directionality_definition_count != function_info._task_definition_count)
			{
				std::cerr << parameter_info._augmented_definition_locus << " Error: parameter '" << parameter_name <<"' declared in " << direction_name << "' clause appears more than once in the directionality clauses." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			
			if (parameter_type.is_pointer() && parameter_type.points_to().is_void() && direction != INPUT_DIR)
			{
				std::cerr << parameter_info._augmented_definition_locus << " Error: parameter '" << parameter_name <<"' declared in " << direction_name << "' clause is an opaque pointer and can only appear in the input clause." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			
			if (!parameter_type.is_non_derived_type() && !parameter_type.is_array() && !parameter_type.is_pointer())
			{
				std::cerr << parameter_info._augmented_definition_locus << " Error: parameter '" << parameter_name <<"' declared in " << direction_name << "' clause is a derived type and cannot be passed by value." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			
			if (parameter_info._augmented_declaration_type.is_valid())
			{
				if (!TypeUtils::parameter_types_match(parameter_type, parameter_info._augmented_declaration_type, construct.get_scope_link()))
				{
					std::cerr << parameter_info._augmented_definition_locus << " Error: parameter '" << parameter_name <<"' declared in " << direction_name << "' clause as '" << parameter_type.get_simple_declaration(task_definition.get_scope(), parameter_name) << "' conflicts with previous declaration." << std::endl;
					std::cerr << parameter_info._augmented_declaration_locus << " previously declared as '"
						<< parameter_info._augmented_declaration_type.get_simple_declaration(function_info._declaration_scope, parameter_name)
						<< "'." << std::endl;
					function_info._has_errors = true;
					TaskAnalysis::fail();
				}
				if (parameter_info._direction != direction)
				{
					std::cerr << parameter_info._augmented_definition_locus << " Error: Inconsistent directionality for parameter '" << parameter_name <<"', declared as " << direction_name << std::endl;
					std::cerr << parameter_info._augmented_declaration_locus << " previously declared as " << direction_to_name(parameter_info._direction) << "." << std::endl;
					function_info._has_errors = true;
					TaskAnalysis::fail();
				}
			}
			parameter_info._direction = direction;
		}
		catch (SyntaxErrorException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Syntax error while parsing the " << direction_name << " clause." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		catch (UnknownParameterException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Unknown parameter '" << parameter_name << "' in " << direction_name << " clause." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		catch (AmbiguousParameterException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Ambiguous parameter '" << parameter_name << "' in " << direction_name << " clause." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		catch (InvalidTypeException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Invalid type for parameter '" << parameter_name << "' in " << direction_name << " clause." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		catch (InvalidDimensionSpecifiersException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Invalid dimension specifiers for parameter '" << parameter_name << "' in " << direction_name << " clause. Please check that they are only present in the task directive or in the function definition." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
	}
	
	
	void TL::TaskAnalysis::handle_declaration_parameter(PragmaCustomConstruct &construct, DeclaredEntity &task_declaration, FunctionInfo &function_info, std::string const &parameter_specification, ParameterDirection direction)
	{
		std::string const direction_name = direction_to_name(direction);
		
		Symbol parameter_symbol = Symbol::invalid();
		std::string parameter_name;
		
		try {
			ObjectList<ParameterDeclaration> parameter_declarations = task_declaration.get_parameter_declarations();
			if (parameter_declarations.empty()) {
				std::cerr << construct.get_ast().get_locus() << " Error: Specified an " << direction_name << " parameter in a task without parameters." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			
			AST_t reference_ast = parameter_declarations[0].get_ast();
			Type parameter_type = SourceBits::handle_superscalar_declarator(reference_ast, construct.get_scope_link(), parameter_specification, parameter_symbol);
			if (!parameter_symbol.is_parameter())
			{
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
			}
			parameter_name = parameter_symbol.get_name(); // FIXME: C++ get_qualified_name()?
			unsigned int parameter_index = parameter_symbol.get_parameter_position();
			
			ParameterInfo &parameter_info = function_info._parameters[parameter_index];
			parameter_info._symbol = parameter_symbol;
			parameter_info._augmented_declaration_type = parameter_type;
			parameter_info._augmented_declaration_locus = construct.get_ast().get_locus();
			parameter_info._directionality_declaration_count++;
			
			if (parameter_info._directionality_declaration_count != function_info._task_declaration_count)
			{
				std::cerr << parameter_info._augmented_declaration_locus << " Error: parameter '" << parameter_name <<"' declared in " << direction_name << "' clause appears more than once in the directionality clauses." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			
			if (parameter_type.is_pointer() && parameter_type.points_to().is_void() && direction != INPUT_DIR)
			{
				std::cerr << parameter_info._augmented_declaration_locus << " Error: parameter '" << parameter_name << "' declared in " << direction_name << "' clause is an opaque pointer and can only appear in the input clause." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			
			if (!parameter_type.is_non_derived_type() && !parameter_type.is_array() && !parameter_type.is_pointer())
			{
				std::cerr << parameter_info._augmented_definition_locus << " Error: parameter '" << parameter_name <<"' declared in " << direction_name << "' clause is a derived type and cannot be passed by value." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			
			if (parameter_info._augmented_definition_type.is_valid())
			{
				if (!TypeUtils::parameter_types_match(parameter_type, parameter_info._augmented_definition_type, construct.get_scope_link()))
				{
					std::cerr << parameter_info._augmented_declaration_locus << " Error: parameter '" << parameter_name <<"' declared in " << direction_name << "' clause as '" << parameter_type.get_simple_declaration(task_declaration.get_scope(), parameter_name) << "' conflicts with previous declaration" << std::endl;
					std::cerr << parameter_info._augmented_definition_locus << " previously declared as '"
						<< parameter_info._augmented_definition_type.get_simple_declaration(function_info._definition_scope, parameter_name)
						<< "'." << std::endl;
					function_info._has_errors = true;
					TaskAnalysis::fail();
				}
				if (parameter_info._direction != direction)
				{
					std::cerr << parameter_info._augmented_declaration_locus << " Error: Inconsistent directionality for parameter '" << parameter_name <<"', declared as " << direction_name << std::endl;
					std::cerr << parameter_info._augmented_definition_locus << " previously declared as " << direction_to_name(parameter_info._direction) << "." << std::endl;
					function_info._has_errors = true;
					TaskAnalysis::fail();
				}
			}
			else if (parameter_info._direction != UNKNOWN_DIR)
			{
				std::cerr << parameter_info._augmented_declaration_locus << " Error: Inconsistent directionality for parameter '" << parameter_name <<"', declared as " << direction_name << std::endl;
				std::cerr << parameter_info._augmented_definition_locus << " previously declared as " << direction_to_name(parameter_info._direction) << "." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			
			parameter_info._direction = direction;
		}
		catch (SyntaxErrorException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Syntax error while parsing the " << direction_name << " clause." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		catch (UnknownParameterException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Unknown parameter '" << parameter_name << "' in " << direction_name << " clause." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		catch (AmbiguousParameterException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Ambiguous parameter '" << parameter_name << "' in " << direction_name << " clause." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		catch (InvalidTypeException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Invalid type for parameter '" << parameter_name << "' in " << direction_name << " clause." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		catch (InvalidDimensionSpecifiersException ex)
		{
			std::cerr << construct.get_ast().get_locus() << " Error: Invalid dimension specifiers for parameter '" << parameter_name << "' in " << direction_name << " clause. Please check that they are only present in the task directive or in the function definition." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
	}
	
	
	void TL::TaskAnalysis::process_task_definition(PragmaCustomConstruct construct)
	{
		FunctionDefinition task_definition(construct.get_declaration(), construct.get_scope_link());
		
		std::string function_name = task_definition.get_function_name().mangle_id_expression();
		if (!_function_map.contains(function_name))
		{
			std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
			throw FatalException();
		}
		
		FunctionInfo &function_info = _function_map[function_name];
		
		if (function_info._task_definition_count)
		{
			std::cerr << task_definition.get_ast().get_locus() << " Error: redefinition of task '" << function_name << "'." << std::endl;
			std::cerr << function_info._task_definition_locus << " previously defined here." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
			return;
		}
		
		if (function_info._has_ellipsis)
		{
			std::cerr << task_definition.get_ast().get_locus() << " Error: task '" << function_name << "' has a variable number of parameters." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
			return;
		}
		
		bool has_high_priority = construct.get_clause("highpriority").is_defined();
		
		if (has_high_priority && !construct.get_clause("highpriority").get_arguments().empty())
		{
			std::cerr << task_definition.get_ast().get_locus() << " Error: the 'highpriority' clause does not accept any parameter." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		
		if (function_info._task_declaration_count != 0)
		{
			if (function_info._has_high_priority != has_high_priority)
			{
				std::cerr << task_definition.get_ast().get_locus() << " Error: definition of task '" << function_name << "' has different priority that previous declaration." << std::endl;
				std::cerr << function_info._task_declaration_locus << " previously declared here." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
				return;
			}
		}
		
		function_info._is_task = true;
		function_info._has_high_priority = has_high_priority;
		function_info._task_definition_count++;
		function_info._task_definition_locus = task_definition.get_ast().get_locus();
		
		//
		// Check parameters
		//
		
		// Check that all parameters in the input clause exist.
		// Check and compute parameter types
		// Complete ParameterInfo data
		// Check that parameters match their corresponding ones in a previous task declaration
		ObjectList<std::string> input_parameters = construct.get_clause("input").get_arguments();
		for (ObjectList<std::string>::iterator it = input_parameters.begin(); it != input_parameters.end(); it++)
		{
			std::string const &parameter_specification = *it;
			handle_definition_parameter(construct, task_definition, function_info, parameter_specification, INPUT_DIR);
		}
		
		// Check that all parameters in the output clause exist.
		// Check and compute parameter types
		// Complete ParameterInfo data
		// Check that parameters match their corresponding ones in a previous task declaration
		ObjectList<std::string> output_parameters = construct.get_clause("output").get_arguments();
		for (ObjectList<std::string>::iterator it = output_parameters.begin(); it != output_parameters.end(); it++)
		{
			std::string const &parameter_specification = *it;
			handle_definition_parameter(construct, task_definition, function_info, parameter_specification, OUTPUT_DIR);
		}
		
		// Check that all parameters in the inout clause exist.
		// Check and compute parameter types
		// Complete ParameterInfo data
		// Check that parameters match their corresponding ones in a previous task declaration
		ObjectList<std::string> inout_parameters = construct.get_clause("inout").get_arguments();
		for (ObjectList<std::string>::iterator it = inout_parameters.begin(); it != inout_parameters.end(); it++)
		{
			std::string const &parameter_specification = *it;
			handle_definition_parameter(construct, task_definition, function_info, parameter_specification, INOUT_DIR);
		}
		
		
		// Check that all parameters have appeared in a clause
		// Check that output and inout parameters are either a pointer or an array
		unsigned int parameter_index = 0;
		for (ObjectList<ParameterInfo>::iterator it = function_info._parameters.begin(); it != function_info._parameters.end(); it++)
		{
			ParameterInfo &parameter_info = *it;
			if (parameter_info._direction == UNKNOWN_DIR)
			{
				std::cerr << construct.get_ast().get_locus() << " Error: parameter number " << parameter_index+1 << " in task '" << function_name << "' is not present in any direction clause." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			else if (parameter_info._direction == OUTPUT_DIR || parameter_info._direction == INOUT_DIR)
			{
				if (!parameter_info._definition_type.is_pointer() && !parameter_info._definition_type.is_array())
				{
					std::cerr << construct.get_ast().get_locus() << " Error: " << direction_to_name(parameter_info._direction) << " parameter number" << parameter_index+1 << " in task '" << function_name << "' must be either an array or a scalar passed through a pointer." << std::endl;
					function_info._has_errors = true;
					TaskAnalysis::fail();
				}
			}
			parameter_index++;
		}
	}
	
	
	void TL::TaskAnalysis::process_task_declaration(PragmaCustomConstruct construct)
	{
		Declaration general_declaration(construct.get_declaration(), construct.get_scope_link());
		ObjectList<DeclaredEntity> declared_entities = general_declaration.get_declared_entities();
		
		if (declared_entities.size() != 1)
		{
			std::cerr << construct.get_declaration().get_locus() << " Error: only one declaration per task construct is allowed." << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		DeclaredEntity declared_entity = *(declared_entities.begin());
		if (!declared_entity.is_functional_declaration())
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Error: the task construct can only be applied to functions." << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		std::string function_name = declared_entity.get_declared_entity().mangle_id_expression();
		
		if (declared_entity.functional_declaration_lacks_prototype())
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Error: task '" << function_name << "' declared without parameters. If there is none, please specify void." << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		if (!_function_map.contains(function_name))
		{
			std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
			throw FatalException();
		}
		
		FunctionInfo &function_info = _function_map[function_name];
		
		if (function_info._task_declaration_count != 0)
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Warning: redeclaration of task '" << function_name << "'." << std::endl;
			std::cerr << function_info._task_declaration_locus << " previously declared here." << std::endl;
		}
		
		if (function_info._has_ellipsis)
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Error: task '" << function_name << "' has a variable number of parameters." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
			return;
		}
		
		bool has_high_priority = construct.get_clause("highpriority").is_defined();
		
		if (has_high_priority && !construct.get_clause("highpriority").get_arguments().empty())
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Error: the 'highpriority' clause does not accept any parameter." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		
		if (function_info._task_definition_count != 0) {
			if (function_info._has_high_priority != has_high_priority) {
				std::cerr << declared_entity.get_ast().get_locus() << " Error: declaration of task '" << function_name << "' has different priority that previous definition." << std::endl;
				std::cerr << function_info._task_definition_locus << " previously defined here." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
				return;
			}
		}
		
		function_info._is_task = true;
		function_info._has_high_priority = has_high_priority;
		function_info._task_declaration_count++;
		function_info._task_declaration_locus = declared_entity.get_ast().get_locus();
		
		
		//
		// Check parameters
		//
		
		// Check that all parameters in the input clause exist.
		// Check and compute parameter types
		// Complete ParameterInfo data
		// Check that parameters match their corresponding ones in a previous task definition
		ObjectList<std::string> input_parameters = construct.get_clause("input").get_arguments();
		for (ObjectList<std::string>::iterator it = input_parameters.begin(); it != input_parameters.end(); it++)
		{
			std::string const &parameter_specification = *it;
			handle_declaration_parameter(construct, declared_entity, function_info, parameter_specification, INPUT_DIR);
		}
		
		// Check that all parameters in the output clause exist.
		// Check and compute parameter types
		// Complete ParameterInfo data
		// Check that parameters match their corresponding ones in a previous task definition
		ObjectList<std::string> output_parameters = construct.get_clause("output").get_arguments();
		for (ObjectList<std::string>::iterator it = output_parameters.begin(); it != output_parameters.end(); it++)
		{
			std::string const &parameter_specification = *it;
			handle_declaration_parameter(construct, declared_entity, function_info, parameter_specification, OUTPUT_DIR);
		}
		
		// Check that all parameters in the inout clause exist.
		// Check and compute parameter types
		// Complete ParameterInfo data
		// Check that parameters match their corresponding ones in a previous task definition
		ObjectList<std::string> inout_parameters = construct.get_clause("inout").get_arguments();
		for (ObjectList<std::string>::iterator it = inout_parameters.begin(); it != inout_parameters.end(); it++)
		{
			std::string const &parameter_specification = *it;
			handle_declaration_parameter(construct, declared_entity, function_info, parameter_specification, INOUT_DIR);
		}
		
		
		// Check that all parameters have appeared in a clause
		// Check that output and inout parameters are either a pointer or an array
		unsigned int parameter_index = 0;
		for (ObjectList<ParameterInfo>::iterator it = function_info._parameters.begin(); it != function_info._parameters.end(); it++)
		{
			ParameterInfo &parameter_info = *it;
			if (parameter_info._direction == UNKNOWN_DIR)
			{
				std::cerr << construct.get_ast().get_locus() << " Error: parameter number " << parameter_index+1 << " in task '" << function_name << "' is not present in any direction clause." << std::endl;
				function_info._has_errors = true;
				TaskAnalysis::fail();
			}
			else if (parameter_info._direction == OUTPUT_DIR || parameter_info._direction == INOUT_DIR)
			{
				if (!parameter_info._declaration_type.is_pointer() && !parameter_info._declaration_type.is_array())
				{
					std::cerr << construct.get_ast().get_locus() << " Error: " << direction_to_name(parameter_info._direction) << " parameter number " << parameter_index+1 << " in task '" << function_name << "' must be either an array or a scalar passed through a pointer." << std::endl;
					function_info._has_errors = true;
					TaskAnalysis::fail();
				}
			}
			parameter_index++;
		}
		
	}
	
	
	void TL::TaskAnalysis::process_target(PragmaCustomConstruct construct)
	{
		if (construct.is_function_definition())
		{
			process_target_on_definition(construct);
		}
		else
		{
			process_target_on_declaration(construct);
		}
		
	}
	
	
	void TL::TaskAnalysis::process_target_on_definition(PragmaCustomConstruct construct)
	{
		FunctionDefinition function_definition(construct.get_declaration(), construct.get_scope_link());
		
		std::string function_name = function_definition.get_function_name().mangle_id_expression();
		if (!_function_map.contains(function_name))
		{
			std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
			throw FatalException();
		}
		
		FunctionInfo &function_info = _function_map[function_name];
		
		function_info._has_coherced_sides = true;
		
		// FIXME: These names are too Cell specific
		PragmaCustomClause target_spu = construct.get_clause("spu");
		PragmaCustomClause target_ppu = construct.get_clause("ppu");
		
		if (target_spu.is_defined() && !target_spu.get_arguments().empty())
		{
			std::cerr << construct.get_ast().get_locus() << " Error: the 'spu' clause does not accept any parameter." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		if (target_ppu.is_defined() && !target_ppu.get_arguments().empty())
		{
			std::cerr << construct.get_ast().get_locus () << " Error: the 'ppu' clause does not accept any parameter." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		
		if (!target_ppu.is_defined() && !target_spu.is_defined())
		{
			std::cerr << construct.get_ast().get_locus () << " Error: 'target' construct without any architecture." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
			return;
		}
		
		function_info._is_on_task_side = target_spu.is_defined();
		function_info._is_on_non_task_side = target_ppu.is_defined();
	}
	
	
	void TL::TaskAnalysis::process_target_on_declaration(PragmaCustomConstruct construct)
	{
		Declaration general_declaration(construct.get_declaration(), construct.get_scope_link());
		ObjectList<DeclaredEntity> declared_entities = general_declaration.get_declared_entities();
		
		if (declared_entities.size() != 1)
		{
			std::cerr << construct.get_declaration().get_locus() << " Error: the target construct allows one declaration only."  << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		DeclaredEntity declared_entity = *(declared_entities.begin());
		Symbol function_symbol = declared_entity.get_declared_symbol();
		
		if (!function_symbol.is_function())
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Error: the target construct can only be applied to functions."  << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		std::string function_name = function_symbol.get_name();
		if (!_function_map.contains(function_name))
		{
			std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
			throw FatalException();
		}
		
		FunctionInfo &function_info = _function_map[function_name];
		
		function_info._has_coherced_sides = true;
		
		// FIXME: These names are too Cell specific
		PragmaCustomClause target_spu = construct.get_clause("spu");
		PragmaCustomClause target_ppu = construct.get_clause("ppu");
		
		if (target_spu.is_defined() && !target_spu.get_arguments().empty())
		{
			std::cerr << construct.get_ast().get_locus() << " Error: the 'spu' clause does not accept any parameter." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		if (target_ppu.is_defined() && !target_ppu.get_arguments().empty())
		{
			std::cerr << construct.get_ast().get_locus () << " Error: the 'ppu' clause does not accept any parameter." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
		}
		
		if (!target_ppu.is_defined() && !target_spu.is_defined())
		{
			std::cerr << construct.get_ast().get_locus () << " Error: 'target' construct without any architecture." << std::endl;
			function_info._has_errors = true;
			TaskAnalysis::fail();
			return;
		}
		
		function_info._is_on_task_side = target_spu.is_defined();
		function_info._is_on_non_task_side = target_ppu.is_defined();
	}


}


// FIXME: Somewhere, probably in another phase, we should check that the number of definitions and task definitions match for tasks and the number of declarations and task declarations match too


EXPORT_PHASE(TL::TaskAnalysis);


