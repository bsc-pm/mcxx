/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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



#include <iostream>
#include <map>
#include <sstream>
#include <algorithm>

#include "tl-langconstruct.hpp"

#include "tl-augmented-symbol.hpp"
#include "tl-region.hpp"
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
			FunctionDefinition task_definition(construct.get_declaration(), construct.get_scope_link());
			AST_t context_ast = task_definition.get_function_body().get_ast();
			DeclaredEntity declared_entity = task_definition.get_declared_entity();
			
			process_task(construct, context_ast, declared_entity);
		}
		else
		{
			Declaration general_declaration(construct.get_declaration(), construct.get_scope_link());
			ObjectList<DeclaredEntity> declared_entities = general_declaration.get_declared_entities();
			
			if (declared_entities.size() != 1)
			{
				std::cerr << construct.get_declaration().get_locus() << " Error: only one declaration per task construct is allowed." << std::endl;
				TaskAnalysis::fail();
				return;
			}
			
			DeclaredEntity task_declaration = declared_entities[0];
			
			AST_t context_ast;
			ObjectList<ParameterDeclaration> parameter_declarations = task_declaration.get_parameter_declarations();
			if (!parameter_declarations.empty())
			{
				context_ast = parameter_declarations[0].get_ast();
			}
			else
			{
				context_ast = task_declaration.get_ast();
			}
			
			process_task(construct, context_ast, task_declaration);
		}
	}
	
	
	std::string TL::TaskAnalysis::direction_to_name(Region::Direction direction)
	{
		std::string direction_name;
		
		switch (direction)
		{
			case Region::INPUT_DIR:
				direction_name = std::string("input");
				break;
			case Region::OUTPUT_DIR:
				direction_name = std::string("output");
				break;
			case Region::INOUT_DIR:
				direction_name = std::string("inout");
				break;
			default:
				direction_name = std::string("INTERNAL ERROR");
				break;
		}
		
		return direction_name;
	}
	
	
	Region TL::TaskAnalysis::handle_parameter(AST_t construct_ast, AST_t context_ast, ScopeLink scope_link, std::string const &parameter_specification, std::string const &line_annotation, Region::Direction direction, Region::Reduction reduction, AugmentedSymbol &parameter_symbol)
	{
		std::string const direction_name = direction_to_name(direction);
		
		parameter_symbol = AugmentedSymbol::invalid();
		std::string parameter_name = parameter_specification;
		
		// Separate the ".." so that we can parse it properly
		std::string separated_parameter_specification;
		size_t index=0;
		while (index != parameter_specification.size())
		{
			size_t dotdot_pos = parameter_specification.find("..", index);
			if (dotdot_pos == std::string::npos)
			{
				separated_parameter_specification.append(parameter_specification.substr(index));
				break;
			}
			else
			{
				separated_parameter_specification.append(parameter_specification.substr(index, dotdot_pos - index));
				separated_parameter_specification.append(" .. ");
				index = dotdot_pos + 2;
			}
		}
		
		try
		{
			Region region = SourceBits::handle_superscalar_declarator(context_ast, scope_link, line_annotation + separated_parameter_specification, direction, reduction, parameter_symbol);
			
			if (!parameter_symbol.is_parameter())
			{
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
			}
			
			return region;
		}
		catch (SyntaxErrorException ex)
		{
			std::cerr << construct_ast.get_locus() << " Error: Syntax error while parsing '" << parameter_specification << "' in the " << direction_name << " clause." << std::endl;
			TaskAnalysis::fail();
		}
		catch (UnknownParameterException ex)
		{
			std::cerr << construct_ast.get_locus() << " Error: Unknown parameter '" << parameter_name << "' in " << direction_name << " clause." << std::endl;
			TaskAnalysis::fail();
		}
		catch (AmbiguousParameterException ex)
		{
			std::cerr << construct_ast.get_locus() << " Error: Ambiguous parameter '" << parameter_name << "' in " << direction_name << " clause." << std::endl;
			TaskAnalysis::fail();
		}
		catch (InvalidTypeException ex)
		{
			std::cerr << construct_ast.get_locus() << " Error: Invalid type for parameter '" << parameter_symbol.get_qualified_name() << "' in " << direction_name << " clause." << std::endl;
			TaskAnalysis::fail();
		}
		catch (InvalidDimensionSpecifiersException ex)
		{
			std::cerr << construct_ast.get_locus() << " Error: Invalid dimension specifiers for parameter '" << parameter_specification << "' in " << direction_name << " clause. Please check that they are present either in the task directive or in the function definition." << std::endl;
			TaskAnalysis::fail();
		}
		catch (InvalidRegionException ex)
		{
			std::cerr << construct_ast.get_locus() << " Error: Invalid region specification for parameter '" << parameter_specification << "' in " << direction_name << " clause." << std::endl;
			TaskAnalysis::fail();
		}
		catch (MismatchedNumberOfRangeSpecifiers ex)
		{
			std::cerr << construct_ast.get_locus() << " Error: Number of range specifiers for parameter '" << parameter_specification << "' in " << direction_name << " clause different from number of dimensions." << std::endl;
			TaskAnalysis::fail();
		}
		
		return Region();
	}
	
	
	void TL::TaskAnalysis::process_task(PragmaCustomConstruct construct, AST_t context_ast, DeclaredEntity declared_entity)
	{
		if (!declared_entity.is_functional_declaration())
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Error: the task construct can only be applied to functions." << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		if (declared_entity.functional_declaration_lacks_prototype())
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Error: task '" << declared_entity.get_declared_symbol().get_qualified_name() << "' declared without parameters. If there is none, please specify void." << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		
		AST_t construct_ast = construct.get_ast();
		AugmentedSymbol symbol = declared_entity.get_declared_symbol();
		std::string function_name = symbol.get_qualified_name();
		
		
		// Check for var args
		bool has_ellipsis;
		ObjectList<ParameterDeclaration> parameters = declared_entity.get_parameter_declarations(/*OUT*/ has_ellipsis);
		
		if (has_ellipsis)
		{
			std::cerr << context_ast.get_locus() << " Error: task '" << function_name << "' has a variable number of parameters." << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		
		// High priority
		bool has_high_priority = construct.get_clause("highpriority").is_defined();
		
		if (has_high_priority && !construct.get_clause("highpriority").get_arguments().empty())
		{
			std::cerr << context_ast.get_locus() << " Error: the 'highpriority' clause does not accept any parameter." << std::endl;
			TaskAnalysis::fail();
		}
		
		bool is_blocking = construct.get_clause("target").is_defined();
		
		if (is_blocking)
		{
			ObjectList< std::string > args = construct.get_clause("target").get_arguments(ExpressionTokenizerTrim());
			if ( args.size() != 1 )
			{
				std::cerr << context_ast.get_locus() << " Error: the 'target' clause needs one and just one parameter for this version." << std::endl;
				TaskAnalysis::fail();
			}

			if ( (*args.begin()) != std::string("comm_thread") )
			{
				std::cerr << context_ast.get_locus() << " Error: the 'target' only accepts the 'comm_thread' argument for this version." << std::endl;
				TaskAnalysis::fail();
			}
		}

		if (has_high_priority && is_blocking)
		{
			std::cerr << context_ast.get_locus() << " Warning: ignoring 'highpriority' clause used together with target(comm_thread) task." << std::endl;
		}
		
		std::map<Symbol, RegionList> parameter_region_lists;

		std::string line_annotation;
		std::ostringstream oss;
		oss << "# " << construct.get_ast().get_line() << " \"" << construct.get_ast().get_file() << "\"" << std::endl;
		line_annotation = oss.str();
		
		ObjectList<std::string> reduction_regions = construct.get_clause("reduction").get_arguments(ExpressionTokenizer());
		
		// Build all input regions
		ObjectList<std::string> input_parameters = construct.get_clause("input").get_arguments(ExpressionTokenizer());
		for (ObjectList<std::string>::iterator it = input_parameters.begin(); it != input_parameters.end(); it++)
		{
			std::string const &parameter_specification = *it;
			AugmentedSymbol parameter_symbol = AugmentedSymbol::invalid();
			
			Region::Reduction red = Region::NON_REDUCTION;
			if (reduction_regions.contains(parameter_specification))
			{
				red = Region::REDUCTION;
				ObjectList<std::string>::iterator it= find(reduction_regions.begin(), reduction_regions.end(), parameter_specification);
				reduction_regions.erase(it);
			}
			
			Region region = handle_parameter(construct_ast, context_ast, construct.get_scope_link(), parameter_specification, line_annotation, Region::INPUT_DIR, red, parameter_symbol);
			bool correct = parameter_region_lists[parameter_symbol].add(region);
			if (!correct)
			{
				std::cerr << context_ast.get_locus() << " Error: parameter region '" << parameter_specification << "' is duplicated." << std::endl;
				TaskAnalysis::fail();
			}
		}
		
		// Build all output regions
		ObjectList<std::string> output_parameters = construct.get_clause("output").get_arguments(ExpressionTokenizer());
		for (ObjectList<std::string>::iterator it = output_parameters.begin(); it != output_parameters.end(); it++)
		{
			std::string const &parameter_specification = *it;
			AugmentedSymbol parameter_symbol = AugmentedSymbol::invalid();
			
			Region::Reduction red = Region::NON_REDUCTION;
			if (reduction_regions.contains(parameter_specification))
			{
				red = Region::REDUCTION;
				ObjectList<std::string>::iterator it= find(reduction_regions.begin(), reduction_regions.end(), parameter_specification);
				reduction_regions.erase(it);
			}
			
			Region region = handle_parameter(construct_ast, context_ast, construct.get_scope_link(), parameter_specification, line_annotation, Region::OUTPUT_DIR, red, parameter_symbol);
			bool correct = parameter_region_lists[parameter_symbol].add(region);
			if (!correct)
			{
				std::cerr << context_ast.get_locus() << " Error: parameter region '" << parameter_specification << "' is duplicated." << std::endl;
				TaskAnalysis::fail();
			}
		}
		
		// Build all inout regions
		ObjectList<std::string> inout_parameters = construct.get_clause("inout").get_arguments(ExpressionTokenizer());
		for (ObjectList<std::string>::iterator it = inout_parameters.begin(); it != inout_parameters.end(); it++)
		{
			std::string const &parameter_specification = *it;
			AugmentedSymbol parameter_symbol = AugmentedSymbol::invalid();
			
			Region::Reduction red = Region::NON_REDUCTION;
			if (reduction_regions.contains(parameter_specification))
			{
				red = Region::REDUCTION;
				ObjectList<std::string>::iterator it= find(reduction_regions.begin(), reduction_regions.end(), parameter_specification);
				reduction_regions.erase(it);
			}
			
			Region region = handle_parameter(construct_ast, context_ast, construct.get_scope_link(), parameter_specification, line_annotation, Region::INOUT_DIR, red, parameter_symbol);
			bool correct = parameter_region_lists[parameter_symbol].add(region);
			if (!correct)
			{
				std::cerr << context_ast.get_locus() << " Error: parameter region '" << parameter_specification << "' is duplicated." << std::endl;
				TaskAnalysis::fail();
			}
		}
		
		for (ObjectList<std::string>::iterator it = reduction_regions.begin(); it !=reduction_regions.end(); it++)
		{
			std::cerr << context_ast.get_locus() << " Error: reduction parameter '" << *it << "' not specified present in any input, output or inout clause." << std::endl;
			TaskAnalysis::fail();
		}
		
		
		// Check that all parameters have appeared in a clause
		for (ObjectList<ParameterDeclaration>::iterator it = parameters.begin(); it != parameters.end(); it++)
		{
			ParameterDeclaration &parameter_declaration = *it;
			if (!parameter_declaration.is_named())
			{
				std::cerr << parameter_declaration.get_ast().get_locus() << " Error: task parameters must be named." << std::endl;
				TaskAnalysis::fail();
				return;
			}
			Symbol parameter_symbol = parameter_declaration.get_name().get_symbol();
			if (parameter_region_lists.find(parameter_symbol) == parameter_region_lists.end())
			{
				std::cerr << parameter_declaration.get_ast().get_locus() << " Error: parameter '" << parameter_symbol.get_qualified_name() << "' does not appear in any directionality clause." << std::endl;
				TaskAnalysis::fail();
			}
		}
		
		if (TaskAnalysis::_status == PHASE_STATUS_ERROR)
		{
			return;
		}
		
		// Check that output and inout parameters are either a pointer or an array
		for (std::map<Symbol, RegionList>::iterator it = parameter_region_lists.begin(); it != parameter_region_lists.end(); it++)
		{
			Symbol const &parameter_symbol = it->first;
			if (!parameter_symbol.get_type().is_pointer() && !parameter_symbol.get_type().is_array())
			{
				RegionList const &region_list = it->second;
				for (RegionList::const_iterator it2 = region_list.begin(); it2 != region_list.end(); it2++)
				{
					Region const &region = *it2;
					if (region.get_direction() == Region::OUTPUT_DIR)
					{
						std::cerr << parameter_symbol.get_point_of_declaration().get_locus() << " Error: parameter '" << parameter_symbol.get_qualified_name() << "' has output directionality but is neither a pointer nor an array." << std::endl;
						TaskAnalysis::fail();
					}
					else if (region.get_direction() == Region::INOUT_DIR)
					{
						std::cerr << parameter_symbol.get_point_of_declaration().get_locus() << " Error: parameter '" << parameter_symbol.get_qualified_name() << "' has output directionality but is neither a pointer nor an array." << std::endl;
						TaskAnalysis::fail();
					}
				}
			}
		}
		
		
		if (symbol.is_task())
		{
			// Check that everything still matches
			if (symbol.has_high_priority() != has_high_priority)
			{
				std::cerr << construct.get_ast().get_locus() << " Error: priority does not match with previous definition or declaration." << std::endl;
				TaskAnalysis::fail();
			}
			if (symbol.is_blocking() != is_blocking)
			{
				std::cerr << construct.get_ast().get_locus() << " Error: blocking property does not match with previous definition or declaration." << std::endl;
				TaskAnalysis::fail();
			}
			
			RefPtr<ParameterRegionList> previous_parameter_region_lists = symbol.get_parameter_region_list();
			for (std::map<Symbol, RegionList>::iterator it = parameter_region_lists.begin(); it != parameter_region_lists.end(); it++)
			{
				// For each parameter, get the current region list
				Symbol const &parameter_symbol = it->first;
				RegionList const &region_list = it->second;
				
				if (!parameter_symbol.is_parameter())
				{
					std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
					throw FatalException();
				}
				
				// Get its previous region list
				int index = parameter_symbol.get_parameter_position();
				RegionList const &previous_parameter_region_list = (*previous_parameter_region_lists.get_pointer())[index];
				
				// And check that they are equivalent
				if (region_list != previous_parameter_region_list)
				{
					std::cerr << parameter_symbol.get_point_of_declaration().get_locus() << " Error: parameter '" << parameter_symbol.get_qualified_name() << "' was previously declared with different shape, directionality, or regions." << std::endl;
					TaskAnalysis::fail();
				}
			}
		}
		else
		{
			// Mark it as a task
			symbol.set_as_task(true);
			
			// Set high priority
			symbol.set_high_priority(has_high_priority);
			
			// Set blocking
			symbol.set_blocking(is_blocking);
			
			// Set parameter region list
			RefPtr<ParameterRegionList> parameter_region_list_ref(new ParameterRegionList());
			parameter_region_list_ref->initialize();
			parameter_region_list_ref->reserve(parameter_region_lists.size());
			
			for (std::map<Symbol, RegionList>::iterator it = parameter_region_lists.begin(); it != parameter_region_lists.end(); it++)
			{
				Symbol const &parameter_symbol = it->first;
				RegionList const &region_list = it->second;
				
				if (!parameter_symbol.is_parameter())
				{
					std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
					throw FatalException();
				}
				
				int index = parameter_symbol.get_parameter_position();
				(*parameter_region_list_ref.get_pointer())[index] = region_list;
			}
			
			symbol.set_parameter_region_list(parameter_region_list_ref);
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
		AugmentedSymbol symbol = function_definition.get_function_name().get_symbol();
		
		symbol.set_coherced_sides(true);
		
		// FIXME: These names are too Cell specific
		PragmaCustomClause target_spu = construct.get_clause("spu");
		PragmaCustomClause target_ppu = construct.get_clause("ppu");
		
		if (target_spu.is_defined() && !target_spu.get_arguments().empty())
		{
			std::cerr << construct.get_ast().get_locus() << " Error: the 'spu' clause does not accept any parameter." << std::endl;
			TaskAnalysis::fail();
		}
		if (target_ppu.is_defined() && !target_ppu.get_arguments().empty())
		{
			std::cerr << construct.get_ast().get_locus () << " Error: the 'ppu' clause does not accept any parameter." << std::endl;
			TaskAnalysis::fail();
		}
		
		if (!target_ppu.is_defined() && !target_spu.is_defined())
		{
			std::cerr << construct.get_ast().get_locus () << " Error: 'target' construct without any architecture." << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		symbol.set_as_task_side(target_spu.is_defined());
		symbol.set_as_non_task_side(target_ppu.is_defined());
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
		AugmentedSymbol symbol = declared_entity.get_declared_symbol();
		
		if (!symbol.is_function())
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Error: the target construct can only be applied to functions."  << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		symbol.set_coherced_sides(true);
		
		// FIXME: These names are too Cell specific
		PragmaCustomClause target_spu = construct.get_clause("spu");
		PragmaCustomClause target_ppu = construct.get_clause("ppu");
		
		if (target_spu.is_defined() && !target_spu.get_arguments().empty())
		{
			std::cerr << construct.get_ast().get_locus() << " Error: the 'spu' clause does not accept any parameter." << std::endl;
			TaskAnalysis::fail();
		}
		if (target_ppu.is_defined() && !target_ppu.get_arguments().empty())
		{
			std::cerr << construct.get_ast().get_locus () << " Error: the 'ppu' clause does not accept any parameter." << std::endl;
			TaskAnalysis::fail();
		}
		
		if (!target_ppu.is_defined() && !target_spu.is_defined())
		{
			std::cerr << construct.get_ast().get_locus () << " Error: 'target' construct without any architecture." << std::endl;
			TaskAnalysis::fail();
			return;
		}
		
		symbol.set_as_task_side(target_spu.is_defined());
		symbol.set_as_non_task_side(target_ppu.is_defined());
	}


}


// FIXME: Somewhere, probably in another phase, we should check that the number of definitions and task definitions match for tasks and the number of declarations and task declarations match too


EXPORT_PHASE(TL::TaskAnalysis);


