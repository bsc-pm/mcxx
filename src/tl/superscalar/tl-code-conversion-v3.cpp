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


#include <sstream>

#include "tl-augmented-symbol.hpp"
#include "tl-code-conversion-v3.hpp"
#include "tl-parameter-expression.hpp"
#include "tl-type-utils.hpp"


namespace TL
{
	CompilerPhase::PhaseStatus CodeConversion::_status;
	
	
	void CodeConversion::TaskCallHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void CodeConversion::TaskCallHandler::postorder(Context ctx, AST_t node)
	{
		ScopeLink scope_link = ctx.scope_link;
		Expression function_call(node, ctx.scope_link);
		
		if (!function_call.is_function_call())
		{
			std::cerr << __FILE__ << ":" << __LINE__ << ": " << "Internal compiler error" << std::endl;
			throw FatalException();
		}
		
		Expression function_called_expresion = function_call.get_called_expression();
		if (!function_called_expresion.is_id_expression())
		{
			// We do not handle indirect calls (through variables)
			return;
		}
		
		AugmentedSymbol symbol = function_called_expresion.get_id_expression().get_symbol();
		if (!symbol.is_task())
		{
			return;
		}
		
		Type type = symbol.get_type();
		ObjectList<Type> parameter_types = type.nonadjusted_parameters();
		RefPtr<ParameterRegionList> parameter_region_list = symbol.get_parameter_region_list();
		std::string function_name = symbol.get_qualified_name();
		
		ObjectList<Expression> arguments = function_call.get_argument_list();
		if (arguments.size() != parameter_types.size())
		{
			std::cerr << function_call.get_ast().get_locus() << " Error: Call to function '" << function_name << "' with incorrect number of parameters." << std::endl;
			CodeConversion::fail();
			return;
		}
		
		if (parameter_region_list->size() != parameter_types.size())
		{
			std::cerr << __FILE__ << ":" << __LINE__ << ": " << "Internal compiler error" << std::endl;
			throw FatalException();
		}
		
		// Get and check its corresponding statement expression
		Expression top_expression = function_call.get_top_enclosing_expression();
		AST_t statement_expression_ast = top_expression.original_tree();
		PredicateAttr expression_statement_predicate(LANG_IS_EXPRESSION_STATEMENT) ;
		if (!expression_statement_predicate(statement_expression_ast))
		{
			std::cerr << function_call.get_ast().get_locus() << " Error: Call to task '" << function_name << "' cannot return a value." << std::endl;
			CodeConversion::fail();
			return;
		}
		
		
		Source source;
		Source parameter_initializers_source;
		Source constant_redirection_source;
		Source add_task_code;
		
		source
			<< "{"
				<< constant_redirection_source
				<< "css_parameter_t const parameters__cssgenerated[] = {" << parameter_initializers_source << "};"
				<< add_task_code
			<< "}";
		
		std::string flag_code;
		
		if (symbol.has_high_priority())
		{
			flag_code = "CSS_HIGH_PRIORITY_FLAG";
		}
		else if (symbol.is_blocking())
		{
			flag_code = "CSS_BLOCKING_FLAG";
		}
		else
		{
			flag_code = "CSS_NO_FLAG";
		}
		
		add_task_code
			<< "css_addTask("
				<< function_name << "_task_id__cssgenerated" << ", "
				<< flag_code << ", "
				<< (*parameter_region_list).size() << ", "
				<< "parameters__cssgenerated" << ");";
		
		for (unsigned int index = 0; index < arguments.size(); index++)
		{
			Source direction_source;
			Source size_source;
			Source address_source;
			
			parameter_initializers_source
				<< "{"
					<< direction_source
					<< ", " << size_source
					<< ", " << address_source
				<< "}, ";
			
			Expression argument = arguments[index];
			RegionList region_list = (*parameter_region_list.get_pointer())[index];
			if (region_list.size() != 1)
			{
				std::cerr << function_call.get_ast().get_locus() << " Error: In call to task '" << function_name << "'. Sorry, this compiler version does not support ranges. Please check that you are compiling for an architecture that supports ranges." << std::endl;
				CodeConversion::fail();
				return;
			}
			
			Region &region = region_list[0];
			if (!region.is_full())
			{
				std::cerr << function_call.get_ast().get_locus() << " Error: In call to task '" << function_name << "'. Sorry, this compiler version does not support ranges. Please check that you are compiling for an architecture that supports ranges." << std::endl;
				CodeConversion::fail();
				return;
			}
			
			switch (region.get_direction())
			{
				case Region::INPUT_DIR:
					direction_source << "CSS_IN_DIR";
					break;
				case Region::OUTPUT_DIR:
					direction_source << "CSS_OUT_DIR";
					break;
				case Region::INOUT_DIR:
					direction_source << "CSS_INOUT_DIR";
					break;
				case Region::UNKNOWN_DIR:
					std::cerr << __FILE__ << ":" << __LINE__ << "Internal compiler error." << std::cerr;
					throw FatalException();
			}
			
			switch (region.get_reduction())
			{
				case Region::NON_REDUCTION:
					break;
				case Region::REDUCTION:
					direction_source << " | CSS_REDUCTION_FLAG";
					break;
				case Region::UNKNOWN_RED:
					std::cerr << __FILE__ << ":" << __LINE__ << "Internal compiler error." << std::cerr;
					throw FatalException();
			}
			
			bool is_lvalue;
			Type argument_type = argument.get_type(is_lvalue);
			Type parameter_type = parameter_types[index];
			
			if (region.get_dimension_count() != 0)
			{
				// An array
				
				// Calulate the parametrized size
				Source parametrized_size_source;
				for (Region::iterator it = region.begin(); it != region.end(); it++)
				{
					Region::DimensionSpecifier *dimension = *it;
					parametrized_size_source 
						<< "(" << dimension->get_dimension_length().prettyprint() << ") * ";
				}
				parametrized_size_source
					<< "sizeof("
						<< (
								parameter_type.is_pointer()
								? parameter_type.points_to()
								: TypeUtils::get_array_element_type(parameter_type, scope_link)
							).get_declaration(ctx.scope_link.get_scope(argument.get_ast()), std::string(""))
					<< ")";
				AST_t parametrized_size_ast = parametrized_size_source.parse_expression(
					(*region.begin())->get_dimension_length().get_ast(),
					scope_link
				);
				Expression parametrized_size(parametrized_size_ast, scope_link);
				
				// Replace the parameters in the parametrized size
				ParameterExpression::substitute(parametrized_size, arguments, argument.get_ast(), scope_link);
				
				size_source
					<< parametrized_size.prettyprint();
				address_source
					<< argument.prettyprint();
			}
			else if (parameter_type.is_pointer())
			{
				if (!argument_type.is_pointer() && !argument_type.is_array())
				{
					std::cerr << argument.get_ast().get_locus() << " Error: expression '" << argument.prettyprint() << "' passed as parameter number " << index+1 << " in call to task '" << function_name << "' must be a pointer or an array." << std::endl;
					CodeConversion::fail();
					return;
				}
				
				Type base_type = parameter_type.points_to();
				if (base_type.is_void())
				{
					// An opaque parameter, we should pass a pointer to it, since it is treated as a scalar
					if (region.get_direction() == Region::INPUT_DIR)
					{
						direction_source = Source("CSS_IN_SCALAR_DIR");
					}
					size_source
						<< "sizeof(void *)";
					std::ostringstream temporary_name;
					temporary_name << "parameter_" << index << "__cssgenerated";
					constant_redirection_source
						<< "void *" << temporary_name.str() << " = " << argument.prettyprint() << ";";
					address_source << "&" << temporary_name.str();
				}
				else if (argument.is_literal())
				{
					if (region.get_direction() == Region::INPUT_DIR)
					{
						direction_source = Source("CSS_IN_SCALAR_DIR");
					}
					else
					{
						std::cerr << argument.get_ast().get_locus() << " Error: expression '" << argument.prettyprint() << "' passed as parameter number " << index+1 << " in call to task '" << function_name << "' is a literal passed as either output or inout." << std::endl;
						CodeConversion::fail();
						return;
					}
					// A "string" or an L"string"
					if (!base_type.is_char() && !base_type.is_wchar_t())
					{
						std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
						throw FatalException();
					}
					size_source
						<< "sizeof("
							<< base_type.get_declaration(scope_link.get_scope(argument.get_ast()), std::string(""))
						<< ")";
					address_source << argument.prettyprint();
				}
				else
				{
					// A struct
					size_source
						<< "sizeof("
							<< base_type.get_declaration(scope_link.get_scope(argument.get_ast()), std::string(""))
						<< ")";
					address_source << argument.prettyprint();
				}
			}
			else if (parameter_type.is_non_derived_type())
			{
				// A scalar
				if (region.get_direction() != Region::INPUT_DIR) {
					std::cerr << "Internal compiler error at " << __FILE__ << ":" << __LINE__ << std::endl;
					throw FatalException();
				}
				
				// Must not be a pointer but we have to pass its address anyway
				if (argument_type.is_pointer() || argument_type.is_array())
				{
					std::cerr << argument.get_ast().get_locus() << " Error: expression '" << argument.prettyprint() << "' passed as parameter number " << index+1 << " in call to task '" << function_name << "' can neither be a pointer nor an array." << std::endl;
					CodeConversion::fail();
					return;
				}
				
				if (region.get_direction() == Region::INPUT_DIR)
				{
					direction_source = Source("CSS_IN_SCALAR_DIR");
				}
				size_source
					<< "sizeof("
						<< parameter_type.get_declaration(ctx.scope_link.get_scope(argument.get_ast()), std::string(""))
					<< ")";
				
				if (is_lvalue && argument_type.is_same_type(parameter_type))
				{
					address_source
						<< "&(" << argument.prettyprint() << ")";
				}
				else
				{
					std::ostringstream temporary_name;
					temporary_name << "parameter_" << index << "__cssgenerated";
					constant_redirection_source
						<< parameter_type.get_declaration_with_initializer(
							ctx.scope_link.get_scope(argument.get_ast()),
							temporary_name.str(),
							argument.prettyprint()
						)
						<< ";";
					address_source
						<< "&" << temporary_name.str();
				}
			}
			else
			{
				// A derived type passed by value
				std::cerr << "Internal compiler error at " << __FILE__ << ":" << __LINE__ << std::endl;
				throw FatalException();
			}
		}
		
		AST_t tree = source.parse_statement(node, ctx.scope_link);
		// The replacement must be performed on the statement expression, since the replacement is a statement
		statement_expression_ast.replace(tree);
	}
	
	
	void CodeConversion::FunctionDefinitionHandler::handle_function_body(AST_t node, ScopeLink scope_link)
	{
		if (_do_traverse)
		{
			_body_traverser.traverse(node, scope_link);
		}
	}
	
	
	void CodeConversion::FunctionDefinitionHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void CodeConversion::FunctionDefinitionHandler::postorder(Context ctx, AST_t node)
	{
		FunctionDefinition function_definition(node, ctx.scope_link);
		AugmentedSymbol symbol = function_definition.get_function_name().get_symbol();
		
		std::string function_name = symbol.get_name();
		
		if (function_name.find("__cssgenerated") != std::string::npos) {
			// An adapter or similar generated code
			return;
		}
		
		// Handle non task function definitions
		if (!symbol.is_task())
		{
			if ( ((bool)symbol.is_on_task_side() && _generate_task_side) ||
				((bool)symbol.is_on_non_task_side() && _generate_non_task_side) )
			{
				// Generate the function definition
				handle_function_body(node, ctx.scope_link);
			}
			else
			{
				// Do not generate it
				_kill_list.push_back(node);
			}
			return;
		}
		
		// Erase the task if not generating task code
		if (!_generate_task_side)
		{
			// Do not generate the task definition
			_kill_list.push_back(node);
			return;
		}
		
	}
	
	
	void CodeConversion::DeclarationHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void CodeConversion::DeclarationHandler::postorder(Context ctx, AST_t node)
	{
		Declaration general_declaration(node, ctx.scope_link);
		
		bool is_extern = TypeUtils::is_extern_declaration(general_declaration);
		
		ObjectList<DeclaredEntity> declared_entities = general_declaration.get_declared_entities();
		int total_entities = declared_entities.size();
		
		if (total_entities > 0)
		{
			DeclaredEntity &first_entity = *(declared_entities.begin());
			Symbol symbol = first_entity.get_declared_symbol();
			Type declaration_type = symbol.get_type();
			
			// Handle all the declarators
			for (ObjectList<DeclaredEntity>::iterator it = declared_entities.begin(); it != declared_entities.end(); it++)
			{
				DeclaredEntity &entity = *it;
				AugmentedSymbol symbol = entity.get_declared_symbol();
				
				if (symbol.is_function())
				{
					std::string function_name = symbol.get_qualified_name();
					
					if (function_name.find("__cssgenerated") != std::string::npos) {
						// An adapter or similar generated code
					}
					
					if (symbol.is_task())
					{
						if (!_generate_task_side)
						{
							entity.get_ast().remove_in_list();
							total_entities--;
						}
					}
					else
					{
						if ( ((bool)symbol.is_on_task_side() && _generate_task_side) ||
							((bool)symbol.is_on_non_task_side() && _generate_non_task_side) )
						{
							// Generate the declaration
						}
						else
						{
							// Do not generate it
							entity.get_ast().remove_in_list();
							total_entities--;
						}
					}
				}
				else
				{
					// Not a function
					// Get the real type
					if (symbol.is_variable() && !is_extern)
					{
						if (!_generate_non_task_side)
						{
							entity.get_ast().remove_in_list();
							total_entities--;
						}
					}
				}
			} // For each declared entity
			
			if (total_entities == 0)
			{
				// All declarators have been removed
				// Leave empty declaration for non builtin types, otherwise remove the declaration
				if (TypeUtils::get_basic_type(declaration_type).is_non_derived_type() || TypeUtils::get_basic_type(declaration_type).is_function())
				{
					_kill_list.push_back(node);
				}
			}
		} // If it had any declared entity
	}
	
	
	void CodeConversion::MallocHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void CodeConversion::MallocHandler::postorder(Context ctx, AST_t node)
	{
		Expression function_call(node, ctx.scope_link);
		
		Expression called_expresion = function_call.get_called_expression();
		ObjectList<Expression> arguments = function_call.get_argument_list();
		
		Source source;
		
		if (arguments.size() != 1)
		{
			std::cerr << function_call.get_ast().get_locus() << " Error: Invalid number of arguments in call to malloc." << std::endl;
			CodeConversion::fail();
			return;
		}
		
		ObjectList<Expression>::iterator it = arguments.begin();
		source
			<< "css_aligned_malloc" << "("
				<< it->prettyprint()
			<< ")";
		
		AST_t tree = source.parse_expression(node, ctx.scope_link);
		node.replace_with(tree);
	}
	
	
	void CodeConversion::CallocHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void CodeConversion::CallocHandler::postorder(Context ctx, AST_t node)
	{
		Expression function_call(node, ctx.scope_link);
		
		Expression called_expresion = function_call.get_called_expression();
		ObjectList<Expression> arguments = function_call.get_argument_list();
		
		Source source;
		
		if (arguments.size() != 2)
		{
			std::cerr << function_call.get_ast().get_locus() << " Error: Invalid number of arguments in call to calloc." << std::endl;
			CodeConversion::fail();
			return;
		}
		
		ObjectList<Expression>::iterator it = arguments.begin();
		source
			<< "css_aligned_calloc" << "("
				<< it->prettyprint();
		it++;
		source
				<< ", " << it->prettyprint()
			<< ")";
		
		AST_t tree = source.parse_expression(node, ctx.scope_link);
		node.replace_with(tree);
	}
	
	
	void CodeConversion::generate_task_adapters(TaskTable &task_table, AST_t translation_unit, ScopeLink scope_link)
	{
		for (TaskTable::iterator it = task_table.begin(); it != task_table.end(); it++)
		{
			AugmentedSymbol symbol = *it;
			
			Source source;
			Source adapter_parameters;
			
			source
				<< "void __attribute__((weak)) " << symbol.get_qualified_name() << "_adapter__cssgenerated" << "(void **parameter_data)"
				<< "{"
					<< symbol.get_qualified_name() << "(" << adapter_parameters << ");"
				<< "}";
			
			Type task_type = symbol.get_type();
			ObjectList<Type> parameter_types = task_type.parameters();
			RefPtr<ParameterRegionList> parameter_region_list = symbol.get_parameter_region_list();
			for (unsigned int index = 0; index < parameter_region_list->size(); index++)
			{
				Type parameter_type = parameter_types[index];
				
				if (index != 0)
				{
					adapter_parameters
						<< ",";
				}
				if ((parameter_type.is_pointer() && !parameter_type.points_to().is_void()) || parameter_type.is_array())
				{
					adapter_parameters
						<< "parameter_data[" << index << "]";
				}
				else
				{
					adapter_parameters
						<< "*("
							<< "(" << parameter_type.get_pointer_to().get_declaration(scope_link.get_scope(symbol.get_point_of_declaration()), "") << ")"
							<< "parameter_data[" << index << "]"
						<< ")";
				}
			}
			
			AST_t tree = source.parse_global(symbol.get_point_of_declaration(), scope_link);
			symbol.get_point_of_declaration().append_to_translation_unit(tree);
		}
	}
	
	void CodeConversion::generate_task_ids(TaskTable &task_table, AST_t translation_unit, ScopeLink scope_link)
	{
		for (TaskTable::iterator it = task_table.begin(); it != task_table.end(); it++)
		{
			AugmentedSymbol symbol = *it;
			
			Source id_declaration_source;
			id_declaration_source
				<< "extern int const " << symbol.get_qualified_name() << "_task_id__cssgenerated" << ";";
			AST_t id_declaration_ast = id_declaration_source.parse_declaration(translation_unit, scope_link);
			translation_unit.prepend_to_translation_unit(id_declaration_ast);
		}
	}
	
	
	void CodeConversion::pre_run(DTO &dto)
	{
	}
	
	void CodeConversion::run(DTO &dto)
	{
		_status = PHASE_STATUS_OK;
		
		try
		{
			Bool generate_task_side = dto["superscalar_generate_task_side"];
			Bool generate_non_task_side = dto["superscalar_generate_non_task_side"];
			Bool generate_adapters = dto["superscalar_generate_task_adapters"];
			Bool generate_ids = dto["superscalar_generate_task_ids"];
			Bool align_memory = dto["superscalar_align_memory"];
			
			AST_t translation_unit = dto["translation_unit"];
			ScopeLink scope_link = dto["scope_link"];
			
			ObjectList<AST_t> kill_list;
			
			TaskTable task_table(translation_unit, scope_link);
			if (generate_adapters)
			{
				generate_task_adapters(task_table, translation_unit, scope_link);
			}
			
			if (generate_ids)
			{
				generate_task_ids(task_table, translation_unit, scope_link);
			}
			
			DepthTraverse depth_traverse;
			
			// WARNING: order is important since function definitions appear to be also declarations
			PredicateAttr function_definition_predicate(LANG_IS_FUNCTION_DEFINITION) ;
			TraverseASTPredicate function_definition_traverser(function_definition_predicate, AST_t::NON_RECURSIVE);
			FunctionDefinitionHandler function_definition_handler(kill_list, scope_link, generate_task_side, generate_non_task_side, align_memory);
			depth_traverse.add_functor(function_definition_traverser, function_definition_handler);
			
			PredicateAttr declaration_predicate(LANG_IS_DECLARATION) ;
			TraverseASTPredicate declaration_traverser(declaration_predicate, AST_t::NON_RECURSIVE);
			DeclarationHandler declaration_handler(kill_list, generate_task_side, generate_non_task_side);
			depth_traverse.add_functor(declaration_traverser, declaration_handler);
			
			depth_traverse.traverse(translation_unit, scope_link);
			
			// Erase nodes from the code
			for (ObjectList<AST_t>::iterator it = kill_list.begin(); it != kill_list.end(); it++)
			{
				AST_t node = *it;
				node.remove_in_list();
			}
		}
		catch (FatalException ex)
		{
			_status = PHASE_STATUS_ERROR;
		}
		
		set_phase_status(_status);
	}

}


EXPORT_PHASE(TL::CodeConversion);

