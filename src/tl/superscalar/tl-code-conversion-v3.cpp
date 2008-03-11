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

#include <sstream>

#include "tl-code-conversion-v2.hpp"
#include "tl-type-utils.hpp"


namespace TL
{
	CompilerPhase::PhaseStatus CodeConversion::_status;
	
	
	void CodeConversion::TaskCallHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void CodeConversion::TaskCallHandler::postorder(Context ctx, AST_t node)
	{
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
		
		std::string function_name = function_called_expresion.get_id_expression().mangle_id_expression();
		if (_function_map.find(function_name) == _function_map.end())
		{
			// Already emmited elsewhere
			// std::cerr << node.get_locus() << " Warning: call to function '" << function_name << "' without a prototype." << endl;
			return;
		}
		
		FunctionInfo const &function_info = _function_map[function_name];
		if (!function_info._is_task)
		{
			return;
		}
		
		ObjectList<Expression> arguments = function_call.get_argument_list();
		if (arguments.size() != function_info._parameters.size())
		{
			std::cerr << function_call.get_ast().get_locus() << " Error: Call to function '" << function_name << "' with incorrect number of parameters." << std::endl;
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
				<< "css_parameter_t __cssgenerated_parameters[] = {" << parameter_initializers_source << "};"
				<< add_task_code
			<< "}";
		
		add_task_code
			<< "css_addTask("
				<< "__cssgenerated_" << function_name << "_task_id" << ", "
				<< (function_info._has_high_priority ? "1" : "0") << ", "
				<< function_info._parameters.size() << ", "
				<< "__cssgenerated_parameters" << ");";
		
		
		// Fill in the argumet mapper
		ReplaceIdExpression argument_mapper;
		ObjectList<ParameterInfo>::const_iterator it = function_info._parameters.begin();
		ObjectList<Expression>::iterator it2 = arguments.begin();
		while (it != function_info._parameters.end())
		{
			ParameterInfo const &parameter_info = *it;
			Expression &argument = *it2;
			Type parameter_type(NULL);
			
			if (parameter_info._augmented_definition_type.is_valid())
			{
				parameter_type = parameter_info._augmented_definition_type;
			}
			else if (parameter_info._augmented_declaration_type.is_valid())
			{
				parameter_type = parameter_info._augmented_declaration_type;
			}
			else
			{
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
			}
			
			if (!parameter_type.is_array() && !parameter_type.is_pointer() && parameter_type.is_non_derived_type())
			{
				// A scalar
				argument_mapper.add_replacement(parameter_info._symbol, argument.get_ast());
			}
			it++;
			it2++;
		}
		
		
		unsigned int parameter_index = 0;
		it = function_info._parameters.begin();
		it2 = arguments.begin();
		while (it != function_info._parameters.end())
		{
			ParameterInfo const &parameter_info = *it;
			Expression &argument = *it2;
			
			Source direction_source;
			Source scalar_source;
			Source dimensions_source;
			Source size_source;
			Source address_source;
			Source bounds_source;
			
			parameter_initializers_source
				<< "{"
					<< direction_source
					<< ", " << scalar_source
					<< ", " << dimensions_source
					<< ", " << size_source
					<< ", " << address_source
					<< ", " << bounds_source
				<< "}, ";
			
			switch (parameter_info._direction)
			{
				case INPUT_DIR:
					direction_source << "CSS_IN_DIR";
					break;
				case OUTPUT_DIR:
					direction_source << "CSS_OUT_DIR";
					break;
				case INOUT_DIR:
					direction_source << "CSS_INOUT_DIR";
					break;
				case UNKNOWN_DIR:
					std::cerr << __FILE__ << ":" << __LINE__ << "Internal compiler error." << std::cerr;
					return;
			}
			
			Type parameter_type(NULL);
			
			if (parameter_info._augmented_definition_type.is_valid())
			{
				parameter_type = parameter_info._augmented_definition_type;
			}
			else if (parameter_info._augmented_declaration_type.is_valid())
			{
				parameter_type = parameter_info._augmented_declaration_type;
			}
			else
			{
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
			}
			
			
			bool is_lvalue;
			Type argument_type = argument.get_type(is_lvalue);
			
			if (parameter_type.is_array())
			{
				scalar_source << "0";
				
				// Size
				ObjectList<Expression> dimensions = TypeUtils::get_array_dimensions(parameter_type, ctx.scope_link);
				dimensions_source << dimensions.size();
				for (ObjectList<Expression>::const_iterator dimension_it = dimensions.begin(); dimension_it != dimensions.end(); dimension_it++)
				{
					Expression const &dimension = *dimension_it;
					size_source 
						<< "(" << argument_mapper.replace(dimension).prettyprint() << ") *";
				}
				size_source
					<< "sizeof("
						<< TypeUtils::get_array_element_type(parameter_type, ctx.scope_link)
							.get_declaration(ctx.scope_link.get_scope(argument.get_ast()), std::string(""))
					<< ")";
				address_source << argument.prettyprint();
				bounds_source << "(void *)0";
			}
			else if (parameter_type.is_pointer())
			{
				if (!argument_type.is_pointer() && !argument_type.is_array())
				{
					std::cerr << argument.get_ast().get_locus() << " Error: expression '" << argument.prettyprint() << "' passed as parameter number " << parameter_index+1 << " in call to task '" << function_info._name << "' must be a pointer or an array." << std::endl;
					CodeConversion::fail();
					return;
				}
				
				Type base_type = parameter_type.points_to();
				if (base_type.is_void())
				{
					// An opaque parameter, we should pass a pointer to it, since it is treated as a scalar
					scalar_source << "1";
					dimensions_source << "0";
					size_source
						<< "sizeof(void *)";
					std::ostringstream temporary_name;
					temporary_name << "__cssgenerated_parameter_" << parameter_index;
					constant_redirection_source
						<< "void *" << temporary_name.str() << " = " << argument.prettyprint() << ";";
					address_source << "&" << temporary_name.str();
					bounds_source << "(void *)0";
				} else {
					scalar_source << "0";
					dimensions_source << "0";
					size_source
						<< "sizeof("
							<< base_type.get_declaration(ctx.scope_link.get_scope(argument.get_ast()), std::string(""))
						<< ")";
					address_source << argument.prettyprint();
					bounds_source << "(void *)0";
				}
			}
			else if (parameter_type.is_non_derived_type())
			{
				// A scalar
				if (parameter_info._direction != INPUT_DIR) {
					std::cerr << "Internal compiler error at " << __FILE__ << ":" << __LINE__ << std::endl;
					throw FatalException();
				}
				
				// Must not be a pointer but we have to pass its address anyway
				if (argument_type.is_pointer() || argument_type.is_array())
				{
					std::cerr << argument.get_ast().get_locus() << " Error: expression '" << argument.prettyprint() << "' passed as parameter number " << parameter_index+1 << " in call to task '" << function_info._name << "' cannot be a pointer or an array." << std::endl;
					CodeConversion::fail();
					return;
				}
				
				scalar_source << "1";
				dimensions_source << "0";
				size_source
					<< "sizeof("
						<< parameter_type.get_declaration(ctx.scope_link.get_scope(argument.get_ast()), std::string(""))
					<< ")";
				
				if (is_lvalue)
				{
					address_source
						<< "&(" << argument.prettyprint() << ")";
				}
				else
				{
					std::ostringstream temporary_name;
					temporary_name << "__cssgenerated_parameter_" << parameter_index;
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
				
				bounds_source << "(void *)0";
			}
			else
			{
				// A derived type passed by value
				std::cerr << "Internal compiler error at " << __FILE__ << ":" << __LINE__ << std::endl;
				throw FatalException();
			}
			
			parameter_index++;
			it++;
			it2++;
		}
		
		AST_t tree = source.parse_statement(node, ctx.scope_link);
		node.replace(tree);
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
		
		std::string function_name = function_definition.get_function_name().mangle_id_expression();
		
		if (function_name.find("__cssgenerated_") == 0) {
			// An adapter or similar generated code
			return;
		}
		
		FunctionInfo &function_info = _function_map[function_name];
		
		// Handle non task function definitions
		if (!function_info._is_task)
		{
			if ( (function_info._is_on_task_side && _generate_task_side) ||
				(function_info._is_on_non_task_side && _generate_non_task_side) )
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
		
		// Generate the task identifier if generating non task side
		if (_generate_non_task_side)
		{
			Source id_declaration_source;
			id_declaration_source
				<< "extern int const " << "__cssgenerated_" << function_name << "_task_id" << ";";
			AST_t id_declaration_ast = id_declaration_source.parse_declaration(node, ctx.scope_link);
			node.prepend_to_translation_unit(id_declaration_ast);
		}
		
		// Erase the task if not generating task code
		if (!_generate_task_side)
		{
			// Do not generate the task definition
			_kill_list.push_back(node);
			return;
		}
		
		Source source;
		Source adapter_parameters;
		
		source
			<< "void __attribute__((weak)) " << "__cssgenerated_" << function_name << "_adapter" << "(void **parameter_data)"
			<< "{"
				<< function_name << "(" << adapter_parameters << ");"
			<< "}";
		
		int parameter_index = 0;
		for (ObjectList<ParameterInfo>::iterator it = function_info._parameters.begin(); it != function_info._parameters.end(); it++)
		{
			ParameterInfo &parameter_info = *it;
			
			Type parameter_type(NULL);
			if (parameter_info._augmented_definition_type.is_valid())
			{
				parameter_type = parameter_info._augmented_definition_type;
			}
			else if (parameter_info._augmented_declaration_type.is_valid())
			{
				parameter_type = parameter_info._augmented_declaration_type;
			}
			else
			{
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
			}
			
			if (it != function_info._parameters.begin())
			{
				adapter_parameters
					<< ",";
			}
			if ((parameter_type.is_pointer() && !parameter_type.points_to().is_void()) || parameter_type.is_array())
			{
				adapter_parameters
					<< "parameter_data[" << parameter_index << "]";
			}
			else
			{
				adapter_parameters
					<< "*("
						<< "(" << parameter_type.get_pointer_to().get_declaration(/* FIXME: probably wrong */ ctx.scope_link.get_scope(node), std::string("")) << ")"
						<< "parameter_data[" << parameter_index << "]"
					<< ")";
			}
			
			parameter_index++;
		}
		
		AST_t tree = source.parse_global(node, ctx.scope_link);
		node.append_to_translation_unit(tree);
	}
	
	
	void CodeConversion::TaskDeclarationHandler::preorder(Context ctx, AST_t node, FunctionInfo &function_info)
	{
	}
	
	
	void CodeConversion::TaskDeclarationHandler::postorder(Context ctx, AST_t node, FunctionInfo &function_info)
	{
		if (function_info._definition_count != 0)
		{
			// Emmit adaptors only once per translation unit (preferably for the task definition)
			return;
		}
		
		function_info._task_declarations_processed++;
		if (function_info._task_declarations_processed != function_info._task_declaration_count)
		{
			// Skip adaptor generation until the last declaration
			return;
		}
		
		if (_generate_non_task_side)
		{
			// Generate the task identifier but only once
			Source id_declaration_source;
			id_declaration_source
				<< "extern int const " << "__cssgenerated_" << function_info._name << "_task_id" << ";";
			AST_t id_declaration_ast = id_declaration_source.parse_declaration(node, ctx.scope_link);
			node.prepend_to_translation_unit(id_declaration_ast);
		}
		
		if (_generate_task_side)
		{
			// Generate the task adapter but only once
			Source source;
			Source adapter_parameters;
			
			source
				<< "void __attribute__((weak)) " << "__cssgenerated_" << function_info._name << "_adapter" << "(void **parameter_data)"
				<< "{"
					<< function_info._name << "(" << adapter_parameters << ");"
				<< "}";
			
			int parameter_index = 0;
			for (ObjectList<ParameterInfo>::iterator it = function_info._parameters.begin(); it != function_info._parameters.end(); it++)
			{
				ParameterInfo &parameter_info = *it;
				
				Type parameter_type(NULL);
				if (parameter_info._augmented_definition_type.is_valid())
				{
					parameter_type = parameter_info._augmented_definition_type;
				}
				else if (parameter_info._augmented_declaration_type.is_valid())
				{
					parameter_type = parameter_info._augmented_declaration_type;
				}
				else
				{
					std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
					throw FatalException();
				}
				
				if (it != function_info._parameters.begin())
				{
					adapter_parameters
						<< ",";
				}
				if ((parameter_type.is_pointer() && !parameter_type.points_to().is_void()) || parameter_type.is_array())
				{
					adapter_parameters
						<< "parameter_data[" << parameter_index << "]";
				}
				else
				{
					adapter_parameters
						<< "*("
							<< "(" << parameter_type.get_pointer_to().get_declaration(/* FIXME: probably wrong */ ctx.scope_link.get_scope(node), std::string("")) << ")"
							<< "parameter_data[" << parameter_index << "]"
						<< ")";
				}
				
				parameter_index++;
			}
			
			AST_t tree = source.parse_global(_declaration_node, _ctx.scope_link);
			_declaration_node.append_to_translation_unit(tree);
		} // _generate_task_side
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
			TaskDeclarationHandler task_declaration_handler(_function_map, ctx, node, _generate_task_side, _generate_non_task_side);
			
			DeclaredEntity &first_entity = *(declared_entities.begin());
			Symbol symbol = first_entity.get_declared_symbol();
			Type declaration_type = symbol.get_type();
			
			// Handle all the declarators
			for (ObjectList<DeclaredEntity>::iterator it = declared_entities.begin(); it != declared_entities.end(); it++)
			{
				DeclaredEntity &entity = *it;
				Symbol symbol = entity.get_declared_symbol();
				
				if (symbol.is_function())
				{
					// std::string function_name = entity.get_declared_entity().mangle_id_expression();
					std::string function_name = symbol.get_name();
					FunctionInfo &function_info = _function_map[function_name];
					
					if (function_info._is_task)
					{
						task_declaration_handler.postorder(ctx, node, function_info);
						if (!_generate_task_side)
						{
							entity.get_ast().remove_in_list();
							total_entities--;
						}
					}
					else
					{
						if ( (function_info._is_on_task_side && _generate_task_side) ||
							(function_info._is_on_non_task_side && _generate_non_task_side) )
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
		std::string function_name = called_expresion.get_id_expression().mangle_id_expression();
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
			<< "memalign" << "("
				<< 128
				<< ", " << it->prettyprint()
			<< ")" << ";";
		
		AST_t tree = source.parse_statement(node, ctx.scope_link);
		node.replace_with(tree);
	}
	
	
	void CodeConversion::CallocHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void CodeConversion::CallocHandler::postorder(Context ctx, AST_t node)
	{
		Expression function_call(node, ctx.scope_link);
		
		Expression called_expresion = function_call.get_called_expression();
		std::string function_name = called_expresion.get_id_expression().mangle_id_expression();
		ObjectList<Expression> arguments = function_call.get_argument_list();
		
		Source source;
		
		if (arguments.size() != 2)
		{
			std::cerr << function_call.get_ast().get_locus() << " Error: Invalid number of arguments in call to calloc." << std::endl;
			CodeConversion::fail();
			return;
		}
		
		source
			<< "({"
				<< "void *__cssgenerated_aligned_mem = ";
		ObjectList<Expression>::iterator it = arguments.begin();
		source
					<< "memalign" << "("
						<< 128
						<< ", " << "(" << it->prettyprint() << ")";
		it++;
		source
						<< "*" << "(" << it->prettyprint() << ")"
					<< ")" << ";";
		
		// FIXME: must include strings.h
		it = arguments.begin();
		source
				<< "bzero(__cssgenerated_aligned_mem, "
					<< "(" << it->prettyprint() << ")";
		it++;
		source
					<< "*" << "(" << it->prettyprint() << ")"
					<< ")" << ";";
		
		source
				<< "__cssgenerated_aligned_mem;"
			<< "})";
		
		AST_t tree = source.parse_statement(node, ctx.scope_link);
		node.replace_with(tree);
	}
	
	
	void CodeConversion::run(DTO &dto)
	{
		_status = PHASE_STATUS_OK;
		
		try
		{
			FunctionMap function_map = dto["superscalar_function_table"];
			
			Bool generate_task_side = dto["superscalar_generate_task_side"];
			Bool generate_non_task_side = dto["superscalar_generate_non_task_side"];
			Bool align_memory = dto["superscalar_align_memory"];
			
			AST_t translation_unit = dto["translation_unit"];
			ScopeLink scope_link = dto["scope_link"];
			
			ObjectList<AST_t> kill_list;
			
			
			DepthTraverse depth_traverse;
			
			// WARNING: order is important since function definitions appear to be also declarations
			PredicateAST<LANG_IS_FUNCTION_DEFINITION> function_definition_predicate;
			TraverseASTPredicate function_definition_traverser(function_definition_predicate, AST_t::NON_RECURSIVE);
			FunctionDefinitionHandler function_definition_handler(function_map, kill_list, scope_link, generate_task_side, generate_non_task_side, align_memory);
			depth_traverse.add_functor(function_definition_traverser, function_definition_handler);
			
			PredicateAST<LANG_IS_DECLARATION> declaration_predicate;
			TraverseASTPredicate declaration_traverser(declaration_predicate, AST_t::NON_RECURSIVE);
			DeclarationHandler declaration_handler(function_map, kill_list, generate_task_side, generate_non_task_side);
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

