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

#include "tl-langconstruct.hpp"

#include "tl-ast-predicates.hpp"
#include "tl-exceptions.hpp"
#include "tl-preanalysis.hpp"
#include "tl-type-utils.hpp"


namespace TL
{
	CompilerPhase::PhaseStatus PreAnalysis::_status;
	
	
	void TL::PreAnalysis::FunctionDefinitionHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void TL::PreAnalysis::FunctionDefinitionHandler::postorder(Context ctx, AST_t node)
	{
		FunctionDefinition function_definition(node, ctx.scope_link);
		std::string function_name = function_definition.get_function_name().mangle_id_expression();
		Statement function_body = function_definition.get_function_body();
		bool has_ellipsis;
		ObjectList<ParameterDeclaration> parameters = function_definition.get_declared_entity().get_parameter_declarations(has_ellipsis);
		
		bool existed = _function_map.contains(function_name);
		FunctionInfo &function_info = _function_map[function_name]; // autocreate
		
		//
		// Check if the function was already declared or even defined
		// If already declared, then check that the parameters are equivalent
		//
		// This is already done by the semantic check
		
		//
		// Handle the function definition itself
		//
		function_info._name = function_name;
		function_info._has_ellipsis = has_ellipsis;
		function_info._definition_scope = function_definition.get_scope();
		function_info._definition_count++;
		function_info._definition_locus = function_definition.get_ast().get_locus();
		
		unsigned int parameter_index = 0;
		for (ObjectList<ParameterDeclaration>::iterator it = parameters.begin(); it != parameters.end(); it++)
		{
			ParameterDeclaration parameter_declaration = *it;
			Type type = parameter_declaration.get_type();
			
			std::string parameter_name = parameter_declaration.get_name().mangle_id_expression();
			
			ParameterInfo parameter_info;
			if (existed)
			{
				parameter_info = function_info._parameters[parameter_index];
			}
			
			parameter_info._definition_type = type.original_type();
			parameter_info._definition_locus = parameter_declaration.get_ast().get_locus();
			parameter_info._direction = UNKNOWN_DIR;
			
			if (existed)
			{
				function_info._parameters[parameter_index] = parameter_info;
			}
			else
			{
				function_info._parameters.push_back(parameter_info);
			}
			parameter_index++;
		}
		
		function_info._definition_scope = function_definition.get_scope();
		
		// Fill in the list of callees
		DepthTraverse depth_traverse;
		
		PredicateAST<LANG_IS_FUNCTION_CALL> function_call_predicate;
		FunctionCallHandler function_call_handler(function_info, _function_map);
		depth_traverse.add_predicate(function_call_predicate, function_call_handler);
		
		depth_traverse.traverse(function_body.get_ast(), ctx.scope_link);
	}
	
	
	void TL::PreAnalysis::FunctionDeclarationHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void TL::PreAnalysis::FunctionDeclarationHandler::postorder(Context ctx, AST_t node)
	{
		DeclaredEntity function_declaration(node, ctx.scope_link);
		std::string function_name = function_declaration.get_declared_entity().mangle_id_expression();
		
		bool has_ellipsis;
		ObjectList<ParameterDeclaration> parameters = function_declaration.get_parameter_declarations(has_ellipsis);
		
		bool existed = _function_map.contains(function_name);
		FunctionInfo &function_info = _function_map[function_name]; // autocreate
		
		//
		// Check if the function was already declared or even defined
		// If already declared, then check that the parameters are equivalent
		//
		// This is already done by the semantic check
		
		//
		// Handle the function declaration itself
		//
		function_info._name = function_name;
		function_info._has_ellipsis = has_ellipsis;
		function_info._declaration_scope = function_declaration.get_scope();
		function_info._declaration_count++;
		function_info._declaration_locus = function_declaration.get_ast().get_locus();
		
		unsigned int parameter_index = 0;
		for (ObjectList<ParameterDeclaration>::iterator it = parameters.begin(); it != parameters.end(); it++)
		{
			ParameterDeclaration parameter_declaration = *it;
			Type type = parameter_declaration.get_type();
			
			ParameterInfo parameter_info;
			
			if (existed)
			{
				parameter_info = function_info._parameters[parameter_index];
			}
			
			parameter_info._declaration_type = type.original_type();
			parameter_info._declaration_locus = parameter_declaration.get_ast().get_locus();
			parameter_info._direction = UNKNOWN_DIR;
			
			if (existed)
			{
				function_info._parameters[parameter_index] = parameter_info;
			}
			else
			{
				function_info._parameters.push_back(parameter_info);
			}
			parameter_index++;
		}
		
		function_info._declaration_scope = function_declaration.get_scope();
	}
	
	
	void TL::PreAnalysis::FunctionCallHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void TL::PreAnalysis::FunctionCallHandler::postorder(Context ctx, AST_t node)
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

		if (!_function_map.contains(function_name))
		{
			// FIXME: Should have a list of valid spu_ function names
			if (function_name.find("__builtin_") == 0 || function_name.find("spu_") == 0)
			{
				// Do not warn about builtin functions
			}
			else
			{
				std::cerr << node.get_locus() << " Warning: call to function '" << function_name << "' without a prototype." << std::endl;
			}
			return;
		}
		
		_function_map[function_name]._caller_functions.insert(_function_info._name);
		_function_info._called_functions.insert(function_name);
	}
	
	
	void TL::PreAnalysis::run(DTO &dto)
	{
		_status = PHASE_STATUS_OK;
		
		try
		{
			FunctionMap *original_function_map = new FunctionMap();
			original_function_map->initialize();
			RefPtr<FunctionMap> function_map_ref(original_function_map);
			dto.set_object("superscalar_function_table", function_map_ref);
			
			FunctionMap function_map = dto["superscalar_function_table"];
			AST_t translation_unit = dto["translation_unit"];
			ScopeLink scope_link = dto["scope_link"];
			
			DepthTraverse depth_traverse;
			
			PredicateAST<LANG_IS_FUNCTION_DEFINITION> function_definition_predicate;
			TraverseASTPredicate function_definition_traverser(function_definition_predicate, AST_t::NON_RECURSIVE);
			FunctionDefinitionHandler function_definition_handler(function_map);
			depth_traverse.add_functor(function_definition_traverser, function_definition_handler);
			
			FunctionDeclarationPredicate function_declaration_predicate(scope_link);
			TraverseASTPredicate function_declaration_traverser(function_declaration_predicate, AST_t::NON_RECURSIVE);
			FunctionDeclarationHandler function_declaration_handler(function_map);
			depth_traverse.add_functor(function_declaration_traverser, function_declaration_handler);
			
			depth_traverse.traverse(translation_unit, scope_link);
		}
		catch (FatalException ex)
		{
			_status = PHASE_STATUS_ERROR;
		}
		
		set_phase_status(_status);
	}

}

EXPORT_PHASE(TL::PreAnalysis);

