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
		Symbol symbol = function_definition.get_function_name().get_symbol();
		
		Statement function_body = function_definition.get_function_body();
		
		bool has_ellipsis;
		ObjectList<ParameterDeclaration> parameters = function_definition.get_declared_entity().get_parameter_declarations(has_ellipsis);
		// Fill in the list of callees
		DepthTraverse depth_traverse;
		
		PredicateAttr function_call_predicate(LANG_IS_FUNCTION_CALL) ;
		FunctionCallHandler function_call_handler(symbol, _function_table);
		depth_traverse.add_predicate(function_call_predicate, function_call_handler);
		
		depth_traverse.traverse(function_body.get_ast(), ctx.scope_link);
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
		
		AugmentedSymbol called_function = function_called_expresion.get_id_expression().get_symbol();
		std::string function_name = called_function.get_qualified_name();
		
		if ( _function_table.find(function_name) == _function_table.end() )
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
		
		called_function.add_caller_function(_caller_function);
		_caller_function.add_callee_function(called_function);
	}
	
    void TL::PreAnalysis::pre_run(DTO& dto)
    {
    }
	
	void TL::PreAnalysis::run(DTO &dto)
	{
		_status = PHASE_STATUS_OK;
		
		try
		{
			AST_t translation_unit( dto["translation_unit"] );
			ScopeLink scope_link( dto["scope_link"] );
			
			FunctionTable function_table(translation_unit, scope_link);
			
			DepthTraverse depth_traverse;
			
			PredicateAttr function_definition_predicate(LANG_IS_FUNCTION_DEFINITION) ;
			TraverseASTPredicate function_definition_traverser(function_definition_predicate, AST_t::NON_RECURSIVE);
			FunctionDefinitionHandler function_definition_handler(function_table);
			depth_traverse.add_functor(function_definition_traverser, function_definition_handler);
			
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

