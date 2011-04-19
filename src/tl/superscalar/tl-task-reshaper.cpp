/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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




#include <sstream>

#include "tl-augmented-symbol.hpp"
#include "tl-parameter-expression.hpp"
#include "tl-task-reshaper.hpp"
#include "tl-type-utils.hpp"


namespace TL
{
	CompilerPhase::PhaseStatus TaskReshaper::_status;
	
	
	void TaskReshaper::FunctionDefinitionHandler::preorder(Context ctx, AST_t node)
	{
	}
	
	
	void TaskReshaper::FunctionDefinitionHandler::postorder(Context ctx, AST_t node)
	{
		ScopeLink scope_link = ctx.scope_link;
		FunctionDefinition function_definition(node, scope_link);
		AugmentedSymbol symbol = function_definition.get_function_name().get_symbol();
		
		if (!symbol.is_task())
		{
			return;
		}
		
		Source reshaped_task_code;
		Source parameter_declarations_code;
		Source reshaped_parameter_declarations_code;
		
		Type type = symbol.get_type();
		std::string function_name = symbol.get_name();
		Scope scope = scope_link.get_scope(node);
		
		reshaped_task_code
			<< type.returns().get_simple_declaration(scope, "")
			<< " "
			<< function_name << "__minimal_reshape__csssynthesised"
			<< " (" << parameter_declarations_code << ")"
			<< function_definition.get_function_body().prettyprint();
		
		Statement body = function_definition.get_function_body();
		Scope body_scope = scope_link.get_scope(body.get_ast());
		
		DeclaredEntity function_declaration = function_definition.get_declared_entity();
		ObjectList<ParameterDeclaration> parameter_declarations = function_declaration.get_parameter_declarations();
		ObjectList<Type> parameter_types = type.nonadjusted_parameters();
		RefPtr<AccessBoundsList> access_bounds_list = symbol.get_parameter_access_bounds_list();
		for (unsigned int i=0; i < access_bounds_list->size(); i++)
		{
			ParameterDeclaration &original_parameter_declaration = parameter_declarations[i];
			AccessBounds &access_bounds = (*access_bounds_list)[i];
			if (access_bounds == AccessBounds())
			{
				// A non-reshapable parameter
				Source parameter_declaration_source;
				
				parameter_declaration_source
					<< original_parameter_declaration.prettyprint();
				
				parameter_declarations_code.append_with_separator(parameter_declaration_source, ",");
			}
			else
			{
				Source reshaped_parameter_declaration_source;
				
				std::string parameter_name = original_parameter_declaration.get_name().get_symbol().get_name();
				Type original_parameter_type = parameter_types[i];
				Type final_parameter_type = TypeUtils::remove_array_levels(original_parameter_type, scope_link, access_bounds.size());
				for (unsigned int j=0; j < access_bounds.size(); j++)
				{
					Expression dimension_expression = access_bounds[j];
					final_parameter_type = final_parameter_type.get_array_to(dimension_expression.get_ast(), body_scope);
				}
				reshaped_parameter_declaration_source
					<< final_parameter_type.get_declaration(body_scope, parameter_name);
				
				reshaped_parameter_declarations_code.append_with_separator(reshaped_parameter_declaration_source, ",");
			}
		}
		
		parameter_declarations_code.append_with_separator(reshaped_parameter_declarations_code, ", ");
		
		AST_t reshaped_task_ast = reshaped_task_code.parse_global(node, scope_link);
		
		// Mark the reshaped task as task-side
		FunctionDefinition reshaped_task(reshaped_task_ast, scope_link);
		AugmentedSymbol reshaped_task_symbol = reshaped_task.get_declared_entity().get_declared_symbol();
		reshaped_task_symbol.set_as_task_side(true);
		
		node.append_to_translation_unit(reshaped_task_ast);
	}
	
	
	void TaskReshaper::generate_reshaped_task_adapters_and_shapers(TaskTable &task_table, AST_t translation_unit, ScopeLink scope_link)
	{
		for (TaskTable::iterator it = task_table.begin(); it != task_table.end(); it++)
		{
			AugmentedSymbol symbol = *it;
			Declaration function_declaration_global(symbol.get_point_of_declaration(), scope_link);
			DeclaredEntity function_declaration = *function_declaration_global.get_declared_entities().begin();
			ObjectList<ParameterDeclaration> parameter_declarations = function_declaration.get_parameter_declarations();
			Type type = symbol.get_type();
			ObjectList<Type> parameter_types = type.nonadjusted_parameters();
			
			Source adapter_source;
			Source adapter_parameters;
			Source adapter_reshaped_parameters;
			
			adapter_source
				<< "void __attribute__((weak)) " << symbol.get_qualified_name() << "__minimal_reshape__csssynthesised" << "_adapter__cssgenerated" << "(void **parameter_data)"
				<< "{"
					<< symbol.get_qualified_name() << "__minimal_reshape__csssynthesised" << "(" << adapter_parameters << ");"
				<< "}";
			
			Source shaper_source;
			Source shaper_parameters;
			Source shaper_datastructure_filling;
			
			adapter_source
				<< "void __attribute__((weak)) " << symbol.get_qualified_name() << "__minimal_reshape__csssynthesised" << "_shaper__cssgenerated" << "(void **parameter_data, reshaping_dimensional_data **dimensions)"
				<< "{"
					<< shaper_parameters
					<< shaper_datastructure_filling
				<< "}";
			
			Scope global_scope = scope_link.get_scope(translation_unit);
			
			RefPtr<ParameterRegionList> parameter_region_list = symbol.get_parameter_region_list();
			RefPtr<AccessBoundsList> access_bounds_list = symbol.get_parameter_access_bounds_list();
			RefPtr<AccessBoundsList> min_index_list = symbol.get_parameter_min_index_list();
			for (unsigned int index = 0; index < parameter_region_list->size(); index++)
			{
				// Type parameter_type = parameter_declarations[index].get_type();
				Type parameter_type = parameter_types[index];
				Symbol parameter_symbol = parameter_declarations[index].get_name().get_symbol();
				Scope parameter_scope = parameter_symbol.get_scope();
				
				if (parameter_type.is_array())
				{
					Source adapter_reshaped_parameter;
					adapter_reshaped_parameter
						<< "parameter_data[" << index << "]";
					adapter_reshaped_parameters.append_with_separator(adapter_reshaped_parameter, ", ");
					
					// Calculate the shape of the array (if needed)
					AccessBounds const &access_bounds = (*access_bounds_list)[index];
					AccessBounds const &min_index = (*min_index_list)[index];
					ObjectList<Expression> dimension_sizes = TypeUtils::get_array_dimensions(parameter_type, scope_link);
					
					shaper_datastructure_filling
						<< "if (dimensions[" << index << "] != 0) {";
					Type parameter_basic_type = TypeUtils::remove_array_levels(parameter_type, scope_link, dimension_sizes.size());
					if (access_bounds.size() > 0)
					{
						shaper_datastructure_filling
							<< "dimensions[" << index << "][0].original_size = (" << dimension_sizes[0] << ")*" << "sizeof(" << parameter_basic_type.get_declaration(parameter_scope, "") << ")" << ";"
							<< "dimensions[" << index << "][0].reshaped_size = (" << access_bounds[0] << ")*" << "sizeof(" << parameter_basic_type.get_declaration(parameter_scope, "") << ")" << ";"
							<< "dimensions[" << index << "][0].first_index = (" << min_index[0] << ")*" << "sizeof(" << parameter_basic_type.get_declaration(parameter_scope, "") << ")" << ";";
							
					}
					for (unsigned int dimension=1; dimension < access_bounds.size(); dimension++) {
						shaper_datastructure_filling
							<< "dimensions[" << index << "][" << dimension << "].original_size = " << dimension_sizes[dimension] << ";"
							<< "dimensions[" << index << "][" << dimension << "].reshaped_size = " << access_bounds[dimension] << ";"
							<< "dimensions[" << index << "][" << dimension << "].first_index = " << min_index[dimension] << ";";
					}
					shaper_datastructure_filling
						<< "}";
				}
				else if (parameter_type.is_pointer() && !parameter_type.points_to().is_void())
				{
					Source adapter_parameter;
					adapter_parameter
						<< "parameter_data[" << index << "]";
					adapter_parameters.append_with_separator(adapter_parameter, ",");
					
					// Declarations used for determining the shapes
					Source shaper_parameter_initializer;
					shaper_parameter_initializer
						<< "(" << parameter_type.get_declaration(global_scope, "") << ")" << "parameter_data[" << index << "]";
					shaper_parameters
						<< parameter_type.get_declaration_with_initializer(parameter_scope, parameter_symbol.get_name(), shaper_parameter_initializer) << ";";
						
					shaper_datastructure_filling
						<< "if (dimensions[" << index << "] != 0) {";
					Type parameter_basic_type = parameter_type.points_to();
					shaper_datastructure_filling
						<< "dimensions[" << index << "][0].original_size = (" << 1 << ")*" << "sizeof(" << parameter_basic_type.get_declaration(parameter_scope, "") << ")" << ";"
						<< "dimensions[" << index << "][0].reshaped_size = (" << 1 << ")*" << "sizeof(" << parameter_basic_type.get_declaration(parameter_scope, "") << ")" << ";"
						<< "dimensions[" << index << "][0].first_index = (" << 0 << ")*" << "sizeof(" << parameter_basic_type.get_declaration(parameter_scope, "") << ")" << ";";
					shaper_datastructure_filling
						<< "}";
				}
				else
				{
					Source adapter_parameter;
					adapter_parameter
						<< "*("
							<< "(" << parameter_type.get_pointer_to().get_declaration(scope_link.get_scope(symbol.get_point_of_declaration()), "") << ")"
							<< "parameter_data[" << index << "]"
						<< ")";
					adapter_parameters.append_with_separator(adapter_parameter, ",");
					
					// Declarations used for determining the shapes
					Source shaper_parameter_initializer;
					shaper_parameter_initializer
						<< "(" << parameter_type.get_declaration(global_scope, "") << ")"
						<< "*("
							<< "(" << parameter_type.get_pointer_to().get_declaration(scope_link.get_scope(symbol.get_point_of_declaration()), "") << ")"
							<< "parameter_data[" << index << "]"
						<< ")";
					shaper_parameters
						<< parameter_type.get_declaration_with_initializer(parameter_scope, parameter_symbol.get_name(), shaper_parameter_initializer) << ";";
				}
			}
			
			adapter_parameters.append_with_separator(adapter_reshaped_parameters, ", ");
			
			AST_t tree = adapter_source.parse_global(symbol.get_point_of_declaration(), scope_link);
			symbol.get_point_of_declaration().append_to_translation_unit(tree);
		}
	}
	
	
	void TaskReshaper::run(DTO &dto)
	{
		_status = PHASE_STATUS_OK;
		
		try
		{
			Bool generate_task_side = dto["superscalar_generate_task_side"];
			Bool generate_adapters = dto["superscalar_generate_task_adapters"];
			
			AST_t translation_unit = dto["translation_unit"];
			ScopeLink scope_link = dto["scope_link"];
			
			if (generate_task_side)
			{
				DepthTraverse depth_traverse;
				
				PredicateAttr function_definition_predicate(LANG_IS_FUNCTION_DEFINITION) ;
				TraverseASTPredicate function_definition_traverser(function_definition_predicate, AST_t::NON_RECURSIVE);
				FunctionDefinitionHandler function_definition_handler(scope_link);
				depth_traverse.add_functor(function_definition_traverser, function_definition_handler);
				
				depth_traverse.traverse(translation_unit, scope_link);
			}
			
			TaskTable task_table(translation_unit, scope_link, false);
			if (generate_adapters)
			{
				generate_reshaped_task_adapters_and_shapers(task_table, translation_unit, scope_link);
			}
			
		}
		catch (FatalException ex)
		{
			_status = PHASE_STATUS_ERROR;
		}
		
		set_phase_status(_status);
	}

}


EXPORT_PHASE(TL::TaskReshaper);

