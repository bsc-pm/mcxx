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




#include "tl-transform-directives-v4.hpp"
#include "tl-langconstruct.hpp"
#include "tl-parameter-expression.hpp"
#include "tl-region.hpp"
#include "tl-source-bits.hpp"
#include "tl-type-utils.hpp"


namespace TL
{
	template<typename V, typename E>
	static bool inline value_in_list(V value, E elem)
	{
		return value == elem;
	}
	
	template<typename V, typename E>
	static bool inline value_in_list(V value, E elem1, E elem2)
	{
		return value_in_list(value, elem1) || value == elem2;
	}
	
	template<typename V, typename E>
	static bool inline value_in_list(V value, E elem1, E elem2, E elem3)
	{
		return value_in_list(value, elem1, elem2) || value == elem3;
	}
	
	template<typename V, typename E>
	static bool inline value_in_list(V value, E elem1, E elem2, E elem3, E elem4)
	{
		return value_in_list(value, elem1, elem2) || value_in_list(value, elem3, elem4);
	}
	
	template<typename V, typename E>
	static bool inline value_in_list(V value, E elem1, E elem2, E elem3, E elem4, E elem5)
	{
		return value_in_list(value, elem1, elem2, elem3, elem4) || value == elem5;
	}
	
	template<typename V, typename E>
	static bool inline value_in_list(V value, E elem1, E elem2, E elem3, E elem4, E elem5, E elem6)
	{
		return value_in_list(value, elem1, elem2, elem3) || value_in_list(value, elem4, elem5, elem6);
	}
	
	template<typename V, typename E>
	static bool inline value_in_list(V value, E elem1, E elem2, E elem3, E elem4, E elem5, E elem6, E elem7)
	{
		return value_in_list(value, elem1, elem2, elem3) || value_in_list(value, elem4, elem5, elem6, elem7);
	}
	
	
	
	CompilerPhase::PhaseStatus TransformDirectives::_status;
	
	void TL::TransformDirectives::process_start(PragmaCustomConstruct directive)
	{
		if (directive.is_construct())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'start' directive used as a construct." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		if (!directive.get_clause_names().empty())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: the 'start' directive does not accept any clauses." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		Source source;
		source
			<< "css_preInit();"
			<< "task_registration__cssgenerated();"
			<< "css_init();" ;
		
		AST_t tree = source.parse_statement(directive.get_ast(), directive.get_scope_link());
		directive.get_ast().replace_with(tree);
	}
	
	void TL::TransformDirectives::process_finish(PragmaCustomConstruct directive)
	{
		if (directive.is_construct())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'finish' directive used as a construct." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		if (!directive.get_clause_names().empty())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: the 'finish' directive does not accept any clauses." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		Source source;
		source << "css_finish();";
		
		AST_t tree = source.parse_statement(directive.get_ast(), directive.get_scope_link());
		directive.get_ast().replace_with(tree);
	}
	
	void TL::TransformDirectives::process_barrier(PragmaCustomConstruct directive)
	{
		if (directive.is_construct())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'barrier' directive used as a construct." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		if (!directive.get_clause_names().empty())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: the 'barrier' directive does not accept any clauses." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		Source source;
		source << "css_barrier();";
		
		AST_t tree = source.parse_statement(directive.get_ast(), directive.get_scope_link());
		directive.get_ast().replace_with(tree);
	}
	
	void TL::TransformDirectives::process_wait(PragmaCustomConstruct directive)
	{
		if (directive.is_construct())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'wait' directive used as a construct." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		PragmaCustomClause on_clause = directive.get_clause("on");
		
		if (!directive.is_directive())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'wait' directive used as a construct." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		if (!on_clause.is_defined())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'wait' directive without 'on' clause." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		ObjectList<std::string> clauses = directive.get_clause_names();
		for (ObjectList<std::string>::const_iterator it = clauses.begin(); it != clauses.end(); it++)
		{
			std::string const &clause = *it;
			if (!value_in_list(clause, "on"))
			{
				std::cerr << directive.get_ast().get_locus() << " Error: invalid clause '" << clause << "' used in 'wait' directive." << std::endl;
				TransformDirectives::fail();
			}
		}
		
		ObjectList<std::string> on_list = on_clause.get_arguments(ExpressionTokenizerTrim());
		
		Source variableListSource;
		
		Source dimensions_source;
		Source expression_region_source;
		Source expression_initializers_source;
		Source wait_on_code;
		
		Source source;
		source
			<< "{"
				<< dimensions_source
				<< expression_region_source
				<< "css_parameter_t expressions__cssgenerated[" << on_list.size() << "];"
				<< expression_initializers_source
				<< wait_on_code
			<< "}";
		
		wait_on_code <<
			"css_waitOn(" << on_list.size() << ", expressions__cssgenerated);";
		
		AST_t ref_ast = directive.get_ast();
		ScopeLink scope_link = directive.get_scope_link();
		
		int index = 0;
		for (ObjectList<std::string>::iterator it = on_list.begin(); it != on_list.end(); it++)
		{
			Source address_source;
			
			Region region;
			Expression expression =
				SourceBits::handle_superscalar_expression(ref_ast, scope_link, *it, region);
			
			expression_initializers_source
				<< "expressions__cssgenerated[" << index << "].address = " << address_source << ";"
				<< "expressions__cssgenerated[" << index << "].region_count = 1;"
				<< "expressions__cssgenerated[" << index << "].regions = expression_" << index << "_region__cssgenerated;";
			
			expression_region_source
				<< "css_parameter_region_t expression_" << index << "_region__cssgenerated[1];";
			
			Source dimension_count_source;
			
			expression_region_source
				<< "expression_" << index << "_region__cssgenerated[0].flags = 0;"
				<< "expression_" << index << "_region__cssgenerated[0].dimension_count = " << dimension_count_source << ";"
				<< "expression_" << index << "_region__cssgenerated[0].dimensions = expression_" << index << "_region_dimensions__cssgenerated;";
			
			dimensions_source
				<< "css_parameter_dimension_t expression_" << index << "_region_dimensions__cssgenerated[" << dimension_count_source << "];";
			
			bool is_lvalue;
			Type expression_type = TypeUtils::normalize_type(expression.get_type(is_lvalue), ref_ast, scope_link);
			
			if (region.get_dimension_count() > 0)
			{
				// An array
				dimension_count_source
					<< region.get_dimension_count();
				
				Source dimension_base;
				dimension_base
					<< "sizeof("
						<< TypeUtils::get_array_element_type(expression_type, scope_link)
							.get_declaration(expression.get_scope(), std::string(""))
					<< ")";
				
				// The first dimension is base 1 (in terms of bytes)
				// The rest of the dimensions are automatically based on the previous one
				for (unsigned int dimension_index = 0; dimension_index < region.get_dimension_count(); dimension_index++)
				{
					Expression parametrized_dimension_length = region[dimension_index].get_dimension_length();
					Expression parametrized_dimension_start = region[dimension_index].get_dimension_start();
					Expression parametrized_accessed_length = region[dimension_index].get_accessed_length();
					
					dimensions_source
						<< "expression_" << index << "_region_dimensions__cssgenerated[" << dimension_index << "].size = "
							<< dimension_base.get_source() << " * (" << parametrized_dimension_length.prettyprint() << ");"
						<< "expression_" << index << "_region_dimensions__cssgenerated[" << dimension_index << "].lower_bound = "
							<< dimension_base.get_source() << " * (" << parametrized_dimension_start.prettyprint() << ");"
						<< "expression_" << index << "_region_dimensions__cssgenerated[" << dimension_index << "].accessed_length = "
							<< dimension_base.get_source() << " * (" << parametrized_accessed_length.prettyprint() << ");";
					dimension_base = Source("1");
				}
				
				address_source
					<< expression.prettyprint();
			}
			else if (expression_type.is_pointer())
			{
				// A pointer to one element
				dimension_count_source
					<< "1";
				Source dimension_base;
				dimension_base
					<< "sizeof("
						<< expression_type.points_to()
							.get_declaration(expression.get_scope(), std::string(""))
					<< ")";
				dimensions_source
					<< "expression_" << index << "_region_dimensions__cssgenerated[0].size = "
						<< dimension_base.get_source() << " * 1;"
					<< "expression_" << index << "_region_dimensions__cssgenerated[0].lower_bound = "
						<< dimension_base.get_source() << " * 0;"
					<< "expression_" << index << "_region_dimensions__cssgenerated[0].accessed_length = "
						<< dimension_base.get_source() << " * 1;";
				
				address_source
					<< expression.prettyprint();
			}
			else if (is_lvalue)
			{
				// An element
				dimension_count_source
					<< "1";
				Source dimension_base;
				dimension_base
					<< "sizeof("
						<< expression_type
							.get_declaration(expression.get_scope(), std::string(""))
					<< ")";
				dimensions_source
					<< "expression_" << index << "_region_dimensions__cssgenerated[0].size = "
						<< dimension_base.get_source() << " * 1;"
					<< "expression_" << index << "_region_dimensions__cssgenerated[0].lower_bound = "
						<< dimension_base.get_source() << " * 0;"
					<< "expression_" << index << "_region_dimensions__cssgenerated[0].accessed_length = "
						<< dimension_base.get_source() << " * 1;";
				
				address_source
					<< "&(" << expression.prettyprint() << ")";
			}
			else
			{
				std::cerr << expression.get_ast().get_locus() << " Error: '" << expression.prettyprint() << "' is not a pointer, an array or an lvalue.";
				TransformDirectives::fail();
			}
			
			index++;
		}
		
		AST_t tree = source.parse_statement(directive.get_ast(), directive.get_scope_link());
		directive.get_ast().replace_with(tree);
	}
	
	
	void TL::TransformDirectives::process_restart(PragmaCustomConstruct directive)
	{
		if (directive.is_construct())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'restart' directive used as a construct." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		if (!directive.is_directive())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'restart' directive used as a construct." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		if (!directive.get_clause_names().empty())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: the 'restart' directive does not accept any clauses." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		Source source;
		source << "css_restart();";
		
		AST_t tree = source.parse_statement(directive.get_ast(), directive.get_scope_link());
		directive.get_ast().replace_with(tree);
	}
	
	
	void TL::TransformDirectives::process_task(PragmaCustomConstruct construct)
	{
		if (construct.is_directive())
		{
			std::cerr << construct.get_ast().get_locus() << " Error: 'task' construct used as a directive." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		ObjectList<std::string> clauses = construct.get_clause_names();
		for (ObjectList<std::string>::const_iterator it = clauses.begin(); it != clauses.end(); it++)
		{
			std::string const &clause = *it;
			if (!value_in_list(clause, "input", "output", "inout", "highpriority", "target", "reduction", "device"))
			{
				std::cerr << construct.get_ast().get_locus() << " Error: invalid clause '" << clause << "' used in 'task' construct." << std::endl;
				TransformDirectives::fail();
			}
		}
		
		if (construct.is_function_definition())
		{
			process_task_definition(construct);
		}
		else
		{
			process_task_declaration(construct);
		}
		
	}
	
	
	void TL::TransformDirectives::process_task_declaration(PragmaCustomConstruct construct)
	{
		Declaration general_declaration(construct.get_declaration(), construct.get_scope_link());
		ObjectList<DeclaredEntity> declared_entities = general_declaration.get_declared_entities();
		
		if (declared_entities.size() != 1)
		{
			// Already emmited error in TaskAnalysis
			return;
		}
		
		DeclaredEntity declared_entity = *(declared_entities.begin());
		if (!declared_entity.is_functional_declaration())
		{
			// Already emmited error in TaskAnalysis
			return;
		}
		
		construct.get_ast().replace_with(general_declaration.get_ast());
	}
	
	
	void TL::TransformDirectives::process_task_definition(PragmaCustomConstruct construct)
	{
		FunctionDefinition task_definition(construct.get_declaration(), construct.get_scope_link());
		construct.get_ast().replace_with(task_definition.get_ast());
	}
	
	
	void TL::TransformDirectives::process_target(PragmaCustomConstruct construct)
	{
		if (construct.is_directive())
		{
			std::cerr << construct.get_ast().get_locus() << " Error: 'target' construct used as a directive." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		ObjectList<std::string> clauses = construct.get_clause_names();
		for (ObjectList<std::string>::const_iterator it = clauses.begin(); it != clauses.end(); it++)
		{
			std::string const &clause = *it;
			if (!value_in_list(clause, "spu", "ppu"))
			{
				std::cerr << construct.get_ast().get_locus() << " Error: invalid clause '" << clause << "' used in 'target' construct." << std::endl;
				TransformDirectives::fail();
			}
		}
		
		if (construct.is_function_definition())
		{
			process_target_definition(construct);
		}
		else
		{
			process_target_declaration(construct);
		}
		
	}
	
	
	void TL::TransformDirectives::process_target_declaration(PragmaCustomConstruct construct)
	{
		Declaration general_declaration(construct.get_declaration(), construct.get_scope_link());
		ObjectList<DeclaredEntity> declared_entities = general_declaration.get_declared_entities();
		
		if (declared_entities.size() != 1)
		{
			std::cerr << construct.get_declaration().get_locus() << " Error: the target construct allows one declaration only."  << std::endl;
			return;
		}
		
		DeclaredEntity declared_entity = *(declared_entities.begin());
		Symbol function_symbol = declared_entity.get_declared_symbol();
		
		if (!function_symbol.is_function())
		{
			std::cerr << declared_entity.get_ast().get_locus() << " Error: the target construct can only be applied to functions."  << std::endl;
			return;
		}
		
		construct.get_ast().replace_with(general_declaration.get_ast());
	}
	
	
	void TL::TransformDirectives::process_target_definition(PragmaCustomConstruct construct)
	{
		FunctionDefinition target_definition(construct.get_declaration(), construct.get_scope_link());
		construct.get_ast().replace_with(target_definition.get_ast());
	}
	
	
	void TL::TransformDirectives::process_mutex(PragmaCustomConstruct directive)
	{
		if (directive.is_construct())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'mutex' directive used as a construct." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		PragmaCustomClause lock_clause = directive.get_clause("lock");
		PragmaCustomClause unlock_clause = directive.get_clause("unlock");
		
		if (!lock_clause.is_defined() && !unlock_clause.is_defined())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'mutex' directive used without 'lock' or 'unlock' clause." << std::endl;
			TransformDirectives::fail();
			return;
		}

		if (lock_clause.is_defined() && unlock_clause.is_defined())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'mutex' directive can't be used with both 'lock' and 'unlock' clauses at the same time." << std::endl;
			TransformDirectives::fail();
		}

		ObjectList<Expression> argument_list;
		Source action;
		
		if (lock_clause.is_defined())
		{
			argument_list = lock_clause.get_expression_list();
			action << "lock";
		} else {
			argument_list = unlock_clause.get_expression_list();
			action << "unlock";
		}
		
		
		Source source;
		Source parameter;
		source << "css_"<< action <<"((unsigned long)" << parameter << ");";
		
		if (argument_list.size() != 1)
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'lock' and 'unlock' only accept one argument." << std::endl;
			TransformDirectives::fail();
		}
		
		Expression expression = *argument_list.begin();
		parameter << expression;
		
		AST_t tree = source.parse_statement(directive.get_ast(), directive.get_scope_link());
		directive.get_ast().replace_with(tree);
	}

}


EXPORT_PHASE(TL::TransformDirectives);

