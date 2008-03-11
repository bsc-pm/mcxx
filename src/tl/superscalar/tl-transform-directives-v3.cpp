/*
    SMP superscalar Compiler
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

#include "tl-transform-directives.hpp"
#include "tl-langconstruct.hpp"


namespace TL
{
	CompilerPhase::PhaseStatus TransformDirectives::_status;
	
	void TL::TransformDirectives::process_start(PragmaCustomConstruct directive)
	{
		if (directive.is_construct())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'start' directive used as a construct." << std::endl;
			TransformDirectives::fail();
			return;
		}
		
		Source source;
		source
			<< "css_preInit();"
			<< "__cssgenerated_task_registration();"
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
			return;
		}
		
		if (!on_clause.is_defined())
		{
			std::cerr << directive.get_ast().get_locus() << " Error: 'wait' directive without 'on' clause." << std::endl;
			return;
		}
		
		ObjectList<Expression> on_list = on_clause.get_expression_list();
		
		Source addressListSource;
		Source source;
		source
			<< "{"
				<< "void *__cssgenerated_addresses[] = {" << addressListSource << "};"
				<< "css_waitOn(" << on_list.size() << ", __cssgenerated_addresses);"
			<< "}";
		
		for (ObjectList<Expression>::iterator it = on_list.begin(); it != on_list.end(); it++) {
			Expression expression = *it;
			if (it != on_list.begin()) {
				addressListSource << ", ";
			}
			bool can_be_lvalue;
			Type type = expression.get_type(can_be_lvalue);
			if (!type.is_pointer() && !type.is_array()) {
				if (can_be_lvalue) {
					addressListSource << "&(" << expression.prettyprint() << ")";
				} else {
					std::cerr << expression.get_ast().get_locus() << " Error: '" << expression.prettyprint() << "' is not a pointer, an array or an lvalue.";
					TransformDirectives::fail();
				}
			} else {
				addressListSource << expression.prettyprint();
			}
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
		if (!declared_entity.is_functional_declaration())
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
	
	
}


EXPORT_PHASE(TL::TransformDirectives);

