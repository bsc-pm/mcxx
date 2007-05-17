/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
	Acotes Translation Phase
	Copyright (C) 2007 - David Rodenas Pico <david.rodenas@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-symboltransformhelper.hpp"

#include <assert.h>


#include "tl-ast.hpp"
#include "tl-langconstruct.hpp"
#include "tl-scopelink.hpp"
#include "tl-symbol.hpp"


namespace TL
{

// copy_all_from_struct --------------------------------------------------------
std::string 
SymbolTransformHelper::
copy_all_from_struct
		( const std::set<Symbol>& symbols
		, const std::string& struct_instance_name
		)
{
	std::stringstream ss;	
	
	for		( std::set<Symbol>::iterator it= symbols.begin()
			; it != symbols.end()
			; it++)
	{
		Symbol symbol= *it;
		
		ss << copy_from_struct(symbol, struct_instance_name);
	}
	
	return ss.str();
}

// copy_all_to_struct ----------------------------------------------------------
std::string 
SymbolTransformHelper::
copy_all_to_struct
		( const std::set<Symbol>& symbols
		, const std::string& struct_instance_name
		)
{
	std::stringstream ss;	
	
	ss << "";
	for		( std::set<Symbol>::iterator it= symbols.begin()
			; it != symbols.end()
			; it++)
	{
		Symbol symbol= *it;
		
		ss << copy_to_struct(symbol, struct_instance_name);
	}
	ss << "";
	
	return ss.str();
}

// copy_from_struct ------------------------------------------------------------
std::string 
SymbolTransformHelper::
copy_from_struct
		( const Symbol& symbol
		, const std::string& struct_instance_name
		)
{
	std::stringstream ss;	

	std::string name= symbol.get_name();

	ss		<< "memcpy"
			<< "( &(" << name << ")"
			<< ", &(" << struct_instance_name << "." << name << ")"
			<< ", sizeof(" << name << ")"
			<< ");"
			;
	
	return ss.str();
}

// copy_to_struct --------------------------------------------------------------
std::string 
SymbolTransformHelper::
copy_to_struct
		( const Symbol& symbol 
		, const std::string& struct_instance_name
		)
{
	std::stringstream ss;	

	std::string name= symbol.get_name();

	ss		<< "memcpy"
			<< "( &(" << struct_instance_name << "." << name << ")"
			<< ", &(" << name << ")"
			<< ", sizeof(" << name << ")"
			<< ");"
			;
	
	return ss.str();
}

// declare ---------------------------------------------------------------------
std::string 
SymbolTransformHelper::
declare
		( const Symbol& symbol
		)
{
	Type type= symbol.get_type();
	Scope scope= symbol.get_scope();
	std::string name= symbol.get_name();

	std::stringstream ss;	
	
	ss
			<< type.get_declaration(scope, name)
			<< ";"
			;
	
	return ss.str();
}

// declare_all -----------------------------------------------------------------
std::string 
SymbolTransformHelper::
declare_all
		( const std::set<Symbol>& symbols
		)
{
	std::stringstream ss;	
	
	for		( std::set<Symbol>::iterator it= symbols.begin()
			; it != symbols.end()
			; it++)
	{
		Symbol symbol= *it;
		
		ss << declare(symbol);
	}
	
	return ss.str();
}

// declare_reference -----------------------------------------------------------
std::string 
SymbolTransformHelper::
declare_reference
		( const Symbol& symbol
		)
{
	Type type= symbol.get_type();
	Scope scope= symbol.get_scope();
	std::string name= symbol.get_name();

	std::stringstream ss;	
	
	ss
			<< type.get_pointer_to().get_declaration(scope, name)
			<< ";"
			;
	
	return ss.str();
}

// declare_reference_all -------------------------------------------------------
std::string 
SymbolTransformHelper::
declare_reference_all
		( const std::set<Symbol>& symbols
		)
{
	std::stringstream ss;	
	
	for		( std::set<Symbol>::iterator it= symbols.begin()
			; it != symbols.end()
			; it++)
	{
		Symbol symbol= *it;
		
		ss << declare_reference(symbol);
	}
	
	return ss.str();
}

// transform_to_reference ------------------------------------------------------
Statement 
SymbolTransformHelper::
transform_to_reference
		( const Symbol& symbol
		, const Statement& fake
		)
{
	ReplaceIdExpression replacer;

	Statement body= fake;
	AST_t body_ast = body.get_ast();
	ScopeLink body_scope_link = body.get_scope_link();	
	Source reference_src= std::string("(*") + symbol.get_name() + ")";
	AST_t reference_ast= reference_src.
			parse_expression(body_ast, body_scope_link);
	
	replacer.add_replacement(symbol, reference_ast);

	Statement modified_body = replacer.replace(body);
	
	return modified_body;	
}

// transform_all_to_reference --------------------------------------------------
Statement 
SymbolTransformHelper::
transform_all_to_reference
		( const std::set<Symbol>& symbols
		, const Statement& body
		)
{
	Statement replaced= body;
	
	for 	( std::set<Symbol>::iterator it= symbols.begin()
			; it != symbols.end()
			; it++
			)
	{
		Symbol symbol= *it;
		
		replaced= transform_to_reference(symbol, replaced);
	}
	
	return replaced;
}

// SymbolTransformHelper constructor -------------------------------------------
SymbolTransformHelper::
SymbolTransformHelper()
{
	// is private and not constructed
	assert(0);
}

}
