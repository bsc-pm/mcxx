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

#include <cstring>

#include "cxx-buildscope.h"
#include "cxx-scope.h"
#include "cxx-utils.h"

#include "tl-exceptions.hpp"
#include "tl-source-bits.hpp"
#include "tl-type-utils.hpp"


namespace TL
{
	Type SourceBits::handle_superscalar_declarator(AST_t ref_tree, ScopeLink scope_link, std::string const &declarator_string, Symbol &original_symbol)
	{
		std::string mangled_text = "@SUPERSCALAR_DECLARATOR@ " + declarator_string;
		char* str = strdup(mangled_text.c_str());
		CXX_LANGUAGE()
		{
			mcxx_prepare_string_for_scanning(str);
		}
		C_LANGUAGE()
		{
			mc99_prepare_string_for_scanning(str);
		}
		
		AST a;
		int parse_result = 0;
		CXX_LANGUAGE()
		{
			parse_result = mcxxparse(&a);
		}
		C_LANGUAGE()
		{
			parse_result = mc99parse(&a);
		}
		if (parse_result != 0)
		{
			throw SyntaxErrorException();
		}
		
		decl_context_t decl_context = scope_link_get_decl_context(scope_link.get_internal_scope_link(), ref_tree.get_internal_ast());
		
		scope_link_set(scope_link.get_internal_scope_link(), a, decl_context);
		
		// Get the name of the declarator
		AST declarator_name = get_declarator_name(a, decl_context);
		std::string const name = std::string(ASTText(declarator_name));
		scope_entry_list_t* entry_list = query_nested_name_flags(decl_context, NULL, NULL, declarator_name, DF_NONE);
		
		if (entry_list == NULL)
		{
			throw UnknownParameterException();
		}
		else if (entry_list->next != NULL)
		{
			throw AmbiguousParameterException();
		}
		
		// Get the original type
		scope_entry_t* entry = entry_list->entry;
		original_symbol = Symbol(entry);
		
		// Create a new variable inside the "virtual" scope, with both, the old type and the declarator part
		gather_decl_spec_t gather_info;
		memset(&gather_info, 0, sizeof(gather_info));
		type_t* declared_type = NULL;
		compute_declarator_type(a, &gather_info, original_symbol.get_type().get_internal_type(), &declared_type, decl_context);
		Type augmented_type(declared_type);
		
		// Fix up the type of the variable (merge the two types into a consistant type)
		return TypeUtils::fix_type(original_symbol.get_type(), augmented_type, scope_link);
	}
	
}

