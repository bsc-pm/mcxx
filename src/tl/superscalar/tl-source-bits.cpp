/*
    Cell/SMP superscalar Compiler
    Copyright (C) 2007-2009 Barcelona Supercomputing Center

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

#include "cxx-attrnames.h"
#include "cxx-buildscope.h"
#include "cxx-scope.h"
#include "cxx-utils.h"
#include "cxx-parser.h"
#include "c99-parser.h"

#include "tl-augmented-symbol.hpp"
#include "tl-exceptions.hpp"
#include "tl-source-bits.hpp"
#include "tl-type-utils.hpp"
#include "tl-source.hpp"


namespace TL
{
    Region SourceBits::handle_superscalar_declarator(AST_t ref_tree, ScopeLink
            scope_link, std::string const &declarator_string, Region::Direction
            direction, Region::Reduction reduction, Symbol &original_symbol)
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
		
		AST superscalar_declarator_ast;
		int parse_result = 0;
		CXX_LANGUAGE()
		{
			parse_result = mcxxparse(&superscalar_declarator_ast);
		}
		C_LANGUAGE()
		{
			parse_result = mc99parse(&superscalar_declarator_ast);
		}
		if (parse_result != 0)
		{
			throw SyntaxErrorException();
		}
		
		decl_context_t decl_context = scope_link_get_decl_context(scope_link.get_internal_scope_link(), ref_tree.get_internal_ast());
		
		AST c_declarator_ast = ASTSon0(superscalar_declarator_ast);
		
		
		scope_link_set(scope_link.get_internal_scope_link(), c_declarator_ast, decl_context);
		
		// Get the name of the declarator
		AST declarator_name = get_declarator_name(c_declarator_ast, decl_context);
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
		
		// Get the original symbol in order to get its type and to enrichen it
		scope_entry_t* entry = entry_list->entry;
		original_symbol = Symbol(entry);
		
		// Create a new variable inside the "virtual" scope, with both, the old type and the declarator part
		type_t* declared_type = NULL;
		gather_decl_spec_t gather_info;
		memset(&gather_info, 0, sizeof(gather_info));
		compute_declarator_type(c_declarator_ast, &gather_info, original_symbol.get_type().get_internal_type(), &declared_type, decl_context); // This call also checks the declarator and adds the tree attributes
		
		// Fix up the type of the variable (merge the two types into a consistant type)
		Type augmented_type = TypeUtils::fix_type(original_symbol.get_type(), Type(declared_type), scope_link);
		
		// Generate the region
		ObjectList<Expression> array_subscripts = get_array_subscript_list(augmented_type, ref_tree, scope_link);
		Region region(direction, reduction, array_subscripts, ASTSon1(superscalar_declarator_ast), ref_tree, scope_link);
		
		// Set scope link to the outermost node so if we query in this tree
		// they will be solved in the proper scope
		scope_link_set(scope_link.get_internal_scope_link(), superscalar_declarator_ast, decl_context);
		
		return region;
	}

#if 0
    ObjectList<Region> SourceBits::handle_superscalar_declarator_list(AST_t ref_tree, 
            ScopeLink scope_link, 
            std::string const &declarator_string, 
            Region::Direction direction,
            Region::Reduction reduction, Symbol &original_symbol)
    {
		std::string mangled_text = "@SUPERSCALAR_DECLARATOR_LIST@ " + declarator_string;
		char* str = strdup(mangled_text.c_str());
		CXX_LANGUAGE()
		{
			mcxx_prepare_string_for_scanning(str);
		}
		C_LANGUAGE()
		{
			mc99_prepare_string_for_scanning(str);
		}
		
		AST superscalar_declarator_ast_list;
		int parse_result = 0;
		CXX_LANGUAGE()
		{
			parse_result = mcxxparse(&superscalar_declarator_ast_list);
		}
		C_LANGUAGE()
		{
			parse_result = mc99parse(&superscalar_declarator_ast_list);
		}
		if (parse_result != 0)
		{
			throw SyntaxErrorException();
		}
		
		decl_context_t decl_context = scope_link_get_decl_context(scope_link.get_internal_scope_link(), 
                ref_tree.get_internal_ast());
		
        AST list, iter;
        list = superscalar_declarator_ast_list;

        ObjectList<Region> result;

        for_each_element(list, iter)
        {
            AST superscalar_declarator_ast = ASTSon1(iter);
            AST c_declarator_ast = ASTSon0(superscalar_declarator_ast);

            scope_link_set(scope_link.get_internal_scope_link(), c_declarator_ast, decl_context);

            // Get the name of the declarator
            AST declarator_name = get_declarator_name(c_declarator_ast, decl_context);
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

            // Get the original symbol in order to get its type and to enrichen it
            scope_entry_t* entry = entry_list->entry;
            original_symbol = Symbol(entry);

            // Create a new variable inside the "virtual" scope, with both, the old type and the declarator part
            type_t* declared_type = NULL;
            gather_decl_spec_t gather_info;
            memset(&gather_info, 0, sizeof(gather_info));
            compute_declarator_type(c_declarator_ast, &gather_info, original_symbol.get_type().get_internal_type(), &declared_type, decl_context); // This call also checks the declarator and adds the tree attributes

            // Fix up the type of the variable (merge the two types into a consistant type)
            Type augmented_type = TypeUtils::fix_type(original_symbol.get_type(), Type(declared_type), scope_link);

            // Generate the region
            ObjectList<Expression> array_subscripts = get_array_subscript_list(augmented_type, ref_tree, scope_link);
            Region region(direction, reduction, array_subscripts, ASTSon1(superscalar_declarator_ast), ref_tree, scope_link);

            // Set scope link to the outermost node so if we query in this tree
            // they will be solved in the proper scope
            scope_link_set(scope_link.get_internal_scope_link(), superscalar_declarator_ast, decl_context);

            result.append(region);
        }

        return result;
    }
#endif
	
	ObjectList<Expression> SourceBits::get_array_subscript_list(Type type, AST_t ref_tree, ScopeLink scope_link)
	{
		ObjectList<Expression> _result;
		
		if (type.is_array())
		{
			_result = get_array_subscript_list(type.array_element(), ref_tree, scope_link);
			if (!type.explicit_array_dimension())
			{
				throw MissingArrayDimensions();
			}
			
			_result.push_back(
				Expression(
					Source(type.array_dimension().prettyprint())
					.parse_expression(ref_tree, scope_link),
					scope_link
			    	)
			);
			
		}
		
		return _result;
	}
	
	
}

