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




#ifndef TL_TYPE_UTILS_HPP
#define TL_TYPE_UTILS_HPP


#include "tl-langconstruct.hpp"
#include "tl-objectlist.hpp"
#include "tl-scopelink.hpp"
#include "tl-type.hpp"

#include "tl-exceptions.hpp"


namespace TL
{
	class TypeUtils
	{
		private:
			static Type remove_inner_pointer_rec(Type type, bool &filter, ScopeLink scope_link)
			{
				if (type.is_pointer())
				{
					Type subtype = type.points_to();
					type = remove_inner_pointer_rec(subtype, filter, scope_link);
					if (filter)
					{
						filter = false;
						return subtype;
					}
					else
					{
						return type;
					}
				}
				else if (type.is_reference())
				{
					Type subtype = type.references_to();
					subtype = remove_inner_pointer_rec(subtype, filter, scope_link);
					type = subtype.get_reference_to();
					return type;
				}
				else if (type.is_array())
				{
					Type subtype = type.array_element();
					subtype = remove_inner_pointer_rec(subtype, filter, scope_link);
					type = subtype.get_array_to(type.array_dimension(), scope_link.get_scope(type.array_dimension()));
					return type;
				}
				else
				{
					filter = true;
					return type;
				}
			}
			
			static bool parameter_types_match(AST_t first, AST_t second, ScopeLink scope_link)
			{
				Expression first_expression(first, scope_link);
				Expression second_expression(second, scope_link);
				
				return parameter_types_match(first_expression, second_expression, scope_link);
			}
			
			static bool parameter_types_match(Expression first, Expression second, ScopeLink scope_link)
			{
				return parameter_types_match(first.get_type(), second.get_type(), scope_link);
			}
			
		public:
			static Type get_basic_type(Type type)
			{
				if (type.is_pointer())
				{
					return get_basic_type(type.points_to());
				}
				else if (type.is_array())
				{
					return get_basic_type(type.array_element());
				}
				else if (type.is_reference())
				{
					return get_basic_type(type.references_to());
				}
				
				return type;
			}
			
			static ObjectList<Expression> get_array_dimensions(Type type, ScopeLink scope_link)
			{
				ObjectList<Expression> result;
				if (type.is_array())
				{
					result = get_array_dimensions(type.array_element(), scope_link);
					if (!type.explicit_array_dimension())
					{
						throw NotFoundException();
					}
					result.push_back(Expression(type.array_dimension(), scope_link));
				}
				return result;
			}
			
			static Type remove_array_levels(Type type, ScopeLink scope_link, int levels)
			{
				if (type.is_array() && levels > 0)
				{
					return remove_array_levels(type.array_element(), scope_link, levels-1);
				}
				else
				{
					return type;
				}
			}
			
			static Type get_array_element_type(Type type, ScopeLink scope_link)
			{
				if (type.is_array())
				{
					return get_array_element_type(type.array_element(), scope_link);
				}
				else
				{
					return type;
				}
			}
			
			static Type remove_inner_pointer(Type type, ScopeLink scope_link)
			{
				bool filter = false;
				return remove_inner_pointer_rec(type, filter, scope_link);
			}
			
			static unsigned int count_dimensions(Type type)
			{
				if (type.is_pointer())
				{
					return count_dimensions(type.points_to());
				}
				else if (type.is_reference())
				{
					return count_dimensions(type.references_to());
				}
				else if (type.is_array())
				{
					return count_dimensions(type.array_element()) + 1;
				}
				else
				{
					return 0;
				}
			}
			
			// We pass the original parameter type, and then the augmented type creaded with type original type + the superscalar declarator.
			// If the declarator had any array specifier, then the augmented type is invalid and must be fixed and checked
			static Type fix_type(Type original_type, Type augmented_type, ScopeLink scope_link)
			{
				if (original_type.is_pointer() && augmented_type.is_array())
				{
					// #pragma css task direction(a[N])
					// void f(type *a)
					return remove_inner_pointer(augmented_type, scope_link);
				}
				else if ( original_type.is_pointer() == (augmented_type.is_pointer() || augmented_type.is_array()) )
				{
					// #pragma css task direction(a)
					// void f(type *a)
					return original_type;
				}
				else if (original_type.is_array() && augmented_type.is_array())
				{
					if (count_dimensions(augmented_type) == count_dimensions(original_type))
					{
						// #pragma css task direction(a)
						// void f(type a[N])
						return original_type;
					}
					else
					{
						// #pragma css task direction(a[xx])
						// void f(type a[N])
						throw InvalidDimensionSpecifiersException();
					}
				}
				else
				{
					// One (or both) of the types is invalid (e.g. a function)
					throw InvalidTypeException();
				}
			}
			
			// Check if a parameter declaration from a function declaration/definition matches with the same parameter from another declaration/definition of the same function.
			static bool parameter_types_match(Type first, Type second, ScopeLink scope_link)
			{
				if (!first.is_valid())
				{
					throw InvalidTypeException();
				}
				if (!second.is_valid())
				{
					throw InvalidTypeException();
				}
				
				return first.is_same_type(second);
			}
			
			
			static bool is_extern_declaration(Declaration declaration)
			{
				DeclarationSpec specifiers = declaration.get_declaration_specifiers();
				AST_t ast = specifiers.get_ast();
				
				// FIXME: bug Rofi about adding this functionality to the type or to the symbol class
				return (ast.prettyprint(false).find("extern") != std::string::npos);
			}
			
			static Type normalize_type(Type type, AST_t ref_ast, ScopeLink scope_link)
			{
				if (type.is_pointer())
				{
					Source maximum_source;
					maximum_source
						<< "~0UL";
					AST_t maximum_ast = maximum_source.parse_expression(ref_ast, scope_link);
					
					return type.points_to().get_array_to(maximum_ast, scope_link.get_scope(ref_ast));
				}
				else
				{
					return type;
				}
			}
			
	};
	
	
}


#endif // TL_TYPE_UTILS_HPP
