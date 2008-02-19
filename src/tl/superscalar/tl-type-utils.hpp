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
				if (!parameter_types_match(first.get_type(), second.get_type(), scope_link))
				{
					return false;
				}
				
				if (first.is_id_expression())
				{
					if (second.is_id_expression())
					{
						Symbol first_symbol = first.get_id_expression().get_symbol();
						Symbol second_symbol = second.get_id_expression().get_symbol();
						
						if (first_symbol.is_parameter())
						{
							return second_symbol.is_parameter()
								&& (first_symbol.get_parameter_position() == second_symbol.get_parameter_position());
						}
						return (first_symbol == second_symbol);
					}
					else
					{
						return false;
					}
				}
				if (first.is_binary_operation())
				{
					return second.is_binary_operation()
						&& (first.get_operation_kind() == second.get_operation_kind())
						&& parameter_types_match(first.get_first_operand(), second.get_first_operand(), scope_link)
						&& parameter_types_match(first.get_second_operand(), second.get_second_operand(), scope_link);
				}
				if (first.is_unary_operation())
				{
					return second.is_unary_operation()
						&& (first.get_operation_kind() == second.get_operation_kind())
						&& parameter_types_match(first.get_unary_operand(), second.get_unary_operand(), scope_link);
				}
				if (first.is_casting())
				{
					return second.is_casting()
						&& parameter_types_match(first.get_cast_type(), second.get_cast_type(), scope_link)
						&& parameter_types_match(first.get_casted_expression(), second.get_casted_expression(), scope_link);
				}
				if (first.is_literal())
				{
					return second.is_literal()
						&& (first.prettyprint() == second.prettyprint());
				}
				if (first.is_function_call())
				{
					if (second.is_function_call()
						&& parameter_types_match(first.get_called_expression(), second.get_called_expression(), scope_link)
						&& first.get_argument_list().size() == second.get_argument_list().size())
					{
						ObjectList<Expression> list1 = first.get_argument_list();
						ObjectList<Expression> list2 = second.get_argument_list();
						ObjectList<Expression>::iterator it1 = list1.begin();
						ObjectList<Expression>::iterator it2 = list2.begin();
						for (; it1 != list1.end(); it1++, it2++)
						{
							if (!parameter_types_match(*it1, *it2, scope_link))
							{
								return false;
							}
						}
						return true;
					}
					else
					{
						return false;
					}
				}
				if (first.is_assignment())
				{
					return second.is_assignment()
						&& parameter_types_match(first.get_first_operand(), second.get_first_operand(), scope_link)
						&& parameter_types_match(first.get_second_operand(), second.get_second_operand(), scope_link);
				}
				if (first.is_operation_assignment())
				{
					return second.is_operation_assignment()
						&& (first.get_operation_kind() == second.get_operation_kind())
						&& parameter_types_match(first.get_first_operand(), second.get_first_operand(), scope_link)
						&& parameter_types_match(first.get_second_operand(), second.get_second_operand(), scope_link);
				}
				if (first.is_array_subscript())
				{
					return second.is_array_subscript()
						&& parameter_types_match(first.get_subscript_expression(), second.get_subscript_expression(), scope_link)
						&& parameter_types_match(first.get_subscripted_expression(), second.get_subscripted_expression(), scope_link);
				}
				if (first.is_member_access())
				{
					return second.is_member_access()
						&& parameter_types_match(first.get_accessed_entity(), second.get_accessed_entity(), scope_link)
						&& parameter_types_match(first.get_accessed_member().get_ast(), second.get_accessed_member().get_ast(), scope_link);
				}
				if (first.is_pointer_member_access())
				{
					return second.is_pointer_member_access()
						&& parameter_types_match(first.get_accessed_entity(), second.get_accessed_entity(), scope_link)
						&& parameter_types_match(first.get_accessed_member().get_ast(), second.get_accessed_member().get_ast(), scope_link);
				}
				if (first.is_conditional())
				{
					return second.is_conditional()
						&& parameter_types_match(first.get_condition_expression(), second.get_condition_expression(), scope_link)
						&& parameter_types_match(first.get_true_expression(), second.get_true_expression(), scope_link)
						&& parameter_types_match(first.get_false_expression(), second.get_false_expression(), scope_link);
				}
				
				return false;
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
				else if (original_type.is_pointer() == augmented_type.is_pointer())
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
				if (!first.is_valid() || !second.is_valid())
				{
					throw InvalidTypeException();
				}
				
				if (first == second)
				{
					return true;
				}
				
				if (first.is_non_derived_type())
				{
					return false;
				}
				if (first.is_non_derived_type())
				{
					return false;
				}
				if (first.is_class())
				{
					return false;
				}
				if (first.is_enum())
				{
					return false;
				}
				if (first.is_function())
				{
					return false;
				}
				if (first.is_pointer())
				{
					if (second.is_pointer())
					{
						return parameter_types_match(first.points_to(), second.points_to(), scope_link);
					}
					else
					{
						return false;
					}
				}
				if (first.is_array())
				{
					if (second.is_array())
					{
						if ( parameter_types_match(first.array_element(), second.array_element(), scope_link)
							&& (first.explicit_array_dimension() == second.explicit_array_dimension()) )
						{
							if (first.explicit_array_dimension())
							{
								return parameter_types_match(first.array_dimension(), second.array_dimension(), scope_link);
							}
							else
							{
								return true;
							}
						}
						else
						{
							return false;
						}
					}
					else
					{
						return false;
					}
				}
				if (first.is_reference())
				{
					if (second.is_reference())
					{
						return parameter_types_match(first.references_to(), second.references_to(), scope_link);
					}
					else
					{
						return false;
					}
				}
				return first.is_void() && second.is_void();
			}
			
			
			static bool is_extern_declaration(Declaration declaration)
			{
				DeclarationSpec specifiers = declaration.get_declaration_specifiers();
				AST_t ast = specifiers.get_ast();
				
				// FIXME: bug Rofi about adding this functionality to the type or to the symbol class
				return (ast.prettyprint(false).find("extern") != std::string::npos);
			}
			
	};
	
	
#if 0
	class SimpleType
	{
		public:
			enum type_t
			{
				BOOL_T,
				CHAR_T,
				WCHAR_T,
				SHORT_T,
				INT_T,
				LONG_T,
				LONG_LONG_T,
				FLOAT_T,
				DOUBLE_T,
				LONG_DOUBLE_T,
				VOID_T
			};
		
		private:
			type_t _type;
			
		public:
			SimpleType(type_t type)
				: _type(type)
			{
			}
			
			SimpleType(Type const &type)
			{
				if (!type.is_non_derived_type())
				{
					throw InvalidArgumentException();
				}
				
				if (type.is_bool())
				{
					_type = BOOL_T;
				}
				else if (type.is_char())
				{
					_type = CHAR_T;
				}
				else if (type.is_wchar_t())
				{
					_type = WCHAR_T;
				}
				else if (type.is_integral_type())
				{
					if (type.is_signed_int() || type.is_unsigned_int())
					{
						_type = INT_T;
					}
					else if (type.is_signed_short_int() || type.is_unsigned_short_int())
					{
						_type = SHORT_T;
					}
					else if (type.is_signed_long_int() || type.is_unsigned_long_int())
					{
						_type = LONG_T;
					}
					else if (type.is_signed_long_long_int() || type.is_unsigned_long_long_int())
					{
						_type = LONG_LONG_T;
					}
				}
				else if (type.is_floating_type())
				{
					if (type.is_float())
					{
						_type = FLOAT_T;
					}
					else if (type.is_double())
					{
						_type = DOUBLE_T;
					}
					else if (type.is_long_double())
					{
						_type = LONG_DOUBLE_T;
					}
				}
				else if (type.is_void())
				{
					_type = VOID_T;
				}
				else
				{
					throw InvalidArgumentException();
				}
			}
			
			bool operator==(SimpleType const &other) const
			{
				return _type == other._type;
			}
			bool operator==(SimpleType other) const
			{
				return _type == other._type;
			}
			bool operator!=(SimpleType const &other) const
			{
				return _type == other._type;
			}
			bool operator!=(SimpleType other) const
			{
				return _type == other._type;
			}
			
	};
#endif
	
	
}


#endif // TL_TYPE_UTILS_HPP
