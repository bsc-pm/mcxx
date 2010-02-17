/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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


#include "cxx-ambiguity.h"
#include "cxx-exprtype.h"

#include <list>

#include "tl-ast.hpp"
#include "tl-langconstruct.hpp"
#include "tl-object.hpp"
#include "tl-scopelink.hpp"

#include "tl-exceptions.hpp"
#include "tl-parameter-expression.hpp"
#include "tl-region.hpp"


namespace TL {
	bool Region::DimensionSpecifier::is_full()
	{
		Source zero_source;
		zero_source << "0";
		Expression zero(
			zero_source.parse_expression(_dimension.get_ast(), _dimension.get_scope_link())
			, _dimension.get_scope_link()
		);
		
#if 0
		return
			ParameterExpression::match(zero, get_dimension_start())
			&&
			ParameterExpression::match(get_dimension_length(), get_accessed_length());
#else
		return false;
#endif
	}
	
	class FullDimensionSpecifier : public Region::DimensionSpecifier
	{
		public:
			FullDimensionSpecifier(Expression dimension)
				: DimensionSpecifier(dimension)
			{
			}
			
			virtual ~FullDimensionSpecifier()
			{
			}
			
			Expression get_dimension_start()
			{
				Source source;
				source << "0";
				return Expression(
					source.parse_expression(_dimension.get_ast(), _dimension.get_scope_link())
					, _dimension.get_scope_link()
				);
			}
			Expression get_accessed_length()
			{
				return _dimension;
			}
			
			bool is_full()
			{
				return true;
			}
	};
	
	
	class PointDimensionSpecifier : public Region::DimensionSpecifier
	{
		protected:
			Expression _point;
			
		public:
			PointDimensionSpecifier(Expression dimension, Expression point)
				: DimensionSpecifier(dimension), _point(point)
			{
			}
			
			virtual ~PointDimensionSpecifier()
			{
			}
			
			Expression get_dimension_start()
			{
				return _point;
			}
			Expression get_accessed_length()
			{
				Source source;
				source << "1";
				return Expression(
					source.parse_expression(_dimension.get_ast(), _dimension.get_scope_link())
					, _dimension.get_scope_link()
				);
			}
	};
	
	
	class StartAndEndDimensionSpecifier : public Region::DimensionSpecifier
	{
		protected:
			Expression _start;
			Expression _end;
			
		public:
			StartAndEndDimensionSpecifier(Expression dimension, Expression start, Expression end)
				: DimensionSpecifier(dimension), _start(start), _end(end)
			{
			}
			
			virtual ~StartAndEndDimensionSpecifier()
			{
			}
			
			Expression get_dimension_start()
			{
				return _start;
			}
			Expression get_accessed_length()
			{
				Source source;
				source << "(" << _end.prettyprint() << ") - (" << _start.prettyprint() << ") + 1";
				return Expression(
					source.parse_expression(_dimension.get_ast(), _dimension.get_scope_link())
					, _dimension.get_scope_link()
				);
			}
	};
	
	
	class StartAndLengthDimensionSpecifier : public Region::DimensionSpecifier
	{
		protected:
			Expression _start;
			Expression _length;
			
		public:
			StartAndLengthDimensionSpecifier(Expression dimension, Expression start, Expression length)
				: DimensionSpecifier(dimension), _start(start), _length(length)
			{
			}
			
			virtual ~StartAndLengthDimensionSpecifier()
			{
			}
			
			Expression get_dimension_start()
			{
				return _start;
			}
			Expression get_accessed_length()
			{
				return _length;
			}
	};
	
	
	// Shamelessly copied from Source::parse_expression ( however we do not have any ongoing friendship with AST_t and ScopeLink ;) )
	bool Region::check_expression(AST expression_ast, AST_t ref_ast, ScopeLink scope_link)
	{
		CURRENT_CONFIGURATION->scope_link = scope_link.get_internal_scope_link();
		decl_context_t decl_context = scope_link_get_decl_context(scope_link.get_internal_scope_link(), ref_ast.get_internal_ast());
		enter_test_expression();
		char c = check_for_expression(expression_ast, decl_context);
		leave_test_expression();
		CURRENT_CONFIGURATION->scope_link = NULL;
		
		scope_link_set(scope_link.get_internal_scope_link(), expression_ast, decl_context);
		return (c != 0);
	}
	
	
	Region::Region(Direction direction, Reduction reduction, ObjectList<Expression> dimension_list, AST_t ast, AST_t ref_ast, ScopeLink scope_link)
		: _direction(direction), _reduction(reduction), _dimensions()
	{
		// If there are range specifiers
		if (ast != NULL)
		{
			// Copy the nodes to a list since we have to traverse them in reverse order
			// The region components start from the smallest strided one to the highest one
			std::list<AST> specifier_list;
			
			AST it;
			for_each_element(ast.get_internal_ast(), it)
			{
				AST region_specifier = ASTSon1(it);
				specifier_list.push_front(region_specifier); // Reverse order
			}
			
			unsigned int index = 0;
			for (std::list<AST>::iterator it = specifier_list.begin(); it != specifier_list.end(); it++)
			{
				AST region_specifier = *it;
				if (index == dimension_list.size())
				{
					throw MismatchedNumberOfRangeSpecifiers();
				}
				
				if (ASTType(region_specifier) == AST_SUPERSCALAR_REGION_SPEC_FULL)
				{
					_dimensions.push_back(
						new FullDimensionSpecifier(dimension_list[index])
					);
				}
				else if (ASTType(region_specifier) == AST_SUPERSCALAR_REGION_SPEC_SINGLE)
				{
					if (!check_expression(ASTSon0(region_specifier), ref_ast, scope_link))
					{
						std::cerr << AST_t(ASTSon0(region_specifier)).get_locus() << " Invalid expression in region specifier '" << AST_t(ASTSon0(region_specifier)).prettyprint() << "'." << std::endl;
						throw InvalidRegionException();
					}
					
					_dimensions.push_back(
						new PointDimensionSpecifier(
							dimension_list[index],
							Expression(AST_t(ASTSon0(region_specifier)), scope_link)
						)
					);
				}
				else if (ASTType(region_specifier) == AST_SUPERSCALAR_REGION_SPEC_RANGE)
				{
					if (!check_expression(ASTSon0(region_specifier), ref_ast, scope_link))
					{
						std::cerr << AST_t(ASTSon0(region_specifier)).get_locus() << " Invalid expression in region specifier '" << AST_t(ASTSon0(region_specifier)).prettyprint() << "'." << std::endl;
						throw InvalidRegionException();
					}
					
					if (!check_expression(ASTSon1(region_specifier), ref_ast, scope_link))
					{
						std::cerr << AST_t(ASTSon1(region_specifier)).get_locus() << " Invalid expression in region specifier '" << AST_t(ASTSon1(region_specifier)).prettyprint() << "'." << std::endl;
						throw InvalidRegionException();
					}
					
					_dimensions.push_back(
						new StartAndEndDimensionSpecifier(
							dimension_list[index],
							Expression(AST_t(ASTSon0(region_specifier)), scope_link),
							Expression(AST_t(ASTSon1(region_specifier)), scope_link)
						)
					);
				}
				else if (ASTType(region_specifier) == AST_SUPERSCALAR_REGION_SPEC_LENGTH)
				{
					if (!check_expression(ASTSon0(region_specifier), ref_ast, scope_link))
					{
						std::cerr << AST_t(ASTSon0(region_specifier)).get_locus() << " Invalid expression in region specifier '" << AST_t(ASTSon0(region_specifier)).prettyprint() << "'." << std::endl;
						throw InvalidRegionException();
					}
					
					if (!check_expression(ASTSon1(region_specifier), ref_ast, scope_link))
					{
						std::cerr << AST_t(ASTSon1(region_specifier)).get_locus() << " Invalid expression in region specifier '" << AST_t(ASTSon1(region_specifier)).prettyprint() << "'." << std::endl;
						throw InvalidRegionException();
					}
					
					_dimensions.push_back(
						new StartAndLengthDimensionSpecifier(
							dimension_list[index],
							Expression(AST_t(ASTSon0(region_specifier)), scope_link),
							Expression(AST_t(ASTSon1(region_specifier)), scope_link)
						)
					);
				}
				else
				{
					std::cerr << __FILE__ << ":" << __LINE__ << ": " << "Internal compiler error" << std::endl;
					throw FatalException();
				}
				
				index++;
			}
			
			if (index != dimension_list.size())
			{
				throw MismatchedNumberOfRangeSpecifiers();
			}
			
		} else {
			
			// No range specified -> full range
			for (unsigned int index=0; index != dimension_list.size(); index++)
			{
				_dimensions.push_back( new FullDimensionSpecifier(dimension_list[index]) );
			}
			
		}
	}
	
	
	bool Region::operator==(Region const &other) const
	{
		if (_dimensions.size() != other._dimensions.size())
		{
			return false;
		}
		
		if (_direction != other._direction)
		{
			return false;
		}
		
#if 0
		for (unsigned int i=0; i < _dimensions.size(); i++)
		{
			if ( !ParameterExpression::match(_dimensions[i]->get_dimension_start(), other._dimensions[i]->get_dimension_start()) )
			{
				return false;
			}
			if ( !ParameterExpression::match(_dimensions[i]->get_accessed_length(), other._dimensions[i]->get_accessed_length()) )
			{
				return false;
			}
			if ( !ParameterExpression::match(_dimensions[i]->get_dimension_length(), other._dimensions[i]->get_dimension_length()) )
			{
				return false;
			}
		}
#endif
		
		return true;
	}
	
	
	bool Region::is_full()
	{
		for (unsigned int i=0; i < _dimensions.size(); i++)
		{
			DimensionSpecifier *dimension = _dimensions[i];
			if (!dimension->is_full())
			{
				return false;
			}
		}
		
		return true;
	}
	
	
}

