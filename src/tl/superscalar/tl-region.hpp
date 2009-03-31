/*
    SMP superscalar Compiler
    Copyright (C) 2008 Barcelona Supercomputing Center

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

#ifndef TL_REGION_HPP
#define TL_REGION_HPP

#include "tl-langconstruct.hpp"
#include "tl-object.hpp"
#include "tl-scopelink.hpp"


namespace TL {
	class Region : public Object
	{
		public:
			
			enum Direction {
				UNKNOWN_DIR=0,
				INPUT_DIR=1,
				OUTPUT_DIR=2,
				INOUT_DIR=3
			};
			
			enum Reduction {
				UNKNOWN_RED=0,
				NON_REDUCTION=1,
				REDUCTION=2
			};
			
			
			class DimensionSpecifier
			{
				protected:
					Expression _dimension;
					
				public:
					DimensionSpecifier(Expression dimension)
						: _dimension(dimension)
					{
					}
					virtual ~DimensionSpecifier()
					{
					}
					
					virtual Expression get_dimension_start()=0;
					virtual Expression get_accessed_length()=0;
					Expression get_dimension_length()
					{
						return _dimension;
					}
					
					virtual bool is_full();
			};
			
			typedef ObjectList<DimensionSpecifier *>::iterator iterator;
			typedef ObjectList<DimensionSpecifier *>::const_iterator const_iterator;
			
			
		private:
			Direction _direction;
			Reduction _reduction;
			ObjectList<DimensionSpecifier *> _dimensions;
			
			bool check_expression(AST expression_ast, AST_t ref_ast, ScopeLink scope_link);
			
		public:
			Region()
				: _direction(UNKNOWN_DIR), _dimensions()
			{
			}
			
			Region(RefPtr<Object> object)
			{
				RefPtr<Region> region = RefPtr<Region>::cast_dynamic(object);
				if (region.get_pointer() != NULL)
				{
					_direction = region->_direction;
					_reduction = region->_reduction;
					_dimensions = region->_dimensions;
				}
				else
				{
					if (typeid(*object.get_pointer()) != typeid(Undefined))
					{
						std::cerr << "Bad initialization for Region" << std::endl;
					}
					_direction = UNKNOWN_DIR;
					_reduction = UNKNOWN_RED;
					_dimensions.clear();
				}
			}
			
			Region(Direction direction, Reduction reduction, ObjectList<Expression> dimension_list, AST_t ast, AST_t ref_ast, ScopeLink scope_link);
			
			// The default descructor destroys the contents of _dimensions
			
			Direction get_direction() const
			{
				return _direction;
			}
			
			Reduction get_reduction() const
			{
				return _reduction;
			}
			const_iterator begin() const
			{
				return _dimensions.begin();
			}
			iterator begin()
			{
				return _dimensions.begin();
			}
			
			iterator end()
			{
				return _dimensions.end();
			}
			
			size_t get_dimension_count() const
			{
				return _dimensions.size();
			}
			
			DimensionSpecifier const &operator[](int index) const
			{
				return *_dimensions[index];
			}
			
			DimensionSpecifier &operator[](int index)
			{
				return *_dimensions[index];
			}
			
			bool operator==(Region const &other) const;
			bool is_full();
	};
}


#endif // TL_REGION_HPP
