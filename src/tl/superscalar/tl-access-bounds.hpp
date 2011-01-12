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




#ifndef TL_ACCESS_BOUNDS_HPP
#define TL_ACCESS_BOUNDS_HPP

#include <cstddef>

#include "tl-object.hpp"


namespace TL {
	class AccessBounds : public Object
	{
		private:
			ObjectList<Expression> *_list;
			
		public:
			typedef ObjectList<Expression>::iterator iterator;
			typedef ObjectList<Expression>::const_iterator const_iterator;
			
			AccessBounds()
				: Object(), _list(NULL)
			{
			}
			
			AccessBounds(RefPtr<Object> object)
			{
				RefPtr<AccessBounds> access_bounds = RefPtr<AccessBounds>::cast_dynamic(object);
				if (access_bounds.get_pointer() != NULL)
				{
					_list = access_bounds->_list;
				}
				else
				{
					if (typeid(*object.get_pointer()) != typeid(Undefined))
					{
						std::cerr << "Bad initialization for AccessBounds" << std::endl;
					}
					_list = NULL;
				}
			}
			
			AccessBounds(AccessBounds const &other)
				: Object(other), _list(other._list)
			{
			}
			
			~AccessBounds()
			{
			}
			
			void initialize()
			{
				_list = new ObjectList<Expression>();
			}
			
			ObjectList<Expression>::const_iterator begin() const
			{
				return _list->begin();
			}
			
			ObjectList<Expression>::iterator begin()
			{
				return _list->begin();
			}
			
			ObjectList<Expression>::const_iterator end() const
			{
				return _list->end();
			}
			
			Expression const &operator[](int index) const
			{
				return (*_list)[index];
			}
			
			Expression &operator[](int index)
			{
				return (*_list)[index];
			}
			
			size_t size() const
			{
				return (*_list).size();
			}
			
			void add(Expression const &bound)
			{
				if (_list == NULL)
				{
					initialize();
				}
				(*_list).push_back(bound);
			}
			
			bool operator==(AccessBounds const &other) const
			{
				if ((_list == NULL) != (other._list == NULL))
				{
					return false;
				}
				
				if (_list == NULL && other._list == NULL)
				{
					return true;
				}
				
				if (_list->size() != other._list->size())
				{
					return false;
				}
				
				for (unsigned int i=0; i < _list->size(); i++)
				{
					if ((*_list)[i].prettyprint() != (*other._list)[i].prettyprint())
					{
						return false;
					}
				}
				
				return true;
			}
			
			bool operator!=(AccessBounds const &other) const
			{
				return !(*this == other);
			}
			
	};
}


#endif // TL_ACCESS_BOUNDS_HPP
