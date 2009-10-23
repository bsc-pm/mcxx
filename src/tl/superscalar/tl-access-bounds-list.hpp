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

#ifndef TL_ACCESS_BOUNDS_LIST_HPP
#define TL_ACCESS_BOUNDS_LIST_HPP

#include <cstddef>

#include "tl-object.hpp"

#include "tl-access-bounds.hpp"


namespace TL {
	class AccessBoundsList : public Object
	{
		private:
			ObjectList<AccessBounds> *_list;
			
		public:
			typedef ObjectList<AccessBounds>::iterator iterator;
			typedef ObjectList<AccessBounds>::const_iterator const_iterator;
			
			AccessBoundsList()
				: Object(), _list(NULL)
			{
			}
			
			AccessBoundsList(RefPtr<Object> object)
			{
				RefPtr<AccessBoundsList> access_bounds_list = RefPtr<AccessBoundsList>::cast_dynamic(object);
				if (access_bounds_list.get_pointer() != NULL)
				{
					_list = access_bounds_list->_list;
				}
				else
				{
					if (typeid(*object.get_pointer()) != typeid(Undefined))
					{
						std::cerr << "Bad initialization for AccessBoundsList" << std::endl;
					}
					_list = NULL;
				}
			}
			
			AccessBoundsList(AccessBoundsList const &other)
				: Object(other), _list(other._list)
			{
			}
			
			~AccessBoundsList()
			{
			}
			
			void initialize()
			{
				_list = new ObjectList<AccessBounds>();
			}
			
			ObjectList<AccessBounds>::const_iterator begin() const
			{
				return _list->begin();
			}
			
			ObjectList<AccessBounds>::iterator begin()
			{
				return _list->begin();
			}
			
			ObjectList<AccessBounds>::const_iterator end() const
			{
				return _list->end();
			}
			
			AccessBounds const &operator[](int index) const
			{
				return (*_list)[index];
			}
			
			AccessBounds &operator[](int index)
			{
				return (*_list)[index];
			}
			
			size_t size() const
			{
				return (*_list).size();
			}
			
			void add(AccessBounds const &region)
			{
				if (_list == NULL)
				{
					initialize();
				}
				(*_list).push_back(region);
			}
			
			bool operator==(AccessBoundsList const &other) const
			{
				if ((_list == NULL) != (other._list == NULL))
				{
					return false;
				}
				
				if (_list->size() != other._list->size())
				{
					return false;
				}
				
				for (unsigned int i=0; i < _list->size(); i++)
				{
					if ((*_list)[i] != (*other._list)[i])
					{
						return false;
					}
				}
				
				return true;
			}
			
			void reserve(size_t size)
			{
				_list->reserve(size);
				// Looks like reserve does not actually create default objects
				for (unsigned int i = 0 ; i < size; i++)
				{
					_list->push_back(AccessBounds());
				}
			}
			
	};
}


#endif // TL_ACCESS_BOUNDS_LIST_HPP
