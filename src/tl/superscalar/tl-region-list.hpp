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


#ifndef TL_REGION_LIST_HPP
#define TL_REGION_LIST_HPP

#include <cstddef>

#include "tl-object.hpp"

#include "tl-region.hpp"


namespace TL {
	class RegionList : public Object
	{
		private:
			ObjectList<Region> *_list;
			
		public:
			typedef ObjectList<Region>::iterator iterator;
			typedef ObjectList<Region>::const_iterator const_iterator;
			
			RegionList()
				: _list(NULL)
			{
			}
			
			RegionList(RefPtr<Object> object)
			{
				RefPtr<RegionList> region_list = RefPtr<RegionList>::cast_dynamic(object);
				if (region_list.get_pointer() != NULL)
				{
					_list = region_list->_list;
				}
				else
				{
					if (typeid(*object.get_pointer()) != typeid(Undefined))
					{
						std::cerr << "Bad initialization for RegionList" << std::endl;
					}
					_list = NULL;
				}
			}
			
			void initialize()
			{
				_list = new ObjectList<Region>();
			}
			
			ObjectList<Region>::const_iterator begin() const
			{
				return _list->begin();
			}
			
			ObjectList<Region>::iterator begin()
			{
				return _list->begin();
			}
			
			ObjectList<Region>::const_iterator end() const
			{
				return _list->end();
			}
			
			Region const &operator[](int index) const
			{
				return (*_list)[index];
			}
			
			Region &operator[](int index)
			{
				return (*_list)[index];
			}
			
			size_t size() const
			{
				return (*_list).size();
			}
			
			bool add(Region const &region)
			{
				if (_list == NULL)
				{
					initialize();
				}
				
#if 0
				for (ObjectList<Region>::iterator it = _list->begin(); it != _list->end(); it++)
				{
					if (*it == region)
					{
						return false;
					}
				}
#endif
				
				(*_list).push_back(region);
				
				return true;
			}
			
			// Unfortunately this is quadratic due to order
			bool operator==(RegionList const &other) const
			{
				if ((_list == NULL) != (other._list == NULL))
				{
					return false;
				}
				
				if (_list->size() != other._list->size())
				{
					return false;
				}
				
#if 0
				for (unsigned int i=0; i < _list->size(); i++)
				{
					bool do_match = false;
					for (unsigned int j = 0; j < other._list->size() && !do_match; j++)
					{
						do_match = ((*_list)[i] == (*other._list)[j]);
					}
					if (!do_match)
					{
						return false;
					}
				}
#endif
				
				return true;
			}
			
			bool operator!=(RegionList const &other) const
			{
				return !((*this) == other);
			}
			
	};
}


#endif // TL_REGION_LIST_HPP
