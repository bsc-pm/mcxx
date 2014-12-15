/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-link-data.hpp"

namespace TL {
namespace Analysis {

    LinkData::LinkData()
    {
        _data_list = new Dict;
        _num_copies = new int(1);
    }

    LinkData::LinkData(const LinkData& l)
    {
        _data_list = l._data_list;
        _num_copies = l._num_copies;

        (*_num_copies)++;
    }

    void LinkData::release_code()
    {
        (*_num_copies)--;
        if (*_num_copies == 0)
        {
            for (Dict::iterator it = _data_list->begin();
                 it != _data_list->end(); it ++)
            {
                data_info d = it->second;
                d.destructor(d.data);
            }

            delete _num_copies;
            delete _data_list;
        }
    }

    LinkData::~LinkData()
    {
        release_code();
    }

    LinkData& LinkData::operator=(const LinkData& l)
    {
        if (&l != this)
        {
            release_code();

            _num_copies = l._num_copies;
            _data_list = l._data_list;

            (*_num_copies)++;
        }

        return *this;
    }
}
}
