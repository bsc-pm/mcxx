/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-builtin.hpp"

namespace TL
{
    LinkData::LinkData()
    {
        _data_list = new std::map<std::string, data_info>;
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
            for (std::map<std::string, data_info>::iterator it = _data_list->begin();
                    it != _data_list->end();
                    it ++)
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
