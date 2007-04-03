/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef TL_DTO_HPP
#define TL_DTO_HPP

#include <string>
#include <map>
#include "tl-object.hpp"
#include "tl-refptr.hpp"

namespace TL
{
    class DTO 
    {
        private:
            typedef std::map<std::string, RefPtr<Object> > DTO_inner;
            DTO_inner _dto;
        public :
            RefPtr<Object> operator[](const std::string& str)
            {
                DTO_inner::iterator it = _dto.find(str);
                if (it == _dto.end())
                {
                    return RefPtr<Undefined>(new Undefined);
                }
                else
                {
                    return it->second;
                }
            }

            void set_object(const std::string& str, RefPtr<Object> obj)
            {
                _dto[str] = obj;
            }
    };
}

#endif // TL_DTO_HPP
