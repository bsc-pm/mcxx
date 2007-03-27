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
#ifndef TL_OBJECT_HPP
#define TL_OBJECT_HPP

#include <iostream>
#include <string>
#include <typeinfo>
#include "cxx-tltype.h"
#include "extstruct.h"

namespace TL
{
    class Object 
    { 
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& name) const
            {
                return NULL;
            }

        public:
            /* do not override */
            Object& get_attribute(const std::string& name) const;

            virtual ~Object() { }

            bool has_attribute(const std::string& name) const;

            virtual bool is_bool() const
            {
                return false;
            }

            virtual bool is_integer() const
            {
                return false;
            }

            virtual bool is_ast() const
            {
                return false;
            }

            virtual bool is_symbol() const
            {
                return false;
            }

            virtual bool is_type() const
            {
                return false;
            }

            virtual bool is_scope() const
            {
                return false;
            }

            virtual bool is_string() const
            {
                return false;
            }

            virtual bool is_source() const
            {
                return false;
            }
    };

    class Undefined : public Object
    {
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& name) const
            {
                return NULL;
            }
        public :
            virtual ~Undefined() { }
    };
}

#endif // TL_OBJECT_HPP
