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

#ifndef TL_PARAMETERINFO_HPP
#define TL_PARAMETERINFO_HPP

#include "tl-type.hpp"
#include "tl-langconstruct.hpp"
#include "tl-symbol.hpp"

namespace TL
{
    class ParameterInfo
    {
        public:
            typedef enum 
            {
                UNKNOWN = 0,
                BY_VALUE,
                BY_POINTER,
                BY_REFERENCE // Unused
            } parameter_kind_t;

            std::string parameter_name;
            std::string argument_name;
            Symbol symbol;
            Type type;
            parameter_kind_t kind;

            int parameter_position;

            // VLA support
            // {
            bool is_variably_modified;
            Type full_type_in_outline;
            // Like full_type_in_outline but first array has been removed
            Type type_in_outline;
            std::string vla_cast_name;
            // }

            ParameterInfo(const std::string& _parameter_name, 
                    const std::string& _argument_name, 
                    Symbol _symbol, 
                    Type _type,
                    parameter_kind_t _kind)
                : parameter_name(_parameter_name), 
                argument_name(_argument_name), 
                symbol(_symbol),
                type(_type), 
                kind(_kind),
                is_variably_modified(false),
                full_type_in_outline(NULL),
                type_in_outline(NULL)
            {
            }
    };
}

#endif // TL_PARAMETERINFO_HPP
