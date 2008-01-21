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

            ParameterInfo(const std::string& _parameter_name, 
                    const std::string& _argument_name, 
                    Symbol _symbol, 
                    Type _type,
                    parameter_kind_t _kind)
                : parameter_name(_parameter_name), 
                argument_name(_argument_name), 
                symbol(_symbol),
                type(_type), 
                kind(_kind)
            {
            }
    };
}

#endif // TL_PARAMETERINFO_HPP
