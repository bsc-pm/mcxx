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
#ifndef TL_COMPILER_PHASE_HPP
#define TL_COMPILER_PHASE_HPP

#include "tl-object.hpp"
#include "tl-dto.hpp"

namespace TL
{
    class CompilerPhase : Object
    {
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& str) const
            {
                return NULL;
            }
        public:
            virtual void run(DTO& data_flow) = 0;
            virtual ~CompilerPhase() { }
    };
}

#define EXPORT_PHASE(ClassName) \
extern "C"  \
{ \
    extern TL::CompilerPhase* give_compiler_phase_object(void) \
    { \
        return new ClassName(); \
    } \
}

#endif // TL_COMPILER_PHASE_HPP
