/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "tl-omp-core.hpp"

namespace TL
{
    OmpCore::OmpCore()
        : PragmaCustomCompilerPhase("omp")
    {
    }

    void OmpCore::run(TL::DTO& dto)
    {
    }

    void OmpCore::pre_run(TL::DTO& dto)
    {
        register_omp_constructs();
    }

    void OmpCore::register_omp_constructs()
    {
#define CONSTRUCT_INFO(_name) \
        { \
            register_construct(#_name); \
            on_directive_pre[#_name].connect(functor(&OmpCore::_name##_handler, *this)); \
        }
#define DIRECTIVE_INFO(_name) \
        { \
            register_directive(#_name); \
            on_directive_pre[#_name].connect(functor(&OmpCore::_name##_handler, *this)); \
        }
#include "tl-omp-core.def"
#undef CONSTRUCT_INFO
#undef DIRECTIVE_INFO
    }
}
