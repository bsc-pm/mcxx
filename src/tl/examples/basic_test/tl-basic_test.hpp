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
#ifndef TL_BASIC_TEST_HPP
#define TL_BASIC_TEST_HPP

#include "tl-compilerphase.hpp"

namespace TL
{
    class BasicTestPhase : public TL::CompilerPhase
    {
        private:
        public:
            BasicTestPhase();
            virtual void run(TL::DTO& dto);
            virtual void pre_run(TL::DTO& dto);
    };
}

#endif // TL_BASIC_TEST_HPP
