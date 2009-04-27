/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - David Rodenas Pico
    Copyright (C) 2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef ACOTES_OUTPUT_HPP
#define ACOTES_OUTPUT_HPP

#include "tl-pragmasupport.hpp"
#include "tl-langconstruct.hpp"
#include "tl-symbol.hpp"
#include "tl-source.hpp"

#include "acotes-outputtasks.hpp"

namespace TL
{
    class AcotesOutputPhase : public CompilerPhase
    {
        private:
            RefPtr<OutputTasks> _output_tasks;
        public:
            virtual void run(DTO& dto);
            virtual void pre_run(DTO& dto);
    };
}

#endif // ACOTES_OUTPUT_HPP
