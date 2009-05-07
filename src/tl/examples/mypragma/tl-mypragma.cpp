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
#include "tl-mypragma.hpp"
#include "tl-pragmasupport.hpp"
#include "tl-langconstruct.hpp"

#include <vector>
#include <stack>
#include <sstream>

#include <fstream>

namespace TL
{
    class MyPragmaPhase : public PragmaCustomCompilerPhase
    {
        public:
            MyPragmaPhase()
                : PragmaCustomCompilerPhase("mypragma")
            {
                register_construct("test");
                on_directive_post["test"].connect(functor(&MyPragmaPhase::construct_post, *this));
            }

            virtual void run(DTO& dto)
            {
                std::cerr << " --> RUNNING MYPRAGMA <-- " << std::endl;
                PragmaCustomCompilerPhase::run(dto);
            }

            void construct_post(PragmaCustomConstruct pragma_custom_construct)
            {
                std::cerr << " -> PRAGMA FOUND <- " << std::endl;
                CompilationProcess::add_file("myprova.c", "mytestcc");

                std::fstream of("myprova.c");
                of << "void f(void);" << std::endl;
                of.close();
            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
