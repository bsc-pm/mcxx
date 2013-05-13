/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
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

            void construct_post(PragmaCustomConstruct construct)
            {
                std::cerr << " --> RUNNING CONSTRUCT POST <-- " << std::endl;

                std::cerr << "Getting enclosing function def" << std::endl;

                FunctionDefinition function_def = construct.get_enclosing_function();

                Statement fun_body = function_def.get_function_body();
                std::cerr << "BODY -->" << fun_body << "<--" << std::endl;

                std::cerr << "BODY is compound statement? " << fun_body.is_compound_statement() << std::endl;

            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
