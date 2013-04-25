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



#include <tl-compilerphase.hpp>
#include <tl-compilerpipeline.hpp>
#include <fstream>

using namespace TL;

class MultifileTest : public CompilerPhase
{
    public:
        virtual void run(DTO& dto);
};

void MultifileTest::run(DTO& dto)
{
    std::cerr << "Running multifile test" << std::endl;

    const std::string hello_file = "hello_world.c";

    std::fstream hello_world("hello_world.c", std::ios_base::trunc | std::ios_base::out);

    if (!hello_world)
    {
        std::cerr << "Descriptor is bad. Bailing out" << std::endl;
        return;
    }

    hello_world 
        << "#include <stdio.h>\n"

        << "int main(int argc, char *argv[])\n"
        << "{\n"
        <<    "printf(\"Hello world in a world where pointers are of size %zd bytes\\n\", sizeof(void*));\n"
        <<    "return 0;\n"
        << "}"
        << std::endl
        ;
        
    hello_world.close();

    CompilationProcess::add_file(hello_file, "secondcc");
}

EXPORT_PHASE(MultifileTest);
