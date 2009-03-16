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
#include "tl-basic_test.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"
#include "tl-source.hpp"
#include <iostream>

namespace TL
{
    BasicTestPhase::BasicTestPhase()
    {
        std::cerr << "Basic test phase created" << std::endl;
    }

    void BasicTestPhase::pre_run(TL::DTO& dto)
    {
        std::cerr << "Basic test phase pre_run" << std::endl;

        AST_t ast = dto["translation_unit"];
        ScopeLink sl = dto["scope_link"];

        Source src;

        src << "extern void f(int n);"
            ;

        src.parse_global(ast, sl);
    }

    void BasicTestPhase::run(TL::DTO& dto)
    {
        std::cerr << "Basic test phase run" << std::endl;
    }
}

EXPORT_PHASE(TL::BasicTestPhase);
