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
#include "tl-pragmasupport.hpp"
#include "tl-traverse.hpp"
#include "tl-langconstruct.hpp"
#include "tl-ast.hpp"

namespace TL
{
    class TraverseDecls : public PragmaCustomCompilerPhase
    {
        public:
            TraverseDecls()
            {
            }

            void pre_run(DTO& dto) { }

            void run(DTO& dto)
            {
                AST_t ast = dto["translation_unit"];
                ScopeLink scope_link = dto["scope_link"];

            }
    };
}

EXPORT_PHASE(TL::TraverseDecls);
