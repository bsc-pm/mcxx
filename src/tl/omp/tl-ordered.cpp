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
#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::ordered_postorder(OpenMP::OrderedConstruct ordered_construct)
    {
        IdExpression induction_var = induction_var_stack.top();

        Statement construct_body = ordered_construct.body();
        Source ordered_source;

        ordered_source
            << "{"
            <<   "in__tone_enter_ordered_ (& "<< induction_var.prettyprint() << ");"
            <<   construct_body.prettyprint()
            <<   "in__tone_leave_ordered_ (&" << induction_var.prettyprint() << ");"
            << "}"
            ;

        AST_t ordered_code = ordered_source.parse_statement(ordered_construct.get_ast(),
                ordered_construct.get_scope_link());

        ordered_construct.get_ast().replace(ordered_code);
    }
}

