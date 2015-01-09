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

#include "tl-lowering-visitor.hpp"
#include "tl-lowering-utils.hpp"

namespace TL { namespace Intel {

Nodecl::NodeclBase LoweringVisitor::emit_barrier(const Nodecl::NodeclBase &construct)
{
    TL::Symbol ident_symbol = Intel::new_global_ident_symbol(construct);

    Source src;
    src << "__kmpc_barrier(&" << as_symbol(ident_symbol)
        << ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << "));";

    Nodecl::NodeclBase barrier_code = src.parse_statement(construct);
    return barrier_code;
}

void LoweringVisitor::visit(const Nodecl::OpenMP::BarrierFull& construct)
{
    construct.replace(emit_barrier(construct));
}


} }
