/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include "tl-nanos.hpp"
#include "tl-lowering-visitor.hpp"
#include "cxx-diagnostic.h"

namespace TL {  namespace Nanox {

void LoweringVisitor::visit(const Nodecl::OpenMP::Register& construct)
{
    if (!Nanos::Version::interface_is_at_least("copies_api", 1004))
    {
        error_printf("%s: error: '#pragma omp register' requires a newer Nanos++ library with 'copies_api' >= 1004\n",
                construct.get_locus_str().c_str());
        return;
    }

    OutlineInfo outline_info(*_lowering);

    OutlineInfoRegisterEntities outline_info_register(outline_info, construct.retrieve_context());
    outline_info_register.add_copies(
            construct.get_registered_set().as<Nodecl::List>(), OutlineDataItem::COPY_IN);

    int num_copies;
    int num_static_copies, num_dynamic_copies;
    count_copies(outline_info, num_static_copies, num_dynamic_copies);
    int num_copies_dimensions = count_copies_dimensions(outline_info);

    if (num_dynamic_copies != 0)
    {
        internal_error("Not yet implemented", 0);
    }
    else
    {
        num_copies = num_static_copies;
    }

    TL::Symbol structure_symbol;

    TL::Source copy_ol_decl,
           copy_ol_arg,
           copy_ol_setup,
           copy_imm_arg,
           copy_imm_setup;

    fill_copies_region(
            construct,
            outline_info,
            num_copies,
            num_copies_dimensions,
            // out
            copy_ol_decl,
            copy_ol_arg,
            copy_ol_setup,
            copy_imm_arg,
            copy_imm_setup);

    Source src;
    src 
        << "{"
        << copy_imm_setup
        << "nanos_err_t err;"
        << "err = nanos_register_object(" << num_copies << ", imm_copy_data);"
        << "if (err != NANOS_OK) nanos_handle_error(err);"
        << "}";

    Nodecl::NodeclBase new_stmt = src.parse_statement(construct);

    construct.replace(new_stmt);
}

} }
