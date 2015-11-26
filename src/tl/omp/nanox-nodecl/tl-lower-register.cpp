/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

void LoweringVisitor::visit(const Nodecl::OmpSs::Register& construct)
{
    if (!Nanos::Version::interface_is_at_least("copies_api", 1004))
    {
        error_printf_at(
                construct.get_locus(),
                "'#pragma omp register' requires a newer Nanos++ library with 'copies_api' >= 1004\n");
        return;
    }

    OutlineInfo outline_info(*_lowering);

    OutlineInfoRegisterEntities outline_info_register(outline_info, construct.retrieve_context());
    outline_info_register.add_copies(
            construct.get_registered_set().as<Nodecl::List>(), OutlineDataItem::COPY_IN);

    Source num_copies;
    int num_static_copies, num_dynamic_copies;
    count_copies(outline_info, num_static_copies, num_dynamic_copies);
    Nodecl::NodeclBase num_copies_dimensions = count_copies_dimensions(outline_info);

    if (num_dynamic_copies != 0)
    {
        if (num_static_copies != 0)
        {
            num_copies << num_static_copies << "+";
        }

        num_copies << as_expression(count_dynamic_copies(outline_info));
    }
    else
    {
        num_copies << num_static_copies;
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
            num_static_copies,
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
        <<     copy_imm_setup
        <<     "nanos_err_t nanos_err;"
        <<     "nanos_err = nanos_register_object(" << num_copies << ", imm_copy_data);"
        <<     "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
        << "}";

    FORTRAN_LANGUAGE()
    {
        // Parse in C
        Source::source_language = SourceLanguage::C;
    }

    Nodecl::NodeclBase new_stmt = src.parse_statement(construct);

    FORTRAN_LANGUAGE()
    {
        Source::source_language = SourceLanguage::Current;
    }

    construct.replace(new_stmt);
}


void LoweringVisitor::visit(const Nodecl::OmpSs::Unregister& construct)
{
    if (!Nanos::Version::interface_is_at_least("copies_api", 1005))
    {
        error_printf_at(
                construct.get_locus(),
                "'#pragma omp unregister' requires a newer Nanos++ library with 'copies_api' >= 1005\n");
        return;
    }

   Nodecl::List list_expr = construct.get_unregistered_set().as<Nodecl::List>();

   std::string array_name = "nanos_base_addresses";

   int i = 0;
   TL::Source initialize_array;
   for (Nodecl::List::iterator it = list_expr.begin();
         it != list_expr.end();
         it++, i++)
   {
      if (!it->is<Nodecl::Symbol>())
      {
         error_printf_at(
               construct.get_locus(),
               "Invalid expression in '#pragma omp unregister' directive\n");
      }
      TL::DataReference data_ref(*it);
      Nodecl::NodeclBase address_of_object = data_ref.get_address_of_symbol();

      initialize_array
         << array_name << "["  << i << "] = "
         <<       as_expression(address_of_object) << ";"
         ;
   }

    Source src;
    src
        << "{"
        <<     "nanos_err_t nanos_err;"
        <<     "void* " << array_name << "[" << list_expr.size() << "];"
        <<     initialize_array
        <<     "nanos_err = nanos_unregister_object(" << list_expr.size() << ", " << array_name << ");"
        <<     "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
        << "}";

    FORTRAN_LANGUAGE()
    {
        // Parse in C
        Source::source_language = SourceLanguage::C;
    }

    Nodecl::NodeclBase new_stmt = src.parse_statement(construct);

    FORTRAN_LANGUAGE()
    {
        Source::source_language = SourceLanguage::Current;
    }
    construct.replace(new_stmt);
}
} }
