/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
#include "tl-outline-info.hpp"
#include "tl-predicateutils.hpp"

namespace TL { namespace Nanox { 

    void LoweringVisitor::reduction_initialization_code(
            Source max_threads,
            OutlineInfo& outline_info,
            Nodecl::NodeclBase construct,
            // out
            Source &reduction_declaration,
            Source &register_code,
            Source &fill_outline_arguments,
            Source &fill_immediate_arguments)
    {
        TL::ObjectList<OutlineDataItem> reduction_items = outline_info.get_data_items().filter(
                predicate(&OutlineDataItem::is_reduction));

        if (!reduction_items.empty())
        {
            for (TL::ObjectList<OutlineDataItem>::iterator it = reduction_items.begin();
                    it != reduction_items.end();
                    it++)
            {
                std::string nanos_red_name = "nanos_red_" + it->get_symbol().get_name();

                OpenMP::UDRInfoItem *udr_info = it->get_reduction_info();
                ERROR_CONDITION(udr_info == NULL, "Invalid reduction info", 0);

                TL::Type reduction_type = it->get_symbol().get_type();
                if (reduction_type.is_any_reference())
                    reduction_type = reduction_type.references_to();

                reduction_declaration
                    << "nanos_reduction_t* " << nanos_red_name << ";"
                    ;

                register_code
                    << "err = nanos_malloc((void**)&" << nanos_red_name << ", sizeof(nanos_reduction_t), " 
                    << "\"" << construct.get_filename() << "\", " << construct.get_line() << ");"
                    << "if (err != NANOS_OK)"
                    <<     "nanos_handle_error(err);"
                    << nanos_red_name << "->original = (void*)&" << it->get_symbol().get_name() << ";"
                   << "err = nanos_malloc(&" << nanos_red_name << "->privates, sizeof(" << as_type(reduction_type) << ") * " << max_threads <<", "
                    << "\"" << construct.get_filename() << "\", " << construct.get_line() << ");"
                    << "if (err != NANOS_OK)"
                    <<     "nanos_handle_error(err);"
                    << nanos_red_name << "->bop = " << udr_info->get_basic_reductor_function().get_name() << ";"
                    << nanos_red_name << "->cleanup = " << udr_info->get_cleanup_function().get_name() << ";"
                    << "nanos_register_reduction(" << nanos_red_name << ");"
                    ;

                fill_outline_arguments
                    << "ol_args->" << it->get_field_name() << " = " << nanos_red_name << "->privates;"
                    ;
                fill_immediate_arguments
                    << "imm_args." << it->get_field_name() << " = " << nanos_red_name << "->privates;"
                    ;
            }
        }
    }

} }
