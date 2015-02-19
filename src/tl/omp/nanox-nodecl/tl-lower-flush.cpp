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


#include "tl-lowering-visitor.hpp"

namespace TL { namespace Nanox {


    void LoweringVisitor::visit(const Nodecl::OpenMP::FlushMemory& construct)
    {
        Source flush_source;

        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            flush_source
                << "{"
                << "__sync_synchronize();"
                << "}"
                ;
        }
        else
        {
            flush_source
                << "{"
                << "nanos_err_t nanos_err;"
                << "nanos_err = nanos_memory_fence();"
                << "if (nanos_err != NANOS_OK)"
                <<    "nanos_handle_error(nanos_err);"
                << "}"
                ;
        }

        Nodecl::NodeclBase flush_code;
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::C;
        }
        flush_code = flush_source.parse_statement(construct);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }

        construct.replace(flush_code);
    }

} }
