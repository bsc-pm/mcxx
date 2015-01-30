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



#include "tl-source.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-lowering-utils.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL { namespace Intel {

    void LoweringVisitor::visit(const Nodecl::OpenMP::Master& construct)
    {
        Nodecl::NodeclBase statements = construct.get_statements();

        walk(statements);

        statements = construct.get_statements();
        
        TL::Symbol ident_symbol = Intel::new_global_ident_symbol(construct);

        Nodecl::NodeclBase placeholder;
        Source transform_code;
        transform_code
            << "if (__kmpc_global_thread_num(&" << as_symbol(ident_symbol)
            << ") == 0)"
            << "{"
            << statement_placeholder(placeholder)
            << "}"
            ;

        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase n = transform_code.parse_statement(construct);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }

        Nodecl::NodeclBase copied_statements = Nodecl::Utils::deep_copy(statements, placeholder);
        placeholder.replace(copied_statements);

        construct.replace(n);
    }
} }

