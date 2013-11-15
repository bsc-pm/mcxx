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

namespace TL { namespace Nanox {

    LoweringVisitor::LoweringVisitor(Lowering* lowering,RefPtr<OpenMP::FunctionTaskSet> function_task_set)
        : _lowering(lowering), _function_task_set(function_task_set), _is_nanos_get_cublas_handle(false)
    {
        ERROR_CONDITION(_lowering == NULL, "Invalid lowering class\n", 0);
    }

    LoweringVisitor::~LoweringVisitor() { }

    // We redefine the visitor of Nodecl::FunctionCode because  we need to
    // visit first the internal functions and later the statements
    void LoweringVisitor::visit(const Nodecl::FunctionCode& function_code)
    {
        Nodecl::List stmts = function_code.get_statements().as<Nodecl::List>();

        // First, traverse nested functions
        for (Nodecl::List::iterator it = stmts.begin();
                it != stmts.end();
                it++)
        {
            if (it->is<Nodecl::FunctionCode>())
            {
                walk(*it);
            }
        }
        // Second, remaining statements
        for (Nodecl::List::iterator it = stmts.begin();
                it != stmts.end();
                it++)
        {
            if (!it->is<Nodecl::FunctionCode>())
            {
                walk(*it);
            }
        }
    }

    // We need to check if there is a function call to the 'nanos_get_cublas_handle'
    // function because we want to initialize CUBLAS automatically
    void LoweringVisitor::visit(const Nodecl::FunctionCall& function_call)
    {
        if (_is_nanos_get_cublas_handle)
            return;

        TL::Symbol called_symbol = function_call.get_called().get_symbol();
        if (called_symbol.is_valid()
                && called_symbol.get_name() == "nanos_get_cublas_handle")
        {
            _is_nanos_get_cublas_handle = true;

            Source src;
            src << "__attribute__((weak)) char gpu_cublas_init = 1;";

            if (IS_FORTRAN_LANGUAGE)
                Source::source_language = SourceLanguage::C;

            Nodecl::NodeclBase tree = src.parse_global(function_call);

            if (IS_FORTRAN_LANGUAGE)
                Source::source_language = SourceLanguage::Current;

            Nodecl::Utils::append_to_top_level_nodecl(tree);
        }
    }
} }
