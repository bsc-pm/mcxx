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

    Nodecl::NodeclBase LoweringVisitor::emit_critical_region(
            const std::string lock_name,
            Nodecl::NodeclBase construct,
            Nodecl::NodeclBase statements)
    {
        TL::Symbol nanos_lock_t_name = ReferenceScope(construct).get_scope().get_symbol_from_name("nanos_lock_t");
        ERROR_CONDITION(!nanos_lock_t_name.is_valid(), "nanos_lock_t required but not found in the scope", 0);

        if (_lock_names.find(lock_name) == _lock_names.end())
        {
            _lock_names.insert(lock_name);
            // We need to sign in the global lock
            Source global_lock_decl, initialization_if_needed;

            // Nanos locks are initialized to zero, so we can use common here
            global_lock_decl
                << "__attribute__((common)) "
                << as_type(nanos_lock_t_name.get_user_defined_type())
                << " " << lock_name << ";";

            FORTRAN_LANGUAGE()
            {
                // Parse in C
                Source::source_language = SourceLanguage::C;
            }

            Nodecl::NodeclBase global_lock_tree = global_lock_decl.parse_global(construct);

            FORTRAN_LANGUAGE()
            {
                Source::source_language = SourceLanguage::Current;
            }

            if (IS_CXX_LANGUAGE)
            {
                Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, global_lock_tree);
            }
        }
        if (IS_FORTRAN_LANGUAGE)
        {
            // Make this TYPE a SEQUENCE one, otherwise we cannot put it in the COMMON
            class_type_set_is_packed(nanos_lock_t_name.get_type().get_internal_type(), 1);
        }

        Nodecl::NodeclBase stmt_placeholder;
        Source critical_postorder_src;
        critical_postorder_src
            << "{"
            <<    "nanos_err_t nanos_err;"
            <<    "nanos_err = nanos_set_lock(&" << lock_name << ");"
            <<    "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
            <<    statement_placeholder(stmt_placeholder)
            <<    "nanos_err = nanos_unset_lock(&" << lock_name << ");"
            <<    "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
            << "}"
            ;

        Nodecl::NodeclBase critical_code;
        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        critical_code = critical_postorder_src.parse_statement(construct);
        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::Current;
        }

        stmt_placeholder.replace(statements.shallow_copy());

        return critical_code;
    }

    void LoweringVisitor::visit(const Nodecl::OpenMP::Critical& construct)
    {
        Nodecl::NodeclBase environment = construct.get_environment();
        Nodecl::NodeclBase statements = construct.get_statements();

        walk(statements);

        // Get the new statements
        statements = construct.get_statements();

        std::string lock_name = "nanos_default_critical_lock";
        if (!environment.is_null())
        {
            Nodecl::NodeclBase critical_name_node = environment.as<Nodecl::List>().find_first<Nodecl::OpenMP::CriticalName>();
            if (!critical_name_node.is_null())
            {
                lock_name = "nanos_critical_lock_" + critical_name_node.get_text();
            }
        }

        Nodecl::NodeclBase critical_code = emit_critical_region(lock_name, construct, statements);

        construct.replace(critical_code);
    }

} }
