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
#include "tl-counters.hpp"
#include "cxx-cexpr.h"

namespace TL { namespace Nanox {

    namespace {
        Nodecl::NodeclBase lower_sections_into_switch(
                const Nodecl::OpenMP::Sections sections)
        {
            Nodecl::List execution_environment = sections.get_environment().shallow_copy().as<Nodecl::List>();
            Nodecl::List section_list = sections.get_sections().as<Nodecl::List>();

            Counter& counter = CounterManager::get_counter("nanox_sections_to_for");

            std::stringstream symbol_name;
            symbol_name << "nanos_omp_index_" << (int)counter;
            counter++;

            Scope sc = sections.retrieve_context();
            // Create index symbol
            TL::Symbol index_symbol = sc.new_symbol(symbol_name.str());
            scope_entry_t* index_sym = index_symbol.get_internal_symbol();
            index_sym->kind = SK_VARIABLE;
            index_sym->type_information = get_signed_int_type();
            index_sym->file = uniquestr(sections.get_filename().c_str());
            index_sym->line = sections.get_line();

            // Make this symbol private
            execution_environment.push_back(
                    Nodecl::OpenMP::Private::make(
                        Nodecl::List::make(
                            Nodecl::Symbol::make(index_symbol,
                                sections.get_filename(),
                                sections.get_line())),
                        sections.get_filename(),
                        sections.get_line())
                    );

            execution_environment.push_back(
                    Nodecl::OpenMP::Schedule::make(
                        Nodecl::IntegerLiteral::make(::get_signed_int_type(),
                            const_value_get_signed_int(1),
                            sections.get_filename(),
                            sections.get_line()),
                        "static",
                        sections.get_filename(),
                        sections.get_line())
                    );


            Nodecl::List switch_statements;

            int index = 0;
            for (Nodecl::List::iterator it = section_list.begin();
                    it != section_list.end();
                    it++, index++)
            {
                ERROR_CONDITION(!it->is<Nodecl::OpenMP::Section>(), "Invalid tree", 0);
                Nodecl::OpenMP::Section section = it->as<Nodecl::OpenMP::Section>();

                Nodecl::List current_case_statements = section.get_statements().shallow_copy().as<Nodecl::List>();
                if (!IS_FORTRAN_LANGUAGE)
                {
                    // C/C++ needs a break here
                    current_case_statements.push_back(
                            Nodecl::BreakStatement::make(Nodecl::NodeclBase::null(),
                                section.get_filename(), section.get_line()));
                }

                Nodecl::NodeclBase current_case =
                    Nodecl::CaseStatement::make(
                            Nodecl::List::make(
                                Nodecl::IntegerLiteral::make(::get_signed_int_type(),
                                    const_value_get_signed_int(index),
                                    section.get_filename(),
                                    section.get_line())),
                            current_case_statements,
                            section.get_filename(),
                            section.get_line()
                            );

                switch_statements.push_back(current_case);
            }

            Nodecl::NodeclBase switch_body = switch_statements;

            if (!IS_FORTRAN_LANGUAGE)
            {
                switch_body = Nodecl::CompoundStatement::make(
                        switch_body, // This is a list here
                        Nodecl::NodeclBase::null(), // No finalizers,
                        sections.get_filename(),
                        sections.get_line());
                switch_body = Nodecl::List::make(switch_body);
            }

            Nodecl::NodeclBase index_reference = Nodecl::Symbol::make(index_symbol, sections.get_filename(), sections.get_line());
            index_reference.set_type(::lvalue_ref(index_sym->type_information));

            Nodecl::NodeclBase switch_statement =
                Nodecl::SwitchStatement::make(
                        index_reference,
                        switch_body);

            Nodecl::NodeclBase range = Nodecl::OpenMP::ForRange::make(
                    Nodecl::IntegerLiteral::make(::get_signed_int_type(), ::const_value_get_signed_int(0)),
                    Nodecl::IntegerLiteral::make(::get_signed_int_type(), ::const_value_get_signed_int(index - 1)),
                    Nodecl::IntegerLiteral::make(::get_signed_int_type(), ::const_value_get_signed_int(1)),
                    index_symbol,
                    sections.get_filename(),
                    sections.get_line());

            Nodecl::OpenMP::For for_construct =
                Nodecl::OpenMP::For::make(
                        execution_environment,
                        Nodecl::List::make(range),
                        Nodecl::List::make(switch_statement),
                        sections.get_filename(),
                        sections.get_line());

            return for_construct;
        }
    }

    void LoweringVisitor::visit(const Nodecl::OpenMP::Sections& construct)
    {
        // We do not handle sections as such, we lower them as a for + switch
        Nodecl::NodeclBase omp_for_construct = lower_sections_into_switch(construct);

        construct.replace(omp_for_construct);

        walk(construct);
    }

} }
