/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
#include "tl-omp-nanox.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::sections_preorder(PragmaCustomConstruct ctr)
{
    // New section list
    _section_info.push_back(SectionInfoList());
}

void OMPTransform::sections_postorder(PragmaCustomConstruct ctr)
{
    // Remove section list info
    SectionInfoList& section_list(_section_info.back());

    Statement compound_statement = ctr.get_statement();
    ERROR_CONDITION(!compound_statement.is_compound_statement(), "This must be a compound statement", 0);

    ObjectList<Statement> inner_statements = compound_statement.get_inner_statements();

    if (inner_statements.empty())
    {
        // Give up
        std::cerr << "%s: warning: skipping empty sections" << ctr.get_ast().get_locus() << std::endl;
        ctr.get_ast().remove_in_list();
        return;
    }

    Source final_barrier;
    if (!ctr.get_clause("nowait").is_defined())
    {
        final_barrier << get_barrier_code(ctr.get_ast())
            ;
    }

    Source wd_declaration_src;
    wd_declaration_src
        << "nanos_wd_t _wd_section_list[" << section_list.size() << "] = { 0 };"
        ;

    Statement& first = inner_statements.front();
    Statement& last = inner_statements.back();

    AST_t wd_declaration_tree = wd_declaration_src.parse_statement(first.get_ast(), ctr.get_scope_link());
    first.prepend(Statement(wd_declaration_tree, ctr.get_scope_link()));

    int i = 0;
    for (SectionInfoList::iterator it = section_list.begin();
            it != section_list.end();
            it++, i++)
    {
        SectionInfo& section_info(*it);

        Source src;
        src
            << "_wd_section_list[" << i << "] = wd;"
            ;

        AST_t tree = src.parse_statement(section_info.placeholder, ctr.get_scope_link());
        section_info.placeholder.replace(tree);
    }

    Source alignment, slicer_alignment;
    if (Nanos::Version::interface_is_at_least("master", 5004))
    {
        alignment <<  "__alignof__(nanos_compound_wd_data_t),"
            ;
        slicer_alignment << "1,";
    }

    Source compound_wd_src;
    compound_wd_src
        << "{"
        <<    "nanos_wd_props_t props;"
        <<    "__builtin_memset(&props, 0, sizeof(props));"
        <<    "props.mandatory_creation = 1;"

        <<    "nanos_err_t err;"
        <<    "nanos_slicer_t compound_slicer = nanos_find_slicer(\"compound_wd\");"

        <<    "void* compound_dev = (void*)0;"
        // FIXME: What does this mean?
        <<    "nanos_slicer_get_specific_data(compound_slicer, &compound_dev);"

        // FIXME: What about devices?
        // FIXME: Can this be reused for other devices? 
        // FIXME: Looks like we will have to change something in DeviceProvider...
        <<    "nanos_smp_args_t compound_devices_smp_args = { (void(*)(void*))compound_dev };"
        <<    "nanos_device_t compound_device[1] = { { nanos_smp_factory, nanos_smp_dd_size, &compound_devices_smp_args } };"

        <<    "nanos_compound_wd_data_t *list_of_wds = (nanos_compound_wd_data_t*)0;"
        <<    "void *dummy = (void*)0;"

        <<    "nanos_wd_t cwd = (nanos_wd_t)0;"
        <<    "err = nanos_create_sliced_wd(&cwd, "
        // FIXME - Devices is hardcoded to SMP!
        <<            "1, compound_device,"
        <<            "sizeof(nanos_compound_wd_data_t) + (" << section_list.size() << ") * sizeof(nanos_wd_t),"
        <<            alignment
        <<            "(void**)&list_of_wds,"
        <<            "nanos_current_wd(),"
        <<            "compound_slicer,"
        // No data for this WD
        <<            /* sizeof */ "0,"
        <<            slicer_alignment
        <<            "&dummy,"
        <<            "&props,"
        // No copies either
        <<            "0, (nanos_copy_data_t**)0);"
        <<    "if (err != NANOS_OK) nanos_handle_error(err);"
        // Fill slicer data
        <<    "list_of_wds->nsect = " << section_list.size() << ";"
        <<    "__builtin_memcpy(list_of_wds->lwd, _wd_section_list, sizeof(_wd_section_list));"
        <<    "err = nanos_submit(cwd, 0, (nanos_dependence_t*)0, 0);"
        <<    "if (err != NANOS_OK) nanos_handle_error(err);"
        <<    final_barrier
        << "}"
        ;

    AST_t compound_wd_tree = compound_wd_src.parse_statement(last.get_ast(), ctr.get_scope_link());

    last.append(Statement(compound_wd_tree, ctr.get_scope_link()));

    // Remove the pragma
    ctr.get_ast().replace(compound_statement.get_ast());

    _section_info.pop_back();
}
