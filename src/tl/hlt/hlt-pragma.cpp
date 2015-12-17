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

#include "tl-pragmasupport.hpp"

#include "hlt-pragma.hpp"
#include "hlt-loop-unroll.hpp"
#include "hlt-loop-normalize.hpp"
#include "cxx-cexpr.h"
#include "cxx-diagnostic.h"

namespace TL { namespace HLT {

HLTPragmaPhase::HLTPragmaPhase()
    : PragmaCustomCompilerPhase()
{
    set_phase_name("High Level Transformations");
    set_phase_description("This phase implements several high level "
            "transformations available through the usage of #pragma hlt");

    register_construct("hlt", "normalize");
    dispatcher("hlt").statement.post["normalize"].connect(
            std::bind(
                &HLTPragmaPhase::do_loop_normalize,
                this,
                std::placeholders::_1)
            );

    register_construct("hlt", "unroll");
    dispatcher("hlt").statement.post["unroll"].connect(
            std::bind(
                &HLTPragmaPhase::do_loop_unroll,
                this,
                std::placeholders::_1)
            );
}

void HLTPragmaPhase::run(TL::DTO& dto)
{
    PragmaCustomCompilerPhase::run(dto);
}

namespace {

    Nodecl::NodeclBase get_statement_from_pragma(
            const TL::PragmaCustomStatement& construct)
    {
        Nodecl::NodeclBase stmt = construct.get_statements();

        ERROR_CONDITION(!stmt.is<Nodecl::List>(), "Invalid tree", 0);
        stmt = stmt.as<Nodecl::List>().front();

        ERROR_CONDITION(!stmt.is<Nodecl::Context>(), "Invalid tree", 0);
        stmt = stmt.as<Nodecl::Context>().get_in_context();

        ERROR_CONDITION(!stmt.is<Nodecl::List>(), "Invalid tree", 0);
        stmt = stmt.as<Nodecl::List>().front();

        return stmt;
    }

}

void HLTPragmaPhase::do_loop_unroll(TL::PragmaCustomStatement construct)
{
    HLT::LoopUnroll loop_unroll;
    Nodecl::NodeclBase loop = get_statement_from_pragma(construct);
    loop_unroll.set_loop(loop);

    int unroll_factor = 0;

    TL::PragmaCustomLine custom_line = construct.get_pragma_line();
    TL::PragmaCustomParameter clause = custom_line.get_parameter();
    if (clause.is_defined())
    {
        TL::ObjectList<Nodecl::NodeclBase> expr_list = clause.get_arguments_as_expressions();
        if (!expr_list.empty())
        {
            Nodecl::NodeclBase first = expr_list[0];
            if (first.is_constant())
            {
                unroll_factor = const_value_cast_to_signed_int(first.get_constant());
                loop_unroll.set_unroll_factor(unroll_factor);
            }
        }
    }

    loop_unroll.unroll();
    info_printf_at(construct.get_locus(), "loop unrolled by a factor %d\n",
            unroll_factor);

    Nodecl::NodeclBase transformed_code = loop_unroll.get_whole_transformation();
    construct.replace(transformed_code);
}

void HLTPragmaPhase::do_loop_normalize(TL::PragmaCustomStatement construct)
{
    HLT::LoopNormalize loop_normalize;
    Nodecl::NodeclBase loop = get_statement_from_pragma(construct);
    loop_normalize.set_loop(loop);

    loop_normalize.normalize();
    info_printf_at(construct.get_locus(), "loop normalized\n");

    Nodecl::NodeclBase transformed_code = loop_normalize.get_whole_transformation();
    construct.replace(transformed_code);
}

} }

EXPORT_PHASE(TL::HLT::HLTPragmaPhase)
