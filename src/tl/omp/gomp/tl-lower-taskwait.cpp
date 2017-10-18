
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


#include "tl-counters.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-lowering-utils.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-lower-reductions.hpp"


namespace TL { namespace GOMP {


    struct DependencesVisitor : Nodecl::ExhaustiveVisitor<void>
    {
        bool has_dependences;

        DependencesVisitor() : has_dependences(false) {}

        void visit(const Nodecl::OpenMP::DepIn& n)
        {
            has_dependences = true;
        }
        void visit(const Nodecl::OpenMP::DepOut& n)
        {
            has_dependences = true;
        }
        void visit(const Nodecl::OpenMP::DepInout& n)
        {
            has_dependences = true;
        }
        void visit(const Nodecl::OmpSs::DepCommutative& n)
        {
            error_printf_at(n.get_locus(),
                    "commutative dependences are not supported on the taskwait construct\n");
        }
        void visit(const Nodecl::OmpSs::DepConcurrent& n)
        {
            error_printf_at(n.get_locus(),
                    "concurrent dependences are not supported on the taskwait construct\n");
        }
    };
    void LoweringVisitor::visit(const Nodecl::OpenMP::Taskwait& construct)
    {
        Nodecl::NodeclBase environment = construct.get_environment();
        DependencesVisitor visitor;
        visitor.walk(environment);

        if (visitor.has_dependences)
        {
            warn_printf_at(construct.get_locus(),
                    "taskwait with dependences implemented as a taskwait construct without dependences\n");
        }
        Source src;
        src << "GOMP_taskwait();"
            ;

        construct.replace(src.parse_statement(construct));
    }

} }
