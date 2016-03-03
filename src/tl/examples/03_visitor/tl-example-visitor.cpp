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

#include "tl-example-visitor.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {

    VisitorExamplePhase::VisitorExamplePhase()
    {
    }

    VisitorExamplePhase::~VisitorExamplePhase()
    {
    }

    class SimpleExhaustiveVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        public:

            virtual void visit_pre(const Nodecl::IfElseStatement &node)
            {
                std::cerr << "Entering the if-statement at " << node.get_locus_str() << std::endl;
            }

            virtual void visit_post(const Nodecl::IfElseStatement &node)
            {
                std::cerr << "Leaving the if-statement at " << node.get_locus_str() << std::endl;
            }
    };

    void VisitorExamplePhase::run(TL::DTO& dto)
    {
        Nodecl::NodeclBase top_level = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

        SimpleExhaustiveVisitor simple_exhaustive_visitor;
        simple_exhaustive_visitor.walk(top_level);
    }
}

EXPORT_PHASE(TL::VisitorExamplePhase);
