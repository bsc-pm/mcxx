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

#ifndef CODEGEN_PRUNE_HPP
#define CODEGEN_PRUNE_HPP

#include "tl-nodecl-visitor.hpp"

#include <set>

namespace Codegen
{
    class PruneVLAVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            std::set<TL::Symbol> _used_symbols;
            std::set<TL::Type> _visited_types;
            TL::ObjectList<Nodecl::NodeclBase> _saved_expressions;
        public:

            virtual void visit(const Nodecl::Symbol& sym);
            virtual void visit(const Nodecl::ObjectInit& n);
            virtual void visit(const Nodecl::Conversion& n);
            virtual void visit(const Nodecl::FunctionCode& function_code);

            void walk_type(TL::Type t);
    };
}

#endif // CODEGEN_PRUNE_HPP
