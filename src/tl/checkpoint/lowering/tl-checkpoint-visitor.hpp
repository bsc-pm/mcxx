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

#ifndef TL_CHECKPOINT_VISITOR_HPP
#define TL_CHECKPOINT_VISITOR_HPP

#include "tl-checkpoint-lowering.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL { namespace Checkpoint {

    struct CheckpointEnvironment : public Nodecl::ExhaustiveVisitor<void>
    {
        Nodecl::NodeclBase id;
        Nodecl::NodeclBase level;
        Nodecl::NodeclBase communicator;
        Nodecl::NodeclBase if_expr;
        Nodecl::NodeclBase kind;

        TL::ObjectList<Nodecl::NodeclBase> data_exprs;

        CheckpointEnvironment(Nodecl::NodeclBase env)
        {
            walk(env);
        }

        void visit(const Nodecl::Checkpoint::Id& node) { id = node.get_expr(); }

        void visit(const Nodecl::Checkpoint::Level& node) { level = node.get_expr(); }

        void visit(const Nodecl::Checkpoint::Comm& node) { communicator = node.get_expr(); }

        void visit(const Nodecl::Checkpoint::If& node) { if_expr = node.get_expr(); }

        void visit(const Nodecl::Checkpoint::Kind& node) { kind = node.get_expr(); }

        void visit(const Nodecl::Checkpoint::Data& node)
        {
            data_exprs = node.get_exprs().as<Nodecl::List>().to_object_list();
        }
    };

    class CheckpointVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            LoweringPhase* _phase;
        public:
            CheckpointVisitor(LoweringPhase* phase) : _phase(phase) {}
            void visit(const Nodecl::Checkpoint::Init& init_construct);
            void visit(const Nodecl::Checkpoint::Shutdown& shutdown_construct);
            void visit(const Nodecl::Checkpoint::Load& load_construct);
            void visit(const Nodecl::Checkpoint::Store& store_construct);
    };

} }
#endif // TL_CHECKPOINT_VISITOR_HPP

