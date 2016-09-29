/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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


#ifndef TL_NANOS6_VISITOR_HPP
#define TL_NANOS6_VISITOR_HPP

#include "tl-nanos6.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL { namespace Nanos6 {

    struct Lower : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            LoweringPhase* _phase;

        public:
            Lower(LoweringPhase* phase) : _phase(phase) { }

            virtual void visit(const Nodecl::OpenMP::Task& n);
            virtual void visit(const Nodecl::OpenMP::TaskwaitShallow& n);
            virtual void visit(const Nodecl::OmpSs::TaskCall& n);
            virtual void visit(const Nodecl::OpenMP::Critical& n);
            virtual void visit(const Nodecl::OpenMP::Atomic& n);

            // Unsupported
            virtual void visit(const Nodecl::OpenMP::Taskyield &n);
            virtual void visit(const Nodecl::OpenMP::For &n);
            virtual void visit(const Nodecl::OpenMP::BarrierFull &n);
            virtual void visit(const Nodecl::OmpSs::WaitOnDependences &n);
            virtual void visit(const Nodecl::OpenMP::FlushMemory &n);
            virtual void visit(const Nodecl::OmpSs::Register &n);
            virtual void visit(const Nodecl::OmpSs::Unregister &n);

        private:
            void visit_task_call(const Nodecl::OmpSs::TaskCall& construct);
            void visit_task_call_c(const Nodecl::OmpSs::TaskCall& construct);
            void visit_task_call_fortran(const Nodecl::OmpSs::TaskCall& construct);

            void capture_argument_for_task_call(
                TL::Symbol called_sym,
                TL::Scope new_block_context_sc,
                TL::Type parameter_type,
                Nodecl::NodeclBase argument,
                /* out */ TL::ObjectList<TL::Symbol> &argument_captures_syms,
                /* out */ Nodecl::List &new_args,
                /* out */ Nodecl::List &argument_captures);
    };

} }

#endif // TL_NANOS6_VISITOR_HPP
