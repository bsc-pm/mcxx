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
#include "tl-nanos6-device-manager.hpp"

#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-omp-reduction.hpp"

#include "cxx-diagnostic.h"

namespace TL { namespace Nanos6 {

    struct Lower : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            LoweringPhase* _phase;
            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase> _final_stmts_map;
            DeviceManager _device_manager;

        public:
            Lower(LoweringPhase* phase,
                std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& final_stmts_map)
            : _phase(phase), _final_stmts_map(final_stmts_map) { }

            void visit(const Nodecl::OmpSs::Loop &n);
            void visit(const Nodecl::OmpSs::Release &n);
            void visit(const Nodecl::OmpSs::TaskCall &n);
            void visit(const Nodecl::OpenMP::Atomic &n);
            void visit(const Nodecl::OpenMP::Critical &n);
            void visit(const Nodecl::OpenMP::Task &n);
            void visit(const Nodecl::OpenMP::Taskwait &n);

            // Unsupported
#define UNIMPLEMENTED_VISITOR(TYPE) \
            void visit(const TYPE &n) { \
                error_printf_at(n.get_locus(), \
                        "this construct is not supported by Nanos6\n"); \
            } \

            UNIMPLEMENTED_VISITOR(Nodecl::OmpSs::Register)
            UNIMPLEMENTED_VISITOR(Nodecl::OmpSs::Unregister)
            UNIMPLEMENTED_VISITOR(Nodecl::OpenMP::BarrierFull)
            UNIMPLEMENTED_VISITOR(Nodecl::OpenMP::FlushMemory)
            UNIMPLEMENTED_VISITOR(Nodecl::OpenMP::For)
            UNIMPLEMENTED_VISITOR(Nodecl::OpenMP::Taskgroup)
            UNIMPLEMENTED_VISITOR(Nodecl::OpenMP::Taskloop)
            UNIMPLEMENTED_VISITOR(Nodecl::OpenMP::Taskyield)

#undef UNIMPLEMENTED_VISITOR

            struct ReductionFunctions
            {
                ReductionFunctions()
                    : initializer(), combiner()
                {}

                ReductionFunctions(TL::Symbol init, TL::Symbol comb)
                    : initializer(init), combiner(comb)
                {}

                TL::Symbol initializer;
                TL::Symbol combiner;
            };

            // Note: This comparator takes into consideration the reduction info
            // and type only, ignoring the symbol
            struct ReductionTypeComparator {
                bool operator() (const TL::OpenMP::Lowering::ReductionItem& x, const TL::OpenMP::Lowering::ReductionItem& y) const
                {
                    if (&x == &y) return false;

                    if (x._reduction_info != y._reduction_info)
                        return (x._reduction_info < y._reduction_info);
                    else if (!x._reduction_type.is_same_type(y._reduction_type))
                        return (x._reduction_type < y._reduction_type);
                    else
                        return false;
                }
            };
            typedef std::map<TL::OpenMP::Lowering::ReductionItem, ReductionFunctions, ReductionTypeComparator>
                reduction_functions_map_t;
            reduction_functions_map_t _reduction_functions_map;

            DeviceManager& get_device_manager() { return _device_manager; }


        private:
            void lower_taskwait(const Nodecl::OpenMP::Taskwait& n);
            void lower_taskwait_with_dependences(const Nodecl::OpenMP::Taskwait& n);

            void lower_task(const Nodecl::OpenMP::Task& n);
            void lower_task(const Nodecl::OpenMP::Task& n, const Nodecl::NodeclBase& serial_stmts);

            void visit_task_call(const Nodecl::OmpSs::TaskCall& construct);
            void visit_task_call_c(const Nodecl::OmpSs::TaskCall& construct);
            void visit_task_call_fortran(const Nodecl::OmpSs::TaskCall& construct);
    };

} }

#endif // TL_NANOS6_VISITOR_HPP
