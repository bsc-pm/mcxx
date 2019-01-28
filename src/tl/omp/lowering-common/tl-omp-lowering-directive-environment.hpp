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


#ifndef TL_OMP_LOWERING_DIRECTIVE_ENVIRONMENT_HPP
#define TL_OMP_LOWERING_DIRECTIVE_ENVIRONMENT_HPP

#include "tl-nodecl.hpp"
#include "tl-type.hpp"
#include "tl-symbol.hpp"

#include "tl-omp-reduction.hpp"

namespace TL { namespace OpenMP { namespace Lowering {

    //! It represents all the information that we need to keep in order to
    //! support task reductions
    struct ReductionItem
    {
        TL::Symbol _symbol;
        TL::Type _reduction_type;
        TL::OpenMP::Reduction* _reduction_info;
        bool _is_weak;

        ReductionItem(TL::Symbol symbol, TL::Type reduction_type,
                TL::OpenMP::Reduction* reduction_info, bool is_weak)
            : _symbol(symbol), _reduction_type(reduction_type),
            _reduction_info(reduction_info), _is_weak(is_weak)
        {}

        TL::Symbol get_symbol() const
        {
            return _symbol;
        }

        bool operator==(const ReductionItem& red_item) const
        {
            return (this == &red_item) ||
                ((_symbol == red_item._symbol)
                && _reduction_type.is_same_type(red_item._reduction_type)
                && (_reduction_info == red_item._reduction_info)
                && (_is_weak == red_item._is_weak));
        }
    };

    //! This class contains all the information associated with the environment directive
    struct DirectiveEnvironment
    {
        /* -------- Data-Sharing information ------ */
        TL::ObjectList<TL::Symbol> shared;
        TL::ObjectList<TL::Symbol> private_;
        TL::ObjectList<TL::Symbol> captured_value; // Superset of firstprivate
        TL::ObjectList<TL::Symbol> lastprivate;
        TL::ObjectList<TL::Symbol> firstlastprivate;
        TL::ObjectList<ReductionItem> reduction;
        TL::ObjectList<ReductionItem> in_reduction;

        /* --------  Dependences information ------ */
        TL::ObjectList<Nodecl::NodeclBase> dep_in;
        TL::ObjectList<Nodecl::NodeclBase> dep_out;
        TL::ObjectList<Nodecl::NodeclBase> dep_inout;
        TL::ObjectList<Nodecl::NodeclBase> dep_weakin;
        TL::ObjectList<Nodecl::NodeclBase> dep_weakout;
        TL::ObjectList<Nodecl::NodeclBase> dep_weakinout;
        TL::ObjectList<Nodecl::NodeclBase> dep_commutative;
        TL::ObjectList<Nodecl::NodeclBase> dep_concurrent;
        TL::ObjectList<Nodecl::NodeclBase> dep_weakcommutative;
        TL::ObjectList<Nodecl::NodeclBase> dep_reduction;
        TL::ObjectList<Nodecl::NodeclBase> dep_weakreduction;

        /* --------  OmpSs-2 scheduling & threshold information ------ */
        Nodecl::NodeclBase final_clause;
        Nodecl::NodeclBase if_clause;
        Nodecl::NodeclBase cost_clause;
        Nodecl::NodeclBase priority_clause;
        Nodecl::NodeclBase chunksize; // Taskloop

        /* --------  Task flags & other stuff  ------ */
        bool is_tied;
        bool task_is_loop;
        bool task_is_taskwait_with_deps;
        bool task_is_taskcall;
        bool wait_clause;
        bool any_task_dependence;

        /* --------  Debug & profiling information  ------ */
        std::string task_label;
        const locus_t* locus_of_task_declaration;

        /* --------  Device Information  ------ */
        TL::ObjectList<std::string> device_names;
        TL::ObjectList<Nodecl::NodeclBase> ndrange;

        DirectiveEnvironment(Nodecl::NodeclBase environment);

        private:

        TL::ObjectList<TL::Symbol> _firstprivate;

        //! OpenMP::Base marks C++'s 'this' as shared, which concepually is if
        //! it weren't because 'this' is an rvalue pointer. We should
        //! firstprivatized it instead.
        void fix_data_sharing_of_this();

        void compute_captured_values();

        void firstprivatize_symbols_without_data_sharing();

        void compute_captured_saved_expressions();

        void fix_data_sharing_of_captured_saved_expressions();

        void handle_array_bound(Nodecl::NodeclBase n);

        void walk_type_for_saved_expressions(TL::Type t);

        bool symbol_has_data_sharing_attribute(TL::Symbol sym) const;

        friend class FirstprivateSymbolsWithoutDataSharing;
        friend class DirectiveEnvironmentVisitor;


    };

} } }
#endif // TL_OMP_LOWERING_DIRECTIVE_ENVIRONMENT_HPP

