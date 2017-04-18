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

#ifndef HLT_COLLAPSE_LOOP_HPP
#define HLT_COLLAPSE_LOOP_HPP

#include "hlt-transform.hpp"
#include "tl-scope.hpp"

namespace TL { namespace HLT {

    //! \addtogroup HLT High Level Transformations

    //! Collapse the iteration space of two or more loops
    //
    //  -- For example, those loops:
    //
    //    #pragma hlt collapse(2)
    //    for (int i = 0; i < 16; i += a)
    //    for (int j = 0; j < 10; j += 2)
    //    {
    //        M[i][j];
    //    }
    //
    //  -- would be collapsed as follows:
    //
    //    {
    //      -- Variables for innermost loop:
    //
    //        -- Loop step
    //        const int collapse_1_step = 2;
    //
    //        -- Ceiling division between size and step -> number of effective elements
    //        int collapse_1_num_elements = (9 - 0 + collapse_1_step)/collapse_1_step;
    //        collapse_1_num_elements = collapse_1_num_elements < 0 ? 0 : collapse_1_num_elements;
    //
    //        -- Complete dimension of innermost loop, to be used to mask 'j' increment
    //        const int collapse_1_rounded_size = collapse_1_num_elements*collapse_1_step;
    //
    //      -- Variables for outermost loop:
    //
    //        -- Loop step
    //        const int collapse_0_step = a;
    //
    //        -- Ceiling division between size and step -> number of effective elements
    //        int collapse_0_num_elements = (15 - 0 + collapse_0_step)/collapse_0_step;
    //        collapse_0_num_elements = collapse_0_num_elements < 0 ? 0 : collapse_0_num_elements;
    //
    //        -- Complete dimension of outermost loop, to be used to mask 'i' increment
    //        const int collapse_0_rounded_size = collapse_0_num_elements*collapse_0_step;
    //
    //      -- Collapsed loop:
    //
    //        for (long long int collapse_it = 0; collapse_it < collapse_1_num_elements*collapse_0_num_elements; ++collapse_it)
    //        {
    //          -- Compute new i, j indexes:
    //
    //            int collapse_i = 0 + ((collapse_it/collapse_1_num_elements)*collapse_0_step)%collapse_0_rounded_size;
    //            int collapse_j = 0 + ((collapse_it/1)*collapse_1_step)%collapse_1_rounded_size;
    //
    //          -- User code:
    //
    //            M[collapse_i][collapse_j];
    //        }
    //    }

    class LIBHLT_CLASS LoopCollapse : public Transform
    {
        private:
            Nodecl::NodeclBase _loop;
            Nodecl::NodeclBase _transformation;
            TL::Scope _pragma_context;

            int _collapse_factor;

            struct LoopInfo
            {
                TL::Symbol induction_var;
                Nodecl::NodeclBase lower_bound;
                Nodecl::NodeclBase upper_bound;
                Nodecl::NodeclBase step;
            };

            static void compute_collapse_statements(
                    int collapse_factor,
                    TL::Symbol collapse_induction_var,
                    // Inout
                    TL::ObjectList<LoopInfo>& loop_info,
                    TL::Scope& collapse_scope,
                    TL::Scope& loop_statements_scope,
                    // Out
                    Nodecl::List& collapse_statements,
                    Nodecl::List& loop_statements,
                    Nodecl::NodeclBase& condition_bound);

            static void compute_loop_information(
                    Nodecl::NodeclBase node,
                    int collapse_factor,
                    // Out
                    TL::ObjectList<LoopInfo>& loop_info,
                    Nodecl::List& loop_statements);

        public:
            LoopCollapse();

            // Properties
            LoopCollapse& set_loop(Nodecl::NodeclBase loop);
            LoopCollapse& set_collapse_factor(int collapse_factor);
            LoopCollapse& set_pragma_context(const TL::Scope& context);

            // Action
            void collapse();

            // Results
            Nodecl::NodeclBase get_whole_transformation() const { return _transformation; }
    };
}}
#endif // HLT_COLLAPSE_LOOP_HPP
