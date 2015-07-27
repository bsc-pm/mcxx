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
#ifndef TL_OMP_BASE_UTILS_HPP
#define TL_OMP_BASE_UTILS_HPP

#include "tl-omp-base.hpp"
#include "tl-omp-core.hpp"

namespace TL { namespace OpenMP {

    template <typename T, typename List>
        void Base::make_dependency_list(
                List& dependences,
                DependencyDirection kind,
                const locus_t* locus,
                ObjectList<Nodecl::NodeclBase>& result_list)
        {
            TL::ObjectList<Nodecl::NodeclBase> data_ref_list;
            for (typename List::iterator it = dependences.begin();
                    it != dependences.end();
                    it++)
            {
                if (it->get_kind() != kind)
                    continue;

                data_ref_list.append(it->get_dependency_expression().shallow_copy());

                if (emit_omp_report())
                {
                    // Let's make sure this is properly aligned
                    std::stringstream ss;
                    ss
                        << OpenMP::Report::indent
                        << OpenMP::Report::indent
                        << it->get_dependency_expression().prettyprint()
                        ;

                    int length = ss.str().size();
                    int diff = 20 - length;
                    if (diff > 0)
                        std::fill_n( std::ostream_iterator<const char*>(ss), diff, " ");

                    ss
                        << " " << dependence_direction_to_str(kind) << "\n"
                        ;

                    *_omp_report_file
                        << ss.str();
                }
            }

            if (!data_ref_list.empty())
            {
                result_list.append(T::make(Nodecl::List::make(data_ref_list), locus));
            }
        }

    template <typename T, typename List>
        void Base::make_copy_list(
                List& dependences,
                TL::OmpSs::CopyDirection kind,
                const locus_t* locus,
                ObjectList<Nodecl::NodeclBase>& result_list)
        {
            TL::ObjectList<Nodecl::NodeclBase> data_ref_list;
            for (typename List::iterator it = dependences.begin();
                    it != dependences.end();
                    it++)
            {
                if (it->get_kind() != kind)
                    continue;

                data_ref_list.append(it->get_copy_expression().shallow_copy());

                if (emit_omp_report())
                {
                    // Let's make sure this is properly aligned
                    std::stringstream ss;
                    ss
                        << OpenMP::Report::indent
                        << OpenMP::Report::indent
                        << it->get_copy_expression().prettyprint()
                        ;

                    int length = ss.str().size();
                    int diff = 20 - length;
                    if (diff > 0)
                        std::fill_n( std::ostream_iterator<const char*>(ss), diff, " ");

                    ss
                        << " " << copy_direction_to_str(kind) << "\n"
                        ;

                    *_omp_report_file
                        << ss.str();
                }
            }

            if (!data_ref_list.empty())
            {
                result_list.append(T::make(Nodecl::List::make(data_ref_list), locus));
            }
        }
}}
#endif // TL_OMP_BASE_UTILS_HPP
