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


#include "tl-omp-base.hpp"
#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"

namespace TL { namespace OpenMP {

    namespace {

        Nodecl::NodeclBase handle_device_clause(
                TL::PragmaCustomClause &device,
                const locus_t* locus)
        {
            TL::ObjectList<Nodecl::NodeclBase> expr_list;
            if (device.is_defined())
            {
                expr_list = device.get_arguments_as_expressions();
            }

            Nodecl::NodeclBase device_id_expr;
            if (expr_list.empty())
            {
                // Our default device is 0
                device_id_expr = const_value_to_nodecl(const_value_get_signed_int(0));
            }
            else 
            {
                device_id_expr = expr_list[0];
                if (expr_list.size() > 1)
                {
                    error_printf("%s: error: too many expressions in 'device' clause\n",
                            expr_list[1].get_locus_str().c_str());
                }
            }
            ERROR_CONDITION(device_id_expr.is_null(), "Expecting a valid device id here", 0);
            
            if (!::is_integer_type(no_ref(device_id_expr.get_type().get_internal_type())))
            {
                error_printf("%s: error: expression of 'device' must be an integer-expression\n",
                        device_id_expr.get_locus_str().c_str());
            }

            return device_id_expr;
        }

        Nodecl::NodeclBase handle_device_clause(TL::PragmaCustomLine &ctr)
        {
            TL::PragmaCustomClause device = ctr.get_clause("device");
            return handle_device_clause(device, ctr.get_locus());
        }

        Nodecl::NodeclBase handle_if_clause(TL::PragmaCustomClause &if_clause,
                const locus_t* locus)
        {
            if (!if_clause.is_defined())
                return Nodecl::NodeclBase::null();

            TL::ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions();

            Nodecl::NodeclBase result;
            if (expr_list.empty())
            {
                error_printf("%s: error: expecting expression in 'if' clause\n",
                        locus_to_str(locus));
            }
            else
            {
                if (expr_list.size() > 1)
                {
                    error_printf("%s: error: too many expression in 'if' clause\n",
                            expr_list[1].get_locus_str().c_str());
                }
                result = expr_list[0];
            }

            return result;
        }

        Nodecl::NodeclBase handle_if_clause(TL::PragmaCustomLine& ctr)
        {
            TL::PragmaCustomClause if_clause = ctr.get_clause("if");
            return handle_if_clause(if_clause, ctr.get_locus());
        }

        void handle_target_data_clauses(TL::PragmaCustomStatement &ctr,
                // out
                Nodecl::NodeclBase &device_id,
                Nodecl::NodeclBase &if_clause)
        {
            TL::PragmaCustomLine line = ctr.get_pragma_line();

            device_id = handle_device_clause(line);
            if_clause = handle_if_clause(line);
        }

        template <typename T>
            Nodecl::NodeclBase make_map_node(Nodecl::NodeclBase n)
            {
                return T::make(n, n.get_locus());
            }

        Nodecl::NodeclBase make_device_data_environment(OpenMP::DataEnvironment& data_environment)
        {
            TL::ObjectList<OpenMP::MappingValue> all_device_mappings = data_environment.get_all_device_mappings();
            struct Filter
            {
                static bool is_map_to(const OpenMP::MappingValue& m) { return m.direction == OpenMP::MAP_DIR_TO; }
                static bool is_map_from(const OpenMP::MappingValue& m) { return m.direction == OpenMP::MAP_DIR_FROM; }
                static bool is_map_tofrom(const OpenMP::MappingValue& m) { return m.direction == OpenMP::MAP_DIR_TOFROM; }
                static bool is_map_alloc(const OpenMP::MappingValue& m) { return m.direction == OpenMP::MAP_DIR_ALLOC; }
            };

            TL::ObjectList<OpenMP::MappingValue> mappings_to =
                all_device_mappings.filter(Filter::is_map_to);
            TL::ObjectList<OpenMP::MappingValue> mappings_from =
                all_device_mappings.filter(Filter::is_map_from);
            TL::ObjectList<OpenMP::MappingValue> mappings_tofrom =
                all_device_mappings.filter(Filter::is_map_tofrom);
            TL::ObjectList<OpenMP::MappingValue> mappings_alloc =
                all_device_mappings.filter(Filter::is_map_alloc);

            struct aux
            {
                TL::ObjectList<OpenMP::MappingValue>& s;
                Nodecl::NodeclBase (*builder)(Nodecl::NodeclBase n);
            } nodes[4] = {
                { mappings_to,     make_map_node<Nodecl::OpenMP::MapTo> },
                { mappings_from,   make_map_node<Nodecl::OpenMP::MapFrom> },
                { mappings_tofrom, make_map_node<Nodecl::OpenMP::MapToFrom> },
                { mappings_alloc, make_map_node<Nodecl::OpenMP::MapAlloc> }
            };

            Nodecl::List result;
            for (int i = 0; i < 4; i++)
            {
                if (nodes[i].s.empty())
                    continue;

                TL::ObjectList<OpenMP::MappingValue> &current_mappings = nodes[i].s;

                Nodecl::List current_list;
                for (TL::ObjectList<OpenMP::MappingValue>::iterator
                        it = current_mappings.begin();
                        it != current_mappings.end();
                        it++)
                {
                    ERROR_CONDITION(it->map_expr.is_null(), "Invalid tree at this point", 0);

                    current_list.append(it->map_expr.shallow_copy());
                }

                result.append(
                        nodes[i].builder(current_list)
                        );
            }

            return result;
        }
    }

    void Base::target_data_handler_pre(TL::PragmaCustomStatement ctr) { }
    void Base::target_data_handler_post(TL::PragmaCustomStatement ctr)
    {
        OpenMP::DataEnvironment &data_environment =
            _core.get_openmp_info()->get_data_environment(ctr);

        Nodecl::NodeclBase device_id, if_clause;
        handle_target_data_clauses(ctr,
                // out
                device_id,
                if_clause);

        if (this->emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << ctr.get_locus_str() << ": " << "TARGET DATA construct\n"
                << ctr.get_locus_str() << ": " << "---------------------\n"
                ;
            // TODO - Report explicit mappings
        }

        Nodecl::List device_data_environment;
        device_data_environment.append(
                Nodecl::OpenMP::Device::make(device_id, device_id.get_locus()));
        if (!if_clause.is_null())
        {
            device_data_environment.append(
                    Nodecl::OpenMP::If::make(if_clause, if_clause.get_locus()));
        }

        Nodecl::NodeclBase map_clause = make_device_data_environment(data_environment);

        device_data_environment.append(map_clause);

        Nodecl::OpenMP::TargetData target_data =
            Nodecl::OpenMP::TargetData::make(
                    device_data_environment,
                    ctr.get_statements().shallow_copy(),
                    ctr.get_locus());

        ctr.replace(target_data);
    }

} }
