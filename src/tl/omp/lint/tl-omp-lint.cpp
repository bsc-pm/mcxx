/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "tl-omp-lint.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-analysis-singleton.hpp"
#include "tl-datareference.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace OpenMP {

    struct FunctionCodeVisitor : Nodecl::ExhaustiveVisitor<void>
    {
        public:
            void visit(const Nodecl::FunctionCode& function_code)
            {
                // std::cerr << "At function " << function_code.get_locus_str() << std::endl;
                TL::Analysis::PCFGAnalysis_memento memento;

                TL::Analysis::AnalysisSingleton& singleton = TL::Analysis::AnalysisSingleton::get_analysis();

                TL::ObjectList<TL::Analysis::ExtensibleGraph*> extensible_graphs =
                    singleton.parallel_control_flow_graph(memento, function_code);

                ERROR_CONDITION(extensible_graphs.size() != 1, "I expected 1 graph", 0);

                TL::Analysis::ExtensibleGraph* graph = extensible_graphs[0];

                graph->print_graph_to_dot(false, false, false, false, false, false);

                // Get all task nodes
                TL::ObjectList<TL::Analysis::Node*> tasks = graph->get_tasks_list();

                for (TL::ObjectList<TL::Analysis::Node*>::iterator it = tasks.begin();
                        it != tasks.end();
                        it++)
                {
                    if (task_is_locally_bound(*it)
                            && task_is_statically_determined_to_late_execution(*it))
                    {
                        Nodecl::NodeclBase task = (*it)->get_graph_label();

                        warn_printf("%s: warning: '#pragma omp task' uses local data but may be"
                                " executed after the function ends\n", task.get_locus_str().c_str());
                    }
                }
            }

            // If this function returns false it may mean both unknown/no
            bool data_ref_is_local(TL::DataReference data_ref)
            {
                if (!data_ref.is_valid())
                {
                    return false;
                }

                return data_ref_is_local_rec(data_ref);
            }

            // If this function returns false it may mean both unknown/no
            bool data_ref_is_local_rec(TL::DataReference data_ref)
            {
                TL::Symbol base_sym = data_ref.get_base_symbol();
                if (!base_sym.is_valid())
                    return false;

                if (data_ref.is<Nodecl::Symbol>())
                {
                    return base_sym.get_scope().is_block_scope();
                }
                else if (data_ref.is<Nodecl::Dereference>())
                {
                    // *&a -> a
                    if (data_ref.as<Nodecl::Dereference>().get_rhs().is<Nodecl::Reference>())
                    {
                        return data_ref_is_local_rec(
                                data_ref.as<Nodecl::Dereference>().get_rhs().as<Nodecl::Reference>().get_rhs());
                    }
                    else
                    {
                        return data_ref_is_local_rec(
                                data_ref.as<Nodecl::Dereference>().get_rhs())
                            && base_sym.get_type().is_array();
                    }
                }
                else if (data_ref.is<Nodecl::Reference>())
                {
                    // &*a -> a
                    if (data_ref.as<Nodecl::Reference>().get_rhs().is<Nodecl::Dereference>())
                    {
                        return data_ref_is_local_rec(
                                data_ref.as<Nodecl::Reference>().get_rhs().as<Nodecl::Dereference>().get_rhs());
                    }
                    else
                    {
                        return data_ref_is_local_rec(data_ref.as<Nodecl::Reference>().get_rhs());
                    }
                }
                else if (data_ref.is<Nodecl::ArraySubscript>())
                {
                    return data_ref_is_local_rec(
                            data_ref.as<Nodecl::ArraySubscript>().get_subscripted())
                        && base_sym.get_type().is_array();
                }
                else if (data_ref.is<Nodecl::ClassMemberAccess>())
                {
                    return data_ref_is_local_rec(
                            data_ref.as<Nodecl::ClassMemberAccess>().get_lhs());
                }
                return false;
            }


            bool any_data_ref_is_local(Nodecl::List item_list)
            {
                for (Nodecl::List::iterator it = item_list.begin();
                        it != item_list.end();
                        it++)
                {
                    if (data_ref_is_local(*it))
                        return true;
                }

                return false;
            }

            bool task_is_locally_bound(TL::Analysis::Node *n)
            {
                bool result = false;

                Nodecl::NodeclBase task = n->get_graph_label();
                // std::cerr << "Checking if task at " << task.get_locus_str() << " is locally bound" << std::endl;

                ERROR_CONDITION(task.is_null(), "Invalid target task tree", 0);
                ERROR_CONDITION(!task.is<Nodecl::OpenMP::Task>()
                        && !task.is<Nodecl::OpenMP::TaskCall>(),
                        "Expecting an OpenMP::Task or OpenMP::TaskCall target node here got a %s", 
                        ast_print_node_type(task.get_kind()));
                Nodecl::List task_env;
                if (task.is<Nodecl::OpenMP::Task>())
                {
                    Nodecl::OpenMP::Task inline_task(task.as<Nodecl::OpenMP::Task>());
                    task_env = inline_task.get_environment().as<Nodecl::List>();
                }
                else if (task.is<Nodecl::OpenMP::TaskCall>())
                {
                    Nodecl::OpenMP::TaskCall function_task(task.as<Nodecl::OpenMP::TaskCall>());
                    task_env = function_task.get_site_environment().as<Nodecl::List>();
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }

                Nodecl::OpenMP::Shared shared;
                Nodecl::OpenMP::DepIn dep_in;
                Nodecl::OpenMP::DepOut dep_out;
                Nodecl::OpenMP::DepInout dep_inout;
                for (Nodecl::List::iterator it = task_env.begin();
                        it != task_env.end();
                        it++)
                {
                    if (it->is<Nodecl::OpenMP::DepIn>())
                        dep_in = it->as<Nodecl::OpenMP::DepIn>();
                    else if (it->is<Nodecl::OpenMP::DepOut>())
                        dep_out = it->as<Nodecl::OpenMP::DepOut>();
                    else if (it->is<Nodecl::OpenMP::DepInout>())
                        dep_inout = it->as<Nodecl::OpenMP::DepInout>();
                    else if (it->is<Nodecl::OpenMP::Shared>())
                        shared = it->as<Nodecl::OpenMP::Shared>();
                }

                result = (!shared.is_null() && any_data_ref_is_local(shared.get_shared_symbols().as<Nodecl::List>()))
                    || (!dep_in.is_null() && any_data_ref_is_local(dep_in.get_in_deps().as<Nodecl::List>()))
                    || (!dep_out.is_null() && any_data_ref_is_local(dep_out.get_out_deps().as<Nodecl::List>()))
                    || (!dep_inout.is_null() && any_data_ref_is_local(dep_inout.get_inout_deps().as<Nodecl::List>()));

                // if (result)
                //     std::cerr << "Yes, it is locally bound" << std::endl;
                // else
                //     std::cerr << "No, it is not locally bound " << std::endl;

                return result;
            }

            bool task_is_statically_determined_to_late_execution(TL::Analysis::Node *n)
            {
                bool result = false;

                Nodecl::NodeclBase task = n->get_graph_label();
                // std::cerr << "Checking if task at " << task.get_locus_str()
                //     << " can be late executed" << std::endl;

                bool seen_static_edges = false;
                bool seen_maybe_edges = false;
                bool seen_post_edges = false;

                typedef std::map<std::string, bool*> map_edge_kind_t;

                map_edge_kind_t map_edge_kind;
                map_edge_kind["static"] = &seen_static_edges;
                map_edge_kind["maybe"] = &seen_maybe_edges;
                map_edge_kind["post"] = &seen_post_edges;

                TL::ObjectList<TL::Analysis::Edge*> exit_edges = n->get_exit_edges();

                // ERROR_CONDITION (exit_edges.empty(), "We should have computed at least some exit edge for this task", 0);
                if (!exit_edges.empty())
                {
                    for (TL::ObjectList<TL::Analysis::Edge*>::iterator it = exit_edges.begin();
                            it != exit_edges.end();
                            it++)
                    {
                        std::string exit_label = (*it)->get_label();
                        map_edge_kind_t::iterator mit = map_edge_kind.find(exit_label);

                        if (mit == map_edge_kind.end()) // weird
                            continue;

                        *(mit->second) = true;
                    }

                    // If none of the exit edges is a "maybe" and there is a "post", there is a change
                    // it runs lately
                    result = (!seen_maybe_edges && seen_post_edges);
                }
                else
                {
                    // std::cerr << "No exit edges?" << std::endl;
                }


                // if (result)
                //     std::cerr << "Yes, it can be lated executed" << std::endl;
                // else
                //     std::cerr << "No, we cannot assert this" << std::endl;

                return result;
            }
    };

    Lint::Lint()
    {
        set_phase_name("OpenMP Lint");
        set_phase_description("This phase is able to detect some common pitfalls when using OpenMP");
    }

    void Lint::run(TL::DTO& dto)
    {
        // std::cerr << "Running OpenMP Lint" << std::endl;
        Nodecl::NodeclBase top_level = dto["nodecl"];

        FunctionCodeVisitor function_codes;
        function_codes.walk(top_level);
    }

    void Lint::pre_run(TL::DTO& dto)
    {
    }

} }

EXPORT_PHASE(TL::OpenMP::Lint)
