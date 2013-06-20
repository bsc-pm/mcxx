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

namespace TL { namespace OpenMP {

    struct FunctionCodeVisitor : Nodecl::ExhaustiveVisitor<void>
    {
        public:
            void visit(const Nodecl::FunctionCode& function_code)
            {
                TL::Analysis::PCFGAnalysis_memento memento;

                TL::Analysis::AnalysisSingleton& singleton = TL::Analysis::AnalysisSingleton::get_analysis();

                TL::ObjectList<TL::Analysis::ExtensibleGraph*> extensible_graphs =
                    singleton.parallel_control_flow_graph(memento, function_code);

                ERROR_CONDITION(extensible_graphs.size() != 1, "I expected 1 graph", 0);

                TL::Analysis::ExtensibleGraph* graph = extensible_graphs[0];

                // Get all task nodes
                TL::ObjectList<TL::Analysis::Node*> tasks = graph->get_tasks_list();

                for (TL::ObjectList<TL::Analysis::Node*>::iterator it = tasks.begin();
                        it != tasks.end();
                        it++)
                {
                    if (task_is_locally_bounded(*it)
                            && task_is_statically_determined_to_late_execution(*it))
                    {
                        Nodecl::NodeclBase task = (*it)->get_graph_label();

                        std::cerr << task.get_locus_str() << ": warning: '#pragma omp task' uses local data but may be executed after the function ends" << std::endl;
                    }
                }
            }

            bool task_is_locally_bounded(TL::Analysis::Node *n)
            {
                return false;
            }

            bool task_is_statically_determined_to_late_execution(TL::Analysis::Node *n)
            {
                return false;
            }
    };

    Lint::Lint()
    {
    }

    void Lint::run(TL::DTO& dto)
    {
        std::cerr << "Running OpenMP Lint" << std::endl;
    }

    void Lint::pre_run(TL::DTO& dto)
    {
    }

} }

EXPORT_PHASE(TL::OpenMP::Lint)
