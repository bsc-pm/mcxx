/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#ifndef TL_TASKAGGREGATION_HPP
#define TL_TASKAGGREGATION_HPP

#include "tl-compilerphase.hpp"
#include "tl-omp.hpp"
#include "tl-statement.hpp"
#include "tl-source.hpp"

namespace TL
{
    class TaskAggregationPhase : public OpenMP::OpenMPPhase
    {
        private:
            int _num_aggregations;
        public:
            TaskAggregationPhase();

            void while_preorder(OpenMP::CustomConstruct);
            void while_postorder(OpenMP::CustomConstruct);

            void check_construction(Statement st,
                    bool &is_valid,
                    ObjectList<Statement> &list_of_tasks,
                    ObjectList<Statement> &sequentiation_code,
                    int nesting_level);
            void check_task_aggregated_body(
                    Statement st,
                    Statement body,
                    bool &is_valid,
                    ObjectList<Statement> &list_of_tasks,
                    ObjectList<Statement> &sequentiation_code,
					bool is_for);

            Source get_aggregated_code(OpenMP::CustomConstruct);
            Source get_aggregated_code_while(Statement st, 
                    ObjectList<Statement> &list_of_tasks,
                    ObjectList<Statement> &sequentiation_code,
                    AST_t chunk);
            Source get_aggregated_code_for(Statement st, 
                    ObjectList<Statement> &list_of_tasks,
                    ObjectList<Statement> &sequentiation_code,
                    AST_t chunk);

            void check_aggregated_data_sharing_clauses(
                    Statement st,
                    ObjectList<Statement> &task_list, 
                    bool &is_valid);

            Source aggregate_data_sharing_clauses(
                    ObjectList<OpenMP::Directive> directive_list);
    };
}

EXPORT_PHASE(TL::TaskAggregationPhase);

#endif // TL_TASKAGGREGATION_HPP
