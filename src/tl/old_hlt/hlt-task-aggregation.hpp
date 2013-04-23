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




#ifndef HLT_TASK_AGGREGATION_HPP
#define HLT_TASK_AGGREGATION_HPP

#include "tl-langconstruct.hpp"
#include "tl-omp.hpp"
#include "hlt-transform.hpp"

namespace TL
{
    namespace HLT
    {
        class TaskPart
        {
            private:
                ObjectList<Statement> _prolog;
                TL::PragmaCustomConstruct *_task;

            public:
                TaskPart(ObjectList<Statement> prolog, TL::PragmaCustomConstruct task)
                    : _prolog(prolog), _task(new TL::PragmaCustomConstruct(task)) { }

                TaskPart(TL::PragmaCustomConstruct task)
                    : _task(new TL::PragmaCustomConstruct(task)) { }

                TaskPart(ObjectList<Statement> prolog)
                    : _prolog(prolog), _task(NULL) { }

                TaskPart(const TaskPart& t)
                    : _prolog(t._prolog), _task(new TL::PragmaCustomConstruct(*t._task))
                {
                }

                ~TaskPart() 
                {
                    delete _task;
                }

                ObjectList<Statement> get_prolog()
                {
                    return _prolog;
                }

                bool has_task()
                {
                    return (_task != NULL);
                }

                TL::PragmaCustomConstruct get_task()
                {
                    return *_task;
                }
        };

        class TaskAggregation : public BaseTransform
        {
            public:
                enum AggregationMethod
                {
                    PREDICATION,
                    BUNDLING
                };
            private:
                Statement _stmt;
                AggregationMethod _method;
                int _bundling_amount;

				bool _do_not_create_tasks;
				bool _timing;

                Source *_global_bundling_src;
                Source *_finish_bundling_src;

                AST_t _enclosing_function_def_tree;

                static void get_task_parts_aux(ObjectList<TaskPart>& result, 
                        ObjectList<Statement> &current_prologue, Statement stmt);

                Source do_aggregation(bool contains_conditional_code);
                Source do_predicated_aggregation();
                Source do_bundled_aggregation();
                Source do_simple_aggregation();
            protected:
                virtual Source get_source();
            public:
                static bool contains_relevant_openmp(Statement stmt, bool &contains_conditional_code);
                static bool contains_relevant_openmp(Statement stmt);
                static ObjectList<TaskPart> get_task_parts(Statement stmt);
                TaskAggregation(Statement stmt, AggregationMethod = PREDICATION);

                TaskAggregation& set_aggregation_method(AggregationMethod);

                TaskAggregation& set_bundling_amount(int amount);

                TaskAggregation& set_global_bundling_source(Source& src);
                TaskAggregation& set_finish_bundling_source(Source& src);

				TaskAggregation& set_do_not_create_tasks(bool b);

                TaskAggregation& set_enclosing_function_tree(AST_t enclosing_tree);

				TaskAggregation& set_timing(bool b);
        };
    }
}

#endif // HLT_TASK_AGGREGATION_HPP
