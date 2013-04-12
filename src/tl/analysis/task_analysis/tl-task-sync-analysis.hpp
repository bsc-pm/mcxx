/*--------------------------------------------------------------------
 (C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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

#ifndef TL_TASK_SYNC_ANALYSIS_HPP
#define TL_TASK_SYNC_ANALYSIS_HPP

#include "tl-extended-symbol.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL { namespace Analysis {

    // Task synchronization relationship
    enum TaskSyncRel
    {
        TaskSync_Unknown = 0,
        TaskSync_Yes = 1,
        TaskSync_No = 2
    };

    typedef std::set<Node*> AliveTaskSet;
    typedef std::map<Node*, AliveTaskSet > AliveTasks;
    typedef std::set<Node*> PointOfSyncSet;
    typedef std::map<Node*, PointOfSyncSet> PointsOfSync;

    struct LIBTL_CLASS TaskSynchronizations
    {
        private:
            ExtensibleGraph* _graph;


            void compute_task_synchronizations_rec(Node* current,
                    bool &changed,
                    PointsOfSync& points_of_sync);
        public:
            TaskSynchronizations(ExtensibleGraph* graph);

            void compute_task_synchronizations();
    };


} }

#endif
