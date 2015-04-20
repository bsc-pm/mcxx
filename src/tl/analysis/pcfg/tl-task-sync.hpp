/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

#include "tl-extensible-graph.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL { 
namespace Analysis {
namespace TaskAnalysis {

    // **************************************************************************************************** //
    // *************************** Class implementing task PCFG synchronization *************************** //

    typedef std::pair<Node*, SyncKind> PointOfSyncInfo;
    typedef ObjectList<PointOfSyncInfo> PointOfSyncList;
    typedef std::map<Node*, PointOfSyncList> PointsOfSync;

    struct LIBTL_CLASS TaskSynchronizations
    {
    private:
        ExtensibleGraph* _graph;

    public:
        TaskSynchronizations(ExtensibleGraph* graph, bool is_ompss_enabled);

        void compute_task_synchronizations();
    };

    // ************************* END class implementing task PCFG synchronization ************************* //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // *************************** Class implementing task concurrency analysis *************************** //

    class LIBTL_CLASS TaskConcurrency
    {
    private:
        // *** Private members *** //
        ExtensibleGraph* _graph;

        // *** Private methods *** /
        void find_next_synchronization_points(Node* task);
        void find_last_synchronization_points_for_sequential_code(Node* task_creation, Node* task);
        void find_last_synchronization_points_for_tasks(Node* current, Node* task);

        //! This method calculates the next and last synchronization points of a task
        void define_concurrent_regions_limits(Node* task);

        /*!Computes the tasks that are concurrent with a given task
         * Also computes the last synchronization point in the encountering thread of the task
         */
        void compute_concurrent_tasks(Node* task);

    public:
        // *** Constructor *** //
        TaskConcurrency(ExtensibleGraph* graph);

        // *** Modifiers *** //
        void compute_tasks_concurrency();
    };

    // ************************* END class implementing task concurrency analysis ************************* //
    // **************************************************************************************************** //
} 
}
}

#endif
