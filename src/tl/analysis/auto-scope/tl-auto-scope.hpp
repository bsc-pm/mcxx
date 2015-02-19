/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona* Supercomputing Center             *
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

#ifndef TL_AUTO_SCOPE_HPP
#define TL_AUTO_SCOPE_HPP

#include <climits>

#include "tl-extensible-graph.hpp"
#include "tl-task-sync.hpp"

namespace TL {
namespace Analysis {

    class AutoScoping
    {
    private:

        // *********************** Private members *********************** //

        ExtensibleGraph* _graph;
        
        ObjectList<Node*> _simultaneous_tasks;
        
        bool _check_only_local;

        // *********************** Private methods *********************** //

        void scope_variable(Node* task, Utils::UsageKind usage, const NBase& n, NodeclSet& scoped_vars);

        void compute_task_auto_scoping_rec(Node* task, Node* current, NodeclSet& scoped_vars);
        void compute_task_auto_scoping(Node* task);

    public:

        // *** Constructor *** //
        AutoScoping(ExtensibleGraph* graph);

        // *** Modifiers *** //
        /*!
         * 1. Determine the different regions that interfere in the analysis of t:
         *   − One region is the one defined by the code in the encountered thread that can potentially be executed
         *     in parallel with the task. This region is defined by two points:
         *      · Scheduling: is the point where the task is scheduled. Any previous access to v by the encountering
         *                    thread is irrelevant when analysing the task because it is already executed.
         *      · Next_sync: is the point where the task is synchronized with the rest of the threads in execution.
         *                   This point can only be a barrier or a taskwait. Here we take into account that taskwait
         *                   constructs only enforces the synchronization of these tasks which are child of the current
         *                   task region.
         *   − Other regions are the ones enclosed in tasks that can be executed in parallel with t. We will call these
         *     tasks ti; i2[0::T ] and the region of code where we can find tasks in this condition is defined by:
         *      · Last_sync: is the immediately previous point to the scheduling point where a synchronization enforces
         *                   all previous executions to be synchronized. We can only assure this point with a barrier and
         *                   in specific cases with a taskwait. We only can trust the taskwait if we know all the code
         *                   executed previously and we can assure that the current task region has not generated
         *                   grandchild tasks.
         *      · Next_sync: is the same point as explained for the analysis of the encountered thread. In order to
         *                   simplify the reading of the algorithm bellow, from now on we will talk about the region
         *                   defined between the scheduling point and the next_sync point and the different regions defined
         *                   by the tasks ti; i2[0::T ] as one unique region defined by the points:
         *                   − init, referencing both scheduling and any entry point to the tasks ti; i2[0::T ].
         *                   − end, referencing both next_sync and any exit point to the tasks ti; i2[0::T ].
         * 2. For each v scalar variable appearing within the task t:
         *      (a) If we cannot determine the type of access (read or write) performed over v either within the task
         *          or between init and end because the variable appears as a parameter in a function call that we do not
         *          have access to, then v is scoped as UNDEFINED.
         *      (b) If v is not used between init and end, then:
         *              i. If v is only read within the task, then v is scoped as FIRSTPRIVATE.
         *             ii. If v is write within the task, then:
         *                   A. If v is live after end, then v is scoped as SHARED.
         *                   B. If v is dead after end, then:
         *                       − If the first action performed in v is a write, then v is scoped as PRIVATE.
         *                       − If the first action performed in v is a read, then v is scoped as FIRSTPRIVATE.
         *              i. If v is live after end, then v is scoped as SHARED.
         *              ii. If v is dead after end, then:
         *                  A. If the first action performed in v is a write, then v is scoped as PRIVATE.
         *                  B. If the first action performed in v is a read, then v is scoped as FIRSTPRIVATE.
         *      (c) If v is used between init and end, then:
         *              i. If v is only read in both between init and end and within the task, then the v is scoped as FIRSTPRIVATE.
         *              ii. If v is written in either between init and end or within the task, then we look for data race
         *                  conditions, thus:
         *                  A. If it can occur a data race condition, then v has to be privatized. Sic:
         *                      − If the first action performed in v within the task is a write, then v is scoped as PRIVATE.
         *                      − If the first action performed in v within the task is a read, then v is scoped as FIRSTPRIVATE.
         *                  B. If we can assure that no data race can occur, then v is scoped as SHARED.
         * 3. For each use ai; i2[0::N] (where N is the number of uses) of an array variable a appearing within the task t.
         *      (a) We apply the methodology used for the scalars.
         *      (b) Since OpenMP does not allow different scopes for the subparts of a variable, then we have to mix all the
         *          results we have get in the previous step. In order to do that we will follow the rules bellow:
         *          i. If the whole array a or all the parts ai have the same scope sc, then a is scoped as sc.
         *          ii. If there are different regions of the array with different scopes, then:
         *              A. If some ai has been scoped as UNDEFINED then a is scoped as UNDEFINED.
         *              B. If at least one ai is FIRSTPRIVATE and all aj; j2[0::N] where j! = i are PRIVATE, then a is scoped as FIRSTPRIVATE.
         *              C. If at least one ai is SHARED and all aj; j2[0::N] where j! = i are PRIVATE or FIRSTPRIVATE, then,
         *                 fulfilling the sequential consistency rules, a is scoped as SHARED.
         * 4. NOTE: If we cannot determine the init point, then we cannot analyze the task because we do not know which regions
         *          of code can be executed in parallel with t.
         * 5. NOTE: If we cannot determine the end point, then we can only scope those variables that are local to the function containing t.
         *
         * Data race conditions can appear when two threads can access to the same memory unit in the same time and at least one
         * of these accesses is a write. In order to analyze data race conditions in the process of auto-scoping the variables
         * of a task we have to analyze the code appearing in all regions defined between the init and end points described in
         * the previous section. Any variable v appearing in two different regions where at least one of the accesses is a write and
         * no one of the two accesses is blocked by either and atomic construct, a critical construct or a lock routine
         * (omp_init_lock / omp_destroy_lock, omp_set_lock / omp_unset_lock), can trigger a data race situation.
         */
        void compute_auto_scoping();
    };

}
}

#endif      // TL_AUTO_SCOPE_HPP