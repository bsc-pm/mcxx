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
#include "tl-pcfg-utils.hpp"

namespace TL { 
namespace Analysis {
namespace TaskAnalysis {

    // **************************************************************************************************** //
    // *************************** Class implementing task PCFG synchronization *************************** //
    
    #define SYNC_KIND_LIST \
    SYNC_KIND(unknown) \
    SYNC_KIND(strict) \
    SYNC_KIND(static) \
    SYNC_KIND(post) \
    SYNC_KIND(maybe)

    enum SyncKind
    {
#undef SYNC_KIND
#define SYNC_KIND(X) Sync_##X,
        SYNC_KIND_LIST
#undef SYNC_KIND
    };

    inline const char* sync_kind_to_str(SyncKind sk)
    {
        switch (sk)
        {
#undef SYNC_KIND
#define SYNC_KIND(X) case Sync_##X : return #X;
        SYNC_KIND_LIST
#undef SYNC_KIND
            default: return "";
        }
        return "";
    };

    typedef std::pair<Node*, SyncKind> PointOfSyncInfo;
    typedef std::set<PointOfSyncInfo> PointOfSyncSet;
    typedef std::map<Node*, PointOfSyncSet> PointsOfSync;

    struct LIBTL_CLASS TaskSynchronizations
    {
    private:
        ExtensibleGraph* _graph;

    public:
        TaskSynchronizations( ExtensibleGraph* graph );

        void compute_task_synchronizations( );
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
        
        // There can be more that one _last_sync node when a task is inside a loop and
        // there is taskwait / barrier after the task scheduling point inside the loop
        // In that case, the first iteration _last_sync will be previous to the loop, 
        // but for the next iterations, the _last_sync will be the one inside the loop
        ObjectList<Node*> _last_sync;
        ObjectList<Node*> _next_sync;
        
        
        // *** Private methods *** /
        
        void find_last_synchronization_point_in_parents( Node* current );
        void find_last_synchronization_point_in_children( Node* current, Node* loop );
        
        //! This method calculates the next and last synchronization points of a task
        void define_concurrent_regions_limits( Node* task );
        
        /*!Computes the tasks that are concurrent with a given task
         * Also computes the last synchronization point in the encountering thread of the task
         */
        void compute_concurrent_tasks( Node* task );
        
    public:
        // *** Constructor *** //
        TaskConcurrency( ExtensibleGraph* graph );
        
        // *** Modifiers *** //
        void compute_tasks_concurrency( );
        void compute_task_concurrency( Node* task );
    };
    
    // ************************* END class implementing task concurrency analysis ************************* //
    // **************************************************************************************************** //
} 
}
}

#endif
