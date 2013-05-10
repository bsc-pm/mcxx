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

    struct AliveTaskItem
    {
        Node* node;
        // Arbitrary domain id. Every nesting domain has its own domain id
        int domain;

        AliveTaskItem(Node* node_, int domain_)
            : node(node_), domain(domain_)
        {
        }

        bool operator<(const AliveTaskItem& it) const
        {
            return (this->node < it.node)
                || (!(it.node < this->node) && 
                        (this->domain < it.domain));
        }

        bool operator==(const AliveTaskItem& it) const
        {
            return (this->node == it.node)
                && (this->domain == it.domain);
        }

    };


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

    inline std::string sync_kind_to_str(SyncKind sk)
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

    typedef std::set<AliveTaskItem> StaticSyncTaskSet;
    typedef std::set<AliveTaskItem> AliveTaskSet;

    typedef std::pair<Node*, SyncKind> PointOfSyncInfo;
    typedef std::set<PointOfSyncInfo> PointOfSyncSet;
    typedef std::map<Node*, PointOfSyncSet> PointsOfSync;

    // bool operator<(const PointOfSyncInfo& a, const PointsOfSyncInfo& b);
    // bool operator==(const PointOfSyncInfo& a, const PointsOfSyncInfo& b);

    struct LIBTL_CLASS TaskSynchronizations
    {
        private:
            ExtensibleGraph* _graph;


            void compute_task_synchronizations_rec(Node* current,
                    bool &changed,
                    PointsOfSync& points_of_sync,
                    int current_domain_id,
                    int &next_domain_id);
        public:
            TaskSynchronizations(ExtensibleGraph* graph);

            void compute_task_synchronizations();
    };


} }

#endif
