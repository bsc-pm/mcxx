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

#include "cxx-cexpr.h"
#include "cxx-codegen.h"
#include "tl-edge.hpp"
#include "tl-node.hpp"

namespace TL {
namespace Analysis {

    Edge::Edge(Node *source, Node *target, bool is_task_edge_, EdgeType type,
               NBase label, bool is_back_edge_)
        : _source(source), _target(target), _type(type),
          _label(label), _is_task_edge(is_task_edge_), _is_back_edge(is_back_edge_)
    {}

    Node* Edge::get_source() const
    {
        return _source;
    }

    void Edge::set_type(EdgeType type)
    {
        set_data(_EDGE_TYPE, type);
    }
    
    Node* Edge::get_target() const
    {
        return _target;
    }

    EdgeType Edge::get_type() const
    {
        return _type;
    }

    bool Edge::is_task_edge() const
    {
        return _is_task_edge;
    }

    bool Edge::is_back_edge() const
    {
        return _is_back_edge;
    }
    
    std::string Edge::get_label_as_string()
    {
        switch (_type)
        {
            case __Always:
            case __Catch:
            case __GotoEdge:    {   if(!_label.is_null())
                                    {
                                        char is_null_ended = 0;
                                        if(_label.is<Nodecl::StringLiteral>())       // avoid printing "\"...\""
                                            return std::string(const_value_string_unpack_to_string(_label.get_constant(), &is_null_ended));
                                        else
                                            return _label.prettyprint();
                                    }
                                    break;
                                }
            case __Case:        return (_label.is_null() ? "default" : _label.prettyprint());
            case __FalseEdge:   return "FALSE";
            case __TrueEdge:    return "TRUE";
            default:            WARNING_MESSAGE("Unexpected type '%d'\n", _type);
        };
        return "";
    }

    const NBase& Edge::get_label() const
    {
        return _label;
    }
    
    void Edge::add_label(const NBase& label)
    {
        if (!_label.is_null())
        {
            const NBase& new_label = Nodecl::BitwiseAnd::make(_label.shallow_copy(), label, _label.get_type());
            _label.replace(new_label);
        }
        else
        {
            _label = label;
        }
    }
    
    void Edge::set_label(const NBase& label)
    {
        _label = label;
    }

    const char* Edge::get_sync_kind_as_string()
    {
        SyncKind kind = get_sync_kind();
        switch(kind)
        {
            #undef SYNC_KIND
            #define SYNC_KIND(X) case __##X : return #X;
            SYNC_KIND_LIST
            #undef SYNC_KIND
            default: WARNING_MESSAGE("Unexpected kind of synchronization edge '%d'", kind);
        }
        return "";
    }

    SyncKind Edge::get_sync_kind()
    {
        return get_data<SyncKind>(_SYNC_KIND);
    }

    void Edge::set_sync_kind(SyncKind kind)
    {
        set_data(_SYNC_KIND, kind);
    }

    NBase Edge::get_condition()
    {
        ERROR_CONDITION((!_source->is_omp_task_node()
                            && !_source->is_omp_sync_target_node()
                            && !_source->is_omp_async_target_node())
                        || (!_target->is_omp_task_node()
                            && !_source->is_omp_sync_target_node()
                            && !_target->is_omp_async_target_node()
                            && !_target->is_omp_taskwait_node()
                            && !_target->is_omp_barrier_graph_node()),
                        "Only edges between tasks or targets and synchronization points "
                        "(tasks, targets, taskwaits or barriers)"
                        "can have a condition. Edge between %d and %d does not fulfill.\n",
                        _source->get_id(), _target->get_id());
        NBase cond;
        if (has_key(_CONDITION))
            cond = get_data<NBase>(_CONDITION);
        return cond;
    }
    
    void Edge::set_condition(const NBase& condition)
    {
        set_data(_CONDITION, condition);
    }
    
    void Edge::set_true_edge()
    {
        _type = __TrueEdge;
    }

    void Edge::set_false_edge()
    {
        _type = __FalseEdge;
    }

    void Edge::set_catch_edge()
    {
        _type = __Catch;
    }

    bool Edge::is_always_edge()
    {
        return (_type == __Always);
    }

    bool Edge::is_case_edge()
    {
        return (_type == __Case);
    }

    bool Edge::is_catch_edge()
    {
        return (_type == __Catch);
    }

    bool Edge::is_false_edge()
    {
        return (_type == __FalseEdge);
    }

    bool Edge::is_goto_edge()
    {
        return (_type == __GotoEdge);
    }

    bool Edge::is_true_edge()
    {
        return (_type == __TrueEdge);
    }


    // ****************************************************************************** //
    // **************** Getters and setters for constants analysis ****************** //

    void Edge::set_executable(bool value)
    {
        set_data(_IS_EXECUTABLE, value);
    }

    bool Edge::is_executable()
    {
        if (has_key(_IS_EXECUTABLE))
        {
            return get_data<bool>(_IS_EXECUTABLE);
        }
        else
        {
            internal_error("Requesting execution information in edge '%d->%d', \
                            which does not contain this info", _source->get_id(), _target->get_id());
        }
    }

    // ************** END getters and setters for constants analysis **************** //
    // ****************************************************************************** //
}
}
