/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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


#include "tl-edge.hpp"

namespace TL
{
    Edge::Edge(Node *source, Node *target, Edge_type type, std::string label)
        : _source(source), _target(target)
    {
        set_data<Edge_type>(_EDGE_TYPE, type);
        set_data<std::string>(_EDGE_LABEL, label);
    }

    Node* Edge::get_source() const
    {
        return _source;
    }

    Node* Edge::get_target() const
    {
        return _target;
    }

    Edge_type Edge::get_type()
    {
        if (has_key(_EDGE_TYPE))
        {    
            return get_data<Edge_type>(_EDGE_TYPE);
        }
        else
        {    
            return UNCLASSIFIED_EDGE;
        }
    }

    std::string Edge::get_type_as_string()
    {
        std::string result = "";
        
        if (has_key(_EDGE_TYPE))
        {
            Edge_type etype = get_data<Edge_type>((const std::string) _EDGE_TYPE);
            
            switch(etype)
            {
                case TRUE_EDGE:     result = "TRUE_EDGE";
                break;
                case FALSE_EDGE:    result = "FALSE_EDGE";
                break;
                case ALWAYS_EDGE:   result = "ALWAYS_EDGE";
                break;
                case CASE_EDGE:     result = "CASE_EDGE";
                break;
                case CATCH_EDGE:    result = "CATCH_EDGE";
                break;
                case GOTO_EDGE:     result = "GOTO_EDGE";
                break;
                case TASK_EDGE:     result = "TASK_EDGE";
                break;
                case UNCLASSIFIED_EDGE: result = "UNCLASSIFIED_EDGE";
                break;
                default:            std::cerr << " ** Edge.cpp :: get_label() ** "
                                   << "warning: Unexpected type '" << etype << "' while getting "
                                   << "the Edge type as a string" << std::endl;
            }
        }
        
        return result;
    }

    std::string Edge::get_label()
    {
        std::string label = "";
            
        if (has_key(_EDGE_TYPE) && 
            get_data<Edge_type>(_EDGE_TYPE) != UNCLASSIFIED_EDGE)
        {
            Edge_type etype = get_data<Edge_type>((const std::string) _EDGE_TYPE);
            switch (etype)
            {
                case TRUE_EDGE:     label = "True";
                break;
                case FALSE_EDGE:    label = "False";
                break;
                case TASK_EDGE:
                case ALWAYS_EDGE:   label = "";
                break;
                case CASE_EDGE:     {
                                        ObjectList<Nodecl::NodeclBase> labels = get_data<ObjectList<Nodecl::NodeclBase> >(_EDGE_LABEL);
                                        if (labels[0].is_null())
                                            label = "default";
                                        else
                                            label = labels[0].get_symbol().get_name();
                                        int i = 1;
                                        while (i<labels.size())
                                        {
                                            if (labels[i].is_null())
                                                label += ", default";
                                            else
                                                label += ", " + labels[i].get_symbol().get_name();                                         
 
                                            ++i;
                                        }
                                    }
                break;
                case CATCH_EDGE:    {
                                        ObjectList<Nodecl::NodeclBase> labels = get_data<ObjectList<Nodecl::NodeclBase> >(_EDGE_LABEL);
                                        if (labels[0].is_null())
                                            label = "...";
                                        else
                                            label = labels[0].get_symbol().get_name();
                                    }
                break;
                case GOTO_EDGE:     label = get_data<std::string>(_EDGE_LABEL);
                break;
                default: std::cerr << " ** Edge.cpp :: get_label() ** "
                                   << "warning: Unexpected type '" << etype << "' while getting "
                                   << "the Edge label" << std::endl;
            };
        }
        
        return label;
    }
}