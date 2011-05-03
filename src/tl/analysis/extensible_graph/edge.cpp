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


#include "edge.hpp"

namespace TL
{
    Edge::Edge(Node *source, Node *target, Edge_type type, std::string label)
        : _source(source), _target(target)
    {
        set_data<Edge_type>("type", type);
        set_data<std::string>("label", label);
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
        if (has_key("type"))
        {    
            return get_data<Edge_type>("type");
        }
        else
        {    
            return UNCLASSIFIED_EDGE;
        }
    }

    std::string Edge::get_label()
    {
        std::string label = "";
        
        if (has_key("type") && 
            get_data<Edge_type>("type") != UNCLASSIFIED_EDGE)
        {
            Edge_type etype = get_data<Edge_type>((const std::string) "type");
            switch (etype)
            {
                case TRUE_EDGE:     label = "True";
                break;
                case FALSE_EDGE:    label = "False";
                break;
                case ALWAYS_EDGE:   label = "";
                break;
                case CASE_EDGE:
                case CATCH_EDGE:    label = get_data<std::string>(std::string("label"));
                break;
                default: std::cerr << " ** Edge.cpp :: get_label() ** "
                                   << "warning: Unexpected type '" << etype << "' while getting "
                                   << "the Edge label" << std::endl;
            };
        }
        
        return label;
    }
}