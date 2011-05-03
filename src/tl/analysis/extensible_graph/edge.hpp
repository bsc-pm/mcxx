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


#ifndef EDGE_HPP
#define EDGE_HPP

#include "node.hpp"
#include "structures.hpp"
#include "tl-builtin.hpp"

namespace TL
{
    class Node;
    
    /*!
      This class represents an edge within a graph that connects two nodes
     */
    class LIBTL_CLASS Edge : public LinkData {
        private:
            Node* _source;
            Node* _target;
            
        public:
            // *** Constructors *** //
            
            //! Edge Constructor
            /*!
              A new edge connecting to nodes is built.
              This method does not modify the information of source and target nodes.
              \param source Pointer to the source node of the new edge
              \param target Pointer to the target node of the new edge
              \param type Type of the new edge, belonging to the enum Edge_type
              \param label Additional argument, when the edge will not be always taken in the graph
                           flow. It indicates the condition of the edge.
             */
            Edge(Node *source, Node *target, Edge_type type,
                 std::string label="");
            
            
            // *** Getters and Setters *** //
            
            //! Returns a pointer the the source node of the edge
            Node* get_source() const;
            
            //! Returns a pointer the the target node of the edge
            Node* get_target() const;
            
            //! Returns the type of the edge
            Edge_type get_type();
            
            //! Returns the label of the edge. 
            /*!
              \return When the label is empty, meaning the edge is always taken, an empty string
                      is returned.
              */
            std::string get_label();
    };
}

#endif // EDGE_HPP