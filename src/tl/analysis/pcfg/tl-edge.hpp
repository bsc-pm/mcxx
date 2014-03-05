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



#ifndef TL_EDGE_HPP
#define TL_EDGE_HPP

#include "tl-builtin.hpp"
#include "tl-pcfg-utils.hpp"

namespace TL {
namespace Analysis {

    class Node;

    //! This class represents an edge within a graph that connects two nodes
    class LIBTL_CLASS Edge : public LinkData {
    private:
        Node* _source;
        Node* _target;

    public:
        // *** Constructors *** //

        //! Edge Constructor
        /*!
        * A new edge connecting to nodes is built.
        * This method does not modify the information of source and target nodes.
        * \param source         Pointer to the source node of the new edge
        * \param target         Pointer to the target node of the new edge
        * \param is_task_edge   Boolean indicating whether the edge target is a Task
        * \param type           Type of the new edge, belonging to the enum Edge_type
        * \param label          Additional argument, when the edge will not be always taken in the graph
        *                       flow. It indicates the condition of the edge.
        */
        Edge( Node *source, Node *target, bool is_task_edge, Edge_type type, std::string label = "" );


        // *** Getters and Setters *** //

        //! Returns a pointer the the source node of the edge
        Node* get_source( ) const;

        //! Returns a pointer the the target node of the edge
        Node* get_target( ) const;

        void set_type( Edge_type type );
        
        //! Returns the type of the edge
        Edge_type get_type( );

        //! Returns a string with the type of the node
        std::string get_type_as_string( );

        //! Returns the boolean indicating whether the target of the edge is a Task
        bool is_task_edge( );

        //! Returns the label of the edge.
        /*!
        \return When the label is empty, meaning the edge is always taken, an empty string
                is returned.
        */
        std::string get_label( );
        void add_label( std::string label );
        void set_label( std::string label );
        
        void set_true_edge( );
        void set_false_edge( );
        void set_catch_edge( );

        // Getters for all edge types
        bool is_always_edge( );
        bool is_case_edge( );
        bool is_catch_edge( );
        bool is_false_edge( );
        bool is_goto_edge( );
        bool is_true_edge( );

        // ****************************************************************************** //
        // **************** Getters and setters for constants analysis ****************** //

        //! Set the attribute #executable as \value indicates
        void set_executable( bool value );

        //! Returns whether the edge is executable or not
        bool is_executable( );

        // ************** END getters and setters for constants analysis **************** //
        // ****************************************************************************** //
    };
}
}

#endif      // TL_EDGE_HPP