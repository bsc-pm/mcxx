/*--------------------------------------------------------------------
 ( *C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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
#include "tl-range-utils.hpp"

namespace TL {
namespace Analysis {

    static unsigned int node_last_id = 0;
    static unsigned int scc_last_id = 0;

    // ************************************************************* //
    // ****************** Constraint Graph Nodes ******************* //

    CGNode::CGNode(CGNodeType type, const NBase& constraint)
        : _id(++node_last_id), _type(type),
          _constraint(constraint), _valuation(),
          _entries(), _exits()
    {}

    unsigned int CGNode::get_id() const
    { 
        return _id;
    }

    CGNodeType CGNode::get_type() const
    {
        return _type;
    }

    std::string CGNode::get_type_as_string() const
    {
        return get_node_type_as_string(_type);
    }

    const NBase& CGNode::get_constraint() const
    { 
        return _constraint; 
    }

    void CGNode::set_constraint(const NBase& constraint)
    {
        _constraint = constraint;
    }

    const NBase& CGNode::get_valuation() const
    { 
        return _valuation;
    }

    void CGNode::set_valuation(const NBase& valuation)
    {
        _valuation = valuation;
    }

    ObjectList<CGEdge*>& CGNode::get_entries()
    {
        return _entries;
    }

    ObjectList<CGNode*> CGNode::get_parents()
    {
        ObjectList<CGNode*> parents;
        for (ObjectList<CGEdge*>::iterator it = _entries.begin(); it != _entries.end(); ++it)
            parents.append((*it)->get_source());
        return parents;
    }

    void CGNode::add_entry(CGEdge* e)
    {
        _entries.append(e);
    }

    void CGNode::remove_entry(CGNode* source)
    {
        ObjectList<CGEdge*>::iterator it;
        for (it = _entries.begin(); it != _entries.end(); ++it)
        {
            if ((*it)->get_source() == source)
            {
                _entries.erase(it);
                --it;   // Decrement to allow the correctness of the comparison outside the loop
                break;
            }
        }
        if (it == _entries.end())
        {
            internal_error("Trying to delete an non-existent edge between nodes '%d' and '%d'",
                           source->get_id(), _id);
        }
    }

    const std::set<CGEdge*>& CGNode::get_exits() const
    {
        return _exits;
    }

    std::set<CGNode*> CGNode::get_children()
    {
        std::set<CGNode*> children;
        for (std::set<CGEdge*>::iterator it = _exits.begin(); it != _exits.end(); ++it)
            children.insert((*it)->get_target());
        return children;
    }

    CGEdge* CGNode::add_child(
            CGNode* child,
            bool is_back_edge,
            bool is_future_edge)
    {
        CGEdge* e = NULL;
        std::set<CGNode*> children = get_children();
        if (children.find(child) == children.end())
        {   // If the node is not there, insert it
            e = new CGEdge(this, child, is_back_edge, is_future_edge);
            _exits.insert(e);
        }
        else
        {   // If the node is already there, look for the edge connecting it
            for (std::set<CGEdge*>::iterator it = _exits.begin(); it != _exits.end(); ++it)
                if ((*it)->get_target() == child)
                    e = *it;
        }
        return e;
    }

    void CGNode::remove_exit(CGEdge* e)
    {
        int n_erased = _exits.erase(e);

        if (n_erased == 0)
        {
            CGNode* target = e->get_target();
            internal_error("Trying to delete an non-existent edge between nodes '%d' and '%d'",
                           _id, target->get_id());
        }
    }

    // **************** END Constraint Graph Nodes ***************** //
    // ************************************************************* //



    // ************************************************************* //
    // ****************** Constraint Graph Edges ******************* //

    CGEdge::CGEdge(CGNode* source,
                   CGNode* target,
                   bool back_edge,
                   bool future_edge)
        : _source(source), _target(target),
          _is_back_edge(back_edge), _is_future_edge(future_edge)
    {}

    CGNode* CGEdge::get_source() const
    {
        return _source;
    }

    CGNode* CGEdge::get_target() const
    {
        return _target;
    }

    bool CGEdge::is_back_edge() const
    {
        return _is_back_edge;
    }

    bool CGEdge::is_future_edge() const
    {
        return _is_future_edge;
    }

    // **************** END Constraint Graph Edges ***************** //
    // ************************************************************* //



    // *********************************************** //
    // ********************* SCC ********************* //

    SCC::SCC(std::map<CGNode*, SCC*>* const node_to_scc_map)
        : _nodes(), _roots(), _id(++scc_last_id), _node_to_scc_map(node_to_scc_map)
    {}

    bool SCC::empty() const
    {
        return _nodes.empty();
    }

    const std::vector<CGNode*>& SCC::get_nodes() const
    {
        return _nodes;
    }

    void SCC::add_node(CGNode* n)
    {
        _nodes.push_back(n);
    }

    const std::list<CGNode*>& SCC::get_roots() const
    {
        return _roots;
    }

    void SCC::add_root(CGNode* root)
    {
        _roots.push_back(root);
    }

    unsigned int SCC::get_id() const
    {
        return _id;
    }

    bool SCC::is_trivial() const
    {
        return (_nodes.size() == 1);
    }

    ObjectList<SCC*> SCC::get_scc_exits()
    {
        ObjectList<SCC*> res;
        for (std::vector<CGNode*>::iterator it = _nodes.begin(); it != _nodes.end(); ++it)
        {
            const std::set<CGNode*>& children = (*it)->get_children();
            for(std::set<CGNode*>::const_iterator itt = children.begin(); itt != children.end(); ++itt)
            {
                SCC* scc = (*_node_to_scc_map)[*itt];
                if (scc != this)
                    res.append(scc);
            }
        }
        return res;
    }

    // ******************* END SCC ******************* //
    // *********************************************** //



    // *********************************************** //
    // ***************** I/O methods ***************** //

    void print_sccs(const std::vector<SCC*>& scc_list)
    {
        if (RANGES_DEBUG)
        {
            std::cerr << "STRONGLY CONNECTED COMPONENTS" << std::endl;
            for(std::vector<SCC*>::const_iterator it = scc_list.begin(); it != scc_list.end(); ++it)
            {
                SCC* scc = *it;
                std::cerr << "    SCC " << (*it)->get_id() << ": ";

                // Print the nodes of the component
                const std::vector<CGNode*>& nodes = scc->get_nodes();
                for(std::vector<CGNode*>::const_iterator itt = nodes.begin(); itt != nodes.end(); )
                {
                    std::cerr << (*itt)->get_id();
                    ++itt;
                    if(itt != nodes.end())
                        std::cerr << ", ";
                }

                // Print the roots of the component
                const std::list<CGNode*>& roots = scc->get_roots();
                if (roots.size() == 1)
                    std::cerr << " (root: ";
                else
                    std::cerr << " (roots: ";
                for (std::list<CGNode*>::const_iterator itr = roots.begin(); itr != roots.end(); )
                {
                    std::cerr << (*itr)->get_id();
                    ++itr;
                    if (itr != roots.end())
                        std::cerr << ", ";
                }
                std::cerr << ")" << std::endl;
            }
        }
    }
    
    // *************** END I/O methods *************** //
    // *********************************************** //

    void reset_ids()
    {
        node_last_id = 0;
        scc_last_id = 0;
    }
}
}
