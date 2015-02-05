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

//     #define RANGES_DEBUG
    
    // *********************************************** //
    // ****************** CG Nodes ******************* //
    
namespace {
    unsigned int cgnode_id = 0;
}

    CGNode::CGNode(CGOpType type, const NBase& constraint)
        : _id(++cgnode_id), _type(type), 
          _constraint(constraint), _valuation(), 
          _entries(), _exits()
    {}
    
    unsigned int CGNode::get_id() const
    { 
        return _id;
    }

    CGOpType CGNode::get_type() const
    {
        return _type;
    }
    
    std::string CGNode::get_type_as_string() const
    {
        return get_op_type_as_string(_type);
    }
    
    NBase CGNode::get_constraint() const
    { 
        return _constraint; 
    }
    
    NBase CGNode::get_valuation() const
    { 
        return _valuation;
    }
    
    void CGNode::set_valuation(const NBase& valuation)
    {
        _valuation = valuation;
    }
    
    ObjectList<CGEdge*> CGNode::get_entries() const
    {
        return _entries;
    }
    
    ObjectList<CGNode*> CGNode::get_parents()
    {
        ObjectList<CGNode*> parents;
        for(ObjectList<CGEdge*>::iterator it = _entries.begin(); it != _entries.end(); ++it)
            parents.append((*it)->get_source());
        return parents;
    }
    
    void CGNode::add_entry(CGEdge* e)
    {
        _entries.insert(e);
    }
    
    ObjectList<CGEdge*> CGNode::get_exits() const
    {
        return _exits;
    }
    
    ObjectList<CGNode*> CGNode::get_children()
    {
        ObjectList<CGNode*> children;
        for(ObjectList<CGEdge*>::iterator it = _exits.begin(); it != _exits.end(); ++it)
            children.append((*it)->get_target());
        return children;
    }
    
    CGEdge* CGNode::add_child(CGNode* child,
            CGOpType edge_type,
            NBase predicate,
            bool is_back_edge)
    {
        CGEdge* e = NULL;
        ObjectList<CGNode*> children = get_children();
        if (!children.contains(child))
        {
            e = new CGEdge(this, child, edge_type, predicate, is_back_edge);
            _exits.insert(e); 
        }
        else
        {
            for (ObjectList<CGEdge*>::iterator it = _exits.begin(); it != _exits.end(); ++it)
            {
                if ((*it)->get_target() == child)
                {
                    e = *it;
                    break;
                }
            }
        }
        return e;
    }
    
    // **************** END CG Nodes ***************** //
    // *********************************************** //
    
    
    
    // *********************************************** //
    // ****************** CG Edges ******************* //
    
    CGEdge::CGEdge(CGNode* source, CGNode* target,
            CGOpType edge_type,
            const NBase& predicate,
            bool back_edge)
        : _source(source), _target(target),
          _edge_type(edge_type), _predicate(predicate), _is_back_edge(back_edge)
    {}

    CGNode* CGEdge::get_source() const
    {
        return _source;
    }

    CGNode* CGEdge::get_target() const
    {
        return _target;
    }

    CGOpType CGEdge::get_edge_type() const
    {
        return _edge_type;
    }

    NBase CGEdge::get_predicate() const
    {
        return _predicate;        
    }

    bool CGEdge::is_back_edge() const
    {
        return _is_back_edge;
    }

    std::string CGEdge::get_type_as_string() const
    {
        return get_op_type_as_string(_edge_type);
    }

    // **************** END CG Edges ***************** //
    // *********************************************** //



    // *********************************************** //
    // ********************* SCC ********************* //
 
namespace {
    unsigned int SccId = 0;
}
    
    SCC::SCC(std::map<CGNode*, SCC*>* const node_to_scc_map)
        : _nodes(), _root(NULL), _id(++SccId), _node_to_scc_map(node_to_scc_map)
    {}
    
    bool SCC::empty() const
    {
        return _nodes.empty();
    }
    
    std::vector<CGNode*> SCC::get_nodes() const
    {
        return _nodes;
    }
    
    void SCC::add_node(CGNode* n)
    {
        _nodes.push_back(n);
    }
    
    CGNode* SCC::get_root() const
    {
        return _root;
    }
    
    void SCC::set_root(CGNode* root)
    {
        _root = root;
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
        ObjectList<CGNode*> children;
        for(std::vector<CGNode*>::iterator it = _nodes.begin(); it != _nodes.end(); ++it)
        {
            children = (*it)->get_children();
            for(ObjectList<CGNode*>::iterator itt = children.begin(); itt != children.end(); ++itt)
            {
                SCC* scc = (*_node_to_scc_map)[*itt];
                if(scc != this)
                    res.append(scc);
            }
        }
        return res;
    }

    void SCC::find_path_and_direction(
            const CGNode* const source, 
            const CGNode* target, 
            Utils::CycleDirection& dir, 
            NBase& value, 
            std::set<const CGNode*>& visited)
    {
        if ((target==source) || (visited.find(target)!=visited.end()))
            return;
        
        const ObjectList<CGEdge*> exits = target->get_exits();
        ObjectList<NBase> new_values;
        for (ObjectList<CGEdge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
        {
            CGNode* new_target = (*it)->get_target();
            if((*_node_to_scc_map)[new_target] != this)
                continue;
            
            Utils::CycleDirection new_dir = dir;
            NBase new_value = value;
            const NBase predicate = (*it)->get_predicate();
            CGOpType edge_type = (*it)->get_edge_type();
            if (edge_type == __Sub || edge_type == __Div)
            {
                switch(dir._cycle_direction)
                {
                    case Utils::CycleDirection::NONE:
                        new_dir = Utils::CycleDirection::NEGATIVE;
                        new_value = predicate;
                        break;
                    case Utils::CycleDirection::NEGATIVE:
                        if(value.is_constant() && predicate.is_constant())
                        {
                            new_value = NBase(const_value_to_nodecl(const_value_sub(value.get_constant(), predicate.get_constant())));
                        }
                        else
                        {
                            new_value = Nodecl::Add::make(value.shallow_copy(), predicate.shallow_copy(), value.get_type());
                        }
                        break;
                    case Utils::CycleDirection::POSITIVE:  // The cycle may change direction!
                        if(value.is_constant() && predicate.is_constant())
                        {
                            new_value = NBase(const_value_to_nodecl(const_value_sub(value.get_constant(), predicate.get_constant())));
                            if(const_value_is_positive(new_value.get_constant()))
                            {
                                new_dir = Utils::CycleDirection::POSITIVE;
                            }
                            else if(const_value_is_zero(new_value.get_constant()))
                            {
                                new_value = NBase::null();
                                new_dir = Utils::CycleDirection::NONE;
                            }
                        }
                        else
                        {
                            internal_error("Impossible to evaluate the direction of the cycle starting in node %d\n" 
                                           "because sometimes increases and sometimes decreases with non-constant values", 
                                           source->get_id());
                        }
                }
            }
            if (edge_type == __Add || edge_type == __Mul)
            {
                switch(dir._cycle_direction)
                {
                    case Utils::CycleDirection::NONE:
                    {
                        new_dir = Utils::CycleDirection::POSITIVE;
                        new_value = predicate;
                        break;
                    }
                    case Utils::CycleDirection::NEGATIVE:   // The cycle may change direction!
                    {
                        if(value.is_constant() && predicate.is_constant())
                        {
                            new_value = NBase(const_value_to_nodecl(const_value_add(value.get_constant(), predicate.get_constant())));
                            if(const_value_is_positive(new_value.get_constant()))
                            {
                                new_dir = Utils::CycleDirection::POSITIVE;
                            }
                            else if(const_value_is_zero(new_value.get_constant()))
                            {
                                new_value = NBase::null();
                                new_dir = Utils::CycleDirection::NONE;
                            }
                        }
                        else
                        {
                            internal_error("Impossible to evaluate the direction of the cycle starting in node %d\n" 
                                           "because sometimes increases and sometimes decreases with non-constant values", 
                                           source->get_id());
                        }
                        break;
                    }
                    case Utils::CycleDirection::POSITIVE:  
                    {
                        if(value.is_constant() && predicate.is_constant())
                        {
                            new_value = NBase(const_value_to_nodecl(const_value_add(value.get_constant(), predicate.get_constant())));
                        }
                        else
                        {
                            new_value = Nodecl::Add::make(value.shallow_copy(), predicate.shallow_copy(), value.get_type());
                        }
                    }
                }
            }
            
            // Keep iterating 
            if(exits.empty())
            {   // Avoid copying the set of visited nodes one a unique branch exits the current target
                // because we do not need to keep track of different paths here
                find_path_and_direction(source, new_target, new_dir, new_value, visited);
            }
            else
            {
                std::set<const CGNode*> new_visited(visited);
                new_visited.insert(target);
                find_path_and_direction(source, new_target, new_dir, new_value, new_visited);
            }
            
            new_values.append(new_value);
            dir = dir | new_dir;
        }
    }

    Utils::CycleDirection SCC::get_cycle_direction(const CGEdge* const edge)
    {
        Utils::CycleDirection dir = Utils::CycleDirection::NONE;
        CGNode* source = edge->get_source();
        CGNode* target = edge->get_target();
        NBase n;
        std::set<const CGNode*> visited;
        find_path_and_direction(source, target, dir, n, visited);
        
        return dir;
    }
    
    // ******************* END SCC ******************* //
    // *********************************************** //
    
    
    
    // *********************************************** //
    // **************** Utils methods **************** //
    
    NBase join_valuations(NBase (*join_function)(const NBase&, const NBase&), 
                          const ObjectList<NBase>& valuations)
    {
        ObjectList<NBase>::const_iterator it = valuations.begin();
        NBase next_valuation = *it;
        if(valuations.size() > 1)
        {
            ++it;
            while(it != valuations.end())
            {
                next_valuation = join_function(next_valuation, *it);
                ++it;
            }
        }
        return next_valuation;
    }
    
    // ************** END Utils methods ************** //
    // *********************************************** //
    
    
    
    // *********************************************** //
    // ***************** I/O methods ***************** //
    
    inline std::string print_constraint_kind(ConstraintKind c_kind)
    {
        switch(c_kind)
        {
            #undef CONSTRAINT_KIND
            #define CONSTRAINT_KIND(X) case __##X : return #X;
            CONSTRAINT_KIND_LIST
            #undef CONSTRAINT_KIND
            default: WARNING_MESSAGE("Unexpected type of node '%d'", c_kind);
        }
        return "";
    }

    void print_constraint(ConstraintKind c_kind, const Symbol& s, const NBase& val, const Type& t)
    {
#ifdef RANGES_DEBUG
        std::cerr << "    " << print_constraint_kind(c_kind) << " Constraint "
                  << s.get_name() << " = " << val.prettyprint()
                  << " (" << t.print_declarator() << ")" << std::endl;
#endif
    }
    
    void print_sccs(const std::vector<SCC*>& scc_list)
    {
        #ifdef RANGES_DEBUG
        std::cerr << "STRONGLY CONNECTED COMPONENTS" << std::endl;
        for(std::vector<SCC*>::const_iterator it = scc_list.begin(); it != scc_list.end(); ++it)
        {
            SCC* scc = *it;
            std::vector<CGNode*> nodes = scc->get_nodes();
            std::cerr << "    SCC " << (*it)->get_id() << ": ";
            for(std::vector<CGNode*>::iterator itt = nodes.begin(); itt != nodes.end(); )
            {
                std::cerr << (*itt)->get_id();
                ++itt;
                if(itt != nodes.end())
                    std::cerr << ", ";
            }
            std::cerr << " (root: " << scc->get_root()->get_id() << ")" << std::endl;
        }
        #endif
    }
    
    // *************** END I/O methods *************** //
    // *********************************************** //

}
}
