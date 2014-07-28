/*--------------------------------------------------------------------
 ( *C) Copyright 2006-2013 Barcelona Supercomputing Center             *
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

    // *********************************************** //
    // ****************** CG Nodes ******************* //
    
namespace {
    unsigned int cgnode_id = 0;
}
    
    CGNode::CGNode(CGNode_type type, const NBase& constraint)
        : _id(++cgnode_id), _type(type), 
          _constraint(constraint), _valuation(), 
          _entries(), _exits()
    {}
    
    unsigned int CGNode::get_id() const
    { 
        return _id;
    }
    
    CGNode_type CGNode::get_type() const
    {
        return _type;
    }
    
    std::string CGNode::get_type_as_str() const
    {
        switch(_type)
        {
            #undef CGNODE_TYPE
            #define CGNODE_TYPE(X) case __##X : return #X;
            CGNODE_TYPE_LIST
            #undef CGNODE_TYPE
            default: WARNING_MESSAGE("Unexpected type of node '%d'", _type);
        }
        return "";
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
    
    CGEdge* CGNode::add_child(CGNode* child, bool is_back_edge, NBase predicate)
    {
        CGEdge* e;
        ObjectList<CGNode*> children = get_children();
        if(!children.contains(child))
        {
            e = new CGEdge(this, child, is_back_edge, predicate);
            _exits.insert(e); 
        }
        else
        {
            for(ObjectList<CGEdge*>::iterator it = _exits.begin(); it != _exits.end(); ++it)
            {
                if((*it)->get_target() == child)
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
    
    CGEdge::CGEdge(CGNode* source, CGNode* target, bool is_back, const NBase& predicate)
        : _source(source), _target(target), _is_back_edge(is_back), _predicate(predicate)
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
    
    NBase CGEdge::get_predicate() const
    {
        return _predicate;        
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
    
    void SCC::find_path_and_direction(const CGNode* const source, const CGNode* const target, 
                                      Utils::CycleDirection& dir, NBase& value)
    {
        if(target==source)
            return;
        
        const ObjectList<CGEdge*> exits = target->get_exits();
        ObjectList<NBase> new_values;
        for(ObjectList<CGEdge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
        {
            CGNode* new_target = (*it)->get_target();
            if((*_node_to_scc_map)[new_target] != this)
                continue;
            
            Utils::CycleDirection new_dir = dir;
            NBase new_value = value;
            const NBase predicate = (*it)->get_predicate();
            if(predicate.is<Nodecl::Neg>())
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
            else if(predicate.is<Nodecl::Plus>())
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
            find_path_and_direction(source, new_target, new_dir, new_value);
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
        
        find_path_and_direction(source, target, dir, n);
        
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
    
    void print_constraint(std::string stmt_name, const Symbol& s, const NBase& val, const Type& t)
    {
        #ifdef RANGES_DEBUG
        std::cerr << "    " << stmt_name << " Constraint " << s.get_name() << " = " << val.prettyprint() 
                  << " (" << t.print_declarator() << ")" << std::endl;
        #endif
    }
    
    void print_sccs(const std::vector<SCC*>& scc_list)
    {
        #ifdef RANGES_DEBUG
        std::cerr << "STRONGLY CONNECTED COMPONENTS" << std::endl;
        for(std::vector<SCC*>::iterator it = scc_list.begin(); it != scc_list.end(); ++it)
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