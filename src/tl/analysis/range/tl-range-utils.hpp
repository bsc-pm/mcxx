/*--------------------------------------------------------------------
(C) Copyright 2006-2013 Barcelona Supercomputing Center             *
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

#include "tl-ranges-common.hpp"

namespace TL {
namespace Analysis {    
    
    // *********************************************** //
    // ****************** CG Nodes ******************* //
    class CGEdge;
    
    #define CGNODE_TYPE_LIST \
    CGNODE_TYPE(CG_Sym) \
    CGNODE_TYPE(CG_Phi) \
    CGNODE_TYPE(CG_Add) \
    CGNODE_TYPE(CG_Sub)
    
    enum CGNodeType {
        #undef CGNODE_TYPE
        #define CGNODE_TYPE(X) __##X,
        CGNODE_TYPE_LIST
        #undef CGNODE_TYPE
    };
    
    class LIBTL_CLASS CGNode
    {
    private:
        // *** Members *** //
        unsigned int _id;
        CGNodeType _type;
        NBase _constraint;
        NBase _valuation;
        ObjectList<CGEdge*> _entries;
        ObjectList<CGEdge*> _exits;
        
    public:    
        // *** Constructor *** //
        CGNode(CGNodeType type, const NBase& constraint=NBase::null());
        
        // *** Getters and setters *** //
        unsigned int get_id() const;
        CGNodeType get_type() const;
        std::string get_type_as_str() const;
        
        NBase get_constraint() const;
        NBase get_valuation() const;
        void set_valuation(const NBase& valuation);
        
        ObjectList<CGEdge*> get_entries() const;
        ObjectList<CGNode*> get_parents();
        void add_entry(CGEdge* e);
        
        ObjectList<CGEdge*> get_exits() const;
        ObjectList<CGNode*> get_children();
        CGEdge* add_child(CGNode* child, bool is_back_edge, NBase predicate = NBase::null());
    };
    
    // **************** END CG Nodes ***************** //
    // *********************************************** //
    
    
    
    // *********************************************** //
    // ****************** CG Edges ******************* //
    
    class LIBTL_CLASS CGEdge
    {
    private:
        // *** Members *** //
        CGNode* _source;
        CGNode* _target;
        bool _is_back_edge;
        NBase _predicate;
        
    public:
        // *** Constructor *** //
        CGEdge(CGNode* source, CGNode* target, bool is_back, const NBase& predicate);
        
        // *** Getters and setters *** //
        CGNode* get_source() const;
        CGNode* get_target() const;
        bool is_back_edge() const;
        NBase get_predicate() const;
    };
    
    // **************** END CG Edges ***************** //
    // *********************************************** //
    
    
    
    // *********************************************** //
    // ********************* SCC ********************* //
    
    class LIBTL_CLASS SCC
    {
    private:
        // *** Members *** //
        std::vector<CGNode*> _nodes;
        CGNode* _root;
        unsigned int _id;
        std::map<CGNode*, SCC*>* const _node_to_scc_map;
        
        // *** Private members *** //
        void find_path_and_direction(
                const CGNode* const source, 
                const CGNode* target, 
                Utils::CycleDirection& dir, 
                NBase& value, 
                std::set<const CGNode*>& visited);
        
    public:
        // *** Constructor *** //
        SCC(std::map<CGNode*, SCC*>* const node_to_scc_map);
        
        // *** Getters and setters *** //
        bool empty() const;
        std::vector<CGNode*> get_nodes() const;
        void add_node(CGNode* n);
        CGNode* get_root() const;
        void set_root(CGNode* root);
        unsigned int get_id() const;
        
        // *** Consultants *** //
        bool is_trivial() const;
        ObjectList<SCC*> get_scc_exits();
        Utils::CycleDirection get_cycle_direction(const CGEdge* const edge);
    };
    
    // ******************* END SCC ******************* //
    // *********************************************** //
    
    
    
    // *********************************************** //
    // **************** Utils methods **************** //
    
    NBase join_valuations(NBase (*join_function)(const NBase&, const NBase&), 
                          const ObjectList<NBase>& valuations);
    
    // ************** END Utils methods ************** //
    // *********************************************** //
    
    
    
    // *********************************************** //
    // ***************** I/O methods ***************** //
    
    void print_constraint(std::string stmt_name, const Symbol& s, const NBase& val, const Type& t);
    
    void print_sccs(const std::vector<SCC*>& scc_list);
    
    // *************** END I/O methods *************** //
    // *********************************************** //
    
}
}
