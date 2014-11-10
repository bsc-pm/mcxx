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
    // ***************** Constraints ***************** //

    #define CONSTRAINT_KIND_LIST \
    CONSTRAINT_KIND(BackEdge) \
    CONSTRAINT_KIND(BinaryOp) \
    CONSTRAINT_KIND(Comparator) \
    CONSTRAINT_KIND(ComparatorTrue) \
    CONSTRAINT_KIND(ComparatorFalse) \
    CONSTRAINT_KIND(Function) \
    CONSTRAINT_KIND(GlobalVar) \
    CONSTRAINT_KIND(Mod) \
    CONSTRAINT_KIND(ModTrue) \
    CONSTRAINT_KIND(ModFalse) \
    CONSTRAINT_KIND(Parameter) \
    CONSTRAINT_KIND(Propagated) \
    CONSTRAINT_KIND(Replace) \
    CONSTRAINT_KIND(UnaryOp)

    enum ConstraintKind {
        #undef CONSTRAINT_KIND
        #define CONSTRAINT_KIND(X) __##X,
        CONSTRAINT_KIND_LIST
        #undef CONSTRAINT_KIND
    };

    #define CG_OP_TYPE_LIST \
    CG_OP_TYPE(Add) \
    CG_OP_TYPE(Div) \
    CG_OP_TYPE(Flow) \
    CG_OP_TYPE(Intersection) \
    CG_OP_TYPE(Mul) \
    CG_OP_TYPE(Phi) \
    CG_OP_TYPE(Sub) \
    CG_OP_TYPE(Sym)

    enum CGOpType {
        #undef CG_OP_TYPE
        #define CG_OP_TYPE(X) __##X,
        CG_OP_TYPE_LIST
        #undef CG_OP_TYPE
    };

    inline std::string get_op_type_as_string(CGOpType op_type)
    {
        switch(op_type)
        {
            #undef CG_OP_TYPE
            #define CG_OP_TYPE(X) case __##X : return #X;
            CG_OP_TYPE_LIST
            #undef CG_OP_TYPE
            default: WARNING_MESSAGE("Unexpected type of op '%d' in constraint.\n", op_type);
        }
        return "";
    }

    // *************** END Constraints *************** //
    // *********************************************** //



    // *********************************************** //
    // ****************** CG Edges ******************* //

    class CGNode;

    class LIBTL_CLASS CGEdge
    {
    private:
        // *** Members *** //
        CGNode* _source;
        CGNode* _target;
        CGOpType _edge_type;
        NBase _predicate;
        bool _is_back_edge;

    public:
        // *** Constructor *** //
        CGEdge(CGNode* source,
               CGNode* target,
               CGOpType edge_type,
               const NBase& predicate,
               bool back_edge);

        // *** Getters and setters *** //
        CGNode* get_source() const;
        CGNode* get_target() const;
        CGOpType get_edge_type() const;
        std::string get_type_as_string() const;
        NBase get_predicate() const;
        bool is_back_edge() const;
    };

    // **************** END CG Edges ***************** //
    // *********************************************** //



    // *********************************************** //
    // ****************** CG Nodes ******************* //

    class LIBTL_CLASS CGNode
    {
    private:
        // *** Members *** //
        unsigned int _id;
        CGOpType _type;
        NBase _constraint;
        NBase _valuation;
        ObjectList<CGEdge*> _entries;
        ObjectList<CGEdge*> _exits;
        
    public:    
        // *** Constructor *** //
        CGNode(CGOpType type, const NBase& constraint=NBase::null());
        
        // *** Getters and setters *** //
        unsigned int get_id() const;
        CGOpType get_type() const;
        std::string get_type_as_string() const;
        
        NBase get_constraint() const;
        NBase get_valuation() const;
        void set_valuation(const NBase& valuation);
        
        ObjectList<CGEdge*> get_entries() const;
        ObjectList<CGNode*> get_parents();
        void add_entry(CGEdge* e);
        
        ObjectList<CGEdge*> get_exits() const;
        ObjectList<CGNode*> get_children();
        CGEdge* add_child(CGNode* child,
                CGOpType edge_type = __Flow,
                NBase predicate = NBase::null(),
                bool is_back_edge = false);
    };
    
    // **************** END CG Nodes ***************** //
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
    
    void print_constraint(ConstraintKind c_kind, const Symbol& s, const NBase& val, const Type& t);
    
    void print_sccs(const std::vector<SCC*>& scc_list);
    
    // *************** END I/O methods *************** //
    // *********************************************** //
    
}
}
