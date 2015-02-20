/*--------------------------------------------------------------------
(C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

#include <list>
#include "tl-ranges-common.hpp"

namespace TL {
namespace Analysis {

    // *********************************************** //
    // ***************** Constraints ***************** //

    #define CONSTRAINT_KIND_LIST        \
    CONSTRAINT_KIND(BackEdge)           \
    CONSTRAINT_KIND(BinaryOp)           \
    CONSTRAINT_KIND(Comparator)         \
    CONSTRAINT_KIND(ComparatorTrue)     \
    CONSTRAINT_KIND(ComparatorFalse)    \
    CONSTRAINT_KIND(Function)           \
    CONSTRAINT_KIND(GlobalVar)          \
    CONSTRAINT_KIND(Mod)                \
    CONSTRAINT_KIND(ModTrue)            \
    CONSTRAINT_KIND(ModFalse)           \
    CONSTRAINT_KIND(Parameter)          \
    CONSTRAINT_KIND(Propagated)         \
    CONSTRAINT_KIND(Replace)            \
    CONSTRAINT_KIND(UnaryOp)

    enum ConstraintKind {
        #undef CONSTRAINT_KIND
        #define CONSTRAINT_KIND(X) __##X,
        CONSTRAINT_KIND_LIST
        #undef CONSTRAINT_KIND
    };

    #define CG_NODE_TYPE_LIST   \
    CG_NODE_TYPE(Add)           \
    CG_NODE_TYPE(Const)         \
    CG_NODE_TYPE(Div)           \
    CG_NODE_TYPE(Intersection)  \
    CG_NODE_TYPE(Mul)           \
    CG_NODE_TYPE(Phi)           \
    CG_NODE_TYPE(Sub)           \
    CG_NODE_TYPE(Sym)

    enum CGNodeType {
        #undef CG_NODE_TYPE
        #define CG_NODE_TYPE(X) __##X,
        CG_NODE_TYPE_LIST
        #undef CG_NODE_TYPE
    };

    inline std::string get_node_type_as_string(CGNodeType type)
    {
        switch(type)
        {
            #undef CG_NODE_TYPE
            #define CG_NODE_TYPE(X) case __##X : return #X;
            CG_NODE_TYPE_LIST
            #undef CG_NODE_TYPE
            default: WARNING_MESSAGE("Unexpected type of op '%d' in constraint.\n", type);
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
        bool _is_back_edge;
        bool _is_future_edge;

    public:
        // *** Constructor *** //
        CGEdge(CGNode* source,
               CGNode* target,
               bool back_edge,
               bool future_edge);

        // *** Getters and setters *** //
        CGNode* get_source() const;
        CGNode* get_target() const;
        bool is_back_edge() const;
        bool is_future_edge() const;
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
        CGNodeType _type;
        NBase _constraint;
        NBase _valuation;
        std::set<CGEdge*> _entries;
        std::set<CGEdge*> _exits;

        static unsigned int _last_id;

    public:    
        // *** Constructor *** //
        CGNode(CGNodeType type, const NBase& constraint=NBase::null());

        // *** Getters and setters *** //
        unsigned int get_id() const;
        CGNodeType get_type() const;
        std::string get_type_as_string() const;

        const NBase& get_constraint() const;
        const NBase& get_valuation() const;
        void set_valuation(const NBase& valuation);

        const std::set<CGEdge*>& get_entries() const;
        std::set<CGNode*> get_parents();
        void add_entry(CGEdge* e);

        const std::set<CGEdge*>& get_exits() const;
        std::set<CGNode*> get_children();
        CGEdge* add_child(CGNode* child, bool is_back_edge = false,
                          bool is_future_edge = false);
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
        std::list<CGNode*> _roots;
        unsigned int _id;
        std::map<CGNode*, SCC*>* const _node_to_scc_map;

        static unsigned int _last_id;

    public:
        // *** Constructor *** //
        SCC(std::map<CGNode*, SCC*>* const node_to_scc_map);

        // *** Getters and setters *** //
        bool empty() const;
        const std::vector<CGNode*>& get_nodes() const;
        void add_node(CGNode* n);
        const std::list<CGNode*>& get_roots() const;
        void add_root(CGNode* root);
        unsigned int get_id() const;

        // *** Consultants *** //
        bool is_trivial() const;
        ObjectList<SCC*> get_scc_exits();
    };

    // ******************* END SCC ******************* //
    // *********************************************** //



    // *********************************************** //
    // ***************** I/O methods ***************** //

    void print_constraint(ConstraintKind c_kind, const Symbol& s, const NBase& val, const Type& t);

    void print_sccs(const std::vector<SCC*>& scc_list);

    // *************** END I/O methods *************** //
    // *********************************************** //

}
}
