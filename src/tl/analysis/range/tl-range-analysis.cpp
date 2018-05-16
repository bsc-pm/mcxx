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

#include <algorithm>
#include <limits.h>
#include <fstream>
#include <list>
#include <set>
#include <unistd.h>
#include <sys/stat.h>
#include <unistd.h>

#include "cxx-process.h"
#include "tl-expression-reduction.hpp"
#include "tl-range-analysis.hpp"

namespace TL {
namespace Analysis {

namespace {
    // ************************************************************************************** //
    // ***************** initialize global variables for ranges operations ****************** //

    const_value_t* zero = const_value_get_zero(/*num_bytes*/ 4, /*signed*/ 1);
    const_value_t* long_max = const_value_get_integer(LONG_MAX, /*num_bytes*/sizeof(long), /*sign*/1);
    NBase plus_inf = Nodecl::Analysis::PlusInfinity::make(Type::get_long_int_type(), long_max);
    const_value_t* long_min = const_value_get_integer(LONG_MIN, /*num_bytes*/sizeof(long), /*sign*/1);
    NBase minus_inf = Nodecl::Analysis::MinusInfinity::make(Type::get_long_int_type(), long_min);

    // ************************************************************************************** //
}


    // ***************************************************************************** //
    // ******************* Class implementing constraint graph ********************* //

    ConstraintGraph::ConstraintGraph(std::string name)
        : _name(name), _nodes(), _node_to_scc_map()
    {}

    CGNode* ConstraintGraph::get_node_from_ssa_var(const NBase& n)
    {
        CGValueToCGNode_map::iterator it = _nodes.find(n);
        ERROR_CONDITION(it == _nodes.end(),
                        "No SSA variable '%s' found in Constraint Graph '%s'",
                        n.prettyprint().c_str(), _name.c_str());
        return it->second;
    }

    CGNode* ConstraintGraph::insert_node(const NBase& value, CGNodeType type)
    {
        // If the node is a SSA symbol, there can only be one (no repetitions)
        // So, if it already existed, return it
        if (value.is<Nodecl::Symbol>()
            && (_nodes.find(value) != _nodes.end()))
            return _nodes.find(value)->second;

        // Otherwise, create the node and return it
        CGNode* node = new CGNode(type, value);
        _nodes.insert(std::pair<NBase, CGNode*>(value, node));
        return node;
    }

    CGNode* ConstraintGraph::insert_node(CGNodeType type)
    {
        CGNode* node = new CGNode(type, NBase::null());
        NBase value = Nodecl::IntegerLiteral::make(Type::get_long_int_type(), 
                                                   const_value_get_integer(node->get_id(), /*num_bytes*/4, /*sign*/1));
        _nodes.insert(std::pair<NBase, CGNode*>(value, node));
        return node;
    }

    void ConstraintGraph::connect_nodes(
            CGNode* source, CGNode* target,
            bool is_back_edge, bool is_future_edge)
    {
        CGEdge* e = source->add_child(target, is_back_edge, is_future_edge);
        target->add_entry(e);
    }
    
    void ConstraintGraph::disconnect_nodes(
            CGNode* source, CGEdge* exit)
    {
        source->remove_exit(exit);
        exit->get_target()->remove_entry(source);
    }

    static CGNodeType get_op_type_from_value(const NBase& val)
    {
        switch(val.get_kind())
        {
            case NODECL_ADD:                            return __Add;
            case NODECL_DIV:                            return __Div;
            case NODECL_ANALYSIS_RANGE_INTERSECTION:    return __Intersection;
            case NODECL_MUL:                            return __Mul;
            case NODECL_ANALYSIS_PHI:                   return __Phi;
            case NODECL_MINUS:                          return __Sub;
            case NODECL_SYMBOL:                         return __Sym;
            default:
                internal_error("Unexpected Constraint value %s.\n", val.prettyprint().c_str());
        }
    }

    static NBase generate_range_from_constant(const NBase& constant)
    {
        Optimizations::ReduceExpressionVisitor rev;
        rev.walk(constant);
        Nodecl::Range const_range = Nodecl::Range::make(
                constant.shallow_copy(),
                constant.shallow_copy(),
                const_value_to_nodecl(zero),
                constant.get_type());
        return const_range;
    }

    CGNode* ConstraintGraph::fill_cg_with_binary_op_rec(
            const NBase& val)
    {
        ERROR_CONDITION(!Nodecl::Utils::nodecl_is_arithmetic_op(val),
                        "Expected arithmetic operation in constraint, but found '%s'.\n",
                        val.prettyprint().c_str());

        // We take the liberty of casting to Nodecl::Add always because
        // all binary operation structurally have the same tree
        const NBase& lhs = val.as<Nodecl::Add>().get_lhs().no_conv();
        const NBase& rhs = val.as<Nodecl::Add>().get_rhs().no_conv();
        CGNodeType val_type = get_op_type_from_value(val);

        CGNode* target_op = NULL;
        if (lhs.is<Nodecl::Symbol>())
        {
            if (rhs.is<Nodecl::Symbol>())
            {   // var1 OP var2
                CGNode* source1 = insert_node(lhs);                 // var1
                CGNode* source2 = insert_node(rhs);                 // var2
                target_op = insert_node(val_type);                  // OP

                connect_nodes(source1, target_op);                  // var1 -> OP
                connect_nodes(source2, target_op);                  // var2 -> OP
            }
            else if (rhs.is_constant())
            {   // var OP const
                CGNode* source1 = insert_node(lhs);                 // var

                NBase const_range = generate_range_from_constant(rhs.shallow_copy());
                CGNode* source2 = insert_node(const_range, __Const);// const

                target_op = insert_node(val_type);                  // OP

                connect_nodes(source1, target_op);                  // var -> OP
                connect_nodes(source2, target_op);                  // const -> OP
            }
            else if (Nodecl::Utils::nodecl_is_arithmetic_op(rhs))
            {   // var OP (...)
                CGNode* source1 = insert_node(lhs);                 // var
                CGNode* source2 = fill_cg_with_binary_op_rec(rhs);  // (...)
                target_op = insert_node(val_type);                  // OP

                connect_nodes(source1, target_op);                  // var -> OP
                connect_nodes(source2, target_op);                  // (...) -> OP
            }
            else
            {
                internal_error("Unexpected value '%s' for a Constraint.\n", val.prettyprint().c_str());
            }
        }
        else if (lhs.is_constant())
        {
            if (rhs.is<Nodecl::Symbol>())
            {   // const OP var
                NBase const_range = generate_range_from_constant(lhs.shallow_copy());
                CGNode* source1 = insert_node(const_range, __Const);    // const
                CGNode* source2 = insert_node(rhs);                     // var

                target_op = insert_node(val_type);                      // OP

                connect_nodes(source1, target_op);                      // const -> OP
                connect_nodes(source2, target_op);                      // var -> OP
            }
            else if (rhs.is_constant())
            {
                NBase const_range1 = generate_range_from_constant(lhs.shallow_copy());
                CGNode* source1 = fill_cg_with_binary_op_rec(const_range1);             // (...)

                NBase const_range2 = generate_range_from_constant(rhs.shallow_copy());
                CGNode* source2 = insert_node(const_range2, __Const);                   // const

                target_op = insert_node(val_type);                                      // OP

                connect_nodes(source1, target_op);                                      // (...) -> OP
                connect_nodes(source2, target_op);                                      // const -> OP
            }
            else if (Nodecl::Utils::nodecl_is_arithmetic_op(rhs))
            {   // const OP (...)
                CGNode* source1 = fill_cg_with_binary_op_rec(rhs);              // (...)

                NBase const_range = generate_range_from_constant(lhs.shallow_copy());
                CGNode* source2 = insert_node(const_range, __Const);            // const

                target_op = insert_node(val_type);                              // OP

                connect_nodes(source1, target_op);                              // (...) -> OP
                connect_nodes(source2, target_op);                              // const -> OP
            }
            else
            {
                internal_error("Unexpected value '%s' for a Constraint.\n", val.prettyprint().c_str());
            }
        }
        else if (Nodecl::Utils::nodecl_is_arithmetic_op(lhs))
        {
            CGNode* source1 = fill_cg_with_binary_op_rec(lhs);
            if (rhs.is<Nodecl::Symbol>())
            {   // (...) OP var
                CGNode* source2 = insert_node(rhs);             // var
                target_op = insert_node(val_type);              // OP
                connect_nodes(source1, target_op);              // var -> OP
                connect_nodes(source2, target_op);              // var -> OP
            }
            else if (rhs.is_constant())
            {   // var OP const
                NBase const_range = generate_range_from_constant(rhs.shallow_copy());
                CGNode* source2 = insert_node(const_range, __Const);    // const

                target_op = insert_node(val_type);                      // OP

                connect_nodes(source1, target_op);                      // var -> OP
                connect_nodes(source2, target_op);                      // const -> OP
            }
            else if (Nodecl::Utils::nodecl_is_arithmetic_op(rhs))
            {   // (.x.) OP (.y.)
                CGNode* source2 = fill_cg_with_binary_op_rec(rhs);      // (.y.)
                target_op = insert_node(val_type);                      // OP
                connect_nodes(source1, target_op);                      // (.x.) -> OP
                connect_nodes(source2, target_op);                      // (.y.) -> OP
            }
            else
            {
                internal_error("Unexpected value '%s' for a Constraint.\n", val.prettyprint().c_str());
            }
        }
        else
        {
            internal_error("Unexpected value '%s' for a Constraint.\n", val.prettyprint().c_str());
        }

        return target_op;
    }

    void ConstraintGraph::fill_cg_with_binary_op(
            const NBase& s,
            const NBase& val)
    {
        // res = (...) OP (...)
        CGNode* target_op = fill_cg_with_binary_op_rec(val);    // OP
        CGNode* target = insert_node(s);                        // res
        connect_nodes(target_op, target);                       // OP -> res
    }

    /*! Generate a constraint graph from the PCFG and the precomputed Constraints for each node
     *  A Constraint Graph is created as follows:
     *  - The nodes of the graphs are:
     *    - A. A node for each SSA variable from Constraints: c0 = ...              Node    c0
     *    - B. A node for each Constraint Value which is a Range: c1 = [lb, ub]     Node [lb, ub]
     *  - The edges are built following the rules below:
     *    - D. Constraint Phi: c2 = Phi(c0, c1)                                     Edge    c0 ---------------> c2
     *                                                                              Edge    c1 ---------------> c2
     *    - E. Constraint Intersection: c1 = c0 ∩ [lb, ub]                          Edge    c0 ----[lb, ub]---> c1
     *    - F. Constraint Range: c0 = [lb, ub]                                      Edge [lb, ub]-------------> c1
     *    - G. Constraint Arithmetic op: a. c1 = c0 + 1                             Edge    c1 ------ 1 ------> c0
     *                                   b. c1 = d + e                              Edge    d  ---------------> c1
     *                                                                              Edge    e  ---------------> c1
     *    - H. Constraint : c1 = c0                                                 Edge    c0 ---------------> c1
     */
    void ConstraintGraph::fill_constraint_graph(
        const Constraints& constraints,
        const std::vector<Symbol>& ordered_constraints)
    {
        if (RANGES_DEBUG)
        {
            std::cerr << "________________________________________________" << std::endl;
            std::cerr << "BUILD CONSTRAINT GRAPH:" << std::endl;
            std::cerr << "-----------------------" << std::endl;
        }

        std::map<NBase, CGNode*> back_edges;
        for (std::vector<Symbol>::const_iterator ito = ordered_constraints.begin();
             ito != ordered_constraints.end(); ++ito)
        {
            Symbol ssa_sym = *ito;
            Constraints::const_iterator it = constraints.find(ssa_sym);
            ERROR_CONDITION(it == constraints.end(), 
                            "SSA constraint symbol %s not found in the constraints' container.\n",
                            ssa_sym.get_name().c_str());
            const NBase& val = it->second;

            // Insert in the CG the edges corresponding to the current Constraint
            const NBase& ssa_var = ssa_sym.make_nodecl(/*set_ref_type*/false);
            if (val.is<Nodecl::Symbol>())
            {   // H. 
                CGNode* source = insert_node(val);
                CGNode* target = insert_node(ssa_var);  // A.
                connect_nodes(source, target);
            }
            else if (val.is<Nodecl::Analysis::Phi>())
            {   // D.
                // Create the target
                CGNode* target_phi = insert_node(__Phi);
                // Create the sources
                std::queue<CGNode*> sources;
                Nodecl::List expressions = val.as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                for (Nodecl::List::iterator ite = expressions.begin(); ite != expressions.end(); ++ite)
                {
                    NBase e = *ite;
                    if (_nodes.find(e) == _nodes.end())
                    {   // The expression inside the Phi node comes from a back edge
                        // We will print it later so the order of node creation respects the order in the sequential code
                        back_edges.insert(std::pair<NBase, CGNode*>(e, target_phi));
                    }
                    else
                    {
                        CGNode* source = insert_node(e);
                        sources.push(source);
                    }
                }
                CGNode* target = insert_node(ssa_var);  // A.
                // Connect them
                while (!sources.empty())
                {
                    CGNode* source = sources.front();
                    sources.pop();
                    connect_nodes(source, target_phi);
                }
                connect_nodes(target_phi, target);
            }
            else if (val.is<Nodecl::Analysis::RangeIntersection>())
            {   // E.
                const NBase& lhs = val.as<Nodecl::Analysis::RangeIntersection>().get_lhs().no_conv();
                const NBase& rhs = val.as<Nodecl::Analysis::RangeIntersection>().get_rhs().no_conv();

                CGNode* source = insert_node(lhs);
                CGNode* target = insert_node(ssa_var);  // A.
                NBase range = rhs.shallow_copy();
                Optimizations::ReduceExpressionVisitor rev;
                rev.walk(range);
                CGNode* range_node = insert_node(range, __Intersection);
                connect_nodes(source, range_node);
                connect_nodes(range_node, target);

                // If the range contains "future" edges, we have to create them here
                const ObjectList<Symbol>& range_syms = Nodecl::Utils::get_all_symbols(range);
                for (ObjectList<Symbol>::const_iterator its = range_syms.begin();
                     its != range_syms.end(); ++its)
                {
                    const NBase& ssa_ft_sym = its->make_nodecl(/*set_ref_type*/false);
                    CGNode* ft_source = insert_node(ssa_ft_sym);
                    connect_nodes(ft_source, range_node, /*is_back_edge*/false, /*is_future_edge*/true);
                }
            }
            else if (val.is<Nodecl::Range>())
            {
                // B. Create a new node if the Constraint Value is a Range
                CGNode* source = insert_node(val, __Const);
                CGNode* target = insert_node(ssa_var);  // A.
                // F. Create edge between the Range node and the Constraint node
                connect_nodes(source, target);
            }
            else if (Nodecl::Utils::nodecl_is_arithmetic_op(val))
            {
                fill_cg_with_binary_op(ssa_var, val);
            }
            else
            {
                internal_error("Unexpected type of Constraint value '%s' for constraint '%s'.\n", 
                               ast_print_node_type(val.get_kind()), val.prettyprint().c_str());
            }
        }

        // Connect now the back edges
        for (std::map<NBase, CGNode*>::iterator it = back_edges.begin(); it != back_edges.end(); ++it)
        {   // Both source and target must exist already
            CGNode* source = insert_node(it->first);
            CGNode* target = it->second;
            connect_nodes(source, target, /*is_back_edge*/true);
        }
    }

    void ConstraintGraph::print_graph() const
    {
        // Get a file to print a DOT with the Constraint Graph
        // Create the directory of dot files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/dot/";
        if (stat(directory_name.c_str(), &st) != 0)
        {
            int dot_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if (dot_directory != 0)
                internal_error ("An error occurred while creating the dot directory in '%s'", 
                                directory_name.c_str());
        }

        // Create the file where we will store the DOT CG
        std::string dot_file_name = directory_name + _name + "_cg.dot";
        std::ofstream dot_cg;
        dot_cg.open(dot_file_name.c_str());
        if (!dot_cg.good())
            internal_error ("Unable to open the file '%s' to store the CG.", dot_file_name.c_str());
        if (VERBOSE)
            std::cerr << "- CG DOT file '" << dot_file_name << "'" << std::endl;
        dot_cg << "digraph CG {\n";

        // 1.- Print the general information of the graph
        dot_cg << "\tcompound=true;\n";
        dot_cg << "\tlabel=\"Constraint Graph of function '" << _name << "'\"";
        dot_cg << "\tnode [shape=record, fontname=\"Times-Roman\", fontsize=14];\n";
        for (CGValueToCGNode_map::const_iterator it = _nodes.begin(); it != _nodes.end(); ++it)
        {
            CGNode* n = it->second;
            unsigned int source = n->get_id();

            // 2.- Print the Node
            CGNodeType t = n->get_type();
            if (t == __Sym || t == __Const || t == __Intersection)
            {   // Print nodes with a valuation associated
                NBase constraint = n->get_constraint();
                dot_cg << "\t" << source << " [label=\"[" << source << "] " << constraint.prettyprint() << "\", shape=\"polygon\"];\n";
                NBase val = n->get_valuation();
                if (!constraint.is<Nodecl::Range>() && !val.is_null())
                {
                    // Print a node containing the valuation
                    dot_cg << "\t0" << source << " [label=\"" << val.prettyprint() << "\", "
                           << "style=\"dashed\", color=\"gray55\", fontcolor=\"gray27\", shape=\"polygon\"];\n";
                    // Connect it to its constraint node
                    dot_cg << "\t" << source << "->" << "0" << source << " [label=\"\", style=\"dashed\", color=\"gray55\"];\n";
                    // set the same rank to the constraint node an its valuation node
                    dot_cg << "\t{rank=same; " << source << "; 0" << source << ";}";
                }
            }
            else
            {   // Print operation nodes
                dot_cg << "\t" << source << " [label=\"[" << source << "] " << n->get_type_as_string() << "\", shape=\"polygon\"];\n";
            }

            // Print the node relations
            const std::set<CGEdge*>& exits = n->get_exits();
            for (std::set<CGEdge*>::iterator ite = exits.begin(); ite != exits.end(); ++ite)
            {
                unsigned int target = (*ite)->get_target()->get_id();
                std::string edge_style =
                        ((*ite)->is_back_edge() ? "dotted"
                                                : ((*ite)->is_future_edge() ? "dashed"
                                                                            : "solid"));
                        std::string attrs = " [label=\"\", style=\"" + edge_style + "\"]";
                dot_cg << "\t" << source << "->" << target << attrs << ";\n";
            }
        }
        dot_cg << "}\n";
        dot_cg.close();
        if (!dot_cg.good())
            internal_error ("Unable to close the file '%s' where CG has been stored.", dot_file_name.c_str());
    }

namespace {
    bool stack_contains_cgnode(const std::stack<CGNode*>& s, CGNode* n)
    {
        bool result = false;
        std::stack<CGNode*> tmp = s;
        while(!tmp.empty() && !result)
        {
            if(tmp.top()==n)
                result = true;
            tmp.pop();
        }
        return result;
    }
}
    
    void ConstraintGraph::strong_connect(CGNode* n, unsigned int& scc_current_index, 
            std::stack<CGNode*>& s, std::vector<SCC*>& scc_list, 
            std::map<CGNode*, int>& scc_lowlink_index,
            std::map<CGNode*, int>& scc_index)
    {
        // Set the depth index for 'n' to the smallest unused index
        scc_index[n] = scc_current_index;
        scc_lowlink_index[n] = scc_current_index;
        ++scc_current_index;
        s.push(n);

        // Consider the successors of 'n'
        const std::set<CGEdge*>& succ = n->get_exits();
        for (std::set<CGEdge*>::iterator it = succ.begin(); it != succ.end(); ++it)
        {
            // Never follow future edges to compose the SCCs
            if ((*it)->is_future_edge())
                continue;

            CGNode* m = (*it)->get_target();
            if (scc_index.find(m)==scc_index.end())
            {   // Initialize values for this node if it has not yet been initialized
                scc_index[m] = -1;
                scc_lowlink_index[m] = -1;
            }
            if (scc_index[m] == -1)
            {   // Successor 'm' has not yet been visited: recurse on it
                strong_connect(m, scc_current_index, s, scc_list, scc_lowlink_index, scc_index);
                scc_lowlink_index[n] = std::min(scc_lowlink_index[n], scc_lowlink_index[m]);
            }
            else if (stack_contains_cgnode(s, m))
            {   // Successor 'm' is in the current SCC
                scc_lowlink_index[n] = std::min(scc_lowlink_index[n], scc_index[m]);
            }
        }   

        // If 'n' is a root node, pop the set and generate an SCC
        if ((scc_lowlink_index[n] == scc_index[n]) && !s.empty())
        {
            SCC* scc = new SCC(&_node_to_scc_map);
            while (!s.empty() && s.top()!=n)
            {
                scc->add_node(s.top());
                s.pop();
            }
            if (!s.empty() && s.top()==n)
            {
                scc->add_node(s.top());
                s.pop();
            }
            scc_list.push_back(scc);
        }
    }
    
    // Implementation of the Tarjan's strongly connected components algorithm
    std::vector<SCC*> ConstraintGraph::topologically_compose_strongly_connected_components()
    {
        std::vector<SCC*> scc_list;
        std::stack<CGNode*> s;
        unsigned int scc_current_index = 0;

        // 1.- Collect each set of nodes that form a SCC
        std::map<CGNode*, int> scc_lowlink_index;
        std::map<CGNode*, int> scc_index;
        for (CGValueToCGNode_map::iterator it = _nodes.begin(); it != _nodes.end(); ++it)
        {
            CGNode* n = it->second;
            if ((scc_index.find(n) == scc_index.end()) || (scc_index[n] == -1))
                strong_connect(n, scc_current_index, s, scc_list, scc_lowlink_index, scc_index);
        }

        // 2.- Compute the directionality of each scc_current_index
        // 3.- Create a map between the Constraint Graph nodes and their SCC
        for (std::vector<SCC*>::iterator it = scc_list.begin(); it != scc_list.end(); ++it)
        {
            std::vector<CGNode*> scc_nodes = (*it)->get_nodes();
            for (std::vector<CGNode*>::iterator itt = scc_nodes.begin(); itt != scc_nodes.end(); ++itt)
            {
                _node_to_scc_map.insert(std::pair<CGNode*, SCC*>(*itt, *it));
            }
        }

        // Compute the root of each SCC
        for (std::vector<SCC*>::iterator it = scc_list.begin(); it != scc_list.end(); ++it)
        {
            SCC* scc = *it;
            const std::vector<CGNode*>& nodes = scc->get_nodes();
            if (scc->is_trivial())
            {
                scc->add_root(nodes[0]);
            }
            else
            {
                for (std::vector<CGNode*>::const_iterator itt = nodes.begin();
                     itt != nodes.end(); ++itt)
                {   // In a cycle, the root is a Phi node
                    if ((*itt)->get_type() == __Phi)
                    {   // Only when it has one back edge and
                        // the other edge comes from a node outside the SCC
                        // Otherwise, these Phi nodes are joining IfElse branches
                        bool has_back_edge = 0;
                        bool has_entry_from_other_scc = 0;
                        const ObjectList<CGEdge*>& entries = (*itt)->get_entries();
                        if (entries.size() != 2)
                            continue;
                        if (entries[0]->is_back_edge()
                                || entries[1]->is_back_edge())
                            has_back_edge = 1;
                        if (_node_to_scc_map[entries[0]->get_source()] != _node_to_scc_map[*itt]
                                || _node_to_scc_map[entries[1]->get_source()] != _node_to_scc_map[*itt])
                            has_entry_from_other_scc = 1;

                        if (has_back_edge && has_entry_from_other_scc)
                            scc->add_root(*itt);
                    }
                }
            }
        }

        print_sccs(scc_list);

        // Collect the roots of each SCC tree
        std::vector<SCC*> roots;
        for (std::map<CGNode*, SCC*>::iterator it = _node_to_scc_map.begin();
             it != _node_to_scc_map.end(); ++it)
        {
            if (it->first->get_entries().empty())
                roots.push_back(it->second);
        }

        return roots;
    }

namespace {

    NBase evaluate_binary_operation(CGNode* n)
    {
        const ObjectList<CGNode*>& parents = n->get_parents();

        // Check the integrity of the Constraint Graph at this point
        ERROR_CONDITION(parents.size() != 2,
                        "An operation node is expected to have two entries, "
                        "but node %d has %d entries.\n",
                        n->get_id(), parents.size());

        // Get first operator
        NBase op1;
        CGNode* n1 = parents[0];
        CGNodeType n1_t = n1->get_type();
        if ((n1_t == __Sym) || (n1_t == __Const) || (n1_t == __Intersection))
            op1 = n1->get_valuation();
        else
            op1 = evaluate_binary_operation(n1);

        // Get second operator
        NBase op2;
        CGNode* n2 = parents[1];
        CGNodeType n2_t = n2->get_type();
        if ((n2_t == __Sym) || (n2_t == __Const) || (n2_t == __Intersection))
            op2 = n2->get_valuation();
        else
            op2 = evaluate_binary_operation(n2);

        // Compute the operation
        NBase valuation;
        switch (n->get_type())
        {
            case __Add:
            {
                valuation = Utils::range_addition(op1, op2);
                break;
            }
            case __Sub:
            {
                valuation = Utils::range_subtraction(op1, op2);
                break;
            }
            case __Mul:
            {
                valuation = Utils::range_multiplication(op1, op2);
                break;
            }
            case __Div:
            {
                valuation = Utils::range_division(op1, op2);
                break;
            }
            default:
            {
                internal_error("Unexpected parent type %s while evaluating node %d. "
                               "Expecting binary operation.\n",
                               n->get_type_as_string().c_str(), n->get_id());
            }
        };

        return valuation;
    }
}

    void ConstraintGraph::evaluate_cgnode(
            CGNode* const n,
            bool narrowing)
    {
        const NBase& old_valuation = n->get_valuation();
        NBase new_valuation;
        CGNodeType node_type = n->get_type();
        switch (node_type)
        {
            case __Const:
            {   // The node is a range => its valuation is the constraint itself
                new_valuation = n->get_constraint();
                break;
            }
            case __Sym:
            {   // The node is an SSA symbol => its valuation depend on the entry operation
                // 1.- Check the integrity of the Constraint Graph at this point
                const ObjectList<CGNode*>& parents = n->get_parents();
                ERROR_CONDITION(parents.size() != 1,
                                "A symbol node is expected to have one unique entry, "
                                "but node %d has %d entries.\n",
                                n->get_id(), parents.size());
                // 2.- Evaluate the node
                CGNode* parent = *parents.begin();
                const ObjectList<CGNode*>& grandparents = parent->get_parents();
                CGNodeType parent_type = parent->get_type();
                NBase op1, op2;     // Variables defined and used in different cases of the following switch case
                switch (parent_type)
                {
                    case __Const:
                    {   // For a __Const node already evaluated, the methods 'get_valuation' and
                        // 'get_constraint' return the same, the range
                        new_valuation = parent->get_valuation();
                        break;
                    }
                    case __Sym:
                    {   // FIXME We should try to avoid redounding SSA variables when they surely have the same valuation
                        // Two consecutive __Sym nodes will always have the same valuation
                        new_valuation = parent->get_valuation();
                        break;
                    }
                    case __Intersection:
                    {
                        // 2.1.- Check the integrity of the Constraint Graph at this point
                        // Intersection nodes may have one unique flow entry and many future entries
                        ObjectList<CGEdge*>& parent_entries = parent->get_entries();
                        CGNode* flow_grandparent = NULL;
                        std::set<CGNode*> future_grandparents;
                        unsigned int n_flow_entries = 0;
                        for (ObjectList<CGEdge*>::const_iterator it = parent_entries.begin();
                             it != parent_entries.end(); ++it)
                        {
                            if ((*it)->is_future_edge())
                            {
                                future_grandparents.insert((*it)->get_source());
                            }
                            else
                            {
                                flow_grandparent = (*it)->get_source();
                                n_flow_entries++;
                            }
                        }
                        ERROR_CONDITION(n_flow_entries != 1,
                                        "An intersection node is expected to have exactly 1 flow entry, "
                                        "but node %d has %d entries.\n", 
                                        parent->get_id(), grandparents.size());

                        // 2.2.- Compute the intersection
                        op1 = flow_grandparent->get_valuation();
                        op2 = parent->get_constraint();
                        if (!narrowing && !future_grandparents.empty())
                            new_valuation = op1;
                        else
                        {   // Simplify the intersection range as possible
                            ObjectList<NBase> op2_syms = Nodecl::Utils::get_all_memory_accesses(op2);
                            for (ObjectList<NBase>::iterator it = op2_syms.begin();
                                 it != op2_syms.end(); ++it)
                            {
                                for (std::set<CGNode*>::iterator itf = future_grandparents.begin();
                                     itf != future_grandparents.end(); ++itf)
                                {
                                    NBase constr = (*itf)->get_constraint();
                                    if (Nodecl::Utils::nodecl_contains_nodecl_by_structure(constr, *it))
                                    {   // Replace the symbol by its constant value, if it has
                                        NBase itf_val = (*itf)->get_valuation();
                                        if (itf_val.is_constant())
                                        {
                                            NBase itf_val_const = const_value_to_nodecl(itf_val.get_constant());
                                            Nodecl::Utils::nodecl_replace_nodecl_by_structure(op2, *it, itf_val_const);
                                        }
                                    }
                                }
                                Optimizations::ReduceExpressionVisitor rev;
                                rev.walk(op2);
                            }

                            new_valuation = Utils::range_intersection(op1, op2);
                        }
                        break;
                    }
                    case __Phi:
                    {
                        // 2.1.- Check the integrity of the Constraint Graph at this point
                        ERROR_CONDITION(grandparents.size() < 2,
                                        "A phi node is expected to have, at least, 2 entries, "
                                        "but node %d has %d entries.\n", 
                                        parent->get_id(), grandparents.size());
                        
                        // 2.2.- Join all entry valuations into the phi node
                        ObjectList<NBase> entry_valuations;
                        ObjectList<CGEdge*>& parent_entries = parent->get_entries();
                        int n_entries = parent_entries.size();
                        int i = 0;
                        while (i < n_entries)
                        {
                            CGNode* source = parent_entries[i]->get_source();
                            new_valuation = Utils::range_union(
                                    new_valuation,
                                    source->get_valuation());
                            i++;
                        }
                        break;
                    }
                    // Any other operation must be a binary operation
                    default:
                    {
                        new_valuation = evaluate_binary_operation(parent);
                        break;
                    }
                };

                // Check that we indeed compute some valuation
                ERROR_CONDITION(new_valuation.is_null(),
                                "Unexpected node %d of type %s.\n",
                                parent->get_id(), parent->get_type_as_string().c_str());
                break;
            }
            default:
                internal_error("Unexpected node type %d while evaluating node %d.\n",
                                n->get_type_as_string().c_str(), n->get_id());
        };

        if (RANGES_DEBUG)
            std::cerr << "        EVALUATE " << n->get_id()
                      << "  ::  " << n->get_constraint().prettyprint()
                      << "  ::  " << (old_valuation.is_null() ? "[⊥, ⊥]" : old_valuation.prettyprint())
                      << " -> " << new_valuation.prettyprint() << std::endl;
        n->set_valuation(new_valuation);
    }

    static NBase get_next_lower(const std::set<const_value_t*>& const_values, const_value_t* c)
    {
        for (std::set<const_value_t*>::const_reverse_iterator it = const_values.rbegin();
             it != const_values.rend(); ++it)
        {
            const_value_t* diff = const_value_sub(c, *it);
            if (const_value_is_positive(diff) || const_value_is_zero(diff))
            {
                if (const_value_is_zero(const_value_sub(*it, long_max)))
                {
                    internal_error("The next lower value of a sequence of constants can never be +inf.\n", 0);
                }
                else if (const_value_is_zero(const_value_add(*it, long_min)))
                    return minus_inf.shallow_copy();
                else
                    return const_value_to_nodecl(*it);
            }
        }
        return minus_inf.shallow_copy();
    }

    static NBase get_next_greater(const std::set<const_value_t*>& const_values, const_value_t* c)
    {
        for (std::set<const_value_t*>::const_iterator it = const_values.begin();
             it != const_values.end(); ++it)
        {
            const_value_t* diff = const_value_sub(*it, c);
            if (const_value_is_positive(diff) || const_value_is_zero(diff))
            {   // if we have a constant value equivalent to +inf or -inf, return +inf or -inf
                if (const_value_is_zero(const_value_sub(*it, long_max)))
                    return plus_inf.shallow_copy();
                else if (const_value_is_zero(const_value_add(*it, long_min)))
                {
                    internal_error("The next lower value of a sequence of constants can never be +inf.\n", 0);
                }
                else
                    return const_value_to_nodecl(*it);
            }
        }
        return plus_inf.shallow_copy();
    }

    static void gather_constants_from_const_node(CGNode* n, std::set<const_value_t*>& const_values)
    {
        const Nodecl::Range& range = n->get_constraint().as<Nodecl::Range>();
        const NBase& lb = range.get_lower(); 
        const NBase& ub = range.get_upper();
        if (lb.is_constant())
            const_values.insert(lb.get_constant());
        if (ub.is_constant())
            const_values.insert(ub.get_constant());
    }

    std::set<const_value_t*> ConstraintGraph::gather_scc_constants(SCC* scc)
    {
        std::set<const_value_t*> const_values;     // Result

        // 1.- Start looking for the values from the root of the component
        const std::list<CGNode*>& roots = scc->get_roots();
        std::queue<CGNode*,std::list<CGNode*> > worklist(roots);

        // 2.- Iterate until all nodes in the component have been visited
        while (!worklist.empty())
        {
            // 2.1.- Get the first node in the queue
            CGNode* n = worklist.front();
            worklist.pop();

            // 2.2.- Base case: we are exiting the component
            if (_node_to_scc_map[n] != scc)
                continue;

            // 2.3.- Evaluate the current node: if it contains a constant, store it
            CGNodeType type = n->get_type();
            if (type == __Const || type == __Intersection)
            {
                gather_constants_from_const_node(n, const_values);
            }
            // 2.4.- It may happen that an operation node receives a constant from outside the component
            else if (type != __Sym && type != __Intersection)
            {
                const ObjectList<CGNode*>& parents = n->get_parents();
                for (ObjectList<CGNode*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
                {
                    CGNode* p = *it;
                    // Only check node form outside the component
                    // since the ones inside are already checked in the normal work-flow
                    if (_node_to_scc_map[p] != scc
                            && (p->get_type() == __Const || type == __Intersection))
                    {
                        gather_constants_from_const_node(p, const_values);
                    }
                }
            }

            // 2.5.- Prepare the following iterations
            const std::set<CGEdge*>& exits = n->get_exits();
            for (std::set<CGEdge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
            {
                if (!(*it)->is_back_edge())     // Avoid cycles
                    worklist.push((*it)->get_target());
            }
        }
        return const_values;
    }

    // The widen operator used is a generalization of the Cousot and Cousot's widening operator:
    //     [e(Y) is the new valuation and I(I) is the old valuation]
    //     I[Y] = | [⊥, ⊥]                            -> e(Y)
    //            | e(Y)_ < I[Y]_ && e(Y)^ > I[Y]^   -> [-inf , +inf ]
    //            | e(Y)_ < I[Y]_                    -> [-inf , I[Y]^]
    //            | e(Y)^ > I[Y]^                    -> [I[Y]_, +inf ]
    void ConstraintGraph::widen(SCC* scc)
    {
        const std::list<CGNode*> roots = scc->get_roots();     // This is the phi node with an entry back edge

        // 1.- Gather all constants in this component
        std::set<const_value_t*> const_values = gather_scc_constants(scc);

        // 2.- Traverse the component applying the widen operation
        std::queue<CGNode*,std::list<CGNode*> > worklist(roots);
        while (!worklist.empty())
        {
            // 2.1.- Get the next node to be treated
            CGNode* n = worklist.front();
            worklist.pop();

            // 2.2.- Base case: if the node is not in the same SCC, then we will treat it later
            if (_node_to_scc_map[n] != scc)
                continue;

            // 2.3.- Keep the old valuation to be able to compare if there has been some change
            //     We make a copy because otherwise the pointer may be modified
            const NBase& old_valuation = n->get_valuation().shallow_copy();
            NBase widen_valuation = old_valuation;
            // 2.4.- Calculate the current node, only if it is a symbol, because:
            //        - __Const nodes will n ever change their valuation since it is the constraint itself
            //        - Operation nodes are never evaluated
            if (n->get_type() == __Sym)
            {
                // 2.4.1.- Compute the new valuation of the node
                evaluate_cgnode(n);

                // 2.4.2.- Apply the widen operation
                const NBase& new_valuation = n->get_valuation();
                ERROR_CONDITION(!new_valuation.is<Nodecl::Range>()
                                    && !new_valuation.is<Nodecl::Analysis::EmptyRange>(),
                                "Non-range interval '%s' found for CG-Node %d. Range expected\n",
                                new_valuation.prettyprint().c_str(), n->get_id());
                widen_valuation = new_valuation;
                if (!old_valuation.is_null())
                {   // Note that I_old[Y] = [⊥, ⊥] -> I_new[Y] = e(Y)
                    const Nodecl::Range& last_range = old_valuation.as<Nodecl::Range>();
                    const NBase& old_lb = last_range.get_lower();
                    const NBase& old_ub = last_range.get_upper();
                    const Nodecl::Range& new_range = new_valuation.as<Nodecl::Range>();
                    const NBase& new_lb = new_range.get_lower();
                    const NBase& new_ub = new_range.get_upper();
                    if (last_range.is<Nodecl::Analysis::EmptyRange>())
                    {
                        widen_valuation = new_range;
                    }
                    else if (old_lb.is_constant() && new_lb.is_constant())
                    {
                        const_value_t* old_lb_c = old_lb.get_constant();
                        const_value_t* new_lb_c = new_lb.get_constant();
                        const_value_t* lb_diff = const_value_sub(new_lb_c, old_lb_c);
                        if (const_value_is_negative(lb_diff))
                        {   // e(Y)_ < I[Y]_
                            if (old_ub.is_constant() && new_ub.is_constant())
                            {
                                const_value_t* old_ub_c = old_ub.get_constant();
                                const_value_t* new_ub_c = new_ub.get_constant();
                                const_value_t* ub_diff = const_value_sub(new_ub_c, old_ub_c);
                                if (const_value_is_positive(ub_diff))
                                {   // e(Y)^ > I[Y]^ -> [-inf , +inf]
                                    const NBase& next_lower = get_next_lower(const_values, new_lb_c);
                                    const NBase& next_upper = get_next_greater(const_values, new_ub_c);
                                    widen_valuation = Nodecl::Range::make(
                                            next_lower,
                                            next_upper,
                                            const_value_to_nodecl(zero),
                                            Utils::get_range_type(next_lower.get_type(), next_upper.get_type()));
                                }
                                else
                                {   // e(Y)^ <= I[Y]^ -> [-inf , I[Y]^]
                                    const NBase& next_lower = get_next_lower(const_values, new_lb_c);
                                    widen_valuation = Nodecl::Range::make(
                                            next_lower,
                                            old_ub,
                                            const_value_to_nodecl(zero),
                                            Utils::get_range_type(next_lower.get_type(), old_ub.get_type()));
                                }
                            }
                            else
                            {
                                WARNING_MESSAGE("Mixing UBs '%s' and '%s' is not implemented yet "
                                                "because they are not constant values.\n",
                                                old_ub.prettyprint().c_str(), new_ub.prettyprint().c_str());
                            }
                        }
                        else if (old_ub.is_constant() && new_ub.is_constant())
                        {   // e(Y)_ > I[Y]_
                            const_value_t* old_ub_c = old_ub.get_constant();
                            const_value_t* new_ub_c = new_ub.get_constant();
                            const_value_t* ub_diff = const_value_sub(new_ub_c, old_ub_c);
                            if (const_value_is_positive(ub_diff))
                            {
                                const NBase& next_upper = 
                                        (new_ub.is_constant() ? get_next_greater(const_values, new_ub.get_constant())
                                                              : plus_inf.shallow_copy());
                                widen_valuation = Nodecl::Range::make(
                                        old_lb,
                                        next_upper,
                                        const_value_to_nodecl(zero),
                                        Utils::get_range_type(old_lb.get_type(), next_upper.get_type()));
                            }
                        }
                        else
                        {
                            WARNING_MESSAGE("Mixing UBs '%s' and '%s' is not implemented yet "
                                            "because they are not constant values.\n",
                                            old_ub.prettyprint().c_str(), new_ub.prettyprint().c_str());
                        }
                    }
                    else
                    {
                        WARNING_MESSAGE("Mixing valuations '%s' and '%s' is not implemented yet "
                                        "because they are not constant values.\n",
                                        old_lb.prettyprint().c_str(), new_lb.prettyprint().c_str());
                    }

                    // Set the new valuation after widening
                    if (RANGES_DEBUG)
                    {
                        const NBase& constraint = n->get_constraint();
                        std::cerr << "        WIDEN " << n->get_id()
                                  << "  ::  " << (constraint.is_null() ? n->get_type_as_string() : constraint.prettyprint())
                                  << " = " << widen_valuation.prettyprint() << std::endl;
                    }
                    n->set_valuation(widen_valuation);
                }
            }

            // 2.5.- Prepare next iteration by adding to the worklist the children nodes
            // only if the new valuation is different from the previous one
            // Since at the beginning all evaluation are null, we are sure we pass through all nodes
            // in the component at least once
            if (n->get_type() != __Sym      // Nothing can change
                || !Nodecl::Utils::structurally_equal_nodecls(old_valuation, widen_valuation,
                                                              /*skip_conversions*/true))
            {
                const std::set<CGNode*>& children = n->get_children();
                for (std::set<CGNode*>::const_iterator it = children.begin();
                     it != children.end(); ++it)
                    worklist.push(*it);
            }
        }
    }

namespace {
    void compute_boundary_from_future(
            NBase& boundary,
            const NBase& future_sym,
            const NBase& future_sym_valuation)
    {
        // In the boundary, replace the future symbol with its valuation
        Nodecl::Utils::nodecl_replace_nodecl_by_structure(
                boundary, future_sym, future_sym_valuation);

        // If the resulting node has infinity operands, reduce them
        Utils::InfinityCalculator inf_calc;
        boundary = inf_calc.compute(boundary);

        // If the resulting node may be synthesized (calculated), then do it
        Optimizations::Calculator calc;
        const_value_t* boundary_const = calc.compute_const_value(boundary);
        if (boundary_const != NULL)
            boundary = const_value_to_nodecl(boundary_const);
    }
}

    // Solve future edges propagating the values computed after the widening process
    // Remove from the Constraint Graph the future edges, so they are not taken into account any more
    void ConstraintGraph::futures(SCC* scc)
    {
        // Traverse the component looking for future edges
        const std::list<CGNode*> roots = scc->get_roots();     // This is the phi node with an entry back edge
        std::queue<CGNode*,std::list<CGNode*> > worklist(roots);
        std::set<CGNode*> visited;
        while (!worklist.empty())
        {
            // 1.- Get the next node to be treated
            CGNode* n = worklist.front();
            worklist.pop();

            // 2.- Base case: we do not have to exit the component
            if (_node_to_scc_map[n] != scc)
                continue;

            // 3.- Check whether the node has any future entry
            const ObjectList<CGEdge*>& entries = n->get_entries();
            for (ObjectList<CGEdge*>::const_iterator it = entries.begin(); it != entries.end(); )
            {
                if ((*it)->is_future_edge())
                {
                    // Recalculate the constraint
                    // (since it is a range, it has no valuation,
                    // for it is the constraint itself)

                    // 3.1.- Get the symbol generating the future edge
                    CGNode* source = (*it)->get_source();
                    const NBase& s = source->get_constraint();
                    const Nodecl::Range& s_val = source->get_valuation().as<Nodecl::Range>();
                    const NBase& s_val_lb = s_val.get_lower();
                    const NBase& s_val_ub = s_val.get_upper();

                    // 3.2.- Get the current constraint
                    const Nodecl::Range& c_val = n->get_constraint().as<Nodecl::Range>();
                    NBase c_val_lb = c_val.get_lower();
                    NBase c_val_ub = c_val.get_upper();
                    NBase future_valuation;
                    if (Nodecl::Utils::nodecl_contains_nodecl_by_structure(c_val_lb, s))
                    {
                        compute_boundary_from_future(c_val_lb, s, s_val_lb);
                        future_valuation = Nodecl::Range::make(
                                c_val_lb,
                                c_val_ub,
                                const_value_to_nodecl(zero),
                                Utils::get_range_type(c_val_lb.get_type(), c_val_ub.get_type()));
                    }
                    else if (Nodecl::Utils::nodecl_contains_nodecl_by_structure(c_val_ub, s))
                    {
                        Nodecl::Utils::nodecl_replace_nodecl_by_structure(
                                c_val_ub, s, s_val_ub);
                        compute_boundary_from_future(c_val_ub, s, s_val_ub);
                        future_valuation = Nodecl::Range::make(
                                c_val_lb,
                                c_val_ub,
                                const_value_to_nodecl(zero),
                                Utils::get_range_type(c_val_lb.get_type(), c_val_ub.get_type()));
                    }
                    else
                    {
                        internal_error("Unreachable code\n", 0);
                    }
                    n->set_constraint(future_valuation);

                    // Remove the edge from source and target, so we do not use it anymore
                    disconnect_nodes(source, *it);
                    // FIXME We should delete the edge

                    if (RANGES_DEBUG)
                    {
                        std::cerr << "    FUTURES " << n->get_id()
                                  << "  ::  " << c_val.prettyprint()
                                  << " -> " << future_valuation.prettyprint() << std::endl;
                    }
                }
                else
                {
                    ++it;
                }
            }

            // 4.- Prepare the following iterations (do not follow backward edges)
            const std::set<CGEdge*>& exits = n->get_exits();
            for (std::set<CGEdge*>::iterator it = exits.begin(); it != exits.end(); ++it)
            {
                if (!(*it)->is_back_edge())
                    worklist.push((*it)->get_target());
            }
        }
    }

    // The narrow operator used is the one proposed by Cousot and Cousot's:
    //     [e(Y) is the new valuation and I(I) is the old valuation]
    //     I[Y] = | I[Y]_ = -inf && e(Y)_ > -inf     -> [e(Y)_, I[Y]^]
    //            | I[Y]^ = +inf && e(Y)^ < +inf     -> [I[Y]_, e(Y)^]
    //            | I[Y]_ > e(Y)_                    -> [e(Y)_, I[Y]^]
    //            | I[Y]^ < e(Y)^                    -> [I[Y]_, e(Y)^]
    void ConstraintGraph::narrow(SCC* scc)
    {
        // Traverse the component applying the narrow operation
        const std::list<CGNode*> roots = scc->get_roots();     // This is the phi node with an entry back edge
        std::queue<CGNode*,std::list<CGNode*> > worklist(roots);
        std::set<CGNode*> visited;
        while (!worklist.empty())
        {
            // 1.- Get the next node to be treated
            CGNode* n = worklist.front();
            worklist.pop();

            // 2.- Base case: if the node is not in the same SCC, then we will treat it later
            if (_node_to_scc_map[n] != scc)
                continue;

            // 3.- Keep the old valuation to be able to compare if there has been some change
            //     We make a copy because otherwise the pointer may be modified
            const NBase& old_valuation = n->get_valuation().shallow_copy();
            NBase narrow_valuation = old_valuation;

            // 4.- Calculate the current node, only if it is a symbol, because:
            //        - __Const nodes will n ever change their valuation since it is the constraint itself
            //        - Operation nodes are never evaluated
            if (n->get_type() == __Sym)
            {
                // 4.1.- Compute the new valuation of the node
                evaluate_cgnode(n, /*narrowing*/ 1);

                // 4.2.- Apply the narrow operation
                const NBase& new_valuation = n->get_valuation();
                ERROR_CONDITION(!new_valuation.is<Nodecl::Range>()
                                    && !new_valuation.is<Nodecl::Analysis::EmptyRange>(),
                                "Non-range interval '%s' found for CG-Node %d. Range expected\n",
                                new_valuation.prettyprint().c_str(), n->get_id());

                if (new_valuation.is<Nodecl::Analysis::EmptyRange>()
                    || old_valuation.is<Nodecl::Analysis::EmptyRange>())
                {
                    narrow_valuation = new_valuation;
                }
                else
                {
                    narrow_valuation = new_valuation;
                    const Nodecl::Range& last_range = old_valuation.as<Nodecl::Range>();
                    const NBase& old_lb = last_range.get_lower();
                    const NBase& old_ub = last_range.get_upper();
                    const Nodecl::Range& new_range = new_valuation.as<Nodecl::Range>();
                    const NBase& new_lb = new_range.get_lower();
                    const NBase& new_ub = new_range.get_upper();
                    if (old_lb.is<Nodecl::Analysis::MinusInfinity>()
                            && new_lb.is_constant()
                            && const_value_is_positive(const_value_sub(new_lb.get_constant(),
                                                                    minus_inf.get_constant())))
                    {   // I[Y]_ = -inf && e(Y)_ > -inf ---> [e(Y)_, I[Y]^]
                        narrow_valuation =
                                Nodecl::Range::make(new_lb, old_ub,
                                                    const_value_to_nodecl(zero),
                                                    Utils::get_range_type(new_lb.get_type(), old_ub.get_type()));
                    }
                    else if (old_ub.is<Nodecl::Analysis::PlusInfinity>()
                                && new_ub.is_constant()
                                && const_value_is_positive(const_value_sub(plus_inf.get_constant(),
                                                                        new_ub.get_constant())))
                    {   // I[Y]^ = +inf && e(Y)^ < +inf ---> [I[Y]_, e(Y)^]
                        narrow_valuation =
                                Nodecl::Range::make(old_lb, new_ub,
                                                    const_value_to_nodecl(zero),
                                                    Utils::get_range_type(old_lb.get_type(), new_ub.get_type()));
                    }
                    else
                    {
                        if (old_lb.is_constant() && new_lb.is_constant())
                        {
                            const_value_t* old_lb_c = old_lb.get_constant();
                            const_value_t* new_lb_c = new_lb.get_constant();
                            const_value_t* diff = const_value_sub(old_lb_c, new_lb_c);
                            if (const_value_is_positive(diff))
                            {   // I[Y]_ > e(Y)_ ---> [e(Y)_, I[Y]^]
                                narrow_valuation =
                                    Nodecl::Range::make(new_lb, old_ub,
                                                        const_value_to_nodecl(zero),
                                                        Utils::get_range_type(new_lb.get_type(), old_ub.get_type()));
                            }
                            else if (const_value_is_negative(diff))
                            {   // I[Y]^ < e(Y)^ ---> [I[Y]_, e(Y)^]
                                narrow_valuation =
                                    Nodecl::Range::make(old_lb, new_ub,
                                                        const_value_to_nodecl(zero),
                                                        Utils::get_range_type(old_lb.get_type(), new_ub.get_type()));
                            }
                        }
                        else
                        {
                            WARNING_MESSAGE("Mixing valuations '%s' and '%s' is not implemented yet "
                                            "because they contain non-constant values.\n",
                                            old_valuation.prettyprint().c_str(), new_valuation.prettyprint().c_str());
                        }
                    }
                }

                // Set the new valuation after narrowing
                if (RANGES_DEBUG)
                {
                    const NBase& constraint = n->get_constraint();
                    std::cerr << "        NARROW " << n->get_id()
                              << "  ::  " << (constraint.is_null() ? n->get_type_as_string() : constraint.prettyprint())
                              << " = " << narrow_valuation.prettyprint() << std::endl;
                }
                n->set_valuation(narrow_valuation);
            }

            // 5.- Prepare next iteration by adding to the worklist the children nodes
            // only if the new valuation is different from the previous one
            // or it is the first time we try to narrow this node
            if (n->get_type() != __Sym      // Always add operation nodes, because they never change
                    || !Nodecl::Utils::structurally_equal_nodecls(old_valuation, narrow_valuation,
                                                                  /*skip_conversions*/true)
                    || visited.find(n) == visited.end())
            {
                const std::set<CGNode*>& children = n->get_children();
                for (std::set<CGNode*>::const_iterator it = children.begin();
                    it != children.end(); ++it)
                    worklist.push(*it);
            }

            // Mark the node as visited: from now on, if the valuation in this node does not change,
            // then we will not keep iterating over its children
            visited.insert(n);
        }
    }

    // Only __Sym nodes are evaluated!
    // FIXME: The type of the valuations must be adjusted to the type of the corresponding symbol.
    //        For example:
    //            int a = 10;           --> [10, 10] of type int
    //            unsigned int b = a;   --> [10, 10] of type unsigned int (currently, the type here is int)
    void ConstraintGraph::solve_constraints(const std::vector<SCC*>& root_sccs)
    {
        if (RANGES_DEBUG)
        {
            std::cerr << "________________________________________________" << std::endl;
            std::cerr << "SOLVE CONSTRAINTS:" << std::endl;
            std::cerr << "------------------" << std::endl;
        }

        // Are suitable to be solved those nodes that are roots
        // because they do not depend on the previous evaluation of any other node
        std::queue<SCC*> next_scc;
        for (std::vector<SCC*>::const_iterator it = root_sccs.begin(); it != root_sccs.end(); ++it)
            next_scc.push(*it);

        // Keep track of those SCC already solved, so we do not solve them again
        std::set<SCC*> visited;

        // First iteration solves all trivial components and,
        // for cycles, applies the widen operation
        // We store the components which are cycles,
        // so we do not have to traverse the whole graph again
        std::vector<SCC*> cycle_scc;    // Store them in the same order we solve them the first time
        if (RANGES_DEBUG)
            std::cerr << " ================= WIDEN =================" << std::endl;
        while (!next_scc.empty())
        {
            // 1.- Get the next SCC to be solved
            SCC* scc = next_scc.front();
            next_scc.pop();
            // 2.- Base case: the SCC has already been solved
            if (visited.find(scc) != visited.end())
                continue;

            // 2.- Check whether this SCC is ready to be solved:
            //     All its entries, but those coming from back edges, must have been already solved
            bool is_ready = true;
            const std::vector<CGNode*>& nodes = scc->get_nodes();
            std::set<CGEdge*> entries;
            for (std::vector<CGNode*>::const_iterator it = nodes.begin(); it != nodes.end(); ++it)
            {
                ObjectList<CGEdge*> n_entries = (*it)->get_entries();
                for (ObjectList<CGEdge*>::iterator itt = n_entries.begin();
                     itt != n_entries.end(); ++itt)
                {
                    CGNode* parent = (*itt)->get_source();
                    SCC* parent_scc = _node_to_scc_map[parent];
                    if (scc != parent_scc                               // This parent belongs to a different component
                        && visited.find(parent_scc) == visited.end())   // That other component has not been solved yet
                    {
                        is_ready = false;
                        break;
                    }
                }
            }
            if (!is_ready)
            {
                next_scc.push(scc);
                continue;
            }

            // Solve the current SCC
            if (scc->is_trivial())
            {   // Evaluate the only node within the SCC, if necessary (operation nodes are not evaluated)
                CGNode* n = scc->get_nodes()[0];
                if (n->get_type() == __Sym || n->get_type() == __Const)
                {
                    if (RANGES_DEBUG)
                        std::cerr << "    SCC " << scc->get_id() << std::endl;
                    evaluate_cgnode(n);
                }
            }
            else
            {   // Cycle widening operation
                if (RANGES_DEBUG)
                    std::cerr << "    SCC " << scc->get_id() << std::endl;
                widen(scc);
                cycle_scc.push_back(scc);
            }
            visited.insert(scc);

            // Prepare next iterations, if there are
            // We will add more than one child here when a single node generates more than one constraint
            const ObjectList<SCC*>& scc_exits = scc->get_scc_exits();
            for (ObjectList<SCC*>::const_iterator it = scc_exits.begin();
                 it != scc_exits.end(); ++it)
                next_scc.push(*it);
        }

        // Apply the "futures" operation
        if (RANGES_DEBUG)
            std::cerr << " ================= FUTURES =================" << std::endl;
        for (std::vector<SCC*>::iterator it = cycle_scc.begin(); it != cycle_scc.end(); ++it)
        {
            SCC* scc = *it;
            if (RANGES_DEBUG)
                std::cerr << "    SCC " << scc->get_id() << std::endl;
            futures(scc);
        }

        // Apply the narrow operation and re-evaluate trivial nodes, for they may have changedZ
        visited.clear();
        for (std::vector<SCC*>::const_iterator it = root_sccs.begin(); it != root_sccs.end(); ++it)
            next_scc.push(*it);
        if (RANGES_DEBUG)
            std::cerr << " ================= NARROW =================" << std::endl;
        while (!next_scc.empty())
        {
            // 1.- Get the next SCC to be solved
            SCC* scc = next_scc.front();
            next_scc.pop();

            // 2.- Base case: the SCC has already been solved
            if (visited.find(scc) != visited.end())
                continue;

            // 2.- Check whether this SCC is ready to be solved:
            //     All its entries, but those coming from back edges, must have been already solved
            bool is_ready = true;
            const std::vector<CGNode*>& nodes = scc->get_nodes();
            std::set<CGEdge*> entries;
            for (std::vector<CGNode*>::const_iterator it = nodes.begin(); it != nodes.end(); ++it)
            {
                ObjectList<CGEdge*> n_entries = (*it)->get_entries();
                for (ObjectList<CGEdge*>::iterator itt = n_entries.begin();
                     itt != n_entries.end(); ++itt)
                {
                    CGNode* parent = (*itt)->get_source();
                    SCC* parent_scc = _node_to_scc_map[parent];
                    if (scc != parent_scc                               // This parent belongs to a different component
                        && visited.find(parent_scc) == visited.end())   // That other component has not been solved yet
                    {
                        is_ready = false;
                        break;
                    }
                }
            }
            if (!is_ready)
            {
                next_scc.push(scc);
                continue;
            }

            // Solve the current SCC
            if (scc->is_trivial())
            {   // Evaluate the only node within the SCC, if necessary (operation nodes are not evaluated)
                CGNode* n = scc->get_nodes()[0];
                if (n->get_type() == __Sym || n->get_type() == __Const)
                {
                    if (RANGES_DEBUG)
                        std::cerr << "    SCC " << scc->get_id() << std::endl;
                    evaluate_cgnode(n, /*narrowing*/ 1);
                }
            }
            else
            {   // Cycle widening operation
                if (RANGES_DEBUG)
                    std::cerr << "    SCC " << scc->get_id() << std::endl;
                narrow(scc);
            }
            visited.insert(scc);

            // Prepare next iterations, if there are
            // We will add more than one child here when a single node generates more than one constraint
            const ObjectList<SCC*>& scc_exits = scc->get_scc_exits();
            for (ObjectList<SCC*>::const_iterator it = scc_exits.begin();
                 it != scc_exits.end(); ++it)
                next_scc.push(*it);
        }
    }

    // ***************** END Class implementing constraint graph ******************* //
    // ***************************************************************************** //



    // ***************************************************************************** //
    // ********************* Class implementing range analysis ********************* //

    Scope ssa_scope;
    std::map<Symbol, NBase> ssa_to_original_var;

    RangeAnalysis::RangeAnalysis(ExtensibleGraph* pcfg)
        : _pcfg(pcfg), _cg(new ConstraintGraph(pcfg->get_name())), 
          _constraints(), _ordered_constraints()
    {
        const NBase& pcfg_ast = pcfg->get_graph()->get_graph_related_ast();
        if (pcfg_ast.is<Nodecl::FunctionCode>())
        {
            ssa_scope = pcfg_ast.as<Nodecl::FunctionCode>().get_statements().retrieve_context();
        }
        else
        {
            ssa_scope = pcfg_ast.retrieve_context();
        }

        // Restart the global identifiers for CGNodes and SCCs
        reset_ids();
    }
    
    void RangeAnalysis::compute_range_analysis()
    {   
        // 1.- Compute the constraints of the current PCFG
        std::map<Node*, VarToConstraintMap> pcfg_constraints;
        compute_constraints(pcfg_constraints);

        // 2.- Build the Constraint Graph (CG) from the computed constraints
        build_constraint_graph();

        // 3.- Extract the Strongly Connected Components (SCC) of the graph
        //     And get the root of each topologically ordered subgraph
        std::vector<SCC*> roots = _cg->topologically_compose_strongly_connected_components();

        // 4.- Constraints evaluation
        _cg->solve_constraints(roots);
        _cg->print_graph();

        // 5.- Insert computed ranges in the PCFG
        set_ranges_to_pcfg(pcfg_constraints);
    }
    
    void RangeAnalysis::compute_constraints(
        /*out*/ std::map<Node*, VarToConstraintMap>& pcfg_constraints)
    {
        if (RANGES_DEBUG)
        {
            std::cerr << "________________________________________________" << std::endl;
            std::cerr << "COMPUTE CONSTRAINTS:" << std::endl;
            std::cerr << "--------------------" << std::endl;
        }

        // 1.- Create constraint [-∞, +∞] for each parameter
        compute_parameters_constraints(pcfg_constraints);
    
        // 2.- Create constraints for all constants (macros, static variables, etc.)
        compute_non_local_symbols_constraints(pcfg_constraints);

        // 3.- Compute constraints for the function statements
        Node* entry = _pcfg->get_graph()->get_graph_entry_node();
        std::queue<Node*> worklist; worklist.push(entry);
        std::set<Node*> treated;
        compute_constraints_rec(worklist, treated, pcfg_constraints);

        // 4.- Remove the constraints that are never used
//         remove_unnecessary_constraints(pcfg_constraints);

        // 5.- Print in std out the constraints, if requested
        if (RANGES_DEBUG)
            print_constraints();
    }
    
    void RangeAnalysis::build_constraint_graph()
    {
        _cg->fill_constraint_graph(_constraints, _ordered_constraints);
    }
    
    // Set an constraint to the graph entry node for each parameter of the function
    void RangeAnalysis::compute_parameters_constraints(
        /*out*/ std::map<Node*, VarToConstraintMap>& pcfg_constraints)
    {
        Symbol func_sym = _pcfg->get_function_symbol();
        if (!func_sym.is_valid())    // The PCFG have been built for something other than a FunctionCode
            return;

        Node* entry = _pcfg->get_graph()->get_graph_entry_node();
        const ObjectList<Symbol>& params = func_sym.get_function_parameters();
        ConstraintBuilder cbv(
                /*unnecessary for parameters*/pcfg_constraints[entry],
                &_constraints, &_ordered_constraints);
        cbv.compute_parameters_constraints(params);
        pcfg_constraints[entry] = cbv.get_output_constraints();
    }

    void RangeAnalysis::compute_non_local_symbols_constraints(
        /*out*/ std::map<Node*, VarToConstraintMap>& pcfg_constraints)
    {
        const NBase& ast = _pcfg->get_graph()->get_graph_related_ast();
        const ObjectList<Nodecl::Symbol>& non_local_syms =
                Nodecl::Utils::get_nonlocal_symbols_first_occurrence(ast);
        Node* entry = _pcfg->get_graph()->get_graph_entry_node();
        ConstraintBuilder cbv(
            /*unnecessary for parameters*/pcfg_constraints[entry],
            &_constraints, &_ordered_constraints);
        cbv.compute_non_local_symbols_constraints(non_local_syms);
        VarToConstraintMap output_constraints = cbv.get_output_constraints();
        pcfg_constraints[entry].insert(output_constraints.begin(),
                                       output_constraints.end());
    }

namespace {

    // This method builds Phi constraints caused by a back edge
    // In order to keep the algorithms working the method must:
    // 1.- Create the new phi constraint
    // 2.- Insert it in the proper place
    // Example:
    //    * [IN]  Constraint from previous nodes (child):  C1
    //    * [IN]  Constraint from the back edge (parent):  C2
    //    * [OUT] Constraint generated in this method:     C3 = phi(C1, C2)
    //    -> The order in the ordered_constraints container must be: C1, C3, C2
    //       Because the phi constraint appears before C2
    //       in a sequential execution of the program
    void compute_constraint_from_back_edge(
        Constraints *constraints,
        std::vector<Symbol> *ordered_constraints,
        std::map<Node*, VarToConstraintMap>& pcfg_constraints,
        /*dominated*/ Node* parent, /*dominator*/ Node* child)
    {
        const VarToConstraintMap& parent_constrs = pcfg_constraints[parent];
        VarToConstraintMap& child_constrs = pcfg_constraints[child];
        ConstraintBuilder cbv(child_constrs, constraints, ordered_constraints);

        // 1.- Get the loop node containing both parent and child
        Node* outer_loop = parent->get_outer_node();
        while (!outer_loop->is_loop_node())
        {
            outer_loop = outer_loop->get_outer_node();
        }
        ERROR_CONDITION(!child->node_is_enclosed_by(outer_loop),
                        "Unexpected loop structure: loop '%d' encloses parent '%d' but not child '%d'.\n",
                        outer_loop->get_id(), parent->get_id(), child->get_id());

        // 2.- Generate the constraints caused by the back edge
        for (VarToConstraintMap::const_iterator it = parent_constrs.begin();
             it != parent_constrs.end(); ++it)
        {
            const NBase& orig_var = it->first;
            // 2.1.- Base case: the variable has not been modified => no propagation needed

            Utils::Constraint parent_c = it->second;

            // 2.2.- Base case: the child's constraint is the same as the parent's constraint
                      //Recompute constraint if the child has
            if ((child_constrs.find(orig_var) == child_constrs.end())
                    || (child_constrs[orig_var] == parent_c))
                continue;

            // 2.3.- Base case: the variable has not been modified within the loop
            const NodeclSet& killed_vars = outer_loop->get_killed_vars();
            if (killed_vars.find(orig_var) == killed_vars.end())
                continue;

            // 2.4.- In any other case: Rebuild the constraint
            // 2.4.1.- Get a new symbol for the new constraint
            std::stringstream ss; ss << get_next_id(orig_var);
            Symbol orig_sym(orig_var.get_symbol());
            std::string constr_name = orig_sym.get_name() + "_" + ss.str();
            Symbol phi_ssa_sym(ssa_scope.new_symbol(constr_name));
            Type t(orig_sym.get_type());
            phi_ssa_sym.set_type(t);
            ssa_to_original_var[phi_ssa_sym] = orig_var;

            // 2.4.2.- Replace the occurrences of the child_ssa_sym with phi_ssa_sym
            //         only if there are modifications within the loop caused by the back edge
            //         Do this before inserting the new one, because we do not want it to be replaced
            std::stack<Node*> nodes;
            nodes.push(parent->get_children()[0]);  // parent is the last node of a loop,
                                                    // and it only has one child, the condition of the loop
            NBase phi_ssa_var = phi_ssa_sym.make_nodecl(/*set_ref_type*/false);
            Utils::Constraint& child_c = child_constrs[orig_var];
            Symbol child_ssa_sym = child_c.get_symbol();
            NBase child_ssa_var = child_ssa_sym.make_nodecl(/*set_ref_type*/false);
            std::set<Node*> treated;
            treated.insert(parent);
            while (!nodes.empty())
            {
                // Get the node to be treated
                Node* n = nodes.top();
                nodes.pop();

                // Base case: the node has already been processed
                if (treated.find(n) != treated.end())
                    continue;
                treated.insert(n);

                // Gather the constraints generated by this node
                VarToConstraintMap& n_constrs = pcfg_constraints[n];
                if (n_constrs.find(orig_var) != n_constrs.end())
                {
                    Utils::Constraint& n_c = n_constrs[orig_var];
                    NBase& n_val = n_c.get_value();
                    if (RANGES_DEBUG
                        && Nodecl::Utils::nodecl_contains_nodecl_by_structure(n_val, child_ssa_var))
                    {
                        std::cerr << "    REPLACE " << child_ssa_var.prettyprint()
                                  << " WITH " << phi_ssa_var.prettyprint()
                                  << " IN " << n_val.prettyprint() << std::endl;
                    }
                    Nodecl::Utils::nodecl_replace_nodecl_by_structure(n_val, child_ssa_var, phi_ssa_var);
                }

                // Prepare following iterations
                ObjectList<Node*> children =
                    n->is_exit_node()
                        ? n->get_outer_node()->get_children()                   // exit graph nodes
                        : n->is_graph_node()
                            ? ObjectList<Node*>(1, n->get_graph_entry_node())   // enter graph nodes
                            : n->get_children();                                // regular child
                for (ObjectList<Node*>::iterator itc = children.begin();
                    itc != children.end(); ++itc)
                {
                    nodes.push(*itc);
                }
            }

            // 2.4.3.- Build the value of the new constraint    (i1 = phi(i3,i2))
            Symbol parent_ssa_sym = parent_c.get_symbol();
            NBase parent_ssa_var = parent_ssa_sym.make_nodecl(/*set_ref_type*/false);
            Nodecl::List exprs = Nodecl::List::make(child_ssa_var, parent_ssa_var);
            NBase val = Nodecl::Analysis::Phi::make(exprs, t);

            // 2.4.4.- Build the new constraint and insert it in the proper list
            Utils::Constraint recomputed_c
                    = cbv.build_constraint(phi_ssa_sym, val,
                                           Utils::__BackEdge);
            child_c = recomputed_c;

            // 2.4.5.- Reorder the new constraint in the proper place in the ordered list
            //         build_constraint has inserted prior to the parent_ssa_var
            // 2.4.5.1.- Remove the new SSA symbol (the last in the vector)
            ordered_constraints->pop_back();
            // 2.4.5.2.- Insert the new SSA symbol properly
            std::vector<Symbol>::iterator ito = ordered_constraints->begin();
            while (*ito != parent_ssa_sym && ito != ordered_constraints->end())
                ++ito;
            ERROR_CONDITION(ito == ordered_constraints->end(),
                            "SSA variable %s not found in the list of ordered constraints\n",
                            phi_ssa_sym.get_name().c_str());
            ordered_constraints->insert(ito, phi_ssa_sym);
        }
    }
}

    // This is a breadth-first search because for a given node we need all its parents 
    // Since the graph has loops, we have to solve first the loops,
    // so we can properly propagate the inner loop constraints
    void RangeAnalysis::compute_constraints_rec(
        /*inout*/ std::queue<Node*>& worklist,
        /*inout*/ std::set<Node*>& treated,
        /*inout*/ std::map<Node*, VarToConstraintMap>& pcfg_constraints)
    {
        // For each loop, we need to treat the inner nodes before treat the exit node.
        // Since the algorithm traverses children without an order,
        // we impose that order by inserting into the worklist the exit node
        // each time we find an already treated node (the condition of a loop)
        // next_worklist stores the exit node of each loop
        // (it is a container because we may find nested loops)
        std::queue<Node*> next_worklist;
        while (!worklist.empty())
        {
            Node* n = worklist.front();
            worklist.pop();

            if (treated.find(n) != treated.end())
            {
                if (!n->is_exit_node())
                {
                    // Insert in the worklist the remaining nodes
                    // exit node of the treated node, which is the condition of a loop
                    if (!next_worklist.empty())
                    {
                        worklist.push(next_worklist.front());
                        next_worklist.pop();
                    }
                }

                // Keep iterating
                continue;
            }

            // 1.- Check whether all n parents (coming from non-back-edges) are already computed
            //     Also save whether the node has back-edges (needed to order the computation of the next steps)
            bool n_has_backedge = false;
            const ObjectList<Edge*>& entries = n->get_entry_edges();
            bool ready = true;
            for (ObjectList<Edge*>::const_iterator it = entries.begin(); it != entries.end(); ++it)
            {
                Edge* e = *it;
                if (e->is_back_edge())
                    n_has_backedge = true;
                if (!e->is_back_edge()                                      // e is a dominator of n
                        && !e->get_source()->is_omp_task_node()             // e is not a task node
                        && treated.find(e->get_source())==treated.end())    // e is not yet visited
                {
                    ready = false;
                    break;
                }
            }
            if (!ready)
            {
                worklist.push(n);
                continue;
            }

            // 2.- The element is ready to be computed, let's do it
            if (n->is_graph_node())
            {
                std::queue<Node*> inner_worklist;
                inner_worklist.push(n->get_graph_entry_node());
                // 2.1.- For graph nodes, call recursively to compute inner nodes constraints
                // 2.1.1.- Recursive call with the graph entry node
                compute_constraints_rec(inner_worklist, treated, pcfg_constraints);
                // 2.1.2.- Propagate the information from the exit node to the graph node
                Node* graph_exit = n->get_graph_exit_node();
                pcfg_constraints[n] = pcfg_constraints[graph_exit];

                // Do not consider those graphs that finish *only* with a return/continue/break statement
                // That may break the sequential order of execution
                const ObjectList<Node*> exit_parents = graph_exit->get_parents();
                if (exit_parents.size()==0)
                {
                    treated.insert(n);
                    continue;
                }
            }
            else
            {
                // 2.2.- For the rest of nodes,
                //   - merge information from parents
                //   - compute current node information
                //   - merge parents' info with current node's info

                // 2.2.1.- Collect and join constraints computed for all the parents
                VarToConstraintMap input_constrs;           // Constraints coming directly from parents => they are propagated
                VarToConstraintMap merged_input_constrs;    // Constraints resulting from the join of parents' constraints
                                                            // These constraints belong to the current node => they are not propagated
                // ConstraintBuilder necessary for merging the constraints from the parents
                // The 'input_constrs' are not used here, so we use a random empty map
                ConstraintBuilder cbv_propagated(input_constrs, &_constraints, &_ordered_constraints);

                const ObjectList<Node*>& parents = (n->is_entry_node() ? n->get_outer_node()->get_parents()
                                                                       : n->get_parents());
                NodeclSet treated_omp_private_vars;
                for (ObjectList<Node*>::const_iterator itp = parents.begin(); itp != parents.end(); ++itp)
                {
                    VarToConstraintMap parent_constrs = pcfg_constraints[*itp];
                    for (VarToConstraintMap::iterator itc = parent_constrs.begin();
                         itc != parent_constrs.end(); ++itc)
                    {
                        const NBase& orig_var = itc->first;
                        // 2.2.1.1.- Treat omp nodes depending on the data-sharing of the variables
                        if (n->is_entry_node() && n->get_outer_node()->is_omp_task_node())
                        {
                            // If the variable is:
                            //    - firstprivate -> the only parent we have to take into account is the task creation node
                            //    - private      -> no propagation exists and the range of the variable is [-inf, +inf]
                            //    - shared       -> take into account all parents, and any possible modification done in the concurrent code
                            Node* task_node = n->get_outer_node();
                            const NodeclSet& fp_vars = task_node->get_firstprivate_vars();
                            const NodeclSet& p_vars = task_node->get_private_vars();
                            if (Utils::nodecl_set_contains_nodecl(orig_var, fp_vars))
                            {
                                if (!(*itp)->is_omp_task_creation_node())
                                    continue;   // Skip this parent because its values for #orig_var must not be propagated
                            }
                            else if (Utils::nodecl_set_contains_nodecl(orig_var, p_vars))
                            {
                                // Avoid propagating the range [-inf, +inf] for the same variable multiple times
                                if (Utils::nodecl_set_contains_nodecl(orig_var, treated_omp_private_vars))
                                    continue;
                                treated_omp_private_vars.insert(orig_var);
                                // Build a new constraint with value [-inf, +inf] because
                                // the initialization value of this variable is undefined
                                // 2.2.1.1.1.- Get a new symbol for the new constraint
                                std::stringstream ss; ss << get_next_id(orig_var);
                                Symbol orig_sym(Utils::get_nodecl_base(orig_var).get_symbol());
                                std::string constr_name;
                                if (orig_var.no_conv().is<Nodecl::ArraySubscript>())
                                {
                                    VarToConstraintMap all_constrs = merged_input_constrs;
                                    all_constrs.insert(input_constrs.begin(), input_constrs.end());
                                    constr_name = get_array_subscript_string(
                                                        orig_var.no_conv().as<Nodecl::ArraySubscript>(),
                                                        all_constrs);
                                }
                                else
                                {
                                    constr_name = orig_sym.get_name() + "_";
                                }
                                constr_name += ss.str();
                                Symbol ssa_var(ssa_scope.new_symbol(constr_name));
                                Type t(orig_sym.get_type());
                                ssa_var.set_type(t);
                                ssa_to_original_var[ssa_var] = orig_var;
                                // 2.2.1.1.2.- Build the value of the new constraint
                                NBase new_constraint_val = Nodecl::Range::make(minus_inf.shallow_copy(),
                                                                               plus_inf.shallow_copy(),
                                                                               const_value_to_nodecl(zero), t);
                                // 2.2.1.1.3.- Remove the old constraint from the input_constrs
                                //         If it was in the merged_input_constrs map, it will be deleted with the insertion
                                if (input_constrs.find(orig_var) != input_constrs.end())
                                    input_constrs.erase(orig_var);
                                // 2.2.1.1.4.- Build the current constraint and insert it in the proper list
                                Utils::Constraint new_c = cbv_propagated.build_constraint(ssa_var, new_constraint_val,
                                                                                          Utils::__Propagated);
                                merged_input_constrs[orig_var] = new_c;
                            }
                            else
                            {   // FIXME
                                // The variable is shared: much more values could be propagated here (from all concurrent definitions)
                                // But, as an initial approach, we propagate the values of all parents
                                // so we do nothing special here, as it was a non-OpenMP node
                            }
                        }
                        // 2.2.1.2.- Treat non-OpenMP nodes
                        Utils::Constraint c = itc->second;
                        if (input_constrs.find(orig_var) == input_constrs.end() &&
                            merged_input_constrs.find(orig_var) == merged_input_constrs.end())
                        {   // No constraints found yet for variable orig_var
                            input_constrs[orig_var] = c;
                        }
                        else
                        {   // Constraints for variable orig_var already found: merge them with the new constraint
                            // 2.2.1.2.1.- Get the existing constraint
                            Utils::Constraint old_c =
                                    ((input_constrs.find(orig_var) != input_constrs.end()) ? input_constrs[orig_var]
                                                                                           : merged_input_constrs[orig_var]);
                            NBase old_value = old_c.get_value();

                            // 2.2.1.2.2.- If the new constraint is different from the old one, compute the combination of both
                            NBase& current_value = c.get_value();
                            if (!Nodecl::Utils::structurally_equal_nodecls(old_value, current_value,
                                /*skip_conversion_nodes*/true))
                            {
                                // 2.2.1.2.2.1.- Build the value of the new constraint
                                NBase new_value;
                                if (old_value.is<Nodecl::Analysis::Phi>())
                                {   // Attach a new element to the list inside the node Phi
                                    Nodecl::List expressions = old_value.as<Nodecl::Analysis::Phi>().get_expressions().shallow_copy().as<Nodecl::List>();
                                    Nodecl::Symbol new_expr = c.get_symbol().make_nodecl(/*set_ref_type*/false);
                                    if (!Nodecl::Utils::nodecl_is_in_nodecl_list(new_expr, expressions, /*skip_conversions*/ true))
                                    {   // The value was already an expression of the Phi operation
                                        expressions.append(new_expr);
                                        new_value = Nodecl::Analysis::Phi::make(expressions, orig_var.get_type());
                                    }
                                    else
                                    {   // nothing to do
                                        continue;
                                    }
                                }
                                else
                                {   // Create a new node Phi with the combination of the old constraint and the new one
                                    Nodecl::Symbol tmp1 = old_c.get_symbol().make_nodecl(/*set_ref_type*/false);
                                    Nodecl::Symbol tmp2 = c.get_symbol().make_nodecl(/*set_ref_type*/false);
                                    Nodecl::List expressions = Nodecl::List::make(tmp1, tmp2);
                                    new_value = Nodecl::Analysis::Phi::make(expressions, current_value.get_type());
                                }

                                // 2.2.1.2.2.2.- Get a new symbol for the new constraint
                                std::stringstream ss; ss << get_next_id(orig_var);
                                Symbol orig_sym(Utils::get_nodecl_base(orig_var).get_symbol());
                                std::string constr_name;
                                if (orig_var.no_conv().is<Nodecl::ArraySubscript>())
                                {
                                    VarToConstraintMap all_constrs = merged_input_constrs;
                                    all_constrs.insert(input_constrs.begin(), input_constrs.end());
                                    constr_name = get_array_subscript_string(
                                                        orig_var.no_conv().as<Nodecl::ArraySubscript>(),
                                                        all_constrs);
                                }
                                else
                                {
                                    constr_name = orig_sym.get_name() + "_";
                                }
                                constr_name += ss.str();
                                Symbol ssa_var(ssa_scope.new_symbol(constr_name));
                                Type t(orig_sym.get_type());
                                ssa_var.set_type(t);
                                ssa_to_original_var[ssa_var] = orig_var;

                                // 2.2.1.2.2.3.- Remove the old constraint from the input_constrs
                                // If it was in the merged_input_constrs map, it will be renewed with the insertion
                                if (input_constrs.find(orig_var) != input_constrs.end())
                                    input_constrs.erase(orig_var);
                                // 2.2.1.2.2.4.- Build the current constraint and insert it in the proper list
                                Utils::Constraint new_c = cbv_propagated.build_constraint(ssa_var, new_value,
                                                                                          Utils::__Propagated);
                                merged_input_constrs[orig_var] = new_c;
                            }
                        }
                    }
                }

                // 3.- Propagate constraints from parent nodes to the current node
                pcfg_constraints[n].insert(merged_input_constrs.begin(), merged_input_constrs.end());

                // 4.- Compute the constraints generated in the current node
                VarToConstraintMap inner_constrs;
                if (n->has_statements())
                {
                    // 4.1.- Compute the constraints of the current node
                    // Note: take into account the constraints the node may already have (if it is the TRUE or FALSE child of a conditional)
                    VarToConstraintMap all_input_constrs = pcfg_constraints[n];
                    all_input_constrs.insert(merged_input_constrs.begin(), merged_input_constrs.end());
                    all_input_constrs.insert(input_constrs.begin(), input_constrs.end());
                    ConstraintBuilder cbv(all_input_constrs, &_constraints, &_ordered_constraints);
                    NodeclList stmts = n->get_statements();
                    for (NodeclList::iterator itt = stmts.begin(); itt != stmts.end(); ++itt)
                        cbv.walk(*itt);

                    inner_constrs = cbv.get_output_constraints();
                    pcfg_constraints[n].insert(inner_constrs.begin(), inner_constrs.end());

                    // 4.3.- Set true/false output constraints to current children, if applies
                    ObjectList<Edge*> exits = n->get_exit_edges();
                    if (exits.size()==2 &&
                        ((exits[0]->is_true_edge() && exits[1]->is_false_edge()) || (exits[1]->is_true_edge() && exits[0]->is_false_edge())))
                    {
                        VarToConstraintMap out_true_constrs = cbv.get_output_true_constraints();
                        VarToConstraintMap out_false_constrs = cbv.get_output_false_constraints();

                        // 4.3.1.- We always propagate to the TRUE edge
                        Node* true_node = (exits[0]->is_true_edge() ? exits[0]->get_target() : exits[1]->get_target());
                        Node* real_true_node = true_node;
                        while (true_node->is_exit_node())
                            true_node = true_node->get_outer_node()->get_children()[0];
                        if (true_node->is_graph_node())
                            true_node = true_node->get_graph_entry_node();
                        pcfg_constraints[true_node].insert(out_true_constrs.begin(), out_true_constrs.end());

                        // 4.3.2.- For the if_else cases, we only propagate to the FALSE edge when it contains statements ('else' statements)
                        Node* false_node = (exits[0]->is_true_edge() ? exits[1]->get_target() : exits[0]->get_target());
                        ObjectList<Node*> real_true_node_children = real_true_node->get_children();
                        if ((false_node->get_entry_edges().size() == 1) || !real_true_node_children.contains(false_node))
                        {   // If the true_node is a parent of the false_node, then there are no statements
                            // Avoid cases where the FALSE edge leads to the end of the graph
                            ObjectList<Node*> children;
                            while (false_node->is_exit_node())
                            {
                                children = false_node->get_outer_node()->get_children();
                                if (!children.empty())
                                    false_node = children[0];
                                else
                                {
                                    false_node = NULL;
                                    break;
                                }
                            }
                            if (false_node != NULL)
                            {
                                if (false_node->is_graph_node())
                                    false_node = false_node->get_graph_entry_node();
                                pcfg_constraints[false_node].insert(out_false_constrs.begin(), out_false_constrs.end());
                            }
                        }
                    }
                }

                // 5.- Purge propagated constraints:
                // When the node generates a constraint for a given variable
                // any propagated constraint from parents for that variable is deleted here
                for (VarToConstraintMap::iterator itt = inner_constrs.begin();
                     itt != inner_constrs.end(); ++itt)
                {
                    VarToConstraintMap::iterator ittt = input_constrs.find(itt->first);
                    if (ittt != input_constrs.end())
                        input_constrs.erase(ittt);
                }
                pcfg_constraints[n].insert(input_constrs.begin(), input_constrs.end());
            }

            treated.insert(n);

            // Make sure we compute constraints in the proper order (sequential order of the statements)
            if (!n->is_omp_task_node()
                    && !n->is_break_node()
                    && !n->is_continue_node()
                    && !n->is_return_node()
                    && !n->is_goto_node())
            {
                const ObjectList<Edge*>& exits = n->get_exit_edges();
                if (n_has_backedge)
                {   // This means the node is the condition of a loop
                    //     - first compute the loop constraints (the TRUE edge)
                    //     - then compute the other children (store the nodes in next_worklist)
                    for (ObjectList<Edge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
                    {
                        Node* t = (*it)->get_target();
                        if ((*it)->is_true_edge())
                            worklist.push(t);
                        else
                            next_worklist.push(t);
                    }
                }
                else
                {   // Otherwise,
                    //    - first propagate information to back edges, if there are
                    //    - then compute the other children
                    for (ObjectList<Edge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
                    {
                        Node* t = (*it)->get_target();
                        if ((*it)->is_back_edge())
                        {   // Propagate here constraints from the back edge
                            compute_constraint_from_back_edge(
                                &_constraints, &_ordered_constraints,
                                pcfg_constraints, n, t);
                        }

                        worklist.push(t);
                    }
                }
            }
        }
    }

    void RangeAnalysis::remove_unnecessary_constraints(std::map<Node*, VarToConstraintMap>& pcfg_constraints)
    {
        if (RANGES_DEBUG)
            std::cerr << std::endl;

        // This purge is performed in order, because a constraint may only be used
        // in the following constraints definitions, never in the previous
        for (std::vector<Symbol>::iterator ssa_sym_it = _ordered_constraints.begin();
             ssa_sym_it != _ordered_constraints.end(); )
        {
            // Look for the actual constraint only in constraints build after the current one
            bool found = false;
            std::vector<Symbol>::iterator its = ssa_sym_it;
            for (++its; its != _ordered_constraints.end(); ++its)
            {
                const NBase& val = _constraints.find(*its)->second;
                const ObjectList<NBase>& mem_accesses = Nodecl::Utils::get_all_memory_accesses(val);
                for (ObjectList<NBase>::const_iterator itm = mem_accesses.begin();
                        itm != mem_accesses.end() && !found; ++itm)
                {
                    if (itm->get_symbol() == *ssa_sym_it)
                    {
                        found = true;
                        goto ssa_sym_found;
                    }
                }
            }

            // If SSA symbol has not been found in any constraint in the system,
            // remove the constraint that generates the symbol from the corresponding sets
ssa_sym_found:
            if (!found)
            {
                // Remove the constraint from the actual list of constraints
                // This must be done first, otherwise the iterator points to some other place!
                if (RANGES_DEBUG)
                {
                    std::cerr << "    Remove Constraint " << ssa_sym_it->get_name()
                            << " = " << _constraints.find(*ssa_sym_it)->second.prettyprint() << std::endl;
                }
                _constraints.erase(_constraints.find(*ssa_sym_it));

                // Remove the entry from the var-to_ssa_var container
                for (std::map<Node*, VarToConstraintMap>::iterator it = pcfg_constraints.begin();
                     it != pcfg_constraints.end(); ++it)
                {
                    VarToConstraintMap& constrs = it->second;
                    for(VarToConstraintMap::iterator itc = constrs.begin();
                        itc != constrs.end(); )
                    {
                        if (itc->second.get_symbol() == *ssa_sym_it)
                        {
                            constrs.erase(itc++);
                        }
                        else
                        {
                            ++itc;
                        }
                    }
                }

                // Remove the symbol from the ordered list of SSA symbols
                _ordered_constraints.erase(ssa_sym_it);
            }
            else
            {
                ++ssa_sym_it;
            }
        }
    }

    void RangeAnalysis::set_ranges_to_pcfg(
            const std::map<Node*, VarToConstraintMap>& pcfg_constraints)
    {
        for (std::map<Node*, VarToConstraintMap>::const_iterator it = pcfg_constraints.begin();
             it != pcfg_constraints.end(); ++it)
        {
            const VarToConstraintMap constraints = it->second;
            if (!constraints.empty())
            {
                for (VarToConstraintMap::const_iterator itt = constraints.begin();
                     itt != constraints.end(); ++itt)
                {
                    Symbol s(itt->second.get_symbol());
                    std::map<Symbol, NBase>::iterator ssa_to_var_it;
                    ssa_to_var_it = ssa_to_original_var.find(s);
                    ERROR_CONDITION (ssa_to_var_it == ssa_to_original_var.end(), 
                                     "SSA symbol '%s' is not related to any variable of the original code\n", 
                                     s.get_name().c_str());
                    CGNode* n = _cg->get_node_from_ssa_var(itt->second.get_symbol().make_nodecl(/*set_ref_type*/false));
                    it->first->set_range(ssa_to_var_it->second, n->get_valuation());
                }
            }
        }
    }

    void RangeAnalysis::print_constraints()
    {
        std::cerr << "________________________________________________" << std::endl;
        std::cerr << "CONSTRAINT MAP:" << std::endl;
        std::cerr << "---------------" << std::endl;
        for (std::vector<Symbol>::iterator it = _ordered_constraints.begin();
             it != _ordered_constraints.end(); ++it)
        {
            std::pair<Symbol, NBase> c = *_constraints.find(*it);
            std::cerr << "    " << c.first.get_name() << "  ->  " << c.second.prettyprint() << std::endl;
        }
    }

    // ********************* END Class implementing range analysis ********************* //
    // ********************************************************************************* //
}
}
