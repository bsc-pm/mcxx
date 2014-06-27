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

#include "cxx-cexpr.h"
#include "cxx-process.h"
#include "tl-range-analysis.hpp"

#include <algorithm>
#include <fstream>
#include <set>
#include <unistd.h>
#include <sys/stat.h>
#include <unistd.h>

namespace TL {
namespace Analysis {
    
    // **************************************************************************************************** //
    // **************************** Visitor implementing constraint building ****************************** //
    
namespace {
        
//     #define RANGES_DEBUG
    
    static unsigned int cgnode_id = 0;
    
    unsigned int non_sym_constraint_id = 0;
    
    //! This maps stores the relationship between each variable in a given node and 
    //! the last identifier used to create a constraint for that variable
    std::map<NBase, unsigned int, Nodecl::Utils::Nodecl_structural_less> var_to_last_constraint_id;
    
    unsigned int get_next_id(const NBase& n)
    {
        unsigned int next_id = 0;
        if(!n.is_null())
        {
            if(var_to_last_constraint_id.find(n) != var_to_last_constraint_id.end())
                next_id = var_to_last_constraint_id[n] + 1;
            var_to_last_constraint_id[n] = next_id;
        }
        else
        {
            next_id = ++non_sym_constraint_id;
        }
        return next_id;
    }
}
    
    ConstraintReplacement::ConstraintReplacement(Utils::ConstraintMap constraints_map)
        : _constraints_map(constraints_map)
    {}
    
    void ConstraintReplacement::visit(const Nodecl::ArraySubscript& n)
    {
        if(_constraints_map.find(n) != _constraints_map.end())
            n.replace(Nodecl::Symbol::make(_constraints_map[n].get_symbol()));
        else
        {
            walk(n.get_subscripted());
            walk(n.get_subscripts());
        }
    }
    
    void ConstraintReplacement::visit(const Nodecl::ClassMemberAccess& n)
    {
        if(_constraints_map.find(n) != _constraints_map.end())
            n.replace(Nodecl::Symbol::make(_constraints_map[n].get_symbol()));
        else
        {
            walk(n.get_lhs());
            walk(n.get_member());
        }
    }
    
    void ConstraintReplacement::visit(const Nodecl::Symbol& n)
    {
        ERROR_CONDITION(_constraints_map.find(n) == _constraints_map.end(),
                        "No constraints found for symbol %s in locus %s. "
                        "We should replace the variable with the corresponding constraint.", 
                        n.prettyprint().c_str(), n.get_locus_str().c_str());
        
        n.replace(Nodecl::Symbol::make(_constraints_map[n].get_symbol()));
    }
    
    ConstraintBuilderVisitor::ConstraintBuilderVisitor(Utils::ConstraintMap input_constraints_map, 
                                                       Utils::ConstraintMap current_constraints)
        : _input_constraints_map(input_constraints_map), _output_constraints_map(current_constraints), 
          _output_true_constraints_map(), _output_false_constraints_map()
    {}
    
    void ConstraintBuilderVisitor::compute_constraints(const NBase& n)
    {
        walk(n);
    }
    
    Utils::ConstraintMap ConstraintBuilderVisitor::get_output_constraints_map()
    {
        return _output_constraints_map;
    }

    Utils::ConstraintMap ConstraintBuilderVisitor::get_output_true_constraints_map()
    {
        return _output_true_constraints_map;
    }
    
    Utils::ConstraintMap ConstraintBuilderVisitor::get_output_false_constraints_map()
    {
        return _output_false_constraints_map;
    }
    
    Utils::Constraint ConstraintBuilderVisitor::join_list(TL::ObjectList<Utils::Constraint>& list)
    {
        Utils::Constraint result = list[0];
        WARNING_MESSAGE("join_list of a list of constraint is not yet supported. Doing nothing.", 0);
        return result;    
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit_assignment(const NBase& lhs, const NBase& rhs)
    {
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(lhs);
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        std::string constr_name = orig_s.get_name() + "_" + ss.str();
        Symbol s(lhs.retrieve_context().new_symbol(constr_name));
        Type t = orig_s.get_type();
        s.set_type(t);
        // Build the value of the constraint
        NBase val;
        if(rhs.is_constant())       // x = c;    -->    X1 = c
            val = Nodecl::Analysis::Range::make(rhs.shallow_copy(), rhs.shallow_copy(), t);
        else 
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            val = rhs.shallow_copy();
            cr.walk(val);
        }
        
        // Build the constraint and insert it in the corresponding maps
        Utils::Constraint c(s, val);
#ifdef RANGES_DEBUG
        std::cerr << "Assignment Constraint " << s.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        _input_constraints_map[lhs] = c;
        _output_constraints_map[lhs] = c;
        
        return c;
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::AddAssignment& n)
    {
        NBase lhs = n.get_lhs();
        NBase rhs = n.get_rhs();
        NBase new_rhs = Nodecl::Assignment::make(lhs.shallow_copy(), 
                                                              Nodecl::Add::make(lhs.shallow_copy(), rhs.shallow_copy(), rhs.get_type()), 
                                                              lhs.get_type());
        n.replace(new_rhs);
        return visit_assignment(n.get_lhs(), n.get_rhs());
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::Assignment& n)
    {
        return visit_assignment(n.get_lhs(), n.get_rhs());
    }
    
    // x < c;    ---TRUE-->    X1 = X0 ∩ [-∞, c-1]
    //           --FALSE-->    X1 = X0 ∩ [ c,  +∞]
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::LowerThan& n)
    {
        Utils::Constraint c;
        NBase val;
        
        NBase lhs = n.get_lhs();
        NBase rhs = n.get_rhs();
        
        // Check the input is something we expect: LHS has a constraint or is a parameter
        ERROR_CONDITION(_input_constraints_map.find(lhs) == _input_constraints_map.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        Type t = orig_s.get_type();
        
        // 1. Compute the first constraint that corresponds to the current node: x < c
        // -->    X1 = [0, 1]
        // 1.1 Get a new symbol for the constraint
        std::stringstream ss; ss << get_next_id(NBase::null());
        Symbol s_x(n.retrieve_context().new_symbol("_x_" + ss.str()));
        s_x.set_type(t);
        // 1.2 Build the value of the constraints
        const_value_t* zero = const_value_get_zero(/*num_bytes*/ 4, /*sign*/1);
        const_value_t* one = const_value_get_one(/*num_bytes*/ 4, /*sign*/1);
        val = Nodecl::Analysis::Range::make(const_value_to_nodecl(zero), 
                                            const_value_to_nodecl(one), t);
        // 1.3 Build the actual constraint and insert it in the corresponding map
#ifdef RANGES_DEBUG
        std::cerr << "LowerThan Constraint " << s_x.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        c = Utils::Constraint(s_x, val);
        Symbol var_s(n.retrieve_context().new_symbol("_x"));
        var_s.set_type(t);
        Nodecl::Symbol var = Nodecl::Symbol::make(var_s);
        var.set_type(t);
        _output_constraints_map[var] = c;
        
        // 2. Compute the second constraint that corresponds to the current node: x < c
        // -->    X1 = X0
        // 2.1 Get a new symbol for the constraint
        std::stringstream ss_tmp; ss_tmp << get_next_id(lhs);
        Symbol s(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_tmp.str()));
        s.set_type(t);
        // 2.2 Build the value of the constraints
        val = Nodecl::Symbol::make(_input_constraints_map[lhs].get_symbol());
        // 2.3 Build the actual constraint and insert it in the corresponding map
#ifdef RANGES_DEBUG
        std::cerr << "LowerThan Constraint " << s.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        c = Utils::Constraint(s, val);
        _input_constraints_map[lhs] = c;
        _output_constraints_map[lhs] = c;
        
        // 3. Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            cr.walk(val);
        }
        // 3.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < x;       --TRUE-->       X1 = X0 ∩ [-∞, x-1]
        // 3.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_true.str()));
        s_true.set_type(t);
        // 3.1.2.- Build the TRUE constraint value
        NBase ub = (rhs.is_constant() ? const_value_to_nodecl(const_value_sub(rhs.get_constant(), one)) 
                                                   : Nodecl::Minus::make(val.shallow_copy(), const_value_to_nodecl(one), t));
        NBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                Nodecl::Symbol::make(s), 
                Nodecl::Analysis::Range::make(Nodecl::Analysis::MinusInfinity::make(), ub, t),
                t);
        // 3.1.3.- Build the TRUE constraint and store it
#ifdef RANGES_DEBUG
        std::cerr << "LowerThan TRUE Constraint " << s_true.get_name() << " = " << val_true.prettyprint() << std::endl;
#endif
        _output_true_constraints_map[lhs] = Utils::Constraint(s_true, val_true);
        
        // 3.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ [ c, +∞]
        // 3.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_false.str()));
        s_false.set_type(t);
        // 3.2.2.- Build the FALSE constraint value
        NBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                Nodecl::Symbol::make(s), 
                Nodecl::Analysis::Range::make(val.shallow_copy(),
                                              Nodecl::Analysis::PlusInfinity::make(),
                                              t), 
                t);
        // 3.2.3.- Build the FALSE constraint and store it
#ifdef RANGES_DEBUG
        std::cerr << "LowerThan FALSE Constraint " << s_false.get_name() << " = " << val_false.prettyprint() << std::endl;
#endif
        _output_false_constraints_map[lhs] = Utils::Constraint(s_false, val_false);
        
        return c;
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::Mod& n)
    {
        // Build the constraint symbol
        std::stringstream ss; ss << get_next_id(NBase::null());
        std::string sym_name = "_x_" + ss.str();
        Symbol s(n.retrieve_context().new_symbol(sym_name));
        Type t = n.get_lhs().get_type();
        s.set_type(t);
        
        // Build the constraint value
        const_value_t* zero = const_value_get_zero(/*num_bytes*/ 4, /*sign*/1);
        const_value_t* one = const_value_get_one(/*num_bytes*/ 4, /*sign*/1);
        NBase rhs = n.get_rhs();
        NBase ub = 
                (rhs.is_constant() ? const_value_to_nodecl(const_value_sub(rhs.get_constant(), one)) 
                                   : Nodecl::Minus::make(rhs.shallow_copy(), const_value_to_nodecl(one), rhs.get_type()));
        NBase val = Nodecl::Analysis::Range::make(const_value_to_nodecl(zero), ub, t);
        
        // Build the constraint
#ifdef RANGES_DEBUG
        std::cerr << "Mod Constraint " << s.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        Utils::Constraint c(s, val);
        Nodecl::Symbol var = Nodecl::Symbol::make(TL::Symbol(n.retrieve_context().new_symbol("_x")));
        var.set_type(t);
        _output_constraints_map[var] = c;
        
        return c;
    }
    
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::ObjectInit& n)
    {
        Nodecl::Symbol lhs = Nodecl::Symbol::make(n.get_symbol(), n.get_locus());
        NBase rhs = n.get_symbol().get_value();
        return visit_assignment(lhs, rhs);
    }
    
    // ++x;    -->    X1 = X0 + 1
    Utils::Constraint ConstraintBuilderVisitor::visit(const Nodecl::Preincrement& n)
    {
        NBase rhs = n.get_rhs();
        ERROR_CONDITION(_input_constraints_map.find(rhs) == _input_constraints_map.end(), 
                        "Some input constraint required for the RHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(rhs);
        Symbol orig_s(Utils::get_nodecl_base(rhs).get_symbol());
        std::string constr_name = orig_s.get_name() + "_" + ss.str();
        Symbol s(n.retrieve_context().new_symbol(constr_name));
        s.set_type(orig_s.get_type());
        
        NBase val = Nodecl::Add::make(Nodecl::Symbol::make(_input_constraints_map[rhs].get_symbol()), 
                                                   const_value_to_nodecl(const_value_get_one(/*num_bytes*/ 4, /*sign*/1)), 
                                                   rhs.get_type());
#ifdef RANGES_DEBUG
        std::cerr << "Preincrement Constraint " << s.get_name() << " = " << val.prettyprint() << std::endl;
#endif
        Utils::Constraint c(s, val);
        _input_constraints_map[rhs] = c;
        _output_constraints_map[rhs] = c;
        
        return c;
    }
    
    // ************************** END Visitor implementing constraint building **************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ******************************* Class implementing constraint graph ******************************** //
    
namespace {
    std::set<CGNode*> convert_cgnodes_map_in_set(CGNode_map nodes_map)
    {
        std::set<CGNode*> result;
        for(CGNode_map::iterator it = nodes_map.begin(); it != nodes_map.end(); ++it)
            result.insert(it->second);
        return result;
    }
}
    
    CGNode::CGNode(const NBase& value)
        : _id(++cgnode_id), _value(value), _entries(), _exits(), 
        _scc_index(-1), _scc_lowlink_index(-1)
    {}
    
    unsigned int CGNode::get_id() const
    { 
        return _id;
    }
    
    NBase CGNode::get_value() const
    { 
        return _value; 
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
    
    CGEdge* CGNode::add_parent(CGNode* parent, NBase predicate)
    {
        CGEdge* e;
        ObjectList<CGNode*> parents = get_parents();
        if(!parents.contains(parent))
        {
            e = new CGEdge(parent, this, predicate);
            _entries.insert(e); 
        }
        else
        {
            for(ObjectList<CGEdge*>::iterator it = _entries.begin(); it != _entries.end(); ++it)
            {
                if((*it)->get_source() == parent)
                {
                    e = *it;
                    break;
                }
            }
        }
        return e;
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
    
    void CGNode::add_exit(CGEdge* e)
    {
        _exits.insert(e);
    }
    
    CGEdge* CGNode::add_child(CGNode* child, NBase predicate)
    {
        CGEdge* e;
        ObjectList<CGNode*> children = get_children();
        if(!children.contains(child))
        {
            e = new CGEdge(this, child, predicate);
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
    
    int CGNode::get_scc_index() const
    {
        return _scc_index;
    }
    
    void CGNode::set_scc_index(int scc_index)
    {
        _scc_index = scc_index;
    }
    
    int CGNode::get_scc_lowlink_index() const
    {
        return _scc_lowlink_index;
    }
    
    void CGNode::set_scc_lowlink_index(int scc_lowlink_index)
    {
        _scc_lowlink_index = scc_lowlink_index;
    }
    
    CGEdge::CGEdge(CGNode* source, CGNode* target, const NBase& predicate)
        : _source(source), _target(target), _predicate(predicate)
    {}
    
    CGNode* CGEdge::get_source() const
    {
        return _source;
    }
    
    CGNode* CGEdge::get_target() const
    {
        return _target;
    }
    
    NBase CGEdge::get_predicate() const
    {
        return _predicate;        
    }
    
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
    
    ConstraintGraph::ConstraintGraph(std::string name)
        : _name(name), _nodes()
    {}
    
    CGNode* ConstraintGraph::insert_node(const NBase& value)
    {
        CGNode* node = get_node(value);
        if(node==NULL)
        {
            node = new CGNode(value);
            _nodes[value] = node;
        }
        return node;
    }
    
    CGNode* ConstraintGraph::get_node(const NBase& value)
    {
        return ((_nodes.find(value)==_nodes.end()) ? NULL 
                                                   : _nodes[value]);
    }
    
    void ConstraintGraph::connect_nodes(CGNode* source, CGNode* target, NBase predicate)
    {
        ObjectList<CGNode*> children = source->get_children();
        if(!children.contains(target))
        {
            CGEdge* e = source->add_child(target, predicate);
            target->add_entry(e);
        }
    }
    
    void ConstraintGraph::print_graph_rec(std::ofstream& dot_file)
    {
        // Print all nodes
        for(CGNode_map::iterator it = _nodes.begin(); it != _nodes.end(); ++it)
            dot_file << "\t" << it->second->get_id() << " [label=\"" << it->second->get_value().prettyprint() << "\"];\n";
        
        // Print all nodes relations
        for(CGNode_map::iterator it = _nodes.begin(); it != _nodes.end(); ++it)
        {
            unsigned int source = it->second->get_id();
            ObjectList<CGEdge*> exits = it->second->get_exits();
            for(ObjectList<CGEdge*>::iterator ite = exits.begin(); ite != exits.end(); ++ite)
            {
                unsigned int target = (*ite)->get_target()->get_id();
                const NBase predicate = (*ite)->get_predicate();
                std::string label = (predicate.is_null() ? "" : " [label=\"" + predicate.prettyprint() + "\"]");
                dot_file << "\t" << source << "->" << target << label << ";\n";
            }
        }
    }
    
    void ConstraintGraph::print_graph()
    {
        // Get a file to print a DOT with the Constraint Graph
        // Create the directory of dot files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/dot/";
        if(stat(directory_name.c_str(), &st) != 0)
        {
            int dot_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if(dot_directory != 0)
                internal_error ("An error occurred while creating the dot directory in '%s'", 
                                directory_name.c_str());
        }
        
        // Create the file where we will store the DOT CG
        std::string dot_file_name = directory_name + _name + "_cg.dot";
        std::ofstream dot_cg;
        dot_cg.open(dot_file_name.c_str());
        if(!dot_cg.good())
            internal_error ("Unable to open the file '%s' to store the CG.", dot_file_name.c_str());
        if(VERBOSE)
            std::cerr << "- CG DOT file '" << dot_file_name << "'" << std::endl;
        dot_cg << "digraph CG {\n";
            dot_cg << "\tcompound=true;\n";
            print_graph_rec(dot_cg);
        dot_cg << "}\n";
        dot_cg.close();
        if(!dot_cg.good())
            internal_error ("Unable to close the file '%s' where CG has been stored.", dot_file_name.c_str());
    }
    
    static bool stack_contains_cgnode(const std::stack<CGNode*>& s, CGNode* n)
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
    
    static void strong_connect(CGNode* n, unsigned int& scc_index, std::stack<CGNode*>& s, std::vector<SCC>& scc_list)
    {
        // Set the depth index for 'n' to the smallest unused index
        n->set_scc_index(scc_index);
        n->set_scc_lowlink_index(scc_index);
        ++scc_index;
        s.push(n);
        
        // Consider the successors of 'n'
        ObjectList<CGNode*> succ = n->get_children();
        for(ObjectList<CGNode*>::iterator it = succ.begin(); it != succ.end(); ++it)
        {
            CGNode* m = *it;
            if(m->get_scc_index() == -1)
            {   // Successor 'm' has not yet been visited: recurse on it
                strong_connect(m, scc_index, s, scc_list);
                n->set_scc_lowlink_index(std::min(n->get_scc_lowlink_index(), m->get_scc_lowlink_index()));
            }
            else if(stack_contains_cgnode(s, m))
            {   // Successor 'm' is in the current SCC
                n->set_scc_lowlink_index(std::min(n->get_scc_lowlink_index(), m->get_scc_index()));
            }
        }   
        
        // If 'n' is a root node, pop the set and generate an SCC
        if((n->get_scc_lowlink_index() == n->get_scc_index()) && !s.empty())
        {
            SCC scc;
            while(!s.empty() && s.top()!=n)
            {
                scc.add_node(s.top());
                s.pop();
            }
            if(!s.empty() && s.top()==n)
            {
                scc.add_node(s.top());
                s.pop();
            }
            scc_list.push_back(scc);
        }
    }
    
    // Implementation of the Tarjan's strongly connected components algorithm
    std::vector<SCC> ConstraintGraph::extract_strongly_connected_components()
    {
        std::vector<SCC> scc_list;
        std::stack<CGNode*> s;
        unsigned int scc_index = 0;
        
        std::set<CGNode*> nodes = convert_cgnodes_map_in_set(_nodes);
        for(std::set<CGNode*>::iterator it = nodes.begin(); it != nodes.end(); ++it)
        {
            if((*it)->get_scc_index() == -1)
                strong_connect(*it, scc_index, s, scc_list);
        }
        
        return scc_list;
    }
    
    // ***************************** END class implementing constraint graph ****************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ******************************** Class implementing range analysis ********************************* //
    
    RangeAnalysis::RangeAnalysis(ExtensibleGraph* pcfg)
        : _pcfg(pcfg), _cg(new ConstraintGraph(pcfg->get_name()))
    {}
    
    void RangeAnalysis::compute_range_analysis()
    {   // Compute the constraints of the current graph
        set_parameters_constraints();
        
        Node* entry = _pcfg->get_graph()->get_graph_entry_node();
        compute_initial_constraints(entry);
        ExtensibleGraph::clear_visits(entry);
        
        propagate_constraints_from_backwards_edges(entry);
        ExtensibleGraph::clear_visits(entry);
        
        create_constraint_graph(entry);
        ExtensibleGraph::clear_visits(entry);
        if(VERBOSE)
            _cg->print_graph();
        std::vector<SCC> scc_list = _cg->extract_strongly_connected_components();
#ifdef RANGES_DEBUG
        for(std::vector<SCC>::iterator it = scc_list.begin(); it != scc_list.end(); ++it)
        {
            std::vector<CGNode*> scc_nodes = it->get_nodes();
            std::cerr << "SCC : ";
            for(std::vector<CGNode*>::iterator itt = scc_nodes.begin(); itt != scc_nodes.end(); )
            {
                std::cerr << (*itt)->get_value().prettyprint() << ", ";
                ++itt;
                if(itt != scc_nodes.end())
                    std::cerr << ",";
            }
            std::cerr << std::endl;
        }
#endif
    }
    
    // Set an constraint to the graph entry node for each parameter of the function
    void RangeAnalysis::set_parameters_constraints()
    {
        Symbol func_sym = _pcfg->get_function_symbol();
        if(!func_sym.is_valid())    // The PCFG have been built for something other than a FunctionCode
            return;
        
        // Set the constraints for each parameter to both nodes
        Utils::ConstraintMap constraints;
        Node* entry = _pcfg->get_graph()->get_graph_entry_node();
        const ObjectList<Symbol> params = func_sym.get_function_parameters();
        for(ObjectList<Symbol>::const_iterator it = params.begin(); it != params.end(); ++it)
        {
            Type t = it->get_type();
            Nodecl::Symbol param_s = Nodecl::Symbol::make(*it);
            param_s.set_type(t);
            
            // Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(param_s);
            std::string constr_name = it->get_name() + "_" + ss.str();
            Symbol s(it->get_scope().new_symbol(constr_name));
            s.set_type(t);
            
            // Get the value for the constraint
            NBase val = Nodecl::Analysis::Range::make(Nodecl::Analysis::MinusInfinity::make(), Nodecl::Analysis::PlusInfinity::make(), t);
            
            // Build the constraint and insert it in the constraints map
            constraints[param_s] = Utils::Constraint(s, val);
        }
        // Attach the constraints to the entry node of the graph
        entry->set_constraints_map(constraints);
    }
    
    // This is a breadth-first search because for a given node we need all its parents 
    // (except from those that come from back edges, for they will be recalculated later 'propagate_constraints_from_backwards_edges')
    // to be computed before propagating their information to the node
    void RangeAnalysis::compute_initial_constraints(Node* entry)
    {
        ERROR_CONDITION(!entry->is_entry_node(), 
                        "Expected ENTRY node but found %s node.", entry->get_type_as_string().c_str());
        
        ObjectList<Node*> currents(1, entry);
        while(!currents.empty())
        {
            ObjectList<Node*>::iterator it = currents.begin();
            while(it != currents.end())
            {
                Node* c = *it;
                if((*it)->is_visited())
                {
                    currents.erase(it);
                    continue;
                }
                else
                {
                    (*it)->set_visited(true);
                    ++it;
                }
                
                if(c->is_graph_node())
                {
                    // recursively compute the constraints for the inner nodes
                    compute_initial_constraints(c->get_graph_entry_node());
                    // propagate constraint from the inner nodes (summarized in the exit node) to the graph node
                    c->set_propagated_constraints_map(c->get_graph_exit_node()->get_propagated_constraints_map());
                    c->add_propagated_constraints_map(c->get_graph_exit_node()->get_constraints_map());
                }
                else
                {
                    // Collect the constraints computed from all the parents
                    Utils::ConstraintMap input_constraints_map, new_input_constraints;
                    ObjectList<Node*> parents = (c->is_entry_node() ? c->get_outer_node()->get_parents() : c->get_parents());
                    for(ObjectList<Node*>::iterator itt = parents.begin(); itt != parents.end(); ++itt)
                    {
                        Utils::ConstraintMap it_constraints_map = (*itt)->get_all_constraints_map();
                        for(Utils::ConstraintMap::iterator ittt = it_constraints_map.begin(); 
                            ittt != it_constraints_map.end(); ++ittt)
                        {
                            if(input_constraints_map.find(ittt->first)==input_constraints_map.end() && 
                               new_input_constraints.find(ittt->first)==new_input_constraints.end())
                            {   // No constraints already found for variable ittt->first
                                input_constraints_map[ittt->first] = ittt->second;
                            }
                            else
                            {   // Merge the constraint that already existed with the new one
                                // Get the existing constraint
                                Utils::Constraint old_constraint;
                                if(input_constraints_map.find(ittt->first)!=input_constraints_map.end())
                                    old_constraint = input_constraints_map[ittt->first];
                                else
                                    old_constraint = new_input_constraints[ittt->first];
                                NBase current_constraint = ittt->second.get_constraint();
                                TL::Symbol current_symbol = ittt->second.get_symbol();
                                // If the new constraint is different from the old one, compute the combination of both
                                if(!Nodecl::Utils::structurally_equal_nodecls(old_constraint.get_constraint(), current_constraint, 
                                                                            /*skip_conversion_nodes*/true))
                                {
                                    // Get a new symbol for the new constraint
                                    NBase lhs = ittt->first;
                                    std::stringstream ss; ss << get_next_id(lhs);
                                    Symbol orig_s(lhs.get_symbol());
                                    std::string constr_name = orig_s.get_name() + "_" + ss.str();
                                    Symbol s(lhs.retrieve_context().new_symbol(constr_name));
                                    s.set_type(orig_s.get_type());
                                    
                                    // Build a the value of the new constraint
                                    NBase new_constraint_val;
                                    if(old_constraint.get_constraint().is<Nodecl::Analysis::Phi>())
                                    {   // Attach a new element to the list inside the node Phi
                                        Nodecl::List expressions = old_constraint.get_constraint().as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                                        expressions.append(ittt->first);
                                        new_constraint_val = Nodecl::Analysis::Phi::make(expressions, ittt->first.get_type());
                                    }
                                    else
                                    {   // Create a new node Phi with the combination of the old constraint and the new one
                                        Symbol tmp_s1(old_constraint.get_symbol());
                                        Nodecl::Symbol tmp1 = Nodecl::Symbol::make(tmp_s1);
                                        tmp1.set_type(tmp_s1.get_type());
                                        
                                        Symbol tmp_s2(ittt->second.get_symbol());
                                        Nodecl::Symbol tmp2 = Nodecl::Symbol::make(tmp_s2);
                                        tmp2.set_type(tmp_s2.get_type());
                                        
                                        Nodecl::List expressions = Nodecl::List::make(tmp1, tmp2);
                                        new_constraint_val = Nodecl::Analysis::Phi::make(expressions, ittt->second.get_constraint().get_type());
                                    }
                                    
                                    // Remove the old constraint (if it was in the new_input_constraints map, it will be deleted with the insertion)
                                    if(input_constraints_map.find(ittt->first)!=input_constraints_map.end())
                                        input_constraints_map.erase(input_constraints_map.find(ittt->first));
                                    // Build the actual constraint and insert it in the proper list
                                    new_input_constraints[ittt->first] = Utils::Constraint(s, new_constraint_val);
                                }
                            }
                        }
                    }
                    
                    // Propagate constraints from parent nodes to the current node
                    c->set_propagated_constraints_map(input_constraints_map);
                    c->add_constraints_map(new_input_constraints);
                    if(c->has_statements())
                    {
                        // Compute the constraints of the current node
                        // Note: take into account the constraints the node may already have (if it is the TRUE or FALSE child of a conditional)
                        Utils::ConstraintMap current_constraints_map = c->get_constraints_map();
#ifdef RANGES_DEBUG
                        std::cerr << "NODE " << c->get_id() << std::endl;
#endif
                        ConstraintBuilderVisitor cbv(input_constraints_map, current_constraints_map);
                        NodeclList stmts = c->get_statements();
                        for(NodeclList::iterator itt = stmts.begin(); itt != stmts.end(); ++itt)
                            cbv.compute_constraints(*itt);
                        c->add_constraints_map(cbv.get_output_constraints_map());
                        
                        // Set true/false output constraints to current children, if applies
                        ObjectList<Edge*> exits = c->get_exit_edges();
                        if(exits.size()==2 &&
                            ((exits[0]->is_true_edge() && exits[1]->is_false_edge()) || (exits[1]->is_true_edge() && exits[0]->is_false_edge())))
                        {
                            Utils::ConstraintMap out_true_constraints_map = cbv.get_output_true_constraints_map();
                            Utils::ConstraintMap out_false_constraints_map = cbv.get_output_false_constraints_map();
                            
                            // We always propagate to the TRUE edge
                            Node* true_node = (exits[0]->is_true_edge() ? exits[0]->get_target() : exits[1]->get_target());
                            Node* real_true_node = true_node;
                            while(true_node->is_exit_node())
                                true_node = true_node->get_outer_node()->get_children()[0];
                            if(true_node->is_graph_node())
                                true_node = true_node->get_graph_entry_node();
                            true_node->add_constraints_map(out_true_constraints_map);
                            
                            // For the if_else cases, we only propagate to the FALSE edge when it contains statements ('else' statements)
                            Node* false_node = (exits[0]->is_true_edge() ? exits[1]->get_target() : exits[0]->get_target());
                            ObjectList<Node*> real_true_node_children = real_true_node->get_children();
                            if((false_node->get_entry_edges().size() == 1) || !real_true_node_children.contains(false_node) )
                            {   // If the true_node is a parent of the false_node, then there are no statements
                                // Avoid cases where the FALSE edge leads to the end of the graph
                                ObjectList<Node*> children;
                                while(false_node->is_exit_node())
                                {
                                    children = false_node->get_outer_node()->get_children();
                                    if(!children.empty())
                                        false_node = children[0];
                                    else 
                                    {
                                        false_node = NULL;
                                        break;
                                    }
                                }
                                if(false_node!=NULL)
                                {
                                    if(false_node->is_graph_node())
                                        false_node = false_node->get_graph_entry_node();
                                    false_node->add_constraints_map(out_false_constraints_map);
                                }
                            }
                        }
                    }
                }
            }
            
            ObjectList<Node*> next_currents;
            for(ObjectList<Node*>::iterator itt = currents.begin(); itt != currents.end(); ++itt)
                next_currents.append((*itt)->get_children());
            currents = next_currents;
        }
    }
    
    static void recompute_node_constraints(Node* n, Utils::ConstraintMap new_constraint_map)
    {
        Utils::ConstraintMap current_constraint_map = n->get_constraints_map();
        for(Utils::ConstraintMap::iterator it = new_constraint_map.begin(); it != new_constraint_map.end(); ++it)
        {
            if((current_constraint_map.find(it->first) != current_constraint_map.end()) && 
                (current_constraint_map[it->first] != it->second))
            {
                NBase c1 = current_constraint_map[it->first].get_constraint().shallow_copy();
                NBase c2 = Nodecl::Symbol::make(it->second.get_symbol());
                Nodecl::List expressions = Nodecl::List::make(c1, c2);
                NBase c_val = Nodecl::Analysis::Phi::make(expressions, Utils::get_nodecl_base(it->first).get_symbol().get_type());
                // Set the new constraint
                current_constraint_map[it->first] = Utils::Constraint(current_constraint_map[it->first].get_symbol(), c_val);
            }
        }
        n->set_constraints_map(current_constraint_map);
    }
    
    void RangeAnalysis::propagate_constraints_from_backwards_edges(Node* n)
    {
        if(n->is_visited())
            return;
        n->set_visited(true);
        
        if(n->is_graph_node())
            propagate_constraints_from_backwards_edges(n->get_graph_entry_node());
        
        // Check the exit edges looking for back edges
        ObjectList<Edge*> exit_edges = n->get_exit_edges();
        for(ObjectList<Edge*>::iterator it = exit_edges.begin(); it != exit_edges.end(); ++it)
        {
            if((*it)->is_back_edge())
                recompute_node_constraints((*it)->get_target(), n->get_all_constraints_map());
        }
        
        // Recursively treat the children
        ObjectList<Node*> children = n->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            propagate_constraints_from_backwards_edges(*it);
    }
    
    /*! Generate a constraint graph from the PCFG and the precomputed Constraints for each node
     *  A Constraint Graph is created as follows:
     *  - The nodes of the graphs are:
     *    - A. A node for each SSA variable from Constraints: c0 = ...             Node    c0
     *    - B. A node for each Constraint Value which is a Range: c1 = [lb, ub]    Node [lb, ub]
     *  - The edges are built following the rules below:
     *    - C. Si Constraint Phi: c0 = Phi(c1, c2)                                 Edge    c0 ---------------> c2
     *                                                                             Edge    c1 ---------------> c2
     *    - D. Si Constraint Intersection: c1 = c0 ∩ [lb, ub]                      Edge    c0 ----[lb, ub]---> c1
     *    - E. Si Constraint Range: c0 = [lb, ub]                                  Edge [lb, ub]-------------> c1
     *    - F. Si Constraint Arithmetic op: c1 = c0 + 1                            Edge    c1 ------ 1 ------> c0
     */
    void RangeAnalysis::create_constraint_graph(Node* n)
    {
        if(!n->is_visited())
        {
            n->set_visited(true);
            
            const Utils::ConstraintMap constraints_map = n->get_constraints_map();
            if(!constraints_map.empty())
            {
                for(Utils::ConstraintMap::const_iterator it = constraints_map.begin(); it != constraints_map.end(); ++it)
                {
                    Utils::Constraint c = it->second;
                    Nodecl::Symbol s = Nodecl::Symbol::make(c.get_symbol());
                    NBase val = c.get_constraint().no_conv();
                    
                    // Insert in the CG the nodes corresponding to the current Constraint
                    // A. Create a new node in the Contraint Graph for the current Constraint (if does not exist yet)
                    CGNode* target = _cg->insert_node(s);
                    // B. Create a new node if the Constraint Value is a Range
                    CGNode* source;
                    if(val.is<Nodecl::Analysis::Range>())
                        source = _cg->insert_node(val);
                    
                    // Insert in the CG the edges corresponding to the current Constraint
                    if(val.is<Nodecl::Symbol>())
                    {
                        source = _cg->get_node(val);
                        if(source==NULL)
                            source = _cg->insert_node(val);
                        _cg->connect_nodes(source, target, Nodecl::Text::make("="));
                    }
                    else if(val.is<Nodecl::Analysis::Phi>())
                    {   // C.
                        Nodecl::List expressions = val.as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                        for(Nodecl::List::iterator ite = expressions.begin(); ite != expressions.end(); ++ite)
                        {
                            source = _cg->get_node(*ite);
                            if(source==NULL)
                                source = _cg->insert_node(*ite);
                            _cg->connect_nodes(source, target);
                        }
                    }
                    else if(val.is<Nodecl::Analysis::RangeIntersection>())
                    {   // D.
                        NBase lhs = val.as<Nodecl::Analysis::RangeIntersection>().get_lhs().no_conv();
                        const NBase rhs = val.as<Nodecl::Analysis::RangeIntersection>().get_rhs();
                        ERROR_CONDITION(!lhs.is<Nodecl::Symbol>() || !rhs.is<Nodecl::Analysis::Range>(), 
                                        "Expected a RangeIntersection with the structure 'c0 ∩ [lb, ub]' but found %s.\n", 
                                        val.prettyprint().c_str());
                        
                        source = _cg->get_node(lhs);
                        if(source==NULL)
                            source = _cg->insert_node(lhs);
                        _cg->connect_nodes(source, target, rhs.shallow_copy());
                    }
                    else if(val.is<Nodecl::Analysis::Range>())
                    {   // E.
                        _cg->connect_nodes(source, target);
                    }
                    else if(val.is<Nodecl::Add>())
                    {
                        NBase lhs = val.as<Nodecl::Add>().get_lhs().no_conv();
                        const NBase rhs = val.as<Nodecl::Add>().get_rhs();
                        ERROR_CONDITION(!lhs.is<Nodecl::Symbol>() || (!rhs.is_constant() && !rhs.is<Nodecl::Symbol>()), 
                                        "Expected a Nodecl::Add with the structure 'c1 + v' but found %s.\n", 
                                        val.prettyprint().c_str());
                        source = _cg->get_node(lhs);
                        if(source==NULL)
                            source = _cg->insert_node(lhs);
                        _cg->connect_nodes(source, target, rhs.shallow_copy());
                    }
                    else if(val.is<Nodecl::Minus>())
                    {
                        NBase lhs = val.as<Nodecl::Add>().get_lhs().no_conv();
                        const NBase rhs = val.as<Nodecl::Add>().get_rhs();
                        ERROR_CONDITION(!lhs.is<Nodecl::Symbol>() || (!rhs.is_constant() && !rhs.is<Nodecl::Symbol>()), 
                                        "Expected a Nodecl::Add with the structure 'c1 + v' but found %s.\n", 
                                        val.prettyprint().c_str());
                        source = _cg->get_node(lhs);
                        if(source==NULL)
                            source = _cg->insert_node(lhs);
                        NBase neg_rhs = Nodecl::Neg::make(rhs.shallow_copy(), rhs.get_type());
                        _cg->connect_nodes(source, target, neg_rhs);
                    }
                    else
                    {
                        internal_error("Unexpected type of Constraint value '%s' for constraint '%s'.\n", 
                                       ast_print_node_type(val.get_kind()), val.prettyprint().c_str());
                    }
                }
            }
            
            // Iterate over nested nodes if necessary
            if(n->is_graph_node())
                create_constraint_graph(n->get_graph_entry_node());
            
            // Keep traversing the PCFG through the node's children
            ObjectList<Node*> children = n->get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                create_constraint_graph(*it);
        }
    }
    
    // ****************************** End class implementing range analysis ******************************* //
    // **************************************************************************************************** //
}
}
