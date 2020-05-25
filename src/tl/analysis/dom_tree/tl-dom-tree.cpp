/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
  implied warranty of MERCHANTABILITY or Fstd::set<DomTreeNode*> get_predecessors();
        std::set<DomTreeNode*> get_successors()ITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.

  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

#include <stack>
#include <fstream>
#include <sstream>
#include <sys/stat.h>
#include <unistd.h>

#include "tl-analysis-utils.hpp"
#include "tl-datareference.hpp"
#include "tl-dom-tree.hpp"
#include "tl-nodecl-utils-c.hpp"

namespace TL {
namespace Analysis {

    DomTreeNode::DomTreeNode(Symbol func_sym)
        : _func_sym(func_sym)
    {}

    void DomTreeNode::add_predecessor(DomTreeNode* pred)
    {
        _predecessors.insert(pred);
    }

    void DomTreeNode::add_successor(DomTreeNode* succ)
    {
        _successors.insert(succ);
    }

    Symbol DomTreeNode::get_function_symbol() const
    {
        return _func_sym;
    }

    Nodecl::FunctionCode DomTreeNode::get_function_code()
    {
        return _func_sym.get_function_code().as<Nodecl::FunctionCode>();
    }

    std::set<DomTreeNode*> DomTreeNode::get_predecessors()
    {
        return _predecessors;
    }

    std::set<DomTreeNode*> DomTreeNode::get_successors()
    {
        return _successors;
    }

    DominatorTree::DominatorTree(std::string name, const Nodecl::NodeclBase& nodecl)
        : _name(name), _roots(), _leafs(), _func_syms()
    {
        create_dominator_tree(nodecl);

        if (debug_options.print_dt)
            print_to_dot();
    }

    void DominatorTree::create_dominator_tree(const Nodecl::NodeclBase& nodecl)
    {
        std::string current_file = nodecl.get_filename();
        ObjectList<Nodecl::NodeclBase> func_codes
                = Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::FunctionCode>(nodecl);
        ObjectList<Nodecl::NodeclBase> user_func_codes;
        for (ObjectList<Nodecl::NodeclBase>::iterator it = func_codes.begin();
             it != func_codes.end(); ++it)
        {
            if (it->get_filename() == current_file)
            {
                user_func_codes.append(*it);
                _func_syms.append(it->as<Nodecl::FunctionCode>().get_symbol());
            }
        }

        std::map<Symbol, DomTreeNode*> sym_to_dtn;
        std::stack<Nodecl::NodeclBase> funcs_to_visit;
        for (ObjectList<Nodecl::NodeclBase>::iterator it = user_func_codes.begin();
             it != user_func_codes.end(); ++it)
        {
            funcs_to_visit.push(*it);
        }

        while (!funcs_to_visit.empty())
        {
            Nodecl::FunctionCode current_func = funcs_to_visit.top().as<Nodecl::FunctionCode>();
            Symbol current_sym = current_func.get_symbol();
            funcs_to_visit.pop();

            DomTreeNode* dtn;
            if (sym_to_dtn.find(current_sym) == sym_to_dtn.end())
            {
                dtn = new DomTreeNode(current_sym);
                sym_to_dtn[current_sym] = dtn;
                _roots.insert(dtn);
            }
            else
            {
                dtn = sym_to_dtn[current_sym];
            }

            ObjectList<Nodecl::NodeclBase> current_func_nested_funcs
                    = Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::FunctionCall>(current_func);
            bool has_children = false;
            for (ObjectList<Nodecl::NodeclBase>::iterator it = current_func_nested_funcs.begin();
                    it != current_func_nested_funcs.end() && !has_children; ++it)
            {
                Symbol nested_sym = it->as<Nodecl::FunctionCall>().get_called().get_symbol();
                if (_func_syms.contains(nested_sym))
                    has_children = true;
            }
            if (!has_children)
                _leafs.insert(dtn);

            for (ObjectList<Nodecl::NodeclBase>::iterator it = current_func_nested_funcs.begin();
                    it != current_func_nested_funcs.end(); ++it)
            {
                Symbol nested_sym = it->as<Nodecl::FunctionCall>().get_called().get_symbol();
                if (!_func_syms.contains(nested_sym))
                    continue;

                if (current_sym == nested_sym)
                {   // Recursive call
                    dtn->add_successor(dtn);
                    dtn->add_predecessor(dtn);
                    if (current_func_nested_funcs.size() == 1)
                    {
                        _leafs.insert(dtn);
                    }
                }
                else
                {
                    DomTreeNode* nested_dtn;
                    if (sym_to_dtn.find(nested_sym) == sym_to_dtn.end())
                    {
                        nested_dtn = new DomTreeNode(nested_sym);
                        sym_to_dtn[nested_sym] = nested_dtn;
                    }
                    else
                    {
                        nested_dtn = sym_to_dtn[nested_sym];
                    }
                    dtn->add_successor(nested_dtn);
                    nested_dtn->add_predecessor(dtn);

                    _roots.erase(nested_dtn);
                }
            }
        }
    }

    std::string DominatorTree::get_name() const
    {
        return _name;
    }

    std::set<DomTreeNode*> DominatorTree::get_roots() const
    {
        return _roots;
    }

    std::set<DomTreeNode*> DominatorTree::get_leafs() const
    {
        return _leafs;
    }

    ObjectList<Symbol> DominatorTree::get_function_symbols() const
    {
        return _func_syms;
    }

    void DominatorTree::print_to_dot()
    {
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
                internal_error ("An error occurred while creating the dot files directory in '%s'", directory_name.c_str());
        }

        // Generate the full path to the file where to store the PCFG
        std::string dot_file_name = directory_name + _name + "_dt.dot";
        std::ofstream dot_dt;
        dot_dt.open(dot_file_name.c_str());
        if(!dot_dt.good())
            internal_error ("Unable to open the file '%s' to store the Dominator Tree.", dot_file_name.c_str());
        dot_dt << "digraph DT {\n";
            dot_dt << "\tcompound=true;\n";
            std::set<DomTreeNode*> visited;
            std::stack<DomTreeNode*> dtnodes;
            std::cerr << "Printing dot with " << _roots.size() << " roots" << std::endl;
            for (std::set<DomTreeNode*>::iterator it = _roots.begin(); it != _roots.end(); ++it)
            {
                dtnodes.push(*it);
            }
            while (!dtnodes.empty())
            {
                DomTreeNode* n = dtnodes.top();
                dtnodes.pop();
                dot_dt << n->get_function_symbol().get_name()  << "\n";
                visited.insert(n);
                std::set<DomTreeNode*> succ = n->get_successors();
                std::cerr << "Function " << n->get_function_symbol().get_name() << " has " << succ.size() << " successors" << std::endl;
                for (std::set<DomTreeNode*>::iterator it = succ.begin(); it != succ.end(); ++it)
                {
                    dot_dt << n->get_function_symbol().get_name() << " -> " << (*it)->get_function_symbol().get_name() << "\n";
                    if (visited.find(*it) == visited.end())
                    {
                        dtnodes.push(*it);
                    }
                }
            }
        dot_dt << "}\n";
    }
}
}
