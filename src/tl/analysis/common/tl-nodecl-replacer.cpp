/*--------------------------------------------------------------------
 ( C) Copyright 2006-2015 Barcelona Supercomputing Center             *
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

#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-replacer.hpp"

namespace TL {
namespace Analysis {

    NodeclReplacer::NodeclReplacer(SymToNodeclMap sym_rename_map)
        : _sym_rename_map(sym_rename_map), _nodecl_rename_map()
    {}

    NodeclReplacer::NodeclReplacer(VarToNodeclMap nodecl_rename_map)
        : _sym_rename_map(), _nodecl_rename_map(nodecl_rename_map)
    {}

    void NodeclReplacer::visit(const Nodecl::ArraySubscript& n)
    {
        // First try replacement
        if (!_nodecl_rename_map.empty())
        {
            if (_nodecl_rename_map.find(n) != _nodecl_rename_map.end())
            {
                n.replace(_nodecl_rename_map[n]);
                return;
            }
        }
        // If no replacement performed, then traverse the children
        walk(n.get_subscripted());
        walk(n.get_subscripts());
    }

    void NodeclReplacer::visit(const Nodecl::ClassMemberAccess& n)
    {
        // First try replacement
        if (!_nodecl_rename_map.empty())
        {
            if (_nodecl_rename_map.find(n) != _nodecl_rename_map.end())
            {
                n.replace(_nodecl_rename_map[n]);
                return;
            }
        }
        // If no replacement performed, then traverse the children
        walk(n.get_lhs());
        walk(n.get_member());
    }

    void NodeclReplacer::visit(const Nodecl::Symbol& n)
    {
        if (!_sym_rename_map.empty())
        {
            Symbol s(n.get_symbol());
            if(_sym_rename_map.find(s) != _sym_rename_map.end())
            {
                n.replace(_sym_rename_map[s]);
            }
        }
        else
        {
            if(_nodecl_rename_map.find(n) != _nodecl_rename_map.end())
            {
                n.replace(_nodecl_rename_map[n]);
            }
        }
    }

}
}
