/*--------------------------------------------------------------------
 ( C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#ifndef TL_RENAME_VISITOR_HPP
#define TL_RENAME_VISITOR_HPP

#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Analysis {

    typedef std::map<Symbol, Nodecl::NodeclBase> SymToNodeclMap;
    typedef std::map<Nodecl::NodeclBase, Nodecl::NodeclBase, Nodecl::Utils::Nodecl_structural_less> VarToNodeclMap;
    
    class LIBTL_CLASS NodeclReplacer : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        SymToNodeclMap _sym_rename_map;     // Map of possible replacements where
                                                // - the key is the symbol to be replaced and
                                                // - the value is the nodecl to use in the replacement
        VarToNodeclMap _nodecl_rename_map;  // Map of possible replacements where
                                                // - the key is the symbol to be replaced and
                                                // - the value is the nodecl to use in the replacement

    public:
        // *** Constructor *** //
        NodeclReplacer(SymToNodeclMap sym_rename_map);
        NodeclReplacer(VarToNodeclMap nodecl_rename_map);

        // *** Visiting methods *** //
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
        Ret visit(const Nodecl::Symbol& n);
    };

}
}

#endif      // TL_RENAME_VISITOR_HPP
