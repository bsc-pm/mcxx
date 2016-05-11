/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona* Supercomputing Center             *
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

#ifndef TL_SSA_HPP
#define TL_SSA_HPP

#include <map>

#include "cxx-cexpr.h"
#include "tl-analysis-utils.hpp"
#include "tl-ranges-common.hpp"
#include "tl-scope.hpp"

namespace TL {
namespace Analysis {
    
    typedef std::map<NBase, Utils::Constraint, Nodecl::Utils::Nodecl_structural_less> VarToConstraintMap;


    // ********** Variables and methods to simulate SSA during the Constraint Graph construction ********** //

    extern Scope ssa_scope;

    extern std::map<Symbol, NBase> ssa_to_original_var;

    unsigned int get_next_id(const NBase& n);

    struct CompareString {
        bool operator()(const std::string& first, const std::string& second) {
            return first.size() > second.size();
        }
    };

    std::string get_array_subscript_string(
            const Nodecl::ArraySubscript& n,
            /*in*/ VarToConstraintMap& input_constraints);
    std::string get_class_member_string(
        const Nodecl::ClassMemberAccess& n,
        /*in*/ VarToConstraintMap& input_constraints);

}
}

#endif      // TL_SSA_HPP
