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

#include "cxx-cexpr.h"
#include "cxx-codegen.h"

#include "tl-nodecl-utils.hpp"
#include "tl-induction-variables-data.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

    // ********************************************************************************************* //
    // ************************* Class representing and induction variable ************************* //

    InductionVar::InductionVar(const NBase& var)
        : _var(var), _lb(), _ub(), _incr(NBase::null()), _incrs()
    {}

    InductionVar::InductionVar(const NBase& var, InductionVarType type, const NBase& family)
        : _var(var), _lb(), _ub(), _incr(NBase::null()), _incrs(), _type(type), _family(family)
    {}

    NBase InductionVar::get_variable() const
    {
        return _var;
    }

    void InductionVar::set_variable(const NBase& var)
    {
        _var = var;
    }

    NodeclSet InductionVar::get_lb() const
    {
        return _lb;
    }

    void InductionVar::set_lb(const NBase& lb)
    {
        _lb.clear();
        _lb.insert(lb);
    }

    void InductionVar::set_lb(const NodeclSet& lb)
    {
        _lb = lb;
    }

    NodeclSet InductionVar::get_ub() const
    {
        return _ub;
    }

    void InductionVar::set_ub(const NBase& ub)
    {
        _ub.clear();
        _ub.insert(ub);
    }

    void InductionVar::set_ub(const NodeclSet& ub)
    {
        _ub = ub;
    }

    NBase InductionVar::get_increment() const
    {
        return _incr;
    }

    void InductionVar::set_increment(const NBase& incr)
    {
        _incr = incr;
    }

    bool InductionVar::is_increment_one() const
    {
        return (_incr.is_constant() && (const_value_is_one(_incr.get_constant())));
    }

    ObjectList<NBase> InductionVar::get_increment_list() const
    {
        return _incrs;
    }

    void InductionVar::set_increment_list(const ObjectList<NBase>& incr_list)
    {
        _incrs.insert(incr_list);
    }

    bool InductionVar::is_basic()
    {
        return (_type == BASIC_IV);
    }

    std::string InductionVar::get_type_as_string() const
    {
        switch (_type)
        {
            case BASIC_IV:      return "BASIC_IV";
            case DERIVED_IV:    return "DERIVED_IV";
            default:            return "";
        }
    }

    NBase InductionVar::get_family() const
    {
        return _family;
    }

    bool InductionVar::operator==(const InductionVar& iv) const
    {
        bool equal_bounds = (_lb.size() == iv._lb.size()) && (_ub.size() == iv._ub.size());
        if (!equal_bounds)
            return false;

        // Compare the LBs
        NodeclSet::iterator it = _lb.begin();
        NodeclSet::iterator it_iv = iv._lb.begin();
        for (; it != _lb.end() && equal_bounds; ++it, ++it_iv)
            equal_bounds = equal_bounds || Nodecl::Utils::structurally_equal_nodecls(*it, *it_iv);

        // Compare the UBs
        it = _ub.begin();
        it_iv = iv._ub.begin();
        for (; it != _ub.end() && equal_bounds; ++it, ++it_iv)
            equal_bounds = equal_bounds || Nodecl::Utils::structurally_equal_nodecls(*it, *it_iv);

        // Compare the other fields
        return (equal_bounds
                    && Nodecl::Utils::structurally_equal_nodecls(_var, iv._var)
                    && Nodecl::Utils::structurally_equal_nodecls(_incr, iv._incr)
                    && (_type == iv._type) && (_family == iv._family));
    }

    std::string InductionVar::print_iv_as_range() const
    {
        return ("[" + prettyprint_iv_boundary_list(_lb) +
                ":" + prettyprint_iv_boundary_list(_ub) +
                ":" + (_incr.is_null() ? "NULL" : _incr.prettyprint()) + "]");
    }
    
    // *********************** END class representing and induction variable *********************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ********************************* Induction Variables utils ********************************* //

    std::string prettyprint_iv_boundary_list(const NodeclSet& boundaries)
    {
        std::string boundaries_str;
        if (boundaries.empty())
        {
            boundaries_str = "NULL";
        }
        else
        {
            for (NodeclSet::const_iterator it = boundaries.begin(); it != boundaries.end(); )
            {
                boundaries_str += it->prettyprint();
                ++it;
                if (it != boundaries.end())
                    boundaries_str += ",";
            }
        }
        return boundaries_str;
    }

    static std::string prettyprint_induction_var(InductionVar* iv)
    {
        std::string result;
        std::string lb_str = prettyprint_iv_boundary_list(iv->get_lb());
        std::string ub_str = prettyprint_iv_boundary_list(iv->get_ub());
        NBase incr = iv->get_increment();
        result += iv->get_variable().prettyprint()
                + "[ " + lb_str
                + ":"  + ub_str
                + ":"  + (incr.is_null() ? "NULL" : incr.prettyprint())
                + ":"  + iv->get_type_as_string()
                + " ]";
        return result;
    }
    
    std::string prettyprint_induction_vars(const InductionVarList& iv_list, bool to_dot)
    {
        std::string result = "";
        std::string eol = (to_dot ? "\\n" : "\n");
        for(InductionVarList::const_iterator it = iv_list.begin(); it != iv_list.end(); )
        {
            result += prettyprint_induction_var(*it);
            ++it;
            if(it != iv_list.end())
                result += eol;
        }
        
        return result;
    }

    void print_induction_vars(const InductionVarsPerNode& iv_list)
    {
        std::cerr << "    Induction Variables: " << std::endl;
        for (InductionVarsPerNode::const_iterator it = iv_list.begin(); it != iv_list.end(); ++it)
        {
            std::cerr << "        * " << it->first << ": " << prettyprint_induction_var(it->second) << std::endl;
        }
    }

    bool induction_variable_list_contains_variable(const InductionVarList& iv_list, const NBase& var)
    {
        for (InductionVarList::const_iterator it = iv_list.begin(); it != iv_list.end(); ++it)
            if (Nodecl::Utils::structurally_equal_nodecls((*it)->get_variable(), var, /*skip_conversions*/ true))
                return true;
        return false;
    }

    InductionVar* get_induction_variable_from_list(const InductionVarList& ivs, const NBase& var)
    {
        for (InductionVarList::const_iterator it = ivs.begin(); it != ivs.end(); ++it)
            if (Nodecl::Utils::structurally_equal_nodecls((*it)->get_variable(), var, /*skip conversion*/ true))
                return *it;
        return NULL;
    }

    InductionVar* get_induction_variable_from_list(const Utils::InductionVarsPerNode& ivs, const NBase& var)
    {
        for (InductionVarsPerNode::const_iterator it = ivs.begin(); it != ivs.end(); ++it)
            if (Nodecl::Utils::structurally_equal_nodecls(it->second->get_variable(), var, /*skip conversion*/ true))
                return it->second;
        return NULL;
    }

    // ******************************* END Induction Variables utils ******************************* //
    // ********************************************************************************************* //
}
}
}
