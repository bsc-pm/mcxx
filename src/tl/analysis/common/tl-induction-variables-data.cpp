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
#include "cxx-codegen.h"

#include "tl-nodecl-utils.hpp"
#include "tl-induction-variables-data.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

    // ********************************************************************************************* //
    // ************************* Class representing and induction variable ************************* //

    InductionVar::InductionVar(NBase var)
        : _var(var), _lb(NBase::null()), _ub(NBase::null()),
          _incr(NBase::null()), _incrs()
    {}

    InductionVar::InductionVar(NBase var, InductionVarType type, NBase family)
        : _var(var), _lb(NBase::null()), _ub(NBase::null()),
          _incr(NBase::null()), _incrs(), _type(type), _family(family)
    {}

    NBase InductionVar::get_variable() const
    {
        return _var;
    }

    void InductionVar::set_variable(NBase var)
    {
        _var = var;
    }

    NBase InductionVar::get_lb() const
    {
        return _lb;
    }

    void InductionVar::set_lb(NBase lb)
    {
        _lb = lb;
    }

    NBase InductionVar::get_ub() const
    {
        return _ub;
    }

    void InductionVar::set_ub(NBase ub)
    {
        _ub = ub;
    }

    NBase InductionVar::get_increment() const
    {
        return _incr;
    }

    void InductionVar::set_increment(NBase incr)
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

    void InductionVar::set_increment_list(ObjectList<NBase> incr_list)
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

    bool InductionVar::operator==(const InductionVar& rhs) const
    {
        return (Nodecl::Utils::structurally_equal_nodecls(_var, rhs._var)
                 && Nodecl::Utils::structurally_equal_nodecls(_lb, rhs._lb)
                 && Nodecl::Utils::structurally_equal_nodecls(_ub, rhs._ub)
                 && Nodecl::Utils::structurally_equal_nodecls(_incr, rhs._incr)
                 && (_type == rhs._type) && (_family == rhs._family));
    }

    // *********************** END class representing and induction variable *********************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ********************************* Induction Variables utils ********************************* //

    static std::string prettyprint_induction_var(InductionVar* iv)
    {
        std::string result;
        NBase lb = iv->get_lb();
        NBase ub = iv->get_ub();
        NBase incr = iv->get_increment();
        result += iv->get_variable().prettyprint()
                + "[ " + (lb.is_null()   ? "NULL" : lb.prettyprint())
                + ":"  + (ub.is_null()   ? "NULL" : ub.prettyprint())
                + ":"  + (incr.is_null() ? "NULL" : incr.prettyprint())
                + ":"  + iv->get_type_as_string()
                + " ]";
        return result;
    }
    
    std::string prettyprint_induction_vars(InductionVarList iv_list, bool to_dot)
    {
        std::string result = "";
        std::string eol = (to_dot ? "\\n" : "\n");
        for(InductionVarList::iterator it = iv_list.begin(); it != iv_list.end(); )
        {
            result += prettyprint_induction_var(*it);
            ++it;
            if(it != iv_list.end())
                result += eol;
        }
        
        return result;
    }

    void print_induction_vars(InductionVarsPerNode iv_list)
    {
        std::cerr << "    Induction Variables: " << std::endl;
        for(InductionVarsPerNode::iterator it = iv_list.begin(); it != iv_list.end(); ++it)
        {
            std::cerr << "        * " << it->first << ": " << prettyprint_induction_var(it->second) << std::endl;
        }
    }
    
    bool induction_variable_list_contains_variable(InductionVarList iv_list, NBase var)
    {
        for(InductionVarList::iterator it = iv_list.begin(); it != iv_list.end(); ++it)
            if(Nodecl::Utils::structurally_equal_nodecls((*it)->get_variable(), var, /*skip_conversions*/ true))
                return true;
        return false;
    }

    InductionVar* get_induction_variable_from_list(InductionVarList ivs, NBase var)
    {
        for(InductionVarList::iterator it = ivs.begin(); it != ivs.end(); ++it)
            if(Nodecl::Utils::structurally_equal_nodecls((*it)->get_variable(), var, /*skip conversion*/ true))
                return *it;
        return NULL;
    }

    InductionVar* get_induction_variable_from_list(Utils::InductionVarsPerNode ivs, NBase var)
    {
        for(InductionVarsPerNode::iterator it = ivs.begin(); it != ivs.end(); ++it)
            if(Nodecl::Utils::structurally_equal_nodecls(it->second->get_variable(), var, /*skip conversion*/ true))
                return it->second;
        return NULL;
    }

    // ******************************* END Induction Variables utils ******************************* //
    // ********************************************************************************************* //
}
}
}
