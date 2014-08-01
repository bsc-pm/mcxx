/*--------------------------------------------------------------------
 (C) Copyright 2006*-2012 Barcelona Supercomputing Center
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

#include "tl-node.hpp"
#include "tl-pcfg-utils.hpp"

namespace TL {
namespace Analysis {

    // ************************************************************************************** //
    // ******************************* PCFG Loop Control class ****************************** //

    PCFGLoopControl::PCFGLoopControl()
        : _init(NULL), _cond(NULL), _next(NULL)
    {}

    PCFGLoopControl::~PCFGLoopControl()
    {
        delete _init;
        delete _cond;
        delete _next;
    }

    // ***************************** END PCFG Loop Control class **************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ******************************** PCFG Try block class ******************************** //

    PCFGTryBlock::PCFGTryBlock()
        : _handler_parents(), _handler_exits(), _nhandlers(-1)
    {}

    PCFGTryBlock::~PCFGTryBlock()
    {}

    // ****************************** END PCFG Try block class ****************************** //
    // ************************************************************************************** //


    PCFGSwitch::PCFGSwitch(Node* condition, Node* exit )
        : _condition(condition), _exit(exit)
    {}

    PCFGSwitch::~PCFGSwitch()
    {
        delete _condition;
        delete _exit;
    }

    void PCFGSwitch::set_condition(Node* condition)
    {
        _condition = condition;
    }

    // ************************************************************************************** //
    // ***************************** PCFG OmpSs pragma classes ****************************** //
    
    PCFGClause::PCFGClause(ClauseType ct, const NBase& c)
        : _clause_type(ct), _clause(c)
    {}

    PCFGClause::PCFGClause(const PCFGClause& clause)
    {
        _clause_type = clause._clause_type;
        _clause = clause._clause;
    }
    
    ClauseType PCFGClause::get_type() const
    {
        return _clause_type;
    }
    
    //! Returns a string with the graph type of the node.
    std::string PCFGClause::get_type_as_string() const
    {
        switch(_clause_type)
        {
            #undef CLAUSE
            #define CLAUSE(X) case __##X : return #X;
            CLAUSE_LIST
            #undef CLAUSE
            default: WARNING_MESSAGE("Unexpected clause type '%d'", _clause_type);
        }
        return "";
    }
    
    NBase PCFGClause::get_nodecl() const
    {
        return _clause;
    }
    
    PCFGPragmaInfo::PCFGPragmaInfo()
        : _clauses()
    {}

    PCFGPragmaInfo::PCFGPragmaInfo(const PCFGPragmaInfo& p)
    {
        _clauses = p._clauses;
    }
    
    PCFGPragmaInfo::PCFGPragmaInfo(const PCFGClause& clause)
        : _clauses(ObjectList<PCFGClause>(1, clause))
    {}

    bool PCFGPragmaInfo::has_clause(ClauseType clause) const
    {
        for (ObjectList<PCFGClause>::const_iterator it = _clauses.begin(); it != _clauses.end(); ++it)
        {
            if (it->_clause_type == clause)
                return true;
        }
        return false;
    }

    PCFGClause PCFGPragmaInfo::get_clause(ClauseType clause) const
    {
        for (ObjectList<PCFGClause>::const_iterator it = _clauses.begin(); it != _clauses.end(); ++it)
            if (it->_clause_type == clause)
                return *it;
        
        internal_error("No clause %d found in pragma info.\n", clause);
    }
    
    void PCFGPragmaInfo::add_clause(const PCFGClause& clause)
    {
        _clauses.append(clause);
    }

    ObjectList<PCFGClause> PCFGPragmaInfo::get_clauses() const
    {
        return _clauses;
    }
    
    // **************************** END PCFG OmpSs pragma classes *************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ********************************** PCFG utils class ********************************** //

    PCFGVisitUtils::PCFGVisitUtils()
        : _last_nodes(), _return_nodes(), _outer_nodes(),
          _continue_nodes(), _break_nodes(), _labeled_nodes(), _goto_nodes(),
          _switch_nodes(), _nested_loop_nodes(), _tryblock_nodes(),
          _pragma_nodes(), _context_nodecl(), _section_nodes(), _assert_nodes(),
          _environ_entry_exit(), _is_vector(false), _is_simd(false), _nid(-1)
    {}

    // ************************************************************************************** //
    // ******************************** END PCFG utils class ******************************** //

}
}
