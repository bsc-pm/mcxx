/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#include "tl-nodecl.hpp"
#include "tl-clauses-info.hpp"
#include "tl-predicateutils.hpp"

namespace TL
{
    ClausesInfo::ClausesInfo()
    {
    }
    
    ClausesInfo::DirectiveClauses& ClausesInfo::lookup_map(Nodecl::NodeclBase a)
    {
        return _directive_clauses_map[a];
    }

    void ClausesInfo::set_all_clauses(Nodecl::NodeclBase directive, ObjectList<std::string> all_clauses)
    {
        _directive_clauses_map[directive].all_clauses = all_clauses;
    }

    void ClausesInfo::add_referenced_clause(Nodecl::NodeclBase directive, std::string clause_name)
    {
        _directive_clauses_map[directive].referenced_clauses.append(clause_name);
    }
    
    void ClausesInfo::add_referenced_clause(Nodecl::NodeclBase directive, const ObjectList<std::string> & clause_names)
    {
         for(ObjectList<std::string>::const_iterator it = clause_names.begin();
            it != clause_names.end();
            ++it)
         {
            _directive_clauses_map[directive].referenced_clauses.append(*it);
         }
    }

    void ClausesInfo::set_locus_info(Nodecl::NodeclBase directive)
    {
        _directive_clauses_map[directive].file = directive.get_filename();
        std::stringstream line;
        line << directive.get_line();
        _directive_clauses_map[directive].line = line.str();
    }

    void ClausesInfo::set_pragma(const Nodecl::NodeclBase& node)
    {
        if (node.is<Nodecl::PragmaCustomDirective>())
        {
            Nodecl::PragmaCustomDirective directive = node.as<Nodecl::PragmaCustomDirective>();
            _directive_clauses_map[directive].pragma = directive.get_text() + " " + directive.get_pragma_line().get_text();
        }
        else if (node.is<Nodecl::PragmaCustomDeclaration>())
        {
            Nodecl::PragmaCustomDeclaration directive = node.as<Nodecl::PragmaCustomDeclaration>();
            _directive_clauses_map[directive].pragma = directive.get_text() + " " + directive.get_pragma_line().get_text();
        }
    }

    bool ClausesInfo::directive_already_defined(Nodecl::NodeclBase directive)
    {
        bool defined = false;
        if (_directive_clauses_map.find(directive) != _directive_clauses_map.end())
        {
            defined = true;
        }
        return defined;
    }

    ObjectList<std::string> ClausesInfo::get_unreferenced_clauses(Nodecl::NodeclBase directive)
    {
        DirectiveClauses directive_entry = _directive_clauses_map[directive];
        ObjectList<std::string> unref = directive_entry.all_clauses.filter(not_in_set(directive_entry.referenced_clauses));
        return unref;
    }

    std::string ClausesInfo::get_locus_info(Nodecl::NodeclBase directive)
    {
        return _directive_clauses_map[directive].file + ":" + _directive_clauses_map[directive].line;
    }

    std::string ClausesInfo::get_pragma(Nodecl::NodeclBase directive)
    {
        return _directive_clauses_map[directive].pragma;
    }
}
