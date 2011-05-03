/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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

#include "extensible_symbol.hpp"

namespace TL
{
    ExtensibleSymbol::ExtensibleSymbol()
        : _sym(NULL), _member(NULL, ScopeLink()), _array_accessed_pos()
    {}
    
    ExtensibleSymbol::ExtensibleSymbol(Symbol s)
        : _sym(s.get_internal_symbol()), _member(NULL, ScopeLink()),
        _array_accessed_pos()
    {}
    
    std::string ExtensibleSymbol::get_name() const
    {
        std::string name = "";
        
        if (_sym != NULL)
            name = _sym.get_name();
        
        return name;
    }
    
    Type ExtensibleSymbol::get_type() const
    {
        return _sym.get_type();
    }
    
    IdExpression ExtensibleSymbol::get_member() const
    {
        return _member;
    }
    
    std::set<int> ExtensibleSymbol::get_accessed_positions() const
    {
        return _array_accessed_pos;
    }
   
    bool ExtensibleSymbol::operator==(const ExtensibleSymbol &cfgs) const
    {
        std::vector<int> v(this->_array_accessed_pos.size() + _array_accessed_pos.size());
        std::vector<int>::iterator it = std::set_difference(this->_array_accessed_pos.begin(), 
                                                            this->_array_accessed_pos.end(),
                                                            _array_accessed_pos.begin(),
                                                            _array_accessed_pos.end(),
                                                            v.begin());
        
        return ( (this->_sym == cfgs._sym) && (this->_member.get_ast() == cfgs._member.get_ast()) &&
                 (this->_member.get_scope_link() == cfgs._member.get_scope_link() ) &&
                 (int(it - v.begin()) == 0) );
    }
    
    bool ExtensibleSymbol::operator<(const ExtensibleSymbol &cfgs) const
    {
        if (_sym < cfgs._sym)
            return true;
        else
            return false;
    }
}