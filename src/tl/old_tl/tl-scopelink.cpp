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




#include "tl-scopelink.hpp"
namespace TL
{

Scope ScopeLink::get_scope(AST_t ast) const
{
    AST _ast = ast._ast;
    const decl_context_t* decl_context = scope_link_get_decl_context(_scope_link, _ast);

    Scope result(decl_context);
    return result;
}

ScopeLink& ScopeLink::operator=(ScopeLink sl)
{
    this->_scope_link = sl._scope_link;
    return (*this);
}

bool ScopeLink::operator==(ScopeLink sl)
{
    return this->_scope_link == sl._scope_link;
}

bool ScopeLink::operator!=(ScopeLink sl)
{
    return !(this->operator==(sl));
}

}
