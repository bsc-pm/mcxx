/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-scopelink.hpp"
namespace TL
{

Scope ScopeLink::get_scope(AST_t ast) const
{
    AST _ast = ast._ast;
    decl_context_t decl_context = scope_link_get_decl_context(_scope_link, _ast);

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
