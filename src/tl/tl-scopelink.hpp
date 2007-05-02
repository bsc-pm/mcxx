/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef TL_SCOPELINK_HPP
#define TL_SCOPELINK_HPP

#include <typeinfo>
#include "cxx-scopelink.h"
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"

namespace TL
{
    class ScopeLink : public Object
    {
        private:
            scope_link_t* _scope_link;
        protected :
            virtual tl_type_t* get_extended_attribute(const std::string& str) const
            {
                return NULL;
            }
        public:
            Scope get_scope(AST_t ast) const;

            ScopeLink()
                : _scope_link(NULL)
            {
            }

            ScopeLink(scope_link_t* scope_link)
                : _scope_link(scope_link)
            {
            }

            ScopeLink(RefPtr<Object> obj)
            {
                RefPtr<ScopeLink> sl = RefPtr<ScopeLink>::cast_dynamic(obj);
                if (sl.get_pointer() != NULL)
                {
                    this->_scope_link = sl->_scope_link;
                }
                else
                {
                    if (typeid(*obj.get_pointer()) != typeid(Undefined))
                    {
                        std::cerr << "Bad initialization for ScopeLink" << std::endl;
                    }
                }
            }

            ScopeLink(const ScopeLink& sl)
                : _scope_link(sl._scope_link)
            {
            }

            ScopeLink& operator=(ScopeLink sl);
            bool operator==(ScopeLink sl);
            bool operator!=(ScopeLink sl);

            friend class AST_t;
            friend class Source;
            friend class CompilerPhaseRunner;
            friend class Expression;
    };
}

#endif // TL_SCOPELINK_HPP
