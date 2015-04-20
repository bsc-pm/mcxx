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




#ifndef TL_SCOPELINK_HPP
#define TL_SCOPELINK_HPP

#include "tl-common.hpp"
#include <typeinfo>
#include "cxx-scopelink.h"
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"

namespace TL
{
    //! \addtogroup Wrap 
    //! @{

    class Scope;

    //! This function wraps the internal compiler scope link
    class LIBTL_CLASS ScopeLink : public Object
    {
        private:
            scope_link_t* _scope_link;
        protected :
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }
        public:
            //! Given a tree it returns its related Scope
            /*
             * \return If no scope is found, the global scope of the scope link will be returned
             */
            Scope get_scope(AST_t ast) const;

            ScopeLink()
                : _scope_link(NULL)
            {
            }

            ScopeLink(scope_link_t* scope_link)
                : _scope_link(scope_link)
            {
            }

            //! Creates a ScopeLink after a reference to Object
            ScopeLink(RefPtr<Object> obj)
                : _scope_link(NULL)
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
                : Object(sl), _scope_link(sl._scope_link)
            {
            }

            /* Do not use it unless directed to do so */
            scope_link_t* get_internal_scope_link()
            {
                return _scope_link;
            }


            ScopeLink& operator=(ScopeLink sl);
            bool operator==(ScopeLink sl);
            bool operator!=(ScopeLink sl);

            friend class AST_t;
            friend class Source;
            friend class CompilerPhaseRunner;
            friend class Expression;
    };
    
    //! @}
}

#endif // TL_SCOPELINK_HPP
