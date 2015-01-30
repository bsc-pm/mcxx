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




#ifndef TL_STATIC_CFG_HPP
#define TL_STATIC_CFG_HPP

#include "tl-common.hpp"
#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"
#include "tl-symbol.hpp"
#include "tl-langconstruct.hpp"
#include "tl-predicate.hpp"
#include <map>
#include <utility>

namespace TL
{
    class LIBTL_CLASS StaticCallGraph : public Object
    {
        private:
            ObjectList<Symbol> _explicitly_called;
            ObjectList<Symbol> _implicitly_called;
            typedef std::pair<ObjectList<Symbol>, ObjectList<Symbol> > explicit_implicit_pair_t;
            std::map<Symbol, explicit_implicit_pair_t> _call_map;

            void empty_data();

            void compute_call_graph(AST_t a, 
                    ObjectList<Symbol> &explicitly_called,
                    ObjectList<Symbol> &implicitly_called);
        public:
            StaticCallGraph() { }

            void compute_global(AST_t translation_unit, ScopeLink scope_link);

            void compute_statement(Statement stmt);
            void compute_expression(Expression expr);

            ObjectList<Symbol> get_all_called_functions();
            ObjectList<Symbol> get_all_explicitly_called_functions();
            ObjectList<Symbol> get_all_implicitly_called_functions();

            ObjectList<Symbol> get_all_called_functions_from(Symbol function);
            ObjectList<Symbol> get_all_explicitly_called_functions_from(Symbol function);
            ObjectList<Symbol> get_all_implicitly_called_functions_from(Symbol function);

            ObjectList<Symbol> get_all_functions_calling_to(Symbol function);
            ObjectList<Symbol> get_all_functions_calling_explicitly_to(Symbol function);
            ObjectList<Symbol> get_all_functions_calling_implicitly_to(Symbol function);
    };
}

#endif
