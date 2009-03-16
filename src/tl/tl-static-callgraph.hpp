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
    class LIBTL_CLASS StaticCallGraph : Object
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
