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
#ifndef TL_PRAGMASUPPORT_HPP
#define TL_PRAGMASUPPORT_HPP

#include <string>
#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-handler.hpp"
#include "tl-traverse.hpp"
#include "cxx-attrnames.h"

namespace TL
{
    class PragmaCustomClause : public LangConstruct
    {
        private:
            std::string _clause_name;

            ObjectList<AST_t> filter_pragma_clause();
        public:
            PragmaCustomClause(const std::string& src, AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link), _clause_name(src)
            {
            }

            ObjectList<Expression> get_expression_list();
            ObjectList<IdExpression> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);

            bool is_defined();
    };

    class PragmaCustomConstruct : public LangConstruct
    {
        public:
            PragmaCustomConstruct(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
            }

            std::string get_pragma();
            std::string get_directive();

            bool is_directive();
            bool is_construct();

            Statement get_statement();
            AST_t get_declaration();

            PragmaCustomClause get_clause(const std::string& name);
    };

    typedef std::map<std::string, Signal1<PragmaCustomConstruct> > CustomFunctorMap;

    class PragmaCustomDispatcher : public TraverseFunctor
    {
        private:
            std::string _pragma_handled;
            CustomFunctorMap& _pre_map;
            CustomFunctorMap& _post_map;

            void dispatch_pragma_construct(CustomFunctorMap& search_map, Context ctx, AST_t node);
        public:
            PragmaCustomDispatcher(const std::string& pragma_handled, CustomFunctorMap& pre_map,
                    CustomFunctorMap& post_map);

            virtual void preorder(Context ctx, AST_t node);
            virtual void postorder(Context ctx, AST_t node);
    };

    class PragmaCustomCompilerPhase : public CompilerPhase
    {
        private:
            std::string _pragma_handled;
            PragmaCustomDispatcher _pragma_dispatcher;
        public:
            PragmaCustomCompilerPhase(const std::string& pragma_handled);
            virtual void run(DTO& data_flow);

            CustomFunctorMap on_directive_pre;
            CustomFunctorMap on_directive_post;
    };
}

#endif // TL_PRAGMASUPPORT_HPP
