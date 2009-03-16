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
#include "tl-ompserialize.hpp"
#include "tl-omp.hpp"
#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-predicate.hpp"
#include "tl-builtin.hpp"
#include "tl-traverse.hpp"
#include "cxx-attrnames.h"

namespace TL
{
    namespace Nanos4
    {
        // Predicate for any OpenMP construct
        class AnyOpenMPConstruct : public Predicate<AST_t>
        {
            public:
                virtual bool operator()(AST_t& a) const
                {
                    TL::Bool is_omp_directive = a.get_attribute(OMP_IS_OMP_DIRECTIVE);
                    TL::Bool is_omp_construct = a.get_attribute(OMP_IS_OMP_CONSTRUCT);

                    return is_omp_directive || is_omp_construct;
                }
        };

        class TaskConstructPred : public Predicate<AST_t>
        {
            public:
                virtual bool operator()(AST_t& a) const
                {
                    TL::Bool is_custom_omp_construct = a.get_attribute(OMP_IS_CUSTOM_CONSTRUCT);

                    if (is_custom_omp_construct)
                    {
                        AST_t directive = a.get_attribute(OMP_CONSTRUCT_DIRECTIVE);
                        TL::String directive_name = directive.get_attribute(OMP_CUSTOM_DIRECTIVE_NAME);

                        if (directive_name == "task")
                            return true;
                    }

                    return false;
                }
        };

        class SpecificFunctionDef : public Predicate<AST_t>
        {
            private:
                Symbol _sym;
                ScopeLink _sl;
            public:
                SpecificFunctionDef(Symbol sym, ScopeLink sl)
                    : _sym(sym), _sl(sl)
                {
                }

                virtual bool operator()(AST_t& a) const
                {
                    if (FunctionDefinition::predicate(a))
                    {
                        FunctionDefinition function_def(a, _sl);
                        Symbol function_symbol 
                            = function_def.get_ast().get_attribute(LANG_FUNCTION_SYMBOL);
                        if (function_symbol == _sym)
                        {
                            return true;
                        }
                        else
                        {
                            return false;
                        }
                    }
                    else return false;
                }
        };

        // This traverse functor removes OpenMP constructs
        class RemoveOpenMP : public TraverseFunctor
        {
            public:
                virtual void postorder(Context ctx, AST_t a)
                {
                    TL::Bool b = a.get_attribute(OMP_IS_OMP_DIRECTIVE);
                    if (b)
                    {
                        a.remove_in_list();
                    }
                    else
                    {
                        AST_t body = a.get_attribute(OMP_CONSTRUCT_BODY);
                        a.replace(body);
                    }
                }
        };

        // This traverse functor fixes function calls
        class FixFunctionCalls : public TraverseFunctor
        {
            private:
                ObjectList<Symbol> _sym_list;
            public:
                FixFunctionCalls(ObjectList<Symbol> &sym_list)
                    : _sym_list(sym_list)
                {
                }

                virtual void postorder(Context ctx, AST_t a)
                {
                    Expression funct_call(a, ctx.scope_link);
                    Expression called_entity = funct_call.get_called_expression();
                    if (called_entity.is_id_expression())
                    {
                        IdExpression id_expression = called_entity.get_id_expression();
                        Symbol sym = id_expression.get_symbol();

                        if (_sym_list.contains(sym))
                        {
                            // Replace with the proper reference
                            Source src;
                            src << "__serial_" << sym.get_name() << "_";
                            AST_t function_name = src.parse_expression(a, ctx.scope_link);
                            called_entity.get_ast().replace(function_name);
                        }
                    }
                }
        };
    }
}
