/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#ifndef TL_NODECL_UTILS_C_HPP
#define TL_NODECL_UTILS_C_HPP

#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-symbol-deep-copy.h"
#include <set>

namespace Nodecl { namespace Utils { namespace C {

    struct NestedFunctions : Nodecl::ExhaustiveVisitor<void>
    {
        private:
            std::set<TL::Symbol> _already_visited;
        public:
            TL::ObjectList<Nodecl::NodeclBase> function_codes;

            NestedFunctions()
                : _already_visited(), function_codes()
            {
            }

            virtual void visit(const Nodecl::Symbol& node_sym)
            {
                TL::Symbol sym = node_sym.get_symbol();

                if (sym.is_function()
                        && sym.is_nested_function())
                {
                    if (_already_visited.find(sym) == _already_visited.end())
                    {
                        _already_visited.insert(sym);
                        function_codes.append(sym.get_function_code());
                        walk(sym.get_function_code());
                    }
                }
            }
    };

    struct ExtraDeclsVisitor
    {
            ExtraDeclsVisitor(Nodecl::Utils::SimpleSymbolMap*&,
                    TL::Scope,
                    TL::Symbol)
            { }

            void insert_extra_symbols(const Nodecl::NodeclBase &) { }
            void insert_extra_symbol(TL::Symbol sym) { }
    };

#if 0
    struct ExtraDeclsVisitor : Nodecl::ExhaustiveVisitor<void>
    {
        private:
            // Symbols that require a full duplication
            TL::ObjectList<TL::Symbol> _extra_new_sym;

            Nodecl::Utils::SimpleSymbolMap *_symbol_map;
            TL::Scope _scope;
            TL::Symbol _reference_function;
            std::set<TL::Symbol> _functions_visited;
            std::set<TL::Type> _types_visited;

        private:

            bool in_scope_of_reference_function(TL::Symbol sym)
            {
                return (TL::Symbol(sym.get_scope().get_decl_context()->current_scope->related_entry) == _reference_function);
            }

        public:
            ExtraDeclsVisitor(Nodecl::Utils::SimpleSymbolMap*& symbol_map,
                    TL::Scope new_scope,
                    TL::Symbol reference_function)
                : _scope(new_scope), _reference_function(reference_function)
            {
                _symbol_map = new Nodecl::Utils::SimpleSymbolMap(symbol_map);
                symbol_map = _symbol_map;
                _functions_visited.insert(reference_function);
            }

            virtual void visit(const Nodecl::FunctionCall &function_call)
            {
                Nodecl::NodeclBase function_name = function_call.get_called();
                Nodecl::NodeclBase alternate_name = function_call.get_alternate_name();
                Nodecl::NodeclBase argument_seq = function_call.get_arguments();

                if (alternate_name.is_null())
                {
                    walk(function_name);
                }
                else
                {
                    walk(alternate_name);
                }

                walk(argument_seq);
            }

            virtual void visit(const Nodecl::Symbol &node_sym)
            {
                TL::Symbol sym = node_sym.get_symbol();
                if (!in_scope_of_reference_function(sym))
                    return;

                if (sym.get_scope().is_block_scope())
                {
                    if (sym.is_function())
                    {
                        if (!sym.is_nested_function())
                            _extra_new_sym.insert(sym);

                        if (sym.is_nested_function())
                        {
                            std::pair<std::set<TL::Symbol>::iterator, bool> p = _functions_visited.insert(sym);
                            if (p.second)
                            {
                                walk(sym.get_function_code());
                            }
                        }
                    }
                }

                visit_type(sym.get_type());
            }

            void visit_type(TL::Type t)
            {
                if (!t.is_valid())
                    return;
                std::pair<std::set<TL::Type>::iterator, bool> p = _types_visited.insert(t);

                if (!p.second)
                    return;

                if (t.is_named()
                        && t.get_symbol().get_scope().is_block_scope())
                {
                    _extra_new_sym.insert(t.get_symbol());
                }
                else if (t.is_pointer())
                {
                    visit_type(t.points_to());
                }
                else if (t.is_any_reference())
                {
                    visit_type(t.references_to());
                }
                else if (t.is_function())
                {
                    visit_type(t.returns());

                    TL::ObjectList<TL::Type> parameters = t.parameters();

                    for (TL::ObjectList<TL::Type>::iterator it = parameters.begin();
                            it != parameters.end();
                            it++)
                    {
                        visit_type(*it);
                    }
                }
                else if (t.is_array())
                {
                    // We do not visit the size here, this should be done elsewhere
                    visit_type(t.array_element());
                }
                else if (t.is_vector())
                {
                    visit_type(t.vector_element());
                }

                _types_visited.erase(t);
            }

            void insert_extra_symbol(TL::Symbol sym)
            {
                _extra_new_sym.insert(sym);
            }

            void insert_extra_symbols(const Nodecl::NodeclBase &statements)
            {
                walk(statements);

                const decl_context_t* decl_context = _scope.get_decl_context();
                // New symbols
                // First register them
                TL::ObjectList<TL::Symbol> new_symbols;
                for (TL::ObjectList<TL::Symbol>::iterator it2 = _extra_new_sym.begin();
                        it2 != _extra_new_sym.end();
                        it2++)
                {
                    scope_entry_t* new_sym = ::new_symbol(decl_context, decl_context->current_scope, uniquestr(it2->get_name().c_str()));
                    new_symbols.append(new_sym);

                    new_sym->kind = it2->get_internal_symbol()->kind;
                    new_sym->type_information = it2->get_type().get_internal_type();

                    _symbol_map->add_map(*it2, new_sym);
                }
                // Second fill them
                for (unsigned int i = 0; i < _extra_new_sym.size(); i++)
                {
                    ::symbol_deep_copy(new_symbols[i].get_internal_symbol(), _extra_new_sym[i].get_internal_symbol(),
                            decl_context,
                            _symbol_map->get_symbol_map());
                }
            }
    };
#endif


} } }

#endif // TL_NODECL_UTILS_C_HPP
