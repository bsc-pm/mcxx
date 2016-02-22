/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#ifndef TL_NODECL_UTILS_FORTRAN_HPP
#define TL_NODECL_UTILS_FORTRAN_HPP

#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-symbol-deep-copy.h"
#include <set>

namespace Nodecl { namespace Utils { namespace Fortran {

    struct InternalFunctions : Nodecl::ExhaustiveVisitor<void>
    {
        private:
            std::set<TL::Symbol> _already_visited;
        public:
            TL::ObjectList<Nodecl::NodeclBase> function_codes;

            InternalFunctions()
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

    struct ExtraDeclsVisitor : Nodecl::ExhaustiveVisitor<void>
    {
        private:
            // Symbols that is enough to insert
            TL::ObjectList<TL::Symbol> _extra_insert_sym;
            // Symbols that require a full duplication
            TL::ObjectList<TL::Symbol> _extra_new_sym;

            Nodecl::Utils::SimpleSymbolMap *_symbol_map;
            TL::Scope _scope;
            TL::Symbol _reference_function;
            std::set<TL::Symbol> _functions_visited;

        private:

            bool in_scope_of_reference_function(TL::Symbol sym)
            {
                return (TL::Symbol(sym.get_scope().get_decl_context()->current_scope->related_entry) == _reference_function);
            }

        public:
            ExtraDeclsVisitor(Nodecl::Utils::SimpleSymbolMap& symbol_map,
                    TL::Scope new_scope,
                    TL::Symbol reference_function)
                : _symbol_map(&symbol_map), _scope(new_scope), _reference_function(reference_function)
            {
                _functions_visited.insert(reference_function);
            }

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

                if (sym.is_function())
                {
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
                else if (sym.is_fortran_namelist())
                {
                    _extra_new_sym.insert(sym);
                }
                else if (sym.is_fortran_parameter())
                {
                    _extra_insert_sym.insert(sym);

                    walk(sym.get_value());

                    TL::Type t = sym.get_type();
                    if (t.is_named_class()
                            && in_scope_of_reference_function(t.get_symbol()))
                    {
                        _extra_insert_sym.insert(t.get_symbol());
                    }
                }
            }

            virtual void visit(const Nodecl::StructuredValue &node)
            {
                TL::Type t = node.get_type();
                walk(node.get_items());

                if (t.is_named_class()
                        && in_scope_of_reference_function(t.get_symbol()))
                {
                    _extra_insert_sym.insert(t.get_symbol());
                }
            }

            void insert_extra_symbol(TL::Symbol sym)
            {
                _extra_insert_sym.insert(sym);
            }

            void insert_extra_symbols(const Nodecl::NodeclBase &statements)
            {
                walk(statements);

                const decl_context_t* decl_context = _scope.get_decl_context();
                for (TL::ObjectList<TL::Symbol>::iterator it2 = _extra_insert_sym.begin();
                        it2 != _extra_insert_sym.end();
                        it2++)
                {
                    ::insert_entry(decl_context->current_scope, it2->get_internal_symbol());
                }

                // New symbols
                // First register them
                TL::ObjectList<TL::Symbol> new_symbols;
                for (TL::ObjectList<TL::Symbol>::iterator it2 = _extra_new_sym.begin();
                        it2 != _extra_new_sym.end();
                        it2++)
                {
                    scope_entry_t* new_sym = ::new_symbol(decl_context, decl_context->current_scope, uniquestr(it2->get_name().c_str()));
                    new_symbols.append(new_sym);
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

    // Inserts USEd symbols
    struct InsertUsedSymbols : Nodecl::NodeclVisitor<void>
    {
        private:
            scope_t* _scope;
        public:
            InsertUsedSymbols(TL::Scope sc)
                : _scope(sc.get_decl_context()->current_scope)
            {
            }

            // Any node
            virtual void unhandled_node(const Nodecl::NodeclBase& n)
            {
                if (n.get_symbol().is_valid()
                        && n.get_symbol().is_from_module())
                {
                    ::insert_entry(_scope, n.get_symbol().get_internal_symbol());
                }
                if (n.get_type().is_valid())
                {
                    insert_type(n.get_type());
                }
                Nodecl::NodeclBase::Children children = n.children();
                for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                        it != children.end();
                        it++)
                {
                    walk(*it);
                }
            }
        private:
            void insert_type(TL::Type t)
            {
                if (t.is_named_class() && t.get_symbol().is_from_module())
                {
                    ::insert_entry(_scope, t.get_symbol().get_internal_symbol());
                }
                else if (t.is_lvalue_reference())
                {
                    insert_type(t.references_to());
                }
                else if (t.is_pointer())
                {
                    insert_type(t.points_to());
                }
                else if (t.is_array())
                {
                    insert_type(t.array_element());
                }
            }
    };

    void append_used_modules(TL::Scope orig_scope,
            TL::Scope new_scope);

    void append_module_to_scope(TL::Symbol module,
            TL::Scope scope);
} } }

#endif // TL_NODECL_UTILS_FORTRAN_HPP
