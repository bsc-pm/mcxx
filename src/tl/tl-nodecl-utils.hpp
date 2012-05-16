/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#ifndef TL_NODECL_ALG_HPP
#define TL_NODECL_ALG_HPP

#include "tl-nodecl.hpp"
#include "tl-source.hpp"

#include "cxx-nodecl-deep-copy.h"

#include <tr1/unordered_map>

namespace Nodecl
{
    namespace Utils
    {
        TL::ObjectList<TL::Symbol> get_all_symbols(Nodecl::NodeclBase);
        TL::ObjectList<TL::Symbol> get_nonlocal_symbols(Nodecl::NodeclBase);
        TL::ObjectList<TL::Symbol> get_local_symbols(Nodecl::NodeclBase);

        TL::ObjectList<Nodecl::Symbol> get_all_symbols_occurrences(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_nonlocal_symbols_occurrences(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_local_symbols_occurrences(Nodecl::NodeclBase);

        TL::ObjectList<Nodecl::Symbol> get_all_symbols_first_occurrence(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_nonlocal_symbols_first_occurrence(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_local_symbols_first_occurrence(Nodecl::NodeclBase);

        bool equal_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2);
        struct Nodecl_hash {
            size_t operator() (const Nodecl::NodeclBase& n) const;
        };
        struct Nodecl_comp {
            bool operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const;
        };

        NodeclBase reduce_expression(Nodecl::NodeclBase n);
        NodeclBase algebraic_simplification(Nodecl::NodeclBase n);

        Nodecl::List get_all_list_from_list_node(Nodecl::List);

        void remove_from_enclosing_list(Nodecl::NodeclBase n);

        TL::Symbol get_enclosing_function(Nodecl::NodeclBase n);

        void append_to_top_level_nodecl(Nodecl::NodeclBase n);

        Nodecl::NodeclBase advance_conversions(Nodecl::NodeclBase n);

        std::string get_elemental_operator_of_binary_expression(Nodecl::NodeclBase n);
        std::string get_elemental_operator_of_binary_expression(node_t);

        struct SymbolMap
        {
            virtual TL::Symbol map(TL::Symbol) = 0;
        };

        struct SimpleSymbolMap : public SymbolMap
        {
            virtual TL::Symbol map(TL::Symbol s)
            {
                if (!s.is_valid())
                    return s;

                symbol_map_t::iterator it = _symbol_map.find(s);
                if (it != _symbol_map.end())
                    return it->second;
                else
                    return s;
            }

            virtual void add_map(TL::Symbol source, TL::Symbol target)
            {
                _symbol_map[source] = target;
            }

            private:
            typedef std::map<TL::Symbol, TL::Symbol> symbol_map_t;
            symbol_map_t _symbol_map;
        };

        struct FortranProgramUnitSymbolMap : public SymbolMap
        {
            private:
                SymbolMap* _orig_symbol_map;

                void *_out_info;
                scope_entry_t* (*_out_map_info)(scope_entry_t*, void*);
                void (*_free_closure)(void*);

            public:
                FortranProgramUnitSymbolMap(SymbolMap* original_symbol_map,
                        TL::Symbol source_program_unit,
                        TL::Symbol target_program_unit)
                    : _orig_symbol_map(original_symbol_map),
                    _out_info(NULL),
                    _out_map_info(NULL),
                    _free_closure(NULL)
                {
                    // Copy Fortran functions
                    copy_fortran_program_unit(
                            target_program_unit.get_internal_symbol(),
                            source_program_unit.get_internal_symbol(),
                            &_out_info,
                            &_out_map_info,
                            &_free_closure);
                }

                ~FortranProgramUnitSymbolMap()
                {
                    _free_closure(_out_info);
                    ::free(_out_info);
                    delete _orig_symbol_map;
                }

                virtual TL::Symbol map(TL::Symbol s)
                {
                    TL::Symbol m = _out_map_info(s.get_internal_symbol(), _out_info);
                    if (s == m)
                    {
                        m = _orig_symbol_map->map(s);
                    }
                    return m;
                }
        };


        Nodecl::NodeclBase deep_copy(Nodecl::NodeclBase orig, TL::ReferenceScope ref_scope, SymbolMap& map);

        // Like above but with an empty map
        Nodecl::NodeclBase deep_copy(Nodecl::NodeclBase orig, TL::ReferenceScope ref_scope);
    }
}

namespace TL
{
    struct ForStatement : Nodecl::ForStatement
    {
        private:
            bool _is_omp_valid;

            TL::Symbol _induction_var;

            Nodecl::NodeclBase _lower_bound;
            Nodecl::NodeclBase _upper_bound;
            Nodecl::NodeclBase _step;

            void analyze_loop_header();
        public:
            ForStatement(const Nodecl::ForStatement n)
                : Nodecl::ForStatement(n) 
            { 
                    analyze_loop_header();
            }

            bool is_omp_valid_loop() const;

            Symbol get_induction_variable() const;

            Nodecl::NodeclBase get_lower_bound() const;
            Nodecl::NodeclBase get_upper_bound() const;
            Nodecl::NodeclBase get_step() const;
    };
}

#endif // TL_NODECL_ALG_HPP
