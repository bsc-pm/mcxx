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

#ifndef TL_NODECL_UTILS_HPP
#define TL_NODECL_UTILS_HPP

#include "tl-nodecl.hpp"
#include "tl-nodecl-calc.hpp"
#include "tl-nodecl-visitor.hpp"
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
        TL::ObjectList<Nodecl::NodeclBase> get_all_nodecl_occurrences(Nodecl::NodeclBase target_occurrence, 
                Nodecl::NodeclBase container);
        TL::ObjectList<Nodecl::Symbol> get_nonlocal_symbols_occurrences(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_local_symbols_occurrences(Nodecl::NodeclBase);

        TL::ObjectList<Nodecl::Symbol> get_all_symbols_first_occurrence(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_nonlocal_symbols_first_occurrence(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_local_symbols_first_occurrence(Nodecl::NodeclBase);

        TL::ObjectList<Nodecl::NodeclBase> get_all_memory_accesses(Nodecl::NodeclBase n);

        bool nodecl_is_arithmetic_op( Nodecl::NodeclBase n );
        bool nodecl_is_assignment_op( Nodecl::NodeclBase n );
        bool nodecl_is_bitwise_op( Nodecl::NodeclBase n );
        bool nodecl_is_comparison_op( Nodecl::NodeclBase n );
        bool nodecl_is_literal( Nodecl::NodeclBase n );
        bool nodecl_is_logical_op( Nodecl::NodeclBase n );
        bool nodecl_is_modifiable_lvalue( Nodecl::NodeclBase n );

        bool nodecl_contains_nodecl( Nodecl::NodeclBase container, Nodecl::NodeclBase contained );
        bool nodecl_is_in_nodecl_list( Nodecl::NodeclBase n, Nodecl::List l );
        bool equal_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2);
        bool equal_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2, 
                bool skip_conversion_nodecls);
        int cmp_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2,
                        bool skip_conversion_nodecls = false);
        struct Nodecl_hash {
            size_t operator() (const Nodecl::NodeclBase& n) const;
        };
        struct Nodecl_comp {
            bool operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const;
        };

        /*!
         * This method must be called in pre-order from the bottom of a tree expression
         *
         * R1 :   +                                     R3 :    -
         *      /   \          =>     c1 + c2                 /   \     =>    c1 - c2
         *    c1    c2                                      c1    c2
         *
         * R2 :   +                       +             R4      -                +
         *      /   \          =>       /   \                 /   \     =>     /   \
         *     t    c                  c     t               t    c          -c     t
         *
         * R5 :   -
         *      /   \               =>    0
         *     t1   t2 , t1 = t2
         *
         * R6 :       +                    +
         *          /   \               /     \
         *         +    c2     =>    c1+c2     t
         *       /   \
         *     c1    t
         *
         * R7 :   *                                     R8 :    *                 *
         *      /   \          =>     c1 * c2                 /   \     =>      /   \
         *    c1    c2                                       t    c            c     t
         *
         * R9 :       *                    *
         *          /   \               /     \
         *         *    c2     =>    c1*c2     t
         *       /   \
         *     c1    t
         *
         * R10 :  /
         *      /   \          =>     0
         *     0    c,  c != 0
         *
         * R11 :  %         %
         *      /   \  ,  /   \   =>  0
         *     t    1    t    t
         */
        class LIBTL_CLASS ReduceExpressionVisitor : public Nodecl::ExhaustiveVisitor<void>
        {
        private:
            Calculator _calc;

        public:
            // *** Constructor *** //
            ReduceExpressionVisitor( );

            // *** Visiting methods *** //
            Ret visit_post( const Nodecl::Add& n );
            Ret visit_post( const Nodecl::Div& n );
            Ret visit_post( const Nodecl::LowerOrEqualThan& n );
            Ret visit_post( const Nodecl::Minus& n );
            Ret visit_post( const Nodecl::Mod& n );
            Ret visit_post( const Nodecl::Mul& n );
            Ret visit_post( const Nodecl::ObjectInit& n );
            Ret visit_post( const Nodecl::VectorAdd& n );
            Ret visit_post( const Nodecl::VectorDiv& n );
            Ret visit_post( const Nodecl::VectorLowerOrEqualThan& n );
            Ret visit_post( const Nodecl::VectorMinus& n );
            Ret visit_post( const Nodecl::VectorMod& n );
            Ret visit_post( const Nodecl::VectorMul& n );
        };

        // Basic replacement
        //
        // After this operation dest will be updated to have the same contents
        // as src (including children, type information, etc)
        //
        // If src is a list but dest is not, then dest will be updated to the
        // contents of the first element of the list. The remaining ones, if
        // any, will be appended to that node. Thus, if the list only has one
        // element, this replace will behave as if we replaced it with its
        // item.
        void replace(Nodecl::NodeclBase dest, Nodecl::NodeclBase src);

        Nodecl::List get_all_list_from_list_node(Nodecl::List);

        bool is_in_list(Nodecl::NodeclBase n);
        void remove_from_enclosing_list(Nodecl::NodeclBase n);

        void append_items_after(Nodecl::NodeclBase n, Nodecl::NodeclBase items);
        void prepend_items_before(Nodecl::NodeclBase n, Nodecl::NodeclBase items);

        TL::Symbol get_enclosing_function(Nodecl::NodeclBase n);
        //! Returns the first list node that encloses n
        Nodecl::NodeclBase get_enclosing_list(Nodecl::NodeclBase n);
        //! Returns the first node enclosing n whose parent is a list
        Nodecl::NodeclBase get_enclosing_node_in_list(Nodecl::NodeclBase n);

        void prepend_to_top_level_nodecl(Nodecl::NodeclBase n);
        void append_to_top_level_nodecl(Nodecl::NodeclBase n);

        void prepend_to_enclosing_top_level_location(Nodecl::NodeclBase current_location, Nodecl::NodeclBase n);
        void append_to_enclosing_top_level_location(Nodecl::NodeclBase current_location, Nodecl::NodeclBase n);

        Nodecl::NodeclBase advance_conversions(Nodecl::NodeclBase n);

        TL::ObjectList<Nodecl::NodeclBase> get_declarations_of_entity_at_top_level(TL::Symbol);
        TL::ObjectList<Nodecl::NodeclBase> get_definitions_of_entity_at_top_level(TL::Symbol);
        TL::ObjectList<Nodecl::NodeclBase> get_declarations_or_definitions_of_entity_at_top_level(TL::Symbol);

        std::string get_elemental_operator_of_binary_expression(Nodecl::NodeclBase n);
        std::string get_elemental_operator_of_binary_expression(node_t);

        struct SymbolMap
        {
            private:
                struct adaptor_symbol_map_t : symbol_map_t
                {
                    SymbolMap* obj;
                };

                adaptor_symbol_map_t _adaptor_symbol_map;

                static scope_entry_t* adaptor_symbol_map_fun(symbol_map_t* sym_map, scope_entry_t *symbol_entry)
                {
                    adaptor_symbol_map_t* adaptor_symbol_map = (adaptor_symbol_map_t*)sym_map;

                    return adaptor_symbol_map->obj->map(symbol_entry).get_internal_symbol();
                }

                static void adaptor_symbol_map_dtor(symbol_map_t* sym_map) { }
            public:

            symbol_map_t* get_symbol_map()
            {
                return &_adaptor_symbol_map;
            }

            virtual TL::Symbol map(TL::Symbol) = 0;

            SymbolMap()
            {
                _adaptor_symbol_map.map = &SymbolMap::adaptor_symbol_map_fun;
                _adaptor_symbol_map.dtor = &SymbolMap::adaptor_symbol_map_dtor;
                _adaptor_symbol_map.obj = this;
            }

            SymbolMap(const SymbolMap& copy_symbol_map)
            {
                _adaptor_symbol_map.map = &SymbolMap::adaptor_symbol_map_fun;
                _adaptor_symbol_map.dtor = &SymbolMap::adaptor_symbol_map_dtor;
                _adaptor_symbol_map.obj = this;
            }

            SymbolMap& operator=(const SymbolMap& symbol_map)
            {
                if (this != &symbol_map)
                {
                    _adaptor_symbol_map.map = &SymbolMap::adaptor_symbol_map_fun;
                    _adaptor_symbol_map.dtor = &SymbolMap::adaptor_symbol_map_dtor;
                    _adaptor_symbol_map.obj = this;
                }
                return (*this);
            }

            virtual ~SymbolMap()
            {
            }
        };

        struct SimpleSymbolMap : public SymbolMap
        {
            SimpleSymbolMap()
                : _symbol_map(), _enclosing(NULL) { }
            explicit SimpleSymbolMap(SymbolMap* enclosing)
                : _symbol_map(), _enclosing(enclosing) { }

            virtual TL::Symbol map(TL::Symbol s)
            {
                if (!s.is_valid())
                    return s;

                symbol_map_t::iterator it = _symbol_map.find(s);
                if (it != _symbol_map.end())
                    return it->second;
                else if (_enclosing != NULL)
                    return _enclosing->map(s);
                else
                    return s;
            }

            virtual void add_map(TL::Symbol source, TL::Symbol target)
            {
                _symbol_map[source] = target;
            }

            const std::map<TL::Symbol, TL::Symbol>* get_simple_symbol_map() const
            {
                return &_symbol_map;
            }

            private:
                typedef std::map<TL::Symbol, TL::Symbol> symbol_map_t;
                symbol_map_t _symbol_map;
                SymbolMap* _enclosing;
        };

        struct LabelSymbolMap : public SymbolMap
        {
            private:
                SymbolMap* _orig_symbol_map;

                SimpleSymbolMap _current_map;

            public:
                LabelSymbolMap(
                        SymbolMap* original_symbol_map,
                        Nodecl::NodeclBase code,
                        TL::ReferenceScope ref_scope);

                virtual ~LabelSymbolMap() { }

                virtual TL::Symbol map(TL::Symbol s)
                {
                    TL::Symbol m = _current_map.map(s);
                    if (s == m
                            && _orig_symbol_map != NULL)
                    {
                        m = _orig_symbol_map->map(s);
                    }
                    return m;
                }
        };

        typedef std::map<Nodecl::NodeclBase, Nodecl::NodeclBase> NodeclDeepCopyMap;
        typedef std::map<TL::Symbol, TL::Symbol> SymbolDeepCopyMap;

        Nodecl::NodeclBase deep_copy(Nodecl::NodeclBase orig, TL::ReferenceScope ref_scope, SymbolMap& map);

        Nodecl::NodeclBase deep_copy(Nodecl::NodeclBase orig,
                TL::ReferenceScope ref_scope,
                Utils::SymbolMap& map,
                NodeclDeepCopyMap& nodecl_deep_copy_map,
                SymbolDeepCopyMap& symbol_deep_copy_map);

        // This updates symbols in the given tree using a symbol map
        void update_symbols(Nodecl::NodeclBase orig, SymbolMap& map);

        // Like above but with an empty map
        Nodecl::NodeclBase deep_copy(Nodecl::NodeclBase orig, TL::ReferenceScope ref_scope);
        Nodecl::NodeclBase deep_copy(Nodecl::NodeclBase orig,
                TL::ReferenceScope ref_scope,
                NodeclDeepCopyMap& nodecl_deep_copy_map,
                SymbolDeepCopyMap& symbol_deep_copy_map);

        // Returns the whole ArraySbuscript with a single subscript linearized
        Nodecl::ArraySubscript linearize_array_subscript(const Nodecl::ArraySubscript& n);

        bool list_contains_nodecl(const TL::ObjectList<Nodecl::NodeclBase>& container, 
                const NodeclBase& containee);
    }
}

namespace TL
{
    struct ForStatement : Nodecl::ForStatement
    {
        private:
            bool _is_omp_valid;

            Nodecl::NodeclBase _induction_var;

            Nodecl::NodeclBase _lower_bound;
            Nodecl::NodeclBase _upper_bound;
            Nodecl::NodeclBase _step;

            bool _induction_variable_in_separate_scope;

            void analyze_loop_header();
        public:
            ForStatement(const Nodecl::ForStatement n)
                : Nodecl::ForStatement(n)
            {
                    analyze_loop_header();
            }

            bool is_omp_valid_loop() const;

            Symbol get_induction_variable() const;

            bool induction_variable_in_separate_scope() const;

            Nodecl::NodeclBase get_lower_bound() const;
            Nodecl::NodeclBase get_upper_bound() const;
            Nodecl::NodeclBase get_step() const;
    };

}

#endif // TL_NODECL_UTILS_HPP
