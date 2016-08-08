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

#ifndef TL_NODECL_UTILS_HPP
#define TL_NODECL_UTILS_HPP

#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-source.hpp"

#include "cxx-nodecl-deep-copy.h"

#include <tr1/unordered_map>
#include <functional>

namespace Nodecl {
namespace Utils {

    TL::ObjectList<TL::Symbol> get_all_symbols(Nodecl::NodeclBase);
    TL::ObjectList<TL::Symbol> get_nonlocal_symbols(Nodecl::NodeclBase);
    TL::ObjectList<TL::Symbol> get_local_symbols(Nodecl::NodeclBase);

    TL::ObjectList<Nodecl::Symbol> get_all_symbols_occurrences(Nodecl::NodeclBase);
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

    bool dataref_contains_dataref( Nodecl::NodeclBase container, Nodecl::NodeclBase contained );
    bool nodecl_is_in_nodecl_list(
            const Nodecl::NodeclBase& n,
            const Nodecl::List& l,
            const bool skip_conversion_nodecls = false);
    bool structurally_equal_nodecls(
            const Nodecl::NodeclBase& n1,
            const Nodecl::NodeclBase& n2,
            const bool skip_conversion_nodecls = false);

    int structurally_cmp_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2,
                                 bool skip_conversion_nodecls = false);
    bool structurally_less_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2,
                                 bool skip_conversion_nodecls = false);
 
    struct Nodecl_hash {
        size_t operator() (const Nodecl::NodeclBase& n) const;
    };
    
    struct Nodecl_structural_equal {
        bool operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const;
    };
    
    struct Nodecl_structural_less {
        bool operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const;
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

    Nodecl::NodeclBase skip_contexts_and_lists(
            Nodecl::NodeclBase n);

    bool is_in_list(Nodecl::NodeclBase n);
    void remove_from_enclosing_list(Nodecl::NodeclBase n);

    Nodecl::NodeclBase get_previous_sibling(const Nodecl::NodeclBase& n);
    bool is_nodecl_statement(const Nodecl::NodeclBase& n);
    void prepend_sibling_statement(const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& new_stmt,
            const Nodecl::NodeclBase& obj_init_context = Nodecl::NodeclBase::null());
    void append_sibling_statement(const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& new_stmt,
            const Nodecl::NodeclBase& obj_init_context = Nodecl::NodeclBase::null());
 
    void append_items_after(Nodecl::NodeclBase n, Nodecl::NodeclBase items);
    void prepend_items_before(Nodecl::NodeclBase n, Nodecl::NodeclBase items);

    void append_items_in_nested_compound_statement(
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& items);
    void prepend_items_in_nested_compound_statement(
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& items);

    TL::Symbol get_enclosing_function(Nodecl::NodeclBase n);
    //! Returns the first list node that encloses n
    Nodecl::NodeclBase get_enclosing_list(Nodecl::NodeclBase n);
    //! Returns the first node enclosing n whose parent is a list
    Nodecl::NodeclBase get_enclosing_node_in_list(Nodecl::NodeclBase n);

    void prepend_to_top_level_nodecl(Nodecl::NodeclBase n);
    void append_to_top_level_nodecl(Nodecl::NodeclBase n);

    void prepend_to_enclosing_top_level_location(Nodecl::NodeclBase current_location, Nodecl::NodeclBase n);
    void append_to_enclosing_top_level_location(Nodecl::NodeclBase current_location, Nodecl::NodeclBase n);

    void add_statements_at_beginning_of_function(Nodecl::NodeclBase current_location, Nodecl::NodeclBase new_stmts);

    Nodecl::NodeclBase advance_conversions(Nodecl::NodeclBase n);

    TL::ObjectList<Nodecl::NodeclBase> get_declarations_of_entity_at_top_level(TL::Symbol);
    TL::ObjectList<Nodecl::NodeclBase> get_definitions_of_entity_at_top_level(TL::Symbol);
    TL::ObjectList<Nodecl::NodeclBase> get_declarations_or_definitions_of_entity_at_top_level(TL::Symbol);

    std::string get_elemental_operator_of_binary_expression(Nodecl::NodeclBase n);
    std::string get_elemental_operator_of_binary_expression(node_t);

    template <typename Kind>
    Nodecl::NodeclBase get_enclosing_nodecl_of_kind(
            Nodecl::NodeclBase n)
    {
        while (!n.is_null() && !n.is<Kind>())
        {
            n = n.get_parent();
        }

        return n;
    }

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

    bool list_contains_nodecl_by_structure(
            const TL::ObjectList<Nodecl::NodeclBase>& container,
            const NodeclBase& contained);
    TL::ObjectList<Nodecl::NodeclBase>::iterator list_get_nodecl_by_structure(
            TL::ObjectList<Nodecl::NodeclBase>& container,
            const NodeclBase& contained);

    TL::ObjectList<Nodecl::NodeclBase> get_strings_as_expressions(
            const TL::ObjectList<std::string>& string_list,
            const Nodecl::NodeclBase& ref_scope);

    template <class Comparator>
        struct SimpleNodeFinderVisitor : public Nodecl::NodeclVisitor<void>
    {
        Comparator _comparator;
        Nodecl::NodeclBase _needle;
        Nodecl::NodeclBase _found_node;

        SimpleNodeFinderVisitor( const Nodecl::NodeclBase& needle)
            : _needle(needle) { }
        void generic_finder(const Nodecl::NodeclBase& n);

        void unhandled_node( const Nodecl::NodeclBase& n );
        void visit( const Nodecl::ObjectInit& n );
    };

    typedef SimpleNodeFinderVisitor<std::equal_to<Nodecl::NodeclBase> > PointerSimpleNodeFinderVisitor;
    typedef SimpleNodeFinderVisitor<Nodecl_structural_equal> StructuralSimpleNodeFinderVisitor;

    template <class Comparator>
    struct CollectNodeFinderVisitor : public Nodecl::NodeclVisitor<void>
    {
        Comparator _comparator;
        Nodecl::NodeclBase _needle;
        TL::ObjectList<Nodecl::NodeclBase> _found_nodes;

        CollectNodeFinderVisitor( const Nodecl::NodeclBase& needle)
            : _needle(needle) { }
        void generic_finder(const Nodecl::NodeclBase& n);

        void unhandled_node( const Nodecl::NodeclBase& n );
        void visit( const Nodecl::ObjectInit& n );
    };

    typedef SimpleNodeFinderVisitor<std::equal_to<Nodecl::NodeclBase> > SimplePointerNodeFinderVisitor;
    typedef SimpleNodeFinderVisitor<Nodecl_structural_equal> SimpleStructuralNodeFinderVisitor;
    typedef CollectNodeFinderVisitor<std::equal_to<Nodecl::NodeclBase> > CollectPointerNodeFinderVisitor;
    typedef CollectNodeFinderVisitor<Nodecl_structural_equal> CollectStructuralNodeFinderVisitor;


    template <typename Kind>
    struct SimpleKindFinderVisitor : ExhaustiveVisitor<void>
    {
            Nodecl::NodeclBase found_node;
            SimpleKindFinderVisitor() {}

            virtual void visit(const Nodecl::ObjectInit& n)
            {
                walk(n.get_symbol().get_value());
            }

            virtual void visit(const Kind& k)
            {
                found_node = k;
            }
    };

    struct CollectKindFinderBaseVisitor : ExhaustiveVisitor<void>
    {
        TL::ObjectList<Nodecl::NodeclBase> found_nodes;
        CollectKindFinderBaseVisitor() { }
    };

    template <typename Kind>
    struct CollectKindFinderVisitor : CollectKindFinderBaseVisitor
    {
        CollectKindFinderVisitor() {}

        virtual void visit_pre(const Nodecl::ObjectInit& n)
        {
            walk(n.get_symbol().get_value());
        }

        virtual void visit_pre(const Kind& k)
        {
            found_nodes.append(k);
        }
    };

    template <>
    struct CollectKindFinderVisitor<Nodecl::ObjectInit> : CollectKindFinderBaseVisitor
    {
        CollectKindFinderVisitor() {}

        virtual void visit_pre(const Nodecl::ObjectInit& k)
        {
            found_nodes.append(k);
        }
    };

    bool nodecl_contains_nodecl_by_structure(
            const Nodecl::NodeclBase& haystack,
            const Nodecl::NodeclBase& needle);
    bool nodecl_contains_nodecl_by_pointer(
            const Nodecl::NodeclBase& haystack,
            const Nodecl::NodeclBase& needle);

    template <typename Kind>
    bool nodecl_contains_nodecl_of_kind(
            const Nodecl::NodeclBase& n)
    {
        SimpleKindFinderVisitor<Kind> finder;
        finder.walk(n);
        return !finder.found_node.is_null();
    }

    template <typename Kind>
    Nodecl::NodeclBase nodecl_get_first_nodecl_of_kind(
            const Nodecl::NodeclBase& n)
    {
        SimpleKindFinderVisitor<Kind> finder;
        finder.walk(n);
        return finder.found_node;
    }

    template <typename Kind>
    TL::ObjectList<Nodecl::NodeclBase> nodecl_get_all_nodecls_of_kind(
            const Nodecl::NodeclBase& n)
    {
        CollectKindFinderVisitor<Kind> finder;
        finder.walk(n);
        return finder.found_nodes;
    }

    void nodecl_replace_nodecl_by_structure(
            const Nodecl::NodeclBase& haystack,
            const Nodecl::NodeclBase& needle,
            const Nodecl::NodeclBase& replacement);

    void nodecl_replace_nodecl_by_pointer(
            const Nodecl::NodeclBase& haystack,
            const Nodecl::NodeclBase& needle,
            const Nodecl::NodeclBase& replacement);
}
}

namespace TL
{
    struct NoNewNodePolicy
    {
        static Nodecl::NodeclBase shallow_copy(const Nodecl::NodeclBase& n)
        {
            return n;
        }

        static Nodecl::NodeclBase new_node(const Nodecl::NodeclBase& n)
        {
            internal_error("Attempt to copy code", 0);
        }
    };

    struct UsualCopyPolicy
    {
        static Nodecl::NodeclBase shallow_copy(const Nodecl::NodeclBase& n)
        {
            return n.shallow_copy();
        }

        static Nodecl::NodeclBase new_node(const Nodecl::NodeclBase& n)
        {
            return n;
        }
    };

    struct ForStatementHelperBase
    {
        protected:
            Nodecl::NodeclBase _induction_var;

            Nodecl::NodeclBase _lower_bound;
            Nodecl::NodeclBase _upper_bound;
            Nodecl::NodeclBase _step;

            bool _induction_variable_in_separate_scope;
            bool _is_omp_valid;

            enum loop_trend_t
            {
                STRICTLY_DECREASING_LOOP = -1,
                UNKNOWN_LOOP,
                STRICTLY_INCREASING_LOOP
            };

            loop_trend_t _loop_trend;

        public:

            bool is_omp_valid_loop() const;

            Symbol get_induction_variable() const;

            bool induction_variable_in_separate_scope() const;

            Nodecl::NodeclBase get_lower_bound() const;
            Nodecl::NodeclBase get_upper_bound() const;
            Nodecl::NodeclBase get_step() const;

            bool is_strictly_increasing_loop() const;
    };


    template <typename CopyPolicy>
    struct ForStatementHelper : Nodecl::ForStatement, ForStatementHelperBase
    {
        private:
            void analyze_loop_header();
        public:
            ForStatementHelper(const Nodecl::ForStatement& n)
                : Nodecl::ForStatement(n), ForStatementHelperBase()
            {
                    analyze_loop_header();
            }
    };

    typedef ForStatementHelper<TL::UsualCopyPolicy> ForStatement;

    class LoopControlAdapter 
    {
        Nodecl::NodeclBase _lc;

        public:
            LoopControlAdapter(Nodecl::NodeclBase lc);
            
            Nodecl::NodeclBase get_cond();
            Nodecl::NodeclBase get_next();
    };
}

void deb_print_ast(Nodecl::NodeclBase n);
std::string deb_print_type(TL::Type type);
std::string deb_print_type(const Nodecl::NodeclBase& n);

#endif // TL_NODECL_UTILS_HPP
