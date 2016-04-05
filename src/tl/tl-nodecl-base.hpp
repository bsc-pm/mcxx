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

#ifndef TL_NODECL_BASE_HPP
#define TL_NODECL_BASE_HPP

#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-scope.hpp"
#include "cxx-nodecl.h"
#include "cxx-utils.h"
#include <cstdlib>
#include <tr1/array>

namespace Nodecl {

    class NodeclBase : public TL::Object
    {
        protected:
            nodecl_t _n;
        public:
            // Main API
            NodeclBase() : _n(::nodecl_null()) { }
            NodeclBase(const nodecl_t& n) : _n(n) { }
            node_t get_kind() const { return ::nodecl_get_kind(_n); }
            bool is_null() const { return ::nodecl_is_null(_n); }
            static NodeclBase null() { return NodeclBase(::nodecl_null()); }
            virtual ~NodeclBase() { }
            TL::Type get_type() const { return TL::Type(::nodecl_get_type(_n)); }
            bool has_type() const { return ::nodecl_get_type(_n) != NULL; }
            void set_type(TL::Type t) { ::nodecl_set_type(_n, t.get_internal_type()); }
            TL::Symbol get_symbol() const { return TL::Symbol(::nodecl_get_symbol(_n)); }
            TL::TemplateParameters get_template_parameters() const
            {
                return TL::TemplateParameters(::nodecl_get_template_parameters(_n));
            }
            void set_template_parameters(TL::TemplateParameters template_parameters)
            {
                ::nodecl_set_template_parameters(_n, template_parameters.get_internal_template_parameter_list());
            }
            bool has_symbol() const { return ::nodecl_get_symbol(_n) != NULL; }
            void set_symbol(TL::Symbol sym) { ::nodecl_set_symbol(_n, sym.get_internal_symbol()); }
            TL::Scope retrieve_context() const { return nodecl_retrieve_context(_n); }
            std::string get_text() const { const char* c = ::nodecl_get_text(_n); if (c == NULL) c = ""; return c; }
            void set_text(const std::string& str) { nodecl_set_text(_n, uniquestr(str.c_str())); }
            std::string get_filename() const { const char* c = nodecl_get_filename(_n); if (c == NULL) c = "(null)"; return c; }
            unsigned int get_line() const { return nodecl_get_line(_n); }
            unsigned int get_column() const { return nodecl_get_column(_n); }
            std::string get_locus_str() const { return ::nodecl_locus_to_str(_n); }
            const locus_t* get_locus() const { return ::nodecl_get_locus(_n); }
            void set_locus(const locus_t*l ) { nodecl_set_locus(_n, l); }
            const nodecl_t get_internal_nodecl() const { return _n; }

            typedef std::tr1::array<NodeclBase, MCXX_MAX_AST_CHILDREN> Children;

            Children children() const {
                Children result;
                for (int i = 0; i < ::MCXX_MAX_AST_CHILDREN; i++)
                {
                    result[i] = nodecl_get_child(_n, i);
                }
                return result;
            }
            Nodecl::NodeclBase duplicate() const
            {
                return nodecl_duplicate(this->_n);
            }
            Nodecl::NodeclBase shallow_copy() const
            {
                return nodecl_shallow_copy(this->_n);
            }
            bool is_constant() const { return ::nodecl_is_constant(_n); }
            Nodecl::NodeclBase get_parent() const
            {
                return nodecl_get_parent(this->_n);
            }

            const_value_t* get_constant() const
            {
                if (is_constant())
                {
                    return ::nodecl_get_constant(get_internal_nodecl());
                }
                else
                {
                    return NULL;
                }
            }

            void set_constant(const_value_t* cval)
            {
                ::nodecl_set_constant(get_internal_nodecl(), cval);
            }

            // Convenience function to skip conversion nodes
            Nodecl::NodeclBase no_conv() const;

            // Prettyprint
            std::string prettyprint() const;

            // Simple RTTI
            template <typename T> bool is() const { return !this->is_null() && (T::_kind == this->get_kind()); }
            template <typename T> T as() const { return T(this->_n); }
            template <typename Ret> friend class BaseNodeclVisitor;

            // Sorting of trees by pointer
            bool operator<(const NodeclBase& n) const { return nodecl_get_ast(this->_n) < nodecl_get_ast(n._n); }

            // Equality by pointer
            bool operator==(const NodeclBase& n) const { return nodecl_get_ast(this->_n) == nodecl_get_ast(n._n); }
            bool operator!=(const NodeclBase& n) const { return nodecl_get_ast(this->_n) != nodecl_get_ast(n._n); }

            // Basic replacement
            //
            // See Utils::replace
            void replace(Nodecl::NodeclBase new_node) const;

            // Current node is in a list
            bool is_in_list() const;

            // Append/prepend
            // Note that is_in_list must return true
            void append_sibling(Nodecl::NodeclBase items) const;
            void prepend_sibling(Nodecl::NodeclBase items) const;

            // This sets this Nodecls as childs of the current node
            void rechild(const Children &new_childs)
            {
                for (int i = 0; i < ::MCXX_MAX_AST_CHILDREN; i++)
                {
                    nodecl_set_child(_n, i, new_childs[i].get_internal_nodecl());
                }
            }

            // C++ bits
            bool is_value_dependent() const
            {
                return nodecl_expr_is_value_dependent(_n);
            }
            void set_is_value_dependent(bool b)
            {
                nodecl_expr_set_is_value_dependent(_n, b);
            }

            bool is_type_dependent() const
            {
                return nodecl_expr_is_type_dependent(_n);
            }
            void set_is_type_dependent(bool b)
            {
                nodecl_expr_set_is_type_dependent(_n, b);
            }

            // Internal use only
            void* get_internal_tree_address();
    };

    // This class mimicks a std::list<T> but everything works by value
    class List : public NodeclBase
    {
        private:
            static const int _kind = ::AST_NODE_LIST;
            friend class NodeclBase;

        public:
            typedef Nodecl::NodeclBase value_type;

            struct iterator : std::iterator<std::bidirectional_iterator_tag, Nodecl::NodeclBase>
            {
                public:

                private:
                    nodecl_t _top;
                    nodecl_t _current;

                    mutable std::vector<Nodecl::NodeclBase*> _cleanup;

                    void next()
                    {
                        if (nodecl_get_ast(_current) == nodecl_get_ast(_top))
                            _current = nodecl_null();
                        else
                            _current = nodecl_get_parent(_current);
                    }

                    void previous()
                    {
                        if( nodecl_is_null( _current ) )
                            _current = _top;
                        else
                            _current = nodecl_get_child(_current, 0);
                    }

                    void rewind()
                    {
                        _current = _top;
                        if (nodecl_is_null(_current))
                            return;

                        while (!nodecl_is_null(nodecl_get_child(_current, 0)))
                        {
                            _current = nodecl_get_child(_current, 0);
                        }
                    }
                public:
                    bool operator==(const iterator& it) const
                    {
                        return (nodecl_get_ast(this->_current) == nodecl_get_ast(it._current))
                            && (nodecl_get_ast(this->_top) == nodecl_get_ast(it._top));
                    }

                    bool operator!=(const iterator& it) const
                    {
                        return !this->operator==(it);
                    }

                    Nodecl::NodeclBase operator*() const
                    {
                        return Nodecl::NodeclBase(nodecl_get_child(_current, 1));
                    }

                    Nodecl::NodeclBase* operator->() const
                    {
                        Nodecl::NodeclBase* p = new Nodecl::NodeclBase(nodecl_get_child(_current, 1));

                        _cleanup.push_back(p);

                        return p;
                    }

                    iterator(const iterator& it)
                        : _top(it._top), _current(it._current),
                        // Transfer cleanup ownership
                        _cleanup(it._cleanup)
                    {
                        it._cleanup.clear();
                    }

                    iterator& operator=(const iterator& it)
                    {
                        if (this != &it)
                        {
                            this->_top = it._top;
                            this->_current = it._current;

                            // Transfer cleanup ownership
                            for (std::vector<Nodecl::NodeclBase*>::iterator it2 = it._cleanup.begin();
                                    it2 != it._cleanup.end();
                                    it2++)
                            {
                                this->_cleanup.push_back(*it2);
                            }

                            it._cleanup.clear();
                        }
                        return *this;
                    }

                    ~iterator()
                    {
                        for (std::vector<Nodecl::NodeclBase*>::iterator it = _cleanup.begin();
                                it != _cleanup.end();
                                it++)
                        {
                            Nodecl::NodeclBase* p = *it;
                            delete p;
                        }
                    }

                    iterator operator+(int n)
                    {
                        iterator it(*this);
                        for (int i = 0; i < n; i++)
                        {
                            it.next();
                        }
                        return it;
                    }

                    iterator operator-(int n)
                    {
                        iterator it(*this);
                        for (int i = 0; i < n; i++)
                        {
                            it.previous();
                        }
                        return it;
                    }

                    // it++
                    iterator operator++(int)
                    {
                        iterator t(*this);

                        this->next();

                        return t;
                    }

                    // ++it
                    iterator operator++()
                    {
                        this->next();
                        return *this;
                    }

                    // it--
                    iterator operator--(int)
                    {
                        iterator t(*this);

                        this->previous();

                        return t;
                    }

                    // --it
                    iterator operator--()
                    {
                        this->previous();
                        return *this;
                    }

                    private:
                    // Constructors (only to be used by Nodecl::List)
                    struct Begin { };
                    struct Last { };
                    struct End { };

                    iterator(nodecl_t top, nodecl_t current)
                        : _top(top), _current(current), _cleanup()
                    {
                    }

                    iterator(nodecl_t top, Begin)
                        : _top(top), _current(), _cleanup()
                    {
                        rewind();
                    }

                    iterator(nodecl_t top, Last)
                        : _top(top), _current(top), _cleanup()
                    {
                    }

                    iterator(nodecl_t top, End)
                        : _top(top), _current(), _cleanup()
                    {
                    }

                    friend class List;
            };

            struct reverse_iterator : std::iterator<std::bidirectional_iterator_tag, Nodecl::NodeclBase>
            {
                public:

                private:
                    nodecl_t _top;
                    nodecl_t _current;

                    mutable std::vector<Nodecl::NodeclBase*> _cleanup;

                    // This is iterator::next
                    void previous()
                    {
                        if (nodecl_get_ast(_current) == nodecl_get_ast(_top))
                            _current = nodecl_null();
                        else
                            _current = nodecl_get_parent(_current);
                    }

                    // This is iterator::previous
                    void next()
                    {
                        if( nodecl_is_null( _current ) )
                            _current = _top;
                        else
                            _current = nodecl_get_child(_current, 0);
                    }

                    void rewind()
                    {
                        _current = _top;
                        if (nodecl_is_null(_current))
                            return;

                        while (!nodecl_is_null(nodecl_get_child(_current, 0)))
                        {
                            _current = nodecl_get_child(_current, 0);
                        }
                    }
                public:
                    bool operator==(const reverse_iterator& it) const
                    {
                        return (nodecl_get_ast(this->_current) == nodecl_get_ast(it._current))
                            && (nodecl_get_ast(this->_top) == nodecl_get_ast(it._top));
                    }

                    bool operator!=(const reverse_iterator& it) const
                    {
                        return !this->operator==(it);
                    }

                    Nodecl::NodeclBase operator*() const
                    {
                        return Nodecl::NodeclBase(nodecl_get_child(_current, 1));
                    }

                    Nodecl::NodeclBase* operator->() const
                    {
                        Nodecl::NodeclBase* p = new Nodecl::NodeclBase(nodecl_get_child(_current, 1));

                        _cleanup.push_back(p);

                        return p;
                    }

                    reverse_iterator(const reverse_iterator& it)
                        : _top(it._top), _current(it._current),
                        // Transfer cleanup ownership
                        _cleanup(it._cleanup)
                    {
                        it._cleanup.clear();
                    }

                    reverse_iterator& operator=(const reverse_iterator& it)
                    {
                        if (this != &it)
                        {
                            this->_top = it._top;
                            this->_current = it._current;

                            // Transfer cleanup ownership
                            for (std::vector<Nodecl::NodeclBase*>::iterator it2 = it._cleanup.begin();
                                    it2 != it._cleanup.end();
                                    it2++)
                            {
                                this->_cleanup.push_back(*it2);
                            }

                            it._cleanup.clear();
                        }
                        return *this;
                    }

                    ~reverse_iterator()
                    {
                        for (std::vector<Nodecl::NodeclBase*>::iterator it = _cleanup.begin();
                                it != _cleanup.end();
                                it++)
                        {
                            Nodecl::NodeclBase* p = *it;
                            delete p;
                        }
                    }

                    reverse_iterator operator+(int n)
                    {
                        reverse_iterator it(*this);
                        for (int i = 0; i < n; i++)
                        {
                            it.next();
                        }
                        return it;
                    }

                    reverse_iterator operator-(int n)
                    {
                        reverse_iterator it(*this);
                        for (int i = 0; i < n; i++)
                        {
                            it.previous();
                        }
                        return it;
                    }

                    // it++
                    reverse_iterator operator++(int)
                    {
                        reverse_iterator t(*this);

                        this->next();

                        return t;
                    }

                    // ++it
                    reverse_iterator operator++()
                    {
                        this->next();
                        return *this;
                    }

                    // it--
                    reverse_iterator operator--(int)
                    {
                        reverse_iterator t(*this);

                        this->previous();

                        return t;
                    }

                    // --it
                    reverse_iterator operator--()
                    {
                        this->previous();
                        return *this;
                    }

                    private:
                    // Constructors (only to be used by Nodecl::List)
                    struct Begin { };
                    struct Last { };
                    struct End { };

                    reverse_iterator(nodecl_t top, nodecl_t current)
                        : _top(top), _current(current), _cleanup()
                    {
                    }

                    reverse_iterator(nodecl_t top, Begin)
                        : _top(top), _current(top), _cleanup()
                    {
                    }

                    reverse_iterator(nodecl_t top, Last)
                        : _top(top), _current(), _cleanup()
                    {
                        rewind();
                    }

                    reverse_iterator(nodecl_t top, End)
                        : _top(top), _current(), _cleanup()
                    {
                    }

                    friend class List;
            };

            // Refine this
            typedef iterator const_iterator;
            typedef reverse_iterator reverse_const_iterator;

            List()
                : NodeclBase()
            {
            }

            List(const nodecl_t n) : NodeclBase(n)
            {
            }

            iterator begin()
            {
                return iterator(this->get_internal_nodecl(), iterator::Begin());
            }

            const_iterator begin() const
            {
                return const_iterator(this->get_internal_nodecl(), const_iterator::Begin());
            }

            iterator end()
            {
                return iterator(this->get_internal_nodecl(), iterator::End());
            }

            // This is end() - 1
            const_iterator last() const
            {
                return const_iterator(this->get_internal_nodecl(), const_iterator::Last());
            }

            // This is end() - 1
            iterator last()
            {
                return iterator(this->get_internal_nodecl(), iterator::Last());
            }

            const_iterator end() const
            {
                return const_iterator(this->get_internal_nodecl(), const_iterator::End());
            }

            // Reverse iterator
            reverse_iterator rbegin()
            {
                return reverse_iterator(this->get_internal_nodecl(), reverse_iterator::Begin());
            }

            reverse_const_iterator rbegin() const
            {
                return reverse_const_iterator(this->get_internal_nodecl(), reverse_const_iterator::Begin());
            }

            reverse_iterator rend()
            {
                return reverse_iterator(this->get_internal_nodecl(), reverse_iterator::End());
            }

            reverse_const_iterator rend() const
            {
                return reverse_const_iterator(this->get_internal_nodecl(), reverse_iterator::End());
            }

            size_t size() const
            {
                if (empty())
                    return 0;
                else
                    return nodecl_list_length(this->get_internal_nodecl());
            }

            Nodecl::NodeclBase operator[](int n) const
            {
                // Not exactly the same but it will do
                return at(n);
            }

            Nodecl::NodeclBase at(int n) const
            {
                const_iterator it = this->begin();
                it = it + n;

                return *it;
            }

            Nodecl::NodeclBase front() const
            {
                return *(this->begin());
            }

            Nodecl::NodeclBase back() const
            {
                return *(this->last());
            }

            bool empty() const
            {
                return this->is_null();
            }

            // Inserts _before_ the iterator it
            void insert(iterator it, Nodecl::NodeclBase new_node)
            {
                if (this->empty())
                {
                    // This list is empty, it does not matter where we put it
                    *this = Nodecl::List::make(new_node);
                }
                else if (it != this->end())
                {
                    nodecl_t singleton = nodecl_make_list_1(new_node.get_internal_nodecl());

                    nodecl_set_child(singleton, 0,
                            nodecl_get_child(it._current, 0));

                    nodecl_set_child(it._current, 0, singleton);
                }
                else // If we are the end we have to modify "this"
                {
                    nodecl_t old_previous = nodecl_get_child(this->get_internal_nodecl(), 0);
                    nodecl_t old_last = nodecl_get_child(this->get_internal_nodecl(), 1);

                    nodecl_set_child(this->get_internal_nodecl(), 1, new_node.get_internal_nodecl());

                    nodecl_t singleton = nodecl_make_list_1(old_last);
                    nodecl_set_child(singleton, 0, old_previous);

                    nodecl_set_child(this->get_internal_nodecl(), 0, singleton);
                }
            }

        private:
            void push_back_(Nodecl::NodeclBase n)
            {
                // A null node is conceptually the empty list
                if (n.is_null())
                    return;
                insert(this->end(), n);
            }

            void push_front_(Nodecl::NodeclBase n)
            {
                // A null node is conceptually the empty list
                if (n.is_null())
                    return;
                insert(this->begin(), n);
            }
        public:
            WARN_FUNCTION("You want to call Nodecl::List::prepend instead") void push_front(Nodecl::NodeclBase n)
            {
                this->push_front_(n);
            }

            WARN_FUNCTION("You want to call Nodecl::List::append instead") void push_back(Nodecl::NodeclBase n)
            {
                this->push_back_(n);
            }

            void append(Nodecl::NodeclBase n)
            {
                if (n.is<Nodecl::List>())
                {
                    Nodecl::List l = n.as<Nodecl::List>();
                    for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++)
                    {
                        this->push_back_(*it);
                    }
                }
                else
                    this->push_back_(n);
            }

            // See prepend for an explanation of this function
            void prepend(Nodecl::NodeclBase n)
            {
                if (n.is<Nodecl::List>())
                {
                    Nodecl::List l = n.as<Nodecl::List>();
                    for (Nodecl::List::reverse_iterator it = l.rbegin(); it != l.rend(); it++)
                    {
                        this->push_front_(*it);
                    }
                }
                else
                    this->push_front_(n);
            }

            // This method performs several push_fronts if 'n' is a list which
            // effectively prepends the items of 'n' in reverse order.
            // Use prepend if you want the items of 'n' be prepended
            // in the same order as they appear in 'n'
            void prepend_reversed(Nodecl::NodeclBase n)
            {
                if (n.is<Nodecl::List>())
                {
                    Nodecl::List l = n.as<Nodecl::List>();
                    for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++)
                    {
                        this->push_front_(*it);
                    }
                }
                else
                    this->push_front_(n);
            }

            // Removes the iterator it
            iterator erase(iterator it)
            {
                nodecl_t parent = nodecl_get_parent(it._current);

                nodecl_t prev = nodecl_get_child(it._current, 0);
                bool is_first = nodecl_is_null(prev);

                if (!nodecl_is_null(parent))
                {
                    bool is_last = (it == this->last());

                    bool found = false;
                    for (int i = 0; i < MCXX_MAX_AST_CHILDREN && !found; i++)
                    {
                        if (nodecl_get_ast(nodecl_get_child(parent, i)) == nodecl_get_ast(it._current))
                        {
                            nodecl_set_child(parent, i,
                                    nodecl_get_child(it._current, 0));
                            found = true;
                        }
                    }

                    ERROR_CONDITION(!found, "Wrong chain found in a list", 0);

                    if (!is_last)
                    {
                        // The parent is the next one
                        return iterator(it._top, parent);
                    }
                    else
                    {
                        // Make sure the Nodecl::List now represents the "empty" list
                        if (is_first)
                        {
                            // The list became empty
                            this->operator=(Nodecl::List());
                        }
                        // We removed the last, return end
                        return this->end();
                    }
                }
                else
                {
                    if (is_first)
                    {
                        // The list became empty
                        this->operator=(Nodecl::List());
                    }
                    else
                    {
                        nodecl_set_child(it._current, 0, nodecl_get_child(prev, 0));
                        nodecl_set_child(it._current, 1, nodecl_get_child(prev, 1));
                    }
                    return this->end();
                }
            }

            void pop_front()
            {
                erase(this->begin());
            }

            void pop_back()
            {
                erase(this->last());
            }

            template <typename T>
            T find_first() const
            {
                const_iterator it = this->begin();
                while (it != this->end())
                {
                    if (it->is<T>())
                        return it->as<T>();
                    it++;
                }
                return T(nodecl_null());
            }

            template <typename T>
            TL::ObjectList<T> find_all() const
            {
                TL::ObjectList<T> result;

                const_iterator it = this->begin();
                while (it != this->end())
                {
                    if (it->is<T>())
                        result.append(it->as<T>());
                    it++;
                }
                return result;
            }

            TL::ObjectList<NodeclBase> to_object_list() const;

            template <typename T>
            TL::ObjectList<T> to_object_list_as() const
            {
                TL::ObjectList<T> result;
                for (List::const_iterator it = this->begin(); it != this->end(); ++it)
                {
                    result.append(it->as<T>());
                }
                return result;
            }

            static List make(const TL::ObjectList<NodeclBase>& list);


            static List make(const NodeclBase& item_1);
            static List make(const NodeclBase& item_1, const NodeclBase& item_2);
            static List make(const NodeclBase& item_1, const NodeclBase& item_2, const NodeclBase& item_3);
            static List make(const NodeclBase& item_1, const NodeclBase& item_2, const NodeclBase& item_3, const NodeclBase& item_4);
            static List make(const NodeclBase& item_1, const NodeclBase& item_2, const NodeclBase& item_3, const NodeclBase& item_4, const NodeclBase& item_5);
            static List make(const NodeclBase& item_1, const NodeclBase& item_2, const NodeclBase& item_3, const NodeclBase& item_4, const NodeclBase& item_5, const NodeclBase& item_6);
    };
}

#endif // TL_NODECL_BASE_HPP
