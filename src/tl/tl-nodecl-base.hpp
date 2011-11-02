#ifndef TL_NODECL_BASE_HPP
#define TL_NODECL_BASE_HPP

#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-scope.hpp"
#include "tl-refptr.hpp"
#include "cxx-nodecl.h"
#include "cxx-utils.h"
#include <cstdlib>

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
            TL::Symbol get_symbol() const { return TL::Symbol(::nodecl_get_symbol(_n)); }
            bool has_symbol() const { return ::nodecl_get_symbol(_n) != NULL; }
            TL::Scope retrieve_context() const { return nodecl_retrieve_context(_n); }
            std::string get_text() const { const char* c = ::nodecl_get_text(_n); if (c == NULL) c = ""; return c; }
            std::string get_filename() const { const char* c = nodecl_get_filename(_n); if (c == NULL) c = "(null)"; return c; }
            int get_line() const { return nodecl_get_line(_n); }
            std::string get_locus() const { std::stringstream ss; ss << this->get_filename() << ":" << this->get_line(); return ss.str(); }
            nodecl_t get_internal_nodecl() const { return _n; }
            TL::ObjectList<NodeclBase> children() const { 
                TL::ObjectList<NodeclBase> result;
                for (int i = 0; i < ::MCXX_MAX_AST_CHILDREN; i++)
                {
                    result.push_back(nodecl_get_child(_n, i));
                }
                return result;
            }
            Nodecl::NodeclBase copy() const
            {
                return nodecl_copy(this->_n);
            }
            bool is_constant() const { return ::nodecl_is_constant(_n); }

            // Prettyprint
            std::string prettyprint();

            // Simple RTTI
            template <typename T> bool is() const { return !this->is_null() && (T::_kind == this->get_kind()); }
            template <typename T> T as() const { return T(this->_n); }
            template <typename Ret> friend class BaseNodeclVisitor;

            // Sorting of trees by pointer
            bool operator<(const NodeclBase& n) const { return nodecl_get_ast(this->_n) < nodecl_get_ast(n._n); }

            // Equality by pointer
            bool operator==(const NodeclBase& n) const { return nodecl_get_ast(this->_n) == nodecl_get_ast(n._n); }

            // Convenience
            NodeclBase(TL::RefPtr<TL::Object>);

            // Basic replacement
            void replace(Nodecl::NodeclBase new_node);
    };

    // This class mimicks a std::list<T> but everything works by value
    class List : public NodeclBase
    {
        private:
            static const int _kind = ::AST_NODE_LIST;
            friend class NodeclBase;

        public:
            typedef Nodecl::NodeclBase value_type;
            typedef Nodecl::NodeclBase reference_type;
            typedef Nodecl::NodeclBase const_reference_type;

            struct iterator
            {
                private:
                    nodecl_t _top;
                    nodecl_t _current;

                    mutable std::vector<Nodecl::NodeclBase*> _cleanup;

                    void next()
                    {
                        if (nodecl_get_ast(_current) == nodecl_get_ast(_top))
                        {
                            _current = nodecl_null();
                        }
                        else
                        {
                            _current = nodecl_get_parent(_current);
                        }
                    }

                    void previous()
                    {
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
                        this->next();
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

            // Refine this
            typedef iterator const_iterator;

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
                if (it != this->end())
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

            void push_front(Nodecl::NodeclBase n)
            {
                insert(this->begin(), n);
            }

            void push_back(Nodecl::NodeclBase n)
            {
                insert(this->end(), n);
            }

            // Removes the iterator it
            iterator erase(iterator it)
            {
                nodecl_t parent = nodecl_get_parent(it._current);

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
                        // We removed the last, then we should return end
                        return this->end();
                    }
                }
                else
                {
                    internal_error("Impossible to remove a list without parent", 0);
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

            static List make(const TL::ObjectList<NodeclBase>& list);
    };
}

#endif // TL_NODECL_BASE_HPP
