#ifndef TL_NODECL_BASE_HPP
#define TL_NODECL_BASE_HPP

#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-scope.hpp"
#include "tl-refptr.hpp"
#include "cxx-nodecl.h"
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
            std::string get_text() const { return std::string(::nodecl_get_text(_n)); }
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

    class List : public NodeclBase, public std::vector<NodeclBase>
    {
        private:
            static const int _kind = ::AST_NODE_LIST;
            friend class NodeclBase;
        public:

            List(const nodecl_t& n) : NodeclBase (n)
            {
                int num_items = 0;
                nodecl_t* list = nodecl_unpack_list(_n, &num_items);
                for (int i = 0; i < num_items; i++)
                {
                    this->push_back(list[i]);
                }
                ::free(list);
            }
    };
}

#endif // TL_NODECL_BASE_HPP
