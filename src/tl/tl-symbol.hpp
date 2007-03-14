#ifndef TL_SYMBOL_HPP
#define TL_SYMBOL_HPP

#include <string>
#include <vector>
#include <sstream>
#include "tl-ast.hpp"
#include "tl-object.hpp"
#include "tl-type.hpp"
#include "cxx-scope.h"

namespace TL
{
    class Type;
    class Scope;
    class Symbol : public Object
    {
        public:
            scope_entry_t* _symbol;

        private:
            virtual tl_type_t* get_extended_attribute(const std::string& str) const
            {
                return NULL;
            }

        public:
            static const Symbol invalid();

            bool is_invalid() const;

            bool is_valid() const;

            Symbol(scope_entry_t* symbol)
                : _symbol(symbol)
            {
            }

            Type get_type() const;
            std::string get_name() const;

            std::string get_qualified_name() const;

            Scope get_scope() const;

            virtual ~Symbol()
            {
            }

            virtual bool is_symbol() const
            {
                return true;
            }

            bool operator<(Symbol s) const;
            bool operator==(Symbol s) const;
            bool operator!=(Symbol s) const;
            Symbol& operator=(Symbol s);

            bool is_variable() const;
            bool is_typename() const;
            bool is_function() const;
            bool is_template_function() const;
            bool is_member() const;
            bool is_parameter() const;

            Type get_class_type() const;

            AST_t get_point_of_declaration() const;
    };
}

#endif // TL_SYMBOL_HPP
