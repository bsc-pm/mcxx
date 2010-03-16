#ifndef TL_DATA_REFERENCE_HPP
#define TL_DATA_REFERENCE_HPP

#include "tl-langconstruct.hpp"
#include "tl-source.hpp"

namespace TL
{
    class DataReference : Expression
    {
        private:
            bool _valid;
            Symbol _base_symbol;
            Type _type;
            Source _size;
            Source _addr;

            static bool gather_info_data_expr_rec(Expression expr, 
                    Symbol &base_sym, 
                    Source &size, 
                    Source &addr, 
                    bool enclosing_is_array);

            static bool gather_info_data_expr(Expression &expr, 
                    Symbol &base_sym, 
                    Source &size, 
                    Source &addr);
        public:
            DataReference(AST_t ast, ScopeLink scope_link);
            DataReference(Expression expr);

            bool is_valid();

            Symbol get_base_symbol();
            Type get_type();

            Source get_address();
            Source get_sizeof();
    };
}

#endif // TL_DATA_REFERENCE_HPP
