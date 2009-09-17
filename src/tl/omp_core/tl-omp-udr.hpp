#ifndef TL_OMP_UDR_HPP
#define TL_OMP_UDR_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"

namespace TL
{
    bool function_is_valid_udr_reductor_c(Type reduction_type, Symbol reduction_op);
    bool function_is_valid_udr_reductor_cxx(Type reduction_type, Symbol reduction_op);

    bool udr_is_builtin_operator(const std::string &op_name);
}

#endif // TL_OMP_UDR_HPP
