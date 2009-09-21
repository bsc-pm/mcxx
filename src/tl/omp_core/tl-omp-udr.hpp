#ifndef TL_OMP_UDR_HPP
#define TL_OMP_UDR_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-omp.hpp"

namespace TL
{
    bool function_is_valid_udr_reductor_c(Type reduct_type, Symbol sym, OpenMP::UDRInfoItem::Associativity &assoc);
    bool function_is_valid_udr_reductor_cxx(Type reduct_type, Symbol sym, OpenMP::UDRInfoItem::Associativity &assoc);

    bool udr_is_builtin_operator(const std::string &op_name);
}

#endif // TL_OMP_UDR_HPP
