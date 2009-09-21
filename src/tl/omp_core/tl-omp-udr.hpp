#ifndef TL_OMP_UDR_HPP
#define TL_OMP_UDR_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-omp.hpp"

namespace TL
{
    void initialize_builtin_udr_reductions(Scope global_scope);
    bool udr_is_builtin_operator(const std::string &op_name);
}

#endif // TL_OMP_UDR_HPP
