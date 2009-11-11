#ifndef TL_OMP_UDR_HPP
#define TL_OMP_UDR_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-omp.hpp"

namespace TL
{
    void initialize_builtin_udr_reductions(Scope global_scope);
    bool udr_is_builtin_operator(const std::string &op_name);

    Symbol solve_udr_name_cxx(LangConstruct construct,
            AST_t ref_tree_of_clause,
            std::string &op_name,
            Type reduction_type,
            OpenMP::UDRInfoItem::Associativity &assoc);
}

#endif // TL_OMP_UDR_HPP
