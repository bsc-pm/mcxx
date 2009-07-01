#ifndef TL_OVERLOAD_HPP
#define TL_OVERLOAD_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"

namespace TL
{
    struct Overload
    {
        // Symbol solve_overload(ObjectList<Type> argument_types, bool &valid);
        Symbol solve(
                ObjectList<Symbol> candidate_functions,
                Type implicit_argument_type,
                ObjectList<Type> argument_types, 
                const std::string filename,
                int line,
                bool &valid, 
                ObjectList<Symbol>& argument_conversor);
    };
};

#endif // TL_OVERLOAD_HPP
