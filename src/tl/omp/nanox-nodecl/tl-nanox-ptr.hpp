#ifndef TL_NANOX_PTR_HPP
#define TL_NANOX_PTR_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"

namespace TL { namespace Nanox {
    
    // This is for Fortran only
    TL::Symbol get_function_ptr_of(TL::Symbol sym, TL::Scope original_scope, FILE* ancillary_file);
    TL::Symbol get_function_ptr_of(TL::Type type, TL::Scope original_scope, FILE* ancillary_file);

} }

#endif // TL_NANOX_PTR_HPP
