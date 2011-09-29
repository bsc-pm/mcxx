#include "tl-nodecl-base.hpp"
#include "cxx-utils.h"
#include "cxx-codegen.h"
#ifdef FORTRAN_SUPPORT
#include "fortran03-codegen.h"
#endif

namespace Nodecl
{
    std::string NodeclBase::prettyprint()
    {
        const char* result = NULL;
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            result = c_cxx_codegen_to_str(_n);
        }
#ifdef FORTRAN_SUPPORT
        else if (IS_FORTRAN_LANGUAGE)
        {
            result = fortran_codegen_to_str(_n);
        }
#endif
        if (result == NULL)
        {
            return "<<<null codegen>>>";
        }
        else
        {
            return result;
        }
    }
}
