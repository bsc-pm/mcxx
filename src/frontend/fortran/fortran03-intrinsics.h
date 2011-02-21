#ifndef FORTRAN03_INTRINSICS_H
#define FORTRAN03_INTRINSICS_H

#include "cxx-scope-decls.h"

#define BITMAP(X) (1<<X)

enum intrinsic_kind_t
{
    NONE = 0,
    ATOMIC_SUBROUTINE = BITMAP(0),
    ELEMENTAL_FUNCTION = BITMAP(1),
    ELEMENTAL_SUBROUTINE = BITMAP(2),
    INQUIRY_FUNCTION = BITMAP(3),
    PURE_SUBROUTINE = BITMAP(4),
    IMPURE_SUBROUTINE = BITMAP(5),
    TRANSFORMATIONAL_FUNCTION = BITMAP(6),
};

#undef BITMAP

void register_all_fortran_intrinsics(decl_context_t decl_context);

#endif // FORTRAN03_INTRINSICS_H
