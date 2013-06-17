! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE M
SAVE
INTRINSIC :: NULL
REAL, POINTER :: R(:) => NULL()
END MODULE M
#endif

#ifdef WRITE_MOD2
MODULE M_INTERN

USE M
IMPLICIT NONE
REAL, POINTER :: ARR(:) => NULL()
SAVE
PUBLIC :: FOO
CONTAINS

SUBROUTINE FOO()
        ARR(1) = 2
END SUBROUTINE FOO


END MODULE M_INTERN
#endif WRITE_MOD2

#ifdef USE_MOD
PROGRAM P
USE M_INTERN, ONLY: FOO
IMPLICIT NONE

CALL FOO()

END PROGRAM P
#endif
