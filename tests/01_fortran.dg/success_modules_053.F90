! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE A
    IMPLICIT NONE
    INTEGER, PARAMETER :: AX = 1
    INTEGER, PARAMETER :: AY = 1
END MODULE A
#endif

#ifdef WRITE_MOD2
MODULE B
    USE A, ONLY : AX
    IMPLICIT NONE

    INTEGER, PARAMETER :: BX = AX + 1
END MODULE B
#endif

#ifdef USE_MOD
PROGRAM MAIN
    USE B, ONLY : BX
    USE A, ONLY : AY
    IMPLICIT NONE

    PRINT *, BX, AY
END PROGRAM MAIN
#endif
