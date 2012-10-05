! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE A
    IMPLICIT NONE
    INTEGER, PARAMETER :: AX = 1
    INTEGER, PARAMETER :: AY = 1
END MODULE A

MODULE B
    USE A, ONLY : AX
    IMPLICIT NONE

    INTEGER, PARAMETER :: BX = AX + 1
END MODULE B

PROGRAM MAIN
    USE B, ONLY : BX
    USE A, ONLY : AY
    IMPLICIT NONE

    PRINT *, BX, AY
END PROGRAM MAIN
