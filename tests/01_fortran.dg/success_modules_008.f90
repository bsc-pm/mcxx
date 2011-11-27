! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = 4
END MODULE M1

MODULE M2
    USE M1
    IMPLICIT NONE
END MODULE M2

PROGRAM P
    USE M2
    IMPLICIT NONE
    INTEGER(KIND = K) :: A
    A = K
END PROGRAM P
