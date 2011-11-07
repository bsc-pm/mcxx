! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1
    IMPLICIT NONE
    TYPE T
        INTEGER :: X
    END TYPE T
END MODULE M1

MODULE M2
    IMPLICIT NONE
    USE M1
    TYPE(T) :: S
    INTEGER, PARAMETER :: P = 4
END MODULE M2

MODULE M
    IMPLICIT NONE
    USE M2
CONTAINS 
    SUBROUTINE FOO
        PRINT *, S % X
    END SUBROUTINE FOO
END MODULE M
