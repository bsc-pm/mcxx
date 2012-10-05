! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1
    IMPLICIT NONE
    INTEGER :: X
    INTEGER :: S
END MODULE M1

MODULE M2
    USE M1, ONLY : S1 => S
    IMPLICIT NONE
END MODULE M2

MODULE M3
    USE M2, ONLY : S1
    IMPLICIT NONE

    CONTAINS 
        SUBROUTINE FOO
            IMPLICIT NONE
            S1 = S1 + 1
        END SUBROUTINE FOO

END MODULE M3
