! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1
    IMPLICIT NONE
    INTEGER :: THIS_IS_A_LONG_NAME
END MODULE M1

MODULE M2
    USE M1, ONLY : THIS_IS_A_LONG_NAME

    CONTAINS
        SUBROUTINE S2(X)
            USE M1, ONLY : TIALN => THIS_IS_A_LONG_NAME
            IMPLICIT NONE
            INTEGER :: X

            X = TIALN
        END SUBROUTINE S2
END MODULE M2
