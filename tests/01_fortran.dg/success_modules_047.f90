! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE FOO_MOD
    REAL * 8, EXTERNAL :: reweight_fn
END MODULE FOO_MOD

PROGRAM MAIN
    USE FOO_MOD
    IMPLICIT NONE

    CONTAINS
        SUBROUTINE S
            IMPLICIT NONE

            IF (reweight_fn(1, 2, 3, 4) > 9.3) THEN
                CONTINUE
            END IF
        END SUBROUTINE S
END PROGRAM MAIN
