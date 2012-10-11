! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE FOO_MOD
    REAL * 8, EXTERNAL :: reweight_fn
END MODULE FOO_MOD
#endif

#ifdef USE_MOD
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
#endif
