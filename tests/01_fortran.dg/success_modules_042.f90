! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M
    INTERFACE FOO
        SUBROUTINE BAR1(X)
            INTEGER :: X
        END SUBROUTINE BAR1
        SUBROUTINE BAR2(X)
            REAL :: X
        END SUBROUTINE BAR2
    END INTERFACE FOO
END MODULE M

PROGRAM P
    USE M
    IMPLICIT NONE

    CALL FOO(1)
    CALL FOO(1.3)
END PROGRAM P
