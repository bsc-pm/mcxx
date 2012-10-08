! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
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
#endif

#ifdef USE_MOD
PROGRAM P
    USE M
    IMPLICIT NONE

    CALL FOO(1)
    CALL FOO(1.3)
END PROGRAM P
#endif
