! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2"
! </testinfo>

#ifdef WRITE_MOD
MODULE M1
    IMPLICIT NONE
    INTEGER :: THIS_IS_A_LONG_NAME
END MODULE M1
#endif

#ifdef WRITE_MOD2
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
#endif
