! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE A
    CONTAINS

        SUBROUTINE SUB_A(X, Y)
            INTEGER :: X, Y
            OPTIONAL :: Y
        END SUBROUTINE SUB_A

END MODULE A
#endif

#ifdef WRITE_MOD2
MODULE B
    USE A
END MODULE A
#endif

#ifdef USE_MOD
PROGRAM P
    USE B

    CALL SUB_A(1)
END PROGRAM P
#endif
