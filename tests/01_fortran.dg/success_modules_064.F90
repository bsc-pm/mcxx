! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="-DWRITE_MOD_A"
! test_FFLAGS_mod2="-DWRITE_MOD_B"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD_A -DWRITE_MOD_B -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD_A
MODULE A
    IMPLICIT NONE

    TYPE T1
        SEQUENCE
        INTEGER :: X
    END TYPE T1
END MODULE A
#endif

#ifdef WRITE_MOD_B
MODULE B
    USE A
    IMPLICIT NONE

    TYPE T2
        SEQUENCE
        TYPE(T3), POINTER :: F
    END TYPE T2

    TYPE T3
        SEQUENCE
        TYPE(T1), POINTER :: M
    END TYPE T3

    TYPE(T2) :: S
END MODULE B
#endif

#ifdef USE_MOD
PROGRAM MAIN
    USE B, ONLY : S
    IMPLICIT NONE

    IF (ASSOCIATED(S%F)) STOP 1
END PROGRAM MAIN
#endif
