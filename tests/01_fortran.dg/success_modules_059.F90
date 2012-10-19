! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD

MODULE M1
    TYPE T
        INTEGER :: X
    END TYPE T
    TYPE(T) :: LOCAL_DECOMPOSITION
END MODULE M1

#endif


#ifdef WRITE_MOD2

MODULE M2
    USE M1, ONLY: LDC => LOCAL_DECOMPOSITION
CONTAINS
    SUBROUTINE FUNCT(Y)
        REAL Y(LDC % X)
    END SUBROUTINE FUNCT
END MODULE M2

#endif


#ifdef USE_MOD

SUBROUTINE FOO
    USE M1, ONLY: LDC => LOCAL_DECOMPOSITION
    USE M2
    IMPLICIT NONE
    REAL Y(LDC % X)
    CALL FUNCT(Y)
END SUBROUTINE FOO

#endif
