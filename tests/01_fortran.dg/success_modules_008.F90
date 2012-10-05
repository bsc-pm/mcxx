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
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = 4
END MODULE M1
#endif

#ifdef WRITE_MOD2
MODULE M2
    USE M1
    IMPLICIT NONE
END MODULE M2
#endif

#ifdef USE_MOD
PROGRAM P
    USE M2
    IMPLICIT NONE
    INTEGER(KIND = K) :: A
    A = K
END PROGRAM P
#endif
