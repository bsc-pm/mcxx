! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD
MODULE FOO
    IMPLICIT NONE
     INTEGER, PARAMETER :: K = 10
END MODULE FOO
#endif

#ifdef WRITE_MOD2
MODULE BAR
    USE FOO
    IMPLICIT NONE
    TYPE T
         INTEGER :: X = K
    END TYPE T
END MODULE BAR
#endif

#ifdef USE_MOD
PROGRAM P
    USE BAR
    IMPLICIT NONE
    TYPE(T) :: S
    PRINT *, S
END PROGRAM P
#endif
