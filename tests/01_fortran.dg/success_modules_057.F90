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

    INTEGER :: FOO

END MODULE M1
#endif

#ifdef WRITE_MOD2
MODULE M2

    USE M1, ONLY : BAR => FOO

END MODULE M2
#endif

#ifdef USE_MOD
PROGRAM P

    USE M1, ONLY : FOO
    USE M2, ONLY : BAR
    IMPLICIT NONE


    PRINT *, FOO, BAR
END PROGRAM P
#endif
