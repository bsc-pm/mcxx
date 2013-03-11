! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="-DWRITE_MOD_A"
! test_FFLAGS_mod2="-DWRITE_MOD_B"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD_A -DWRITE_MOD_B -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD_A
MODULE M1
    PRIVATE
    INTEGER, PUBLIC :: X
END MODULE M1
#endif

#ifdef WRITE_MOD_B
MODULE M2
    USE M1, ONLY : Y => X
    PRIVATE
    PUBLIC :: Y
END MODULE M2
#endif

#ifdef USE_MOD
PROGRAM MAIN
    USE M2, ONLY: Y
    PRINT *, Y
END PROGRAM MAIN
#endif
