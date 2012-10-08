! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 mod4 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_mod3="-DWRITE_MOD3"
! test_FFLAGS_mod4="-DWRITE_MOD4"
! test_FFLAGS_all="-DWRITE_MOD1 -DWRITE_MOD2 -DWRITE_MOD3 -DWRITE_MOD4"
! </testinfo>
#ifdef WRITE_MOD
MODULE M1
    IMPLICIT NONE
    TYPE T
        INTEGER :: X
    END TYPE T
END MODULE M1
#endif
#ifdef WRITE_MOD2
MODULE M2
    USE M1
    IMPLICIT NONE
    TYPE K
       TYPE(T) :: GLOBAL_T
    END TYPE K
END MODULE M2
#endif
#ifdef WRITE_MOD3
MODULE M3
    USE M1
    USE M2
END MODULE M3
#endif
#ifdef WRITE_MOD4
MODULE M4
    USE M3
END MODULE M4
#endif
