! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_mod3="-DWRITE_MOD3"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DWRITE_MOD3"
! </testinfo>

#ifdef WRITE_MOD
MODULE M1
    IMPLICIT NONE
    INTEGER :: X
    INTEGER :: S
END MODULE M1
#endif

#ifdef WRITE_MOD2
MODULE M2
    USE M1, ONLY : S1 => S
    IMPLICIT NONE
END MODULE M2
#endif

#ifdef WRITE_MOD3
MODULE M3
    USE M2, ONLY : S1
    IMPLICIT NONE

    CONTAINS 
        SUBROUTINE FOO
            IMPLICIT NONE
            S1 = S1 + 1
        END SUBROUTINE FOO

END MODULE M3
#endif
