! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_mod3="-DWRITE_MOD3"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DWRITE_MOD3 -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE M1
    IMPLICIT NONE

    TYPE, PUBLIC :: MY_TYPE
        PRIVATE
        INTEGER :: OPAQUE
    END TYPE MY_TYPE

END MODULE M1
#endif

#ifdef WRITE_MOD2
MODULE M2
    USE M1
    IMPLICIT NONE

    INTERFACE FOO
        SUBROUTINE FOO_1(H)
            USE M1
            IMPLICIT NONE
            TYPE(MY_TYPE), POINTER :: H
        END SUBROUTINE FOO_1
    END INTERFACE FOO
END MODULE M2
#endif

#ifdef WRITE_MOD3
MODULE M3
    USE M2
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: WP

    TYPE :: WRAPPED_POINTER
       TYPE(MY_TYPE), POINTER :: handle
    END TYPE

    TYPE(WRAPPED_POINTER) :: WP(10, 20)
END MODULE M3
#endif

#ifdef USE_MOD
SUBROUTINE MY_SUBROUTINE
    USE M3, ONLY : WP
    USE M2

    CALL FOO(WP(1,2) % handle)
END SUBROUTINE MY_SUBROUTINE
#endif
