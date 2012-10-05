! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1
    IMPLICIT NONE

    TYPE, PUBLIC :: MY_TYPE
        PRIVATE
        INTEGER :: OPAQUE
    END TYPE MY_TYPE

END MODULE M1

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

SUBROUTINE MY_SUBROUTINE
    USE M3, ONLY : WP
    USE M2

    CALL FOO(WP(1,2) % handle)
END SUBROUTINE MY_SUBROUTINE
