! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE L
    TYPE LA
        INTEGER :: X
    END TYPE LA
END MODULE L

MODULE L1
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE JARL(M)
            USE L
            TYPE(LA) :: M
        END SUBROUTINE JARL
    END INTERFACE

    CONTAINS

        SUBROUTINE FOO
            USE L
            TYPE(LA) :: A

            CALL JARL(A)
        END SUBROUTINE FOO

END MODULE L1
