! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M

    PRIVATE

    PUBLIC :: T
    TYPE T
        INTEGER :: X
    END TYPE T

    CONTAINS

        SUBROUTINE S
            IMPLICIT NONE
            TYPE(T) :: A
            PRINT *, A

        END SUBROUTINE S
END MODULE M
