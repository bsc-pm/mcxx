! <testinfo>
! test_generator=config/mercurium-fortran
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
