! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, POINTER :: X
    INTEGER :: Y

    X => S()
    Y = S()

    CONTAINS
        FUNCTION S()
            INTEGER, POINTER :: S
        END FUNCTION S
END PROGRAM P
