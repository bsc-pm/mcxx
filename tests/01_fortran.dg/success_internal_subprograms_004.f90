! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE FOO
    IMPLICIT NONE
    INTEGER :: X

CONTAINS

    SUBROUTINE SUB
        IMPLICIT NONE

        INTEGER :: Z

        Z = BAR(10) + BAR2(20)
        PRINT *, Z
    CONTAINS
        PURE FUNCTION BAR(Y) RESULT(X)
            INTEGER, INTENT(IN) :: Y
            INTEGER :: X

            X = Y + 1
        END FUNCTION BAR

        PURE FUNCTION BAR2(Y) RESULT(MY_X)
            INTEGER, INTENT(IN) :: Y
            INTEGER :: MY_X

            ! MY_X = Y + 1
        END FUNCTION BAR2
    END SUBROUTINE SUB
END MODULE FOO
