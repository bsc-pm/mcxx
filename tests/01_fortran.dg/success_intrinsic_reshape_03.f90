! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: A(10, 20, 30)
    INTEGER :: I, J, K

    CALL S(RESHAPE(A, (/ I, J, K /)), I, J, K)

    CONTAINS

        SUBROUTINE S(X, I, J, K)
            INTEGER :: I, J, K
            REAL :: X(I, J, K)
        END SUBROUTINE S

END PROGRAM MAIN
