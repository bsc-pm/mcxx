! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    INTEGER, ALLOCATABLE :: A(:, :)


    CALL S(A(1, 1), 1, 2)

    CONTAINS

    SUBROUTINE S(X, N, M)
        IMPLICIT NONE
        INTEGER :: N, M
        INTEGER :: X(N, M)
    END SUBROUTINE S

END PROGRAM P
