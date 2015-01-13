! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
SUBROUTINE FOO(N)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: N
    INTEGER :: RES

    INTEGER :: F, X
    F(X) = X + N

    RES = -1

    !$OMP TASK SHARED(RES, N)
        RES = F(2)
    !$OMP END TASK
    !$OMP TASKWAIT

    IF(RES /= 5) STOP 1

END SUBROUTINE FOO

PROGRAM P
    IMPLICIT NONE

    INTEGER :: N
    N = 3
    CALL FOO(N)
END PROGRAM P
