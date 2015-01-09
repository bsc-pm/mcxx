! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
SUBROUTINE FOO(F_IN, N)
    INTEGER :: N
    INTEGER :: F_IN(N)

    !$OMP TASK SHARED(F_IN)

        !$OMP TASK SHARED(F_IN)
            F_IN(1) = 1;
        !$OMP END TASK
        !$OMP TASKWAIT

    !$OMP END TASK

    !$OMP TASKWAIT
END SUBROUTINE FOO

PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 10
    INTEGER :: F(N)

    CALL FOO(F, N)
END PROGRAM P
