! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE MY_MOD
    IMPLICIT NONE
    TYPE FOO
        INTEGER :: Y
    END TYPE FOO
END MODULE MY_MOD

PROGRAM MAIN
    USE MY_MOD, ONLY : FOO
    IMPLICIT NONE

    TYPE(FOO) :: X(10), Y(10)

    !$OMP TASK SHARED(X) FIRSTPRIVATE(Y)
        X(1)%Y = 2
        Y(1)%Y = 3
    !$OMP END TASK

    !$OMP TASKWAIT
END PROGRAM MAIN
