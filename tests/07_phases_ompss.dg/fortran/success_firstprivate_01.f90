! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: X(10)

    X = 0
    !$OMP TASK FIRSTPRIVATE(X)
        X = 1
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (ANY(X /= 0)) STOP 1
END PROGRAM P
