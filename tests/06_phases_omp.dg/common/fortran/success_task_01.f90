! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM P
    INTEGER :: X

    X = 1

    !$OMP TASK SHARED(X)
        X = 3
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (X /= 3) STOP "ERROR"
END PROGRAM P
