! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    INTEGER, ALLOCATABLE :: X(:, :)

    !$OMP TASK PRIVATE(X)
    ALLOCATE(X(100, 200))
    PRINT *, SIZE(X, DIM=1), SIZE(X, DIM=2)
    DEALLOCATE(X)
    !$OMP END TASK

    !$OMP TASKWAIT

END PROGRAM MAIN
