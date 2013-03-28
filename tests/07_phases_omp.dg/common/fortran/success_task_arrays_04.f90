! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    INTEGER, ALLOCATABLE :: X(:, :)

    ALLOCATE(X(2:3, 4:5))

    !$OMP TASK PRIVATE(X)
      IF (LBOUND(X, DIM=1) /= 2) CALL ABORT()
      IF (UBOUND(X, DIM=1) /= 3) CALL ABORT()

      IF (LBOUND(X, DIM=2) /= 4) CALL ABORT()
      IF (UBOUND(X, DIM=2) /= 5) CALL ABORT()
    !$OMP END TASK

    !$OMP TASKWAIT
END PROGRAM MAIN
