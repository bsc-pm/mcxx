! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    INTEGER, ALLOCATABLE :: X(:, :)

    ALLOCATE(X(2:3, 4:5))

    !$OMP TASK PRIVATE(X)
      IF (LBOUND(X, DIM=1) /= 2) STOP 1
      IF (UBOUND(X, DIM=1) /= 3) STOP 2

      IF (LBOUND(X, DIM=2) /= 4) STOP 3
      IF (UBOUND(X, DIM=2) /= 5) STOP 4
    !$OMP END TASK

    !$OMP TASKWAIT
END PROGRAM MAIN
