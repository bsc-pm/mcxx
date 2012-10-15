! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM MAIN
  IMPLICIT NONE

  INTEGER, ALLOCATABLE :: X(:, :), Y(:, :)

  ALLOCATE(X(10, 10), Y(10, 10))

  !$OMP TASK OUT(X)
  X = 1
  !$OMP END TASK

  !$OMP TASK IN(X) OUT(Y)
  Y = X + 3
  !$OMP END TASK

  !$OMP TASKWAIT

  IF (ANY(Y(:, :) /= 4)) STOP 1
END PROGRAM MAIN

