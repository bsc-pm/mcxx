! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM MAIN
  IMPLICIT NONE

  INTEGER :: A, B
  INTEGER, DIMENSION(:), ALLOCATABLE :: X, Y

  ALLOCATE(X(2:100), Y(2:100))

  A = 5
  B = 10

  Y = 42

  !$OMP TASK OUT(X(A:B))
  X(5:10) = 1
  !$OMP END TASK

  A = 5+1
  B = 10+1

  !$OMP TASK IN(X(A-1:B-1)) OUT(Y(A-1:B-1))
  Y(5:10) = X(5:10) + 3
  !$OMP END TASK

  !$OMP TASK IN(Y(A-1:B-1))
  IF (ANY(Y(5:10) /= 4)) STOP 1
  !$OMP END TASK

  !$OMP TASKWAIT

  IF (ANY(Y(:4) /= 42)) STOP 2
  IF (ANY(Y(11:) /= 42)) STOP 3
END PROGRAM MAIN

