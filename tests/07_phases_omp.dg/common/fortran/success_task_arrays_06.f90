! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE A

  TYPE T
          INTEGER :: X
  END TYPE T

  TYPE(T), ALLOCATABLE :: V(:)

END MODULE A

PROGRAM MAIN
  USE A, ONLY : V
  IMPLICIT NONE


  ALLOCATE(V(10))

  V % X = 3

  !$OMP TASK SHARED(V)
  V % X = V % X + 1
  !$OMP END TASK

  !$OMP TASKWAIT

  IF (ANY( V % X  /= 4 )) STOP 1
END PROGRAM MAIN
