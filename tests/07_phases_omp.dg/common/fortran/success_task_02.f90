! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE M2
IMPLICIT NONE
  INTEGER :: X
END MODULE M2

PROGRAM P
USE M2
IMPLICIT NONE

X = 42
!$OMP TASK
    X = 1
!$OMP END TASK

!$OMP TASKWAIT

IF (X /= 1) STOP 1
END PROGRAM P
