! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE M1
  IMPLICIT NONE
  TYPE T1
    INTEGER :: X
  END TYPE T1
END MODULE M1

MODULE M2
  USE M1
  IMPLICIT NONE

   TYPE T2
       TYPE(T1) :: T1
   END TYPE T2

  TYPE(T2) :: VAR
END MODULE M2

PROGRAM MAIN
   USE M2
   IMPLICIT NONE

   VAR % T1 % X = 0

   !$OMP TASK SHARED(VAR)
       VAR % T1 % X  = 42
   !$OMP END TASK
   !$OMP TASKWAIT

   IF (VAR % T1 % X /= 42) STOP 1
END PROGRAM MAIN
