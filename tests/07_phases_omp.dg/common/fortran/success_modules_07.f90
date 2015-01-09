! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>

MODULE M1
   IMPLICIT NONE
   TYPE T1
     INTEGER :: X
   END TYPE T1
   TYPE(T1) :: V
END MODULE M1

PROGRAM MAIN

   USE M1
   IMPLICIT NONE

   V % X = 0

   !$OMP TASK FIRSTPRIVATE(V)
      V % X = 1
   !$OMP END TASK

   !$OMP TASKWAIT

   IF (V % X /= 0) STOP 1

END PROGRAM MAIN
