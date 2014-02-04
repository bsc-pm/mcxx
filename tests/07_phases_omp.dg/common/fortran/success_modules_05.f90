! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>

MODULE M1
   IMPLICIT NONE
   TYPE T1
     INTEGER :: X
   END TYPE T1
   TYPE(T1), ALLOCATABLE :: V(:)
END MODULE M1

PROGRAM MAIN

   USE M1
   IMPLICIT NONE

   ALLOCATE(V(10))

   V % X = 0

   !$OMP TASK SHARED(V)
      V % X = 1
   !$OMP END TASK

   !$OMP TASKWAIT

   IF (ANY( V % X /= 1)) STOP 1

END PROGRAM MAIN
