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

   !$OMP TASK FIRSTPRIVATE(V)
      V(1) % X = 0
   !$OMP END TASK

   IF (ANY( V % X /= 0)) STOP 1

END PROGRAM MAIN
