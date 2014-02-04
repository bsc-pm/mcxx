! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>

MODULE C
   TYPE T
           INTEGER :: X
   END TYPE T
END MODULE C

MODULE A
   USE C, ONLY : T

   TYPE(T), POINTER :: V(:) => NULL()
END MODULE A

PROGRAM MAIN
   USE A, ONLY : V
   IMPLICIT NONE

   ALLOCATE(V(10))

   V % X = 1

   !$OMP TASK SHARED(V)
   V % X = 2
   !$OMP END TASK

   !$OMP TASKWAIT

   IF ( ANY (V % X /= 2) ) STOP 1

   DEALLOCATE(V)
END PROGRAM MAIN

