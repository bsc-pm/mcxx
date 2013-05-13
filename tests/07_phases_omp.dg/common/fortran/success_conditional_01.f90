! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN

 IMPLICIT NONE
 INTEGER :: A
 !$ INTEGER :: B

 A = 10
 !$ B = 1

 !$OMP PARALLEL DEFAULT(SHARED)

 !$ B = A

 !$OMP END PARALLEL

 !$ IF (A == B) THEN
 !$   CONTINUE
 !$ ELSE
 STOP 1
 !$ END IF

END PROGRAM MAIN
