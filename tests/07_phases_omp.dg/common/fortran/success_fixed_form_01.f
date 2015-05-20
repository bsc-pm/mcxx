! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
C23456789
      PROGRAM MAIN
      INTEGER :: X, I
      DO 10 I =1, 1
!$OMP critical
      X = 3
!$omp end critical
C* Read final mesh surface associated to field jf
!$omp critical
      X = 4
!$OMP end critical
10    CONTINUE
      IF (X /= 4) STOP 1
      END PROGRAM MAIN

