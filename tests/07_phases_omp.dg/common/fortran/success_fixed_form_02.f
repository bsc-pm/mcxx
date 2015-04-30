! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
      PROGRAM MAIN
          IMPLICIT NONE
          INTEGER :: A
          A = 1
C$        A = 2
          IF ( A /= 2) STOP 1
      END PROGRAM MAIN
