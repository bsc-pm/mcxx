! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      PROGRAM MAIN
          IMPLICIT NONE
          INTEGER :: A
C---------------------------------------------------------------------72---------------------------------------------------------132
          A = 1;                                                             A = 2
          IF (A /= 1) THEN
              STOP 1
          END IF
      END PROGRAM MAIN
