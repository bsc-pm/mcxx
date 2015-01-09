! <testinfo>
! test_generator=config/mercurium-fortran
! test_FCFLAGS="--fixed-form-length=132"
! </testinfo>
      PROGRAM MAIN
          IMPLICIT NONE
          INTEGER :: A
C---------------------------------------------------------------------72---------------------------------------------------------132
          A = 1;                                                             A = 2
          IF (A /= 2) THEN
              STOP 1
          END IF
      END PROGRAM MAIN
