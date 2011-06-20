! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
  IMPLICIT NONE

  INTEGER :: A(100), I

  DATA A /100 * 3/

  DO I = 1, 100
    IF (A(I) /= 3) THEN
      STOP 1
    END IF
  END DO

END PROGRAM P
