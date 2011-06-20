! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
  IMPLICIT NONE
  INTEGER :: A(100)
  INTEGER :: I

  A = (/ (I, I=1,100) /)

  DO I = 1, 100
    IF (A(I) /= I) THEN
       STOP 1
    END IF
  END DO
END PROGRAM P
