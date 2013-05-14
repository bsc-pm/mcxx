! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: A(100)
    REAL :: K
    INTEGER :: I

    A = (/ (I, I=1, 100) /)

    K = MAXVAL(SQRT(A))
    IF (ABS(10.0 - K) > 1e-9) STOP 1
END PROGRAM MAIN
