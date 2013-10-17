! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: X(8) = 1.2
    REAL :: Y(8) = 1

    PRINT *, X, Y
END PROGRAM P
