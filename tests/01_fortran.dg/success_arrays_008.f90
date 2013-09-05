! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: A(10) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)

    INTEGER :: B(A(3)), X

    B = 1

    PRINT *, B

    X = SIZE(B)

    IF (X /= 3) STOP 1

END PROGRAM P
