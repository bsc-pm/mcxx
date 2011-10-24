! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: I

    REAL :: A(10)
    INTEGER :: B(3)
    REAL :: C(3)

    B = (/ 1, 4, 7 /)
    A = (/ (I, I=1, 10) /)

    C = A(B)

END PROGRAM P
