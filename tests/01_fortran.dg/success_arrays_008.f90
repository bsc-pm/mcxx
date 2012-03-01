! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: A(10) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)

    INTEGER :: B(A(3))

    B = 1

    PRINT *, B

END PROGRAM P
