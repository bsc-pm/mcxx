! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S(A, B)
    IMPLICIT NONE

    INTEGER :: B(10)
    INTEGER :: A(B(1))

    PRINT *, A, B

END SUBROUTINE S
