! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN

IMPLICIT NONE
INTEGER, PARAMETER :: A(3) = (/ 1, 2, 3 /)

INTERFACE
    SUBROUTINE S(B)
        INTEGER :: B(2)
    END SUBROUTINE S
END INTERFACE

CALL S(A(1))
END PROGRAM MAIN
