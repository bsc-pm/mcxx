! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: Q = 8

    PRINT *, -2**(2._q/3._q)
END PROGRAM P
