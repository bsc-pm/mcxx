! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTEGER, PARAMETER :: A = IBSET(0, 4)
    INTEGER, PARAMETER :: B = IBCLR(16, 4)

    IF (A /= 16) STOP 1
    IF (B /= 0) STOP 2
END PROGRAM MAIN
