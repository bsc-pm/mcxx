! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    REAL, PARAMETER :: ZERO = 0.0
    REAL, PARAMETER :: ONE = 1.0
    COMPLEX, PARAMETER :: X = (ZERO,ONE)

    PRINT *, X
END PROGRAM P
