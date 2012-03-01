! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    COMPLEX, PARAMETER :: C1 = -3.4
    COMPLEX, PARAMETER :: C2 = -3

    REAL, PARAMETER :: F1 = -3
    REAL, PARAMETER :: F2 = -3.4

    PRINT *, C1
    PRINT *, C2

    PRINT *, F1
    PRINT *, F2
END PROGRAM P
