! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    PRINT *, ANINT(1.2)
    PRINT *, AINT(1.2)

    PRINT *, ATAN(ANINT(1.2))
    PRINT *, ATAN(AINT(1.2))
END PROGRAM MAIN
