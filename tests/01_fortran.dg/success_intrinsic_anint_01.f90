! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    IF ( ABS(ANINT (ABS (-2.78) ) - 3.0) > 1e-4) STOP 1
END PROGRAM MAIN
