! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, PARAMETER :: kfp = 4
    REAL(kfp), PARAMETER :: zero = 0.0
    REAL(kfp), PARAMETER :: one = 1.0
    complex(kfp), parameter :: ic = cmplx(zero,one)

    REAL :: X

    PRINT *, ic
END
