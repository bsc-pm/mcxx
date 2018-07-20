! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE BAR(X)
    IMPLICIT NONE
    COMPLEX(8) :: X
    COMPLEX(8) :: RES

    RES = CDSQRT(X)
    RES = ZSQRT(X)

    RES = CDEXP(X)
    RES = ZEXP(X)

    RES = CDCOS(X)
    RES = ZCOS(X)

    RES = CDSIN(X)
    RES = ZSIN(X)
END SUBROUTINE BAR
