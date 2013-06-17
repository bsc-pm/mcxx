! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    EXTERNAL :: etime
    REAL :: etime
    REAL :: A(2), R

    r = etime(a)
END PROGRAM MAIN
