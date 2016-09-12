! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    REAL :: ARR(3,4)
    REAL :: RES

    ARR = 0.0
    RES = NORM2(ARR(:, 1))
END PROGRAM P
