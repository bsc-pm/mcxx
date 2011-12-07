! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    DOUBLE PRECISION :: D1, D2
    REAL :: R

    R = SNGL(DINT(D1) + DNINT(D2)) * CABS((3.0, 4.0))

    PRINT *, R
END PROGRAM P
