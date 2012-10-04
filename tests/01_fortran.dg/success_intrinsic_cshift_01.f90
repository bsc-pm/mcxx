! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    REAL(8) :: ARR(10, 10, 10)

    INTEGER(4) :: SHIFT0, SHIFT2(10, 10)
    INTEGER(4) :: D

    PRINT *, CSHIFT(ARR, SHIFT0, D)
    PRINT *, CSHIFT(ARR, SHIFT2, D)

END PROGRAM MAIN
