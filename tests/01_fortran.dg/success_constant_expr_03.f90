! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE A
    REAL, PARAMETER :: R(4) =  REAL ( (/ 1, 2, 3, 4 /) )

    PRINT *, R
END
