! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    REAL, PARAMETER :: A(10) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /) / 0.3

    PRINT *, A
    PRINT *, A(1)
END PROGRAM P
