! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    COMPLEX, PARAMETER :: C = (1, 2)

    PRINT *, REAL(C)

END PROGRAM P
