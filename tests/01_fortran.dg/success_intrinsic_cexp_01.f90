! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    COMPLEX :: C, C1

    C = (1.2, 3.4)

    C1 = CEXP(C)

END PROGRAM P
