! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S2(P)
    EXTERNAL :: P

    CALL P(1)
END SUBROUTINE S2
