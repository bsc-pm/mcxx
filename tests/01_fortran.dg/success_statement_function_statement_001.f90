! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: IA, IB
    IA(IB) = 3

    PRINT *, IA(9)
END PROGRAM P
