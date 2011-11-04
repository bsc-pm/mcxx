! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: K(2), W(2)

    DATA K / 2 * Z'000000FF' /
    DATA W / 2 * -1 /
END PROGRAM P
