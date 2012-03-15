! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S(N, A)
    IMPLICIT NONE
    !! This is a common extension. 
    !! Note that N is declared after it's been used
    INTEGER :: A(N)
    INTEGER :: N

    PRINT *, A
END SUBROUTINE S
