! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    TYPE T
        INTEGER :: X
        INTEGER :: Y
    END TYPE T

    TYPE(T), PARAMETER :: FOO(2) = (/ T(1, 2), T(3, 4) /)

    PRINT *, FOO
END PROGRAM P
