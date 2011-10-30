! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    TYPE T
        INTEGER :: X
    END TYPE T
    TYPE(T), PARAMETER :: S = T(X = 8)

    INTEGER, PARAMETER :: K = S % X

    PRINT *, K
END PROGRAM P
