! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE MOO
    IMPLICIT NONE
    INTERFACE
        FUNCTION T(Y)
            INTEGER :: Y, X
        END FUNCTION T
    END INTERFACE
CONTAINS
    SUBROUTINE S(X1, X2)
        IMPLICIT NONE
        PROCEDURE(T) :: X1, X2

    END SUBROUTINE S
END MODULE MOO

