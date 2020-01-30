! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE MOO

CONTAINS
    SUBROUTINE S1()
        IMPLICIT NONE

        CALL S2(S)
    END SUBROUTINE S1

    SUBROUTINE S2(FUN)
        IMPLICIT NONE
        PROCEDURE(S) :: FUN
        INTEGER :: T

        T = FUN(3)
        PRINT *, "ADEU", T
    END SUBROUTINE S2

    FUNCTION S(X) RESULT(Y)
        IMPLICIT NONE
        INTEGER :: X, Y

        PRINT *, "HOLA", X
        Y = X + 1
    END FUNCTION S

END MODULE MOO
