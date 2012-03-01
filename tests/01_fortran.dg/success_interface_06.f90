! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    TYPE A
        INTEGER :: B
    END TYPE A

    INTERFACE OPERATOR(/)
        FUNCTION F(X, Y)
            IMPORT 
            IMPLICIT NONE
            TYPE(A) :: F
            TYPE(A), INTENT(IN) :: X, Y
        END 
    END INTERFACE

    TYPE(A) :: M1, M2, M3

    M1 = M2 / M3

END PROGRAM P
