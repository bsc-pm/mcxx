! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    TYPE X
        INTEGER :: N
    END TYPE X

    TYPE(X) :: V(10), W(10), U(10)

    INTERFACE OPERATOR(+)
        ELEMENTAL FUNCTION FOO1(A, B) RESULT(C)
            IMPORT X
            IMPLICIT NONE
            TYPE(X) :: A, B, C
            INTENT(IN) :: A, B
        END FUNCTION FOO1
    END INTERFACE

    INTERFACE OPERATOR(-)
        FUNCTION FOO2(A, B) RESULT(C)
            IMPORT X
            IMPLICIT NONE
            TYPE(X) :: A(:), B(:), C(10)
            INTENT(IN) :: A, B
        END FUNCTION FOO2
    END INTERFACE

    U = V + W
    U = V - W
END PROGRAM P
