! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M
    TYPE FOO
        INTEGER :: X
    END TYPE FOO
END MODULE M
MODULE M1
    USE M

    INTERFACE OPERATOR(+)
        MODULE PROCEDURE ADDFOO
    END INTERFACE

    CONTAINS
        FUNCTION ADDFOO(X, Y)
            TYPE(FOO) :: X, Y, ADDFOO
            POINTER :: ADDFOO
            INTENT(IN) :: X, Y
        END FUNCTION
END MODULE M1
PROGRAM P
    USE M1
    IMPLICIT NONE
    TYPE(FOO) :: A, B, C
    POINTER :: C

    C => A + B
END 
