! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M

    TYPE T
        INTEGER :: X
    END TYPE T

    INTERFACE OPERATOR(+)
        MODULE PROCEDURE SUMA
    END INTERFACE
 CONTAINS

     FUNCTION SUMA(A, B)
         TYPE(T), INTENT(IN) :: A, B
         TYPE(T) :: SUMA
         SUMA % X = A % X + B % X
     END FUNCTION SUMA
END MODULE M

MODULE M1

    TYPE T1
        INTEGER :: X
    END TYPE T1

    INTERFACE OPERATOR(+)
        MODULE PROCEDURE SUMA2
    END INTERFACE
 CONTAINS

     FUNCTION SUMA2(A, B)
         TYPE(T1), INTENT(IN) :: A, B
         TYPE(T1) :: SUMA2
         SUMA2 % X = A % X + B % X
     END FUNCTION SUMA2
END MODULE M1

PROGRAM P
    USE M
    USE M1
    IMPLICIT NONE

    TYPE(T) :: A, B, C

    TYPE(T1) :: A1, B1, C1

    C = A + B
    C1 = A1 + B1
END PROGRAM P
