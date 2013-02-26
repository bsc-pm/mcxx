! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M1
    TYPE T1
        INTEGER :: X
    END TYPE T1

    INTERFACE OPERATOR(==)
        MODULE PROCEDURE FOO
        END INTERFACE

    CONTAINS

        FUNCTION FOO(A, B)
            LOGICAL :: FOO
            TYPE(T1), INTENT(IN) :: A, B

            FOO = (A %X == B % X)
        END FUNCTION FOO

END MODULE M1

MODULE M2
    TYPE T2
        INTEGER :: X
    END TYPE T2

    INTERFACE OPERATOR(==)
        MODULE PROCEDURE BAR
    END INTERFACE

    CONTAINS

        FUNCTION BAR(A, B)
            LOGICAL :: BAR
            TYPE(T2), INTENT(IN) :: A, B

            BAR = (A %X == B % X)
        END FUNCTION BAR

END MODULE M2

MODULE M3
    USE M1
    USE M2
    PRIVATE
    TYPE T3
        INTEGER :: X1
        INTEGER :: X2
    END TYPE T3
    PUBLIC :: T3, T2, T1, OPERATOR(==)
    INTERFACE OPERATOR(==)
        MODULE PROCEDURE QUUX
    END INTERFACE

    CONTAINS
        FUNCTION QUUX(A, B)
            LOGICAL :: QUUX
            TYPE(T3), INTENT(IN) :: A, B

            QUUX = (A %X1 == B % X1) .AND. ( A%X2 == B % X2 )
        END FUNCTION QUUX
END MODULE M3

PROGRAM MAIN

    USE M3
    TYPE(T3) :: A3, B3
    LOGICAL :: L

    A3 = T3(1, 2)
    B3 = T3(3, 2)

    L = (A3 == B3)

END PROGRAM MAIN
