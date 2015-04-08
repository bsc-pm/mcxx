! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE MOO
    IMPLICIT NONE

    INTERFACE NEXT
        MODULE PROCEDURE NEXT_I
    END INTERFACE NEXT

    CONTAINS
        SUBROUTINE S(X)
            IMPLICIT NONE
            INTEGER :: X
            INTEGER :: A(NEXT(X))

            PRINT *, A
        END SUBROUTINE S

        PURE FUNCTION NEXT_I(A)
            IMPLICIT NONE
            INTEGER :: NEXT_I
            INTEGER, INTENT(IN) :: A

            NEXT_I = A + 1
        END FUNCTION NEXT_I
END MODULE MOO
