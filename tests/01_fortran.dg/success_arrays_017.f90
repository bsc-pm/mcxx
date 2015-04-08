! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE MOO
    IMPLICIT NONE

    CONTAINS
        SUBROUTINE S(X)
            IMPLICIT NONE
            INTEGER :: X
            INTEGER :: A(NEXT(X))

            PRINT *, A
        END SUBROUTINE S

        PURE FUNCTION NEXT(A)
            IMPLICIT NONE
            INTEGER :: NEXT
            INTEGER, INTENT(IN) :: A

            NEXT = A + 1
        END FUNCTION NEXT
END MODULE MOO
