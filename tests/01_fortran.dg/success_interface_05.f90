! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    LOGICAL :: L

    INTERFACE FOO
        SUBROUTINE S1(X, A)
            INTEGER :: X
            LOGICAL(1) :: A
        END
        SUBROUTINE S2(X, A)
            INTEGER :: X
            LOGICAL(2) :: A
        END
        SUBROUTINE S4(X, A)
            INTEGER :: X
            LOGICAL(4) :: A
        END
        SUBROUTINE S8(X, A)
            INTEGER :: X
            LOGICAL(8) :: A
        END
    END INTERFACE

    CALL FOO(1, L)
END PROGRAM P
