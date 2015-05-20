! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTERFACE
        SUBROUTINE SUB(X)
            IMPLICIT NONE
            INTEGER(KIND(0)) :: X
        END SUBROUTINE SUB
    END INTERFACE

    CHARACTER(LEN=100) :: C

    CALL SUB(LEN(C))
END PROGRAM MAIN
