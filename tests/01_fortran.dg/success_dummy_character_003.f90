! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE S(X)
            INTEGER :: X(*)
        END SUBROUTINE S
    END INTERFACE

    INTEGER :: X(10)

    CALL S(X)
    CALL S(X(1))
END PROGRAM MAIN
