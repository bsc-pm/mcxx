! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTERFACE FOO
        SUBROUTINE S(X, Y, Z)
            INTEGER, POINTER :: X
            INTEGER, POINTER, OPTIONAL :: Y
            INTEGER :: Z
        END SUBROUTINE S
    END INTERFACE FOO
    INTEGER, POINTER :: PX
    INTEGER :: Z

    CALL FOO(PX, Z=3)

END PROGRAM MAIN
