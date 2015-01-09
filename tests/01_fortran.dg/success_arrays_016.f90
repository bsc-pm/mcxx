! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE S(C)
            CHARACTER :: C(*)
        END SUBROUTINE S
    END INTERFACE
    TYPE T
        CHARACTER(LEN=10) :: C
    END TYPE T
    TYPE(T), POINTER :: X(:)

    CALL S(X % C(1:2))
END PROGRAM MAIN
