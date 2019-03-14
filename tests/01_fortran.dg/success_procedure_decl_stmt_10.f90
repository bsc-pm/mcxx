! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

MODULE M
    IMPLICIT NONE
    TYPE T
        PROCEDURE(), NOPASS, POINTER :: P1
        PROCEDURE(INTEGER), NOPASS, POINTER :: P2
    END TYPE T
END MODULE M

PROGRAM P
    USE M
    CLASS(T), ALLOCATABLE :: VAR

    ALLOCATE(VAR)

    VAR % P1 => FOO
    CALL VAR % P1()

    VAR % P2 => BAR
    PRINT *, VAR % P2()
    CONTAINS
        SUBROUTINE FOO()
            IMPLICIT NONE
            PRINT *, "HOLA"
        END SUBROUTINE FOO

        FUNCTION BAR()
            IMPLICIT NONE
            INTEGER :: BAR
            BAR = 42
        END FUNCTION BAR

END PROGRAM P
