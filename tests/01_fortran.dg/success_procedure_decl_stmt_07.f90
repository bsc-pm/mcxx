! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE FOO(FUN)
    IMPLICIT NONE
    INTERFACE
        SUBROUTINE MY_C_FUN(A, B)
            IMPLICIT NONE
            INTEGER A, B
        END SUBROUTINE MY_C_FUN
    END INTERFACE

    PROCEDURE(MY_C_FUN), OPTIONAL :: FUN

    IF(PRESENT(FUN)) THEN
        CALL FUN(1, 2)
    ENDIF
END SUBROUTINE FOO
