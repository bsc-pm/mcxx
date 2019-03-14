! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

PROGRAM P
    IMPLICIT NONE

    CALL BAR(FOO)

    CONTAINS
        SUBROUTINE BAR(FOO)
            IMPLICIT NONE
            PROCEDURE() :: FOO
            PROCEDURE(), POINTER :: P_FOO

            TYPE T
                PROCEDURE(), NOPASS, POINTER :: T_P_FOO
            END TYPE T

            TYPE(T) :: VAR

            P_FOO => FOO
            VAR % T_P_FOO => FOO

            CALL FOO(1)
            CALL P_FOO(2)
            CALL VAR % T_P_FOO(3)
        END SUBROUTINE BAR

        SUBROUTINE FOO(X)
            IMPLICIT NONE
            INTEGER :: X

            PRINT *, X
        END SUBROUTINE FOO
END PROGRAM P

