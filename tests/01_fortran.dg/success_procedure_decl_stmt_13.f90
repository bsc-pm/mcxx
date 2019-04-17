! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

PROGRAM P
    IMPLICIT NONE

    PROCEDURE(TYPE(MY_TYPE)), POINTER :: FOO2

    TYPE MY_TYPE
    END TYPE MY_TYPE

    TYPE(MY_TYPE) :: VAR

    FOO2 => BAR

    VAR = BAR()

    CONTAINS

        FUNCTION BAR()
            IMPLICIT NONE
            TYPE(MY_TYPE) :: BAR
        END FUNCTION BAR
END PROGRAM P
