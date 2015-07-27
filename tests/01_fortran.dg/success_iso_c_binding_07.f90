! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
MODULE m
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
CONTAINS
    SUBROUTINE S1(prepare_routine)
        IMPLICIT NONE
        optional :: prepare_routine
        INTERFACE
            SUBROUTINE prepare_routine() bind(C)
            END SUBROUTINE
        END INTERFACE

        type(c_funptr) :: X
        X = c_funloc(prepare_routine)
    END SUBROUTINE S1
END MODULE m

SUBROUTINE my_prepare_routine() bind(C)
    PRINT *, "HELLO"
END SUBROUTINE

PROGRAM MAIN
    USE M, ONLY : S1
    INTERFACE
        SUBROUTINE my_prepare_routine() bind(C)
        END SUBROUTINE
    END INTERFACE

    CALL S1(my_prepare_routine)
END PROGRAM MAIN
