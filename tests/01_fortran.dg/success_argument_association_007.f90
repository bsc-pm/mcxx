! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE SUB(S)
            EXTERNAL :: S
        END SUBROUTINE SUB
    END INTERFACE
    EXTERNAL :: S2

    CALL SUB(S2)
END PROGRAM MAIN

