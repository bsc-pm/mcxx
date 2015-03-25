! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S(SIM)
    IMPLICIT NONE
    EXTERNAL :: SIM
    INTERFACE
        SUBROUTINE SUB(X)
            EXTERNAL :: X
        END SUBROUTINE SUB
    END INTERFACE

    CALL SIM(1, 2, 3)
    CALL SUB(SIM)
END SUBROUTINE S
