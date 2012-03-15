! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S
    IMPLICIT NONE

    CONTAINS

        SUBROUTINE S1
            CALL FOO
        END SUBROUTINE S1

        SUBROUTINE S2
            CALL FOO
        END SUBROUTINE S2
END SUBROUTINE S
