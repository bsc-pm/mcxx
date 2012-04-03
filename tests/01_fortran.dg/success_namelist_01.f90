! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM  A
    IMPLICIT NONE
    LOGICAL :: RESTART

    CONTAINS

        SUBROUTINE S
            IMPLICIT NONE
            NAMELIST /NML/ RESTART
            RESTART = .TRUE.
        END
END
