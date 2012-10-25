! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE MAIN_1
    IMPLICIT NONE

    INTEGER :: STATUS

    STATUS = SYSTEM("ls")
END SUBROUTINE MAIN_2

SUBROUTINE MAIN_2
    IMPLICIT NONE

    INTEGER :: STAT

    CALL SYSTEM("ls")
    CALL SYSTEM("ls", stat)
END SUBROUTINE MAIN_2
