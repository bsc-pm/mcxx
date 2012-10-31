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

    ! They broke this in versions 4.5 and 4.6. All others are OK
#if defined(__GNUC__) && ((__GNUC__ > 4) || (__GNUC__ == 4  && ( __GNUC_MINOR__ < 5 || __GNUC_MINOR__ >= 7)))
    INTEGER :: STAT

    CALL SYSTEM("ls")
    CALL SYSTEM("ls", stat)
#endif
END SUBROUTINE MAIN_2
