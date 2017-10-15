! <testinfo>
! test_generator="config/mercurium-fortran run"
! compile_versions="default f2003"
! test_FFLAGS_default=""
! test_FFLAGS_f2003="-std=f2003"
! </testinfo>

MODULE MOO
    IMPLICIT NONE
    ENUM, BIND(C)
        ENUMERATOR :: A = 25, B, C = 29
    END ENUM
END MODULE MOO

PROGRAM MAIN
    USE MOO
    IMPLICIT NONE

    PRINT *, A, B, C
    IF (A /= 25) STOP 1
    IF (B /= 26) STOP 2
    IF (C /= 29) STOP 3
END PROGRAM MAIN
