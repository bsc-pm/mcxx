! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    REAL, PARAMETER :: K = 0.2

    INTERFACE
        SUBROUTINE FOO(X)
            INTEGER :: X
        END SUBROUTINE FOO
    END INTERFACE
    call FOO(NINT(180.0_8/K))
END PROGRAM P

SUBROUTINE FOO(X)
    INTEGER :: X
END SUBROUTINE FOO
