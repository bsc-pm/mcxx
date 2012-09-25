! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE MOO
    INTEGER :: X
END MODULE MOO

PROGRAM MAIN
    USE MOO, ONLY : X
    IMPLICIT NONE

    NAMELIST /FOO/ X

    !$OMP TASK
    PRINT *, X
    WRITE (*, NML=FOO)
    !$OMP END TASK

    CONTAINS
        SUBROUTINE S
            X = 1
            WRITE (*, NML=FOO)
        END SUBROUTINE S
END PROGRAM MAIN
