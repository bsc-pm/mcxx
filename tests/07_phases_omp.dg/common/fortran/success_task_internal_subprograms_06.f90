! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE M
    INTEGER :: FOO
END MODULE M

PROGRAM MAIN
    IMPLICIT NONE
    USE M

    !$OMP TASK
    PRINT *, "HOLA"
    CALL INIT2
    !$OMP END TASK

    CONTAINS
        SUBROUTINE INIT2
            IMPLICIT NONE
            RETURN
            CALL INIT
        END SUBROUTINE INIT2

        SUBROUTINE INIT
            IMPLICIT NONE
            FOO = 3
            RETURN
            CALL INIT2
        END SUBROUTINE INIT
END PROGRAM MAIN
