! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE M1
    IMPLICIT NONE
    CONTAINS
        FUNCTION FOO(X) RESULT(Y)
            IMPLICIT NONE
            INTEGER :: X
            INTEGER :: Y
            Y = X + 1
        END FUNCTION FOO
END MODULE M1

SUBROUTINE ZOO()
    USE M1
    IMPLICIT NONE
    CALL BAR()

    CONTAINS
        SUBROUTINE BAR()
            IMPLICIT NONE
            INTEGER :: X
            INTEGER :: I

            !$OMP TASK
                X = FOO(X)
            !$OMP END TASK

            !$OMP TASKWAIT
        END SUBROUTINE BAR
END SUBROUTINE ZOO

PROGRAM MAIN
    EXTERNAL :: ZOO
    CALL ZOO()
END PROGRAM MAIN
