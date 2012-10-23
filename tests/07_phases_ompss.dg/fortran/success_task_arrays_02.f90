! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
MODULE EXPLICIT
    IMPLICIT NONE
    CONTAINS
        !$OMP TASK INOUT(X)
        SUBROUTINE S1(X, LX1, UX1, LX2, UX2)
            IMPLICIT NONE
            INTEGER :: LX1, UX1, LX2, UX2
            INTEGER :: X(LX1:UX1, LX2:UX2)

            X = X + 1
        END SUBROUTINE S1

        !$OMP TASK INOUT(X(LX1:UX1, LX2:UX2))
        SUBROUTINE S2(X, LX1, UX1, LX2, UX2)
            IMPLICIT NONE
            INTEGER :: LX1, UX1, LX2, UX2
            INTEGER :: X(LX1:UX1, LX2:UX2)

            X = X + 1
        END SUBROUTINE S2

        !$OMP TASK INOUT(X(:, :))
        SUBROUTINE S22(X, LX1, UX1, LX2, UX2)
            IMPLICIT NONE
            INTEGER :: LX1, UX1, LX2, UX2
            INTEGER :: X(LX1:UX1, LX2:UX2)

            X = X + 1
        END SUBROUTINE S22

        !$OMP TASK INOUT(X)
        SUBROUTINE S3(X, LX1, UX1, LX2, UX2)
            IMPLICIT NONE
            INTEGER :: LX1, UX1, LX2, UX2
            INTEGER :: X(LX1:UX1, LX2:UX2)

            IF (ANY(X(:, :) /= 42)) STOP 4
            X = X + 1
        END SUBROUTINE S3

        !$OMP TASK INOUT(X(LX1:UX1, LX2:UX2))
        SUBROUTINE S4(X, LX1, UX1, LX2, UX2)
            IMPLICIT NONE
            INTEGER :: LX1, UX1, LX2, UX2
            INTEGER :: X(LX1:UX1, LX2:UX2)

            IF (ANY(X(:, :) /= 43)) STOP 5
            X = X + 1
        END SUBROUTINE S4

        !$OMP TASK INOUT(X(:, :))
        SUBROUTINE S5(X, LX1, UX1, LX2, UX2)
            IMPLICIT NONE
            INTEGER :: LX1, UX1, LX2, UX2
            INTEGER :: X(LX1:UX1, LX2:UX2)

            IF (ANY(X(:, :) /= 44)) STOP 6
            X = X + 1
        END SUBROUTINE S5
END MODULE EXPLICIT

PROGRAM MAIN
    USE EXPLICIT
    IMPLICIT NONE
    INTEGER :: A(2:300, 4:200)

    INTEGER :: TESTER(5:15,25:34)

    ! THIS WILL EASE CHECKING THE CONTENTS OF A
    EQUIVALENCE(TESTER, A(5,17))

    A = 42

    !! TESTS OF DATA INTEGRITY

    CALL S1(A(5, 17), 5, 15, 25, 34)
    !$OMP TASKWAIT
    IF (ANY(TESTER(:, :) /= 43)) STOP 1

    CALL S2(A(5, 17), 5, 15, 25, 34)
    !$OMP TASKWAIT
    IF (ANY(TESTER(:, :) /= 44)) STOP 2

    CALL S22(A(5, 17), 5, 15, 25, 34)
    !$OMP TASKWAIT
    IF (ANY(TESTER(:, :) /= 45)) STOP 3

    !! TESTS OF DATA DEPENDENCY
    A = 42

    CALL S3(A(5, 17), 5, 15, 25, 34)
    CALL S4(A(5, 17), 5, 15, 25, 34)
    CALL S5(A(5, 17), 5, 15, 25, 34)
    !$OMP TASKWAIT

    IF (ANY(TESTER(:, :) /= 45)) STOP 7
END PROGRAM MAIN
