! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
MODULE EXPLICIT
    IMPLICIT NONE
    CONTAINS
        !$OMP TASK INOUT(X)
        SUBROUTINE S1(X, LX, UX)
            IMPLICIT NONE
            INTEGER :: LX, UX
            INTEGER :: X(LX:UX)

            X = X + 1
        END SUBROUTINE S

        !$OMP TASK INOUT(X(LX:UX))
        SUBROUTINE S2(X, LX, UX)
            IMPLICIT NONE
            INTEGER :: LX, UX
            INTEGER :: X(LX:UX)

            X = X + 1
        END SUBROUTINE S

        !$OMP TASK INOUT(X(:))
        SUBROUTINE S22(X, LX, UX)
            IMPLICIT NONE
            INTEGER :: LX, UX
            INTEGER :: X(LX:UX)

            X = X + 1
        END SUBROUTINE S

        !$OMP TASK INOUT(X)
        SUBROUTINE S3(X, LX, UX)
            IMPLICIT NONE
            INTEGER :: LX, UX
            INTEGER :: X(LX:UX)

            IF (ANY(X(:) /= 42)) STOP 10
            X = X + 1
        END SUBROUTINE S

        !$OMP TASK INOUT(X(LX:UX))
        SUBROUTINE S4(X, LX, UX)
            IMPLICIT NONE
            INTEGER :: LX, UX
            INTEGER :: X(LX:UX)

            IF (ANY(X(:) /= 43)) STOP 11
            X = X + 1
        END SUBROUTINE S
END MODULE EXPLICIT

PROGRAM MAIN
    USE EXPLICIT
    IMPLICIT NONE
    INTEGER :: A(2:300)

    A = 42

    !! TESTS OF DATA INTEGRITY

    CALL S1(A(5), 5, 15)
    !$OMP TASKWAIT
    IF (ANY(A(2:4) /= 42)) STOP 1
    IF (ANY(A(5:15) /= 43)) STOP 2
    IF (ANY(A(16:300) /= 42)) STOP 3

    CALL S2(A(5), 5, 15)
    !$OMP TASKWAIT
    IF (ANY(A(2:4) /= 42)) STOP 4
    IF (ANY(A(5:15) /= 44)) STOP 5
    IF (ANY(A(16:300) /= 42)) STOP 6

    CALL S22(A(5), 5, 15)
    !$OMP TASKWAIT
    IF (ANY(A(2:4) /= 42)) STOP 7
    IF (ANY(A(5:15) /= 45)) STOP 8
    IF (ANY(A(16:300) /= 42)) STOP 9

    !! TESTS OF DATA DEPENDENCY
    A = 42

    CALL S3(A(5), 5, 15)
    CALL S4(A(5), 5, 15)
    !$OMP TASKWAIT

    IF (ANY(A(2:4) /= 42)) STOP 12
    IF (ANY(A(5:15) /= 44)) STOP 13
    IF (ANY(A(16:300) /= 42)) STOP 14
END PROGRAM MAIN
