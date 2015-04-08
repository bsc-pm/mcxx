! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
MODULE MOO
    IMPLICIT NONE
    INTEGER :: NUMITEMS
END MODULE MOO

SUBROUTINE SUB1(X, Y, N)
    USE MOO, ONLY : NUMITEMS
    IMPLICIT NONE
    INTEGER :: N
    INTEGER :: X(NUMITEMS)
    INTEGER :: Y(N)

    !$OMP TASK SHARED(X, Y)
      CALL FOO
    !$OMP END TASK

    CONTAINS
        SUBROUTINE FOO
            X(:) = X(:) + 1
            Y(:) = Y(:) + 2
        END SUBROUTINE FOO
END SUBROUTINE SUB1

SUBROUTINE SUB2(X, Y, N)
    USE MOO, ONLY : NUMITEMS
    IMPLICIT NONE
    INTEGER :: N
    INTEGER :: X(NUMITEMS)
    INTEGER :: Y(N)

    !$OMP TASK SHARED(X, Y)
      CALL FOO
    !$OMP END TASK

    CONTAINS
        SUBROUTINE FOO
            X = X + 1
            Y = Y + 2
        END SUBROUTINE FOO
END SUBROUTINE SUB2

PROGRAM MAIN
    USE MOO, ONLY : NUMITEMS
    IMPLICIT NONE

    INTEGER :: A(10), A_TEST(10)
    INTEGER :: B(5), B_TEST(5)
    INTEGER :: I

    NUMITEMS = 10

    DO I = 1, NUMITEMS
      A_TEST(I) = I + 1
    END DO
    DO I = 1, 5
      B_TEST(I) = I - 1
    END DO

    A = A_TEST
    B = B_TEST
    CALL SUB1(A, B, 5)
    !$OMP TASKWAIT

    IF (ANY(A_TEST + 1 /= A)) STOP 1
    IF (ANY(B_TEST + 2 /= B)) STOP 2

    A = A_TEST
    B = B_TEST
    CALL SUB2(A, B, 5)
    !$OMP TASKWAIT

    IF (ANY(A_TEST + 1 /= A)) STOP 3
    IF (ANY(B_TEST + 2 /= B)) STOP 4

END PROGRAM MAIN
