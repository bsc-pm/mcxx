! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
SUBROUTINE S(A, B, N, V)
    IMPLICIT NONE
    INTEGER :: A(N)
    INTEGER :: B(N)
    INTEGER :: N, V

    !$OMP TASK FIRSTPRIVATE(A) SHARED(B) FIRSTPRIVATE(N, V)
        A = V
        B = V
    !$OMP END TASK
END SUBROUTINE S

PROGRAM P

    INTERFACE
        SUBROUTINE S(A, B, N, V)
            IMPLICIT NONE
            INTEGER :: A(N)
            INTEGER :: B(N)
            INTEGER :: N
            INTEGER :: V
        END SUBROUTINE S
    END INTERFACE

    INTEGER :: A(10)
    INTEGER :: B(10)

    A = 1
    B = 1

    PRINT *, A
    PRINT *, B
    PRINT *, "----"

    CALL S(A, B, 10, 12)
    !$OMP TASKWAIT

    PRINT *, A
    PRINT *, B
    PRINT *, "----"

    DO I = 1, 10
        IF (A(I) /= 1) STOP 1
        IF (B(I) /= 12) STOP 2
    END DO

    CALL S(A, B, 5, 22)
    !$OMP TASKWAIT

    PRINT *, A
    PRINT *, B
    PRINT *, "----"

    DO I = 1, 5
        IF (A(I) /= 1) STOP 3
        IF (B(I) /= 22) STOP 4
    END DO

    CALL S(A(5:10), B(5:10), 6, 42)
    !$OMP TASKWAIT

    PRINT *, A
    PRINT *, B
    PRINT *, "----"

    DO I = 5, 10
        IF (A(I) /= 1) STOP 5
        IF (B(I) /= 42) STOP 6
    END DO
END PROGRAM P
