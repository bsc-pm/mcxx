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

    CALL S(A, B, 5, 22)
    !$OMP TASKWAIT

    PRINT *, A
    PRINT *, B
    PRINT *, "----"

    CALL S(A(5:10), B(5:10), 5, 42)
    !$OMP TASKWAIT

    PRINT *, A
    PRINT *, B
    PRINT *, "----"
END PROGRAM P
