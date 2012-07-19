! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    INTEGER :: A(10), B(10)
    INTEGER :: I

    INTERFACE
        SUBROUTINE S(A, B, N)
            IMPLICIT NONE
            INTEGER :: A(:)
            INTEGER :: B(:)
            INTEGER :: N
        END SUBROUTINE S
    END INTERFACE

    A = 1
    B = 1

    CALL S(A, B, 4)
    !$OMP TASKWAIT
    DO I = 1, 10
      IF (A(I) /= 1) STOP 1
      IF (B(I) /= 4) STOP 2
    END DO
    
    CALL S(A(1:2), B(3:4), 20)
    !$OMP TASKWAIT
    DO I = 1, 2
      IF (A(I) /= 1) STOP 3
    END DO
    DO I = 3, 4
      IF (B(I) /= 20) STOP 4
    END DO
END PROGRAM P

SUBROUTINE S(A, B, N)
    IMPLICIT NONE
    INTEGER :: A(:)
    INTEGER :: B(:)
    INTEGER :: N

    !$OMP TASK FIRSTPRIVATE(A) SHARED(B) FIRSTPRIVATE(N)
        A = N
        B = N
    !$OMP END TASK
END SUBROUTINE S
