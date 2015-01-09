! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

SUBROUTINE F(X, N)
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(INOUT) :: X(N, N)

    DO  I = 1, 3, 1
        DO  J = 1, 5, 1
            X(I,J) =  0
        ENDDO
    ENDDO

END SUBROUTINE F


SUBROUTINE G(X, I, J, N)
    INTEGER, INTENT(IN) :: N, I, J
    INTEGER, INTENT(INOUT) :: X(N, N)

    INTERFACE
        SUBROUTINE F(X, N)
            INTEGER, INTENT(IN) :: N
            INTEGER, INTENT(IN) :: X(N,N)
        END SUBROUTINE F
    END INTERFACE
    !$OMP TASK INOUT(X(1:N, 1:N)) FIRSTPRIVATE(N)
    CALL F( X(I,J),  N)
    !$OMP END TASK


END SUBROUTINE G


PROGRAM P
    IMPLICIT NONE
    INTEGER :: X(5, 5)
    INTEGER :: I, J


    DO  I = 1, 5, 1
        DO  J = 1, 5, 1
            X(I,J) = ((I-1)*5 + J);
            PRINT *, X(I,J)
        ENDDO
    ENDDO

    PRINT *, "--------------------------------------"
    CALL G(X, 3, 1, 5)
    !$OMP TASKWAIT
    PRINT *, "--------------------------------------"

    DO  I = 1, 5, 1
        DO  J = 1, 5, 1
            PRINT *, X(I,J)
        ENDDO
    ENDDO



END PROGRAM P
