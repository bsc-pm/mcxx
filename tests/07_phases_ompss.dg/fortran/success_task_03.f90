! <testinfo>
! test_generator="config/mercurium-ompss no-nanos6"
! </testinfo>

SUBROUTINE F(X, JSIZE, ISIZE)
    INTEGER, INTENT(IN) :: JSIZE, ISIZE
    INTEGER, INTENT(INOUT) :: X(JSIZE, ISIZE)
    X = 0
END SUBROUTINE F

SUBROUTINE G(X, J, I, N)
    INTEGER, INTENT(IN) :: N, I, J
    INTEGER, INTENT(INOUT) :: X(N, N)
    INTEGER :: JSIZE, ISIZE

    INTERFACE
        SUBROUTINE F(X, JSIZE, ISIZE)
            INTEGER, INTENT(IN) :: JSIZE, ISIZE
            INTEGER, INTENT(INOUT) :: X(JSIZE, ISIZE)
        END SUBROUTINE F
    END INTERFACE

    JSIZE = N - J + 1
    ISIZE = N - I + 1
    !$OMP TASK INOUT(X(J:, I:))
    CALL F( X(J, I),  JSIZE, ISIZE)
    !$OMP END TASK
END SUBROUTINE G


PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 5
    INTEGER :: X(N, N)
    INTEGER :: I, J
    INTEGER :: IND_I, IND_J


    IND_I = 3
    IND_J = 1

    DO  I = 1, N
        DO  J = 1, N
            X(J, I) = ((I-1)*N + J)
        ENDDO
    ENDDO

    CALL G(X, IND_J, IND_I, N)
    !$OMP TASKWAIT

    DO I=1, IND_I - 1
        DO J = 1, IND_J - 1
            IF (X(J, I) /= ((I-1)*N + J)) STOP 1
        ENDDO
    ENDDO
    IF (ANY(X(IND_J:, IND_I:) /= 0)) STOP 2
END PROGRAM P
