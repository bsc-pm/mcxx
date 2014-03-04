! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTERFACE
        !$OMP TARGET DEVICE(SMP) COPY_DEPS
        !$OMP TASK INOUT(V)
        SUBROUTINE FOO(V, N)
            IMPLICIT NONE
            INTEGER :: N
            INTEGER, ALLOCATABLE :: V(:)
        END SUBROUTINE FOO
    END INTERFACE

    INTEGER, ALLOCATABLE :: V(:)

    ALLOCATE(V(10))

    V = 1

    CALL FOO(V, 10)
    !$OMP TASKWAIT

    IF (ANY(V /= -2)) STOP 1

    PRINT *, V
END PROGRAM P

SUBROUTINE FOO(V, N)
    IMPLICIT NONE
    INTEGER :: N
    INTEGER, ALLOCATABLE :: V(:)

    V = -2
END SUBROUTINE FOO

