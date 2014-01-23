! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: V(:, :)

    ALLOCATE(V(10, 5))

    V = 1
    !$OMP TARGET DEVICE(SMP) COPY_DEPS
    !$OMP TASK INOUT(V)
        V(1, :)= -2
    !$OMP END TASK

    !$OMP TASKWAIT
    if (ANY(V(1,:) /= -2)) STOP 1
    if (ANY(V(2:,:) /= 1)) STOP 2
END PROGRAM P
