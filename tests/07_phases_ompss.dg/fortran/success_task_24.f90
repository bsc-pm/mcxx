! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, POINTER :: PTR(:, :)

    ALLOCATE(PTR(10, 5))

    PTR = 1

    !$OMP TARGET DEVICE(SMP) COPY_DEPS
    !$OMP TASK INOUT(PTR)
        PTR(1, :)= -2
    !$OMP END TASK

    !$OMP TASKWAIT
    if (ANY(PTR(1,:) /= -2)) STOP 1
    if (ANY(PTR(2:,:) /= 1)) STOP 2
END PROGRAM P
