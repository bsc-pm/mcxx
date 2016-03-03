! <testinfo>
! test_generator=config/mercurium-ompss
! test_FFLAGS="--no-copy-deps"
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTEGER, ALLOCATABLE :: A(:)
    INTEGER, POINTER :: P(:)

    TYPE T
        INTEGER, ALLOCATABLE :: A(:)
        INTEGER, POINTER :: P(:)
    END TYPE T

    INTEGER :: ERR
    INTEGER :: NUM_READY_TASKS

    TYPE(T) :: D

    INTEGER, EXTERNAL :: NANOS_STOP_SCHEDULER, &
        NANOS_WAIT_UNTIL_THREADS_PAUSED, &
        NANOS_START_SCHEDULER, &
        NANOS_WAIT_UNTIL_THREADS_UNPAUSED, &
        NANOS_GET_NUM_READY_TASKS

    ERR = NANOS_STOP_SCHEDULER()
    ERR = NANOS_WAIT_UNTIL_THREADS_PAUSED()

    ! Maybe not needed, but let's play safe
    NULLIFY(P)
    NULLIFY(D % P)

    !--- Whole array dep
    !$OMP TASK INOUT(A)
    CONTINUE
    !$OMP END TASK

    !$OMP TASK INOUT(P)
    CONTINUE
    !$OMP END TASK

    !--- Array section
    !$OMP TASK INOUT(A(1:10))
    CONTINUE
    !$OMP END TASK

    !$OMP TASK INOUT(P(1:10))
    CONTINUE
    !$OMP END TASK

    !--- Whole array dep
    !$OMP TASK INOUT(D % A)
    CONTINUE
    !$OMP END TASK

    !$OMP TASK INOUT(P)
    CONTINUE
    !$OMP END TASK

    !--- Array section
    !$OMP TASK INOUT(D % A(5:6))
    CONTINUE
    !$OMP END TASK

    !$OMP TASK INOUT(D % P(4:5))
    CONTINUE
    !$OMP END TASK

    ERR = NANOS_GET_NUM_READY_TASKS (NUM_READY_TASKS)

    ERR = NANOS_START_SCHEDULER()
    ERR = NANOS_WAIT_UNTIL_THREADS_UNPAUSED()
    !$OMP TASKWAIT

    IF ( NUM_READY_TASKS /= 8) THEN
        PRINT *, NUM_READY_TASKS, "!= 8"
        STOP 1
    END IF
END PROGRAM MAIN
