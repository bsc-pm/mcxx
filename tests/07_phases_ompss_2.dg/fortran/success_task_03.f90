! <testinfo>
! test_generator=config/mercurium-ompss-2
! </testinfo>



!! In OmpSs-2, the data environment of a task is destroyed once all its children tasks have been executed-

PROGRAM P
    IMPLICIT NONE
    INTEGER :: X
    LOGICAL :: ERROR
    INTEGER :: V(5)
    INTEGER, ALLOCATABLE :: V2(:)


    ERROR = .FALSE.

    X = -1
    V = -1

    ALLOCATE(V2(5))

    V2 = -1

    !$OSS TASK PRIVATE(X, V, V2) SHARED(ERROR)
        X = 0
        V = 12
        V2 = 41

        !$OSS TASK SHARED(X, V, V2, ERROR)
            X = X + 1
            IF (X /= 1) ERROR = .TRUE.

            V = V + 1
            IF (ANY(V /= 13)) ERROR = .TRUE.

            V2 = V2 + 1
            IF (ANY(V2 /= 42)) ERROR = .TRUE.
        !$OSS END TASK

    !$OSS END TASK
    !$OSS TASKWAIT
    IF (ERROR) STOP -1

    IF (X /= -1) STOP -2
    IF (ANY(V /= -1)) STOP -3
    IF (ANY(V2 /= -1)) STOP -4
END PROGRAM P
