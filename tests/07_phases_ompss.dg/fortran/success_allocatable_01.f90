! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
SUBROUTINE CHECK_PRIVATE_SEMANTICS()
    IMPLICIT NONE

    LOGICAL :: ERR
    INTEGER, ALLOCATABLE :: VAR1
    INTEGER, ALLOCATABLE :: VAR2(:)

    ERR = .FALSE.

    ALLOCATE(VAR1)
    ALLOCATE(VAR2(4))

    VAR1 = 0
    VAR2 = 0

    !$OMP TASK PRIVATE(VAR1, VAR2) SHARED(ERR)
        ERR = .NOT.(ALLOCATED(VAR1) .AND. ALLOCATED(VAR2))
        VAR1 = -1
        VAR2 = -1
    !$OMP END TASK
    !$OMP TASKWAIT

    !!! The allocatable variables should be allocated inside the task
    IF (ERR) STOP -1

    !!!  Changes inside the task should not be visible outside
    IF (VAR1 /= 0) STOP -2
    IF (ANY(VAR2 /= 0)) STOP -3

    DEALLOCATE(VAR1)
    DEALLOCATE(VAR2)

    !$OMP TASK PRIVATE(VAR1, VAR2) SHARED(ERR)
        ERR = (ALLOCATED(VAR1) .OR. ALLOCATED(VAR2))
        ALLOCATE(VAR1)
        ALLOCATE(VAR2(4))
    !$OMP END TASK
    !$OMP TASKWAIT

    !!! The allocatable variables shouldn't be allocated inside the task
    IF (ERR) STOP -4

    !!! The original allocatable variables should still be unallocated
    IF (ALLOCATED(VAR1) .OR. ALLOCATED(VAR2)) STOP -5
END SUBROUTINE CHECK_PRIVATE_SEMANTICS

SUBROUTINE CHECK_FIRSTPRIVATE_SEMANTICS()
    IMPLICIT NONE

    LOGICAL :: ERR
    INTEGER, ALLOCATABLE :: VAR1
    INTEGER, ALLOCATABLE :: VAR2(:)

    ERR = .FALSE.

    ALLOCATE(VAR1)
    ALLOCATE(VAR2(4))

    VAR1 = 0
    VAR2 = 0

    !$OMP TASK FIRSTPRIVATE(VAR1, VAR2) SHARED(ERR)
        !!! It should be allocated
        ERR = (.NOT.(ALLOCATED(VAR1) .AND. ALLOCATED(VAR2)))
        !!! And the values should be 0
        ERR = ERR .OR. (VAR1 /= 0) .OR. ANY(VAR2 /= 0)

        VAR1 = -1
        VAR2 = -1
    !$OMP END TASK
    !$OMP TASKWAIT

    !!! We didn't fulfill the semantics of the firstprivate clause in the task
    IF (ERR) STOP -6

        !!!  Changes inside the task should not be visible outside
        IF (VAR1 /= 0) STOP -7
        IF (ANY(VAR2 /= 0)) STOP -8

        DEALLOCATE(VAR1)
        DEALLOCATE(VAR2)

        !$OMP TASK FIRSTPRIVATE(VAR1, VAR2) SHARED(ERR)
            ERR = (ALLOCATED(VAR1) .OR. ALLOCATED(VAR2))
            ALLOCATE(VAR1)
            ALLOCATE(VAR2(4))
        !$OMP END TASK
        !$OMP TASKWAIT

        !! The allocatable variables shouldn't be allocated inside the task
        IF (ERR) STOP -9

        !! The original allocatable variables should still be unallocated
        IF (ALLOCATED(VAR1) .OR. ALLOCATED(VAR2)) STOP -10
END SUBROUTINE CHECK_FIRSTPRIVATE_SEMANTICS

SUBROUTINE CHECK_SHARED_SEMANTICS()
    IMPLICIT NONE

    LOGICAL :: ERR
    INTEGER, ALLOCATABLE :: VAR1
    INTEGER, ALLOCATABLE :: VAR2(:)

    ERR = .FALSE.

    ALLOCATE(VAR1)
    ALLOCATE(VAR2(4))

    VAR1 = 0
    VAR2 = 0

    !$OMP TASK SHARED(VAR1, VAR2) SHARED(ERR)
        !!! It should be allocated
        ERR = (.NOT.(ALLOCATED(VAR1) .AND. ALLOCATED(VAR2)))

        !!! And the values should be 0
        ERR = ERR .OR. (VAR1 /= 0) .OR. ANY(VAR2 /= 0)

        VAR1 = -1
        VAR2 = -1
    !$OMP END TASK
    !$OMP TASKWAIT

    !!! We didn't fulfill the semantics of the shared clause in the task
    IF (ERR) STOP -11

    !!!  Changes inside the task should be visible outside
    IF (VAR1 /= -1) STOP -12
    IF (ANY(VAR2 /= -1)) STOP -13

    DEALLOCATE(VAR1)
    DEALLOCATE(VAR2)

    !$OMP TASK SHARED(VAR1, VAR2) SHARED(ERR)
        ERR = (ALLOCATED(VAR1) .OR. ALLOCATED(VAR2))
        ALLOCATE(VAR1)
        ALLOCATE(VAR2(4))

        VAR1 = -1
        VAR2 = -1
    !$OMP END TASK
    !$OMP TASKWAIT

    !! The allocatable variables shouldn't be allocated inside the task
    IF (ERR) STOP -14

    !! The original allocatable variables should be allocated outside
    IF (.NOT. (ALLOCATED(VAR1) .AND. ALLOCATED(VAR2))) STOP -15
    IF (VAR1 /= -1) STOP -16
    IF (ANY(VAR2 /= -1)) STOP -17
END SUBROUTINE CHECK_SHARED_SEMANTICS
PROGRAM P
    IMPLICIT NONE

    CALL CHECK_PRIVATE_SEMANTICS()
    CALL CHECK_FIRSTPRIVATE_SEMANTICS()
    CALL CHECK_SHARED_SEMANTICS()
END PROGRAM P
