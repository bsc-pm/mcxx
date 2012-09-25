! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTEGER, POINTER :: X, Y

    X => NULL()
    ALLOCATE(X)

    Y => NULL()
    ALLOCATE(Y)

    !$OMP TASK SHARED(X) FIRSTPRIVATE(Y)
    DEALLOCATE(X)
    DEALLOCATE(Y)
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (ASSOCIATED(X)) STOP 1
    IF (.NOT. ASSOCIATED(Y)) STOP 1
END PROGRAM MAIN
