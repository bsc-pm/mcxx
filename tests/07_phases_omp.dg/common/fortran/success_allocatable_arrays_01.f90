! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: A(:), B(:)

    ALLOCATE(A(20))
    ALLOCATE(B(20))

    !$OMP TASK SHARED(A) FIRSTPRIVATE(B)
    DEALLOCATE(A)
    ALLOCATE(A(10))
    DEALLOCATE(B)
    ALLOCATE(B(10))
    !$OMP END TASK
    !$OMP TASKWAIT


    IF (.NOT. ALLOCATED(A)) THEN
        PRINT *, "NOT ALLOCATED"
        STOP 1
    END IF
    IF (SIZE(A, DIM=1) /= 10) THEN
        PRINT *, "WRONG DIMENSION", SIZE(A, DIM=1)
        STOP 2
    END IF

    IF (.NOT. ALLOCATED(B)) THEN
        PRINT *, "NOT ALLOCATED"
        STOP 1
    END IF
    IF (SIZE(B, DIM=1) /= 20) THEN
        PRINT *, "WRONG DIMENSION", SIZE(A, DIM=1)
        STOP 2
    END IF

    PRINT *, "ALL OK!"
END PROGRAM MAIN
