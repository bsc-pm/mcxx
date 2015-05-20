! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: I
    I = 1

    !$OMP TASK &
    !!$OMP PRIVATE(I) &
    !$OMP SHARED(I)
    I = 0
    !$OMP END TASK
    !$OMP TASKWAIT

    PRINT *, I
    IF (I /= 0) STOP 1
END PROGRAM MAIN
