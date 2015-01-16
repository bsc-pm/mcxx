! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER :: I, N, X
    LOGICAL :: FLAG

    INTEGER, EXTERNAL :: OMP_GET_MAX_THREADS
    X = 0
    N = 1
    FLAG = .FALSE.


    !$OMP PARALLEL IF (.NOT. FLAG .AND. N > 0 ) SHARED(X)
    !$OMP ATOMIC
     X = X + 1
    !$OMP END PARALLEL
    IF (X /= OMP_GET_MAX_THREADS()) STOP 1
END PROGRAM P
