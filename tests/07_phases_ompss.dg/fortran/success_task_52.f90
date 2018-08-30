! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER :: I, N, X
    LOGICAL :: FLAG

    X = 0
    N = 1
    FLAG = .FALSE.


    !$OMP TASK IF (.NOT. FLAG .AND. N > 0 ) SHARED(X)
    !$OMP ATOMIC
     X = X + 1
    !$OMP END TASK
    !$OMP TASKWAIT
    IF (X /= 1) STOP 1
END PROGRAM P
