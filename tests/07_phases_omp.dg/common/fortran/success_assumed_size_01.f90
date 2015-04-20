! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
SUBROUTINE S(X, Y)
    INTEGER :: X(*)
    INTEGER :: Y(10, *)

    !$OMP TASK SHARED(X, Y)
    X(1) = 3
    Y(1,2) = 5
    !$OMP END TASK

    !$OMP TASKWAIT
END SUBROUTINE S

PROGRAM MAIN
    INTEGER :: V(10)
    INTEGER :: W(10, 20)
    V = 4
    W = 8
    CALL S(V, W)

    IF (V(1) /= 3) STOP 1
    IF (W(1, 2) /= 5) STOP 2
END PROGRAM MAIN
