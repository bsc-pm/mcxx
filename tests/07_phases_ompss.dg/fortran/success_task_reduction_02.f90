! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 100
    INTEGER, ALLOCATABLE :: X(:), Y(:, :)

    ALLOCATE(X(N), Y(N, N))

    !$OMP TASK REDUCTION(+: X) REDUCTION(+: Y)
        X = 0
        Y = 1
    !$OMP END TASK
    !$OMP TASKWAIT
END PROGRAM P
