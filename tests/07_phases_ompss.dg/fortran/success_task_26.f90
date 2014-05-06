! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM P
    INTEGER I,J

    I = 0
    J = 1
    !$OMP TASK INOUT(i,j)
        if (i == 0) J = 0

        !$OMP TASK INOUT(i, j)
        I = I + 1
        !$OMP END TASK
        !$OMP TASKWAIT

       J = J + 1
    !$OMP END TASK
    !$OMP TASKWAIT

    if (I /= 1 .or. J /= 1) STOP 1
END PROGRAM P
