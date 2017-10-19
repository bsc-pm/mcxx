! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER :: I, LIMIT
        !$OMP TASK FINAL(.FALSE.)
        !$OMP END TASK

        !$OMP TASK FINAL(I < LIMIT)
        !$OMP END TASK

        !$OMP TASKWAIT
END PROGRAM P
