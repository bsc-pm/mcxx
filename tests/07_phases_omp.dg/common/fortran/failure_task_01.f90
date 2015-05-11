! <testinfo>
! test_generator=config/mercurium-omp
! test_compile_fail=yes
! </testinfo>

PROGRAM P
    INTEGER :: A(10)

    !$OMP TASK FIRSTPRIVATE(A(3))
        CONTINUE
    !$OMP END TASK

    !$OMP TASKWAIT
END PROGRAM P
