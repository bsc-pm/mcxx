! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    INTEGER :: A(100), ILOW, IUPP

    A = 1

    ILOW = 1
    IUPP = 49

    !$OMP TASK SHARED(A)
    CALL SUB(A)
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (ANY(A(1:49) /= 2)) STOP 1
    IF (ANY(A(50:100) /= 1)) STOP 2

    CONTAINS

        SUBROUTINE SUB(B)
            INTEGER :: B(100)
            B(ILOW:IUPP) = B(ILOW:IUPP) + 1
        END SUBROUTINE SUB


END PROGRAM MAIN
