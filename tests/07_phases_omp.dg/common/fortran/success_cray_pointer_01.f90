! <testinfo>
! test_generator=config/mercurium-omp
! test_FFLAGS=-fcray-pointer
! </testinfo>

PROGRAM MAIN
    INTEGER(KIND=8):: PTR
    INTEGER:: ARR(25)
    INTEGER :: STORAGE(50)

    POINTER (PTR, ARR)

    !! TEST 1
    PTR = LOC(STORAGE(25))

    ARR(1) = 41
    ARR(2) = 42

    !$OMP TASK SHARED(PTR)
       PTR = PTR + 4
       ARR(1) = ARR(1) + 1
    !$OMP END TASK

    !$OMP TASKWAIT
    IF (PTR /= LOC(STORAGE(26))) STOP 1
    IF (STORAGE(26) /= 43) STOP 2
END PROGRAM MAIN
