! <testinfo>
! test_generator=config/mercurium-omp
! test_FFLAGS=--no-openmp
! </testinfo>
PROGRAM P
    INTEGER :: X

    X = 1
    !$OMP PARALLEL 
    X = 2
    !$OMP END PARALLEL

    IF (X /= 2) STOP "TEST 1"

    !$OMP PARALLEL FIRSTPRIVATE(X)
    X = 4
    !$OMP END PARALLEL

    IF (X /= 2) STOP "TEST 1"

END PROGRAM P
