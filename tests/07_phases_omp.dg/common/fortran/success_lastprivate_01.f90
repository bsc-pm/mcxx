! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
  IMPLICIT NONE
  INTEGER :: X, I
  !$OMP PARALLEL DO LASTPRIVATE(X)
  DO I= 1, 100
    X = I
  END DO

  IF (X /= 100) STOP 1
END PROGRAM MAIN
