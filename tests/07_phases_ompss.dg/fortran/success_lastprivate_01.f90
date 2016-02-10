! <testinfo>
! test_generator="config/mercurium-ompss"
! test_compile_fail_nanos6_mercurium=yes
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
