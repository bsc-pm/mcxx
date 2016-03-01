! <testinfo>
! test_generator="config/mercurium-ompss"
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM MAIN
  IMPLICIT NONE
  INTEGER :: X(3), I
  !$OMP PARALLEL DO LASTPRIVATE(X)
  DO I= 1, 100
    X = (/ I, I, I /)
  END DO

  IF (ANY(X /= (/ 100, 100, 100 /))) STOP 1
END PROGRAM MAIN
