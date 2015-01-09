! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>

! It only works with Nanos++ family(openmp) version(8) at least
PROGRAM MAIN
  USE OMP_LIB
  IMPLICIT NONE
  INTEGER :: I

  !$OMP PARALLEL DO SCHEDULE(STATIC)
  DO I = 1, 100
      CONTINUE
  END DO
END PROGRAM MAIN
