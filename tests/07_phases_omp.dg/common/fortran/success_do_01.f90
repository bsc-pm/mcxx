! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: A(100)
    INTEGER :: I

    !$OMP DO
    DO I = 1, 100
      A(I) = I
    END DO
    !$OMP END DO

    DO I = 1, 100
     IF (A(I) /= I) STOP 1
    END DO
END PROGRAM
