! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE MOO
    IMPLICIT NONE
CONTAINS

  SUBROUTINE S(X)
     IMPLICIT NONE
     INTEGER :: X(:, :, :)
     INTEGER :: I, J, K

    !$OMP PARALLEL DO PRIVATE(J, K)
    DO I = LBOUND(X, DIM=3), UBOUND(X, DIM=3)
    DO J = LBOUND(X, DIM=2), UBOUND(X, DIM=2)
    DO K = LBOUND(X, DIM=1), UBOUND(X, DIM=1)
      !! WRITE(UNIT=*, FMT="(3(I0','),Z0)") I, J, K, LOC(X(K, J, I))
      X(K, J, I) = -X(K, J, I)
    END DO
    END DO
    END DO
  END SUBROUTINE S
END MODULE MOO

PROGRAM MAIN
  USE MOO
  IMPLICIT NONE
  INTEGER :: I, J, K, W
  INTEGER, ALLOCATABLE :: X(:, :, :, :)

  ALLOCATE(X(3, 4, 5, 6))

  DO W=1, UBOUND(X, DIM=4)
  DO K=1, UBOUND(X, DIM=3)
  DO J=1, UBOUND(X, DIM=2)
  DO I=1, UBOUND(X, DIM=1)
    X(I, J, K, W) = I - J + K - W
  END DO
  END DO
  END DO
  END DO

  CALL S(X(:, :, 1, :))
  CALL S(X(:, :, 2, :))
  CALL S(X(:, :, 3, :))

  DO W=1, UBOUND(X, DIM=3)
  DO K=1, UBOUND(X, DIM=3)
  DO J=1, UBOUND(X, DIM=2)
  DO I=1, UBOUND(X, DIM=1)
      IF (1 <= K .AND. K <= 3) THEN
          IF (X(I, J, K, W) /= -(I - J + K - W)) STOP 1
      ELSE
          IF (X(I, J, K, W) /= (I - J + K - W)) STOP 2
      END IF
  END DO
  END DO
  END DO
  END DO
END PROGRAM MAIN

