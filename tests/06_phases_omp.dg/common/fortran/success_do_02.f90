! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
SUBROUTINE FILL(V, S)
    IMPLICIT NONE
    INTEGER :: V(10)
    INTEGER :: I, S

    !$OMP PARALLEL SHARED(V)
    !$OMP DO
    DO I= 1, 10, S
        V(I) = 1
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
END SUBROUTINE FILL

PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: W(10)
    INTEGER :: I

    INTERFACE
        SUBROUTINE FILL(V, S)
            IMPLICIT NONE
            INTEGER :: V(10)
            INTEGER :: I, S
        END SUBROUTINE FILL
    END INTERFACE

    W = 42
    CALL FILL(W, 1)
    DO I = 1, 10, 1
     IF (W(I) /= 1) STOP 1
    END DO

    W = 41
    CALL FILL(W, 2)
    DO I = 1, 10, 1
      IF (MOD(I, 2) == 1) THEN
          IF (W(I) /= 1) STOP 2
      ELSE
          IF (W(I) /= 41) STOP 3
      END IF
    END DO

    W = 40
    CALL FILL(W, 3)
    DO I = 1, 10, 1
      IF (MOD(I, 3) == 1) THEN
          IF (W(I) /= 1) STOP 4
      ELSE
          IF (W(I) /= 40) STOP 5
      END IF
    END DO

END PROGRAM MAIN
