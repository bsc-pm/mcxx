! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, PARAMETER :: NA = 10
    INTEGER, PARAMETER :: NB = 20
    INTEGER, PARAMETER :: NC = 30
    INTEGER :: A(NA, NB), B(NA, NB, NC), T(NA, NB, NC)
    INTEGER :: S
    INTEGER :: I, J, K

    DO K = 1, NC
        DO J = 1, NB
            DO I = 1, NA
                B(I, J, K) = I * J + J * K + I * K
                T(I, J, K) = B(I, J, K)
            END DO
        END DO
    END DO

    A = 0

    !$OMP PARALLEL DO REDUCTION(+:A)
    DO J = 1, NB
      DO I = 1, NA
         A(I, J) = A(I, J) + SUM(B(I, J, :))
      END DO
    END DO

    DO K = 1, NC
        DO J = 1, NB
            DO I = 1, NA
              IF (B(I, J, K) /= T(I, J, K)) THEN
                  PRINT *, "ERROR: I=", I, "J=", J, "K=", K, " IS ", &
                      B(I, J, K), " SHOULD BE ", T(I, J, K)
                  STOP 1
              END IF
            END DO
        END DO
    END DO
END PROGRAM MAIN
