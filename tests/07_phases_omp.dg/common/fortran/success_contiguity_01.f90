! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>

!! This test does not feature any OpenMP construct, it is just
!! to verify that Mercurium preserves contiguity in all
!! the compilers we test for OpenMP
PROGRAM MAIN
  INTEGER, ALLOCATABLE :: A(:, :)
  INTEGER, POINTER :: P(:, :)
  INTEGER(KIND=MERCURIUM_C_INTPTR_T) :: ADDRESS_A
  INTEGER(KIND=MERCURIUM_C_INTPTR_T) :: ADDRESS_P

  INTERFACE
      SUBROUTINE SUB(N, X, S)
          INTEGER :: N
          INTEGER :: X(N)
          INTEGER(KIND=MERCURIUM_C_INTPTR_T), INTENT(OUT) :: S
      END SUBROUTINE SUB
  END INTERFACE
  INTEGER, PARAMETER :: N = 100
  INTEGER, PARAMETER :: NP = 4
  INTEGER :: LB1
  INTEGER :: UB1

  ALLOCATE(A(N, NP))
  ALLOCATE(P(N, NP))

  A = 1
  P = 1

  CALL SUB(N, A(:, 3), ADDRESS_A)
  IF (LOC(A(1, 3)) /= ADDRESS_A) STOP 1

  CALL SUB(N, P(:, 3), ADDRESS_P)
  IF (LOC(P(1, 3)) /= ADDRESS_P) STOP 2

  LB1 = LBOUND(a, 1)
  UB1 = UBOUND(a, 1)
  CALL SUB(N, A(LB1:UB1, 3), ADDRESS_A)
  IF (LOC(A(1, 3)) /= ADDRESS_A) STOP 3

  CALL SUB(N, P(LB1:UB1, 3), ADDRESS_P)
  IF (LOC(P(1, 3)) /= ADDRESS_P) STOP 4

  DEALLOCATE(A)
  DEALLOCATE(P)
END PROGRAM MAIN

SUBROUTINE SUB(N, X, S)
    INTEGER :: N
    INTEGER :: X(N)
    INTEGER(KIND=MERCURIUM_C_INTPTR_T), INTENT(OUT) :: S

    S = LOC(X(1))
END SUBROUTINE SUB

