! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE M
   TYPE T
    INTEGER :: X
   END TYPE T
   CONTAINS
     ELEMENTAL FUNCTION ADD_T(X, Y) RESULT(Z)
         TYPE(T) :: X, Y, Z
         INTENT(IN) :: X, Y
         Z % X = X % X + Y % X
     END FUNCTION ADD_T
END MODULE M

PROGRAM MAIN
  USE M, ONLY : T, ADD_T
  IMPLICIT NONE

  !$OMP DECLARE REDUCTION(ADD : TYPE(T) : OMP_OUT = ADD_T(OMP_OUT, OMP_IN))  &
  !$OMP    INITIALIZER(OMP_PRIV = T(0))

  INTEGER :: I
  TYPE(T) :: A(100), S
  A = (/ (T(I), I=1,100) /)
  S = T(0)

  !$OMP PARALLEL DO SHARED(A) REDUCTION(ADD : S)
  DO I = 1, 100
    S = ADD_T(S, A(I))
  END DO

  IF (S % X /= 5050) STOP 1
END PROGRAM MAIN
