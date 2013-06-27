! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
   INTEGER :: A(100), B(100), C(100)
   INTEGER :: I

   DO I = 1, 100
       A(I) = I
       B(I) = I
       C(I) = 0
   END DO

   !$OMP WORKSHARE
   WHERE (A(:) > 50)
      C(:) = 1
   ELSEWHERE (B(:) > 10)
      C(:) = 2
   END WHERE
   !$OMP END WORKSHARE

   DO I = 1, 100
      IF (A(I) > 50) THEN
          IF (C(I) /= 1) STOP 1
      ELSE IF (B(I) > 10) THEN
          IF (C(I) /= 2) STOP 2
      END IF
   END DO
END PROGRAM MAIN
