! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM MAIN
  IMPLICIT NONE

  INTEGER :: A1, B1, A2, B2, I, J
  INTEGER, DIMENSION(:, :), ALLOCATABLE :: X, Y

  ALLOCATE(X(2:100, 3:75), Y(2:100, 3:75))

  A1 = 5
  B1 = 10
  A2 = 6
  B2 = 11

  Y = 42

  !$OMP TASK OUT(X(A1:B1, A2:B2))
  X(5:10, 6:11) = 3
  !$OMP END TASK

  A1 = 5+1
  B1 = 10+1
  A2 = 6+1
  B2 = 11+1

  !$OMP TASK OUT(Y(A1-1:B1-1, A2-1:B2-1)) IN(X(A1-1:B1-1, A2-1:B2-1))
  Y(5:10, 6:11) = X(5:10,6:11) + 1
  !$OMP END TASK

  !$OMP TASKWAIT

  DO I = 5, 10
   DO J = 6, 11
     IF (5 <= I &
         .AND. I <= 10 &
         .AND. 6 <= J &
         .AND. J <= 11) THEN
        IF (Y(I, J) /= 4) STOP 1
     ELSE
        IF (Y(I, J) /= 42) STOP 2
     END IF
   END DO
  END DO
END PROGRAM MAIN

