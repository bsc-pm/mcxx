! <testinfo>
! test_generator="config/mercurium-ompss"
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
      SUBROUTINE S
      IMPLICIT NONE

      INTEGER :: I, J, W

!$OMP PARALLEL DO
      DO 40 I = 1, 100
      IF (W > 20) THEN
      DO 41 J = 1, 200
41    CONTINUE
      ELSE IF (W <= 20) THEN
       DO 42 J = 1, 200
42    CONTINUE
      ELSE
       DO 43 J = 1, 200
43    CONTINUE
      END IF
40    CONTINUE

      END SUBROUTINE S

      PROGRAM MAIN
          CALL S
      END PROGRAM MAIN
