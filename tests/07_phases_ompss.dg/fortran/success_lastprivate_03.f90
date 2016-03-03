! <testinfo>
! test_generator="config/mercurium-ompss"
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
SUBROUTINE SUB(X)
  IMPLICIT NONE
  INTEGER :: X(:), I
  !$OMP PARALLEL DO LASTPRIVATE(X)
  DO I= 1, 100
    X = (/ I, I, I /)
  END DO

  IF (ANY(X /= (/ 100, 100, 100 /))) STOP 1
END SUBROUTINE SUB

PROGRAM MAIN
    IMPLICIT NONE
  INTERFACE
    SUBROUTINE SUB(X)
      IMPLICIT NONE
      INTEGER :: X(:)
    END SUBROUTINE SUB
  END INTERFACE

  INTEGER :: Y(3)
  Y = (/ 1, 2, 3 /)

  CALL SUB(Y)
END PROGRAM MAIN
