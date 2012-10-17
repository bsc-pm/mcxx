! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
MODULE M

  INTEGER :: A
  INTEGER :: B
  INTEGER :: TOT(2)

  EQUIVALENCE(TOT(1), A)
  EQUIVALENCE(TOT(2), B)
END MODULE M

PROGRAM P
    USE M
    IMPLICIT NONE

    A = 42
    B = 64

    IF (TOT(1) /= 42 .OR. TOT(2) /= 64) STOP 1
END PROGRAM P

