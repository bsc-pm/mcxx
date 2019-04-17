! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

PROGRAM P
    IMPLICIT NONE
      PROCEDURE(IFOO), POINTER :: FOO1 => NULL()
      PROCEDURE(BAR),  POINTER :: FOO2

      INTERFACE
          SUBROUTINE IFOO(X)
              INTEGER :: X
          END SUBROUTINE IFOO
      END INTERFACE

      FOO1 => BAR
      FOO2 => BAR

      CALL FOO1(1)
      CALL FOO2(2)
      CONTAINS
          SUBROUTINE BAR(X)
              IMPLICIT NONE
              INTEGER :: X
          END SUBROUTINE BAR
END PROGRAM P

