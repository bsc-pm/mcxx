! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M1
      IMPLICIT NONE

      INTERFACE FOO
          MODULE PROCEDURE FOO
      END INTERFACE

      CONTAINS
        SUBROUTINE FOO(X)
            INTEGER :: X
        END SUBROUTINE FOO
END MODULE M1

MODULE M2
      IMPLICIT NONE
      CONTAINS

        SUBROUTINE FOO()
            USE M1, ONLY: WRAPPER => FOO
        END SUBROUTINE FOO
END MODULE M2
