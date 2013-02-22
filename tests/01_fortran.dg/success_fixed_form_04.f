! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      MODULE M
          PRIVATE
          PUBLIC A
          INTEGER :: A = 10
          INTEGER :: B = 10
      CONTAINS
      END MODULE M

      PROGRAM MAIN
          USE M
          IMPLICIT NONE

          IF (A /= 10) STOP 1
      END PROGRAM MAIN
