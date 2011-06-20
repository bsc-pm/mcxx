! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M
  TYPE T
      INTEGER :: X, Y
  END TYPE T

  INTERFACE ASSIGNMENT(=)
      MODULE PROCEDURE KOO
  END INTERFACE

  CONTAINS

   SUBROUTINE KOO(T1, T2)
     TYPE(T) :: T1, T2
     INTENT(OUT) :: T1
     INTENT(IN) :: T2

     STOP 0
   END SUBROUTINE

END MODULE M

PROGRAM P
  USE M
  TYPE(T) :: A, B

  A = B

  STOP 1
END PROGRAM P
