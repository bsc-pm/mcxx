! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

MODULE MOO
  IMPLICIT NONE

  TYPE T
    PRIVATE
    INTEGER :: X
  CONTAINS
    PROCEDURE :: SET_X => MY_SET_X
    PROCEDURE :: GET_X => MY_GET_X
  END TYPE T

  CONTAINS

     SUBROUTINE MY_SET_X(T1, X)
       IMPLICIT NONE
       CLASS(T) :: T1
       INTEGER :: X

       T1 % X = X
     END SUBROUTINE

     FUNCTION MY_GET_X(T1)
       IMPLICIT NONE
       INTEGER :: MY_GET_X
       CLASS(T) :: T1

       MY_GET_X = T1 % X
     END FUNCTION
END MODULE MOO


PROGRAM MAIN
  USE MOO
  IMPLICIT NONE
  TYPE(T) :: A

  CALL A % SET_X(42)
  IF (A % GET_X() /= 42) STOP 1
END PROGRAM MAIN

