! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE computeT (tIn, tOut)
   IMPLICIT NONE
   TYPE T
         INTEGER :: FOO
   END TYPE T
   INTERFACE OPERATOR(/)
       FUNCTION JARL(a, b)
           IMPORT T
           TYPE(T) :: a, jarl
           REAL(4) :: b
           INTENT(IN) :: A, B
       END FUNCTION JARL
   END INTERFACE
   TYPE(T), DIMENSION(:), INTENT(IN) :: tIn
   TYPE(T), INTENT(OUT) :: tOut
   tOut = tOut / 1.2
END SUBROUTINE computeT

