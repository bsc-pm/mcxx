! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    TYPE T
        INTEGER, ALLOCATABLE :: X(:)
    END TYPE T

   TYPE(T) :: VAR
   LOGICAL :: B

   B = ALLOCATED(VAR % X)
   
END PROGRAM P
