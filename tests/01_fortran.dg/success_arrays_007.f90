! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE A
   IMPLICIT NONE

   TYPE T
      INTEGER, ALLOCATABLE :: v(:)
   END TYPE T

   INTEGER :: n

CONTAINS

   SUBROUTINE FOO

      IMPLICIT NONE
      TYPE(T) :: var
      INTEGER :: suma

      ALLOCATE(var%v(n))
      suma = sum(var%v(:))

   END SUBROUTINE FOO

END MODULE A
