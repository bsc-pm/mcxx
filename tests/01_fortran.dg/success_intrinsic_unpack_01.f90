! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE FOO( resul, vect, mask )

   IMPLICIT NONE
   REAL, INTENT(OUT) :: resul(:,:)
   REAL, INTENT(IN) :: vect(:)
   LOGICAL, INTENT(IN) :: mask(:,:)

   resul = UNPACK(vect, mask, 0.0)

END SUBROUTINE FOO
