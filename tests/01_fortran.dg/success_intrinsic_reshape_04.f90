! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE FOO(resul, source, shap)

   IMPLICIT NONE
   REAL, INTENT(OUT)   :: resul(:,:,:)
   REAL, INTENT(IN)    :: source(:,:,:,:)
   INTEGER, INTENT(IN) :: shap(4)

   resul = RESHAPE(source, (/1,1,1/))
   resul = RESHAPE(source, shap(1:3))

END SUBROUTINE FOO
