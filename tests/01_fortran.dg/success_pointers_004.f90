! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
FUNCTION REAL_POINTER( N1, N2, C)
  IMPLICIT NONE
  INTEGER N1,N2
  REAL(8), POINTER :: REAL_POINTER(:,:)
  REAL(8), TARGET :: C(N1,N2)
  REAL_POINTER => C(:,:)
END FUNCTION REAL_POINTER
