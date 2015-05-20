! <testinfo>
! test_FFLAGS="--free"
! test_generator=config/mercurium-fortran
! </testinfo>
      SUBROUTINE ZZZ(I)
      LOGICAL D,A
      INTEGER I
      PARAMETER(D=.FALSE.,A=.FALSE.)
      IF (D.OR.((I.EQ.6).AND.(.NOT.A)).OR.(I.EQ.5) &
     & .OR.(I.LE.0)) RETURN
      END
