! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
COMPLEX :: C1, C2

C1 = (1,2)
C2 = (3,4)

IF (ABS(C1) > ABS(C2)) THEN
  CONTINUE
END IF

END PROGRAM P
