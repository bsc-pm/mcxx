! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      program p
 10	CONTINUE
	IF(1.LT.0.)THEN
	  GOTO 10
	ENDIF
      end program p
