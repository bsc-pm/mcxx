! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM fun
  INTEGER :: info
  IF (info /= 0) WRITE (*,*) "bla"
END PROGRAM fun
