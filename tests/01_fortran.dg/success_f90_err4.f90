! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM fun
INTEGER ::  in

SELECTCASE(in)
CASE (:-1)
  WRITE (*,*) in
CASE (0)
  WRITE (*,*) in
CASE DEFAULT
  WRITE (*,*) in
ENDSELECT

END PROGRAM fun

