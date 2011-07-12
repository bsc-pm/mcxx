! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P

  INTEGER(MAX1(3.4, 4.2)), TARGET :: I
  INTEGER(4), POINTER :: J

  J => I

END PROGRAM P
