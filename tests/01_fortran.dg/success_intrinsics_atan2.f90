! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program atan2test

  real(kind=8) :: a,b,c

  a = 0.5
  b = 0.5
  c = atan2(a, b)

end program atan2test
