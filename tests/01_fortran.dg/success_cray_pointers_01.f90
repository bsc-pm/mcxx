! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cray"
! test_FFLAGS_cray="-fcray-pointer"
! </testinfo>
PROGRAM P
  REAL :: B(10)
  POINTER(X, A(10))
  X = LOC(B)
  A(1) = 3
END PROGRAM P

