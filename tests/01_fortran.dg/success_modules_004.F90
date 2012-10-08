! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD
Module craig_precisions

  Implicit None
  
  Integer, Parameter               :: i4b = selected_int_kind(9)
  Integer, Parameter               :: rp = kind(0.0)
  Integer, Parameter               :: wp = kind(0.0D0)

End Module craig_precisions
#endif

#ifdef USE_MOD
Program craig

  Use craig_precisions

  Implicit None

  Integer :: kval
  Real(kind=wp), Parameter :: half = 0.5_wp
  Real(kind=wp) :: real1 = 1.0_wp
  Real(kind=8) :: real2

  kval = wp
  print*, kval

  print*,half
  print*,real1

  real2 = 2.0_8 
  print*,real2

End Program
#endif
