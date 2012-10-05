! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
Module craig_precisions

  Implicit None
  
  Integer, Parameter               :: i4b = selected_int_kind(9)
  Integer, Parameter               :: rp = kind(0.0)
  Integer, Parameter               :: wp = kind(0.0D0)

End Module craig_precisions

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
