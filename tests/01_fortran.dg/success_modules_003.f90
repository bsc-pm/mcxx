! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
Module craig_precisions

      Implicit None

      Integer, Parameter               :: wp = kind(0.0D0)
End Module craig_precisions
Program craig

  Use craig_precisions

  Implicit None

  Integer :: kval
  Real(kind=wp), Parameter :: half = 0.5_wp

  kval = wp

  print *, wp
End Program
