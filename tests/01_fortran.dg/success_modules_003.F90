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

      Integer, Parameter               :: wp = kind(0.0D0)
End Module craig_precisions
#endif
#ifdef USE_MOD
Program craig

  Use craig_precisions

  Implicit None

  Integer :: kval
  Real(kind=wp), Parameter :: half = 0.5_wp

  kval = wp

  print *, wp
End Program
#endif
