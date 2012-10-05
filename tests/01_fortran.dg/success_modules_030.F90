! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE P
INTEGER, SAVE :: TAG = 220

CONTAINS

  SUBROUTINE S
     TAG = TAG + 1
  END SUBROUTINE S
  SUBROUTINE S2
     TAG = TAG + 1
  END SUBROUTINE S2

END MODULE P
