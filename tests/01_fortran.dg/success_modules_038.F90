! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M
  INTEGER :: X
END MODULE M

MODULE M1
   USE M
END MODULE M1

MODULE M2
  USE M1
  USE M
  PRIVATE
  CONTAINS
END MODULE M2
