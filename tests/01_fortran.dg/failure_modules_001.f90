! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! test_compile_fail=yes
! </testinfo>
MODULE M
PRIVATE
INTEGER :: X
END MODULE M
PROGRAM P
USE M
IMPLICIT NONE

PRINT *, X
END PROGRAM P
