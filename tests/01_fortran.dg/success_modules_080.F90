! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE A
INTEGER, PUBLIC, SAVE :: X = 10
CONTAINS

        SUBROUTINE SUB(Y)
                INTEGER :: Y

                NAMELIST /CTR/ X
        END SUBROUTINE SUB

END MODULE A
#endif

#ifdef USE_MOD
PROGRAM MAIN
  IMPLICIT NONE
  USE A

  CALL SUB(Y = 3)
END PROGRAM MAIN
#endif
